/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Name, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{InternalCompilerException, SubEffectLevel}
import ca.uwaterloo.flix.language.phase.unification.Substitution

/**
  * This phase generates a list of type constraints, which include
  *   - equality constraints `tpe1 ~ tpe2`
  *   - trait constraints `C[tpe1]`
  *   - purification constraints `eff1 ~ eff2[sym ↦ Pure]`
  *
  * We gather constraints as we traverse each def.
  * Constraints are later resolved in ConstraintResolution.
  */
object ConstraintGen {

  /**
    * Generates constraints for the given expression `exp0`, adding them to the type context `c`.
    *
    * Returns the type of the expression and its effect.
    * The type and effect may include variables that must be resolved.
    */
  def visitExp(exp0: KindedAst.Expr)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    exp0 match {
      case Expr.Var(sym, _) =>
        val resTpe = sym.tvar
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs, econstrs, defTpe, _) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
        c.unifyType(tvar, defTpe, loc)
        val constrs = tconstrs.map(_.copy(loc = loc))
        c.addClassConstraints(tconstrs, loc)
        econstrs.foreach { econstr => c.unifyType(econstr.tpe1, econstr.tpe2, loc) }
        val resTpe = defTpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Sig(sym, tvar, loc) =>
        val sig = root.traits(sym.trt).sigs(sym)
        val (tconstrs, econstrs, sigTpe, _) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
        c.unifyType(tvar, sigTpe, loc)
        val constrs = tconstrs.map(_.copy(loc = loc))
        c.addClassConstraints(constrs, loc)
        econstrs.foreach { econstr => c.unifyType(econstr.tpe1, econstr.tpe2, loc) }
        val resTpe = sigTpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Hole(_, tpe, _) =>
        val resTpe = tpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.HoleWithExp(exp, tvar, evar, loc) =>
        // We ignore exp's type and allow the hole to have any type.
        // We allow the effect to be any superset of exp's effect.
        val (_, eff) = visitExp(exp)
        val atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
        c.unifyType(atLeastEff, evar, loc)
        val resTpe = tvar
        val resEff = atLeastEff
        (resTpe, resEff)

      case e: Expr.OpenAs => RestrictableChooseConstraintGen.visitOpenAs(e)

      case Expr.Use(_, _, exp, _) =>
        visitExp(exp)

      case Expr.Cst(cst, _) =>
        val resTpe = Type.constantType(cst)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Apply(exp, exps, tvar, evar, loc) =>
        //
        // Determine if there is a direct call to a Def or Sig.
        // By treating these as special cases, we can:
        // - have better error messages (knowing the precise types of the arguments, etc)
        // - have better performance (we don't generate unnecessary type variables)
        //
        val knownTarget = exp match {
          case KindedAst.Expr.Def(sym, tvar1, loc1) =>
            // Case 1: Lookup the sym and instantiate its scheme.
            val defn = root.defs(sym)
            val (tconstrs1, econstrs1, declaredType, _) = Scheme.instantiate(defn.spec.sc, loc1.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar1, constrs1, econstrs1, declaredType))

          case KindedAst.Expr.Sig(sym, tvar1, loc1) =>
            // Case 2: Lookup the sym and instantiate its scheme.
            val sig = root.traits(sym.trt).sigs(sym)
            val (tconstrs1, econstrs1, declaredType, _) = Scheme.instantiate(sig.spec.sc, loc1.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar1, constrs1, econstrs1, declaredType))

          case _ =>
            // Case 3: Unknown target.
            None
        }

        knownTarget match {
          case Some((sym, tvar1, constrs1, econstrs1, declaredType)) =>
            //
            // Special Case: We are applying a Def or Sig and we break apart its declared type.
            //
            val declaredEff = declaredType.arrowEffectType
            val declaredArgumentTypes = declaredType.arrowArgTypes
            val declaredResultType = declaredType.arrowResultType

            val (tpes, effs) = exps.map(visitExp).unzip
            c.expectTypeArguments(sym, declaredArgumentTypes, tpes, exps.map(_.loc))
            c.addClassConstraints(constrs1, loc)
            econstrs1.foreach { econstr => c.unifyType(econstr.tpe1, econstr.tpe2, loc) }
            c.unifyType(tvar1, declaredType, loc)
            c.unifyType(tvar, declaredResultType, loc)
            c.unifyType(evar, Type.mkUnion(declaredEff :: effs, loc), loc)
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)

          case None =>
            //
            // Default Case: Apply.
            //
            val lambdaBodyType = Type.freshVar(Kind.Star, loc)
            val lambdaBodyEff = Type.freshVar(Kind.Eff, loc)
            val (tpe, eff) = visitExp(exp)
            val (tpes, effs) = exps.map(visitExp).unzip
            c.expectType(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType, loc), loc)
            c.unifyType(tvar, lambdaBodyType, loc)
            c.unifyType(evar, Type.mkUnion(lambdaBodyEff :: eff :: effs, loc), loc)
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)
        }

      case Expr.Lambda(fparam, exp, loc) =>
        c.unifyType(fparam.sym.tvar, fparam.tpe, loc)
        val (tpe, eff0) = visitExp(exp)
        // Use sub-effecting for lambdas if the appropriate option is set
        val eff = if (flix.options.xsubeffecting < SubEffectLevel.Lambdas) eff0 else Type.mkUnion(eff0, Type.freshVar(Kind.Eff, loc), loc)
        val resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, tpe, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case KindedAst.Expr.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOp.BoolOp.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Bool, actual = tpe, exp.loc)
          c.unifyType(Type.Bool, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Float32Op.Neg =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Float32, actual = tpe, exp.loc)
          c.unifyType(Type.Float32, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Float64Op.Neg =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Float64, actual = tpe, exp.loc)
          c.unifyType(Type.Float64, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int8Op.Neg | SemanticOp.Int8Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Int8, actual = tpe, exp.loc)
          c.unifyType(Type.Int8, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int16Op.Neg | SemanticOp.Int16Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Int16, actual = tpe, exp.loc)
          c.unifyType(Type.Int16, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int32Op.Neg | SemanticOp.Int32Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Int32, actual = tpe, exp.loc)
          c.unifyType(Type.Int32, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)

        case SemanticOp.Int64Op.Neg | SemanticOp.Int64Op.Not =>
          val (tpe, eff) = visitExp(exp)
          c.expectType(expected = Type.Int64, actual = tpe, exp.loc)
          c.unifyType(Type.Int64, tvar, exp.loc)
          val resTpe = tvar
          val resEff = eff
          (resTpe, resEff)
      }

      case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Bool, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Bool, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Float32Op.Add | SemanticOp.Float32Op.Sub | SemanticOp.Float32Op.Mul | SemanticOp.Float32Op.Div
             | SemanticOp.Float32Op.Exp =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Float32, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Float32, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Float32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Float64Op.Add | SemanticOp.Float64Op.Sub | SemanticOp.Float64Op.Mul | SemanticOp.Float64Op.Div
             | SemanticOp.Float64Op.Exp =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Float64, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Float64, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Float64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int8Op.Add | SemanticOp.Int8Op.Sub | SemanticOp.Int8Op.Mul | SemanticOp.Int8Op.Div
             | SemanticOp.Int8Op.Rem | SemanticOp.Int8Op.Exp
             | SemanticOp.Int8Op.And | SemanticOp.Int8Op.Or | SemanticOp.Int8Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Int8, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Int8, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Int8, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int16Op.Add | SemanticOp.Int16Op.Sub | SemanticOp.Int16Op.Mul | SemanticOp.Int16Op.Div
             | SemanticOp.Int16Op.Rem | SemanticOp.Int16Op.Exp
             | SemanticOp.Int16Op.And | SemanticOp.Int16Op.Or | SemanticOp.Int16Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Int16, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Int16, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Int16, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int32Op.Add | SemanticOp.Int32Op.Sub | SemanticOp.Int32Op.Mul | SemanticOp.Int32Op.Div
             | SemanticOp.Int32Op.Rem | SemanticOp.Int32Op.Exp
             | SemanticOp.Int32Op.And | SemanticOp.Int32Op.Or | SemanticOp.Int32Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Int32, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Int32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int64Op.Add | SemanticOp.Int64Op.Sub | SemanticOp.Int64Op.Mul | SemanticOp.Int64Op.Div
             | SemanticOp.Int64Op.Rem | SemanticOp.Int64Op.Exp
             | SemanticOp.Int64Op.And | SemanticOp.Int64Op.Or | SemanticOp.Int64Op.Xor =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Int64, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Int64, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Int64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.Int8Op.Shl | SemanticOp.Int8Op.Shr
             | SemanticOp.Int16Op.Shl | SemanticOp.Int16Op.Shr
             | SemanticOp.Int32Op.Shl | SemanticOp.Int32Op.Shr
             | SemanticOp.Int64Op.Shl | SemanticOp.Int64Op.Shr =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyType(tvar, tpe1, loc)
          c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.BoolOp.Eq | SemanticOp.BoolOp.Neq
             | SemanticOp.CharOp.Eq | SemanticOp.CharOp.Neq
             | SemanticOp.Float32Op.Eq | SemanticOp.Float32Op.Neq
             | SemanticOp.Float64Op.Eq | SemanticOp.Float64Op.Neq
             | SemanticOp.Int8Op.Eq | SemanticOp.Int8Op.Neq
             | SemanticOp.Int16Op.Eq | SemanticOp.Int16Op.Neq
             | SemanticOp.Int32Op.Eq | SemanticOp.Int32Op.Neq
             | SemanticOp.Int64Op.Eq | SemanticOp.Int64Op.Neq =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyType(tpe1, tpe2, loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.CharOp.Lt | SemanticOp.CharOp.Le | SemanticOp.CharOp.Gt | SemanticOp.CharOp.Ge
             | SemanticOp.Float32Op.Lt | SemanticOp.Float32Op.Le | SemanticOp.Float32Op.Gt | SemanticOp.Float32Op.Ge
             | SemanticOp.Float64Op.Lt | SemanticOp.Float64Op.Le | SemanticOp.Float64Op.Gt | SemanticOp.Float64Op.Ge
             | SemanticOp.Int8Op.Lt | SemanticOp.Int8Op.Le | SemanticOp.Int8Op.Gt | SemanticOp.Int8Op.Ge
             | SemanticOp.Int16Op.Lt | SemanticOp.Int16Op.Le | SemanticOp.Int16Op.Gt | SemanticOp.Int16Op.Ge
             | SemanticOp.Int32Op.Lt | SemanticOp.Int32Op.Le | SemanticOp.Int32Op.Gt | SemanticOp.Int32Op.Ge
             | SemanticOp.Int64Op.Lt | SemanticOp.Int64Op.Le | SemanticOp.Int64Op.Gt | SemanticOp.Int64Op.Ge =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.unifyType(tpe1, tpe2, loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)

        case SemanticOp.StringOp.Concat =>
          val (tpe1, eff1) = visitExp(exp1)
          val (tpe2, eff2) = visitExp(exp2)
          c.expectType(expected = Type.Str, actual = tpe1, exp1.loc)
          c.expectType(expected = Type.Str, actual = tpe2, exp2.loc)
          c.unifyType(tvar, Type.Str, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(eff1, eff2, loc)
          (resTpe, resEff)
      }

      case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectType(expected = Type.Bool, actual = tpe1, exp1.loc)
        c.unifyType(tpe2, tpe3, loc)
        val resTpe = tpe3
        val resEff = Type.mkUnion(eff1, eff2, eff3, loc)
        (resTpe, resEff)

      case Expr.Stm(exp1, exp2, loc) =>
        val (_, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.Discard(exp, _) =>
        val (_, eff) = visitExp(exp)
        val resTpe = Type.Unit
        val resEff = eff
        (resTpe, resEff)

      case Expr.Let(sym, _, exp1, exp2, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        c.unifyType(sym.tvar, tpe1, exp1.loc)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.LetRec(sym, _, _, exp1, exp2, loc) =>
        // exp1 is known to be a lambda syntactically
        val (tpe1, eff1) = visitExp(exp1)
        c.unifyType(sym.tvar, tpe1, exp1.loc)
        val (tpe2, eff2) = visitExp(exp2)
        val resTpe = tpe2
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.Region(tpe, _) =>
        val resTpe = tpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Scope(sym, regionVar, exp, evar, loc) =>
        // We must visit exp INSIDE the region
        // (i.e. between `enter` and `exit`)
        // because we need to resolve local constraints
        // BEFORE purifying the region as we exit

        // TODO LEVELS this may change when we do purification properly (?)
        // We must unify sym.tvar and the region var INSIDE the region
        // because we need to ensure that reference to the region are
        // resolved BEFORE purifying the region as we exit
        c.enterRegion(regionVar.sym)
        c.unifyType(sym.tvar, Type.mkRegion(regionVar, loc), loc)
        val (tpe, eff) = visitExp(exp)
        c.exitRegion(evar, eff, loc)
        val resTpe = tpe
        val resEff = evar
        (resTpe, resEff)

      case Expr.Match(exp, rules, loc) =>
        val (tpe, eff) = visitExp(exp)
        val (patTpes, tpes, effs) = rules.map(visitMatchRule).unzip3
        c.unifyAllTypes(tpe :: patTpes, loc)
        c.unifyAllTypes(tpes, loc)
        val resTpe = tpes.headOption.getOrElse(Type.freshVar(Kind.Star, loc))
        val resEff = Type.mkUnion(eff :: effs, loc)
        (resTpe, resEff)

      case Expr.TypeMatch(exp, rules, loc) =>
        val (_, eff) = visitExp(exp)
        val (tpes, effs) = rules.map(visitTypeMatchRule).unzip
        c.unifyAllTypes(tpes, loc)
        val resTpe = tpes.headOption.getOrElse(Type.freshVar(Kind.Star, loc))
        val resEff = Type.mkUnion(eff :: effs, loc)
        (resTpe, resEff)

      case e: Expr.RestrictableChoose => RestrictableChooseConstraintGen.visitRestrictableChoose(e)

      case KindedAst.Expr.Tag(symUse, exp, tvar, loc) =>
        val decl = root.enums(symUse.sym.enumSym)
        val caze = decl.cases(symUse.sym)
        // We ignore constraints as tag schemes do not have them
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        // The tag type is a function from the type of variant to the type of the enum.
        val (tpe, eff) = visitExp(exp)
        c.unifyType(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case e: Expr.RestrictableTag => RestrictableChooseConstraintGen.visitRestrictableTag(e)

      case Expr.Tuple(elms, loc) =>
        val (elmTpes, elmEffs) = elms.map(visitExp).unzip
        val resTpe = Type.mkTuple(elmTpes, loc)
        val resEff = Type.mkUnion(elmEffs, loc)
        (resTpe, resEff)

      case Expr.RecordEmpty(loc) =>
        val resTpe = Type.mkRecord(Type.RecordRowEmpty, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.RecordSelect(exp, label, tvar, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        //       r.label : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(label, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        val (tpe, eff) = visitExp(exp)
        c.unifyType(tpe, expectedRecordType, loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case Expr.RecordExtend(label, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label  :: tpe | r }
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.unifyType(tpe2, Type.mkRecord(freshRowVar, loc), loc)
        c.unifyType(tvar, Type.mkRecord(Type.mkRecordRowExtend(label, tpe1, freshRowVar, loc), loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(eff1, eff2, loc)
        (resTpe, resEff)

      case Expr.RecordRestrict(label, exp, tvar, loc) =>
        //
        //  exp : { label  :: t | r }
        // -------------------------
        // { -label | exp } : {| r }
        //
        val freshLabelType = Type.freshVar(Kind.Star, loc)
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val (tpe, eff) = visitExp(exp)
        c.unifyType(tpe, Type.mkRecord(Type.mkRecordRowExtend(label, freshLabelType, freshRowVar, loc), loc), loc)
        c.unifyType(tvar, Type.mkRecord(freshRowVar, loc), loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case Expr.ArrayLit(exps, exp, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpes, effs) = exps.map(visitExp).unzip
        val (tpe, eff) = visitExp(exp)
        c.expectType(expected = regionType, actual = tpe, exp.loc)
        c.unifyAllTypes(tpes, loc)
        val elmTpe = tpes.headOption.getOrElse(Type.freshVar(Kind.Star, loc))
        c.unifyType(tvar, Type.mkArray(elmTpe, regionVar, loc), loc)
        c.unifyType(evar, Type.mkUnion(Type.mkUnion(effs, loc), eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectType(expected = regionType, actual = tpe1, loc)
        c.expectType(expected = Type.Int32, actual = tpe3, exp3.loc)
        c.unifyType(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
        c.unifyType(evar, Type.mkUnion(eff1, eff2, eff3, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectType(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
        c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.unifyType(evar, Type.mkUnion(regionVar, eff1, eff2, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayStore(exp1, exp2, exp3, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        val (tpe3, eff3) = visitExp(exp3)
        c.expectType(expected = arrayType, actual = tpe1, exp1.loc)
        c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.expectType(expected = elmVar, actual = tpe3, exp3.loc)
        c.unifyType(evar, Type.mkUnion(regionVar, eff1, eff2, eff3, loc), loc)
        val resTpe = Type.Unit
        val resEff = evar
        (resTpe, resEff)

      case Expr.ArrayLength(exp, evar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectType(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
        c.unifyType(evar, Type.mkUnion(regionVar, eff, loc), loc)
        val resTpe = Type.Int32
        val resEff = evar
        (resTpe, resEff)

      case Expr.StructNew(sym, fields, region, tvar, evar, loc) =>
        // This case needs to handle expressions like `new S { f = rhs } @ r` where `f` was not present in the struct declaration
        // Here, we check that `rhs` is itself valid by visiting it but make sure not to unify it with anything
        val (instantiatedFieldTpes, structTpe, regionVar) = instantiateStruct(sym, root.structs)
        val visitedFields = fields.map { case (k, v) => visitExp(v) }
        val (regionTpe, regionEff) = visitExp(region)
        val (fieldTpes, fieldEffs) = visitedFields.unzip
        c.unifyType(tvar, structTpe, loc)
        for {
          ((fieldSym, expr), fieldTpe1) <- fields.zip(fieldTpes)
        } {
          instantiatedFieldTpes.get(fieldSym.sym) match {
            case None => () // if not an actual field, there is nothing to unify
            case Some(fieldTpe2) => c.unifyType(fieldTpe1, fieldTpe2, expr.loc)
          }
        }
        c.unifyType(Type.mkRegion(regionVar, loc), regionTpe, region.loc)
        c.unifyType(evar, Type.mkUnion(fieldEffs :+ regionEff :+ regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.StructGet(exp, field, tvar, evar, loc) =>
        val (instantiatedFieldTpes, structTpe, regionVar) = instantiateStruct(field.structSym, root.structs)
        val (tpe, eff) = visitExp(exp)
        c.expectType(structTpe, tpe, exp.loc)
        val fieldTpe = instantiatedFieldTpes(field)
        c.unifyType(fieldTpe, tvar, loc)
        c.unifyType(Type.mkUnion(eff, regionVar, loc), evar, exp.loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.StructPut(exp1, field, exp2, tvar, evar, loc) =>
        val (instantiatedFieldTpes, structTpe, regionVar) = instantiateStruct(field.structSym, root.structs)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectType(structTpe, tpe1, exp1.loc)
        val fieldTpe = instantiatedFieldTpes(field)
        c.expectType(fieldTpe, tpe2, exp2.loc)
        c.unifyType(Type.mkUnit(loc), tvar, loc)
        c.unifyType(Type.mkUnion(eff1, eff2, regionVar, loc), evar, loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.VectorLit(exps, tvar, evar, loc) =>
        val (tpes, effs) = exps.map(visitExp).unzip
        c.unifyAllTypes(tpes, loc)
        val tpe = tpes.headOption.getOrElse(Type.freshVar(Kind.Star, loc))
        c.unifyType(tvar, Type.mkVector(tpe, loc), loc)
        c.unifyType(evar, Type.mkUnion(effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.VectorLoad(exp1, exp2, tvar, evar, loc) =>
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectType(expected = Type.mkVector(tvar, loc), actual = tpe1, exp1.loc)
        c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.unifyType(evar, Type.mkUnion(eff1, eff2, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.VectorLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectType(Type.mkVector(elmVar, loc), tpe, exp.loc)
        val resTpe = Type.Int32
        val resEff = eff
        (resTpe, resEff)

      case Expr.Ascribe(exp, expectedTpe, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        val (actualTpe, actualEff) = visitExp(exp)
        expectedTpe.foreach { tpe => c.expectType(expected = tpe, actual = actualTpe, loc) }
        c.unifyType(actualTpe, tvar, loc)
        expectedEff.foreach { eff => c.expectType(expected = eff, actual = actualEff, loc) }
        val resTpe = tvar
        val resEff = actualEff
        (resTpe, resEff)

      case Expr.InstanceOf(exp, _, _) =>
        val (_, eff) = visitExp(exp)
        val resTpe = Type.Bool
        val resEff = eff
        (resTpe, resEff)

      case Expr.CheckedCast(cast, exp, tvar, evar, loc) =>
        // A cast expression is sound; the type system ensures the declared type is correct.
        cast match {
          case Ast.CheckedCastType.TypeCast =>
            // We replace the type with a fresh variable to allow any type.
            // The validity of this cast is checked in the Safety phase.
            val (_, eff) = visitExp(exp)
            c.unifyType(evar, eff, loc)
            val resTpe = tvar
            val resEff = evar
            (resTpe, resEff)

          case Ast.CheckedCastType.EffectCast =>
            // We union the effect with a fresh variable to allow unifying with a "larger" effect.
            val (tpe, eff) = visitExp(exp)
            c.unifyType(tvar, tpe, loc)
            val resTpe = tvar
            val resEff = Type.mkUnion(eff, evar, loc)
            (resTpe, resEff)
        }

      case Expr.UncheckedCast(exp, declaredTpe, declaredEff, tvar, loc) =>
        // An unchecked cast expression is unsound; the type system assumes the declared type and effect are correct.
        val (actualTyp, actualEff) = visitExp(exp)
        c.unifyType(tvar, declaredTpe.getOrElse(actualTyp), loc)
        val resTpe = tvar
        val resEff = declaredEff.getOrElse(actualEff)
        (resTpe, resEff)

      case Expr.UncheckedMaskingCast(exp, _) =>
        // A masking cast expression is unsound; the type system assumes the declared type and effect are correct.
        // The expression is treated as impure later in the compiler to prevent erasure in optimizations.
        val (tpe, _) = visitExp(exp)
        val resTpe = tpe
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Without(exp, _, _) =>
        // We ignore the `without` here.
        // TODO EFF-MIGRATION Use set subtraction when we have set effects.
        val (tpe, eff) = visitExp(exp)
        val resTpe = tpe
        val resEff = eff
        (resTpe, resEff)

      case Expr.TryCatch(exp, rules, loc) =>
        val (tpe, eff) = visitExp(exp)
        val (tpes, effs) = rules.map(visitCatchRule).unzip
        c.unifyAllTypes(tpes, loc)
        val ruleTpe = tpes.headOption.getOrElse(Type.freshVar(Kind.Star, loc))
        c.unifyType(tpe, ruleTpe, loc)
        val resTpe = tpe
        val resEff = Type.mkUnion(eff :: effs, loc)
        (resTpe, resEff)

      case KindedAst.Expr.Throw(exp, tvar, evar, loc) =>
        val (_, eff) = visitExp(exp)
        c.unifyType(evar, Type.mkUnion(eff, Type.IO, loc), loc)
        val resultTpe = tvar
        val resultEff = evar
        (resultTpe, resultEff)

      case Expr.TryWith(exp, effUse, rules, tvar, loc) =>
        val (tpe, eff) = visitExp(exp)
        val continuationEffect = Type.freshVar(Kind.Eff, loc)
        val (tpes, effs) = rules.map(visitHandlerRule(_, tpe, continuationEffect, loc)).unzip
        c.unifyAllTypes(tpe :: tvar :: tpes, loc)


        // TODO ASSOC-TYPES The types used here are not correct.
        // TODO ASSOC-TYPES We should use set subtraction instead.
        // We subtract the handled effect from the body
        // Note: Does not work for polymorphic effects.
        val correctedBodyEff = c.purifyEff(effUse.sym, eff)

        // The continuation effect is the effect of all the rule bodies, plus the effect of the try-body
        c.unifyType(continuationEffect, Type.mkUnion(effs, loc), loc) // TODO temp simplification: ignoring try-body
        val resultTpe = tpe

        // TODO ASSOC-TYPES should be continuationEffect
        val resultEff = Type.mkUnion(effs, loc) // TODO temp simplification
        (resultTpe, resultEff)

      case Expr.Do(opUse, exps, tvar, loc) =>
        val op = lookupOp(opUse.sym, opUse.loc)
        val effTpe = Type.Cst(TypeConstructor.Effect(opUse.sym.eff), loc)

        // length check done in Resolver
        val effs = visitOpArgs(op, exps)

        // specialize the return type of the op if needed
        val opTpe = getDoType(op)

        c.unifyType(opTpe, tvar, loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(effTpe :: op.spec.eff :: effs, loc)

        (resTpe, resEff)

      case Expr.InvokeConstructor2(clazz, exps, jvar, evar, loc) =>
        // Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmConstructor(k, eᵢ ...)
        // --------------------------------------------------------
        // Γ ⊢ new k(e₁ ...) : k
        val clazzTpe = Type.getFlixType(clazz)
        val (tpes, effs) = exps.map(visitExp).unzip
        val baseEffs = BaseEffects.of(clazz, loc)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmConstructor(clazz, tpes), loc), loc) // unify constructor
        c.unifyType(evar, Type.mkUnion(Type.IO :: baseEffs :: effs, loc), loc) // unify effects
        val resTpe = clazzTpe
        val resEff = evar
        (resTpe, resEff)

      case Expr.InvokeMethod2(exp, methodName, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ e : τ    Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmMethod(τ, m, τᵢ ...)
        // ---------------------------------------------------------------
        // Γ ⊢ e.m(eᵢ ...) : JvmToType[ι]
        val (tpe, eff) = visitExp(exp)
        val (tpes, effs) = exps.map(visitExp).unzip
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmMethod(tpe, methodName, tpes), loc), loc) // unify method type
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc) // unify method return
        c.unifyType(evar, Type.mkUnion(Type.IO :: eff :: effs, loc), loc) // unify effects
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.InvokeStaticMethod2(clazz, methodName, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmStaticMethod(m, τᵢ ...)
        // ---------------------------------------------------------------
        // Γ ⊢ m(eᵢ ...) : JvmToType[ι]
        val (tpes, effs) = exps.map(visitExp).unzip
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmStaticMethod(clazz, methodName, tpes), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc)
        c.unifyType(evar, Type.mkUnion(Type.IO :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.GetField2(exp, fieldName, jvar, tvar, evar, loc) =>
        // Γ ⊢ e : τ    Γ ⊢ ι ~ JvmFieldMethod(τ, m)
        // ---------------------------------------------------------------
        // Γ ⊢ e.f : JvmToType[ι]
        val (tpe, eff) = visitExp(exp)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmField(tpe, fieldName), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc) // unify field type
        c.unifyType(evar, Type.mkUnion(Type.IO :: eff :: Nil, loc), loc) // unify effects
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.InvokeConstructorOld(constructor, exps, _) =>
        val classTpe = Type.getFlixType(constructor.getDeclaringClass)
        val (_, _) = exps.map(visitExp).unzip
        val resTpe = classTpe
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.InvokeMethodOld(method, clazz, exp, exps, loc) =>
        val classTpe = Type.getFlixType(clazz)
        val (thisTpe, _) = visitExp(exp)
        c.unifyType(thisTpe, classTpe, loc)
        val (_, _) = exps.map(visitExp).unzip
        val resTpe = Type.getFlixType(method.getReturnType)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.InvokeStaticMethodOld(method, exps, _) =>
        val (_, _) = exps.map(visitExp).unzip
        val resTpe = Type.getFlixType(method.getReturnType)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.GetFieldOld(field, clazz, exp, _) =>
        val classType = Type.getFlixType(clazz)
        val fieldType = Type.getFlixType(field.getType)
        val (tpe, _) = visitExp(exp)
        c.expectType(expected = classType, actual = tpe, exp.loc)
        val resTpe = fieldType
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.PutField(field, clazz, exp1, exp2, _) =>
        val fieldType = Type.getFlixType(field.getType)
        val classType = Type.getFlixType(clazz)
        val (tpe1, _) = visitExp(exp1)
        val (tpe2, _) = visitExp(exp2)
        c.expectType(expected = classType, actual = tpe1, exp1.loc)
        c.expectType(expected = fieldType, actual = tpe2, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.GetStaticField(field, _) =>
        val fieldType = Type.getFlixType(field.getType)
        val resTpe = fieldType
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.PutStaticField(field, exp, _) =>
        val (valueTyp, _) = visitExp(exp)
        c.expectType(expected = Type.getFlixType(field.getType), actual = valueTyp, exp.loc)
        val resTpe = Type.Unit
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.NewObject(_, clazz, methods, _) =>
        methods.foreach(visitJvmMethod)
        val resTpe = Type.getFlixType(clazz)
        val resEff = Type.IO
        (resTpe, resEff)

      case Expr.NewChannel(exp1, exp2, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectType(expected = regionType, actual = tpe1, exp1.loc)
        c.expectType(expected = Type.Int32, actual = tpe2, exp2.loc)
        c.unifyType(evar, regionVar, loc)
        // TODO unify tvar with return type?
        val resTpe = tvar
        val resEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        (resTpe, resEff)

      case Expr.GetChannel(exp, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmTpe = Type.freshVar(Kind.Star, loc)
        val receiverTpe = Type.mkReceiver(elmTpe, regionVar, loc)
        val (tpe, eff) = visitExp(exp)
        c.expectType(expected = receiverTpe, actual = tpe, exp.loc)
        c.unifyType(tvar, elmTpe, loc)
        c.unifyType(evar, Type.mkUnion(eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.PutChannel(exp1, exp2, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmTpe = Type.freshVar(Kind.Star, loc)
        val senderTpe = Type.mkSender(elmTpe, regionVar, loc)
        val (tpe1, eff1) = visitExp(exp1)
        val (tpe2, eff2) = visitExp(exp2)
        c.expectType(expected = senderTpe, actual = tpe1, exp1.loc)
        c.expectType(expected = elmTpe, actual = tpe2, exp2.loc)
        c.unifyType(evar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        val resTpe = Type.mkUnit(loc)
        val resEff = evar
        (resTpe, resEff)

      case Expr.SelectChannel(rules, default, tvar, evar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val (ruleTypes, ruleEffs) = rules.map(visitSelectRule(_, regionVar)).unzip
        val (defaultType, eff2) = visitDefaultRule(default, loc)
        c.unifyAllTypes(tvar :: defaultType :: ruleTypes, loc)
        c.unifyType(evar, Type.mkUnion(regionVar :: eff2 :: ruleEffs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)

      case Expr.Spawn(exp1, exp2, loc) =>
        // TODO it is unclear what the type rules of spawn should be
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val (_, _) = visitExp(exp1)
        val (tpe2, _) = visitExp(exp2)
        c.expectType(expected = regionType, actual = tpe2, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.mkUnion(Type.IO, regionVar, loc)
        (resTpe, resEff)

      case Expr.ParYield(frags, exp, _) =>
        // We don't need to keep the types of the fragments
        // since they are all bound to the patterns

        // The result effect is only the effect of exp
        // because the fragments must all be pure.
        frags.foreach(visitParYieldFragment)
        val (tpe, eff) = visitExp(exp)
        val resTpe = tpe
        val resEff = eff
        (resTpe, resEff)

      case Expr.Lazy(exp, loc) =>
        val (tpe, eff) = visitExp(exp)
        c.expectType(expected = Type.Pure, actual = eff, exp.loc)
        val resTpe = Type.mkLazy(tpe, loc)
        val resEff = Type.Pure
        (resTpe, resEff)

      case Expr.Force(exp, tvar, loc) =>
        val (tpe, eff) = visitExp(exp)
        c.expectType(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
        val resTpe = tvar
        val resEff = eff
        (resTpe, resEff)

      case e: Expr.FixpointConstraintSet => SchemaConstraintGen.visitFixpointConstraintSet(e)
      case e: Expr.FixpointLambda => SchemaConstraintGen.visitFixpointLambda(e)
      case e: Expr.FixpointMerge => SchemaConstraintGen.visitFixpointMerge(e)
      case e: Expr.FixpointSolve => SchemaConstraintGen.visitFixpointSolve(e)
      case e: Expr.FixpointFilter => SchemaConstraintGen.visitFixpointFilter(e)
      case e: Expr.FixpointInject => SchemaConstraintGen.visitFixpointInject(e)
      case e: Expr.FixpointProject => SchemaConstraintGen.visitFixpointProject(e)

      case Expr.Error(_, tvar, evar) =>
        // The error expression has whatever type and effect it needs to have.
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)
    }
  }

  /**
    * Generates constraints for the pattern.
    *
    * Returns the pattern's type. The type may be a variable which must later be resolved.
    */
  def visitPattern(pat0: KindedAst.Pattern)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope
    pat0 match {
      case KindedAst.Pattern.Wild(tvar, _) => tvar

      case KindedAst.Pattern.Var(sym, tvar, loc) =>
        c.unifyType(sym.tvar, tvar, loc)
        tvar

      case KindedAst.Pattern.Cst(cst, _) => Type.constantType(cst)

      case KindedAst.Pattern.Tag(symUse, pat, tvar, loc) =>
        val decl = root.enums(symUse.sym.enumSym)
        val caze = decl.cases(symUse.sym)
        // We ignore constraints as tag schemes do not have them
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        // The tag type is a function from the type of variant to the type of the enum.
        val tpe = visitPattern(pat)
        c.unifyType(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
        tvar


      case KindedAst.Pattern.Tuple(elms, loc) =>
        val tpes = elms.map(visitPattern)
        Type.mkTuple(tpes, loc)

      case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc.asSynthetic)
        val freshRecord = Type.mkRecord(freshRowVar, loc.asSynthetic)

        val tailTpe = visitPattern(pat)
        c.unifyType(freshRecord, tailTpe, loc.asSynthetic)
        val patTpes = pats.map(visitRecordLabelPattern)
        val resTpe = mkRecordType(patTpes, freshRowVar, loc)
        c.unifyType(resTpe, tvar, loc)
        resTpe

      case KindedAst.Pattern.RecordEmpty(loc) => Type.mkRecord(Type.RecordRowEmpty, loc)

      case KindedAst.Pattern.Error(tvar, _) => tvar

    }
  }

  /**
    * Generates constraints for the patterns inside the record label pattern.
    *
    * Returns the label, pattern type, and location of the pattern.
    */
  private def visitRecordLabelPattern(pat: KindedAst.Pattern.Record.RecordLabelPattern)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Name.Label, Type, SourceLocation) = pat match {
    case KindedAst.Pattern.Record.RecordLabelPattern(label, tvar, p, loc) =>
      // { Label = Pattern ... }
      val tpe = visitPattern(p)
      c.unifyType(tpe, tvar, loc)
      (label, tpe, loc)
  }

  /**
    * Generates constraints for the given match rule.
    *
    * Returns the pattern type, the body's type, and the body's effect
    */
  private def visitMatchRule(rule: KindedAst.MatchRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type, Type) = rule match {
    case KindedAst.MatchRule(pat, guard, exp) =>
      val patTpe = visitPattern(pat)
      guard.foreach {
        g =>
          val (guardTpe, guardEff) = visitExp(g)
          c.expectType(expected = Type.Bool, actual = guardTpe, g.loc)
          c.expectType(expected = Type.Pure, actual = guardEff, g.loc)
      }
      val (tpe, eff) = visitExp(exp)
      (patTpe, tpe, eff)
  }

  /**
    * Generates constraints for the given typematch rule.
    *
    * Returns the the body's type and the body's effect
    */
  private def visitTypeMatchRule(rule: KindedAst.TypeMatchRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = rule match {
    case KindedAst.TypeMatchRule(sym, declTpe, exp) =>
      // We mark all the type vars in the declared type as rigid.
      // This ensures we get a substitution from the actual type to the declared type.
      // This marking only really affects wildcards,
      // as non-wildcard variables must come from the function signature
      // and are therefore already rigid.
      declTpe.typeVars.map(_.sym).foreach(c.rigidify)

      // Unify the variable's type with the declared type
      c.unifyType(sym.tvar, declTpe, sym.loc)

      visitExp(exp)
  }

  /**
    * Generates constraints for the given catch rule.
    *
    * Returns the the body's type and the body's effect
    */
  private def visitCatchRule(rule: KindedAst.CatchRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = rule match {
    case KindedAst.CatchRule(sym, clazz, exp) =>
      c.expectType(expected = Type.mkNative(clazz, sym.loc), sym.tvar, sym.loc)
      visitExp(exp)
  }

  /**
    * Generates constraints unifying the given expected and actual formal parameters.
    */
  private def unifyFormalParams(op: Symbol.OpSym, expected: List[KindedAst.FormalParam], actual: List[KindedAst.FormalParam], loc: SourceLocation)(implicit c: TypeContext, flix: Flix): Unit = {
    // length check done in Resolver
    c.expectTypeArguments(op, expectedTypes = expected.map(_.tpe), actualTypes = actual.map(_.tpe), actual.map(_.loc))
  }

  /**
    * Generates constraints for the given handler rule.
    *
    * Returns the body's type and the body's effect.
    *
    * @param tryBlockTpe        the type of the try-block associated with the handler
    * @param continuationEffect the effect of the continuation
    */
  private def visitHandlerRule(rule: KindedAst.HandlerRule, tryBlockTpe: Type, continuationEffect: Type, loc: SourceLocation)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = rule match {
    case KindedAst.HandlerRule(op, actualFparams0, body, opTvar) =>
      val effect = root.effects(op.sym.eff)
      val ops = effect.ops.map(op => op.sym -> op).toMap
      // Don't need to generalize since ops are monomorphic
      // Don't need to handle unknown op because resolver would have caught this
      val (actualFparams, List(resumptionFparam)) = actualFparams0.splitAt(actualFparams0.length - 1)
      ops(op.sym) match {
        case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, _, _, _, _)) =>
          val resumptionArgType = opTpe
          val resumptionResType = tryBlockTpe
          val resumptionEff = continuationEffect
          val expectedResumptionType = Type.mkArrowWithEffect(resumptionArgType, resumptionEff, resumptionResType, loc.asSynthetic)
          unifyFormalParams(op.sym, expected = expectedFparams, actual = actualFparams, op.loc)
          c.expectType(expected = expectedResumptionType, actual = resumptionFparam.tpe, resumptionFparam.loc)
          val (actualTpe, actualEff) = visitExp(body)

          // unify the operation return type with its tvar
          c.unifyType(actualTpe, opTvar, body.loc)

          (actualTpe, actualEff)
      }
  }

  /**
    * Generates constraints for the JVM method.
    */
  private def visitJvmMethod(method: KindedAst.JvmMethod)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Unit = method match {
    case KindedAst.JvmMethod(_, fparams, exp, returnTpe, eff, _) =>

      /**
        * Constrains the given formal parameter to its declared type.
        */
      def visitFormalParam(fparam: KindedAst.FormalParam): Unit = fparam match {
        case KindedAst.FormalParam(sym, _, tpe, _, loc) =>
          c.unifyType(sym.tvar, tpe, loc)
      }

      fparams.foreach(visitFormalParam)
      val (bodyTpe, bodyEff) = visitExp(exp)
      c.expectType(expected = returnTpe, actual = bodyTpe, exp.loc)
      c.expectType(expected = eff, actual = bodyEff, exp.loc)
  }

  /**
    * Generates constraints for the SelectChannelRule.
    *
    * Returns the type and effect of the rule.
    */
  private def visitSelectRule(sr0: KindedAst.SelectChannelRule, regionVar: Type)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    sr0 match {
      case KindedAst.SelectChannelRule(sym, chan, body) =>
        val (chanType, eff1) = visitExp(chan)
        val (bodyType, eff2) = visitExp(body)
        c.unifyType(chanType, Type.mkReceiver(sym.tvar, regionVar, sym.loc), sym.loc)
        val resTpe = bodyType
        val resEff = Type.mkUnion(eff1, eff2, regionVar, body.loc)
        (resTpe, resEff)
    }
  }

  /**
    * Generates constraints for the default rule.
    *
    * Returns the type and effect of the rule body.
    */
  private def visitDefaultRule(exp0: Option[KindedAst.Expr], loc: SourceLocation)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    exp0 match {
      case None => (Type.freshVar(Kind.Star, loc), Type.Pure)
      case Some(exp) => visitExp(exp)
    }
  }

  /**
    * Generates constraints unifying each argument's type with the corresponding parameter of the operation.
    *
    * The number of arguments must match the number of parameters (this check is done in Resolver).
    */
  private def visitOpArgs(op: KindedAst.Op, args: List[KindedAst.Expr])(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): List[Type] = {
    (args zip op.spec.fparams) map {
      case (arg, fparam) => visitOpArg(arg, fparam)
    }
  }

  /**
    * Generates constraints unifying the given argument's type with the formal parameter's type.
    *
    * Returns the effect of the argument.
    */
  private def visitOpArg(arg: KindedAst.Expr, fparam: KindedAst.FormalParam)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Type = {
    val (tpe, eff) = visitExp(arg)
    c.expectType(expected = fparam.tpe, actual = tpe, arg.loc)
    eff
  }

  /**
    * Generates constraints for the given ParYieldFragment.
    */
  private def visitParYieldFragment(frag: KindedAst.ParYieldFragment)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): Unit = frag match {
    case KindedAst.ParYieldFragment(pat, exp, loc) =>
      val patTpe = visitPattern(pat)
      val (tpe, eff) = visitExp(exp)
      c.unifyType(patTpe, tpe, loc)
      c.expectType(expected = Type.Pure, actual = eff, exp.loc)
  }

  /**
    * Builds a record type from the given fields and rest type.
    *
    * The rest type represents the "tail" of the record.
    */
  private def mkRecordType(patTypes: List[(Name.Label, Type, SourceLocation)], rest: Type, loc: SourceLocation): Type = {
    val ps = patTypes.foldRight(rest) {
      case ((lbl, t, l), acc) => Type.mkRecordRowExtend(
        lbl, t, acc, l)
    }
    Type.mkRecord(ps, loc)
  }

  /**
    * Returns the operation corresponding to the given symbol.
    */
  private def lookupOp(sym: Symbol.OpSym, loc: SourceLocation)(implicit root: KindedAst.Root): KindedAst.Op = {
    val eff = root.effects(sym.eff)
    eff.ops.find(_.sym == sym)
      .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $sym in effect ${sym.eff}", loc))
  }

  /**
    * Returns the type inferred for `do`ing the given op.
    *
    * This is usually the annotated return type of the op.
    * But if the op returns Void, we return a free variable instead.
    */
  private def getDoType(op: KindedAst.Op)(implicit c: TypeContext, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope
    // We special-case the result type of the operation.
    op.spec.tpe.typeConstructor match {
      case Some(TypeConstructor.Void) =>
        // The operation type is `Void`. Flix does not have subtyping, but here we want something close to it.
        // Hence we treat `Void` as a fresh type variable.
        // An alternative would be to allow empty pattern matches, but that is cumbersome.
        Type.freshVar(Kind.Star, op.spec.tpe.loc, isRegion = false, Ast.VarText.Absent)
      case _ => op.spec.tpe
    }
  }

  /**
    * Instantiates the scheme of the struct in corresponding to `sym` in `structs`
    * Returns a map from field name to its instantiated type, the type of the instantiated struct, and the instantiated struct's region variable
    *
    * For example, for the struct `struct S [v, r] { a: v, b: Int32 }` where `v` instantiates to `v'` and `r` instantiates to `r'`
    *   The first element of the return tuple would be a map with entries `a -> v'` and `b -> Int32`
    *   The second element of the return tuple would be(locations omitted) `Apply(Apply(Cst(Struct(S)), v'), r')`
    *   The third element of the return tuple would be `r'`
    */
  def instantiateStruct(sym: Symbol.StructSym, structs: Map[Symbol.StructSym, KindedAst.Struct])(implicit c: TypeContext, flix: Flix) : (Map[Symbol.StructFieldSym, Type], Type, Type.Var) = {
    implicit val scope: Scope = c.getScope
    val struct = structs(sym)
    val fields = struct.fields
    val (_, _, tpe, substMap) = Scheme.instantiate(struct.sc, struct.loc)
    val subst = Substitution(substMap)
    val instantiatedFields = fields.map(f => f match {
      case KindedAst.StructField(_, fieldSym, tpe, _) =>
        fieldSym -> subst(tpe)
    })
    (instantiatedFields.toMap, tpe, substMap(struct.tparams.last.sym))
  }
}
