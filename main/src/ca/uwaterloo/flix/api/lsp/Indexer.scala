/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.Index.traverse
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._

object Indexer {

  /**
    * Returns a reverse index for the given AST `root`.
    */
  def visitRoot(root: Root): Index = {
    Index.all(
      traverse(root.defs.values)(visitDef),
      traverse(root.enums.values)(visitEnum),
      traverse(root.structs.values)(visitStruct),
      traverse(root.traits.values)(visitTrait),
      traverse(root.instances.values) {
        instances => traverse(instances)(visitInstance)
      },
      traverse(root.typeAliases.values)(visitTypeAlias),
      traverse(root.effects.values)(visitEff),
    )
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  private def visitDef(def0: Def): Index = def0 match {
    case Def(_, spec, exp) =>
      Index.all(
        Index.occurrenceOf(def0),
        visitSpec(spec),
        visitExp(exp),
      )
  }

  /**
    * Returns a reverse index for the given signature `sig0`.
    */
  private def visitSig(sig0: Sig): Index = sig0 match {
    case Sig(_, spec, exp) =>
      Index.all(
        Index.occurrenceOf(sig0),
        visitSpec(spec),
        traverse(exp)(visitExp),
      )
  }

  /**
    * Returns a reverse index for the given `spec`.
    */
  private def visitSpec(spec: Spec): Index = spec match {
    case Spec(_, _, _, tparams, fparams, _, retTpe, eff, tconstrs, econstrs, _) =>
      Index.all(
        traverse(tparams)(visitTypeParam),
        traverse(fparams)(visitFormalParam),
        traverse(tconstrs)(visitTraitConstraint),
        traverse(econstrs)(visitEqualityConstraint),
        visitType(retTpe),
        visitType(eff),
      )
  }

  /**
    * Returns a reverse index for the given enum `enum0`.
    */
  private def visitEnum(enum0: Enum): Index = enum0 match {
    case Enum(_, _, _, _, tparams, derives, cases, _, _) =>
      Index.all(
        Index.occurrenceOf(enum0),
        traverse(tparams)(visitTypeParam),
        traverse(derives.traits) {
          case Ast.Derivation(trt, loc) => Index.useOf(trt, loc)
        },
        traverse(cases.values)(visitCase),
      )
  }

  /**
    * Returns a reverse index for the given enum case `caze0`.
    */
  private def visitCase(caze0: Case): Index = caze0 match {
    case Case(_, tpe, _, _) =>
      Index.occurrenceOf(caze0) ++ visitType(tpe)
  }

  /**
   * Returns a reverse index for the given struct `struct0`.
   */
  private def visitStruct(struct0: Struct): Index = struct0 match {
    case Struct(doc, ann, mod, sym, tparams, sc, fields, loc) =>
      Index.all(
        Index.occurrenceOf(struct0),
        traverse(tparams)(visitTypeParam),
        traverse(fields.values) {
          case f@StructField(sym, tpe, loc) => Index.occurrenceOf(f) ++ visitType(tpe)
        },
      )
  }

  /**
    * Returns a reverse index for the given trait `trait0`.
    */
  private def visitTrait(trait0: TypedAst.Trait): Index = trait0 match {
    case Trait(doc, ann, mod, sym, tparam, superTraits, assocs, signatures, laws, loc) =>
      Index.all(
        Index.occurrenceOf(trait0),
        visitTypeParam(tparam),
        traverse(superTraits)(visitTraitConstraint),
        traverse(assocs)(visitAssocTypeSig),
        traverse(signatures)(visitSig),
        //        laws.map(visitDef) // TODO visit laws?
      )
  }

  /**
    * Returns a reverse index for the given instance `instance0`.
    */
  private def visitInstance(instance0: Instance): Index = instance0 match {
    case Instance(_, _, _, trt, tpe, tconstrs, assocs, defs, _, _) =>
      Index.all(
        Index.useOf(trt.sym, trt.loc),
        visitType(tpe),
        traverse(tconstrs)(visitTraitConstraint),
        traverse(assocs)(visitAssocTypeDef),
        traverse(defs)(visitDef),
      )
  }

  /**
    * Returns a reverse index for the given type alias `alias0`.
    */
  private def visitTypeAlias(alias0: TypeAlias): Index = alias0 match {
    case TypeAlias(_, _, _, _, tparams, tpe, _) =>
      Index.all(
        Index.occurrenceOf(alias0),
        traverse(tparams)(visitTypeParam),
        visitType(tpe),
      )
  }


  /**
    * Returns a reverse index for the given associated type definition `assoc`.
    */
  private def visitAssocTypeDef(assoc: AssocTypeDef): Index = assoc match {
    case AssocTypeDef(_, _, Ast.AssocTypeSymUse(sym, loc), arg, tpe, _) =>
      Index.all(
        Index.useOf(sym, loc),
        visitType(arg),
        visitType(tpe),
      )
  }

  /**
    * Returns a reverse index for the given associated type signature `assoc`.
    */
  private def visitAssocTypeSig(assoc: AssocTypeSig): Index = assoc match {
    case AssocTypeSig(_, _, _, tparam, _, tpe, _) =>
      Index.all(
        Index.occurrenceOf(assoc),
        visitTypeParam(tparam),
        Index.traverse(tpe)(visitType)
      )
  }

  /**
    * Returns a reverse index for the given effect `eff0`
    */
  private def visitEff(eff0: Effect): Index = eff0 match {
    case Effect(_, _, _, _, ops, _) =>
      Index.all(
        Index.occurrenceOf(eff0),
        traverse(ops)(visitOp),
      )
  }

  /**
    * Returns a reverse index for the given effect operation `op0`
    */
  private def visitOp(op0: Op): Index = op0 match {
    case Op(_, spec) =>
      Index.all(
        Index.occurrenceOf(op0),
        visitSpec(spec),
      )
  }

  /**
    * Returns a reverse index for the given expression `exp0`.
    */
  private def visitExp(exp0: Expr): Index = exp0 match {
    case Expr.Cst(_, _, _) =>
      Index.occurrenceOf(exp0)

    case Expr.Var(sym, _, loc) =>
      val parent = Entity.Exp(exp0)
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc, parent)

    case Expr.Def(sym, _, loc) =>
      val parent = Entity.Exp(exp0)
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc, parent)

    case Expr.Sig(sym, _, loc) =>
      val parent = Entity.Exp(exp0)
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc, parent) ++ Index.useOf(sym.trt, loc)

    case Expr.Hole(_, _, _) =>
      Index.occurrenceOf(exp0)

    case Expr.HoleWithExp(exp, _, _, _) =>
      Index.occurrenceOf(exp0) ++ visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) => // TODO RESTR-VARS sym
      Index.occurrenceOf(exp0) ++ visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      Index.occurrenceOf(exp0) ++ visitExp(exp) // TODO NS-REFACTOR add use of sym

    case Expr.Lambda(fparam, exp, _, _) =>
      visitFormalParam(fparam) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Apply(exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.Let(sym, _, exp1, exp2, _, _, _) =>
      Index.occurrenceOf(sym, exp1.tpe) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.LetRec(sym, _, _, exp1, exp2, _, _, _) =>
      Index.occurrenceOf(sym, exp1.tpe) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.Region(_, _) =>
      Index.occurrenceOf(exp0)

    case Expr.Scope(sym, _, exp, _, _, loc) =>
      val tpe = Type.mkRegion(sym.tvar, loc)
      Index.occurrenceOf(sym, tpe) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Match(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(rules) {
        case MatchRule(pat, guard, exp) => visitPat(pat) ++ Index.traverse(guard)(visitExp) ++ visitExp(exp)
      }
      i0 ++ i1

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(rules) {
        case TypeMatchRule(sym, tpe, exp) => Index.occurrenceOf(sym, tpe) ++ visitType(tpe) ++ visitExp(exp)
      }
      i0 ++ i1

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ traverse(rules) {
        case RestrictableChooseRule(_, body) => visitExp(body)
      } ++ Index.occurrenceOf(exp0)

    case Expr.Tag(Ast.CaseSymUse(sym, loc), exp, _, _, _) =>
      val parent = Entity.Exp(exp0)
      visitExp(exp) ++ Index.useOf(sym, loc, parent) ++ Index.occurrenceOf(exp0)

    case Expr.RestrictableTag(Ast.RestrictableCaseSymUse(sym, loc), exp, _, _, _) =>
      val parent = Entity.Exp(exp0)
      // TODO RESTR-VARS use of sym
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expr.RecordEmpty(_, _) =>
      Index.occurrenceOf(exp0)

    case Expr.RecordSelect(exp, label, _, _, _) =>
      Index.occurrenceOf(label) ++ Index.useOf(label) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.RecordExtend(label, exp1, exp2, _, _, _) =>
      Index.occurrenceOf(label) ++ Index.defOf(label) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.RecordRestrict(label, exp, _, _, _) =>
      Index.occurrenceOf(label) ++ Index.defOf(label) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
      val i0 = visitExp(region) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(fields) {
        case (_, e) => visitExp(e)
      }
      val parent = Entity.Exp(exp0)
      val i2 = Index.useOf(sym, loc)
      val fieldSymIndices = fields.map { case (sym, _) => Index.useOf(sym.sym, sym.loc, parent) }
      fieldSymIndices.foldLeft(i0 ++ i1 ++ i2)(_ ++ _)

    case Expr.StructGet(exp, field, tpe, eff, loc) =>
      val parent = Entity.Exp(exp0)
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.StructPut(exp1, field, exp2, tpe, eff, loc) =>
      val parent = Entity.Exp(exp0)
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Ascribe(exp, tpe, eff, _) =>
      visitExp(exp) ++ visitType(tpe) ++ visitType(eff) ++ Index.occurrenceOf(exp0)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, _, _, _) =>
      val dt = declaredType.map(visitType).getOrElse(Index.empty)
      val dp = declaredEff.map(visitType).getOrElse(Index.empty)
      visitExp(exp) ++ dt ++ dp ++ Index.occurrenceOf(exp0)

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, effUse, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(effUse.sym, effUse.loc)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(rules) {
        case CatchRule(_, _, exp) => visitExp(exp)
      }
      i0 ++ i1

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.TryWith(exp, effUse, rules, _, _, _) =>
      val parent = Entity.Exp(exp0)
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(effUse.sym, effUse.loc)
      val i1 = traverse(rules) {
        case HandlerRule(op, fparams, exp) =>
          Index.traverse(fparams)(visitFormalParam) ++ visitExp(exp) ++ Index.useOf(op.sym, op.loc, parent)
      }
      i0 ++ i1

    case Expr.Do(op, exps, _, _, _) =>
      val parent = Entity.Exp(exp0)
      traverse(exps)(visitExp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(op.sym, op.loc, parent)

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.GetStaticField(_, _, _, _) =>
      Index.occurrenceOf(exp0)

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      Index.occurrenceOf(exp0) ++ traverse(methods) {
        case JvmMethod(_, fparams, exp, tpe, eff, _) =>
          Index.traverse(fparams)(visitFormalParam) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)
      }

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val i0 = default.map(visitExp).getOrElse(Index.empty)
      val i1 = traverse(rules) {
        case SelectChannelRule(sym, chan, body) =>
          Index.occurrenceOf(sym, sym.tvar) ++ visitExp(chan) ++ visitExp(body)
      }
      i0 ++ i1 ++ Index.occurrenceOf(exp0)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.ParYield(frags, exp, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(frags) {
        case ParYieldFragment(p, e, _) => visitPat(p) ++ visitExp(e)
      }
      i0 ++ i1

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointConstraintSet(cs, _, _) => traverse(cs)(visitConstraint)

    case Expr.FixpointLambda(pparams, exp, _, _, _) =>
      val i0 = traverse(pparams)(visitPredicateParam)
      i0 ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expr.Error(_, _, _) =>
      Index.occurrenceOf(exp0)
  }

  /**
    * Returns a reverse index for the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expr]): Index = traverse(exps0)(visitExp)

  /**
    * Returns a reverse index for the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern): Index = pat0 match {
    case Pattern.Wild(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Var(sym, tpe, _) =>
      Index.occurrenceOf(pat0) ++ Index.occurrenceOf(sym, tpe)
    case Pattern.Cst(_, _, _) => Index.occurrenceOf(pat0)
    case Pattern.Tag(Ast.CaseSymUse(sym, loc), pat, _, _) =>
      val parent = Entity.Pattern(pat0)
      Index.occurrenceOf(pat0) ++ visitPat(pat) ++ Index.useOf(sym, loc, parent)
    case Pattern.Tuple(elms, _, _) => Index.occurrenceOf(pat0) ++ visitPats(elms)
    case Pattern.Record(pats, pat, _, _) =>
      Index.occurrenceOf(pat0) ++ traverse(pats)(visitRecordLabelPattern) ++ visitPat(pat)
    case Pattern.RecordEmpty(_, _) => Index.empty
    case Pattern.Error(_, _) => Index.empty
  }

  /**
    * Returns a reverse index for the given patterns `pats0`.
    */
  private def visitPats(pats0: List[Pattern]): Index = traverse(pats0)(visitPat)

  /**
    * Returns a reverse index for the given [[Pattern.Record.RecordLabelPattern]] `rfp`.
    */
  private def visitRecordLabelPattern(rfp: Pattern.Record.RecordLabelPattern): Index = {
    Index.useOf(rfp.label) ++ visitType(rfp.tpe) ++ visitPat(rfp.pat)
  }

  /**
    * Returns a reverse index for the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint): Index = c0 match {
    case Constraint(_, head, body, _) =>
      Index.all(
        visitHead(head),
        traverse(body)(visitBody),
      )
  }

  /**
    * Returns a reverse index for the given head predicate `h0`.
    */
  private def visitHead(h0: Predicate.Head): Index = h0 match {
    case Head.Atom(pred, _, terms, tpe, _) => Index.occurrenceOf(pred, tpe) ++ Index.defOf(pred) ++ visitExps(terms)
  }

  /**
    * Returns a reverse index for the given body predicate `b0`.
    */
  private def visitBody(b0: Predicate.Body): Index = b0 match {
    case Body.Atom(pred, _, _, _, terms, tpe, _) => Index.occurrenceOf(pred, tpe) ++ Index.useOf(pred) ++ visitPats(terms)
    case Body.Guard(exp, _) => visitExp(exp)
    case Body.Functional(_, exp, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given type parameter `tparam0`.
    */
  private def visitTypeParam(tparam0: TypeParam): Index = tparam0 match {
    case TypeParam(_, sym, _) => Index.occurrenceOf(sym)
  }

  /**
    * Returns a reverse index for the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam): Index = fparam0 match {
    case FormalParam(_, _, tpe, _, _) =>
      Index.occurrenceOf(fparam0) ++ visitType(tpe)
  }

  /**
    * Returns a reverse index for the given predicate parameter `pparam0`.
    */
  private def visitPredicateParam(pparam0: PredicateParam): Index = pparam0 match {
    case PredicateParam(pred, tpe, _) =>
      Index.occurrenceOf(pred, tpe) ++ Index.defOf(pred) ++ visitType(tpe)
  }

  /**
    * Returns a reverse index for the given type `tpe0`.
    */
  private def visitType(tpe0: Type): Index = tpe0 match {
    case Type.Var(sym, loc) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Arrow(_) =>
        // We do not index arrow constructors.
        Index.empty
      case TypeConstructor.RecordRowExtend(label) => Index.occurrenceOf(tpe0) ++ Index.useOf(label)
      case TypeConstructor.SchemaRowExtend(pred) => Index.occurrenceOf(tpe0) ++ Index.useOf(pred)
      case TypeConstructor.Enum(sym, _) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
      case TypeConstructor.Effect(sym) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
      case _ => Index.occurrenceOf(tpe0)
    }
    case Type.Apply(tpe1, tpe2, _) => visitType(tpe1) ++ visitType(tpe2)
    case Type.Alias(Ast.AliasConstructor(sym, loc), args, _, _) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc) ++ traverse(args)(visitType)
    case Type.AssocType(Ast.AssocTypeConstructor(sym, loc), arg, _, _) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc) ++ visitType(arg)

    // Jvm types should not be exposed to the user.
    case _: Type.JvmToType => Index.empty
    case _: Type.UnresolvedJvmType => Index.empty
  }

  /**
    * Returns a reverse index for the given trait constraint `tconstr0`.
    */
  private def visitTraitConstraint(tconstr0: Ast.TraitConstraint): Index = tconstr0 match {
    case Ast.TraitConstraint(head, arg, _) => visitTraitConstraintHead(head) ++ visitType(arg)
  }

  /**
    * Returns a reverse index for the given trait constraint `head`.
    */
  private def visitTraitConstraintHead(head0: Ast.TraitConstraint.Head): Index = head0 match {
    case Ast.TraitConstraint.Head(sym, loc) => Index.useOf(sym, loc)
  }

  /**
    * Returns a reverse index for the given equality constraint `econstr0`.
    */
  private def visitEqualityConstraint(econstr0: Ast.EqualityConstraint): Index = econstr0 match {
    case Ast.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      visitAssocTypeConstructor(cst) ++ visitType(tpe1) ++ visitType(tpe2)
  }

  /**
    * Returns a reverse index for the given associated type constructor `cst`.
    */
  private def visitAssocTypeConstructor(cst: Ast.AssocTypeConstructor): Index = cst match {
    case Ast.AssocTypeConstructor(sym, loc) => Index.useOf(sym, loc)
  }

}
