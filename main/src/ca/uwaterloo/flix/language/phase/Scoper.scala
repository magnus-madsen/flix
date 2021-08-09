/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{ScopeScheme, Scopedness, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.ScopeError
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}


/**
  * Checks that the AST complies with the following scoping rules:
  *
  * Primary rules
  * 1. Unscoped functions cannot access free Scoped variables.
  * 2. Scoped values cannot be returned from functions.
  * 3. Scoped values cannot be stored in mutable memory.
  *
  * Supplementary rules
  * 4. Scoped values cannot be passed to functions expecting Unscoped parameters.
  * 5. An immutable structure composed of at least one Scoped value is Scoped.
  * 6. A value resulting from the destruction of a Scoped immutable structure is Scoped.
  */
object Scoper extends Phase[Root, Root] {

  private val noScope = (Scopedness.Unscoped, ScopeScheme.Unit, Set.empty[Symbol.VarSym])

  override def run(root: Root)(implicit flix: Flix): Validation[Root, ScopeError] = {
    val defsVal = Validation.traverseX(root.defs.values)(checkDef(_, root))
    val sigsVal = Validation.traverseX(root.sigs.values)(checkSig(_, root))
    val instanceDefsVal = Validation.traverseX(TypedAstOps.instanceDefsOf(root))(checkDef(_, root))

    Validation.sequenceX(List(defsVal, sigsVal, instanceDefsVal)).map(_ => root)
  }

  private def checkSig(sig: Sig, root: Root): Validation[Unit, ScopeError] = sig match {
    case Sig(_, spec, Some(impl)) => checkImpl(spec, impl, root)
    case Sig(_, _, None) => ().toSuccess
  }

  private def checkDef(defn: Def, root: Root): Validation[Unit, ScopeError] = defn match {
    case Def(sym, spec, impl) => checkImpl(spec, impl, root)
  }

  private def checkImpl(spec: Spec, impl: Impl, root: Root): Validation[Unit, ScopeError] = {
    val senv = spec.fparams.map {
      case FormalParam(sym, mod, tpe, scSc, loc) => sym -> (sym.scopedness, scSc.get)
    }.toMap
    checkExp(impl.exp, senv, root).map(_ => ())
  }

  private def checkExp(exp0: Expression, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = exp0 match {
    case Expression.Unit(loc) => noScope.toSuccess
    case Expression.Null(tpe, loc) => noScope.toSuccess
    case Expression.True(loc) => noScope.toSuccess
    case Expression.False(loc) => noScope.toSuccess
    case Expression.Char(lit, loc) => noScope.toSuccess
    case Expression.Float32(lit, loc) => noScope.toSuccess
    case Expression.Float64(lit, loc) => noScope.toSuccess
    case Expression.Int8(lit, loc) => noScope.toSuccess
    case Expression.Int16(lit, loc) => noScope.toSuccess
    case Expression.Int32(lit, loc) => noScope.toSuccess
    case Expression.Int64(lit, loc) => noScope.toSuccess
    case Expression.BigInt(lit, loc) => noScope.toSuccess
    case Expression.Str(lit, loc) => noScope.toSuccess
    case Expression.Default(tpe, loc) => noScope.toSuccess
    case Expression.Wild(tpe, loc) => noScope.toSuccess
    case Expression.Var(sym, tpe, loc) =>
      senv(sym) match {
        case (Scopedness.Scoped, sch) => (sym.scopedness, sch, Set(sym)).toSuccess
        case (Scopedness.Unscoped, sch) => (sym.scopedness, sch, Set.empty[Symbol.VarSym]).toSuccess
      }
    case Expression.Def(sym, tpe, loc) => (Scopedness.Unscoped, root.defs(sym).spec.scSc, Set.empty[Symbol.VarSym]).toSuccess
    case Expression.Sig(sym, tpe, loc) => (Scopedness.Unscoped, root.sigs(sym).spec.scSc, Set.empty[Symbol.VarSym]).toSuccess
    case Expression.Hole(sym, tpe, eff, loc) => (Scopedness.Unscoped, mkScopeScheme(tpe), Set.empty[Symbol.VarSym]).toSuccess
    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val fparamSch = mkScopeScheme(fparam.tpe)
      for {
        (bodySch, bodyVars) <- checkUnscopedExp(exp, senv + (fparam.sym -> (fparam.sym.scopedness, fparamSch)), root)
        freeVars = bodyVars - fparam.sym
        sco = if (freeVars.isEmpty) Scopedness.Unscoped else Scopedness.Scoped
        sch = ScopeScheme.Arrow(fparam.sym.scopedness, fparamSch, bodySch)
      } yield (sco, sch, freeVars)
    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      for {
        (_, funcSch, funcVars) <- checkExp(exp, senv, root)
        args <- Validation.traverse(exps)(checkExp(_, senv, root))
        sch <- Validation.fold(args, funcSch) {
          case (ScopeScheme.Arrow(paramSco, paramSch, rest), (argSco, argSch, _)) =>
            if (!(argSco <= paramSco)) {
              ??? // error bad arg
            } else if (!(argSch <= paramSch)) {
              ??? // error bad arg
            } else {
              rest.toSuccess
            }
          case (ScopeScheme.Unit, _) => throw InternalCompilerException("Unexpected application.")
        }
        argVars = args.flatMap(_._3)

      } yield (Scopedness.Unscoped, sch, funcVars ++ argVars)
    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      for {
        (_, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (_, _, vars2) <- checkExp(exp2, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      for {
        (varSco, varSch, varVars) <- checkExp(exp1, senv, root)
        (sco, sch, vars) <- checkExp(exp2, senv + (sym -> (varSco, varSch)), root)
        freeVars = (varVars ++ vars) - sym
      } yield (sco, sch, freeVars)
    case Expression.LetRegion(sym, exp, tpe, eff, loc) => checkExp(exp, senv, root) // MATT right?
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (sco2, sch2, vars2) <- checkExp(exp2, senv, root)
        (sco3, sch3, vars3) <- checkExp(exp3, senv, root)
      } yield (sco2 max sco3, sch2 max sch3, vars1 ++ vars2 ++ vars3)
    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (sco, sch, vars2) <- checkExp(exp2, senv, root)
      } yield (sco, sch, vars1 ++ vars2)
    case Expression.Match(exp, rules, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
        (ruleScos, ruleSchs, ruleVars) <- Validation.traverse(rules)(checkMatchRule(_, senv, sco, root)).map(_.unzip3)
      } yield (ruleScos.reduce(_ max _), ruleSchs.reduce(_ max _), vars ++ ruleVars.flatten)
    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      for {
        (scos, _, vars) <- Validation.traverse(exps)(checkExp(_, senv, root)).map(_.unzip3)
        expsSco = scos.reduce(_ max _)
        (ruleScos, ruleSchs, ruleVars) <- Validation.traverse(rules)(checkChoiceRule(_, senv, expsSco, root)).map(_.unzip3)
      } yield (ruleScos.reduce(_ max _), ruleSchs.reduce(_ max _), vars.flatten.toSet ++ ruleVars.flatten)
    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.Tuple(elms, tpe, eff, loc) =>
      for {
        (scos, schs, vars) <- Validation.traverse(elms)(checkExp(_, senv, root)).map(_.unzip3)
      } yield (scos.reduce(_ max _), ScopeScheme.Unit, vars.flatten.toSet)
    case Expression.RecordEmpty(tpe, loc) => noScope.toSuccess
    case Expression.RecordSelect(exp, field, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, mkScopeScheme(tpe), vars)
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) =>
      for {
        (valSco, _, valVars) <- checkExp(value, senv, root)
        (restSco, _, restVars) <- checkExp(rest, senv, root)
      } yield (valSco max restSco, ScopeScheme.Unit, valVars ++ restVars)
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(rest, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      for {
        (_, vars) <- Validation.traverse(elms)(checkUnscopedExp(_, senv, root)).map(_.unzip)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars.flatten.toSet)
    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      for {
        (_, elmVars) <- checkUnscopedExp(elm, senv, root)
        (_, _, lenVars) <- checkExp(len, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, elmVars ++ lenVars)
    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      for {
        (baseSco, _, baseVars) <- checkExp(base, senv, root)
        (_, _, indexVars) <- checkExp(index, senv, root)
      } yield (baseSco, mkScopeScheme(tpe), baseVars ++ indexVars)
    case Expression.ArrayLength(base, eff, loc) =>
      for {
        (_, _, baseVars) <- checkExp(base, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, baseVars)
    case Expression.ArrayStore(base, index, elm, loc) =>
      for {
        (_, _, baseVars) <- checkExp(base, senv, root)
        (_, _, indexVars) <- checkExp(index, senv, root)
        (_, elmVars) <- checkUnscopedExp(elm, senv, root)
        // MATT check elem is unscoped
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, baseVars ++ indexVars ++ elmVars)
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      for {
        (baseSco, _, baseVars) <- checkExp(base, senv, root)
        (_, _, beginIndexVars) <- checkExp(beginIndex, senv, root)
        (_, _, endIndexVars) <- checkExp(endIndex, senv, root)
      } yield (baseSco, ScopeScheme.Unit, baseVars ++ beginIndexVars ++ endIndexVars)
    case Expression.Ref(exp, tpe, eff, loc) =>
      for {
        (_, vars) <- checkUnscopedExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.Deref(exp, tpe, eff, loc) =>
      for {
        (_, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, mkScopeScheme(tpe), vars)
    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (_, vars2) <- checkUnscopedExp(exp2, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.Existential(fparam, exp, loc) => noScope.toSuccess // MATT
    case Expression.Universal(fparam, exp, loc) => noScope.toSuccess // MATT
    case Expression.Ascribe(exp, tpe, eff, loc) => checkExp(exp, senv, root)
    case Expression.Cast(exp, tpe, eff, loc) => checkExp(exp, senv, root)
    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      for {
        (expSco, expSch, expVars) <- checkExp(exp, senv, root)
        (ruleScos, ruleSchs, ruleVars) <- Validation.traverse(rules)(checkCatchRule(_, senv, root)).map(_.unzip3)
        sco = ruleScos.foldLeft(expSco)(_ max _)
        sch = ruleSchs.foldLeft(expSch)(_ max _)
        vars = expVars ++ ruleVars.flatten
      } yield (sco, sch, vars)
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      for {
        (_, vars) <- Validation.traverse(args)(checkUnscopedExp(_, senv, root)).map(_.unzip)
      } yield (Scopedness.Unscoped, mkScopeScheme(tpe), vars.flatten.toSet)
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      for {
        (_, expVars) <- checkUnscopedExp(exp, senv, root)
        (_, argVars) <- Validation.traverse(args)(checkUnscopedExp(_, senv, root)).map(_.unzip)
      } yield (Scopedness.Unscoped, mkScopeScheme(tpe), expVars ++ argVars.flatten)
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      for {
        (_, argVars) <- Validation.traverse(args)(checkUnscopedExp(_, senv, root)).map(_.unzip)
      } yield (Scopedness.Unscoped, mkScopeScheme(tpe), argVars.flatten.toSet)
    case Expression.GetField(field, exp, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, mkScopeScheme(tpe), vars)
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (_, vars2) <- checkUnscopedExp(exp2, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.GetStaticField(field, tpe, eff, loc) => (Scopedness.Unscoped, mkScopeScheme(tpe), Set.empty[Symbol.VarSym]).toSuccess
    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      for {
        (_, vars) <- checkUnscopedExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.NewChannel(exp, tpe, eff, loc) =>
      for {
        (_, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.GetChannel(exp, tpe, eff, loc) =>
      for {
        (_, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, mkScopeScheme(tpe), vars)
    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (_, vars2) <- checkUnscopedExp(exp2, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      for {
        (ruleScos, ruleSchs, ruleVars) <- Validation.traverse(rules)(checkSelectChannelRule(_, senv, root)).map(_.unzip3)
        (defScos, defSchs, defVars) <- Validation.traverse(default)(checkExp(_, senv, root)).map(_.unzip3)
        sco = (defScos ++ ruleScos).reduce(_ max _)
        sch = (defSchs ++ ruleSchs).reduce(_ max _)
        vars = ruleVars.flatten.toSet ++ defVars.flatten
      } yield (sco, sch, vars)
    case Expression.Spawn(exp, tpe, eff, loc) =>
      for {
        (_, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.Lazy(exp, tpe, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.Force(exp, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, mkScopeScheme(tpe), vars)
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      for {
        (scos, _, vars) <- Validation.traverse(cs)(checkConstraint(_, senv, root)).map(_.unzip3)
        sco = scos.foldLeft(Scopedness.Unscoped: Scopedness)(_ max _)
      } yield (sco, ScopeScheme.Unit, vars.flatten.toSet)
    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      for {
        (sco1, _, vars1) <- checkExp(exp1, senv, root)
        (sco2, _, vars2) <- checkExp(exp2, senv, root)
      } yield (sco1 max sco2, ScopeScheme.Unit, vars1 ++ vars2)
    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.FixpointFilter(pred, exp, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (sco, ScopeScheme.Unit, vars)
    case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) =>
      for {
        (_, vars) <- checkUnscopedExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
    case Expression.MatchEff(exp1, exp2, exp3, tpe, eff, loc) =>
      for {
        (_, _, vars1) <- checkExp(exp1, senv, root)
        (sco2, sch2, vars2) <- checkExp(exp2, senv, root)
        (sco3, sch3, vars3) <- checkExp(exp3, senv, root)
      } yield (sco2 max sco3, sch2 max sch3, vars1 ++ vars2 ++ vars3)
  }

  private def checkMatchRule(rule: MatchRule, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], sco: Scopedness, root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = rule match {
    case MatchRule(pat, guard, exp) =>
      val patEnv = TypedAstOps.binds(pat).map {
        case (sym, tpe) => sym -> (sco, mkScopeScheme(tpe))
      }
      val fullEnv = senv ++ patEnv
      for {
        (_, _, guardVars) <- checkExp(guard, fullEnv, root)
        (expSco, expSch, expVars) <- checkExp(exp, fullEnv, root)
        freeVars = (guardVars ++ expVars) -- patEnv.keys
      } yield (expSco, expSch, freeVars)
  }

  private def checkChoiceRule(rule: ChoiceRule, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], sco: Scopedness, root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = rule match {
    case ChoiceRule(pat, exp) =>
      val patEnv = pat.foldLeft(Map.empty[Symbol.VarSym, (Scopedness, ScopeScheme)]) {
        case (acc, ChoicePattern.Wild(loc)) => acc
        case (acc, ChoicePattern.Absent(loc)) => acc
        case (acc, ChoicePattern.Present(sym, tpe, loc)) => acc + (sym -> (sco, mkScopeScheme(tpe)))
      }
      val fullEnv = senv ++ patEnv
      for {
        (expSco, expSch, expVars) <- checkExp(exp, fullEnv, root)
        freeVars = expVars -- patEnv.keys
      } yield (expSco, expSch, freeVars)
  }

  private def checkCatchRule(rule: CatchRule, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = rule match {
    case CatchRule(sym, clazz, exp) =>
      val fullEnv = senv + (sym -> (Scopedness.Unscoped, ScopeScheme.Unit))
      for {
        (expSco, expSch, expVars) <- checkExp(exp, fullEnv, root: Root)
        freeVars = expVars - sym
      } yield (expSco, expSch, freeVars)
  }

  private def checkSelectChannelRule(rule: SelectChannelRule, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = rule match {
    case SelectChannelRule(sym, chan, exp) =>
      val elmTpe = chan.tpe match {
        case Type.Apply(Type.Cst(TypeConstructor.Channel, _), tpe) => tpe
        case _ => throw InternalCompilerException("Unexpected channel type in SelectChannelRule.")
      }
      for {
        (_, _, chanVars) <- checkExp(chan, senv, root)
        fullEnv = senv + (sym -> (Scopedness.Unscoped, mkScopeScheme(elmTpe)))
        (expSco, expSch, expVars) <- checkExp(exp, fullEnv, root)
        freeVars = (chanVars ++ expVars) - sym
      } yield (expSco, expSch, freeVars)

  }

  private def checkConstraint(constr: Constraint, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = constr match {
    case Constraint(cparams, head, body, loc) =>
      val cparamEnv = cparams.map { cparam => cparam.sym -> (cparam.sym.scopedness, mkScopeScheme(cparam.tpe)) }.toMap
      val fullEnv = senv ++ cparamEnv
      for {
        (headSco, _, headVars) <- checkHeadPredicate(head, fullEnv, root)
        (bodyScos, _, bodyVars) <- Validation.traverse(body)(checkBodyPredicate(_, fullEnv, root)).map(_.unzip3)
        sco = (headSco :: bodyScos).reduce(_ max _)
        sch = ScopeScheme.Unit
        vars = (headVars ++ bodyVars.flatten) -- cparamEnv.keys
      } yield (sco, sch, vars)

  }

  private def checkHeadPredicate(head: Predicate.Head, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = head match {
    case Head.Atom(pred, den, terms, tpe, loc) =>
      for {
        (scos, _, vars) <- Validation.traverse(terms)(checkExp(_, senv, root)).map(_.unzip3)
      } yield ((Scopedness.Unscoped :: scos).reduce(_ max _), ScopeScheme.Unit, vars.flatten.toSet)
  }

  private def checkBodyPredicate(body: Predicate.Body, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(Scopedness, ScopeScheme, Set[Symbol.VarSym]), ScopeError] = body match {
    case Body.Atom(pred, den, polarity, terms, tpe, loc) => noScope.toSuccess
    case Body.Guard(exp, loc) =>
      for {
        (sco, _, vars) <- checkExp(exp, senv, root)
      } yield (Scopedness.Unscoped, ScopeScheme.Unit, vars)
  }

  private def mkScopeScheme(tpe: Type): ScopeScheme = tpe.typeConstructor match {
    case Some(_: TypeConstructor.Arrow) =>
      val argSchemes = tpe.arrowArgTypes.map(mkScopeScheme)
      val retScheme = mkScopeScheme(tpe.arrowResultType)
      argSchemes.foldRight(retScheme) {
        case (arg, acc) => ScopeScheme.Arrow(Scopedness.Unscoped, arg, acc)
      }
    case _ => ScopeScheme.Unit
  }

  private def checkUnscopedExp(exp: Expression, senv: Map[Symbol.VarSym, (Scopedness, ScopeScheme)], root: Root): Validation[(ScopeScheme, Set[Symbol.VarSym]), ScopeError] = {
    Validation.flatMapN(checkExp(exp, senv, root)) {
      case (Scopedness.Scoped, _, _) => ScopeError.EscapingScopedValue(exp.loc).toFailure
      case (Scopedness.Unscoped, sch, vars) => (sch, vars).toSuccess
    }
  }

}
