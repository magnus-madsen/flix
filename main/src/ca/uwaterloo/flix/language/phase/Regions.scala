/*
 *  Copyright 2022 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Type}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.collection.immutable.SortedSet

/**
  * The region phase ensures that regions do not escape outside of their scope.
  *
  * It does so by keep tracking of every region variable in scope and ensure that every
  * rigid Boolean type variable that occurs anywhere belongs is in scope.
  */
object Regions {

  def run(root: Root)(implicit flix: Flix): Validation[Unit, CompilationMessage] = flix.phase("Regions") {
    val errors = ParOps.parMap(root.defs)(kv => visitDef(kv._2)).flatten

    // TODO: Instances
    Validation.toSuccessOrSoftFailure((), errors)
  }(DebugValidation()(DebugNoOp()))

  private def visitDef(def0: Def)(implicit flix: Flix): List[TypeError.RegionVarEscapes] =
    visitExp(def0.exp)(Nil, flix)

  private def visitExp(exp0: Expr)(implicit scope: List[Type.Var], flix: Flix): List[TypeError.RegionVarEscapes] = exp0 match {
    case Expr.Cst(_, _, _) => Nil

    case Expr.Var(_, tpe, loc) => checkType(tpe, loc)

    case Expr.Def(_, _, _) => Nil

    case Expr.Sig(_, _, _) => Nil

    case Expr.Hole(_, _, _) => Nil

    case Expr.HoleWithExp(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.OpenAs(_, exp, tpe, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Use(_, _, exp, loc) => visitExp(exp)

    case Expr.Lambda(_, exp, tpe, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Apply(exp, exps, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Unary(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Binary(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.Let(_, _, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.LetRec(_, _, _, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.Region(_, _) =>
      Nil

    case Expr.Scope(_, regionVar, exp, tpe, _, loc) =>
      visitExp(exp)(regionVar :: scope, flix) ++ checkType(tpe, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ checkType(tpe, loc)

    case Expr.Stm(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.Discard(exp, _, _) => visitExp(exp)

    case Expr.Match(exp, rules, tpe, _, loc) =>
      val matchErrors = visitExp(exp)
      val rulesErrors = rules.flatMap {
        case MatchRule(pat, guard, body) => guard.map(visitExp).getOrElse(Nil) ++ visitExp(body)
      }
      matchErrors ++ rulesErrors ++ checkType(tpe, loc)

    case Expr.TypeMatch(exp, rules, tpe, _, loc) =>
      val matchErrors = visitExp(exp)
      val rulesErrors = rules.flatMap {
        case TypeMatchRule(_, _, body) => visitExp(body)
      }
      matchErrors ++ rulesErrors ++ checkType(tpe, loc)

    case Expr.RestrictableChoose(_, exp, rules, tpe, _, loc) =>
      val expErrors = visitExp(exp)
      val rulesErrors = rules.flatMap {
        case RestrictableChooseRule(_, exp) => visitExp(exp)
      }
      expErrors ++ rulesErrors ++ checkType(tpe, loc)

    case Expr.Tag(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.RestrictableTag(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Tuple(elms, tpe, _, loc) =>
      elms.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.RecordEmpty(_, _) =>
      Nil

    case Expr.RecordSelect(exp, _, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.RecordExtend(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.RecordRestrict(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.ArrayLit(exps, exp, tpe, _, loc) =>
      exps.flatMap(visitExp) ++ visitExp(exp) ++ checkType(tpe, loc)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ checkType(tpe, loc)

    case Expr.ArrayLoad(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.ArrayLength(exp, _, loc) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.VectorLit(exps, tpe, _, loc) =>
      exps.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.VectorLength(exp, loc) =>
      visitExp(exp)

    case Expr.Ref(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.Deref(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Assign(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.Ascribe(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.InstanceOf(exp, _, loc) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.UncheckedCast(exp, _, _, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.UncheckedMaskingCast(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Without(exp, _, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.TryCatch(exp, rules, tpe, _, loc) =>
      val rulesErrors = rules.flatMap {
        case CatchRule(sym, clazz, e) => visitExp(e)
      }
      rulesErrors ++ visitExp(exp) ++ checkType(tpe, loc)

    case Expr.TryWith(exp, _, rules, tpe, _, loc) =>
      val rulesErrors = rules.flatMap {
        case HandlerRule(_, _, e) => visitExp(e)
      }
      rulesErrors ++ visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Do(_, exps, tpe, _, loc) =>
      exps.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.InvokeConstructor(_, exps, tpe, _, loc) =>
      exps.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.InvokeMethod(_, exp, exps, tpe, _, loc) =>
      visitExp(exp) ++ exps.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.InvokeStaticMethod(_, exps, tpe, _, loc) =>
      exps.flatMap(visitExp) ++ checkType(tpe, loc)

    case Expr.GetField(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.PutField(_, exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.GetStaticField(_, tpe, _, loc) =>
      Nil

    case Expr.PutStaticField(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.NewObject(_, _, tpe, _, methods, loc) =>
      methods.flatMap(visitJvmMethod) ++ checkType(tpe, loc)

    case Expr.NewChannel(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.GetChannel(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.PutChannel(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.SelectChannel(rules, default, tpe, _, loc) =>
      val rulesErrors = rules.flatMap {
        case SelectChannelRule(sym, chan, exp) => visitExp(chan) ++ visitExp(exp)
      }
      val defaultErrors = default.map(visitExp).getOrElse(Nil)
      rulesErrors ++ defaultErrors ++ checkType(tpe, loc)

    case Expr.Spawn(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.ParYield(frags, exp, tpe, _, loc) =>
      val fragsErrors = frags.flatMap {
        case ParYieldFragment(_, e, _) => visitExp(e)
      }
      fragsErrors ++ visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Lazy(exp, tpe, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Force(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      Nil // TODO

    case Expr.FixpointLambda(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.FixpointMerge(exp1, exp2, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ checkType(tpe, loc)

    case Expr.FixpointSolve(exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.FixpointFilter(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.FixpointInject(exp, _, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.FixpointProject(_, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)

    case Expr.Error(_, _, _) =>
      Nil

    case Expr.Mutated(mutExp,_, _ ,_, _) => visitExp(mutExp)

  }

  def visitJvmMethod(method: JvmMethod)(implicit scope: List[Type.Var], flix: Flix): List[TypeError.RegionVarEscapes] = method match {
    case JvmMethod(_, _, exp, tpe, _, loc) =>
      visitExp(exp) ++ checkType(tpe, loc)
  }

  /**
    * Ensures that no region escapes inside `tpe`.
    */
  private def checkType(tpe: Type, loc: SourceLocation)(implicit scope: List[Type.Var], flix: Flix): List[TypeError.RegionVarEscapes] = {
    // Compute the region variables that escape.
    // We should minimize `tpe`, but we do not because of the performance cost.
    val regs = regionVarsOf(tpe)
    for (reg <- regs -- scope) {
      if (essentialTo(reg, tpe)) {
        return TypeError.RegionVarEscapes(reg, tpe, loc) :: Nil
      }
    }
    Nil
  }

  /**
    * Returns true iff the type variable `tvar` is essential to the type `tpe`.
    *
    * A type variable is essential if its ascription has a bearing on the resulting value.
    * For example, in the type `a and (not a)`, `a` is not essential since the result is always `false`.
    */
  def essentialTo(tvar: Type.Var, tpe: Type)(implicit flix: Flix): Boolean = {
    if (!tpe.typeVars.contains(tvar)) {
      // Case 1: The type variable is not present in the type. It cannot be essential.
      false
    } else {
      // Case 2: The type variable is present in the type. Check if it is essential to any of the booleans.
      boolTypesOf(tpe).exists(essentialToBool(tvar, _))
    }
  }

  /**
    * Returns true iff the type variable `tvar` is essential to the boolean formula `tpe`.
    * Assumes that `tvar` is present in the type.
    */
  def essentialToBool(tvar: Type.Var, tpe: Type)(implicit flix: Flix): Boolean = {
    // t0 = tpe[tvar -> False]
    val t0 = Substitution.singleton(tvar.sym, Type.Univ).apply(tpe)

    // t1 = tpe[tvar -> True]
    val t1 = Substitution.singleton(tvar.sym, Type.Pure).apply(tpe)

    // tvar is essential if t0 != t1
    !sameType(t0, t1)
  }

  /**
    * Extracts all the boolean formulas from the given type `t0`.
    */
  private def boolTypesOf(t0: Type): List[Type] = t0 match {
    case t if t.kind == Kind.Eff => List(t)
    case _: Type.Var => Nil
    case _: Type.Cst => Nil
    case Type.Apply(tpe1, tpe2, _) => boolTypesOf(tpe1) ::: boolTypesOf(tpe2)
    case Type.Alias(_, _, tpe, _) => boolTypesOf(tpe)
    case Type.AssocType(_, arg, _, _) => boolTypesOf(arg) // TODO ASSOC-TYPES ???
  }

  /**
    * Returns true iff the two types denote the same Boolean function, using the same variables.
    */
  private def sameType(t1: Type, t2: Type)(implicit flix: Flix): Boolean = {
    val tvars = t1.typeVars ++ t2.typeVars

    /**
      * Evaluates the given boolean formula,
      * where `trueVars` are the variables ascribed the value TRUE,
      * and all other variables are ascribed the value FALSE.
      */
    def eval(tpe: Type, trueVars: SortedSet[Type.Var]): Boolean = tpe match {
      case Type.Pure => true
      case Type.Univ => false
      case Type.Apply(Type.Complement, x, _) => eval(x, trueVars)
      case Type.Apply(Type.Apply(Type.Union, x1, _), x2, _) => eval(x1, trueVars) && eval(x2, trueVars)
      case Type.Apply(Type.Apply(Type.Intersection, x1, _), x2, _) => eval(x1, trueVars) || eval(x2, trueVars)
      case tvar: Type.Var => trueVars.contains(tvar)
      case _ => throw InternalCompilerException(s"unexpected type $tpe", tpe.loc)
    }

    val subsets = tvars.subsets()
    subsets.forall(trueVars => eval(t1, trueVars) == eval(t2, trueVars))
  }

  /**
    * Returns all region variables in the given type `tpe`.
    */
  private def regionVarsOf(tpe: Type): SortedSet[Type.Var] = tpe.typeVars.filter {
    case tvar =>
      val isBool = tvar.sym.kind == Kind.Eff
      val isRegion = tvar.sym.isRegion
      isBool && isRegion
  }

}
