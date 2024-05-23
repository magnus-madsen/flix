/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.extra

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization

object CodeHinter {

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root, sources: Set[String])(implicit flix: Flix, index: Index): List[CodeHint] = {
    val traitHints = root.traits.values.flatMap(visitTrait(_)(root, index)).toList
    val defsHints = root.defs.values.flatMap(visitDef(_)(root, flix)).toList
    val enumsHints = root.enums.values.flatMap(visitEnum(_)(root, index)).toList
    (traitHints ++ defsHints ++ enumsHints).filter(include(_, sources))
  }

  /**
    * Returns `true` if the given code `hint` should be included in the result.
    */
  private def include(hint: CodeHint, sources: Set[String]): Boolean =
    sources.contains(hint.loc.source.name)

  /**
    * Computes code quality hints for the given enum `enum`.
    */
  private def visitEnum(enum0: TypedAst.Enum)(implicit root: Root, index: Index): List[CodeHint] = {
    val tagUses = enum0.cases.keys.flatMap(sym => index.usesOf(sym))
    val enumUses = index.usesOf(enum0.sym)
    val uses = enumUses ++ tagUses
    val isDeprecated = enum0.ann.isDeprecated
    val isExperimental = enum0.ann.isExperimental
    val deprecated = if (isDeprecated) uses.map(CodeHint.Deprecated) else Nil
    val experimental = if (isExperimental) uses.map(CodeHint.Experimental) else Nil
    (deprecated ++ experimental).toList
  }

  /**
    * Computes code quality hints for the given trait `trt`.
    */
  private def visitTrait(trt: TypedAst.Trait)(implicit root: Root, index: Index): List[CodeHint] = {
    val uses = index.usesOf(trt.sym)
    val isDeprecated = trt.ann.isDeprecated
    val deprecated = if (isDeprecated) uses.map(CodeHint.Deprecated) else Nil
    val isExperimental = trt.ann.isExperimental
    val experimental = if (isExperimental) uses.map(CodeHint.Experimental) else Nil
    (deprecated ++ experimental).toList
  }

  /**
    * Computes code quality hints for the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def)(implicit root: Root, flix: Flix): List[CodeHint] = {
    visitExp(def0.exp)
  }

  /**
    * Computes code quality hints for the given expression `exp0`.
    */
  private def visitExp(exp0: Expr)(implicit root: Root, flix: Flix): List[CodeHint] = exp0 match {
    case Expr.Mutated(mutExp,_,_,_,_) => visitExp(mutExp)

    case Expr.Var(_, _, _) => Nil

    case Expr.Def(sym, _, loc) =>
      checkDeprecated(sym, loc) ++
        checkExperimental(sym, loc) ++
        checkParallel(sym, loc) ++
        checkLazy(sym, loc)

    case Expr.Sig(_, _, _) => Nil

    case Expr.Hole(_, _, _) => Nil

    case Expr.HoleWithExp(exp, _, _, _) => visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) => visitExp(exp)

    case Expr.Use(_, _, exp, _) => visitExp(exp)

    case Expr.Cst(_, _, _) => Nil

    case Expr.Lambda(_, exp, _, _) =>
      checkEffect(exp.eff, exp.loc) ++ visitExp(exp)

    case Expr.Apply(exp, exps, _, eff, loc) =>
      val hints0 = (exp, exps) match {
        case (Expr.Def(sym, _, _), lambda :: _) =>
          checkEffect(sym, lambda.tpe, loc)
        case _ => Nil
      }
      val hints1 = checkEffect(eff, loc)
      hints0 ++ hints1 ++ visitExp(exp) ++ visitExps(exps)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(_, _, exp1, exp2, _, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Region(_, _) => Nil

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(matchExp, rules, _, _, _) =>
      visitExp(matchExp) ++ rules.flatMap {
        case MatchRule(_, guard, exp) => guard.toList.flatMap(visitExp) ::: visitExp(exp)
      }

    case Expr.TypeMatch(matchExp, rules, _, _, _) =>
      visitExp(matchExp) ++ rules.flatMap {
        case TypeMatchRule(_, _, exp) => visitExp(exp)
      }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case RestrictableChooseRule(_, body) => visitExp(body)
      }

    case Expr.Tag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.RestrictableTag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expr.RecordEmpty(_, _) => Nil

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp2) ++ visitExp(exp1)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ref(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, tpe, eff, loc) =>
      visitExp(exp)

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case CatchRule(_, _, exp) => visitExp(exp)
      }

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case HandlerRule(_, _, e) => visitExp(e)
      }

    case Expr.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args)

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      Nil

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.flatMap {
        case JvmMethod(_, _, exp, _, _, _) => visitExp(exp)
      }

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap {
        case SelectChannelRule(_, chan, exp) => visitExp(chan) ++ visitExp(exp)
      } ++ default.map(visitExp).getOrElse(Nil)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.flatMap {
        case ParYieldFragment(_, e, _) => visitExp(e)
      } ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.flatMap(visitConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      Nil

  }

  /**
    * Computes code quality hints for the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expr])(implicit root: Root, flix: Flix): List[CodeHint] =
    exps.flatMap(visitExp)

  /**
    * Computes code quality hints for the given constraint `c`.
    */
  private def visitConstraint(c: Constraint)(implicit root: Root, flix: Flix): List[CodeHint] =
    visitHeadPredicate(c.head) ++ c.body.flatMap(visitBodyPredicate)

  /**
    * Computes code quality hints for the given head predicate `p`.
    */
  private def visitHeadPredicate(p: TypedAst.Predicate.Head)(implicit root: Root, flix: Flix): List[CodeHint] = p match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
  }

  /**
    * Computes code quality hints for the given body predicate `p`.
    */
  private def visitBodyPredicate(p: TypedAst.Predicate.Body)(implicit root: Root, flix: Flix): List[CodeHint] = p match {
    case Body.Atom(_, _, _, _, _, _, _) => Nil
    case Body.Functional(_, exp, _) => visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Checks whether `sym` would benefit from `tpe` being pure.
    */
  private def checkEffect(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    if (lazyWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.LazyEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForLazyEvaluation(sym, loc) :: Nil
    } else if (parallelWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.ParallelEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForParallelEvaluation(sym, loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Returns `true` if the the given `sym` is marked being purity reflective
    * and uses lazy evaluation when given a pure function argument.
    */
  private def lazyWhenPure(sym: Symbol.DefnSym)(implicit root: Root): Boolean = {
    val defn = root.defs(sym)
    defn.spec.ann.isLazyWhenPure
  }

  /**
    * Returns `true` if the given `sym` is marked being purity reflective
    * and uses parallel evaluation when given a pure function argument.
    */
  private def parallelWhenPure(sym: Symbol.DefnSym)(implicit root: Root): Boolean = {
    val defn = root.defs(sym)
    defn.spec.ann.isParallelWhenPure
  }

  /**
    * Checks whether `tpe` is a non-trivial effect.
    *
    * NB: Not currently checked for every expression.
    */
  private def checkEffect(tpe: Type, loc: SourceLocation)(implicit flix: Flix): List[CodeHint] = {
    if (numberOfVarOccurs(tpe) < 5) {
      // Case 1: Formula is small. Good.
      Nil
    } else {
      // Case 2: Formula is big. Try to minimize it.
      val minType = TypeMinimization.minimizeType(tpe)
      if (numberOfVarOccurs(minType) < 5) {
        // Case 2.1: Formula is small. Good.
        Nil
      } else {
        // Case 2.2: Formula is still big. Report a code hint.
        CodeHint.NonTrivialEffect(minType, loc) :: Nil
      }
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is deprecated.
    */
  private def checkDeprecated(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isDeprecated = defn.spec.ann.isDeprecated
    if (isDeprecated) {
      CodeHint.Deprecated(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is experimental.
    */
  private def checkExperimental(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isExperimental = defn.spec.ann.isExperimental
    if (isExperimental) {
      CodeHint.Experimental(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is unsafe.
    */
  private def checkParallel(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isParallel = defn.spec.ann.isParallel
    if (isParallel) {
      CodeHint.Parallel(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is lazy.
    */
  private def checkLazy(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isLazy = defn.spec.ann.isLazy
    if (isLazy) {
      CodeHint.Lazy(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Returns `true` if the given function type `tpe` is pure.
    */
  private def isPureFunction(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => tpe.arrowEffectType == Type.Pure
    case _ => false
  }

  /**
    * Returns the total number of variable *occurrences* in the given type `tpe`.
    */
  private def numberOfVarOccurs(tpe: Type): Int = tpe match {
    case Type.Var(_, _) => 1
    case Type.Cst(_, _) => 0
    case Type.Apply(tpe1, tpe2, _) => numberOfVarOccurs(tpe1) + numberOfVarOccurs(tpe2)
    case Type.Alias(_, _, tpe, _) => numberOfVarOccurs(tpe)
    case Type.AssocType(_, arg, _, _) => numberOfVarOccurs(arg)
  }

}
