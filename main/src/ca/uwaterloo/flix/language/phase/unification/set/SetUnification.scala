/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, BoolUnificationException, SveAlgorithm}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}

import scala.collection.mutable

object SetUnification {

  /**
    * The static parameters of set unification.
    *
    * @param sizeThreshold       if positive, [[solve]] will give up before SVE if there are more equations than this
    * @param sveRecSizeThreshold if positive, [[sve]] will give up on formulas beyond this size
    */
  final case class Options(sizeThreshold: Int, sveRecSizeThreshold: Int)

  final object Options {
    /** The default [[Options]]. */
    val default: Options = Options(25, 10_000)
  }

  /** Represents the running mutable state of the solver. */
  final class State(initialEquations: List[Equation]) {
    /** The remaining equations to solve. */
    var eqs: List[Equation] = initialEquations
    /** The current substitution, which has already been applied to `eqs`. */
    var subst: SetSubstitution = SetSubstitution.empty
  }

  /** A listener that observes the operations of [[solve]]. */
  trait SolverListener {
    /** Is called before a unification phase starts. */
    def onEnterPhase(phaseName: String, state: State): Unit = ()

    /** Is called when a unification phase completes. If it made progress, `state` is `true`. */
    def onExitPhase(state: State, progress: Boolean): Unit = ()
  }

  final object SolverListener {

    /** The [[SolverListener]] that does nothing. */
    val DoNothing: SolverListener = new SolverListener {}

    def stringListener(p: String => Unit): SolverListener = new SolverListener {
      override def onEnterPhase(phaseName: String, state: State): Unit =
        p(s"Phase: $phaseName")

      override def onExitPhase(state: State, progress: Boolean): Unit =
        if (progress) p(stateString(state.eqs, state.subst))
    }
  }

  /**
    * Attempts to solve the equation system `eqs` to find the most general substitution.
    *
    * If the returned [[Equation]] list is empty, then the returned substitution is the most general
    * solution to `eqs`.
    *
    * If the returned [[Equation]] list is non-empty, then the returned substitution is the most general
    * solution to the removed equations.
    *
    * All returned equations are either marked [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]]. The returned equations might not exist in `eqs` directly, but
    * will be derived from it.
    */
  def solve(l: List[Equation])(implicit listener: SolverListener, opts: Options): (List[Equation], SetSubstitution) = {
    val state = new State(l)
    val trivialPhaseName = "Trivial Equations"

    runWithState(state, runRule(constantAssignment), "Constant Assignment")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, runRule(variableAlias), "Variable Aliases")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, runRule(variableAssignment), "Simple Variable Assignment")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, duplicatedAndReflective, "Duplicates and Reflective")
    runWithState(state, runRule(trivial), trivialPhaseName)
    runWithState(state, assertSveEquationCount, "Assert Size")
    runWithState(state, runRule(sve), "SVE")

    (state.eqs, state.subst)
  }

  /** Marks all equations as [[Equation.Status.Timeout]] if there are more than [[Options.sizeThreshold]]. */
  private def assertSveEquationCount(eqs: List[Equation])(implicit opts: Options): Option[(List[Equation], SetSubstitution)] = {
    if (opts.sizeThreshold > 0 && eqs.length > opts.sizeThreshold) {
      val errMsg = s"Amount of leftover equations for SVE (${eqs.length}) is over the threshold (${opts.sizeThreshold})."
      Some(eqs.map(_.toTimeout(errMsg)), SetSubstitution.empty)
    } else None
  }

  /**
    * Runs the given equation system solver `phase` on `state`, printing debugging information
    * according to [[Options]].
    */
  private def runWithState(state: State, phase: List[Equation] => Option[(List[Equation], SetSubstitution)], phaseName: String)(implicit listener: SolverListener): Unit = {
    listener.onEnterPhase(phaseName, state)

    phase(state.eqs) match {
      case Some((eqs, subst)) =>
        state.eqs = eqs
        state.subst = subst @@ state.subst
        listener.onExitPhase(state, progress = true)

      case None =>
        listener.onExitPhase(state, progress = false)
    }
  }

  /**
    * Eliminates redundant equations
    *   - equations that occur multiple times
    *   - `f ~ f` (reflective, syntactically trivial)
    */
  private def duplicatedAndReflective(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    var result: List[Equation] = Nil
    val seen = mutable.Set.empty[Equation]
    var changed = false

    for (eq <- eqs) {
      if (eq.f1 == eq.f2 || seen.contains(eq)) {
        // Don't add to result.
        changed = true
      } else {
        // Add to result, thus not making a change.
        seen.add(eq)
        result = eq :: result
      }
    }

    if (changed) Some(result.reverse, SetSubstitution.empty) else None
  }

  /** Run a unification rule on an equation system in a fixpoint. */
  private def runRule(rule: Equation => Option[(List[Equation], SetSubstitution)])(eqs: List[Equation]): Option[(List[Equation], SetSubstitution)] = {
    // Procedure:
    //   - Run the `rule` on all the (pending) equations in `iterationWorkList`, building
    //     `overallSubst` and producing `iterationOutput`.
    //   - If any rule made progress, store `iterationOutput` back into `iterationWorkList` and repeat.

    // The running substitution - applied to `iterationWorkList` equations lazily.
    var overallSubst = SetSubstitution.empty
    var overallProgress = false

    // The mutable state of the iteration.
    var iterationWorkList = eqs
    var iterationOutput: List[Equation] = Nil
    var iterationProgress = true // this is `true` to run the loop at least once.

    // Run iterations until there is no progress.
    while (iterationProgress) {
      iterationProgress = false

      for (eq0 <- iterationWorkList) {
        val eq = overallSubst.apply(eq0)
        // If `eq` is marked with error or timeout, don't try again.
        val appliedRule = if (eq.isPending) rule(eq) else None

        appliedRule match {
          case Some((outputEqs, outputSubst)) =>
            // We made progress - signal that and update state.
            iterationProgress = true
            overallProgress = true

            iterationOutput = outputEqs ++ iterationOutput
            overallSubst = outputSubst @@ overallSubst

          case None =>
            // No progress, just add to the output.
            iterationOutput = eq :: iterationOutput
        }
      }

      // Swap `iterationOutput` into `iterationWorkList` for next iteration or for return.
      iterationWorkList = iterationOutput
      iterationOutput = Nil
    }

    if (overallProgress) {
      // We apply `overallSubst` lazily, not all equations have seen all substitution information.
      val resultEqs = iterationWorkList.map(overallSubst.apply)
      Some(resultEqs, overallSubst)
    } else {
      None
    }
  }

  //
  // Rules.
  //

  /**
    * Solves equations that trivially hold (like `univ ~ univ`) and marks trivially unsolvable
    * equations (like `univ ~ empty`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `univ ~ univ` becomes `({}, [])`
    *   - `univ ~ empty` becomes `({univ ~error empty}, [])`
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x2 ~ univ` becomes `({x2 ~ univ}, [])`
    */
  private def trivial(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq

    def error(): Option[(List[Equation], SetSubstitution)] = {
      Some(List(eq.toUnsolvable), SetSubstitution.empty)
    }

    def success(): Option[(List[Equation], SetSubstitution)] = {
      Some(Nil, SetSubstitution.empty)
    }

    (f1, f2) match {
      // Equations that are solved.
      case (Univ, Univ) => success()
      case (Empty, Empty) => success()
      case (Cst(c1), Cst(c2)) if c1 == c2 => success()
      case (Var(x1), Var(x2)) if x1 == x2 => success()
      case (ElemSet(e1), ElemSet(e2)) if e1 == e2 => success()
      // Equations that are Unsolvable.
      case (Univ, Empty) => error()
      case (Univ, ElemSet(_)) => error()
      case (Univ, Cst(_)) => error()
      case (Empty, Univ) => error()
      case (Empty, ElemSet(_)) => error()
      case (Empty, Cst(_)) => error()
      case (ElemSet(_), Univ) => error()
      case (ElemSet(e), Empty) if e.nonEmpty => error()
      case (ElemSet(i1), ElemSet(i2)) if i1 != i2 => error()
      case (ElemSet(_), Cst(_)) => error()
      case (Cst(_), Univ) => error()
      case (Cst(_), Empty) => error()
      case (Cst(_), ElemSet(_)) => error()
      case (Cst(c1), Cst(c2)) if c1 != c2 => error()
      // Equations that are not trivial.
      case _ => None // Cannot do anything.
    }
  }

  /**
    * Solves equations of ground assignments to variables (e.g. `x ~ c1 ∪ e2`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> f])`
    *   - `!x ~ f` where [[SetFormula.isGround]] on `f` is true, becomes `({}, [x -> !f])`
    *   - `f1 ∩ f2 ∩ .. ~ univ` becomes `({f1 ~ univ, f2 ~ univ, ..}, [])`
    *   - `f1 ∪ f2 ∪ .. ~ empty` becomes `({f1 ~ empty, f2 ~ empty, ..}, [])`
    *   - `f1 ~ f2` where [[SetFormula.isGround]] is true on both sides, becomes `({}, [])` if it
    *     holds or `({f1 ~error f2}, [])` if it does not.
    *
    * This also applies to the symmetric equations.
    */
  private def constantAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, loc) = eq
    (f1, f2) match {
      // x ~ f, where f is ground
      // ---
      // {},
      // [x -> f]
      case (Var(x), f) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, Var(x)) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f is ground
      // ---
      // {},
      // [x -> !f]
      case (Compl(Var(x)), f) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(Var(x))) if f.isGround =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // f1 ∩ f2 ∩ .. ~ univ
      // ---
      // {f1 ~ univ, f2 ~ univ, ..},
      // []
      case (inter@Inter(_, _, _, _, _, _, _), Univ) =>
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        Some(eqs, SetSubstitution.empty)

      // Symmetric case.
      case (Univ, inter@Inter(_, _, _, _, _, _, _)) =>
        val eqs = inter.mapSubformulas(Equation.mk(_, Univ, loc))
        Some(eqs, SetSubstitution.empty)

      // f1 ∪ f2 ∪ .. ~ empty
      // ---
      // {f1 ~ empty, f2 ~ empty, ..},
      // []
      case (union@Union(_), Empty) =>
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        Some(eqs, SetSubstitution.empty)

      // Symmetric Case.
      case (Empty, union@Union(_)) =>
        val eqs = union.mapSubformulas(Equation.mk(_, Empty, loc))
        Some(eqs, SetSubstitution.empty)

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves variable alias equations (e.g. `x1 ~ x2`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x1 ~ x1` becomes `({}, [])`
    *   - `x1 ~ x2` becomes `({}, [x1 -> x2])`
    *   - `!x1 ~ !x1` becomes `({}, [])`
    *   - `!x1 ~ !x2` becomes `({}, [x1 -> x2])`
    *
    * There is a binding-bias towards lower variables, such that `x1 ~ x2` and `x2 ~ x1` both
    * become `({}, [x1 -> x2])`.
    */
  private def variableAlias(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      // x1 ~ x1
      // ---
      // {},
      // []
      case (Var(x), Var(y)) if x == y =>
        Some(Nil, SetSubstitution.empty)

      // x1 ~ x2
      // ---
      // {},
      // [x1 -> x2]
      case (x0@Var(_), y0@Var(_)) =>
        val (x, y) = if (x0.x > y0.x) (y0, x0) else (x0, y0)
        Some(Nil, SetSubstitution.singleton(x.x, y))

      // !x1 ~ !x1
      // ---
      // {},
      // []
      case (Compl(Var(x)), Compl(Var(y))) if x == y =>
        Some(Nil, SetSubstitution.empty)

      // !x1 ~ !x2
      // ---
      // {},
      // [x1 -> x2]
      case (Compl(x0@Var(_)), Compl(y0@Var(_))) =>
        // Make this rule stable on symmetric equations.
        val (x, y) = if (x0.x < y0.x) (x0, y0) else (y0, x0)
        Some(Nil, SetSubstitution.singleton(x.x, y))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves non-recursive variable assignments (e.g. `x1 ~ x2 ∪ c4`).
    *
    * If no progress can be made, [[None]] is returned.
    *
    *   - `x ~ f` where `f` does not contain `x` becomes `({}, [x -> f])`
    *   - `!x ~ f` where `f` does not contain `x` becomes `({}, [x -> !f])`
    *
    * This also applies to the symmetric equations.
    */
  private def variableAssignment(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    val Equation(f1, f2, _, _) = eq
    (f1, f2) match {
      // x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> f]
      case (v@Var(x), f) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // Symmetric case.
      case (f, v@Var(x)) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, f))

      // !x ~ f, where f does not contain x
      // ---
      // {},
      // [x -> !f]
      case (Compl(v@Var(x)), f) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      // Symmetric case.
      case (f, Compl(v@Var(x))) if !f.contains(v) =>
        Some(Nil, SetSubstitution.singleton(x, mkCompl(f)))

      case _ =>
        // Cannot do anything.
        None
    }
  }

  /**
    * Solves equations using successive-variable-elimination, i.e. exhaustive instantiation.
    *
    * SVE can always make progress, so [[None]] is never returned.
    *
    * Always returns no equations or `eq` marked as [[Equation.Status.Unsolvable]] or
    * [[Equation.Status.Timeout]].
    */
  private def sve(eq: Equation): Option[(List[Equation], SetSubstitution)] = {
    implicit val alg: BoolAlg[ZhegalkinExpr] = ZhegalkinAlgebra
    val f1 = Zhegalkin.toZhegalkin(eq.f1)
    val f2 = Zhegalkin.toZhegalkin(eq.f2)
    val q = alg.mkXor(f1, f2)
    val fvs = alg.freeVars(q).toList
    try {
      val subst = SveAlgorithm.successiveVariableElimination(q, fvs)
      val m = subst.m.toList.map {
        case (x, e) => x -> Zhegalkin.toSetFormula(e)
      }.toMap
      Some(Nil, SetSubstitution(m))
    } catch {
      case _: BoolUnificationException =>
        Some(List(eq.toUnsolvable), SetSubstitution.empty)
    }
  }

  //
  // Checking and Debugging.
  //

  /** Returns a multiline string of the given [[Equation]]s and [[SetSubstitution]]. */
  def stateString(eqs: List[Equation], subst: SetSubstitution): String = {
    val sb = new StringBuilder()
    sb.append("Equations:\n")
    for (eq <- eqs) sb.append(s"  $eq\n")
    sb.append(subst)
    sb.append("\n")
    sb.toString
  }
}
