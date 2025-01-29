/*
 * Copyright 2024 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.TypeReduction2.reduce
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification
import ca.uwaterloo.flix.util.collection.ListMap

import scala.annotation.tailrec

/**
  * The constraint solver reduces a collection of constraints by iteratively applying reduction rules.
  * The result of constraint solving is a substitution and a list of constraints that could not be resolved.
  */
object ConstraintSolver2 {

  /**
    * A container for a constraint set and a substitution tree.
    *
    * This class provides several methods for manipulating the constraints.
    */
  // Invariant: the constraints always have the tree applied
  final class Soup(private val constrs: List[TypeConstraint], private val tree: SubstitutionTree) {

    /**
      * Transforms the constraint set by applying a one-to-many constraint function.
      */
    def flatMap(f: TypeConstraint => List[TypeConstraint]): Soup = {
      val newConstrs = constrs.flatMap(f)
      renew(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-one constraint function.
      */
    def map(f: TypeConstraint => TypeConstraint): Soup = {
      val newConstrs = constrs.map(f)
      renew(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-many constraint function,
      * composing the result with the substitution tree.
      */
    def flatMapSubst(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = foldSubstitution(constrs)(f)
      renew(newConstrs, moreTree @@ tree)
    }

    /**
      * Transforms the entire set of constraints with the given many-to-many constraint function.
      */
    def blockApply(f: List[TypeConstraint] => (List[TypeConstraint], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = f(constrs)
      renew(newConstrs, moreTree @@ tree)
    }

    /**
      * Returns the constraints and substitution tree.
      */
    def get: (List[TypeConstraint], SubstitutionTree) = (constrs, tree)

    /**
      * Performs the function `f` on the constraints until no progress is made.
      */
    @tailrec
    def exhaustively(progress: Progress)(f: (Soup, Progress) => Soup): Soup = {
      val innerProgress = Progress()
      val res = f(this, innerProgress)
      if (innerProgress.query()) {
        progress.markProgress()
        res.exhaustively(progress)(f)
      } else {
        this
      }
    }

    /**
      * Creates a new [[Soup]], but reuses this one if the arguments are the same.
      */
    private def renew(newConstrs: List[TypeConstraint], newTree: SubstitutionTree): Soup = {
      if ((constrs eq newConstrs) && (tree eq newTree)) {
        this
      } else {
        new Soup(newConstrs, newTree)
      }
    }

  }

  /**
    * Unifies the given type fully, reducing all generated constraints.
    *
    * Returns None if the type are not unifiable.
    */
  def fullyUnify(tpe1: Type, tpe2: Type, scope: Scope, renv: RigidityEnv)(implicit eqenv: EqualityEnv, flix: Flix): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint.Equality(tpe1, tpe2, Provenance.Match(tpe1, tpe2, SourceLocation.Unknown))
    implicit val r: RigidityEnv = renv
    implicit val s: Scope = scope
    solveAllTypes(List(constr)) match {
      // Case 1: No constraints left. Success.
      case (Nil, subst) => Some(subst)

      // Case 2: Leftover constraints. Failure
      case (_ :: _, _) => None
    }
  }

  /**
    * Solves the given constraint set as far as possible.
    */
  def solveAll(constrs0: List[TypeConstraint], initialSubst: SubstitutionTree)(implicit scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = {
    val constrs = constrs0.map(initialSubst.apply)
    val soup = new Soup(constrs, initialSubst)
    val progress = Progress()
    val res = soup.exhaustively(progress)(solveOne)
    res.get
  }

  /**
    * Solves the given constraint set as far as possible.
    *
    * The constraint set must contain only equality constraints.
    */
  def solveAllTypes(constrs0: List[TypeConstraint])(implicit scope: Scope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): (List[TypeConstraint], Substitution) = {
    solveAll(constrs0, SubstitutionTree.empty)(scope, renv, TraitEnv(Map.empty), eqenv, flix) match {
      case (constrs, subst) => (constrs, subst.root)
    }
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    */
  private def solveOne(soup: Soup, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): Soup = {
    soup
      .exhaustively(progress) {
        (soup, progress) =>
          soup
            .exhaustively(progress) {
              (s, p) => s.flatMap(breakDownConstraints(_, p))
            }
            .flatMap(eliminateIdentities(_, progress))
            .map(reduceTypes(_, progress))
            .flatMapSubst(makeSubstitution(_, progress))
            .exhaustively(progress) {
              (s, p) => s.flatMap(breakDownConstraints(_, p))
            }
            .flatMap(eliminateIdentities(_, progress))
            .map(reduceTypes(_, progress))
            .exhaustively(progress) {
              (s, p) => s.flatMap(breakDownConstraints(_, p))
            }
            .flatMap(eliminateIdentities(_, progress))
            .map(reduceTypes(_, progress))
            .flatMapSubst(recordUnification(_, progress))
            .flatMapSubst(schemaUnification(_, progress))
            .map(purifyEmptyRegion(_, progress))
      }
      .blockApply(blockEffectUnification(_, progress))
      .flatMapSubst(caseSetUnification(_, progress))
      .flatMapSubst(booleanUnification(_, progress))
      .flatMap(contextReduction(_, progress))
  }

  /**
    * Purifies empty regions in the constraint set.
    *
    * {{{
    *   φ₁ ~ φ₂[r ↦ Pure] ∧ ∅
    * }}}
    *
    * becomes
    *
    * {{{
    *   φ₁ ~ φ₃{r ↦ Pure}
    * }}}
    *
    * where `{ }` represents actual substitution
    */
  private def purifyEmptyRegion(constr: TypeConstraint, progress: Progress): TypeConstraint = constr match {
    case TypeConstraint.Purification(sym, eff1, eff2, prov, Nil) =>
      progress.markProgress()
      val purified = Type.purifyRegion(eff2, sym)
      TypeConstraint.Equality(eff1, purified, prov)
    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = nested0.map(purifyEmptyRegion(_, progress))
      TypeConstraint.Purification(sym, eff1, eff2, prov, nested)
    case c: TypeConstraint.Trait => c
    case c: TypeConstraint.Equality => c
  }

  /**
    * Breaks down equality constraints over syntactic types.
    *
    * {{{
    *   τ₁[τ₂] ~ τ₃[τ₄]
    * }}}
    *
    * becomes
    *
    * {{{
    *   τ₁ ~ τ₃, τ₂[τ₄]
    * }}}
    */
  // (appU)
  private def breakDownConstraints(constr: TypeConstraint, progress: Progress): List[TypeConstraint] = constr match {
    case TypeConstraint.Equality(t1@Type.Apply(tpe11, tpe12, _), t2@Type.Apply(tpe21, tpe22, _), prov) if isSyntactic(t1.kind) && isSyntactic(t2.kind) =>
      progress.markProgress()
      List(TypeConstraint.Equality(tpe11, tpe21, prov), TypeConstraint.Equality(tpe12, tpe22, prov))

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = nested0.flatMap(breakDownConstraints(_, progress))
      List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))

    case c => List(c)
  }

  /**
    * Eliminates constraints that are the same on the left and right
    *
    * {{{
    *   τ ~ τ
    * }}}
    *
    * becomes
    *
    * {{{
    *   ∅
    * }}}
    */
  // (reflU)
  private def eliminateIdentities(constr: TypeConstraint, progress: Progress): List[TypeConstraint] = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2, _) =>
      if (tpe1 == tpe2) {
        progress.markProgress()
        Nil
      } else {
        List(c)
      }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = nested0.flatMap(eliminateIdentities(_, progress))
      List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))

    case c: TypeConstraint.Trait =>
      List(c)
  }

  /**
    * Performs context reduction on the given type constraint.
    *
    * Removes a constraint T[τ] if the trait environment contains it.
    *
    * Replaces a constraint with its premises if there is an implication for the constraint in the environment.
    *
    * {{{
    *   Ord[List[τ]]
    * }}}
    *
    * becomes
    *
    * {{{
    *   Ord[τ]
    * }}}
    *
    * given
    *
    * {{{
    *   instance Ord[List[a]] with Ord[a]
    * }}}
    */
  private def contextReduction(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv0: RigidityEnv, trenv: TraitEnv, eqenv: EqualityEnv, flix: Flix): List[TypeConstraint] = constr match {
    // Case 1: Non-trait constraint. Do nothing.
    case c: TypeConstraint.Equality => List(c)

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val nested = nested0.flatMap(contextReduction(_, progress)(scope.enter(sym), renv0, trenv, eqenv, flix))
      List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))

    // Case 2: Trait constraint. Perform context reduction.
    case c@TypeConstraint.Trait(sym, tpe, loc) =>

      // Get all the instances from the context
      val insts = trenv.getInstance(sym, tpe)

      // Find the instance that matches
      val matches = insts.flatMap {
        case Instance(instTparams, instTpe0, instConstrs0) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val renv = tpe.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))

          // Refresh the flexible variables in the instance
          // (variables may be rigid if the instance comes from a constraint on the definition)
          val instVarMap = instTparams.map {
            case fromSym => fromSym -> Type.freshVar(fromSym.kind, fromSym.loc)
          }.toMap
          val instSubst = Substitution(instVarMap)
          val instTpe = instSubst(instTpe0)
          val instConstrs = instConstrs0.map(instSubst.apply)

          // Instantiate all the instance constraints according to the substitution.
          fullyUnify(tpe, instTpe, scope, renv).map {
            case subst => instConstrs.map(subst.apply)
          }
      }

      matches match {
        // Case 1: No match. Throw the constraint back in the pool.
        case None => List(c)

        // Case 2: One match. Use the instance constraints.
        case Some(newConstrs) =>
          progress.markProgress()
          newConstrs.map(traitConstraintToTypeConstraint(_, loc))
      }
  }

  /**
    * Performs case set unification on the given constraint.
    */
  private def caseSetUnification(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2, _) => (tpe1.kind, tpe2.kind) match {
      case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
        CaseSetUnification.unify(tpe1, tpe2, renv, sym1.universe, sym1) match {
          case Some(subst) => (Nil, SubstitutionTree.shallow(subst))
          case None => (List(c), SubstitutionTree.empty)
        }
      case _ => (List(c), SubstitutionTree.empty)
    }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(caseSetUnification(_, progress)(scope.enter(sym), renv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs Boolean unification on the given constraint.
    */
  private def booleanUnification(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case c@TypeConstraint.Equality(tpe1, tpe2, _) if tpe1.kind == Kind.Bool && tpe2.kind == Kind.Bool =>
      // BoolUnification is all-or-nothing:
      // Either we get a substitution and have nothing left over
      // Or we have leftovers but the substitution is empty.
      BoolUnification.unify(tpe1, tpe2, renv) match {
        case Some(subst) => (Nil, SubstitutionTree.shallow(subst))
        case None => (List(c), SubstitutionTree.empty)
      }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(booleanUnification(_, progress)(scope.enter(sym), renv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs effect unification on all the given constraints.
    */
  private def blockEffectUnification(constrs0: List[TypeConstraint], progress: Progress)(implicit scope: Scope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = {

    // Separate out the effect unification stuff
    val (eqConstrs, rest0) = constrs0.partitionMap {
      case eq@TypeConstraint.Equality(tpe1, _, _) if tpe1.kind == Kind.Eff => Left(eq)
      case other => Right(other)
    }

    val eqs = eqConstrs.map {
      case TypeConstraint.Equality(tpe1, tpe2, prov) => (tpe1, tpe2, prov.loc)
    }

    // First solve all the top-level constraints together
    val (leftovers1, subst1) = EffUnification3.unifyAll(eqs, scope, renv) match {
      // If we solved everything, then we can use the new substitution.
      case (Nil, subst) =>
        // We only mark progress if there was something to solve.
        if (eqConstrs.nonEmpty) {
          progress.markProgress()
        }
        (Nil, subst)
      // Otherwise, throw away everything.
      case (_ :: _, _) =>
        (eqConstrs, Substitution.empty)
    }

    val tree0 = SubstitutionTree.shallow(subst1)

    // Apply the substitution to the remaining constraints
    val rest1 = rest0.map(tree0.apply)

    // Now we separate the purification constraints and recurse on those individually
    var branches = Map.empty[Symbol.RegionSym, SubstitutionTree]
    val rest = rest1.map {
      // If it's a purification constraint, solve the nested constraints
      // and put the substitution in the tree
      case TypeConstraint.Purification(sym, eff1, eff2_0, prov, nested0) =>
        val nested1 = nested0.map(tree0.apply)
        val (nested, subst2) = blockEffectUnification(nested1, progress)(scope.enter(sym), renv, flix)
        branches = branches + (sym -> subst2)

        // apply the inner substitution to the to-be-purified effect
        val eff2 = subst2.root(eff2_0)
        TypeConstraint.Purification(sym, eff1, eff2, prov, nested)

      // Otherwise no change
      case c => c
    }

    val tree = SubstitutionTree.mk(tree0.root, branches)
    val constrs = leftovers1 ++ rest

    (constrs, tree)
  }

  /**
    * Performs record row unification on the given type constraint.
    */
  private def recordUnification(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) if tpe1.kind == Kind.RecordRow && tpe2.kind == Kind.RecordRow =>
      RecordConstraintSolver.solve(tpe1, tpe2, scope, renv, prov)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree.shallow(subst))
      }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(recordUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs schema row unification on the given type constraint.
    */
  private def schemaUnification(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqEnv: EqualityEnv, flix: Flix): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) if tpe1.kind == Kind.SchemaRow && tpe2.kind == Kind.SchemaRow =>
      SchemaConstraintSolver.solve(tpe1, tpe2, scope, renv, prov)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree.shallow(subst))
      }

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(schemaUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint.Purification(sym, eff1, eff2, prov, nested))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs reduction on the types in the given type constraints.
    */
  // (redU)
  private def reduceTypes(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqenv: EqualityEnv, flix: Flix): TypeConstraint = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, prov) =>
      TypeConstraint.Equality(reduce(tpe1, scope, renv)(progress, eqenv, flix), reduce(tpe2, scope, renv)(progress, eqenv, flix), prov)
    case TypeConstraint.Trait(sym, tpe, loc) =>
      TypeConstraint.Trait(sym, reduce(tpe, scope, renv)(progress, eqenv, flix), loc)
    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested) =>
      TypeConstraint.Purification(sym, reduce(eff1, scope, renv)(progress, eqenv, flix), reduce(eff2, scope, renv)(progress, eqenv, flix), prov, nested.map(reduceTypes(_, progress)(scope.enter(sym), renv, eqenv, flix)))
  }

  /**
    * Builds a substitution from constraints where one side is a free variable.
    *
    * {{{
    *   τ ~ α
    * }}}
    *
    * becomes
    *
    * {{{
    *   α ↦ τ
    * }}}
    */
  // (varU)
  private def makeSubstitution(constr: TypeConstraint, progress: Progress)(implicit scope: Scope, renv: RigidityEnv): (List[TypeConstraint], SubstitutionTree) = constr match {
    case TypeConstraint.Equality(Type.Var(sym, _), tpe2, prov) if canSubstitute(sym, tpe2) =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe2))

    case TypeConstraint.Equality(tpe1, Type.Var(sym, _), prov) if canSubstitute(sym, tpe1) =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe1))

    case c: TypeConstraint.Equality => (List(c), SubstitutionTree.empty)

    case c: TypeConstraint.Trait => (List(c), SubstitutionTree.empty)

    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0) =>
      val (nested, branch) = foldSubstitution(nested0)(makeSubstitution(_, progress)(scope.enter(sym), renv))
      val c = TypeConstraint.Purification(sym, eff1, eff2, prov, nested)
      val tree = SubstitutionTree.oneBranch(sym, branch)
      (List(c), tree)
  }

  /**
    * Returns true if it is valid to create a substitution from the given type variable to the given type.
    */
  private def canSubstitute(sym: Symbol.KindedTypeVarSym, tpe: Type)(implicit scope: Scope, renv: RigidityEnv): Boolean = {
    renv.isFlexible(sym) &&
      sym.kind == tpe.kind &&
      hasIdempotentSubstitution(sym.kind) &&
      !tpe.typeVars.exists { tvar => tvar.sym == sym } &&
      !Type.hasJvmType(tpe)
  }

  /**
    * Returns true if substitutions over the given kind are idempotent.
    *
    * This means that the substitution can be applied multiple times to a type without changing the result.
    */
  private def hasIdempotentSubstitution(k: Kind) = k match {
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.CaseSet(_) => false
    case _ => true
  }

  /**
    * Folds over the constraints with the given constraint/substitution processing function.
    *
    * Ensures that the resulting substitution has been applied to all constraints.
    */
  private def foldSubstitution(constrs: List[TypeConstraint])(f: TypeConstraint => (List[TypeConstraint], SubstitutionTree)): (List[TypeConstraint], SubstitutionTree) = {
    var subst = SubstitutionTree.empty
    val newConstrs = constrs.flatMap {
      constr =>
        val (cs, s) = f(subst(constr))
        subst = s @@ subst
        cs
    }.map(subst.apply) // apply the substitution to all constraints
    (newConstrs, subst)
  }

  /**
    * Converts a syntactic type constraint into a semantic type constraint.
    *
    * Replaces the the location with the given location.
    */
  def traitConstraintToTypeConstraint(constr: TraitConstraint, loc: SourceLocation): TypeConstraint = constr match {
    case TraitConstraint(head, arg, _) => TypeConstraint.Trait(head.sym, arg, loc)
  }

  /**
    * Returns true if the kind should be unified syntactically.
    */
  @tailrec
  private def isSyntactic(k: Kind): Boolean = k match {
    case Kind.Star => true
    case Kind.Predicate => true
    case Kind.Region => true

    case Kind.Arrow(_, k2) => isSyntactic(k2)

    case Kind.Wild => false
    case Kind.WildCaseSet => false
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.RecordRow => false
    case Kind.SchemaRow => false
    case Kind.Jvm => false
    case Kind.CaseSet(_) => false
    case Kind.Error => false
  }
}
