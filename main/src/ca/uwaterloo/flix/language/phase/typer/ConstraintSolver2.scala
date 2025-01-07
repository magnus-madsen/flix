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
import ca.uwaterloo.flix.language.phase.typer.TypeReduction2.reduce
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

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
  class Soup private(private val constrs: List[TypeConstraint2], private val tree: SubstitutionTree) {

    /**
      * Transforms the constraint set by applying a one-to-many constraint function.
      */
    def flatMap(f: TypeConstraint2 => List[TypeConstraint2]): Soup = {
      val newConstrs = constrs.flatMap(f)
      new Soup(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-one constraint function.
      */
    def map(f: TypeConstraint2 => TypeConstraint2): Soup = {
      val newConstrs = constrs.map(f)
      new Soup(newConstrs, tree)
    }

    /**
      * Transforms the constraint set by applying a one-to-many constraint function,
      * composing the result with the substitution tree.
      */
    def flatMapSubst(f: TypeConstraint2 => (List[TypeConstraint2], SubstitutionTree)): Soup = {
      val (newConstrs, moreTree) = foldSubstitution(constrs)(f)
      new Soup(newConstrs, moreTree @@ tree)
    }

    /**
      * Returns the constraints and substitution tree.
      */
    def get: (List[TypeConstraint2], SubstitutionTree) = (constrs, tree)

    /**
      * Returns the constraints and the root substitution of the substitution tree.
      */
    def getShallow: (List[TypeConstraint2], Substitution) = (constrs, tree.root)

    /**
      * Sorts the constraints using some heuristics.
      */
    def sort(): Soup = {
      def rank(c: TypeConstraint2): (Int, Int) = c match {
        case TypeConstraint2.Purification(_, _, _, _, _) => (0, 0)
        case TypeConstraint2.Equality(_: Type.Var, Type.Pure, _) => (0, 0)
        case TypeConstraint2.Equality(Type.Pure, _: Type.Var, _) => (0, 0)
        case TypeConstraint2.Equality(tpe1, tpe2, _) if tpe1.typeVars.isEmpty && tpe2.typeVars.isEmpty => (0, 0)
        case TypeConstraint2.Equality(tvar1: Type.Var, tvar2: Type.Var, _) if tvar1 != tvar2 => (0, 0)
        case TypeConstraint2.Equality(tvar1: Type.Var, tpe2, _) if !tpe2.typeVars.contains(tvar1) => (1, 0)
        case TypeConstraint2.Equality(tpe1, tvar2: Type.Var, _) if !tpe1.typeVars.contains(tvar2) => (1, 0)
        case TypeConstraint2.Equality(tpe1, tpe2, _) =>
          // We want to resolve type variables to types before looking at effects.
          // Hence, we punish effect variable by a factor 5.
          val punishment = 5

          val tvs1 = tpe1.typeVars.count(_.kind == Kind.Star)
          val tvs2 = tpe2.typeVars.count(_.kind == Kind.Star)
          val evs1 = tpe1.typeVars.count(_.kind == Kind.Eff)
          val evs2 = tpe2.typeVars.count(_.kind == Kind.Eff)
          (2, (tvs1 + tvs2) + punishment * (evs1 + evs2))
        case TypeConstraint2.Trait(_, _, _) => (3, 0)
      }

      // Performance: We want to avoid allocation if the soup is empty or has just one element.
      constrs match {
        case Nil => this
        case _ :: Nil => this
        case _ => new Soup(constrs.sortBy(rank), tree)
      }
    }

    def tap(f: List[TypeConstraint2] => Unit): Soup = {
      f(constrs)
      this
    }
  }

  object Soup {
    /**
      * Creates a new [[Soup]] from the given constraints.
      */
    def of(constrs: List[TypeConstraint2]): Soup = new Soup(constrs, SubstitutionTree.empty)
  }

  /**
    * Unifies the given type fully, reducing all generated constraints.
    *
    * Returns None if the type are not unifiable.
    */
  def fullyUnify(tpe1: Type, tpe2: Type, scope: Scope, renv: RigidityEnv)(implicit eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): Option[Substitution] = {
    // unification is now defined as taking a single constraint and applying rules until it's done
    val constr = TypeConstraint2.Equality(tpe1, tpe2, SourceLocation.Unknown)
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
  def solveAll(constrs0: List[TypeConstraint2])(implicit scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], SubstitutionTree) = {
    var constrs = constrs0
    var subst = SubstitutionTree.empty
    var progressMade = true
    while (progressMade) {
      val progress = Progress()
      val (newConstrs, newSubst) = solveOne(constrs, progress)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = progress.query()
    }
    (constrs, subst)
  }

  /**
    * Solves the given constraint set as far as possible.
    *
    * The constraint set must contain only equality constraints.
    */
  def solveAllTypes(constrs0: List[TypeConstraint2])(implicit scope: Scope, renv: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      val progress: Progress = Progress()
      val (newConstrs, newSubst) = solveOneTypes(constrs, progress)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = progress.query()
    }
    (constrs, subst)
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    */
  private def solveOne(constrs: List[TypeConstraint2], progress: Progress)(implicit scope: Scope, renv: RigidityEnv, trenv: TraitEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], SubstitutionTree) = {
    Soup.of(constrs)
      .tap(_ => println("Original"))
      .tap(cs => println(cs.length))

      .flatMap(breakDownConstraints(_, progress))
      .tap(_ => println("after breakDownConstraints"))
      .tap(cs => println(cs.length))

      .flatMap(eliminateIdentities(_, progress))
      .tap(_ => println("after eliminateIdentities"))
      .tap(cs => println(cs.length))

      .map(reduceTypes(_, progress))
      .tap(_ => println("after reduceTypes"))
      .tap(cs => println(cs.length))

      .map(purifyEmptyRegion)
      .tap(_ => println("after purifyEmptyRegion"))
      .tap(cs => println(cs.length))

      .flatMapSubst(makeSubstitution(_, progress))
      .tap(_ => println("after makeSubstitution"))
      .tap(cs => println(cs.length))
      .tap(cs => cs.foreach(println))

      .flatMapSubst(effectUnification(_, progress))
      .tap(_ => println("after effectUnification"))
      .tap(cs => println(cs.length))

      .flatMapSubst(recordUnification(_, progress))
      .tap(_ => println("after recordUnification"))
      .tap(cs => println(cs.length))

      .flatMapSubst(schemaUnification(_, progress))
      .tap(_ => println("after schemaUnification"))
      .tap(cs => println(cs.length))

      .flatMap(contextReduction(_, progress))
      .tap(_ => println("after contextReduction"))
      .tap(cs => println(cs.length))

      .get
  }

  /**
    * Iterates once over all reduction rules to apply them to the constraint set.
    *
    * Only applies rules relevant to equality constraints.
    */
  private def solveOneTypes(constrs: List[TypeConstraint2], progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], Substitution) = {
    Soup.of(constrs)
      .flatMap(breakDownConstraints(_, progress))
      .flatMap(eliminateIdentities(_, progress))
      .sort()
      .map(reduceTypes(_, progress))
      .flatMapSubst(effectUnification(_, progress))
      .flatMapSubst(makeSubstitution(_, progress))
      .flatMapSubst(recordUnification(_, progress))
      .flatMapSubst(schemaUnification(_, progress))
      .getShallow
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
  private def purifyEmptyRegion(constr: TypeConstraint2): TypeConstraint2 = constr match {
    case TypeConstraint2.Purification(sym, eff1, eff2, Nil, loc) =>
      val purified = Substitution.singleton(sym, Type.Pure)(eff2)
      TypeConstraint2.Equality(eff1, purified, loc)
    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val nested = nested0.map(purifyEmptyRegion)
      TypeConstraint2.Purification(sym, eff1, eff2, nested, loc)
    case c: TypeConstraint2.Trait => c
    case c: TypeConstraint2.Equality => c
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
  private def breakDownConstraints(constr: TypeConstraint2, progress: Progress): List[TypeConstraint2] = constr match {
    case TypeConstraint2.Equality(t1@Type.Apply(tpe11, tpe12, _), t2@Type.Apply(tpe21, tpe22, _), loc) if isSyntactic(t1.kind) && isSyntactic(t2.kind) =>
      progress.markProgress()
      List(TypeConstraint2.Equality(tpe11, tpe21, loc), TypeConstraint2.Equality(tpe12, tpe22, loc))

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val nested = nested0.flatMap(breakDownConstraints(_, progress))
      List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))

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
  private def eliminateIdentities(constr: TypeConstraint2, progress: Progress): List[TypeConstraint2] = constr match {
    case c@TypeConstraint2.Equality(tpe1, tpe2, loc) =>
      if (tpe1 == tpe2) {
        progress.markProgress()
        Nil
      } else {
        List(c)
      }

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val nested = nested0.flatMap(eliminateIdentities(_, progress))
      List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))

    case c: TypeConstraint2.Trait =>
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
  private def contextReduction(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv0: RigidityEnv, trenv: TraitEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): List[TypeConstraint2] = constr match {
    // Case 1: Non-trait constraint. Do nothing.
    case c: TypeConstraint2.Equality => List(c)

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val nested = nested0.flatMap(contextReduction(_, progress)(scope.enter(sym), renv0, trenv, eqenv, flix))
      List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))

    // Case 2: Trait constraint. Perform context reduction.
    case c@TypeConstraint2.Trait(sym, tpe, loc) =>

      // Get all the instances from the context
      val insts = trenv.getInstances(sym)

      // Find the instance that matches
      val matches = insts.flatMap {
        case Instance(instTpe, instConstrs) =>
          // We fully rigidify `tpe`, because we need the substitution to go from instance type to constraint type.
          // For example, if our constraint is ToString[Map[Int32, a]] and our instance is ToString[Map[k, v]],
          // then we want the substitution to include "v -> a" but NOT "a -> v".
          val renv = tpe.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))

          // Instantiate all the instance constraints according to the substitution.
          fullyUnify(tpe, instTpe, scope, renv).map {
            case subst => instConstrs.map(subst.apply)
          }
      }

      // TODO CONSTR-SOLVER-2 ought to be exactly 0 or 1; should check in Resolver
      matches match {
        // Case 1: No match. Throw the constraint back in the pool.
        case Nil => List(c)

        // Case 2: One match. Use the instance constraints.
        case newConstrs :: Nil => newConstrs.map(traitConstraintToTypeConstraint)

        // Case 3: Multiple matches. Throw the constraint back in the pool.
        // TODO CONSTR-SOLVER-2 Right resiliency strategy?
        case _ :: _ :: _ => List(c)
      }
  }

  /**
    * Performs effect unification on the given type constraint.
    */
  private def effectUnification(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, flix: Flix): (List[TypeConstraint2], SubstitutionTree) = constr match {
    case c@TypeConstraint2.Equality(tpe1, tpe2, loc) if tpe1.kind == Kind.Eff && tpe2.kind == Kind.Eff =>
      EffUnification3.unify(tpe1, tpe2, scope, renv) match {
        case Result.Ok(Some(subst)) => (Nil, SubstitutionTree(subst, Map()))
        case _ => (List(c), SubstitutionTree.empty)
      }

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val (nested, branch) = foldSubstitution(nested0)(effectUnification(_, progress)(scope.enter(sym), renv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs record row unification on the given type constraint.
    */
  private def recordUnification(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], SubstitutionTree) = constr match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) if tpe1.kind == Kind.RecordRow && tpe2.kind == Kind.RecordRow =>
      RecordConstraintSolver2.solve(tpe1, tpe2, scope, renv, loc)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree(subst, Map()))
      }

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val (nested, branch) = foldSubstitution(nested0)(recordUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs schema row unification on the given type constraint.
    */
  private def schemaUnification(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): (List[TypeConstraint2], SubstitutionTree) = constr match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) if tpe1.kind == Kind.SchemaRow && tpe2.kind == Kind.SchemaRow =>
      SchemaConstraintSolver2.solve(tpe1, tpe2, scope, renv, loc)(progress, flix) match {
        case (constrs, subst) => (constrs, SubstitutionTree(subst, Map()))
      }

    case TypeConstraint2.Purification(sym, eff1, eff2, nested0, loc) =>
      val (nested, branch) = foldSubstitution(nested0)(schemaUnification(_, progress)(scope.enter(sym), renv, eqEnv, flix))
      val tree = SubstitutionTree.oneBranch(sym, branch)
      val cs = List(TypeConstraint2.Purification(sym, eff1, eff2, nested, loc))
      (cs, tree)

    case c => (List(c), SubstitutionTree.empty)
  }

  /**
    * Performs reduction on the types in the given type constraints.
    */
  // (redU)
  private def reduceTypes(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv: RigidityEnv, eqenv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], flix: Flix): TypeConstraint2 = constr match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) => TypeConstraint2.Equality(reduce(tpe1, scope, renv)(progress, eqenv, flix), reduce(tpe2, scope, renv)(progress, eqenv, flix), loc)
    case TypeConstraint2.Trait(sym, tpe, loc) => TypeConstraint2.Trait(sym, reduce(tpe, scope, renv)(progress, eqenv, flix), loc)
    case TypeConstraint2.Purification(sym, eff1, eff2, nested, loc) => TypeConstraint2.Purification(sym, reduce(eff1, scope, renv)(progress, eqenv, flix), reduce(eff2, scope, renv)(progress, eqenv, flix), nested, loc)
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
  // TODO CONSTR-SOLVER-2 make private
  def makeSubstitution(constr: TypeConstraint2, progress: Progress)(implicit scope: Scope, renv: RigidityEnv): (List[TypeConstraint2], SubstitutionTree) = constr match {
    case TypeConstraint2.Equality(Type.Var(sym, _), tpe2, loc) if !renv.isRigid(sym) && sym.kind == tpe2.kind =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe2))

    case TypeConstraint2.Equality(tpe1, Type.Var(sym, _), loc) if !renv.isRigid(sym) && tpe1.kind == sym.kind =>
      progress.markProgress()
      (Nil, SubstitutionTree.singleton(sym, tpe1))

    case c: TypeConstraint2.Equality => (List(c), SubstitutionTree.empty)

    case c: TypeConstraint2.Trait => (List(c), SubstitutionTree.empty)

    case TypeConstraint2.Purification(sym, eff1, eff2, nested, loc) =>
      val (cs, branch) = foldSubstitution(nested)(makeSubstitution(_, progress)(scope.enter(sym), renv))
      val c = TypeConstraint2.Purification(sym, eff1, eff2, cs, loc)
      val tree = SubstitutionTree.oneBranch(sym, branch)
      (List(c), tree)
  }

  /**
    * Folds over the constraints with the given constraint/substitution processing function.
    *
    * Ensures that the resulting substitution has been applied to all constraints.
    */
  private def foldSubstitution(constrs: List[TypeConstraint2])(f: TypeConstraint2 => (List[TypeConstraint2], SubstitutionTree)): (List[TypeConstraint2], SubstitutionTree) = {
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
    */
  def traitConstraintToTypeConstraint(constr: TraitConstraint): TypeConstraint2 = constr match {
    case TraitConstraint(head, arg, loc) => TypeConstraint2.Trait(head.sym, arg, loc)
  }

  /**
    * Converts a type constraint to a broad equality constraint.
    *
    * The type constraint must be an equality constraint.
    */
  def unsafeTypeConstraintToBroadEqualityConstraint(constr: TypeConstraint2): BroadEqualityConstraint = constr match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) => BroadEqualityConstraint(tpe1, tpe2)
    case c => throw InternalCompilerException("unexpected constraint: " + c, SourceLocation.Unknown)
  }

  /**
    * Returns true if the kind should be unified syntactically.
    */
  @tailrec
  private def isSyntactic(k: Kind): Boolean = k match {
    case Kind.Wild => false // MATT ?
    case Kind.WildCaseSet => false
    case Kind.Star => true
    case Kind.Eff => false
    case Kind.Bool => false
    case Kind.RecordRow => false
    case Kind.SchemaRow => false
    case Kind.Predicate => false
    case Kind.Jvm => false
    case Kind.CaseSet(_) => false
    case Kind.Arrow(_, k2) => isSyntactic(k2)
    case Kind.Error => false
  }
}
