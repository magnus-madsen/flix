/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatScheme}
import ca.uwaterloo.flix.language.phase.typer.*
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Substitution, TraitEnv}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.ListMap

object Scheme {

  /**
    * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
    */
  def partiallyInstantiate(sc: Scheme, quantifier: Symbol.KindedTypeVarSym, value: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      if (!quantifiers.contains(quantifier)) {
        throw InternalCompilerException("Quantifier not in scheme.", loc)
      }
      val subst = Substitution.singleton(quantifier, value)
      val newTconstrs = tconstrs.map(subst.apply)
      val newEconstrs = econstrs.map(subst.apply)
      val newBase = subst(base)
      generalize(newTconstrs, newEconstrs, newBase, RigidityEnv.empty)
  }

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme, loc: SourceLocation)(implicit scope: Scope, flix: Flix): (List[TraitConstraint], List[EqualityConstraint], Type, Map[Symbol.KindedTypeVarSym, Type.Var]) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val substMap = sc.quantifiers.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        macc + (tvar -> Type.freshVar(tvar.kind, loc, VarText.Absent))
    }
    val freshVars = substMap.map { case (k, v) => k.id -> v }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`.
      *
      * Replaces all source locations by `loc`.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def visitType(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, _) =>
        // Performance: Reuse tpe0, if possible.
        freshVars.getOrElse(sym.id, tpe0)

      case Type.Cst(_, _) =>
        // Performance: Reuse tpe0.
        tpe0

      case app@Type.Apply(tpe1, tpe2, loc) =>
        val t1 = visitType(tpe1)
        val t2 = visitType(tpe2)
        // Performance: Reuse tpe0, if possible.
        app.renew(t1, t2, loc)

      case Type.Alias(sym, args, tpe, _) =>
        // Performance: Few aliases, not worth optimizing.
        Type.Alias(sym, args.map(visitType), visitType(tpe), loc)

      case Type.AssocType(sym, args, kind, _) =>
        // // Performance: Few associated types, not worth optimizing.
        Type.AssocType(sym, args.map(visitType), kind, loc)

      case Type.JvmToType(tpe, loc) =>
        Type.JvmToType(visitType(tpe), loc)

      case Type.JvmToEff(tpe, loc) =>
        Type.JvmToEff(visitType(tpe), loc)

      case Type.UnresolvedJvmType(member, loc) =>
        Type.UnresolvedJvmType(member.map(visitType), loc)

      case Type.GetEff(action, tpe, loc) =>
        Type.GetEff(action, visitType(tpe), loc)
    }

    val newBase = visitType(baseType)

    val newTconstrs = sc.tconstrs.map {
      case TraitConstraint(head, tpe0, loc) =>
        val tpe = tpe0.map(visitType)
        TraitConstraint(head, tpe, loc)
    }

    val newEconstrs = sc.econstrs.map {
      case EqualityConstraint(symUse, tpe1, tpe2, loc) =>
        EqualityConstraint(symUse, visitType(tpe1), visitType(tpe2), loc)
    }

    (newTconstrs, newEconstrs, newBase, substMap)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], tpe0: Type, renv: RigidityEnv)(implicit scope: Scope): Scheme = {
    val tvars = tpe0.typeVars ++ tconstrs.flatMap(tconstr => tconstr.arg.typeVars) ++ econstrs.flatMap(econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars)
    val quantifiers = renv.getFlexibleVarsOf(tvars.toList)
    Scheme(quantifiers.map(_.sym), tconstrs, econstrs, tpe0)
  }

  /**
    * Returns `true` if the given schemes are equivalent.
    */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, traitEnv: TraitEnv, eqEnv: EqualityEnv)(implicit scope: Scope, flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, traitEnv, eqEnv) && lessThanEqual(sc2, sc1, traitEnv, eqEnv)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    *
    * Θₚ [T/α₂]π₂ ⊫ₑ {π₁, τ₁ = [T/α₂]τ₂} ⤳! ∙ ; R
    * T new constructors
    * ---------------------------------------
    * Θₚ ⊩ (∀α₁.π₁ ⇒ τ₁) ≤ (∀α₂.π₂ ⇒ τ₂)
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, tenv0: TraitEnv, eenv0: EqualityEnv)(implicit scope: Scope, flix: Flix): Boolean = {

    // Instantiate sc2, creating [T/α₂]π₂ and [T/α₂]τ₂
    // We use the top scope because this function is only used for comparing schemes, which are at top-level.
    val (cconstrs2_0, econstrs2_0, tpe2_0, _) = Scheme.instantiate(sc2, SourceLocation.Unknown)(Scope.Top, flix)

    // Resolve what we can from the new econstrs
    val tconstrs2_0 = econstrs2_0.map { case EqualityConstraint(symUse, t1, t2, loc) => TypeConstraint.Equality(Type.AssocType(symUse, t1, t2.kind, loc), t2, Provenance.Match(t1, t2, loc)) }
    val (econstrs2_1, subst) = ConstraintSolver2.solveAllTypes(tconstrs2_0)(scope, RigidityEnv.empty, eenv0, flix)

    // Anything we didn't solve must be a standard equality constraint
    // Apply the substitution to the new scheme 2
    val econstrs2 = econstrs2_1.map {
      case TypeConstraint.Equality(Type.AssocType(symUse, t1, _, _), t2, prov) => EqualityConstraint(symUse, subst(t1), subst(t2), prov.loc)
      case _ => throw InternalCompilerException("unexpected constraint", SourceLocation.Unknown)
    }
    val tpe2 = subst(tpe2_0)
    val cconstrs2 = cconstrs2_0.map {
      case TraitConstraint(head, arg, loc) =>
        // should never fail
        val t = TypeReduction2.reduce(subst(arg), scope, RigidityEnv.empty)(Progress(), eenv0, flix)
        TraitConstraint(head, t, loc)
    }

    // Add sc2's constraints to the environment
    val eenv = ConstraintSolverInterface.expandEqualityEnv(eenv0, econstrs2)
    val cenv = ConstraintSolverInterface.expandTraitEnv(tenv0, cconstrs2)

    // Mark all the constraints from sc2 as rigid
    val tvars = cconstrs2.flatMap(_.arg.typeVars) ++
      econstrs2.flatMap { econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars } ++
      tpe2.typeVars
    val renv = tvars.foldLeft(RigidityEnv.empty) { case (r, tvar) => r.markRigid(tvar.sym) }

    // Check that the constraints from sc1 hold
    // And that the bases unify
    val cconstrs = sc1.tconstrs.map { case TraitConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg, loc) }
    val econstrs = sc1.econstrs.map { case EqualityConstraint(symUse, t1, t2, loc) => TypeConstraint.Equality(Type.AssocType(symUse, t1, t2.kind, loc), t2, Provenance.Match(t1, t2, loc)) }
    val baseConstr = TypeConstraint.Equality(sc1.base, tpe2, Provenance.Match(sc1.base, tpe2, SourceLocation.Unknown))
    ConstraintSolver2.solveAll(baseConstr :: cconstrs ::: econstrs, SubstitutionTree.shallow(subst))(scope, renv, cenv, eenv, flix) match {
      // We succeed only if there are no leftover constraints
      case (Nil, _) => true
      case (_ :: _, _) => false
    }

  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatSchemeWithOptions(this, FormatOptions.Internal)
  }

}
