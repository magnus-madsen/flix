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

import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.collection.MapOps

/**
  * The substitution tree represents the substitutions that apply to different region scopes.
  * It is structured as a map rather than a proper tree,
  * since regions are uniquely identified by their region variable.
  *
  * @param root     the substitutions at the top level
  * @param branches a map from region variables to the substitutions for those regions
  */
case class SubstitutionTree(root: Substitution, branches: Map[Symbol.KindedTypeVarSym, SubstitutionTree]) {

  /**
    * Applies this substitution tree to the given type constraint.
    */
  def apply(constr: TypeConstraint2): TypeConstraint2 = {
    if (root.isEmpty) {
      // Performance: The substitution is empty. Nothing to be done.
      return constr
    }

    constr match {
      case TypeConstraint2.Equality(tpe1, tpe2, loc) =>
        // Check whether the substitution has to be applied.
        val t1 = if (tpe1.typeVars.isEmpty) tpe1 else root(tpe1)
        val t2 = if (tpe2.typeVars.isEmpty) tpe2 else root(tpe2)
        // Performance: Reuse this, if possible.
        if ((t1 eq tpe1) && (t2 eq tpe2))
          constr
        else
          TypeConstraint2.Equality(t1, t2, loc)

      case TypeConstraint2.Trait(sym, tpe, loc) =>
        TypeConstraint2.Trait(sym, root(tpe), loc)

      case TypeConstraint2.Purification(sym, eff1, eff2, nested, loc) =>
        // Use the root substitution for the external effects.
        // Use the appropriate branch substitution for the nested constraints.
        // MATT what to do if sym not in branches?
        TypeConstraint2.Purification(sym, root(eff1), root(eff2), nested.map(branches.getOrElse(sym, SubstitutionTree.empty).apply), loc)
    }
  }

  /**
    * Applies the substitution at the root of the substitution tree to the given type.
    */
  def apply(t: Type): Type = root.apply(t)

  /**
    * Composes this substitution tree with the given substitution tree.
    */
  def @@(that: SubstitutionTree): SubstitutionTree = that match {
    case SubstitutionTree(thatRoot, thatBranches) =>
      // Performance: Check if `this` or `that` is empty.
      if (this eq SubstitutionTree.empty) {
        return that
      }
      if (that eq SubstitutionTree.empty) {
        return this
      }
      val newRoot = root @@ thatRoot
      val newBranches = MapOps.unionWith(branches, thatBranches)(_ @@ _)
      SubstitutionTree(newRoot, newBranches)
  }
}

object SubstitutionTree {
  /**
    * The empty substitution tree.
    */
  val empty: SubstitutionTree = SubstitutionTree(Substitution.empty, Map.empty)

  /**
    * Returns a substitution tree mapping one key to one value.
    */
  def singleton(key: Symbol.KindedTypeVarSym, value: Type): SubstitutionTree = SubstitutionTree(Substitution.singleton(key, value), Map.empty)

  /**
    * Returns a substitution tree containing one branch.
    */
  def oneBranch(sym: Symbol.KindedTypeVarSym, tree: SubstitutionTree): SubstitutionTree = {
    SubstitutionTree(Substitution.empty, Map(sym -> tree))
  }
}

