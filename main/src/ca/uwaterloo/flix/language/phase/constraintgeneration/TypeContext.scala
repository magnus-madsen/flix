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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Level, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint.Provenance
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

/**
  * The typing context is a mutable environment that tracks information during type constraint generation.
  * It maintains a stack of information: Whenever inference enters a region, the current information is pushed
  * onto the stack, and the information is reset. On exiting a region, the information is popped off, and
  * a special constraint is added which incorporates all the popped constraints.
  */
class TypeContext {

  private object ScopeConstraints {
    /**
      * The empty ScopeConstraints with no associated region.
      */
    val empty: ScopeConstraints = new ScopeConstraints(None)

    /**
      * Creates an empty ScopeConstraints associated with the given region.
      */
    def emptyForRegion(r: Symbol.KindedTypeVarSym): ScopeConstraints = new ScopeConstraints(Some(r))
  }

  /**
    * Stores typing information relating to a particular region scope.
    *
    * @param region the region symbol associated with the scope (None if not in a region).
    */
  private class ScopeConstraints(val region: Option[Symbol.KindedTypeVarSym]) {

    /**
      * The constraints inferred for the scope.
      */
    private val constrs: mutable.ListBuffer[TypingConstraint] = mutable.ListBuffer.empty

    /**
      * Adds the constraint to the constraint list.
      */
    def add(constr: TypingConstraint): Unit = this.constrs.addOne(constr)

    /**
      * Adds all the constraints to the constraint list.
      */
    def addAll(constrs: Iterable[TypingConstraint]): Unit = this.constrs.addAll(constrs)

    /**
      * Returns the gathered constraints.
      */
    def getConstraints: List[TypingConstraint] = this.constrs.toList
  }

  /**
    * The information about the current scope.
    */
  private var currentScopeConstraints: ScopeConstraints = ScopeConstraints.empty

  /**
    * The current rigidity environment.
    *
    * This environment only grows; we don't remove rigid variables as we exit a region.
    * We use a mutable variable because RigidityEnv is an immutable structure.
    */
  private var renv: RigidityEnv = RigidityEnv.empty

  /**
    * The current level. Incremented and decremented as we enter and exit regions.
    */
  private var level: Level = Level.Top

  /**
    * The typing context from outside the current scope.
    *
    * We push and pop information from this stack when we enter and exit regions.
    */
  private val constraintStack: mutable.Stack[ScopeConstraints] = mutable.Stack.empty

  /**
    * Returns the current rigidity environment.
    */
  def getRigidityEnv: RigidityEnv = renv

  /**
    * Returns the current typing constraints.
    */
  def getTypingConstraints: List[TypingConstraint] = currentScopeConstraints.getConstraints

  /**
    * Returns the current level.
    */
  def getLevel: Level = level

  /**
    * Generates constraints unifying the given types.
    *
    * {{{
    *   tpe1 ~ tpe2
    * }}}
    */
  def unifyType(tpe1: Type, tpe2: Type, loc: SourceLocation): Unit = {
    val constr = TypingConstraint.Equality(tpe1, tpe2, Provenance.Match(tpe1, tpe2, loc))
    currentScopeConstraints.add(constr)
  }

  /**
    * Generates constraints unifying the given types.
    *
    * {{{
    *   tpe1 ~ tpe2
    *   tpe1 ~ tpe3
    * }}}
    */
  def unifyType3(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation): Unit = {
    unifyType(tpe1, tpe2, loc)
    unifyType(tpe1, tpe3, loc)
  }

  /**
    * Generates constraints unifying the given types.
    *
    * Returns a fresh type variable if the list is empty.
    *
    * {{{
    *   tpe1 ~ tpe2
    *   tpe1 ~ tpe3
    *   ...
    *   tpe1 ~ tpeN
    * }}}
    */
  def unifyAllTypes(tpes: List[Type], kind: Kind, loc: SourceLocation)(implicit level: Level, flix: Flix): Type = {
    // For performance, avoid creating a fresh type var if the list is non-empty
    tpes match {
      // Case 1: Nonempty list. Unify everything with the first type.
      case tpe1 :: rest =>
        rest.foreach(unifyType(tpe1, _, loc))
        tpe1
      // Case 2: Empty list. Return a fresh type var.
      case Nil => Type.freshVar(kind, loc.asSynthetic)
    }
  }

  /**
    * Generates constraints expecting the given types to unify.
    *
    * {{{
    *   expected ~ actual
    * }}}
    */
  def expectType(expected: Type, actual: Type, loc: SourceLocation): Unit = {
    val constr = TypingConstraint.Equality(expected, actual, Provenance.ExpectType(expected, actual, loc))
    currentScopeConstraints.add(constr)
  }

  /**
    * Generates constraints expecting the given type arguments to unify.
    *
    * For expected types `tpeE1 ... tpeEN` and actual types `tpeA1 ... tpeAN`, generates:
    *
    * {{{
    *   tpeE1 ~ tpeA1
    *   tpeE2 ~ tpeA2
    *   ...
    *   tpeEN ~ tpeAN
    * }}}
    */
  def expectTypeArguments(sym: Symbol, expectedTypes: List[Type], actualTypes: List[Type], actualLocs: List[SourceLocation], loc: SourceLocation)(implicit flix: Flix): Unit = {
    expectedTypes.zip(actualTypes).zip(actualLocs).zipWithIndex.foreach {
      case (((expectedType, actualType), loc), index) =>
        val argNum = index + 1
        val prov = Provenance.ExpectArgument(expectedType, actualType, sym, argNum, loc)
        val constr = TypingConstraint.Equality(expectedType, actualType, prov)
        currentScopeConstraints.add(constr)
    }
  }

  /**
    * Adds the given class constraints to the context.
    */
  def addClassConstraints(tconstrs0: List[Ast.TypeConstraint], loc: SourceLocation): Unit = {
    // convert all the syntax-level constraints to semantic constraints
    val tconstrs = tconstrs0.map {
      case Ast.TypeConstraint(head, arg, _) => TypingConstraint.Class(head.sym, arg, loc)
    }
    currentScopeConstraints.addAll(tconstrs)
  }

  /**
    * Marks the given type variable as rigid in the context.
    */
  def rigidify(sym: Symbol.KindedTypeVarSym): Unit = {
    renv = renv.markRigid(sym)
  }

  /**
    * Replaces every occurrence of the effect symbol `sym` with pure in `eff`.
    *
    * Note: Does not work for polymorphic effects. This should conceptually work
    * like exiting a region or instead use set subtraction.
    */
  // TODO ASSOC-TYPES remove this once we introduce set effects
  def purifyEff(sym: Symbol.EffectSym, eff: Type): Type = {
    def visit(t: Type): Type = t match {
      case Type.Var(_, _) => t
      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Effect(sym2) if sym == sym2 => Type.Pure
        case _ => t
      }
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(visit(tpe1), visit(tpe2), loc)
      case Type.Alias(_, _, tpe, _) => visit(tpe)
      case Type.AssocType(cst, arg, kind, loc) => Type.AssocType(cst, visit(arg), kind, loc)
    }

    visit(eff)
  }

  /**
    * Enters a new region.
    *
    * Current scope information is pushed onto the stack,
    * the region symbol is marked as rigid,
    * the level is incremented,
    * and we get a fresh empty set of constraints for the new scope.
    */
  def enterRegion(sym: Symbol.KindedTypeVarSym): Unit = {
    // save the info from the parent region
    constraintStack.push(currentScopeConstraints)
    renv = renv.markRigid(sym)
    level = level.incr
    currentScopeConstraints = ScopeConstraints.emptyForRegion(sym)
  }

  /**
    * Exits a region, unifying the external effect with a purified version of the internal effect.
    *
    * We generate a fresh purification constraint:
    *
    * {{{
    *   externalEff1 ~ internalEff2[sym ↦ Pure]
    * }}}
    *
    * Where the `sym` is the symbol of the region we are exiting.
    * All the constraints from the inner region are nested under the new purification constraint.
    * (They must be resolved first for the purification to be valid.)
    *
    * We pop the constraints from the parent scope; these become our current constraints.
    * We add the new purification constraints to the current constraints.
    * Finally, we decrement the level.
    */
  def exitRegion(externalEff1: Type, internalEff2: Type, loc: SourceLocation): Unit = {
    val constr = currentScopeConstraints.region match {
      case None => throw InternalCompilerException("unexpected missing region", loc)
      case Some(r) =>
        // TODO ASSOC-TYPES improve prov. We can probably get a better prov than "match"
        val prov = Provenance.Match(externalEff1, internalEff2, loc)
        TypingConstraint.Purification(r, externalEff1, internalEff2, level, prov, currentScopeConstraints.getConstraints)
    }

    currentScopeConstraints = constraintStack.pop()
    currentScopeConstraints.add(constr)
    level = level.decr
  }

}
