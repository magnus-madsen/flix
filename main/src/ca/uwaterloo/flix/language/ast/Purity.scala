/*
 * Copyright 2022 Christian Bonde, Patrick Lundvig, Anna Krogh
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

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.InternalCompilerException

sealed trait Purity

/**
  * Represents the purity of an expression.
  *
  *   - Pure expressions are treated as mathematical functions.
  *   - Impure expressions can use mutation or interact with the system.
  *   - Control-Impure expressions can do all the above but also use unhandled
  *   algebraic effects.
  *
  * In terms of the set of expressions that is allowed under each effect the
  * following holds:
  * `e_pure ⊆ e_impure ⊆ e_control-impure`.
  */
object Purity {

  /**
    * Represents a pure expression (i.e. an expression that cannot have
    * side-effects).
    */
  case object Pure extends Purity

  /**
    * Represents an impure expression (i.e. an expression that could potentially
    * have side-effects).
    */
  case object Impure extends Purity

  /**
    * Represents a control-impure expression (i.e. an expression that could
    * potentially use effects like `do Print.print()`).
    */
  case object ControlImpure extends Purity

  /**
    * Returns true if `p` is is a purity that does not allows algebraic effects.
    */
  def isControlPure(p: Purity): Boolean = p match {
    case Pure => true
    case Impure => true
    case ControlImpure => false
  }

  /**
    * Returns true if `p` is is a purity that allows algebraic effects.
    */
  def isControlImpure(p: Purity): Boolean = p match {
    case Pure => false
    case Impure => false
    case ControlImpure => true
  }

  /**
    * Returns true if p is pure (has no side effects).
    */
  def isPure(p: Purity): Boolean = p match {
    case Pure => true
    case Impure => false
    case ControlImpure => false
  }

  /**
    * Returns the max purity of `p1` and `p2` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def combine(p1: Purity, p2: Purity): Purity = {
    def toInt(p: Purity): Int = p match {
      case Pure => 0
      case Impure => 1
      case ControlImpure => 2
    }

    Ordering.by(toInt).max(p1, p2)
  }

  /**
    * Returns the max purity of `p1`, `p2`, and `p3` according to this ordering:
    * Pure < Impure < ControlImpure
    */
  def combine3(p1: Purity, p2: Purity, p3: Purity): Purity = {
    combine(combine(p1, p2), p3)
  }

  /**
    * Returns the max purity of `p` according to this ordering:
    * Pure < Impure < ControlImpure
    *
    * Returns [[Pure]] if empty.
    */
  def combineAll(p: List[Purity]): Purity = {
    p.foldLeft(Pure: Purity)(combine)
  }

  /**
    * Returns the purity of the given formula `eff`. Returns [[Pure]] for the
    * effect constant of [[TypeConstructor.Pure]], returns [[Impure]] for an
    * effect only containing primitive effects, and [[ControlImpure]] otherwise.
    *
    * Assumes that the given type is a well-formed formula without variables,
    * aliases, or associated types.
    */
  def fromType(eff: Type)(implicit universe: Set[Symbol.EffectSym]): Purity = {
    evaluateFormula(eff) match {
      case set if set.isEmpty => Purity.Pure
      case set if set.forall(Symbol.isPrimitiveEff) => Purity.Impure
      case _ => Purity.ControlImpure
    }
  }

  /**
    * Returns the set of effects described by the formula `f`.
    *
    * Assumes that `f` only contains well-formed [[TypeConstructor.Union]],
    * [[TypeConstructor.Intersection]], [[TypeConstructor.Difference]], and
    * [[TypeConstructor.Complement]] of
    * [[TypeConstructor.Pure]], [[TypeConstructor.Univ]], and
    * [[TypeConstructor.Effect]].
    *
    * plus is union, ampersand is intersection, and exclamation mark is complement.
    *
    * Pure == {}
    * Univ == Top
    * Crash == {Crash}
    * Print + IO == {Print, IO}
    * Univ & (!Print) == !Print
    */
  private def evaluateFormula(f: Type)(implicit universe: Set[Symbol.EffectSym]): Set[Symbol.EffectSym] = f match {
    case Type.Cst(TypeConstructor.Effect(sym), _) =>
      Set(sym)
    case Type.Cst(TypeConstructor.Pure, _) =>
      Set.empty
    case Type.Cst(TypeConstructor.Univ, _) =>
      universe
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) =>
      val t1 = evaluateFormula(tpe1)
      val t2 = evaluateFormula(tpe2)
      t1.union(t2)
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) =>
      val t1 = evaluateFormula(tpe1)
      val t2 = evaluateFormula(tpe2)
      t1.intersect(t2)
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), tpe1, _), tpe2, _) =>
      val t1 = evaluateFormula(tpe1)
      val t2 = evaluateFormula(tpe2)
      t1.diff(t2)
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), tpe1, _), tpe2, _) =>
      val t1 = evaluateFormula(tpe1)
      val t2 = evaluateFormula(tpe2)
      // a ⊕ b = (a ∪ b) - (a ∩ b)
      t1.union(t2).diff(t1.intersect(t2))
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe, _) =>
      val t = evaluateFormula(tpe)
      universe.diff(t)
    case Type.Cst(_, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.Apply(_, _, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.Var(_, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.Alias(_, _, _, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.AssocType(_, _, _, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.JvmToType(_, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.JvmToEff(_, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.UnresolvedJvmType(_, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
    case Type.GetEff(_, _, _) =>
      throw InternalCompilerException(s"Unexpected formula '$f'", f.loc)
  }

}
