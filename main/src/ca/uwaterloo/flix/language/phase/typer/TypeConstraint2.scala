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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}

sealed trait TypeConstraint2 {
  def size: Int = this match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) => tpe1.size + tpe2.size
    case TypeConstraint2.Trait(sym, tpe, loc) => tpe.size
    case TypeConstraint2.Purification(sym, eff1, eff2, nested, loc) => eff1.size + eff2.size + nested.map(_.size).sum
  }

  override def toString: String = this match {
    case TypeConstraint2.Equality(tpe1, tpe2, _) => s"$tpe1 ~ $tpe2"
    case TypeConstraint2.Trait(sym, tpe, _) => s"$sym[$tpe]"
    case TypeConstraint2.Purification(sym, eff1, eff2, nested, _) => s"$eff1 ~ ($eff2)[$sym ↦ Pure] ∧ $nested"
  }
}

object TypeConstraint2 {
  /**
    * A constraint indicating the equivalence of two types.
    * {{{
    *   tpe1 ~ tpe2
    * }}}
    */
  case class Equality(tpe1: Type, tpe2: Type, loc: SourceLocation) extends TypeConstraint2

  /**
    * A constraint indicating that the given type is a member of the given trait.
    * {{{
    *   sym[tpe]
    * }}}
    */
  case class Trait(sym: Symbol.TraitSym, tpe: Type, loc: SourceLocation) extends TypeConstraint2

  /**
    * A constraint indicating that:
    *   - `eff1` is equivalent to `eff2` when the region `sym` is purified in `eff2`, and
    *   - the nested constraints all hold
    *
    * This constraint arises when exiting a region.
    * All nested constraints must be resolved before determining the equality of `eff1` and `eff2`,
    * because the nested constraints influence `eff2`.
    *
    * {{{
    *   eff1 ~ eff2[sym ↦ Pure] ∧ nested
    * }}}
    */
  case class Purification(sym: Symbol.KindedTypeVarSym, eff1: Type, eff2: Type, nested: List[TypeConstraint2], loc: SourceLocation) extends TypeConstraint2
}
