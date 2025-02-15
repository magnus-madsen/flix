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

import ca.uwaterloo.flix.language.fmt.FormatKind

import scala.annotation.tailrec

/**
  * A kind represents the "type" of a type expression.
  */
sealed trait Kind {

  /**
    * Constructs an arrow kind.
    *
    * This is a right-associative operator, i.e., the following two kinds are equivalent:
    *
    *   - `Kind.Star ->: Kind.Star ->: Kind.Star`
    *   - `Kind.Star ->: (Kind.Star ->: Kind.Star)`
    */
  def ->:(left: Kind): Kind = Kind.Arrow(left, this)

  /**
    * Returns a human readable representation of `this` kind.
    */
  override def toString: String = FormatKind.formatKind(this)


}

object Kind {

  /**
    * Represents a wild kind.
    * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
    * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
    */
  case object Wild extends Kind

  /**
    * Represents the wildcard kind only matching Case Sets.
    * A wild kind exists during the kinding phase, but should be eliminated before the following phase,
    * unless the kind is deemed irrelevant (e.g. the kind of a wildcard type).
    */
  case object WildCaseSet extends Kind

  /**
    * Represents the kind of types.
    */
  case object Star extends Kind

  /**
    * Represents the kind of effect sets.
    */
  case object Eff extends Kind

  /**
    * Represents the kind of regions.
    */
  case object Region extends Kind

  /**
    * Represents the kind of Boolean formulas
    */
  case object Bool extends Kind

  /**
    * Represents the kind of record rows.
    */
  case object RecordRow extends Kind

  /**
    * Represents the kind of schema rows.
    */
  case object SchemaRow extends Kind

  /**
    * Represents the kind of predicates.
    */
  case object Predicate extends Kind

  /**
   * Represents the kind of a Java constructor, method, or field.
   */
  case object Jvm extends Kind

  /**
    * Represents the kind of sets of restrictable enum cases.
    */
  case class CaseSet(sym: Symbol.RestrictableEnumSym) extends Kind

  /**
    * Represents the kind of type expressions `k1 -> k2`.
    */
  case class Arrow(k1: Kind, k2: Kind) extends Kind

  /**
    * Represents an error kind.
    */
  case object Error extends Kind

  /**
    * Returns the kind: * -> (* ... -> *)
    */
  def mkArrow(length: Int): Kind = {
    if (length == 0) {
      return Star
    }

    (0 until length).foldRight(Star: Kind) {
      case (_, acc) => Arrow(Star, acc)
    }
  }

  /**
    * Returns the kind: k1 -> (k2 ... -> (kn -> *)) for the given list of kinds `ks`.
    */
  def mkArrow(ks: List[Kind]): Kind = ks match {
    case Nil => Star
    case x :: xs => Arrow(x, mkArrow(xs))
  }

  /**
    * Returns the base of an arrow kind.
    */
  @tailrec
  def base(k: Kind): Kind = k match {
    case Arrow(k1, _) => base(k1)
    case _ => k
  }

  /**
    * Returns the arguments of an arrow kind.
    */
  def kindArgs(k: Kind): List[Kind] = k match {
    case Arrow(k1, k2) => k1 :: kindArgs(k2)
    case _ => Nil
  }

  /**
    * Traverses the kind and returns `true` if `k` contains [[Kind.Error]].
    * Returns `false` otherwise.
    */
  def hasError(k: Kind): Boolean = k match {
    case Wild => false
    case WildCaseSet => false
    case Star => false
    case Eff => false
    case Region => false
    case Bool => false
    case RecordRow => false
    case SchemaRow => false
    case Predicate => false
    case Jvm => false
    case CaseSet(_) => false
    case Arrow(k1, k2) => hasError(k1) || hasError(k2)
    case Error => true
  }
}
