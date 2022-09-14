/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

/**
  * Companion object for the [[BoolAlgebraSubstitution2]] class.
  */
object BoolAlgebraSubstitution2 {
  /**
    * Returns the empty substitution.
    */
  def empty[F]: BoolAlgebraSubstitution2[F] = BoolAlgebraSubstitution2(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton[F](x: Int, f: F): BoolAlgebraSubstitution2[F] = {
    // Ensure that we do not add any x -> x mappings.
    f match {
      case y: BoolAlgebra.Var if x == y.x => empty
      case _ => BoolAlgebraSubstitution2(Map(x -> f))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class BoolAlgebraSubstitution2[F](m: Map[Int, F]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(f: F)(implicit alg: BoolAlgTrait[F]): F= {
    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) {
      f
    } else {
      alg.map(i => m.getOrElse(i, alg.mkVar(i)), f)
    }
  }

  /**
    * Applies `this` substitution to the given types `ts`.
    */
  def apply(ts: List[BoolAlgebra]): List[BoolAlgebra] = if (isEmpty) ts else ts map apply

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: BoolAlgebraSubstitution2[F]): BoolAlgebraSubstitution2[F] = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      BoolAlgebraSubstitution2(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: BoolAlgebraSubstitution2[F])(implicit alg: BoolAlgTrait[F]): BoolAlgebraSubstitution2[F] = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // NB: Use of mutability improve performance.
    import scala.collection.mutable
    val newBoolAlgebraMap = mutable.Map.empty[Int, F]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      newBoolAlgebraMap.update(x, this.apply(t))
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newBoolAlgebraMap.update(x, t)
      }
    }

    BoolAlgebraSubstitution2(newBoolAlgebraMap.toMap) ++ this
  }
}
