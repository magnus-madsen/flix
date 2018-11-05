package ca.uwaterloo.flix.runtime.solver.api.predicate

/**
  * Represents the true predicate.
  */
class TruePredicate extends Predicate {
  /**
    * Returns a copy of this predicate.
    */
  override def copy(): Predicate = new TruePredicate()

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "true"
}
