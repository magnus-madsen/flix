package ca.uwaterloo.flix.language.ast

/**
  * Representation of type constructors.
  */
sealed trait TypeConstructor {
  def kind: Kind
}

object TypeConstructor {

  /**
    * A type constructor that represent 32-bit floating point numbers.
    */
  case object Float32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 64-bit floating point numbers.
    */
  case object Float64 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

}
