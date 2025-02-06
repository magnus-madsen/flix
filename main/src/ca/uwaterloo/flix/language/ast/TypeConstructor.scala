package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.ScalaAnnotations.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.phase.{Kinder, Lowering, Monomorpher, Simplifier}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.SortedSet

/**
  * Representation of type constructors.
  */
sealed trait TypeConstructor {
  def kind: Kind
}

object TypeConstructor {

  /**
    * A type constructor that represent the Void type.
    *
    * The `Void` type is uninhabited and should not be confused which Java's `void` which is `Unit` in Flix.
    */
  case object Void extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent an unconstrained type after monomorphization.
    */
  @IntroducedBy(Monomorpher.getClass)
  case object AnyType extends TypeConstructor {
    override def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the Unit type.
    */
  case object Unit extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the Null type.
    */
  case object Null extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the Bool type.
    */
  case object Bool extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the Char type.
    */
  case object Char extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 32-bit floating point numbers.
    */
  case object Float32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 64-bit floating point numbers.
    */
  case object Float64 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of arbitrary-precision floating point numbers.
    */
  case object BigDecimal extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 8-bit integers.
    */
  case object Int8 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 16-bit integers.
    */
  case object Int16 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 32-bit integers.
    */
  case object Int32 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of 64-bit integers.
    */
  case object Int64 extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent the type of arbitrary-precision integers.
    */
  case object BigInt extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents the type of strings.
    */
  case object Str extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents the type of regex patterns.
    */
  case object Regex extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents the type of functions.
    */
  @IntroducedBy(Kinder.getClass)
  case class Arrow(arity: Int) extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.mkArrow(arity)
  }

  /**
    * A type constructor that represents an arrow type where the effect has been removed.
    *
    * Warning: This is not part of the frontend; it only exists post Simplification.
    */
  @IntroducedBy(Simplifier.getClass)
  case class ArrowWithoutEffect(arity: Int) extends TypeConstructor {
    def kind: Kind = Kind.mkArrow(arity)
  }

  /**
    * A type constructor that represents the type of empty record rows.
    */
  case object RecordRowEmpty extends TypeConstructor {
    def kind: Kind = Kind.RecordRow
  }

  /**
    * A type constructor that represents the type of extended record rows.
    */
  case class RecordRowExtend(label: Name.Label) extends TypeConstructor {
    /**
      * The shape of an extended record is { label = type | rest }
      */
    def kind: Kind = Kind.Star ->: Kind.RecordRow ->: Kind.RecordRow
  }

  /**
    * A type constructor that represents the type of records.
    */
  case object Record extends TypeConstructor {
    /**
      * The shape of a record constructor is Record[row]
      */
    def kind: Kind = Kind.RecordRow ->: Kind.Star
  }

  /**
    * A type constructor that represents the type of empty schema rows.
    */
  case object SchemaRowEmpty extends TypeConstructor {
    def kind: Kind = Kind.SchemaRow
  }

  /**
    * A type constructor that represents the type of extended schema rows.
    */
  case class SchemaRowExtend(pred: Name.Pred) extends TypeConstructor {
    /**
      * The shape of an extended schema is { name :: type | rest }
      */
    def kind: Kind = Kind.Predicate ->: Kind.SchemaRow ->: Kind.SchemaRow
  }

  /**
    * A type constructor that represents the type of schemas.
    */
  case object Schema extends TypeConstructor {
    /**
      * The shape of a schema constructor is Schema[row]
      */
    def kind: Kind = Kind.SchemaRow ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of channel senders.
    */
  @EliminatedBy(Lowering.getClass)
  case object Sender extends TypeConstructor {
    /**
      * The shape of a sender is Sender[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of channel receivers.
    */
  @EliminatedBy(Lowering.getClass)
  case object Receiver extends TypeConstructor {
    /**
      * The shape of a sender is Receiver[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of lazy expressions.
    */
  case object Lazy extends TypeConstructor {
    /**
      * The shape of lazy is Lazy[t].
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represents the type of enums.
    */
  @IntroducedBy(Kinder.getClass)
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends TypeConstructor

  /**
   * A type constructor that represents the type of structs.
   */
  @IntroducedBy(Kinder.getClass)
  case class Struct(sym: Symbol.StructSym, kind: Kind) extends TypeConstructor

  /**
    * A type constructor that represents the type of enums.
    */
  @IntroducedBy(Kinder.getClass)
  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym, kind: Kind) extends TypeConstructor

  /**
    * A type constructor that represent the type of JVM classes.
    */
  case class Native(clazz: Class[?]) extends TypeConstructor {
    def kind: Kind = Kind.Star
  }

  /**
   * A type constructor that represents the type of a Java constructor.
   * */
  case class JvmConstructor(constructor: Constructor[?]) extends TypeConstructor {
    def kind: Kind = Kind.Jvm
  }

  /**
   * A type constructor that represents the type of a Java method.
   */
  case class JvmMethod(method: Method) extends TypeConstructor {
    def kind: Kind = Kind.Jvm
  }

  /**
    * A type constructor that represents the type of a Java field.
    */
  case class JvmField(field: Field) extends TypeConstructor {
    def kind: Kind = Kind.Jvm
  }

  /**
    * A type constructor that represent the type of arrays.
    */
  case object Array extends TypeConstructor {
    /**
      * The shape of an array is `Array[t, l]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Region ->: Kind.Star
  }

  /**
    * A type constructor that represents that represents an array type where the region has been
    * removed.
    *
    * Warning: This is not part of the frontend; it only exists post Simplification.
    */
  @IntroducedBy(Simplifier.getClass)
  case object ArrayWithoutRegion extends TypeConstructor {
    /**
      * The shape of an array is `ArrayWithoutRegion[t]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of vectors.
    */
  case object Vector extends TypeConstructor {
    /**
      * The shape of a vector is `Vector[t]`.
      */
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type constructor that represent the type of tuples.
    */
  case class Tuple(l: Int) extends TypeConstructor {
    /**
      * The shape of a tuple is (t1, ..., tn).
      */
    def kind: Kind = Kind.mkArrow(l)
  }

  /**
    * A type constructor for relations.
    */
  case object Relation extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Predicate
  }

  /**
    * A type constructor for lattices.
    */
  case object Lattice extends TypeConstructor {
    def kind: Kind = Kind.Star ->: Kind.Predicate
  }

  /**
    * A type constructor that represents the Boolean value TRUE.
    */
  case object True extends TypeConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A type constructor that represents the Boolean value FALSE.
    */
  case object False extends TypeConstructor {
    def kind: Kind = Kind.Bool
  }

  /**
    * A type constructor that represents Boolean negation.
    */
  case object Not extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represents Boolean conjunction.
    */
  case object And extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represents Boolean disjunction.
    */
  case object Or extends TypeConstructor {
    def kind: Kind = Kind.Bool ->: Kind.Bool ->: Kind.Bool
  }

  /**
    * A type constructor that represents the empty effect set.
    */
  case object Pure extends TypeConstructor {
    def kind: Kind = Kind.Eff
  }

  /**
    * A type constructor that represents the universal effect set.
    */
  case object Univ extends TypeConstructor {
    def kind: Kind = Kind.Eff
  }

  /**
    * A type constructor that represents the complement of an effect set.
    */
  case object Complement extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.Eff
  }

  /**
    * A type constructor that represents the union of two effect sets.
    */
  case object Union extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.Eff ->: Kind.Eff
  }

  /**
    * A type constructor that represents the intersection of two effect sets.
    */
  case object Intersection extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.Eff ->: Kind.Eff
  }

  /**
    * A type constructor that represents the difference of two effect sets.
    */
  case object Difference extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.Eff ->: Kind.Eff
  }

  /**
    * A type constructor that represents the exclusive or (symmetric difference) of two effect sets.
    */
  case object SymmetricDiff extends TypeConstructor {
    def kind: Kind = Kind.Eff ->: Kind.Eff ->: Kind.Eff
  }

  /**
    * A type constructor that represents a single effect.
    */
  case class Effect(sym: Symbol.EffectSym) extends TypeConstructor {
    def kind: Kind = Kind.Eff
  }

  /**
    * A type constructor that represents the complement of a case set.
    */
  case class CaseComplement(sym: Symbol.RestrictableEnumSym) extends TypeConstructor {
    def kind: Kind = Kind.CaseSet(sym) ->: Kind.CaseSet(sym)
  }

  /**
    * A type constructor that represents the union of two case sets.
    */
  case class CaseUnion(sym: Symbol.RestrictableEnumSym) extends TypeConstructor {
    def kind: Kind = Kind.CaseSet(sym) ->: Kind.CaseSet(sym) ->: Kind.CaseSet(sym)
  }

  /**
    * A type constructor that represents the intersection of two case sets.
    */
  case class CaseIntersection(sym: Symbol.RestrictableEnumSym) extends TypeConstructor {
    def kind: Kind = Kind.CaseSet(sym) ->: Kind.CaseSet(sym) ->: Kind.CaseSet(sym)
  }

  /**
    * A type constructor that represents a case constant.
    */
  case class CaseSet(syms: SortedSet[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym) extends TypeConstructor {
    def kind: Kind = Kind.CaseSet(enumSym)
  }

  /**
    * A type constructor that represents a region.
    */
  case class Region(sym: Symbol.RegionSym) extends TypeConstructor {
    def kind: Kind = Kind.Region
  }

  /**
    * A type constructor that converts a region to a Star type.
    */
  case object RegionToStar extends TypeConstructor {
    /**
      * The shape of a star-kind region is Region[l].
      */
    def kind: Kind = Kind.Region ->: Kind.Star
  }

  /**
    * A type constructor that converts a region to an Eff type.
    */
  case object RegionToEff extends TypeConstructor {
    /**
      * The shape of an effect-kind region is Heap[l].
      */
    def kind: Kind = Kind.Region ->: Kind.Eff
  }

  /**
    * A type constructor that represents that a region type where the region argument has been
    * removed.
    *
    * I.e., The normal `Region[r]` type is `RegionWithoutRegion` where `r` has been removed.
    *
    * Warning: This is not part of the frontend; it only exists post Simplification.
    */
  @IntroducedBy(Simplifier.getClass)
  case object RegionWithoutRegion extends TypeConstructor {
    /**
      * The shape of a region is RegionWithoutRegion.
      */
    def kind: Kind = Kind.Star
  }

  /**
    * A region after monomorphization is reduced to its property.
    */
  @IntroducedBy(Monomorpher.getClass)
  case class GenericRegion(property: RegionProperty) extends TypeConstructor {
    def kind: Kind = Kind.Region
  }

  /**
    * A type constructor which represents an erroneous type of the given `kind`.
    */
  case class Error(id: Int, kind: Kind) extends TypeConstructor

}
