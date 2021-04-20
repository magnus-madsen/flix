package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.InternalCompilerException

// MATT license
// MATT docs
sealed trait UnkindedType {
  // MATT docs
  def typeConstructor: Option[UnkindedType.Constructor] = this match {
    case UnkindedType.Cst(cst, _) => Some(cst)
    case UnkindedType.Apply(t1, _) => t1.typeConstructor
    case UnkindedType.Lambda(_, _) => throw InternalCompilerException("Unexpected type constructor: Lambda")
    case UnkindedType.Var(_, _) => None
  }

  // MATT docs
  def typeArguments: List[UnkindedType] = this match {
    case UnkindedType.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }
}

object UnkindedType {
  case class Cst(cst: Constructor, loc: SourceLocation) extends UnkindedType

  case class Apply(t1: UnkindedType, t2: UnkindedType) extends UnkindedType

  case class Lambda(t1: UnkindedType.Var, t2: UnkindedType) extends UnkindedType

  case class Var(id: Int, text: Option[String] = None) extends UnkindedType

  /**
    * Returns the Unit type with given source location `loc`.
    */
  def mkUnit(loc: SourceLocation): UnkindedType = Cst(Constructor.Unit, loc)

  /**
    * Returns the Null type with the given source location `loc`.
    */
  def mkNull(loc: SourceLocation): UnkindedType = Cst(Constructor.Null, loc)

  /**
    * Returns the Bool type with the given source location `loc`.
    */
  def mkBool(loc: SourceLocation): UnkindedType = Cst(Constructor.Bool, loc)

  /**
    * Returns the Char type with the given source location `loc`.
    */
  def mkChar(loc: SourceLocation): UnkindedType = Cst(Constructor.Char, loc)

  /**
    * Returns the Float32 type with the given source location `loc`.
    */
  def mkFloat32(loc: SourceLocation): UnkindedType = Cst(Constructor.Float32, loc)

  /**
    * Returns the Float64 type with the given source location `loc`.
    */
  def mkFloat64(loc: SourceLocation): UnkindedType = Cst(Constructor.Float64, loc)

  /**
    * Returns the Int8 type with the given source location `loc`.
    */
  def mkInt8(loc: SourceLocation): UnkindedType = Cst(Constructor.Int8, loc)

  /**
    * Returns the Int16 type with the given source location `loc`.
    */
  def mkInt16(loc: SourceLocation): UnkindedType = Cst(Constructor.Int16, loc)

  /**
    * Returns the Int32 type with the given source location `loc`.
    */
  def mkInt32(loc: SourceLocation): UnkindedType = Cst(Constructor.Int32, loc)

  /**
    * Returns the Int64 type with the given source location `loc`.
    */
  def mkInt64(loc: SourceLocation): UnkindedType = Cst(Constructor.Int64, loc)

  /**
    * Returns the BigInt type with the given source location `loc`.
    */
  def mkBigInt(loc: SourceLocation): UnkindedType = Cst(Constructor.BigInt, loc)

  /**
    * Returns the String type with the given source location `loc`.
    */
  def mkString(loc: SourceLocation): UnkindedType = Cst(Constructor.Str, loc)

  /**
    * Returns the Array type with the given source location `loc`.
    */
  def mkArray(loc: SourceLocation): UnkindedType = Cst(Constructor.Array, loc)

  /**
    * Returns the Channel type with the given source location `loc`.
    */
  def mkChannel(loc: SourceLocation): UnkindedType = Cst(Constructor.Channel, loc)

  /**
    * Returns the type `Channel[tpe]` with the given optional source location `loc`.
    */
  def mkChannel(tpe: UnkindedType, loc: SourceLocation = SourceLocation.Unknown): UnkindedType = Apply(Cst(Constructor.Channel, loc), tpe)

  /**
    * Returns the Lazy type with the given source location `loc`.
    */
  def mkLazy(loc: SourceLocation): UnkindedType = Cst(Constructor.Lazy, loc)

  /**
    * Returns the type `Lazy[tpe]` with the given optional source location `loc`.
    */
  def mkLazy(tpe: UnkindedType, loc: SourceLocation = SourceLocation.Unknown): UnkindedType = Apply(Cst(Constructor.Lazy, loc), tpe)

  /**
    * Returns the Ref type with the given source location `loc`.
    */
  def mkRef(loc: SourceLocation): UnkindedType = Cst(Constructor.Ref, loc)

  /**
    * Returns the type `Ref[tpe]` with the given optional source location `loc`.
    */
  def mkRef(tpe: UnkindedType, loc: SourceLocation = SourceLocation.Unknown): UnkindedType = Apply(Cst(Constructor.Ref, loc), tpe)

  /**
    * Constructs the a native type.
    */
  def mkNative(clazz: Class[_]): UnkindedType = Cst(Constructor.Native(clazz), SourceLocation.Unknown)

  // MATT docs
  def freshVar(text: Option[String] = None)(implicit flix: Flix): UnkindedType.Var = {
    Var(flix.genSym.freshId(), text)
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: UnkindedType, ts: List[UnkindedType]): UnkindedType = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t)
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, loc: SourceLocation): UnkindedType = Cst(Constructor.Enum(sym), loc)

  /**
    * Construct the enum type `Sym[ts]`.
    */
  // MATT add loc (also to Type.mkEnum?)
  def mkEnum(sym: Symbol.EnumSym, ts: List[UnkindedType]): UnkindedType = mkApply(UnkindedType.Cst(Constructor.Enum(sym), SourceLocation.Unknown), ts)

  /**
    * Constructs a tag type for the given `sym`, `tag`, `caseType` and `resultType`.
    *
    * A tag type can be understood as a "function type" from the `caseType` to the `resultType`.
    *
    * For example, for:
    *
    * {{{
    * enum List[a] {
    *   case Nil,
    *   case Cons(a, List[a])
    * }
    *
    * We have:
    *
    *   Nil:  Unit -> List[a]           (caseType = Unit, resultType = List[a])
    *   Cons: (a, List[a]) -> List[a]   (caseType = (a, List[a]), resultType = List[a])
    * }}}
    */
  def mkTag(sym: Symbol.EnumSym, tag: Name.Tag, caseType: UnkindedType, resultType: UnkindedType): UnkindedType = {
    mkApply(Cst(Constructor.Tag(sym, tag), SourceLocation.Unknown), List(caseType, resultType))
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[UnkindedType]): UnkindedType = { // MATT needs loc?
    mkApply(Cst(Constructor.Tuple(ts.length), SourceLocation.Unknown), ts)
  }

  // MATT docs
  def mkRecordEmpty(loc: SourceLocation): UnkindedType = {
    Cst(Constructor.RecordEmpty, loc)
  }

  // MATT docs
  def mkRecordExtend(field: Name.Field, tpe: UnkindedType, rest: UnkindedType, loc: SourceLocation): UnkindedType = {
    Apply(Cst(Constructor.RecordExtend(field), loc), rest)
  }

  trait Constructor

  object Constructor {

    /**
      * A type constructor that represent the Unit type.
      */
    case object Unit extends Constructor

    /**
      * A type constructor that represent the Null type.
      */
    case object Null extends Constructor

    /**
      * A type constructor that represent the Bool type.
      */
    case object Bool extends Constructor

    /**
      * A type constructor that represent the Char type.
      */
    case object Char extends Constructor

    /**
      * A type constructor that represent the type of 32-bit floating point numbers.
      */
    case object Float32 extends Constructor

    /**
      * A type constructor that represent the type of 64-bit floating point numbers.
      */
    case object Float64 extends Constructor

    /**
      * A type constructor that represent the type of 8-bit integers.
      */
    case object Int8 extends Constructor

    /**
      * A type constructor that represent the type of 16-bit integers.
      */
    case object Int16 extends Constructor

    /**
      * A type constructor that represent the type of 32-bit integers.
      */
    case object Int32 extends Constructor

    /**
      * A type constructor that represent the type of 64-bit integers.
      */
    case object Int64 extends Constructor

    /**
      * A type constructor that represent the type of arbitrary-precision integers.
      */
    case object BigInt extends Constructor

    /**
      * A type constructor that represent the type of strings.
      */
    case object Str extends Constructor

    /**
      * A type constructor that represents the type of functions.
      */
    case class Arrow(arity: Int) extends Constructor

    /**
      * A type constructor that represents the type of empty records.
      */
    case object RecordEmpty extends Constructor

    /**
      * A type constructor that represents the type of extended records.
      */
    case class RecordExtend(field: Name.Field) extends Constructor

    /**
      * A type constructor that represents the type of empty schemas.
      */
    case object SchemaEmpty extends Constructor

    /**
      * A type constructor that represents the type of extended schemas.
      */
    case class SchemaExtend(pred: Name.Pred) extends Constructor

    /**
      * A type constructor that represent the type of arrays.
      */
    case object Array extends Constructor

    /**
      * A type constructor that represent the type of channels.
      */
    case object Channel extends Constructor

    /**
      * A type constructor that represent the type of lazy expressions.
      */
    case object Lazy extends Constructor

    /**
      * A type constructor that represent the type of tags.
      */
    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag) extends Constructor

    /**
      * A type constructor that represent the type of enums.
      */
    case class Enum(sym: Symbol.EnumSym) extends Constructor

    /**
      * A type constructor that represent the type of JVM classes.
      */
    case class Native(clazz: Class[_]) extends Constructor

    /**
      * A type constructor that represent the type of references.
      */
    case object Ref extends Constructor

    /**
      * A type constructor that represent the type of tuples.
      */
    case class Tuple(l: Int) extends Constructor

    /**
      * A type constructor for relations.
      */
    case object Relation extends Constructor

    /**
      * A type constructor for lattices.
      */
    case object Lattice extends Constructor

    /**
      * A type constructor that represent the Boolean True.
      */
    case object True extends Constructor

    /**
      * A type constructor that represents the Boolean False.
      */
    case object False extends Constructor

    /**
      * A type constructor that represents the negation of an effect.
      */
    case object Not extends Constructor

    /**
      * A type constructor that represents the conjunction of two effects.
      */
    case object And extends Constructor

    /**
      * A type constructor that represents the disjunction of two effects.
      */
    case object Or extends Constructor
  }
}