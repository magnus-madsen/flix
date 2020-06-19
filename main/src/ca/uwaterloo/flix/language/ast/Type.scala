/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable.SortedSet

/**
  * Representation of types.
  */
sealed trait Type {
  /**
    * The kind of `this` type.
    */
  def kind: Kind

  /**
    * Returns the type variables in `this` type.
    *
    * Returns a sorted set to ensure that the compiler is deterministic.
    */
  def typeVars: SortedSet[Type.Var] = this match {
    case x: Type.Var => SortedSet(x)
    case Type.Cst(TypeConstructor.Arrow(_, eff)) => eff.typeVars
    case Type.Cst(tc) => SortedSet.empty
    case Type.Lambda(tvar, tpe) => tpe.typeVars - tvar
    case Type.Apply(tpe1, tpe2) => tpe1.typeVars ++ tpe2.typeVars
  }

  /**
    * Optionally returns the type constructor of `this` type.
    *
    * Return `None` if the type constructor is a variable.
    *
    * Otherwise returns `Some(tc)` where `tc` is the left-most type constructor.
    *
    * For example,
    *
    * {{{
    * x                             =>      None
    * Celsius                       =>      Some(Celsius)
    * Option[Int]                   =>      Some(Option)
    * Arrow[Bool, Char]             =>      Some(Arrow)
    * Tuple[Bool, Int]              =>      Some(Tuple)
    * Result[Bool, Int]             =>      Some(Result)
    * Result[Bool][Int]             =>      Some(Result)
    * Option[Result[Bool, Int]]     =>      Some(Option)
    * }}}
    */
  def typeConstructor: Option[TypeConstructor] = this match {
    case Type.Var(_, _, _) => None
    case Type.Cst(tc) => Some(tc)
    case Type.Lambda(_, _) => None
    case Type.Apply(t1, _) => t1.typeConstructor
  }

  // TODO: Remove
  def typeConstructorDeprecatedWillBeRemoved: Type = this match {
    case Type.Apply(t1, _) => t1.typeConstructorDeprecatedWillBeRemoved
    case _ => this
  }

  /**
    * Returns the type arguments of `this` type.
    *
    * For example,
    *
    * {{{
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    * }}}
    */
  def typeArguments: List[Type] = this match {
    case Type.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  /**
    * Returns the size of `this` type.
    */
  def size: Int = this match {
    case Type.Var(_, _, _) => 1
    case Type.Cst(TypeConstructor.Arrow(_, eff)) => eff.size + 1
    case Type.Cst(tc) => 1
    case Type.Lambda(_, tpe) => tpe.size + 1
    case Type.Apply(tpe1, tpe2) => tpe1.size + tpe2.size + 1
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = FormatType.formatType(this)(Audience.Internal)

}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Type Constants                                                          //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Represents the Unit type.
    */
  val Unit: Type = Type.Cst(TypeConstructor.Unit)

  /**
    * Represents the Bool type.
    */
  val Bool: Type = Type.Cst(TypeConstructor.Bool)

  /**
    * Represents the Char type.
    */
  val Char: Type = Type.Cst(TypeConstructor.Char)

  /**
    * Represents the Float32 type.
    */
  val Float32: Type = Type.Cst(TypeConstructor.Float32)

  /**
    * Represents the Float64 type.
    */
  val Float64: Type = Type.Cst(TypeConstructor.Float64)

  /**
    * Represents the Int8 type.
    */
  val Int8: Type = Type.Cst(TypeConstructor.Int8)

  /**
    * Represents the Int16 type.
    */
  val Int16: Type = Type.Cst(TypeConstructor.Int16)

  /**
    * Represents the Int32 type.
    */
  val Int32: Type = Type.Cst(TypeConstructor.Int32)

  /**
    * Represents the Int64 type.
    */
  val Int64: Type = Type.Cst(TypeConstructor.Int64)

  /**
    * Represents the BigInt type.
    */
  val BigInt: Type = Type.Cst(TypeConstructor.BigInt)

  /**
    * Represents the String type.
    */
  val Str: Type = Type.Cst(TypeConstructor.Str)

  /**
    * Represents the type of an empty record.
    */
  val RecordEmpty: Type = Type.Cst(TypeConstructor.RecordEmpty)

  /**
    * Represents the type of an empty schema.
    */
  val SchemaEmpty: Type = Type.Cst(TypeConstructor.SchemaEmpty)

  /**
    * Represents the Boolean True.
    */
  val True: Type = Type.Cst(TypeConstructor.True)

  /**
    * Represents the Boolean False.
    */
  val False: Type = Type.Cst(TypeConstructor.False)

  /**
    * Represents the Pure effect. (TRUE in the Boolean algebra.)
    */
  val Pure: Type = True

  /**
    * Represents the Impure effect. (FALSE in the Boolean algebra.)
    */
  val Impure: Type = False

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind, rigidity: Rigidity = Rigidity.Flexible) extends Type with Ordered[Type.Var] {
    /**
      * The optional textual name of `this` type variable.
      */
    private var text: Option[String] = None

    /**
      * Optionally returns the textual name of `this` type variable.
      */
    def getText: Option[String] = text

    /**
      * Sets the textual name of `this` type variable.
      */
    def setText(s: String): Unit = {
      text = Some(s)
    }

    /**
      * Returns `true` if `this` type variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of `this` type variable.
      */
    override def hashCode(): Int = id

    /**
      * Compares `this` type variable to `that` type variable.
      */
    override def compare(that: Type.Var): Int = this.id - that.id
  }

  /**
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor) extends Type {
    def kind: Kind = tc.kind
  }

  /**
    * A type expression that represents a type abstraction [x] => tpe.
    */
  case class Lambda(tvar: Type.Var, tpe: Type) extends Type {
    def kind: Kind = Kind.Star ->: Kind.Star
  }

  /**
    * A type expression that a represents a type application tpe1[tpe2].
    */
  case class Apply(tpe1: Type, tpe2: Type) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined
      * from the kind of the first type argument `t1`.
      */
    def kind: Kind = {
      tpe1.kind match {
        case Kind.Arrow(kparams, k) => kparams match {
          case _ :: Nil => k
          case _ :: tail => Kind.Arrow(tail, k)
          case _ => throw InternalCompilerException(s"Illegal kind: '${tpe1.kind}' of type '$tpe1''")
        }
        case _ => throw InternalCompilerException(s"Illegal kind: '${tpe1.kind}' of type '$tpe1''")
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helper Functions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns a fresh type variable.
    */
  def freshTypeVar(k: Kind = Kind.Star, m: Rigidity = Rigidity.Flexible)(implicit flix: Flix): Type.Var =
    Type.Var(flix.genSym.freshId(), k, m)

  /**
    * Returns a fresh type variable of effect kind.
    */
  def freshEffectVar()(implicit flix: Flix): Type.Var =
    Type.Var(flix.genSym.freshId(), Kind.Effect)

  /**
    * Constructs the pure arrow type A -> B.
    */
  def mkPureArrow(a: Type, b: Type): Type = mkArrowWithEffect(a, Pure, b)

  /**
    * Constructs the impure arrow type A ~> B.
    */
  def mkImpureArrow(a: Type, b: Type): Type = mkArrowWithEffect(a, Impure, b)

  /**
    * Constructs the arrow type A -> B & e.
    */
  def mkArrowWithEffect(a: Type, e: Type, b: Type): Type = Apply(Apply(Type.Cst(TypeConstructor.Arrow(2, e)), a), b)

  /**
    * Constructs the pure curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B.
    */
  def mkPureCurriedArrow(as: List[Type], b: Type): Type = mkCurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure curried arrow type A_1 -> (A_2  -> ... -> A_n) ~> B.
    */
  def mkImpureCurriedArrow(as: List[Type], b: Type): Type = mkCurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B & e.
    */
  def mkCurriedArrowWithEffect(as: List[Type], e: Type, b: Type): Type = {
    val a = as.last
    val base = mkArrowWithEffect(a, e, b)
    as.init.foldRight(base)(mkPureArrow)
  }

  /**
    * Constructs the pure uncurried arrow type (A_1, ..., A_n) -> B.
    */
  def mkPureUncurriedArrow(as: List[Type], b: Type): Type = mkUncurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure uncurried arrow type (A_1, ..., A_n) ~> B.
    */
  def mkImpureUncurriedArrow(as: List[Type], b: Type): Type = mkUncurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the uncurried arrow type (A_1, ..., A_n) -> B & e.
    */
  def mkUncurriedArrowWithEffect(as: List[Type], e: Type, b: Type): Type = {
    val arrow = Type.Cst(TypeConstructor.Arrow(as.length + 1, e))
    val inner = as.foldLeft(arrow: Type) {
      case (acc, x) => Apply(acc, x)
    }
    Apply(inner, b)
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: Type, ts: List[Type]): Type = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t)
  }

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
  def mkTag(sym: Symbol.EnumSym, tag: String, caseType: Type, resultType: Type): Type = {
    Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Tag(sym, tag)), caseType), resultType)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val init = Type.Cst(TypeConstructor.Tuple(ts.length))
    ts.foldLeft(init: Type) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordExtend(label: String, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.RecordExtend(label)), List(tpe, rest))
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaExtend(name: String, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.SchemaExtend(name)), List(tpe, rest))
  }
}
