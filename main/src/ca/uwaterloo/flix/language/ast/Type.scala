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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TypeAliasSymUse}
import ca.uwaterloo.flix.language.ast.shared.VarText.Absent
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatType}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.util.Objects
import scala.annotation.tailrec
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
    * The location of `this` type.
    */
  def loc: SourceLocation

  /**
    * Returns the type variables in `this` type.
    *
    * Returns a sorted set to ensure that the compiler is deterministic.
    *
    * Note: We cache `typeVars` to improve performance.
    */
  lazy val typeVars: SortedSet[Type.Var] = this match {
    case x: Type.Var => SortedSet(x)

    case Type.Cst(_, _) => SortedSet.empty

    case Type.Apply(tpe1, tpe2, _) => tpe1.typeVars ++ tpe2.typeVars
    case Type.Alias(_, args, _, _) => args.foldLeft(SortedSet.empty[Type.Var])((acc, t) => acc ++ t.typeVars)
    case Type.AssocType(_, arg, _, _) => arg.typeVars // TODO ASSOC-TYPES throw error?

    case Type.JvmToType(tpe, _) => tpe.typeVars
    case Type.JvmToEff(tpe, _) => tpe.typeVars

    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.foldLeft(SortedSet.empty[Type.Var])((acc, t) => acc ++ t.typeVars)
  }

  /**
    * Gets all the effects in the given type.
    */
  def effects: SortedSet[Symbol.EffectSym] = this match {
    case Type.Cst(TypeConstructor.Effect(sym), _) => SortedSet(sym)

    case _: Type.Var => SortedSet.empty
    case _: Type.Cst => SortedSet.empty

    case Type.Apply(tpe1, tpe2, _) => tpe1.effects ++ tpe2.effects
    case Type.Alias(_, _, tpe, _) => tpe.effects
    case Type.AssocType(_, arg, _, _) => arg.effects // TODO ASSOC-TYPES throw error?

    case Type.JvmToType(tpe, _) => tpe.effects
    case Type.JvmToEff(tpe, _) => tpe.effects

    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.foldLeft(SortedSet.empty[Symbol.EffectSym])((acc, t) => acc ++ t.effects)
  }

  /**
    * Gets all the cases in the given type.
    */
  def cases: SortedSet[Symbol.RestrictableCaseSym] = this match {
    case Type.Cst(TypeConstructor.CaseSet(syms, _), _) => syms

    case _: Type.Var => SortedSet.empty
    case _: Type.Cst => SortedSet.empty

    case Type.Apply(tpe1, tpe2, _) => tpe1.cases ++ tpe2.cases
    case Type.Alias(_, _, tpe, _) => tpe.cases
    case Type.AssocType(_, arg, _, _) => arg.cases // TODO ASSOC-TYPES throw error?

    case Type.JvmToType(tpe, _) => tpe.cases
    case Type.JvmToEff(tpe, _) => tpe.cases
    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.foldLeft(SortedSet.empty[Symbol.RestrictableCaseSym])((acc, t) => acc ++ t.cases)
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
    case Type.Var(_, _) => None
    case Type.Cst(tc, _) => Some(tc)
    case Type.Apply(t1, _, _) => t1.typeConstructor
    case Type.Alias(_, _, tpe, _) => tpe.typeConstructor
    case Type.AssocType(_, _, _, _) => None // TODO ASSOC-TYPE danger!
    case Type.JvmToType(_, _) => None
    case Type.JvmToEff(_, _) => None
    case Type.UnresolvedJvmType(_, _) => None
  }

  /**
    * Returns the base type of a type application.
    * Similar to [[Type.typeConstructor]] except that here we return the type (rather than the constructor),
    * which may be a non-[[Type.Cst]] type.
    *
    * {{{
    * Option[Int] => Option
    * a[Int]      => a
    * }}}
    *
    */
  def baseType: Type.BaseType = this match {
    case Type.Apply(t1, _, _) => t1.baseType
    case bt: Type.BaseType => bt
  }

  /**
    * Returns a list of all type constructors in `this` type.
    */
  def typeConstructors: List[TypeConstructor] = this match {
    case Type.Var(_, _) => Nil
    case Type.Cst(tc, _) => tc :: Nil
    case Type.Apply(t1, t2, _) => t1.typeConstructors ::: t2.typeConstructors
    case Type.Alias(_, _, tpe, _) => tpe.typeConstructors
    case Type.AssocType(_, _, _, _) => Nil // TODO ASSOC-TYPE danger!
    case Type.JvmToType(_, _) => Nil
    case Type.JvmToEff(_, _) => Nil
    case Type.UnresolvedJvmType(_, _) => Nil
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
    case Type.Apply(tpe1, tpe2, _) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  /**
    * Returns e.g. `App(App(Tuple, Char), Char)` as `(Tuple, List(Char, Char))`.
    *
    * A more performant version of `(t.baseType, t.typeArguments)`.
    */
  def fullApply: (Type, List[Type]) = {
    @tailrec
    def helper(tpe0: Type, acc: List[Type]): (Type, List[Type]) = tpe0 match {
      case Type.Apply(tpe1, tpe2, _) => helper(tpe1, tpe2 :: acc)
      case _ => (tpe0, acc)
    }

    helper(this, Nil)
  }

  /**
    * Applies `f` to every type variable in `this` type.
    *
    * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
    */
  def map(f: Type.Var => Type): Type = this match {
    case tvar: Type.Var => f(tvar)

    case Type.Cst(_, _) => this

    case app@Type.Apply(tpe1, tpe2, loc) =>
      val t1 = tpe1.map(f)
      val t2 = tpe2.map(f)
      // Performance: Reuse this, if possible.
      app.renew(t1, t2, loc)

    case Type.Alias(sym, args, tpe, loc) =>
      // Performance: Few aliases, not worth optimizing.
      Type.Alias(sym, args.map(_.map(f)), tpe.map(f), loc)

    case Type.AssocType(sym, args, kind, loc) =>
      // Performance: Few associated types, not worth optimizing.
      Type.AssocType(sym, args.map(_.map(f)), kind, loc)

    case Type.JvmToType(tpe, loc) =>
      Type.JvmToType(tpe.map(f), loc)

    case Type.JvmToEff(tpe, loc) =>
      Type.JvmToEff(tpe.map(f), loc)

    case Type.UnresolvedJvmType(member, loc) =>
      Type.UnresolvedJvmType(member.map(t => t.map(f)), loc)
  }

  /**
    * Returns the argument types of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowArgTypes: List[Type] = typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => typeArguments.drop(1).dropRight(1)
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.", loc)
  }

  /**
    * Returns the result type of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowResultType: Type = typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => typeArguments.last
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.", loc)
  }

  /**
    * Returns the effect type of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowEffectType: Type = typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => typeArguments.head
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.", loc)
  }

  /**
    * Returns the size of `this` type.
    */
  def size: Int = this match {
    case Type.Var(_, _) => 1
    case Type.Cst(_, _) => 1
    case Type.Apply(tpe1, tpe2, _) => tpe1.size + tpe2.size + 1
    case Type.Alias(_, _, tpe, _) => tpe.size
    case Type.AssocType(_, arg, _, _) => arg.size + 1
    case Type.JvmToType(tpe, _) => tpe.size + 1
    case Type.JvmToEff(tpe, _) => tpe.size + 1
    case Type.UnresolvedJvmType(member, _) => member.getTypeArguments.map(_.size).sum + 1
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = FormatType.formatTypeWithOptions(this, FormatOptions.Internal)
}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Constants                                                               //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Reduce usage of these in favor of the ones with source locations.

  /**
    * Represents the Unit type.
    */
  val Unit: Type = Type.Cst(TypeConstructor.Unit, SourceLocation.Unknown)

  /**
    * Represents the Null type.
    */
  val Null: Type = Type.Cst(TypeConstructor.Null, SourceLocation.Unknown)

  /**
    * Represents the Bool type.
    */
  val Bool: Type = Type.Cst(TypeConstructor.Bool, SourceLocation.Unknown)

  /**
    * Represents the Char type.
    */
  val Char: Type = Type.Cst(TypeConstructor.Char, SourceLocation.Unknown)

  /**
    * Represents the Float32 type.
    */
  val Float32: Type = Type.Cst(TypeConstructor.Float32, SourceLocation.Unknown)

  /**
    * Represents the Float64 type.
    */
  val Float64: Type = Type.Cst(TypeConstructor.Float64, SourceLocation.Unknown)

  /**
    * Represents the BigDecimal type.
    */
  val BigDecimal: Type = Type.Cst(TypeConstructor.BigDecimal, SourceLocation.Unknown)

  /**
    * Represents the Int8 type.
    */
  val Int8: Type = Type.Cst(TypeConstructor.Int8, SourceLocation.Unknown)

  /**
    * Represents the Int16 type.
    */
  val Int16: Type = Type.Cst(TypeConstructor.Int16, SourceLocation.Unknown)

  /**
    * Represents the Int32 type.
    */
  val Int32: Type = Type.Cst(TypeConstructor.Int32, SourceLocation.Unknown)

  /**
    * Represents the Int64 type.
    */
  val Int64: Type = Type.Cst(TypeConstructor.Int64, SourceLocation.Unknown)

  /**
    * Represents the BigInt type.
    */
  val BigInt: Type = Type.Cst(TypeConstructor.BigInt, SourceLocation.Unknown)

  /**
    * Represents the String type.
    */
  val Str: Type = Type.Cst(TypeConstructor.Str, SourceLocation.Unknown)

  /**
    * Represents the Regex pattern type.
    */
  val Regex: Type = Type.Cst(TypeConstructor.Regex, SourceLocation.Unknown)

  /**
    * Represents the Lazy type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Lazy: Type = Type.Cst(TypeConstructor.Lazy, SourceLocation.Unknown)

  /**
    * Represents the Relation type constructor.
    */
  val Relation: Type = Type.Cst(TypeConstructor.Relation, SourceLocation.Unknown)

  /**
    * Represents the Lattice type constructor.
    */
  val Lattice: Type = Type.Cst(TypeConstructor.Lattice, SourceLocation.Unknown)

  /**
    * Represents the type of an empty record.
    */
  val RecordRowEmpty: Type = Type.Cst(TypeConstructor.RecordRowEmpty, SourceLocation.Unknown)

  /**
    * Represents the type of an empty schema.
    */
  val SchemaRowEmpty: Type = Type.Cst(TypeConstructor.SchemaRowEmpty, SourceLocation.Unknown)

  /**
    * Represents the empty effect set.
    */
  val Pure: Type = Type.Cst(TypeConstructor.Pure, SourceLocation.Unknown)

  /**
    * The union of the primitive effects.
    */
  def PrimitiveEffs: Type = Type.mkUnion(
    Symbol.PrimitiveEffs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), SourceLocation.Unknown)), SourceLocation.Unknown
  )

  /**
    * Represents the IO effect.
    */
  val IO: Type = Type.Cst(TypeConstructor.Effect(Symbol.IO), SourceLocation.Unknown)

  /**
    * Represents the Chan effect.
    */
  val Chan: Type = Type.Cst(TypeConstructor.Effect(Symbol.Chan), SourceLocation.Unknown)

  /**
   * Represents the Net effect.
   */
  val Net: Type = Type.Cst(TypeConstructor.Effect(Symbol.Net), SourceLocation.Unknown)

  /**
    * Represents the NonDet effect.
    */
  val NonDet: Type = Type.Cst(TypeConstructor.Effect(Symbol.NonDet), SourceLocation.Unknown)

  /**
    * Represents the Sys effect.
    */
  val Sys: Type = Type.Cst(TypeConstructor.Effect(Symbol.Sys), SourceLocation.Unknown)

  /**
    * Represents the universal effect set.
    */
  val Univ: Type = Type.Cst(TypeConstructor.Univ, SourceLocation.Unknown)

  /**
    * Represents the Complement type constructor.
    *
    * NB: This type has kind: Eff -> Eff.
    */
  val Complement: Type = Type.Cst(TypeConstructor.Complement, SourceLocation.Unknown)

  /**
    * Represents the Union type constructor.
    *
    * NB: This type has kind: Eff -> (Eff -> Eff).
    */
  val Union: Type = Type.Cst(TypeConstructor.Union, SourceLocation.Unknown)

  /**
    * Represents the Intersection type constructor.
    *
    * NB: This type has kind: Eff -> (Eff -> Eff).
    */
  val Intersection: Type = Type.Cst(TypeConstructor.Intersection, SourceLocation.Unknown)

  /**
    * Represents the Difference type constructor.
    *
    * NB: This type has kind: Eff -> (Eff -> Eff).
    */
  val Difference: Type = Type.Cst(TypeConstructor.Difference, SourceLocation.Unknown)

  /**
    * Represents the Symmetric Difference type constructor.
    *
    * NB: This type has kind: Eff -> (Eff -> Eff).
    */
  val SymmetricDiff: Type = Type.Cst(TypeConstructor.SymmetricDiff, SourceLocation.Unknown)

  /**
    * Represents the True Boolean algebra value.
    */
  val True: Type = Type.Cst(TypeConstructor.True, SourceLocation.Unknown)

  /**
    * Represents the False Boolean algebra value.
    */
  val False: Type = Type.Cst(TypeConstructor.False, SourceLocation.Unknown)

  /**
    * Represents the Not type constructor.
    *
    * NB: This type has kind: Bool -> Bool.
    */
  val Not: Type = Type.Cst(TypeConstructor.Not, SourceLocation.Unknown)

  /**
    * Represents the And type constructor.
    *
    * NB: This type has kind: Bool -> (Bool -> Bool).
    */
  val And: Type = Type.Cst(TypeConstructor.And, SourceLocation.Unknown)

  /**
    * Represents the Or type constructor.
    *
    * NB: This type has kind: Bool -> (Bool -> Bool).
    */
  val Or: Type = Type.Cst(TypeConstructor.Or, SourceLocation.Unknown)

  /////////////////////////////////////////////////////////////////////////////
  // Constructors                                                            //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * The union of non-Apply types.
    * Used to restrict the range of return values of [[Type.baseType]].
    */
  sealed trait BaseType extends Type

  /**
    * A type variable.
    */
  case class Var(sym: Symbol.KindedTypeVarSym, loc: SourceLocation) extends Type with BaseType with Ordered[Type.Var] {

    def withText(text: VarText): Var = Var(sym.withText(text), loc)

    def kind: Kind = sym.kind

    /**
      * Returns `true` if `this` type variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.sym == that.sym
      case _ => false
    }

    /**
      * Returns the hash code of `this` type variable.
      */
    override def hashCode(): Int = sym.hashCode

    /**
      * Compares `this` type variable to `that` type variable.
      */
    override def compare(that: Type.Var): Int = this.sym.id - that.sym.id
  }

  /**
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor, loc: SourceLocation) extends Type with BaseType {
    def kind: Kind = tc.kind

    override def hashCode(): Int = tc.hashCode()

    override def equals(o: Any): Boolean = o match {
      case that: Cst => this.tc == that.tc
      case _ => false
    }
  }

  /**
    * A type expression that a represents a type application tpe1[tpe2].
    */
  case class Apply(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined from the kind of the first type argument `t1`.
      */
    lazy val kind: Kind = {
      tpe1.kind match {
        case Kind.Arrow(_, k2) => k2
        case _ => Kind.Error
      }
    }

    override def equals(o: Any): Boolean = o match {
      case that: Apply => this.tpe1 == that.tpe1 && this.tpe2 == that.tpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe1, tpe2)

    /**
      * Creates a new Type.Apply, reusing this one if they are equivalent.
      */
    def renew(newTpe1: Type, newTpe2: Type, newLoc: SourceLocation): Type.Apply = {
      if ((tpe1 eq newTpe1) && (tpe2 eq newTpe2) && (loc eq newLoc )) {
        this
      } else {
        Type.Apply(newTpe1, newTpe2, newLoc)
      }
    }
  }

  /**
    * A type alias, including the arguments passed to it and the type it represents.
    */
  case class Alias(symUse: TypeAliasSymUse, args: List[Type], tpe: Type, loc: SourceLocation) extends Type with BaseType {
    override def kind: Kind = tpe.kind
  }

  /**
    * An associated type.
    */
  case class AssocType(symUse: AssocTypeSymUse, arg: Type, kind: Kind, loc: SourceLocation) extends Type with BaseType {
    override def equals(obj: Any): Boolean = obj match {
      case that: AssocType => this.symUse.sym == that.symUse.sym && this.arg == that.arg
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(symUse, arg)
  }

  /**
    * A type which must be reduced by finding the correct JVM constructor, method, or field.
    */
  case class JvmToType(tpe: Type, loc: SourceLocation) extends Type with BaseType {
    override def kind: Kind = Kind.Star
  }

  /**
    * An effect which must be reduced by finding the correct JVM constructor or method.
    */
  case class JvmToEff(tpe: Type, loc: SourceLocation) extends Type with BaseType {
    override def kind: Kind = Kind.Eff
  }

  /**
    * An unresolved Java type. Once the type variables are resolved, this can be reduced to a normal type.
    */
  case class UnresolvedJvmType(member: JvmMember, loc: SourceLocation) extends Type with BaseType {
    override def kind: Kind = Kind.Jvm
  }

  /**
    * An enumeration of the unresolved Java types.
    */
  sealed trait JvmMember {

    /**
      * Returns all the types in `this`.
      */
    def getTypeArguments: List[Type] = this match {
      case JvmMember.JvmConstructor(_, tpes) => tpes
      case JvmMember.JvmField(_, tpe, _) => List(tpe)
      case JvmMember.JvmMethod(tpe, _, tpes) => tpe :: tpes
      case JvmMember.JvmStaticMethod(_, _, tpes) => tpes
    }

    /**
      * Transforms `this` by executing `f` on all the types in `this`.
      */
    def map(f: Type => Type): JvmMember = this match {
      case JvmMember.JvmConstructor(clazz, tpes) => JvmMember.JvmConstructor(clazz, tpes.map(f))
      case JvmMember.JvmField(base, tpe, name) => JvmMember.JvmField(base, f(tpe), name)
      case JvmMember.JvmMethod(tpe, name, tpes) => JvmMember.JvmMethod(f(tpe), name, tpes.map(f))
      case JvmMember.JvmStaticMethod(clazz, name, tpes) => JvmMember.JvmStaticMethod(clazz, name, tpes.map(f))
    }
  }

  object JvmMember {

    /**
      * A Java constructor, defined by its class and argument types.
      */
    case class JvmConstructor(clazz: Class[?], tpes: List[Type]) extends JvmMember

    /**
      * A Java field, defined by the receiver type and the field name.
      */
    case class JvmField(base: SourceLocation, tpe: Type, name: Name.Ident) extends JvmMember

    /**
      * A Java method, defined by the receiver type, the method name, and the argument types.
      */
    case class JvmMethod(tpe: Type, name: Name.Ident, tpes: List[Type]) extends JvmMember

    /**
      * A Java static method, defined by the class, the method name, and the argument types.
      */
    case class JvmStaticMethod(clazz: Class[?], name: Name.Ident, tpes: List[Type]) extends JvmMember
  }

  /////////////////////////////////////////////////////////////////////////////
  // Utility Functions                                                       //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(k: Kind, loc: SourceLocation, text: VarText = VarText.Absent)(implicit scope: Scope, flix: Flix): Type.Var = {
    val sym = Symbol.freshKindedTypeVarSym(text, k, isSlack = false, loc)
    Type.Var(sym, loc)
  }

  /**
    * Returns a fresh effect slack variable.
    */
  def freshEffSlackVar(loc: SourceLocation)(implicit scope: Scope, flix: Flix): Type.Var = {
    val sym = Symbol.freshKindedTypeVarSym(Absent, Kind.Eff, isSlack = true, loc)
    Type.Var(sym, loc)
  }

  /**
    * Returns a fresh error type of the given kind `k`.
    */
  def freshError(k: Kind, loc: SourceLocation)(implicit flix: Flix): Type = {
    val id = flix.genSym.freshId()
    Type.Cst(TypeConstructor.Error(id, k), loc)
  }

  /**
    * Returns the AnyType type with given source location `loc`.
    */
  def mkAnyType(loc: SourceLocation): Type = Type.Cst(TypeConstructor.AnyType, loc)

  /**
    * Returns the Unit type with given source location `loc`.
    */
  def mkUnit(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Unit, loc)

  /**
    * Returns the Null type with the given source location `loc`.
    */
  def mkNull(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Null, loc)

  /**
    * Returns the Bool type with the given source location `loc`.
    */
  def mkBool(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Bool, loc)

  /**
    * Returns the Char type with the given source location `loc`.
    */
  def mkChar(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Char, loc)

  /**
    * Returns the Float32 type with the given source location `loc`.
    */
  def mkFloat32(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Float32, loc)

  /**
    * Returns the Float64 type with the given source location `loc`.
    */
  def mkFloat64(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Float64, loc)

  /**
    * Returns the BigDecimal type with the given source location `loc`.
    */
  def mkBigDecimal(loc: SourceLocation): Type = Type.Cst(TypeConstructor.BigDecimal, loc)

  /**
    * Returns the Int8 type with the given source location `loc`.
    */
  def mkInt8(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int8, loc)

  /**
    * Returns the Int16 type with the given source location `loc`.
    */
  def mkInt16(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int16, loc)

  /**
    * Returns the Int32 type with the given source location `loc`.
    */
  def mkInt32(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int32, loc)

  /**
    * Returns the Int64 type with the given source location `loc`.
    */
  def mkInt64(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int64, loc)

  /**
    * Returns the BigInt type with the given source location `loc`.
    */
  def mkBigInt(loc: SourceLocation): Type = Type.Cst(TypeConstructor.BigInt, loc)

  /**
    * Returns the String type with the given source location `loc`.
    */
  def mkString(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Str, loc)

  /**
    * Returns the Regex pattern type with the given source location `loc`.
    */
  def mkRegex(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Regex, loc)

  /**
    * Returns the Pure type with the given source location `loc`.
    */
  def mkPure(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Pure, loc)

  /**
    * Returns the EffUniv type with the given source location `loc`.
    */
  def mkEffUniv(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Univ, loc)

  /**
    * Returns the type `Sender[tpe]` with the given optional source location `loc`.
    */
  def mkSender(tpe: Type, loc: SourceLocation): Type =
    Apply(Cst(TypeConstructor.Sender, loc), tpe, loc)

  /**
    * Returns the type `Receiver[tpe]` with the given optional source location `loc`.
    */
  def mkReceiver(tpe: Type, loc: SourceLocation): Type =
    Apply(Cst(TypeConstructor.Receiver, loc), tpe, loc)

  /**
    * Returns the Lazy type with the given source location `loc`.
    */
  def mkLazy(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Lazy, loc)

  /**
    * Returns the type `Lazy[tpe]` with the given optional source location `loc`.
    */
  def mkLazy(tpe: Type, loc: SourceLocation): Type = Type.Apply(Type.Cst(TypeConstructor.Lazy, loc), tpe, loc)

  /**
    * Returns the type `Array[tpe, reg]` with the given source location `loc`.
    */
  def mkArray(tpe: Type, reg: Type, loc: SourceLocation): Type =
    Apply(Apply(Cst(TypeConstructor.Array, loc), tpe, loc), reg, loc)

  /**
    * Returns the type `ArrayWithoutRegion[tpe]` with the given source location `loc`.
    */
  def mkArrayWithoutRegion(tpe: Type, loc: SourceLocation): Type =
    Apply(Cst(TypeConstructor.ArrayWithoutRegion, loc), tpe, loc)

  /**
    * Returns the type `Array[tpe]` with the given source location `loc`.
    */
  def mkVector(tpe: Type, loc: SourceLocation): Type =
    Apply(Cst(TypeConstructor.Vector, loc), tpe, loc)

  /**
    * Constructs the pure arrow type A -> B.
    */
  def mkPureArrow(a: Type, b: Type, loc: SourceLocation): Type = mkArrowWithEffect(a, Pure, b, loc)

  /**
    * Constructs the IO arrow type A -> B \ IO.
    */
  def mkIoArrow(a: Type, b: Type, loc: SourceLocation): Type = mkArrowWithEffect(a, IO, b, loc)

  /**
    * Constructs the arrow type A -> B \ p.
    */
  def mkArrowWithEffect(a: Type, p: Type, b: Type, loc: SourceLocation): Type = mkApply(Type.Cst(TypeConstructor.Arrow(2), loc), List(p, a, b), loc)

  /**
    * Constructs the pure curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B.
    *
    * Returns `b` if `as` is empty.
    */
  def mkPureCurriedArrow(as: List[Type], b: Type, loc: SourceLocation): Type = mkCurriedArrowWithEffect(as, Pure, b, loc)

  /**
    * Constructs the curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B \ e.
    *
    * Returns `b` if `as` is empty.
    */
  def mkCurriedArrowWithEffect(as: List[Type], p: Type, b: Type, loc: SourceLocation): Type = {
    if (as.isEmpty) {
      b
    } else {
      val a = as.last
      val base = mkArrowWithEffect(a, p, b, loc)
      as.init.foldRight(base)(mkPureArrow(_, _, loc))
    }
  }

  /**
    * Constructs the pure uncurried arrow type (A_1, ..., A_n) -> B.
    *
    * Returns `b` if `as` is empty.
    */
  def mkPureUncurriedArrow(as: List[Type], b: Type, loc: SourceLocation): Type = mkUncurriedArrowWithEffect(as, Pure, b, loc)

  /**
    * Constructs the IO uncurried arrow type (A_1, ..., A_n) -> B \ IO.
    *
    * Returns `b` if `as` is empty.
    */
  def mkIoUncurriedArrow(as: List[Type], b: Type, loc: SourceLocation): Type = mkUncurriedArrowWithEffect(as, IO, b, loc)

  /**
    * Constructs the uncurried arrow type (A_1, ..., A_n) -> B \ p.
    *
    * Returns `b` if `as` is empty.
    */
  def mkUncurriedArrowWithEffect(as: List[Type], p: Type, b: Type, loc: SourceLocation): Type = {
    if (as.isEmpty) {
      b
    } else {
      val arrow = mkApply(Type.Cst(TypeConstructor.Arrow(as.length + 1), loc), List(p), loc)
      val inner = as.foldLeft(arrow: Type) {
        case (acc, x) => Apply(acc, x, loc)
      }
      Apply(inner, b, loc)
    }
  }

  /**
    * Constructs the backend arrow type (A_1, ..., A_n) -> B.
    *
    * Returns `b` if `as` is empty.
    */
  def mkArrowWithoutEffect(as: List[Type], b: Type, loc: SourceLocation): Type = {
    if (as.isEmpty) {
      b
    } else {
      val arrow = Type.Cst(TypeConstructor.ArrowWithoutEffect(as.length + 1), loc)
      val inner = as.foldLeft(arrow: Type) {
        case (acc, x) => Apply(acc, x, loc)
      }
      Apply(inner, b, loc)
    }
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: Type, ts: List[Type], loc: SourceLocation): Type = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t, loc)
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, k: Kind, loc: SourceLocation): Type = Type.Cst(TypeConstructor.Enum(sym, k), loc)

  /**
    * Construct the enum type `Sym[ts]`.
    */
  def mkEnum(sym: Symbol.EnumSym, ts: List[Type], loc: SourceLocation): Type = mkApply(Type.Cst(TypeConstructor.Enum(sym, Kind.mkArrow(ts.length)), loc), ts, loc)

  /**
    * Construct the struct type `Sym[ts]`
    */
  def mkStruct(sym: Symbol.StructSym, ts: List[Type], loc: SourceLocation): Type = {
    mkApply(Type.Cst(TypeConstructor.Struct(sym, Kind.mkArrow(ts.map(_.kind))), loc), ts, loc)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type], loc: SourceLocation): Type = {
    val init = Type.Cst(TypeConstructor.Tuple(ts.length), loc)
    ts.foldLeft(init: Type) {
      case (acc, x) => Apply(acc, x, loc)
    }
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    *
    * Returns Unit if the list is empty.
    * Returns the head of the list if it is a singleton list.
    */
  def mkTuplish(ts: List[Type], loc: SourceLocation): Type = ts match {
    case Nil => Type.mkUnit(loc)
    case t :: Nil => t
    case list => mkTuple(list, loc)
  }

  /**
    * Constructs the a native type.
    */
  def mkNative(clazz: Class[?], loc: SourceLocation): Type = Type.Cst(TypeConstructor.Native(clazz), loc)

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordRowEmpty(loc: SourceLocation): Type = {
    Type.Cst(TypeConstructor.RecordRowEmpty, loc)
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaRowEmpty(loc: SourceLocation): Type = {
    Type.Cst(TypeConstructor.SchemaRowEmpty, loc)
  }

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordRowExtend(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation): Type = {
    mkApply(Type.Cst(TypeConstructor.RecordRowExtend(label), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaRowExtend(pred: Name.Pred, tpe: Type, rest: Type, loc: SourceLocation): Type = {
    mkApply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a Record type.
    */
  def mkRecord(tpe: Type, loc: SourceLocation): Type = {
    Apply(Type.Cst(TypeConstructor.Record, loc), tpe, loc)
  }

  /**
    * Constructs a Schema type.
    */
  def mkSchema(tpe: Type, loc: SourceLocation): Type = {
    Apply(Type.Cst(TypeConstructor.Schema, loc), tpe, loc)
  }

  /**
    * Construct a relation type with the given list of type arguments `ts0`.
    */
  def mkRelation(ts0: List[Type], loc: SourceLocation): Type = {
    val ts = ts0 match {
      case Nil => Type.Unit
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(Relation, ts, loc)
  }

  /**
    * Construct a lattice type with the given list of type arguments `ts0`.
    */
  def mkLattice(ts0: List[Type], loc: SourceLocation): Type = {
    val ts = ts0 match {
      case Nil => Type.Unit
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(Lattice, ts, loc)
  }

  /**
    * Returns the type `Complement(tpe0)`.
    */
  def mkComplement(tpe0: Type, loc: SourceLocation): Type = tpe0 match {
    case Type.Pure => Type.mkEffUniv(loc)
    case Type.Univ => Type.mkPure(loc)
    case _ => Type.Apply(Type.Cst(TypeConstructor.Complement, loc), tpe0, loc)
  }

  /**
    * Returns the type `Not(tpe0)`.
    */
  def mkNot(tpe0: Type, loc: SourceLocation): Type = tpe0 match {
    case Type.True => Type.False
    case Type.False => Type.True
    case _ => Type.Apply(Type.Cst(TypeConstructor.Not, loc), tpe0, loc)
  }

  /**
    * Returns the type `Union(tpe1, tpe2)`.
    */
  def mkUnion(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.Pure, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.Pure, _)) => tpe1
    case (Type.Cst(TypeConstructor.Univ, _), _) => Type.Univ
    case (_, Type.Cst(TypeConstructor.Univ, _)) => Type.Univ
    case (Type.Var(sym1, _), Type.Var(sym2, _)) if sym1 == sym2 => tpe1
    case _ => Type.Apply(Type.Apply(Type.Union, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `And(tpe1, And(tpe2, tpe3))`.
    */
  def mkUnion(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation): Type =
    mkUnion(tpe1, mkUnion(tpe2, tpe3, loc), loc)

  /**
    * Returns the type `And(tpe1, And(tpe2, And(tpe3, tpe4)))`.
    */
  def mkUnion(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type, loc: SourceLocation): Type =
    mkUnion(tpe1, mkUnion(tpe2, mkUnion(tpe3, tpe4, loc), loc), loc)

  /**
    * Returns the type `And(tpe1, And(tpe2, ...))`.
    */
  def mkUnion(tpes: List[Type], loc: SourceLocation): Type = tpes match {
    case Nil => Type.Pure
    case x :: xs => mkUnion(x, mkUnion(xs, loc), loc)
  }

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkIntersection(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.Pure, _), _) => Type.Pure
    case (_, Type.Cst(TypeConstructor.Pure, _)) => Type.Pure
    case (Type.Cst(TypeConstructor.Univ, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.Univ, _)) => tpe1
    case (Type.Var(sym1, _), Type.Var(sym2, _)) if sym1 == sym2 => tpe1
    case _ => Type.Apply(Type.Apply(Type.Intersection, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `Or(tpe1, Or(tpe2, ...))`.
    */
  def mkIntersection(tpes: List[Type], loc: SourceLocation): Type = tpes match {
    case Nil => Type.Univ
    case x :: xs => mkIntersection(x, mkIntersection(xs, loc), loc)
  }

  /**
    * Returns the type `Xor(tpe1, tpe2)`.
    */
  def mkSymmetricDiff(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.Pure, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.Pure, _)) => tpe1
    case _ => Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, loc), tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `Xor(tpe1, Xor(tpe2, ...))`.
    */
  def mkSymmetricDiff(tpes: List[Type], loc: SourceLocation): Type = tpes match {
    case Nil => Type.Pure
    case x :: xs => mkSymmetricDiff(x, mkSymmetricDiff(xs, loc), loc)
  }

  /**
    * Returns the type `tpe1 - tpe2`.
    */
  def mkDifference(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.Pure, _), _) => Type.Pure
    case (_, Type.Cst(TypeConstructor.Pure, _)) => tpe1
    case (Type.Cst(TypeConstructor.Univ, _), _) => Type.mkComplement(tpe2, loc)
    case (_, Type.Cst(TypeConstructor.Univ, _)) => Type.Pure
    case (Type.Var(sym1, _), Type.Var(sym2, _)) if sym1 == sym2 => Type.Pure
    case _ => Type.Apply(Type.Apply(Type.Difference, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.True, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.True, _)) => tpe1
    case (Type.Cst(TypeConstructor.False, _), _) => Type.False
    case (_, Type.Cst(TypeConstructor.False, _)) => Type.False
    case (Type.Var(sym1, _), Type.Var(sym2, _)) if sym1 == sym2 => tpe1
    case _ => Type.Apply(Type.Apply(Type.And, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.True, _), _) => Type.True
    case (_, Type.Cst(TypeConstructor.True, _)) => Type.True
    case (Type.Cst(TypeConstructor.False, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.False, _)) => tpe1
    case (Type.Var(sym1, _), Type.Var(sym2, _)) if sym1 == sym2 => tpe1
    case _ => Type.Apply(Type.Apply(Type.Or, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns the type `And(tpe1, And(tpe2, ...))`.
    */
  def mkAnd(tpes: List[Type], loc: SourceLocation): Type = tpes match {
    case Nil => Type.True
    case x :: xs => mkAnd(x, mkAnd(xs, loc), loc)
  }

  /**
    * Returns the type `Or(tpe1, Or(tpe2, ...))`.
    */
  def mkOr(tpes: List[Type], loc: SourceLocation): Type = tpes match {
    case Nil => Type.False
    case x :: xs => mkOr(x, mkOr(xs, loc), loc)
  }

  /**
    * Returns the complement of the given type.
    */
  def mkCaseComplement(tpe: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.CaseComplement(_), _), tpe2, _) => tpe2
    // TODO RESTR-VARS use universe?
    case t => Type.Apply(Type.Cst(TypeConstructor.CaseComplement(sym), loc), t, loc)
  }

  /**
    * Returns the type `tpe1 + tpe2`
    */
  def mkCaseUnion(tpe1: Type, tpe2: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
      Type.Cst(TypeConstructor.CaseSet(syms1 ++ syms2, sym), loc)
    case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), t) if syms1.isEmpty => t
    case (t, Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) if syms2.isEmpty => t
    // TODO RESTR-VARS ALL case: universe
    case _ => mkApply(Type.Cst(TypeConstructor.CaseUnion(sym), loc), List(tpe1, tpe2), loc)
  }

  /**
    * Returns the type `tpe1 & tpe2`
    */
  def mkCaseIntersection(tpe1: Type, tpe2: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
      Type.Cst(TypeConstructor.CaseSet(syms1 & syms2, sym), loc)
    case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), _) if syms1.isEmpty => Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), loc)
    case (_, Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) if syms2.isEmpty => Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), loc)
    // TODO RESTR-VARS ALL case: universe
    case _ => mkApply(Type.Cst(TypeConstructor.CaseIntersection(sym), loc), List(tpe1, tpe2), loc)
  }

  /**
    * Returns the difference of the given types.
    */
  def mkCaseDifference(tpe1: Type, tpe2: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = {
    mkCaseIntersection(tpe1, mkCaseComplement(tpe2, sym, loc), sym, loc)
  }

  /**
    * Returns a Region type for the given region argument `r` with the given source location `loc`.
    */
  def mkRegionToStar(r: Type, loc: SourceLocation): Type =
    Type.Apply(Type.Cst(TypeConstructor.RegionToStar, loc), r, loc)

  /**
    * Returns a region type with the given symbol.
    */
  def mkRegion(sym: Symbol.RegionSym, loc: SourceLocation): Type = {
    Type.Cst(TypeConstructor.Region(sym), loc)
  }

  /**
    * Replace type aliases with the types they represent.
    * In general, this function should be used in back-end phases
    * to clear all aliases for easier processing.
    */
  def eraseAliases(t: Type): Type = t match {
    case tvar: Type.Var => tvar
    case Type.Cst(_, _) => t
    case Type.Apply(tpe1, tpe2, loc) => Type.Apply(eraseAliases(tpe1), eraseAliases(tpe2), loc)
    case Type.Alias(_, _, tpe, _) => eraseAliases(tpe)
    case Type.AssocType(cst, args, kind, loc) => Type.AssocType(cst, args.map(eraseAliases), kind, loc)
    case Type.JvmToType(tpe, loc) => Type.JvmToType(eraseAliases(tpe), loc)
    case Type.JvmToEff(tpe, loc) => Type.JvmToEff(eraseAliases(tpe), loc)
    case Type.UnresolvedJvmType(member, loc) => Type.UnresolvedJvmType(member.map(eraseAliases), loc)
  }

  /**
    * Replace top-level type aliases with the types they represent.
    * In general, this function should be used in front-end phases
    * to maintain aliases in error messages to the extent possible.
    */
  @tailrec
  def eraseTopAliases(t: Type): Type = t match {
    case Type.Alias(_, _, tpe, _) => eraseTopAliases(tpe)
    case tpe => tpe
  }

  /**
    * Returns true if the given type contains an associated type somewhere within it.
    */
  def hasAssocType(t: Type): Boolean = t match {
    case Var(_, _) => false
    case Cst(_, _) => false
    case Apply(tpe1, tpe2, _) => hasAssocType(tpe1) || hasAssocType(tpe2)
    case Alias(_, _, tpe, _) => hasAssocType(tpe)
    case AssocType(_, _, _, _) => true
    case JvmToType(tpe, _) => hasAssocType(tpe)
    case JvmToEff(tpe, _) => hasAssocType(tpe)
    case UnresolvedJvmType(member, _) => member.getTypeArguments.exists(hasAssocType)
  }

  /**
    * Returns true if the given type contains [[Type.JvmToType]] or [[Type.UnresolvedJvmType]] somewhere within it.
    */
  def hasJvmType(tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) => false
    case Type.Cst(_, _) => false
    case Type.Apply(tpe1, tpe2, _) => hasJvmType(tpe1) || hasJvmType(tpe2)
    case Type.Alias(_, _, tpe, _) => hasJvmType(tpe)
    case Type.AssocType(_, arg, _, _) => hasJvmType(arg)
    case Type.JvmToType(_, _) => true
    case Type.JvmToEff(_, _) => true
    case Type.UnresolvedJvmType(_, _) => true
  }

  /**
    * Returns true if the given type contains [[TypeConstructor.Error]].
    */
  def hasError(tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) => false
    case Type.Cst(tc, _) => tc match {
      case TypeConstructor.Error(_, _) => true
      case _ => false
    }
    case Type.Apply(tpe1, tpe2, _) => hasError(tpe1) || hasError(tpe2)
    case Type.Alias(_, _, tpe, _) => hasError(tpe)
    case Type.AssocType(_, arg, _, _) => hasError(arg)
    case Type.JvmToType(_, _) => false
    case Type.JvmToEff(_, _) => false
    case Type.UnresolvedJvmType(_, _) => false
  }

  /**
    * Returns the Flix Type of a Java Class.
    *
    * Arrays are returned with the [[Type.IO]] region.
    *
    * Returns a [[TypeConstructor.Native]] of `c` if nothing more specific is found.
    */
  def getFlixType(c: Class[?]): Type = {
    if (c == java.lang.Boolean.TYPE) {
      Type.Bool
    }  else if (c == java.lang.Byte.TYPE) {
      Type.Int8
    }  else if (c == java.lang.Short.TYPE) {
      Type.Int16
    }  else if (c == java.lang.Integer.TYPE) {
      Type.Int32
    }  else if (c == java.lang.Long.TYPE) {
      Type.Int64
    }  else if (c == java.lang.Character.TYPE) {
      Type.Char
    }  else if (c == java.lang.Float.TYPE) {
      Type.Float32
    }  else if (c == java.lang.Double.TYPE) {
      Type.Float64
    }  else if (c == classOf[java.math.BigDecimal]) {
      Type.BigDecimal
    }  else if (c == classOf[java.math.BigInteger]) {
      Type.BigInt
    }  else if (c == classOf[java.lang.String]) {
      Type.Str
    }  else if (c == classOf[java.util.regex.Pattern]) {
      Type.Regex
    }  else if (c == java.lang.Void.TYPE) {
      Type.Unit
    } else if (c.isArray) {
      val comp = c.getComponentType
      val elmType = getFlixType(comp)
      Type.mkArray(elmType, Type.IO, SourceLocation.Unknown)
    } else {
      Type.mkNative(c, SourceLocation.Unknown)
    }
  }

  /**
    * Returns the [[Class]] object of `tpe`, if it exists.
    *
    * Almost the inverse function of [[getFlixType]], but arrays and unit returns None.
    */
  def classFromFlixType(tpe: Type): Option[Class[?]] = tpe match {
    case Type.Bool =>
      Some(java.lang.Boolean.TYPE)
    case Type.Int8 =>
      Some(java.lang.Byte.TYPE)
    case Type.Int16 =>
      Some(java.lang.Short.TYPE)
    case Type.Int32 =>
      Some(java.lang.Integer.TYPE)
    case Type.Int64 =>
      Some(java.lang.Long.TYPE)
    case Type.Char =>
      Some(java.lang.Character.TYPE)
    case Type.Float32 =>
      Some(java.lang.Float.TYPE)
    case Type.Float64 =>
      Some(java.lang.Double.TYPE)
    case Type.Cst(TypeConstructor.BigDecimal, _) =>
      Some(classOf[java.math.BigDecimal])
    case Type.Cst(TypeConstructor.BigInt, _) =>
      Some(classOf[java.math.BigInteger])
    case Type.Cst(TypeConstructor.Str, _) =>
      Some(classOf[String])
    case Type.Cst(TypeConstructor.Regex, _) =>
      Some(classOf[java.util.regex.Pattern])
    case Type.Cst(TypeConstructor.Native(clazz), _) =>
      Some(clazz)
    case _ => None
  }

  /**
    * Returns the type of the given constant.
    */
  def constantType(cst: Constant): Type = cst match {
    case Constant.Unit => Type.Unit
    case Constant.Null => Type.Null
    case Constant.Bool(_) => Type.Bool
    case Constant.Char(_) => Type.Char
    case Constant.Float32(_) => Type.Float32
    case Constant.Float64(_) => Type.Float64
    case Constant.BigDecimal(_) => Type.BigDecimal
    case Constant.Int8(_) => Type.Int8
    case Constant.Int16(_) => Type.Int16
    case Constant.Int32(_) => Type.Int32
    case Constant.Int64(_) => Type.Int64
    case Constant.BigInt(_) => Type.BigInt
    case Constant.Str(_) => Type.Str
    case Constant.Regex(_) => Type.Regex
    case Constant.RecordEmpty => Type.mkRecord(Type.RecordRowEmpty, SourceLocation.Unknown)
  }

  /**
    * Replaces the given region in the type with the Pure effect.
    */
  def purifyRegion(tpe0: Type, sym: Symbol.RegionSym): Type = tpe0 match {
    case Cst(TypeConstructor.Region(sym1), _) if sym == sym1 => Type.Pure
    case t: Cst => t
    case t: Var => t
    case Apply(tpe1, tpe2, loc) =>
      val t1 = purifyRegion(tpe1, sym)
      val t2 = purifyRegion(tpe2, sym)
      Type.Apply(t1, t2, loc)
    case Alias(_, _, tpe, loc) =>
      purifyRegion(tpe, sym)
    case AssocType(symUse, arg, kind, loc) =>
      val a = purifyRegion(arg, sym)
      AssocType(symUse, a, kind, loc)
    case JvmToType(tpe, loc) =>
      val t = purifyRegion(tpe, sym)
      JvmToType(t, loc)
    case JvmToEff(tpe, loc) =>
      val t = purifyRegion(tpe, sym)
      JvmToEff(t, loc)
    case UnresolvedJvmType(member, loc) =>
      val m = member match {
        case JvmMember.JvmConstructor(clazz, tpes) =>
          val ts = tpes.map(purifyRegion(_, sym))
          JvmMember.JvmConstructor(clazz, ts)
        case JvmMember.JvmField(base, tpe, name) =>
          val t = purifyRegion(tpe, sym)
          JvmMember.JvmField(base, t, name)
        case JvmMember.JvmMethod(tpe, name, tpes) =>
          val t = purifyRegion(tpe, sym)
          val ts = tpes.map(purifyRegion(_, sym))
          JvmMember.JvmMethod(t, name, ts)
        case JvmMember.JvmStaticMethod(clazz, name, tpes) =>
          val ts = tpes.map(purifyRegion(_, sym))
          JvmMember.JvmStaticMethod(clazz, name, ts)
      }
      UnresolvedJvmType(m, loc)
  }

}
