/*
 * Copyright 2022 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.shared.ScalaAnnotations.EliminatedBy
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TypeAliasSymUse}
import ca.uwaterloo.flix.language.phase.Resolver
import ca.uwaterloo.flix.util.InternalCompilerException

import java.util.Objects
import scala.collection.immutable.SortedSet

sealed trait UnkindedType {
  def loc: SourceLocation

  /**
    * Maps all the type vars in the type according to the given function `f`.
    */
  def map(f: Symbol.UnkindedTypeVarSym => UnkindedType): UnkindedType = this match {
    case UnkindedType.Var(sym, _) => f(sym)
    case t: UnkindedType.Cst => t
    case t: UnkindedType.Enum => t
    case t: UnkindedType.Struct => t
    case t: UnkindedType.RestrictableEnum => t
    case t: UnkindedType.UnappliedAlias => t
    case t: UnkindedType.UnappliedAssocType => t
    case t: UnkindedType.UnappliedGetEff => t
    case t: UnkindedType.CaseSet => t
    case UnkindedType.Apply(tpe1, tpe2, loc) => UnkindedType.Apply(tpe1.map(f), tpe2.map(f), loc)
    case UnkindedType.Arrow(eff, arity, loc) => UnkindedType.Arrow(eff.map(_.map(f)), arity, loc)
    case UnkindedType.CaseComplement(tpe, loc) => UnkindedType.CaseComplement(tpe.map(f), loc)
    case UnkindedType.CaseUnion(tpe1, tpe2, loc) => UnkindedType.CaseUnion(tpe1.map(f), tpe2.map(f), loc)
    case UnkindedType.CaseIntersection(tpe1, tpe2, loc) => UnkindedType.CaseIntersection(tpe1.map(f), tpe2.map(f), loc)
    case UnkindedType.Ascribe(tpe, kind, loc) => UnkindedType.Ascribe(tpe.map(f), kind, loc)
    case UnkindedType.Alias(cst, args, tpe, loc) => UnkindedType.Alias(cst, args.map(_.map(f)), tpe.map(f), loc)
    case UnkindedType.AssocType(cst, arg, loc) => UnkindedType.AssocType(cst, arg.map(f), loc)
    case UnkindedType.GetEff(action, tpe, loc) => UnkindedType.GetEff(action, tpe.map(f), loc)
    case t: UnkindedType.Error => t
  }

  /**
    * Returns the base type.
    *
    * For example,
    * X[a, b, c] returns X
    *
    */
  def baseType: UnkindedType = this match {
    case UnkindedType.Apply(tpe1, _, _) => tpe1.baseType
    case t => t
  }

  /**
    * Returns the type arguments.
    *
    * For example
    * X[a, b, c] returns [a, b, c]
    */
  def typeArguments: List[UnkindedType] = this match {
    case UnkindedType.Apply(tpe1, tpe2, _) => tpe1.typeArguments :+ tpe2
    case _ => Nil
  }

  /**
    * Type vars that are certain to appear in the type even after reduction.
    *
    * (Boolean variables may appear in this set even if they are not essential.)
    */
  def definiteTypeVars: SortedSet[Symbol.UnkindedTypeVarSym] = this match {
    case UnkindedType.Var(sym, _) => SortedSet(sym)
    case UnkindedType.Cst(_, _) => SortedSet.empty
    case UnkindedType.Enum(_, _) => SortedSet.empty
    case UnkindedType.Struct(_, _) => SortedSet.empty
    case UnkindedType.RestrictableEnum(_, _) => SortedSet.empty
    case UnkindedType.UnappliedAlias(_, _) => SortedSet.empty
    case UnkindedType.UnappliedAssocType(_, _) => SortedSet.empty
    case UnkindedType.UnappliedGetEff(_, _) => SortedSet.empty
    case UnkindedType.Apply(tpe1, tpe2, _) => tpe1.definiteTypeVars ++ tpe2.definiteTypeVars
    case UnkindedType.Arrow(eff, _, _) => eff.iterator.flatMap(_.definiteTypeVars).to(SortedSet)
    case UnkindedType.CaseSet(_, _) => SortedSet.empty
    case UnkindedType.CaseComplement(tpe, _) => tpe.definiteTypeVars
    case UnkindedType.CaseUnion(tpe1, tpe2, _) => tpe1.definiteTypeVars ++ tpe2.definiteTypeVars
    case UnkindedType.CaseIntersection(tpe1, tpe2, _) => tpe1.definiteTypeVars ++ tpe2.definiteTypeVars
    case UnkindedType.Ascribe(tpe, _, _) => tpe.definiteTypeVars

    // For aliases we used the reduced type
    case UnkindedType.Alias(_, _, tpe, _) => tpe.definiteTypeVars
    // For associated types we cannot yet reduce, so we are conservative and say none.
    case UnkindedType.AssocType(_, _, _) => SortedSet.empty
    case UnkindedType.GetEff(_, _, _) => SortedSet.empty

    case UnkindedType.Error(_) => SortedSet.empty
  }
}

object UnkindedType {

  /**
    * A type variable.
    */
  case class Var(sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Var(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }

  /**
    * A type constant.
    */
  case class Cst(tc: TypeConstructor, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Cst(tc2, _) => tc == tc2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tc)
  }

  /**
    * An unkinded enum.
    */
  case class Enum(sym: Symbol.EnumSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Enum(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }

  /**
   * An unkinded struct.
   */
  case class Struct(sym: Symbol.StructSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Struct(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }

  /**
    * An unkinded restrictable enum.
    */
  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case RestrictableEnum(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }

  // MATT docs
  case class GetEff(action: RegionAction, tpe: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case GetEff(action2, tpe2, _) => action == action2 && tpe == tpe2
    }
  }

  /**
    * An unapplied alias.
    * Only exists temporarily in the Resolver until it's converted to an [[Alias]].
    */
  @EliminatedBy(Resolver.getClass)
  case class UnappliedAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case UnappliedAlias(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }

  /**
    * An unapplied associated type.
    * Only exists temporarily in the Resolver until it's converted to an [[AssocType]].
    */
  @EliminatedBy(Resolver.getClass)
  case class UnappliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case UnappliedAssocType(sym2, _) => sym == sym2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(sym)
  }


  // MATT docs
  case class UnappliedGetEff(action: RegionAction, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case UnappliedGetEff(action2, _) => action == action2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(action)
  }

  /**
    * A type application.
    */
  case class Apply(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Apply(tpe1_2, tpe2_2, _) => tpe1 == tpe1_2 && tpe2 == tpe2_2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe1, tpe2)
  }

  /**
    * A function type.
    */
  case class Arrow(eff: Option[UnkindedType], arity: Int, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Arrow(eff2, arity2, _) => eff2 == eff && arity == arity2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(eff, arity)
  }

  /**
    * A case set type.
    */
  case class CaseSet(cases: List[Symbol.RestrictableCaseSym], loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case CaseSet(cases2, _) => cases == cases2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(cases)
  }

  /**
    * A case complement type.
    */
  case class CaseComplement(tpe: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case CaseComplement(tpe2, _) => tpe == tpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe)
  }

  /**
    * A case union type.
    */
  case class CaseUnion(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case CaseUnion(thatTpe1, thatTpe2, _) => tpe1 == thatTpe1 && tpe2 == thatTpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe1, tpe2)
  }

  /**
    * A case intersection type.
    */
  case class CaseIntersection(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case CaseIntersection(thatTpe1, thatTpe2, _) => tpe1 == thatTpe1 && tpe2 == thatTpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe1, tpe2)
  }

  /**
    * A type with a kind ascription.
    */
  case class Ascribe(tpe: UnkindedType, kind: Kind, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Ascribe(tpe2, kind2, _) => tpe == tpe2 && kind == kind2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(tpe, kind)
  }

  /**
    * A fully resolved type alias.
    */
  case class Alias(symUse: TypeAliasSymUse, args: List[UnkindedType], tpe: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Alias(TypeAliasSymUse(sym2, _), args2, tpe2, _) => symUse.sym == sym2 && args == args2 && tpe == tpe2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(symUse, args, tpe)
  }

  /**
    * A fully resolved associated type.
    */
  case class AssocType(assocTypeSymUse: AssocTypeSymUse, arg: UnkindedType, loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case AssocType(AssocTypeSymUse(sym2, _), arg2, _) => assocTypeSymUse.sym == sym2 && arg == arg2
      case _ => false
    }

    override def hashCode(): Int = Objects.hash(assocTypeSymUse, arg)
  }

  /**
    * A fully resolved error type.
    */
  case class Error(loc: SourceLocation) extends UnkindedType {
    override def equals(that: Any): Boolean = that match {
      case Error(_) => true
      case _ => false
    }

    override def hashCode(): Int = 17
  }

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(loc: SourceLocation, text: VarText = VarText.Absent)(implicit scope: Scope, flix: Flix): UnkindedType.Var = {
    val sym = Symbol.freshUnkindedTypeVarSym(text, loc)
    UnkindedType.Var(sym, loc)
  }

  /**
    * Returns the Int32 type.
    */
  def mkInt32(loc: SourceLocation): UnkindedType = {
    UnkindedType.Cst(TypeConstructor.Int32, loc)
  }

  /**
    * Returns the Int64 type.
    */
  def mkInt64(loc: SourceLocation): UnkindedType = {
    UnkindedType.Cst(TypeConstructor.Int64, loc)
  }

  /**
    * Returns the Float64 type.
    */
  def mkFloat64(loc: SourceLocation): UnkindedType = {
    UnkindedType.Cst(TypeConstructor.Float64, loc)
  }

  /**
   * Returns the Bool type.
   */
  def mkBool(loc: SourceLocation): UnkindedType = {
    UnkindedType.Cst(TypeConstructor.Bool, loc)
  }

  /**
   * Returns the Unit type.
   */
  def mkUnit(loc: SourceLocation): UnkindedType = {
    UnkindedType.Cst(TypeConstructor.Unit, loc)
  }

  /**
    * Returns the ##java.lang.Object type.
    */
  def mkObject(loc: SourceLocation): UnkindedType = {
    val obj = Class.forName("java.lang.Object")
    UnkindedType.Cst(TypeConstructor.Native(obj), loc)
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: UnkindedType, ts: List[UnkindedType], loc: SourceLocation): UnkindedType = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t, loc)
  }

  /**
    * Constructs the type a -> b \ IO
    */
  def mkIoArrow(a: UnkindedType, b: UnkindedType, loc: SourceLocation): UnkindedType = {
    val eff = Some(UnkindedType.Cst(TypeConstructor.Effect(Symbol.IO), loc))
    mkApply(UnkindedType.Arrow(eff, 2, loc), List(a, b), loc)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val init = UnkindedType.Cst(TypeConstructor.Tuple(ts.length), loc)
    ts.foldLeft(init: UnkindedType) {
      case (acc, x) => Apply(acc, x, loc)
    }
  }

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordRowExtend(label: Name.Label, tpe: UnkindedType, rest: UnkindedType, loc: SourceLocation): UnkindedType = {
    mkApply(UnkindedType.Cst(TypeConstructor.RecordRowExtend(label), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaRowExtend(pred: Name.Pred, tpe: UnkindedType, rest: UnkindedType, loc: SourceLocation): UnkindedType = {
    mkApply(UnkindedType.Cst(TypeConstructor.SchemaRowExtend(pred), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a Record type.
    */
  def mkRecord(tpe: UnkindedType, loc: SourceLocation): UnkindedType = {
    Apply(UnkindedType.Cst(TypeConstructor.Record, loc), tpe, loc)
  }

  /**
    * Constructs a Schema type.
    */
  def mkSchema(tpe: UnkindedType, loc: SourceLocation): UnkindedType = {
    Apply(UnkindedType.Cst(TypeConstructor.Schema, loc), tpe, loc)
  }


  /**
    * Construct a relation type with the given list of type arguments `ts0`.
    */
  def mkRelation(ts0: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val ts = ts0 match {
      case Nil => UnkindedType.Cst(TypeConstructor.Unit, loc)
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(UnkindedType.Cst(TypeConstructor.Relation, loc), ts, loc)
  }

  /**
    * Construct a lattice type with the given list of type arguments `ts0`.
    */
  def mkLattice(ts0: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val ts = ts0 match {
      case Nil => UnkindedType.Cst(TypeConstructor.Unit, loc)
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(UnkindedType.Cst(TypeConstructor.Lattice, loc), ts, loc)
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, loc: SourceLocation): UnkindedType = UnkindedType.Enum(sym, loc)

  /**
    * Construct the struct type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkStruct(sym: Symbol.StructSym, loc: SourceLocation): UnkindedType = UnkindedType.Struct(sym, loc)

  /**
    * Construct the restrictable enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkRestrictableEnum(sym: Symbol.RestrictableEnumSym, loc: SourceLocation): UnkindedType = UnkindedType.RestrictableEnum(sym, loc)

  /**
    * Construct the effect type for the given symbol.
    */
  def mkEffect(sym: Symbol.EffectSym, loc: SourceLocation): UnkindedType = UnkindedType.Cst(TypeConstructor.Effect(sym), loc)

  /**
    * Constructs a predicate type.
    */
  def mkPredicate(den: Denotation, ts0: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val tycon = den match {
      case Denotation.Relational => UnkindedType.Cst(TypeConstructor.Relation, loc)
      case Denotation.Latticenal => UnkindedType.Cst(TypeConstructor.Lattice, loc)
    }
    val ts = ts0 match {
      case Nil => UnkindedType.Cst(TypeConstructor.Unit, loc)
      case x :: Nil => x
      case xs => UnkindedType.mkTuple(xs, loc)
    }

    UnkindedType.Apply(tycon, ts, loc)
  }

  /**
    * Returns the type `Not(tpe1)`.
    */
  def mkNot(tpe1: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Not, loc), List(tpe1), loc)

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.And, loc), List(tpe1, tpe2), loc)

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Or, loc), List(tpe1, tpe2), loc)

  /**
    * Returns the type `Complement(tpe1)`.
    */
  def mkComplement(tpe1: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Complement, loc), List(tpe1), loc)

  /**
    * Returns the type `Union(tpe1, tpe2)`.
    */
  def mkUnion(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Union, loc), List(tpe1, tpe2), loc)

  /**
    * Returns the type `Intersection(tpe1, tpe2)`.
    */
  def mkIntersection(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Intersection, loc), List(tpe1, tpe2), loc)

  /**
    * Returns the type `Difference(tpe1, tpe2)`.
    */
  def mkDifference(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation): UnkindedType = UnkindedType.mkApply(UnkindedType.Cst(TypeConstructor.Difference, loc), List(tpe1, tpe2), loc)

  /**
    * Constructs the uncurried arrow type (A_1, ..., A_n) -> B \ e.
    */
  def mkUncurriedArrowWithEffect(as: List[UnkindedType], e: Option[UnkindedType], b: UnkindedType, loc: SourceLocation): UnkindedType = {
    val arrow = UnkindedType.Arrow(e, as.length + 1, loc)
    val inner = as.foldLeft(arrow: UnkindedType) {
      case (acc, x) => UnkindedType.Apply(acc, x, loc)
    }
    UnkindedType.Apply(inner, b, loc)
  }

  /**
    * Erases all the aliases from the type.
    */
  def eraseAliases(tpe0: UnkindedType): UnkindedType = tpe0 match {
    case tpe: Var => tpe
    case tpe: Cst => tpe
    case tpe: Enum => tpe
    case tpe: Struct => tpe
    case tpe: RestrictableEnum => tpe
    case tpe: UnkindedType.CaseSet => tpe
    case Apply(tpe1, tpe2, loc) => Apply(eraseAliases(tpe1), eraseAliases(tpe2), loc)
    case Arrow(eff, arity, loc) => Arrow(eff.map(eraseAliases), arity, loc)
    case UnkindedType.CaseComplement(tpe, loc) => UnkindedType.CaseComplement(eraseAliases(tpe), loc)
    case UnkindedType.CaseUnion(tpe1, tpe2, loc) => UnkindedType.CaseUnion(eraseAliases(tpe1), eraseAliases(tpe2), loc)
    case UnkindedType.CaseIntersection(tpe1, tpe2, loc) => UnkindedType.CaseIntersection(eraseAliases(tpe1), eraseAliases(tpe2), loc)
    case Ascribe(tpe, kind, loc) => Ascribe(eraseAliases(tpe), kind, loc)
    case Alias(_, _, tpe, _) => eraseAliases(tpe)
    case AssocType(cst, arg, loc) => AssocType(cst, eraseAliases(arg), loc) // TODO ASSOC-TYPES check that this is valid
    case GetEff(action, tpe, loc) => GetEff(action, eraseAliases(tpe), loc)
    case tpe: UnkindedType.Error => tpe
    case UnappliedAlias(_, loc) => throw InternalCompilerException("unexpected unapplied alias", loc)
    case UnappliedAssocType(_, loc) => throw InternalCompilerException("unexpected unapplied associated type", loc)
    case UnappliedGetEff(_, loc) => throw InternalCompilerException("unexpected unapplied GetEff", loc)
  }

  // TODO remove once typechecking Resolver.lookupJVMMethod is moved to Typer

  /**
    * Returns the Flix UnkindedType of a Java Class
    */
  def getFlixType(c: Class[?]): UnkindedType = {
    if (c == java.lang.Boolean.TYPE) {
      UnkindedType.Cst(TypeConstructor.Bool, SourceLocation.Unknown)
    }
    else if (c == java.lang.Byte.TYPE) {
      UnkindedType.Cst(TypeConstructor.Int8, SourceLocation.Unknown)
    }
    else if (c == java.lang.Short.TYPE) {
      UnkindedType.Cst(TypeConstructor.Int16, SourceLocation.Unknown)
    }
    else if (c == java.lang.Integer.TYPE) {
      UnkindedType.Cst(TypeConstructor.Int32, SourceLocation.Unknown)
    }
    else if (c == java.lang.Long.TYPE) {
      UnkindedType.Cst(TypeConstructor.Int64, SourceLocation.Unknown)
    }
    else if (c == java.lang.Character.TYPE) {
      UnkindedType.Cst(TypeConstructor.Char, SourceLocation.Unknown)
    }
    else if (c == java.lang.Float.TYPE) {
      UnkindedType.Cst(TypeConstructor.Float32, SourceLocation.Unknown)
    }
    else if (c == java.lang.Double.TYPE) {
      UnkindedType.Cst(TypeConstructor.Float64, SourceLocation.Unknown)
    }
    else if (c == classOf[java.math.BigDecimal]) {
      UnkindedType.Cst(TypeConstructor.BigDecimal, SourceLocation.Unknown)
    }
    else if (c == classOf[java.math.BigInteger]) {
      UnkindedType.Cst(TypeConstructor.BigInt, SourceLocation.Unknown)
    }
    else if (c == classOf[java.lang.String]) {
      UnkindedType.Cst(TypeConstructor.Str, SourceLocation.Unknown)
    }
    else if (c == classOf[java.util.regex.Pattern]) {
      UnkindedType.Cst(TypeConstructor.Regex, SourceLocation.Unknown)
    }
    else if (c == java.lang.Void.TYPE) {
      UnkindedType.Cst(TypeConstructor.Unit, SourceLocation.Unknown)
    }
    // handle arrays of types
    else if (c.isArray) {
      val comp = c.getComponentType
      val elmType = getFlixType(comp)
      UnkindedType.mkApply(
        UnkindedType.Cst(TypeConstructor.Array, SourceLocation.Unknown),
        List(elmType, UnkindedType.Cst(TypeConstructor.Effect(Symbol.IO), SourceLocation.Unknown)),
        SourceLocation.Unknown
      )
    }
    // otherwise native type
    else {
      UnkindedType.Cst(TypeConstructor.Native(c), SourceLocation.Unknown)
    }
  }
}
