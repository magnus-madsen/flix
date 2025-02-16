/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock.serialization

import ca.uwaterloo.flix.language.ast.shared.{Scope, SymUse, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.native.Serialization.{read, write}
import org.json4s.Formats

object Serialization {

  type Library = String
  type NamedTypeScheme = (Symbol.DefnSym, Scheme)
  type NamedTypeSchemes = Map[Symbol.DefnSym, Scheme]

  /**
    * Type hints for JSON library to (de)serialize ADTs.
    * If an error occurs, it is probably because the [[TypeHints.formats]]
    * list has not been updated.
    */
  private implicit val formats: Formats = TypeHints.formats

  def serialize(libraryDefs: Map[Library, List[TypedAst.Def]]): String = {
    write(fromLibs(libraryDefs))
  }

  def serialize(tpe: Type): String = {
    write(fromType(tpe))
  }

  def deserialize(json: String): Option[Map[Library, NamedTypeSchemes]] = {
    try {
      val deser = read[Map[Library, List[SDef]]](json)
      Some(toLibs(deser))
    }
    catch {
      case _: Exception => None // TODO: Include error by returning Validation or Result
    }
  }

  def deserializeTpe(tpe: String): Option[Type] = {
    // Toggle error handling
    if (true) {
      val deser = read[SType](tpe)
      Some(toType(deser))
    } else {
      try {
        val deser = read[SType](tpe)
        Some(toType(deser))
      }
      catch {
        case _: Exception => None
      }
    }
  }


  private def fromLibs(libs: Map[Library, List[TypedAst.Def]]): Map[Library, List[SDef]] = {
    libs.map {
      case (l, defs) => l -> defs.map(fromDef)
    }
  }

  private def toLibs(libs: Map[Library, List[SDef]]): Map[Library, NamedTypeSchemes] = {
    libs.map {
      case (l, defs) => l -> defs.map(toNamedTypeScheme).toMap
    }
  }

  private def fromDef(defn0: TypedAst.Def): SDef = defn0 match {
    case TypedAst.Def(sym, spec, _, _) =>
      val ns = sym.namespace
      val text = sym.text
      val sc = fromScheme(spec.declaredScheme)
      SDef(ns, text, sc)
  }

  private def toNamedTypeScheme(defn0: SDef): NamedTypeScheme = defn0 match {
    case SDef(namespace, text, scheme) =>
      val sym = Symbol.mkDefnSym(namespace.mkString("", ".", ".").appendedAll(text))
      sym -> toScheme(scheme)
  }

  private def fromScheme(scheme: Scheme): SScheme = scheme match {
    case Scheme(quantifiers, _, _, base) =>
      val qs = quantifiers.map(fromKindedTypeVarSym)
      val b = fromType(base)
      SScheme(qs, b)
  }

  private def toScheme(scheme: SScheme): Scheme = scheme match {
    case SScheme(quantifiers, base) =>
      val qs = quantifiers.map(toKindedTypeVarSym)
      val b = toType(base)
      Scheme(qs, List.empty, List.empty, b)
  }

  private def fromKindedTypeVarSym(sym: Symbol.KindedTypeVarSym): SSymbol.VarSym = {
    SSymbol.VarSym(sym.id, fromVarText(sym.text), fromKind(sym.kind))
  }

  private def toKindedTypeVarSym(sym: SSymbol.VarSym): Symbol.KindedTypeVarSym = {
    new Symbol.KindedTypeVarSym(sym.id, toVarText(sym.text), toKind(sym.kind), isSlack = false, scope = Scope.Top, loc = SourceLocation.Unknown)
  }

  private def fromType(tpe: Type): SType = tpe match {
    case Type.Var(sym, _) =>
      val serSym = SSymbol.VarSym(sym.id, fromVarText(sym.text), fromKind(sym.kind))
      SType.Var(serSym)

    case Type.Cst(tc, _) =>
      val serTC = fromTypeConstructor(tc)
      SType.Cst(serTC)

    case Type.Apply(tpe1, tpe2, _) =>
      val serT1 = fromType(tpe1)
      val serT2 = fromType(tpe2)
      SType.Apply(serT1, serT2)

    case Type.Alias(SymUse.TypeAliasSymUse(sym, _), args, tpe, _) =>
      val serSym = SSymbol.TypeAliasSym(sym.namespace, sym.name)
      val serTs = args.map(fromType)
      val serT = fromType(tpe)
      SType.Alias(serSym, serTs, serT)

    case Type.AssocType(SymUse.AssocTypeSymUse(sym, _), arg, kind, _) =>
      val serTrtSym = SSymbol.TraitSym(sym.trt.namespace, sym.trt.name)
      val serSym = SSymbol.AssocTypeSym(serTrtSym, sym.name)
      val serT = fromType(arg)
      val serKind = fromKind(kind)
      SType.AssocType(serSym, serT, serKind)

    case Type.JvmToType(_, _) | Type.JvmToEff(_, _) | Type.UnresolvedJvmType(_, _) => throw InternalCompilerException("unexpected jvm type", tpe.loc)

  }

  private def fromTypeConstructor(tc0: TypeConstructor): STC = tc0 match {
    case TypeConstructor.Void => STC.Void
    case TypeConstructor.AnyType => STC.AnyType
    case TypeConstructor.Unit => STC.Unit
    case TypeConstructor.Null => STC.Null
    case TypeConstructor.Bool => STC.Bool
    case TypeConstructor.Char => STC.Char
    case TypeConstructor.Float32 => STC.Float32
    case TypeConstructor.Float64 => STC.Float64
    case TypeConstructor.BigDecimal => STC.BigDecimal
    case TypeConstructor.Int8 => STC.Int8
    case TypeConstructor.Int16 => STC.Int16
    case TypeConstructor.Int32 => STC.Int32
    case TypeConstructor.Int64 => STC.Int64
    case TypeConstructor.BigInt => STC.BigInt
    case TypeConstructor.Str => STC.Str
    case TypeConstructor.Regex => STC.Regex
    case TypeConstructor.Arrow(arity) => STC.Arrow(arity)
    case TypeConstructor.ArrowWithoutEffect(arity) => STC.ArrowWithoutEffect(arity)
    case TypeConstructor.RecordRowEmpty => STC.RecordRowEmpty
    case TypeConstructor.RecordRowExtend(label) => ??? // SerializableTypeConstructor.RecordRowExtend(label)
    case TypeConstructor.Record => STC.Record
    case TypeConstructor.SchemaRowEmpty => STC.SchemaRowEmpty
    case TypeConstructor.SchemaRowExtend(pred) => ??? // SerializableTypeConstructor.SchemaRowExtend(pred)
    case TypeConstructor.Schema => STC.Schema
    case TypeConstructor.Sender => STC.Sender
    case TypeConstructor.Receiver => STC.Receiver
    case TypeConstructor.Lazy => STC.Lazy
    case TypeConstructor.Enum(sym, kind) =>
      val serSym = SSymbol.EnumSym(sym.namespace, sym.text)
      val serKind = fromKind(kind)
      STC.Enum(serSym, serKind)
    case TypeConstructor.Struct(sym, kind) => ??? // SerializableTypeConstructor.Struct(sym, kind)
    case TypeConstructor.RestrictableEnum(sym, kind) => ??? // SerializableTypeConstructor.RestrictableEnum(sym, kind)
    case TypeConstructor.Native(clazz) => ??? // SerializableTypeConstructor.Native(clazz)
    case TypeConstructor.JvmConstructor(constructor) => ??? // SerializableTypeConstructor.JvmConstructor(constructor)
    case TypeConstructor.JvmMethod(method) => ??? // SerializableTypeConstructor.JvmMethod(method)
    case TypeConstructor.JvmField(field) => ??? // SerializableTypeConstructor.JvmField(field)
    case TypeConstructor.Array => STC.Array
    case TypeConstructor.ArrayWithoutRegion => ??? // SerializableTypeConstructor.ArrayWithoutRegion
    case TypeConstructor.Vector => STC.Vector
    case TypeConstructor.Tuple(l) => STC.Tuple(l)
    case TypeConstructor.Relation => STC.Relation
    case TypeConstructor.Lattice => STC.Lattice
    case TypeConstructor.True => STC.True
    case TypeConstructor.False => STC.False
    case TypeConstructor.Not => STC.Not
    case TypeConstructor.And => STC.And
    case TypeConstructor.Or => STC.Or
    case TypeConstructor.Pure => STC.Pure
    case TypeConstructor.Univ => STC.Univ
    case TypeConstructor.Complement => STC.Complement
    case TypeConstructor.Union => STC.Union
    case TypeConstructor.Intersection => STC.Intersection
    case TypeConstructor.Difference => STC.Difference
    case TypeConstructor.SymmetricDiff => STC.SymmetricDiff
    case TypeConstructor.Effect(sym) =>
      val ssym = SSymbol.EffectSym(sym.namespace, sym.name)
      STC.Effect(ssym)
    case TypeConstructor.CaseComplement(sym) => ??? // SerializableTypeConstructor.CaseComplement(sym)
    case TypeConstructor.CaseUnion(sym) => ??? // SerializableTypeConstructor.CaseUnion(sym)
    case TypeConstructor.CaseIntersection(sym) => ??? // SerializableTypeConstructor.CaseIntersection(sym)
    case TypeConstructor.CaseSet(syms, enumSym) => ??? // SerializableTypeConstructor.CaseSet(syms, enumSym)
    case TypeConstructor.Region(_) => ??? // STC.Region(sym)
    case TypeConstructor.RegionToStar => STC.RegionToStar
    case TypeConstructor.RegionWithoutRegion => ??? // SerializableTypeConstructor.RegionWithoutRegion
    case TypeConstructor.Error(id, kind) => ??? // SerializableTypeConstructor.Error(id, kind)
  }

  private def fromVarText(text: VarText): SVT = text match {
    case VarText.Absent => SVT.Absent
    case VarText.SourceText(s) => SVT.SourceText(s)
  }

  private def toVarText(text: SVT): VarText = text match {
    case SVT.Absent => VarText.Absent
    case SVT.SourceText(s) => VarText.SourceText(s)
  }

  private def fromKind(kind0: Kind): SKind = kind0 match {
    case Kind.Wild => SKind.Wild
    case Kind.WildCaseSet => SKind.WildCaseSet
    case Kind.Star => SKind.Star
    case Kind.Eff => SKind.Eff
    case Kind.Bool => SKind.Bool
    case Kind.RecordRow => SKind.RecordRow
    case Kind.SchemaRow => SKind.SchemaRow
    case Kind.Predicate => SKind.Predicate
    case Kind.Jvm => SKind.Jvm
    case Kind.CaseSet(sym) => ???
    case Kind.Arrow(k1, k2) => SKind.Arrow(fromKind(k1), fromKind(k2))
    case Kind.Error => ???
  }

  private def toKind(kind0: SKind): Kind = kind0 match {
    case SKind.Wild => Kind.Wild
    case SKind.WildCaseSet => Kind.WildCaseSet
    case SKind.Star => Kind.Star
    case SKind.Eff => Kind.Eff
    case SKind.Bool => Kind.Bool
    case SKind.RecordRow => Kind.RecordRow
    case SKind.SchemaRow => Kind.SchemaRow
    case SKind.Predicate => Kind.Predicate
    case SKind.Jvm => Kind.Jvm
    case SKind.Arrow(k1, k2) => Kind.Arrow(toKind(k1), toKind(k2))
  }

  private def toType(tpe: SType): Type = tpe match {
    case SType.Var(sym) =>
      Type.Var(toKindedTypeVarSym(sym), SourceLocation.Unknown)

    case SType.Cst(tc) =>
      Type.Cst(toTypeConstructor(tc), SourceLocation.Unknown)

    case SType.Apply(tpe1, tpe2) =>
      Type.Apply(toType(tpe1), toType(tpe2), SourceLocation.Unknown)

    case SType.Alias(symUse, args, tpe) =>
      val sym = new Symbol.TypeAliasSym(symUse.namespace, symUse.name, SourceLocation.Unknown)
      val su = SymUse.TypeAliasSymUse(sym, SourceLocation.Unknown)
      val as = args.map(toType)
      val t = toType(tpe)
      Type.Alias(su, as, t, SourceLocation.Unknown)

    case SType.AssocType(symUse, arg, kind) =>
      val tsym = new Symbol.TraitSym(symUse.trt.namespace, symUse.trt.name, SourceLocation.Unknown)
      val sym = new Symbol.AssocTypeSym(tsym, symUse.name, SourceLocation.Unknown)
      val su = SymUse.AssocTypeSymUse(sym, SourceLocation.Unknown)
      val a = toType(arg)
      val k = toKind(kind)
      Type.AssocType(su, a, k, SourceLocation.Unknown)
  }

  private def toTypeConstructor(tc0: STC): TypeConstructor = tc0 match {
    case STC.Void => TypeConstructor.Void
    case STC.AnyType => TypeConstructor.AnyType
    case STC.Unit => TypeConstructor.Unit
    case STC.Null => TypeConstructor.Null
    case STC.Bool => TypeConstructor.Bool
    case STC.Char => TypeConstructor.Char
    case STC.Float32 => TypeConstructor.Float32
    case STC.Float64 => TypeConstructor.Float64
    case STC.BigDecimal => TypeConstructor.BigDecimal
    case STC.Int8 => TypeConstructor.Int8
    case STC.Int16 => TypeConstructor.Int16
    case STC.Int32 => TypeConstructor.Int32
    case STC.Int64 => TypeConstructor.Int64
    case STC.BigInt => TypeConstructor.BigInt
    case STC.Str => TypeConstructor.Str
    case STC.Regex => TypeConstructor.Regex
    case STC.Arrow(arity) => TypeConstructor.Arrow(arity)
    case STC.ArrowWithoutEffect(arity) => TypeConstructor.ArrowWithoutEffect(arity)
    case STC.RecordRowEmpty => TypeConstructor.RecordRowEmpty
    case STC.Record => TypeConstructor.Record
    case STC.SchemaRowEmpty => TypeConstructor.SchemaRowEmpty
    case STC.Schema => TypeConstructor.Schema
    case STC.Sender => TypeConstructor.Sender
    case STC.Receiver => TypeConstructor.Receiver
    case STC.Lazy => TypeConstructor.Lazy
    case STC.Enum(sym, kind) =>
      val s = new Symbol.EnumSym(sym.namespace, sym.text, SourceLocation.Unknown)
      val k = toKind(kind)
      TypeConstructor.Enum(s, k)
    case STC.Array => TypeConstructor.Array
    case STC.Vector => TypeConstructor.Vector
    case STC.Tuple(l) => TypeConstructor.Tuple(l)
    case STC.Relation => TypeConstructor.Relation
    case STC.Lattice => TypeConstructor.Lattice
    case STC.True => TypeConstructor.True
    case STC.False => TypeConstructor.False
    case STC.Not => TypeConstructor.Not
    case STC.And => TypeConstructor.And
    case STC.Or => TypeConstructor.Or
    case STC.Pure => TypeConstructor.Pure
    case STC.Univ => TypeConstructor.Univ
    case STC.Complement => TypeConstructor.Complement
    case STC.Union => TypeConstructor.Union
    case STC.Intersection => TypeConstructor.Intersection
    case STC.Difference => TypeConstructor.Difference
    case STC.SymmetricDiff => TypeConstructor.SymmetricDiff
    case STC.Effect(ssym) =>
      val s = new Symbol.EffectSym(ssym.namespace, ssym.name, SourceLocation.Unknown)
      TypeConstructor.Effect(s)
    case STC.RegionToStar => TypeConstructor.RegionToStar
  }
}
