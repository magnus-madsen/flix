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

import org.json4s.{Formats, ShortTypeHints}

object TypeHints {

  /**
    * List of type hints for serialization library.
    * Each constructor is registered here, so the library can serialize the name
    * of the constructor and its fields via reflection.
    */
  val formats: Formats = org.json4s.native.Serialization.formats(ShortTypeHints(List(
    // Types
    classOf[SType.Var],
    classOf[SType.Cst],
    classOf[SType.Apply],
    classOf[SType.Alias],
    classOf[SType.AssocType],

    // TypeConstructors
    STC.Void.getClass,
    STC.AnyType.getClass,
    STC.Unit.getClass,
    STC.Null.getClass,
    STC.Bool.getClass,
    STC.Char.getClass,
    STC.Float32.getClass,
    STC.Float64.getClass,
    STC.BigDecimal.getClass,
    STC.Int8.getClass,
    STC.Int16.getClass,
    STC.Int32.getClass,
    STC.Int64.getClass,
    STC.BigInt.getClass,
    STC.Str.getClass,
    STC.Regex.getClass,
    classOf[STC.Arrow],
    classOf[STC.ArrowWithoutEffect],
    STC.RecordRowEmpty.getClass,
    // classOf[SerializableTypeConstructor.RecordRowExtend],
    STC.Record.getClass,
    STC.SchemaRowEmpty.getClass,
    // classOf[SerializableTypeConstructor.SchemaRowExtend],
    STC.Schema.getClass,
    STC.Sender.getClass,
    STC.Receiver.getClass,
    STC.Lazy.getClass,
    classOf[STC.Enum],
    // classOf[SerializableTypeConstructor.Struct],
    // classOf[SerializableTypeConstructor.RestrictableEnum],
    // classOf[SerializableTypeConstructor.Native],
    // classOf[SerializableTypeConstructor.JvmConstructor],
    // classOf[SerializableTypeConstructor.JvmMethod],
    // classOf[SerializableTypeConstructor.JvmField],
    STC.Array.getClass,
    STC.Vector.getClass,
    classOf[STC.Tuple],
    STC.Relation.getClass,
    STC.Lattice.getClass,
    STC.True.getClass,
    STC.False.getClass,
    STC.Not.getClass,
    STC.And.getClass,
    STC.Or.getClass,
    STC.Pure.getClass,
    STC.Univ.getClass,
    STC.Complement.getClass,
    STC.Union.getClass,
    STC.Intersection.getClass,
    STC.Difference.getClass,
    STC.SymmetricDiff.getClass,
    classOf[STC.Effect],
    // classOf[SerializableTypeConstructor.CaseComplement],
    // classOf[SerializableTypeConstructor.CaseUnion],
    // classOf[SerializableTypeConstructor.CaseIntersection],
    // classOf[SerializableTypeConstructor.CaseSet],
    STC.RegionToStar.getClass,

    // Kinds
    SKind.Wild.getClass,
    SKind.WildCaseSet.getClass,
    SKind.Star.getClass,
    SKind.Eff.getClass,
    SKind.Bool.getClass,
    SKind.RecordRow.getClass,
    SKind.SchemaRow.getClass,
    SKind.Predicate.getClass,
    SKind.Jvm.getClass,
    // classOf[SerializableKind.CaseSet],
    classOf[SKind.Arrow],

    // Symbols
    classOf[SSymbol.VarSym],
    classOf[SSymbol.TypeAliasSym],
    classOf[SSymbol.AssocTypeSym],
    classOf[SSymbol.TraitSym],

    // VarText
    SVT.Absent.getClass,
    classOf[SVT.SourceText],
  )))
}
