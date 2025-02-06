/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.TypeConstructor
import ca.uwaterloo.flix.language.dbg.DocAst

object TypeConstructorPrinter {

  /**
    * Returns the [[DocAst.Type]] representation of `tc`.
    *
    * Note that using this function naively will result in types like
    * `Tuple(2)[Char, Char]` instead of `(Char, Char)`.
    */
  def print(tc: TypeConstructor): DocAst.Type = tc match {
    case TypeConstructor.Void => DocAst.Type.Void
    case TypeConstructor.AnyType => DocAst.Type.AnyType
    case TypeConstructor.Unit => DocAst.Type.Unit
    case TypeConstructor.Null => DocAst.Type.Null
    case TypeConstructor.Bool => DocAst.Type.Bool
    case TypeConstructor.Char => DocAst.Type.Char
    case TypeConstructor.Float32 => DocAst.Type.Float32
    case TypeConstructor.Float64 => DocAst.Type.Float64
    case TypeConstructor.BigDecimal => DocAst.Type.BigDecimal
    case TypeConstructor.Int8 => DocAst.Type.Int8
    case TypeConstructor.Int16 => DocAst.Type.Int16
    case TypeConstructor.Int32 => DocAst.Type.Int32
    case TypeConstructor.Int64 => DocAst.Type.Int64
    case TypeConstructor.BigInt => DocAst.Type.BigInt
    case TypeConstructor.Str => DocAst.Type.Str
    case TypeConstructor.Regex => DocAst.Type.Regex
    case TypeConstructor.Arrow(arity) => DocAst.Type.AsIs(s"Arrow($arity)")
    case TypeConstructor.ArrowWithoutEffect(arity) => DocAst.Type.AsIs(s"ArrowWithoutEffect($arity)")
    case TypeConstructor.RecordRowEmpty => DocAst.Type.RecordRowEmpty
    case TypeConstructor.RecordRowExtend(label) => DocAst.Type.AsIs(s"RecordRowExtend($label)")
    case TypeConstructor.Record => DocAst.Type.AsIs("Record")
    case TypeConstructor.SchemaRowEmpty => DocAst.Type.SchemaRowEmpty
    case TypeConstructor.SchemaRowExtend(pred) => DocAst.Type.AsIs(s"SchemaRowExtend($pred)")
    case TypeConstructor.Schema => DocAst.Type.Schema
    case TypeConstructor.Sender => DocAst.Type.AsIs("Sender")
    case TypeConstructor.Receiver => DocAst.Type.AsIs("Receiver")
    case TypeConstructor.Lazy => DocAst.Type.AsIs("Lazy")
    case TypeConstructor.Enum(sym, _) => DocAst.Type.AsIs(sym.toString)
    case TypeConstructor.Struct(sym, _) => DocAst.Type.AsIs(sym.toString)
    case TypeConstructor.RestrictableEnum(sym, _) => DocAst.Type.AsIs(sym.toString)
    case TypeConstructor.Native(clazz) => DocAst.Type.Native(clazz)
    case TypeConstructor.JvmConstructor(constructor) => DocAst.Type.JvmConstructor(constructor)
    case TypeConstructor.JvmMethod(method) => DocAst.Type.JvmMethod(method)
    case TypeConstructor.JvmField(field) => DocAst.Type.JvmField(field)
    case TypeConstructor.Array => DocAst.Type.AsIs("Array")
    case TypeConstructor.ArrayWithoutRegion => DocAst.Type.AsIs("ArrayWithoutRegion")
    case TypeConstructor.Vector => DocAst.Type.AsIs("Vector")
    case TypeConstructor.Tuple(l) => DocAst.Type.AsIs(s"Tuple($l)")
    case TypeConstructor.Relation => DocAst.Type.AsIs("Relation")
    case TypeConstructor.Lattice => DocAst.Type.AsIs("Lattice")
    case TypeConstructor.True => DocAst.Type.AsIs("True")
    case TypeConstructor.False => DocAst.Type.AsIs("False")
    case TypeConstructor.Not => DocAst.Type.AsIs("Not")
    case TypeConstructor.And => DocAst.Type.AsIs("And")
    case TypeConstructor.Or => DocAst.Type.AsIs("Or")
    case TypeConstructor.Pure => DocAst.Type.Pure
    case TypeConstructor.Univ => DocAst.Type.Univ
    case TypeConstructor.Complement => DocAst.Type.AsIs("Complement")
    case TypeConstructor.Union => DocAst.Type.AsIs("Union")
    case TypeConstructor.Intersection => DocAst.Type.AsIs("Intersection")
    case TypeConstructor.Difference => DocAst.Type.AsIs("Difference")
    case TypeConstructor.SymmetricDiff => DocAst.Type.AsIs("SymmetricDiff")
    case TypeConstructor.Effect(sym) => DocAst.Type.AsIs(sym.toString)
    case TypeConstructor.CaseComplement(_) => DocAst.Type.AsIs("CaseComplement")
    case TypeConstructor.CaseUnion(_) => DocAst.Type.AsIs("CaseUnion")
    case TypeConstructor.CaseIntersection(_) => DocAst.Type.AsIs("CaseIntersection")
    case TypeConstructor.CaseSet(syms, _) => DocAst.Type.CaseSet(syms)
    case TypeConstructor.Region(sym) => DocAst.Type.AsIs(sym.toString)
    case TypeConstructor.RegionToStar => DocAst.Type.AsIs("Region")
    case TypeConstructor.RegionToEff => DocAst.Type.AsIs("Heap")
    case TypeConstructor.RegionWithoutRegion => DocAst.Type.AsIs("RegionWithoutRegion")
    case TypeConstructor.GenericRegion(prop) => DocAst.Type.AsIs("GenericRegion(" + prop + ")")
    case TypeConstructor.Error(_, _) => DocAst.Type.Error
  }

}
