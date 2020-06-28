/*
 * Copyright 2020 Matthew Lutze
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.vt.{TerminalContext, VirtualString}
import org.scalatest.FunSuite

class TestFormatType extends FunSuite with TestUtils {
  test("FormatType.WellFormedType.Record.External.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: String }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.External.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | theRest }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    paramType.setText("t1")
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType)

    val expected = "t1 -> t1"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Effect, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType)

    val expected = "a -> b & c"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.03") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, Type.Impure, returnType)

    val expected = "a ~> b"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.04") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3, Type.Impure)), List(Type.Int8, Type.Int16, Type.Int32))

    val expected = "Int8 -> Int16 ~> Int32"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.External.05") {
    val effType = Type.mkApply(Type.Cst(TypeConstructor.And), List(Type.Pure, Type.Impure))
    val tpe = Type.mkArrowWithEffect(Type.BigInt, effType, Type.Bool)

    val expected = "BigInt -> Bool & ((Pure) ∧ (Impure))"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.01") {
    val tupleType = Type.mkTuple(List(Type.Int32, Type.Str))
    val relationType = Type.mkApply(Type.Cst(TypeConstructor.Relation), List(tupleType))
    val tpe = Type.mkSchemaExtend("S", relationType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.External.02") {
    val tupleType1 = Type.Str
    val tupleType2 = Type.mkTuple(List(Type.Int32, Type.Str))
    val latticeType1 = Type.Apply(Type.Cst(TypeConstructor.Lattice), tupleType1)
    val latticeType2 = Type.Apply(Type.Cst(TypeConstructor.Lattice), tupleType2)
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    restType.setText("theRest")
    val tpe = Type.mkSchemaExtend("A", latticeType1, Type.mkSchemaExtend("B", latticeType2, restType))

    val expected = "#{ A<>(String), B<>(Int32, String) | theRest }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.External.07") {
    val enumConstructor = TypeConstructor.Enum(Symbol.mkEnumSym("Triplet"), Kind.Star ->: Kind.Star ->: Kind.Star ->: Kind.Star)
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    tvar1.setText("a")
    tvar2.setText("b")
    tvar3.setText("c")
    val tpe = Type.mkApply(Type.Cst(enumConstructor), List(tvar1, tvar2, tvar3))

    val expected = "Triplet[a, b, c]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatType.WellFormedType.Record.Internal.01") {
    val tpe = Type.mkRecordExtend("x", Type.Int32, Type.mkRecordExtend("y", Type.Str, Type.RecordEmpty))

    val expected = "{ x: Int32, y: String }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Record.Internal.02") {
    val rest = Type.Var(0, Kind.Record, Rigidity.Rigid)
    rest.setText("theRest")
    val tpe = Type.mkRecordExtend("x", Type.Int32, rest)

    val expected = "{ x: Int32 | '0 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.01") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    paramType.setText("t1")
    val tpe = Type.mkArrowWithEffect(paramType, Type.Pure, paramType)

    val expected = "'0 -> '0"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Arrow.Internal.02") {
    val paramType = Type.Var(0, Kind.Star, Rigidity.Rigid)
    val returnType = Type.Var(1, Kind.Star, Rigidity.Rigid)
    val effectType = Type.Var(2, Kind.Effect, Rigidity.Rigid)
    val tpe = Type.mkArrowWithEffect(paramType, effectType, returnType)

    val expected = "'0 -> '1 & ''2"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.01") {
    val tupleType = Type.mkTuple(List(Type.Int32, Type.Str))
    val relationType = Type.Apply(Type.Cst(TypeConstructor.Relation), tupleType)
    val tpe = Type.mkSchemaExtend("S", relationType, Type.SchemaEmpty)

    val expected = "#{ S(Int32, String) }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Schema.Internal.02") {
    val tupleType1 = Type.Str
    val tupleType2 = Type.mkTuple(List(Type.Int32, Type.Str))
    val latticeType1 = Type.Apply(Type.Cst(TypeConstructor.Lattice), tupleType1)
    val latticeType2 = Type.Apply(Type.Cst(TypeConstructor.Lattice), tupleType2)
    val restType = Type.Var(5, Kind.Schema, Rigidity.Flexible)
    restType.setText("theRest")
    val tpe = Type.mkSchemaExtend("A", latticeType1, Type.mkSchemaExtend("B", latticeType2, restType))

    val expected = "#{ A<>(String), B<>(Int32, String) | '5 }"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatWellFormedType.Enum.Internal.07") {
    val enumConstructor = TypeConstructor.Enum(Symbol.mkEnumSym("Triplet"), Kind.Star ->: Kind.Star ->: Kind.Star ->: Kind.Star)
    val tvar1 = Type.Var(1, Kind.Star, Rigidity.Flexible)
    val tvar2 = Type.Var(2, Kind.Star, Rigidity.Flexible)
    val tvar3 = Type.Var(3, Kind.Star, Rigidity.Flexible)
    tvar1.setText("a")
    tvar2.setText("b")
    tvar3.setText("c")
    val tpe = Type.mkApply(Type.Cst(enumConstructor), List(tvar1, tvar2, tvar3))

    val expected = "Triplet['1, '2, '3]"
    val actual = FormatType.formatType(tpe)(Audience.Internal)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.01") {
    val tpe = Type.Cst(TypeConstructor.RecordExtend("x"))

    val expected = "{ x: ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Record.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.RecordExtend("x")), Type.Int32)

    val expected = "{ x: Int32 | ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatIllFormedType.Record.External.03") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.RecordExtend("x")), List(Type.Int32, Type.Int32, Type.Str))

    val expected = "RecordExtend(x)[Int32, Int32, String]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.01") {
    val tpe = Type.Cst(TypeConstructor.SchemaExtend("X"))

    val expected = "#{ X?(???) }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Schema.External.02") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.SchemaExtend("X")), Type.Int32)

    val expected = "#{ X?(Int32) | ??? }"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatIllFormedType.Schema.External.03") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.SchemaExtend("X")), List(Type.Int32, Type.Int32, Type.Str))

    val expected = "SchemaExtend(X)[Int32, Int32, String]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.01") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2)), List(Type.Str))

    val expected = "(String, ???)"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Tuple.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Tuple(2)), List(Type.Str, Type.Int32, Type.Float32))

    val expected = "(String, Int32)[Float32]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.01") {
    val tpe = Type.Cst(TypeConstructor.Not)

    val expected = "¬???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatIllFormedType.Effect.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Not), List(Type.Pure, Type.Impure))

    val expected = "¬[Pure, Impure]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.03") {
    val tpe = Type.Cst(TypeConstructor.And)

    val expected = "??? ∧ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.04") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.And), Type.Pure)

    val expected = "(Pure) ∧ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatIllFormedType.Effect.External.05") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.And), List(Type.Pure, Type.Impure, Type.Impure))

    val expected = "∧[Pure, Impure, Impure]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }


  test("FormatIllFormedType.Effect.External.06") {
    val tpe = Type.Cst(TypeConstructor.Or)

    val expected = "??? ∨ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Effect.External.07") {
    val tpe = Type.Apply(Type.Cst(TypeConstructor.Or), Type.Pure)

    val expected = "(Pure) ∨ ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  ignore("FormatIllFormedType.Effect.External.08") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Or), List(Type.Pure, Type.Impure, Type.Impure))

    val expected = "∨[Pure, Impure, Impure]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.01") {
    val tpe = Type.Cst(TypeConstructor.Arrow(2, Type.Pure))

    val expected = "??? -> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.02") {
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(3, Type.Impure)), List(Type.Str))

    val expected = "String -> ??? ~> ???"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatIllFormedType.Arrow.External.03") {
    val eff = Type.Var(0, Kind.Effect, Rigidity.Flexible)
    eff.setText("e")
    val tpe = Type.mkApply(Type.Cst(TypeConstructor.Arrow(2, eff)), List(Type.Str, Type.Float32, Type.Int8))

    val expected = "(String -> Float32 & e)[Int8]"
    val actual = FormatType.formatType(tpe)(Audience.External)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Tuple.01") {
    val tpe1 = Type.mkTuple(List(Type.Int32, Type.Int32, Type.Int32))
    val tpe2 = Type.mkTuple(List(Type.Int32, Type.Bool, Type.Int32))

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "(*, Int32, *)"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Arrow.01") {
    val tpe1 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Int32)
    val tpe2 = Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.Bool)

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "* -> Int32"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }

  test("FormatTypeDiff.Enum.01") {
    val map = Type.Cst(TypeConstructor.Enum(Symbol.mkEnumSym("Map"), Kind.Star ->: Kind.Star ->: Kind.Star))
    val tpe1 = Type.mkApply(map, List(Type.Int32, Type.Bool))
    val tpe2 = Type.mkApply(map, List(Type.Int32, Type.Str))

    val diff = TypeDiff.diff(tpe1, tpe2)
    val expected = "*[*, Bool]"
    val actual = FormatType.formatTypeDiff(diff, VirtualString.Text)(Audience.External).fmt(TerminalContext.NoTerminal)

    assert(actual == expected)
  }
}
