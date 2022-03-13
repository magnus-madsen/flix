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
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Type

import scala.collection.mutable

object FormatSimpleType {
  // MATT docs
  // MATT decide on how API should look (exposing to all types here)
  def formatWellKindedType(tpe: Type, nameContext: Map[Int, String]): String = {
    format(SimpleType.fromWellKindedType(tpe), nameContext)
  }

  // MATT docs
  // MATT private?
  def format(tpe00: SimpleType, nc0: Map[Int, String]): String = {

    // generates the names a, b, ..., z, a1, b1, ...., z1, a2, b2, ...
    val nameGenerator = Iterator.iterate(('a', 0)) {
      case ('z', n) => ('a', n + 1)
      case (c, n) => ((c + 1).toChar, n)
    } map {
      case (c, 0) => s"$c"
      case (c, n) => s"$c$n"
    }

    val nc = mutable.Map.from(nc0)
    val names = mutable.Set.from(nc0.values)

    def nextAvailableName(): String = {
      nameGenerator.find(!names.contains(_)).get // safe to get since nameGenerator is infinite
    }

    /**
      * Wrap the given type with parentheses if it isn't already wrapped.
      */
    def withParens(tpe: String): String = {
      if (tpe.startsWith("(") && tpe.endsWith(")")) {
        tpe
      } else {
        s"($tpe)"
      }
    }

    val Delimited = 0

    def precedence(tpe0: SimpleType): Int = tpe0 match {
      case SimpleType.Unit => Delimited
      case SimpleType.Null => Delimited
      case SimpleType.Bool => Delimited
      case SimpleType.Char => Delimited
      case SimpleType.Float32 => Delimited
      case SimpleType.Float64 => Delimited
      case SimpleType.Int8 => Delimited
      case SimpleType.Int16 => Delimited
      case SimpleType.Int32 => Delimited
      case SimpleType.Int64 => Delimited
      case SimpleType.BigInt => Delimited
      case SimpleType.Str => Delimited
      case SimpleType.Array => Delimited
      case SimpleType.ScopedRef => Delimited
      case SimpleType.Channel => Delimited
      case SimpleType.Lazy => Delimited
      case SimpleType.True => Delimited
      case SimpleType.False => Delimited
      case SimpleType.Region => Delimited
      case SimpleType.Record(fields, rest) => Delimited
      case SimpleType.RecordRow(fields, rest) => Delimited
      case SimpleType.RecordRowEmpty => Delimited
      case SimpleType.RecordEmpty => Delimited
      case SimpleType.RecordConstructor => Delimited
      case SimpleType.RecordRowConstructor(field) => Delimited
      case SimpleType.RecordRowHead(name, tpe) => Delimited
      case SimpleType.Schema(fields, rest) => Delimited
      case SimpleType.SchemaRow(fields, rest) => Delimited
      case SimpleType.SchemaRowEmpty => Delimited
      case SimpleType.SchemaEmpty => Delimited
      case SimpleType.SchemaConstructor => Delimited
      case SimpleType.SchemaRowConstructor(field) => Delimited
      case SimpleType.SchemaRowHead(name, tpe) => Delimited
      case SimpleType.Not(tpe) => ???
      case SimpleType.And(tpes) => ???
      case SimpleType.Or(tpes) => ???
      case SimpleType.RelationConstructor => Delimited
      case SimpleType.Relation(tpes) => Delimited
      case SimpleType.LatticeConstructor => Delimited
      case SimpleType.Lattice(tpes) => Delimited
      case SimpleType.ArrowConstructor(arity) => ???
      case SimpleType.PartialPureArrow(arity, tpes) => ???
      case SimpleType.PartialImpureArrow(arity, tpes) => ???
      case SimpleType.PartialPolyArrow(arity, tpes, eff) => ???
      case SimpleType.PureArrow(args, ret) => ???
      case SimpleType.ImpureArrow(args, ret) => ???
      case SimpleType.PolyArrow(args, ret, eff) => ???
      case SimpleType.TagConstructor(name) => ???
      case SimpleType.PartialTag(name, args) => ???
      case SimpleType.Tag(name, args, ret) => ???
      case SimpleType.Name(name) => Delimited
      case SimpleType.Apply(tpe, tpes) => Delimited
      case SimpleType.Var(id) => Delimited
      case SimpleType.Tuple(length, fields) => Delimited
    }

    def visit(tpe0: SimpleType): String = tpe0 match {
      case SimpleType.Unit => "Unit"
      case SimpleType.Null => "Null"
      case SimpleType.Bool => "Bool"
      case SimpleType.Char => "Char"
      case SimpleType.Float32 => "Float32"
      case SimpleType.Float64 => "Float64"
      case SimpleType.Int8 => "Int8"
      case SimpleType.Int16 => "Int16"
      case SimpleType.Int32 => "Int32"
      case SimpleType.Int64 => "Int64"
      case SimpleType.BigInt => "BigInt"
      case SimpleType.Str => "Str"
      case SimpleType.Array => "Array"
      case SimpleType.ScopedRef => "ScopedRef"
      case SimpleType.Channel => "Channel"
      case SimpleType.Lazy => "Lazy"
      case SimpleType.True => "True"
      case SimpleType.False => "False"
      case SimpleType.Region => "Region"
      case SimpleType.Record(fields, rest) =>
        val fieldString = fields.map {
          case SimpleType.FieldType(name, tpe) => s"$name :: ${visit(tpe)}"
        }.mkString(", ")
        val restString = rest.map(r => s" | ${visit(r)}").getOrElse("")
        s"{$fieldString$restString}"
      case SimpleType.RecordRow(fields, rest) =>
        val fieldString = fields.map {
          case SimpleType.FieldType(name, tpe) => s"$name :: ${visit(tpe)}"
        }.mkString(", ")
        val restString = rest.map(r => s" | ${visit(r)}").getOrElse("")
        s"($fieldString$restString)"
      case SimpleType.RecordRowEmpty => "()"
      case SimpleType.RecordEmpty => "{}"
      case SimpleType.RecordConstructor => "{ ? }"
      case SimpleType.RecordRowConstructor(field) => s"( $field :: ? | ? )"
      case SimpleType.RecordRowHead(name, tpe) => s"( $name :: ${visit(tpe)} | ? )"
      case SimpleType.Schema(fields, rest) =>
        val fieldString = fields.map {
          case SimpleType.FieldType(name, tpe) => s"$name${withParens(visit(tpe))}"
        }.mkString(", ")
        val restString = rest.map(r => s" | ${visit(r)}").getOrElse("")
        s"#{$fieldString$restString}"
      case SimpleType.SchemaRow(fields, rest) =>
        val fieldString = fields.map {
          case SimpleType.FieldType(name, tpe) => s"$name${withParens(visit(tpe))}"
        }.mkString(", ")
        val restString = rest.map(r => s" | ${visit(r)}").getOrElse("")
        s"#($fieldString$restString)"
      case SimpleType.SchemaRowEmpty => "#()"
      case SimpleType.SchemaEmpty => "#{}"
      case SimpleType.SchemaConstructor => "#{ ? }"
      case SimpleType.SchemaRowConstructor(field) => s"#( $field :: ? | ? )"
      case SimpleType.SchemaRowHead(name, tpe) => s"#( $name :: ${visit(tpe)} | ? )"
      case SimpleType.Not(tpe) => s"not ${tpe.map(visit).getOrElse("?")}" // MATT handle parens
      case SimpleType.And(tpes) =>
        val strings = tpes.map(visit).padTo(2, "?")
        strings.mkString(" and ")
      case SimpleType.Or(tpes) =>
        val strings = tpes.map(visit).padTo(2, "?")
        strings.mkString(" or ")
      case SimpleType.RelationConstructor => "Relation" // MATT ?
      case SimpleType.Relation(tpes) =>
        val terms = tpes.map(visit).mkString(", ")
        s"Relation($terms)"
      case SimpleType.LatticeConstructor => "Lattice" // MATT ?
      case SimpleType.Lattice(tpes) =>
        val lat = visit(tpes.last)
        val terms = tpes.init.map(visit).mkString(", ")
        s"Lattice($terms; $lat)"
      case SimpleType.ArrowConstructor(arity) =>
        val params = Iterable.fill(arity - 1)("?").mkString("(", ", ", ")")
        s"$params -> ? & ?"
      case SimpleType.PartialPureArrow(arity, tpes) =>
        val params = tpes.map(visit).padTo(arity - 1, "?").mkString("(", ", ", ")")
        s"$params -> ?"
      case SimpleType.PartialImpureArrow(arity, tpes) =>
        val params = tpes.map(visit).padTo(arity - 1, "?").mkString("(", ", ", ")")
        s"$params ~> ?"
      case SimpleType.PartialPolyArrow(arity, tpes, eff) =>
        val params = tpes.map(visit).padTo(arity - 1, "?").mkString("(", ", ", ")")
        val effString = visit(eff)
        s"$params -> ? & $effString"
      case SimpleType.PureArrow(args, ret) =>
        val params = args.mkString("(", ", ", ")")
        val retString = visit(ret)
        s"$params -> $retString"
      case SimpleType.ImpureArrow(args, ret) =>
        val params = args.mkString("(", ", ", ")")
        val retString = visit(ret)
        s"$params ~> $retString"
      case SimpleType.PolyArrow(args, ret, eff) =>
        val params = args.mkString("(", ", ", ")")
        val retString = visit(ret)
        val effString = visit(eff)
        s"$params ~> $retString & $effString"
      case SimpleType.TagConstructor(name) => ???
      case SimpleType.PartialTag(name, args) => ???
      case SimpleType.Tag(name, args, ret) => ???
      case SimpleType.Name(name) => name
      case SimpleType.Apply(tpe, tpes) =>
        val string = visit(tpe)
        val strings = tpes.map(visit)
        string + strings.mkString("[", ", ", "]")
      case SimpleType.Var(id) =>
        val name = nc.getOrElseUpdate(id, nextAvailableName())
        names.add(name)
        name
      case SimpleType.Tuple(length, fields) =>
        val strings = fields.map(visit).padTo(length, "?")
        strings.mkString("(", ", ", ")")
    }

    visit(tpe00)
  }

}
