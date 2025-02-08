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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.{Scope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.unification.Substitution

object FormatType {
  /**
    * Transforms the given well-kinded type into a string.
    *
    * Minimizes the given type.
    *
    * Performs alpha renaming if the rigidity environment is present.
    */
  def formatType(tpe0: Type, renv: Option[RigidityEnv] = None)(implicit flix: Flix): String = {
    val tpe = TypeSimplifier.simplify(tpe0)
    val renamed = renv match {
      case None => tpe
      case Some(env) => alphaRename(tpe, env)
    }
    formatTypeWithOptions(renamed, flix.getFormatOptions)
  }

  /**
    * Renames all flexible variables in the given `tpe` to fresh consecutively numbered variables.
    *
    * NOTE: This function should *ONLY* be used for pretty printing.
    */
  private def alphaRename(tpe: Type, renv: RigidityEnv): Type = {
    // Compute the free type variables.
    val freeVars = tpe.typeVars.toList.sortBy(_.sym.id)

    // Compute the flexible variables (i.e. the free variables that are not rigid).
    val flexibleVars = renv.getFlexibleVarsOf(freeVars)(Scope.Top) // TODO LEVELS ideally we should have a proper scope here

    // Compute a substitution that maps the first flexible variable to id 1 and so forth.
    val m = flexibleVars.zipWithIndex.map {
      case (tvar@Type.Var(sym, loc), index) =>
        sym -> (Type.Var(new Symbol.KindedTypeVarSym(index, sym.text, sym.kind, sym.isSlack, sym.scope, loc), loc): Type)
    }
    val s = Substitution(m.toMap)

    // Apply the substitution to the type.
    s(tpe)
  }

  /**
    * Transforms the given well-kinded type into a string, using the given format options.
    */
  def formatTypeWithOptions(tpe: Type, fmt: FormatOptions): String = {
    try {
      format(SimpleType.fromWellKindedType(tpe))(fmt)
    } catch {
      case _: Throwable => "ERR_UNABLE_TO_FORMAT_TYPE"
    }
  }

  /**
    * Transforms the given kinded type variable symbol into a string.
    */
  def formatTypeVarSymWithOptions(sym: Symbol.KindedTypeVarSym, fmt: FormatOptions): String = {
    val tpe = Type.Var(sym, SourceLocation.Unknown)
    formatTypeWithOptions(tpe, fmt)
  }

  /**
    * Transforms the given simple type into a string.
    */
  def formatSimpleType(tpe: SimpleType)(implicit flix: Flix): String =
    formatSimpleTypeWithOptions(tpe, flix.getFormatOptions)

  /**
    * Transforms the given simple type into a string, using the given format options.
    */
  def formatSimpleTypeWithOptions(tpe: SimpleType, fmt: FormatOptions): String =
    format(tpe)(fmt)

  /**
    * Transforms the given type into a string.
    */
  private def format(tpe00: SimpleType)(implicit fmt: FormatOptions): String = {

    /**
      * Wraps the given type with parentheses.
      */
    def parenthesize(s: String): String = "(" + s + ")"

    /**
      * Transforms the given record `labelType` pair into a string.
      */
    def visitRecordLabelType(labelType: SimpleType.RecordLabelType): String = labelType match {
      case SimpleType.RecordLabelType(label, tpe) => s"$label = ${visit(tpe, Mode.Type)}"
    }

    /**
      * Transforms the given schema `fieldType` pair into a string.
      */
    def visitSchemaFieldType(fieldType: SimpleType.PredicateFieldType): String = fieldType match {
      case SimpleType.RelationFieldType(field, tpes) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"$field($tpeString)"
      case SimpleType.LatticeFieldType(field, tpes, lat) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        val latString = visit(lat, Mode.Type)
        s"$field($tpeString; $latString)"
      case SimpleType.NonPredFieldType(field, tpe) =>
        val tpeString = visit(tpe, Mode.Type)
        s"$field(<$tpeString>)"
    }

    /**
      * Transforms the given type into a string,
      * delimiting it as appropriate for display as a function argument.
      */
    def delimitFunctionArg(arg: SimpleType): String = arg match {
      // Tuples get an extra set of parentheses
      case tuple: SimpleType.Tuple => parenthesize(visit(tuple, Mode.Type))
      case tpe => delimit(tpe, Mode.Type)
    }

    /**
      * Returns `true` iff the given `tpe` is innately delimited,
      * meaning that it never needs parenthesization.
      */
    def isDelimited(tpe: SimpleType): Boolean = tpe match {
      // non-delimited types
      case SimpleType.Not(_) => false
      case SimpleType.And(_) => false
      case SimpleType.Or(_) => false
      case SimpleType.Complement(_) => false
      case SimpleType.Intersection(_) => false
      case SimpleType.SymmetricDiff(_) => false
      case SimpleType.Difference(_) => false
      case SimpleType.Plus(_) => false
      case SimpleType.PureArrow(_, _) => false
      case SimpleType.PolyArrow(_, _, _) => false
      case SimpleType.ArrowWithoutEffect(_, _) => false

      // delimited types
      case SimpleType.Hole => true
      case SimpleType.Void => true
      case SimpleType.AnyType => true
      case SimpleType.Unit => true
      case SimpleType.Null => true
      case SimpleType.Bool => true
      case SimpleType.Char => true
      case SimpleType.Float32 => true
      case SimpleType.Float64 => true
      case SimpleType.BigDecimal => true
      case SimpleType.Int8 => true
      case SimpleType.Int16 => true
      case SimpleType.Int32 => true
      case SimpleType.Int64 => true
      case SimpleType.BigInt => true
      case SimpleType.Str => true
      case SimpleType.Regex => true
      case SimpleType.Array => true
      case SimpleType.ArrayWithoutRegion => true
      case SimpleType.Vector => true
      case SimpleType.Sender => true
      case SimpleType.Receiver => true
      case SimpleType.Lazy => true
      case SimpleType.True => true
      case SimpleType.False => true
      case SimpleType.Pure => true
      case SimpleType.Univ => true
      case SimpleType.Region(_) => true
      case SimpleType.RegionToStar => true
      case SimpleType.RegionWithoutRegion => true
      case SimpleType.RecordConstructor(_) => true
      case SimpleType.Record(_) => true
      case SimpleType.RecordExtend(_, _) => true
      case SimpleType.RecordRow(_) => true
      case SimpleType.RecordRowExtend(_, _) => true
      case SimpleType.SchemaConstructor(_) => true
      case SimpleType.Schema(_) => true
      case SimpleType.SchemaExtend(_, _) => true
      case SimpleType.SchemaRow(_) => true
      case SimpleType.SchemaRowExtend(_, _) => true
      case SimpleType.RelationConstructor => true
      case SimpleType.Relation(_) => true
      case SimpleType.LatticeConstructor => true
      case SimpleType.Lattice(_, _) => true
      case SimpleType.TagConstructor(_) => true
      case SimpleType.Name(_) => true
      case SimpleType.Apply(_, _) => true
      case SimpleType.Var(_, _, _) => true
      case SimpleType.Tuple(_) => true
      case SimpleType.JvmToType(_) => true
      case SimpleType.JvmToEff(_) => true
      case SimpleType.JvmUnresolvedConstructor(_, _) => true
      case SimpleType.JvmUnresolvedField(_, _) => true
      case SimpleType.JvmUnresolvedMethod(_, _, _) => true
      case SimpleType.JvmUnresolvedStaticMethod(_, _, _) => true
      case SimpleType.JvmConstructor(_) => true
      case SimpleType.JvmField(_) => true
      case SimpleType.JvmMethod(_) => true
      case SimpleType.Union(_) => true
      case SimpleType.Error => true
    }

    /**
      * Delimits the given `tpe`, parenthesizing it if needed.
      */
    def delimit(tpe: SimpleType, mode: Mode): String = {
      if (isDelimited(tpe)) {
        visit(tpe, mode)
      } else {
        parenthesize(visit(tpe, mode))
      }
    }

    /**
      * Converts the given `tpe0` to a string.
      */
    def visit(tpe0: SimpleType, mode: Mode): String = tpe0 match {
      case SimpleType.Hole => "?"
      case SimpleType.Void => "Void"
      case SimpleType.AnyType => "AnyType"
      case SimpleType.Unit => "Unit"
      case SimpleType.Null => "Null"
      case SimpleType.Bool => "Bool"
      case SimpleType.Char => "Char"
      case SimpleType.Float32 => "Float32"
      case SimpleType.Float64 => "Float64"
      case SimpleType.BigDecimal => "BigDecimal"
      case SimpleType.Int8 => "Int8"
      case SimpleType.Int16 => "Int16"
      case SimpleType.Int32 => "Int32"
      case SimpleType.Int64 => "Int64"
      case SimpleType.BigInt => "BigInt"
      case SimpleType.Str => "String"
      case SimpleType.Regex => "Regex"
      case SimpleType.Array => "Array"
      case SimpleType.ArrayWithoutRegion => "ArrayWithoutRegion"
      case SimpleType.Vector => "Vector"
      case SimpleType.Sender => "Sender"
      case SimpleType.Receiver => "Receiver"
      case SimpleType.Lazy => "Lazy"
      case SimpleType.False => "false"
      case SimpleType.True => "true"
      case SimpleType.Pure => mode match {
        case Mode.Type => "Pure"
        case Mode.Purity => "{}"
      }
      case SimpleType.Univ => "Univ"
      case SimpleType.Region(name) => name
      case SimpleType.RegionToStar => "Region"
      case SimpleType.RegionWithoutRegion => "RegionWithoutRegion"
      case SimpleType.Record(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"{ $labelString }"
      case SimpleType.RecordExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, mode)
        s"{ $labelString | $restString }"
      case SimpleType.RecordRow(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"( $labelString )"
      case SimpleType.RecordRowExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"( $labelString | $restString )"
      case SimpleType.RecordConstructor(arg) => s"{ ${visit(arg, Mode.Type)} }"
      case SimpleType.Schema(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case SimpleType.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#{ $fieldString | $restString }"
      case SimpleType.SchemaRow(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case SimpleType.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#( $fieldString | $restString )"
      case SimpleType.SchemaConstructor(arg) => s"#{ ${visit(arg, Mode.Type)} }"
      case SimpleType.Not(tpe) => s"not ${delimit(tpe, mode)}"
      case SimpleType.And(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" and ")
      case SimpleType.Or(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" or ")
      case SimpleType.Complement(tpe) => s"~${delimit(tpe, mode)}"
      case SimpleType.Union(tpes) =>
        val strings = tpes.map(visit(_, mode))
        strings.mkString("{", ", ", "}")
      case SimpleType.Plus(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" + ")
      case SimpleType.Intersection(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" & ")
      case SimpleType.SymmetricDiff(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" ⊕ ")
      case SimpleType.Difference(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" - ")
      case SimpleType.RelationConstructor => "Relation"
      case SimpleType.Relation(tpes) =>
        val terms = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"Relation($terms)"
      case SimpleType.LatticeConstructor => "Lattice"
      case SimpleType.Lattice(tpes0, lat0) =>
        val lat = visit(lat0, Mode.Type)
        val tpes = tpes0.map(visit(_, Mode.Type)).mkString(", ")
        s"Lattice($tpes; $lat)"
      case SimpleType.PureArrow(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString"
      case SimpleType.PolyArrow(arg, eff, ret) =>
        val argString = delimitFunctionArg(arg)
        val effString = visit(eff, Mode.Purity)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString \\ $effString"
      case SimpleType.ArrowWithoutEffect(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString --> $retString"
      case SimpleType.TagConstructor(name) => name
      case SimpleType.Name(name) => name
      case SimpleType.Apply(tpe, tpes) =>
        val string = visit(tpe, Mode.Type)
        val strings = tpes.map(visit(_, Mode.Type))
        string + strings.mkString("[", ", ", "]")
      case SimpleType.Var(id, kind, text) =>
        val string: String = kind match {
          case Kind.Wild => "_" + id.toString
          case Kind.WildCaseSet => "_c" + id.toString
          case Kind.Star => "t" + id
          case Kind.Eff => "e" + id
          case Kind.Bool => "b" + id
          case Kind.RecordRow => "r" + id
          case Kind.SchemaRow => "s" + id
          case Kind.Predicate => "'" + id.toString
          case Kind.Jvm => "j" + id.toString
          case Kind.CaseSet(_) => "c" + id.toString
          case Kind.Arrow(_, _) => "'" + id.toString
          case Kind.Error => "err" + id.toString
        }
        fmt.varNames match {
          case FormatOptions.VarName.IdBased => string
          case FormatOptions.VarName.NameBased => text match {
            case VarText.Absent => string
            case VarText.SourceText(s) => s
          }
        }

      case SimpleType.Tuple(elms) =>
        elms.map(visit(_, Mode.Type)).mkString("(", ", ", ")")

      case SimpleType.JvmToType(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToType(" + arg + ")"

      case SimpleType.JvmToEff(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToEff(" + arg + ")"

      case SimpleType.JvmUnresolvedConstructor(name, tpes0) =>
        val tpes = tpes0.map(visit(_, Mode.Type))
        "JvmUnresolvedConstructor(" + name + ", " + tpes.mkString(", ") + ")"

      case SimpleType.JvmUnresolvedField(t0, name) =>
        val t = visit(t0, Mode.Type)
        "JvmUnresolvedField(" + t + ", " + name + ")"

      case SimpleType.JvmUnresolvedMethod(t0, name, ts0) =>
        val t = visit(t0, Mode.Type)
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedMethod(" + t + ", " + name + ", " + ts.mkString(", ") + ")"

      case SimpleType.JvmUnresolvedStaticMethod(clazz, name, ts0) =>
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedStaticMethod(" + clazz + ", " + name + ", " + ts.mkString(", ") + ")"

      case SimpleType.JvmConstructor(constructor) =>
        s"JvmConstructor($constructor)"

      case SimpleType.JvmField(field) =>
        s"JvmField($field)"

      case SimpleType.JvmMethod(method) =>
        s"JvmMethod($method)"

      case SimpleType.Error => "Error"

    }

    visit(tpe00, Mode.Type)
  }

  /**
    * Flag indicating whether a type should be formatted as an effect or as a regular type.
    */
  private sealed trait Mode

  private object Mode {
    case object Purity extends Mode

    case object Type extends Mode
  }

}
