/*
 * Copyright 2021 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.TypeConstraint
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

/**
  * A phase that emits a JSON file for library documentation.
  */
object Documentor extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * The "Pseudo-name" of the root namespace.
    */
  val RootNS: String = "@Prelude"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./target/api")

  /**
    * The audience to use for formatting types and effects.
    */
  private implicit val audience: Audience = Audience.External

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Documentor") {
    //
    // Determine whether to generate documentation.
    //
    if (!flix.options.documentor) {
      return root.toSuccess
    }

    //
    // Classes.
    //
    val classesByNS = root.classes.values.groupBy(getNameSpace).map {
      case (ns, decls) =>
        val filtered = decls.filter(_.mod.isPublic).toList
        val sorted = filtered.sortBy(_.sym.name)
        ns -> JArray(sorted.map(visitClass))
    }

    //
    // Enums.
    //
    val enumsByNS = root.enums.values.groupBy(getNameSpace).map {
      case (ns, decls) =>
        val filtered = decls.filter(_.mod.isPublic).toList
        val sorted = filtered.sortBy(_.sym.name)
        ns -> JArray(sorted.map(visitEnum))
    }

    //
    // Type Aliases.
    //
    val typeAliasesByNS = root.typealiases.values.groupBy(getNameSpace).map {
      case (ns, decls) =>
        val filtered = decls.filter(_.mod.isPublic).toList
        val sorted = filtered.sortBy(_.sym.name)
        ns -> JArray(sorted.map(visitTypeAlias))
    }

    //
    // Defs.
    //
    val defsByNS = root.defs.values.groupBy(getNameSpace).map {
      case (ns, decls) =>
        val filtered = decls.filter(_.spec.mod.isPublic).toList
        val sorted = filtered.sortBy(_.sym.name)
        ns -> JArray(sorted.map(visitDef))
    }

    //
    // Compute all namespaces.
    //
    val namespaces = (classesByNS.keySet ++ enumsByNS.keySet ++ typeAliasesByNS.keySet ++ defsByNS.keySet).toList.sorted

    // Construct the JSON object.
    val json = JObject(
      ("namespaces", namespaces),
      ("classes", classesByNS),
      ("enums", enumsByNS),
      ("typeAliases", typeAliasesByNS),
      ("defs", defsByNS)
    )

    // Serialize the JSON object to a string.
    val s = JsonMethods.pretty(JsonMethods.render(json))

    // The path to the file to write.
    val p = OutputDirectory.resolve("api.json")

    // Write the string to the path.
    writeString(s, p)

    root.toSuccess
  }

  /**
    * Returns the namespace of the given class `decl`.
    */
  private def getNameSpace(decl: TypedAst.Class): String = {
    val namespace = decl.sym.namespace
    if (namespace == Nil) RootNS else namespace.mkString(".")
  }

  /**
    * Returns the namespace of the given enum `decl`.
    */
  private def getNameSpace(decl: TypedAst.Enum): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the namespace of the given definition `decl`.
    */
  private def getNameSpace(decl: TypedAst.Def): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the namespace of the given type alias `decl`.
    */
  private def getNameSpace(decl: TypedAst.TypeAlias): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the given definition `defn0` as a JSON object.
    */
  private def visitDef(defn0: Def): JObject = {
    // TODO: Check with Def.d.ts
    // TODO: Deal with  UNit
    ("name" -> defn0.sym.name) ~
      ("tparams" -> defn0.spec.tparams.map(visitTypeParam)) ~
      ("fparams" -> defn0.spec.fparams.map(visitFormalParam)) ~
      ("result" -> FormatType.formatType(defn0.spec.retTpe)) ~
      ("effect" -> FormatType.formatType(defn0.spec.eff)) ~
      ("comment" -> defn0.spec.doc.text.trim) ~
      ("loc" -> visitSourceLocation(defn0.spec.loc))
  }

  // TODO: Refactored until here.

  /**
    * Returns the given instance `inst` as a JSON value.
    */
  private def visitInstance(inst: Instance): JObject = inst match {
    case Instance(_, _, sym, tpe, tconstrs, _, _, _) =>
      ("sym" -> visitInstanceSym(sym)) ~
        //("sym" -> visitClassSym(sym.clazz)) ~
        ("tpe" -> visitType(tpe)) ~
        ("tconstrs" -> tconstrs.map(visitTypeConstraint)) ~
        ("loc" -> visitSourceLocation(sym.loc))
  }

  private def visitInstanceSym(sym: Symbol.InstanceSym): JObject = ("placeholder" -> "placeholder") // TODO

  /**
    * Returns the given type `tpe` as a JSON value.
    */
  private def visitType(tpe: Type): JString = JString(FormatType.formatType(tpe))

  /**
    * Returns the given type constraint `tc` as a JSON value.
    */
  private def visitTypeConstraint(tc: TypeConstraint): JObject = tc match {
    case TypeConstraint(sym, arg, _) =>
      ("sym" -> visitClassSym(sym)) ~
        ("arg" -> visitType(arg))
  }

  /**
    * Returns the given class symbol `sym` as a JSON value.
    */
  private def visitClassSym(sym: Symbol.ClassSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given class symbol `sym` as a JSON value.
    */
  private def visitTypeAliasSym(sym: Symbol.TypeAliasSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given defn symbol `sym` as a JSON value.
    */
  private def visitDefnSym(sym: Symbol.DefnSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.text) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given enum symbol `sym` as a JSON value.
    */
  private def visitEnumSym(sym: Symbol.EnumSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given sig symbol `sym` as a JSON value.
    */
  private def visitSigSym(sym: Symbol.SigSym): JObject =
    ("classSym" -> visitClassSym(sym.clazz)) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  // TODO: Visit the other symbols.

  /**
    * Returns the given source location `loc` as a JSON value.
    */
  private def visitSourceLocation(loc: SourceLocation): JObject = loc match {
    case SourceLocation(_, source, _, beginLine, _, endLine, _) =>
      ("name" -> source.name) ~ ("beginLine" -> beginLine) ~ ("endLine" -> endLine)
  }

  /**
    * Returns the given Kind `kind` as a JSON value.
    */
  def visitKind(kind: Kind): String = kind match {
    case Kind.Wild => "placeholder"
    case Kind.Star => "Star"
    case Kind.Bool => "Bool"
    case Kind.RecordRow => "Record"
    case Kind.SchemaRow => "Schema"
    case Kind.Predicate => "placeholder"
    case Kind.Arrow(k1, k2) => "placeholder"
  }

  /**
    * Returns the given Doc `doc` as a JSON value.
    */
  private def visitDoc(doc: Ast.Doc): JArray =
    JArray(doc.lines.map(JString))

  /**
    * Returns the given Modifier `mod` as a JSON value.
    */
  private def visitModifier(mod: Ast.Modifiers): String = "public"

  /**
    * Returns the given Type Alias `talias` as a JSON value.
    */
  private def visitTypeAlias(talias: TypeAlias): JObject = talias match {
    case TypeAlias(doc, _, sym, tparams, tpe, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("sym" -> visitTypeAliasSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("tpe" -> FormatType.formatType(tpe)) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given Type Parameter `tparam` as a JSON value.
    */
  private def visitTypeParam(tparam: TypeParam): JObject = tparam match {
    case TypeParam(ident, tpe, _) =>
      ("name" -> ident.name) ~ ("kind" -> visitKind(tpe.kind))
  }

  /**
    * Returns the given formal parameter `fparam` as a JSON value.
    */
  private def visitFormalParam(fparam: FormalParam): JObject = fparam match {
    case FormalParam(sym, _, tpe, _) =>
      ("name" -> sym.text) ~ ("tpe" -> visitType(tpe))
  }

  /**
    * Returns the given Sig `sig` as a JSON value.
    */
  private def visitSig(sig: Sig): JObject = sig match {
    case Sig(sym, spec, _) =>
      ("sym" -> visitSigSym(sym)) ~
        ("doc" -> visitDoc(spec.doc)) ~
        ("mod" -> visitModifier(spec.mod)) ~
        ("tparams" -> spec.tparams.map(visitTypeParam)) ~
        ("fparams" -> spec.fparams.map(visitFormalParam)) ~
        ("retTpe" -> visitType(spec.retTpe)) ~
        ("eff" -> visitType(spec.eff)) ~
        ("loc" -> visitSourceLocation(spec.loc))
  }

  /**
    * Returns the given Enum `enum` as a JSON value.
    */
  private def visitEnum(enum: Enum): JObject = enum match {
    case Enum(doc, _, sym, tparams, cases, _, _, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("sym" -> visitEnumSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("cases" -> cases.values.map(visitCase)) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given case `caze` as a JSON value.
    */
  private def visitCase(caze: Case): JObject = caze match {
    case Case(_, tag, _, sc, _) =>
      ("tag" -> tag.name) ~
        ("tpe" -> "TYPE_PLACEHOLDER")
  }

  /**
    * Return the given class `clazz` as a JSON value.
    */
  private def visitClass(cla: Class): JObject = cla match {
    case Class(doc, mod, sym, tparam, superClasses, signatures, laws, loc) =>
      // Compute the type constraints.
      val computedTypeConstraints = superClasses.map {
        tc => visitTypeConstraint(tc)
      }

      // Compute the signatures.
      val computedSig = signatures.map {
        sig => visitSig(sig)
      }

      ("sym" -> visitClassSym(sym)) ~
        ("doc" -> visitDoc(doc)) ~
        ("mod" -> visitModifier(mod)) ~
        ("tparam" -> visitTypeParam(tparam)) ~
        ("superClasses" -> computedTypeConstraints) ~
        ("signatures" -> computedSig) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Writes the given string `s` to the given path `p`.
    */
  private def writeString(s: String, p: Path): Unit = try {
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write to path '$p'.", ex)
  }

}
