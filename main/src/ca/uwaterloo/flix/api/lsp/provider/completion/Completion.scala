/*
 * Copyright 2023 Magnus Madsen, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, CompletionItemLabelDetails, InsertTextFormat, Position, Range, TextEdit}
import ca.uwaterloo.flix.language.ast.Symbol.{EnumSym, ModuleSym, TypeAliasSym}
import ca.uwaterloo.flix.language.ast.shared.AnchorPosition
import ca.uwaterloo.flix.language.ast.{Name, ResolvedAst, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatType}

import java.lang.reflect.{Field, Method}

/**
  * A common super-type for auto-completions.
  */
sealed trait Completion {
  /**
    * Returns a LSP completion item for `this`.
    */
  def toCompletionItem(context: CompletionContext)(implicit flix: Flix): CompletionItem = this match {

    case Completion.EffectCompletion(sym, doc) =>
      val name = sym.toString
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.Lower, name),
        textEdit         = TextEdit(context.range, name),
        documentation    = Some(doc),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.AutoUseEffCompletion(sym, doc, ap) =>
      val name = sym.name
      val qualifiedName = sym.toString
      val additionalTextEdits = List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val labelDetails = CompletionItemLabelDetails(
        None,
        Some(s" use $qualifiedName"))
      CompletionItem(
        label               = name,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(Priority.Lower, name),
        textEdit            = TextEdit(context.range, name),
        documentation       = Some(doc),
        kind                = CompletionItemKind.Enum,
        additionalTextEdits = additionalTextEdits
      )

    case Completion.AutoUseEnumCompletion(sym, doc, ap) =>
      val name = sym.name
      val qualifiedName = sym.toString
      val additionalTextEdits = List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val labelDetails = CompletionItemLabelDetails(
        None,
        Some(s" use $qualifiedName"))
      CompletionItem(
        label               = name,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(Priority.Lower, name),
        textEdit            = TextEdit(context.range, name),
        documentation       = Some(doc),
        kind                = CompletionItemKind.Enum,
        additionalTextEdits = additionalTextEdits
      )

    case Completion.KeywordCompletion(name, priority) =>
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(priority, name),
        textEdit = TextEdit(context.range, s"$name "),
        kind     = CompletionItemKind.Keyword
      )

    case Completion.KeywordLiteralCompletion(name, priority) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Keyword
      )

    case Completion.KindCompletion(kind) =>
      CompletionItem(
        label    = kind,
        sortText = Priority.toSortText(Priority.Highest, kind),
        textEdit = TextEdit(context.range, kind),
        kind     = CompletionItemKind.TypeParameter
      )

    case Completion.LabelCompletion(label, prefix) =>
      val name = s"$prefix#${label.name}"
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.Highest, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Variable
      )

    case Completion.PredicateCompletion(name, arity, detail) =>
      val args = (1 until arity + 1).map(i => s"$${$i:x$i}").mkString(", ")
      CompletionItem(
        label            = s"$name/$arity",
        sortText         = Priority.toSortText(Priority.Lower, name),
        textEdit         = TextEdit(context.range, s"$name($args)"),
        detail           = Some(detail),
        kind             = CompletionItemKind.Field,
        insertTextFormat = InsertTextFormat.Snippet
      )

    case Completion.TypeBuiltinCompletion(name, priority) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, name),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Enum
      )

    case Completion.TypeBuiltinPolyCompletion(name, edit, priority) =>
      CompletionItem(label = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = TextEdit(context.range, edit),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.TypeAliasCompletion(aliasSym, nameSuffix, priority, textEdit, documentation) =>
      CompletionItem(
        label            = s"${aliasSym.name}$nameSuffix",
        sortText         = Priority.toSortText(priority, aliasSym.name),
        textEdit         = textEdit,
        documentation    = documentation,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Enum
      )

    case Completion.WithCompletion(name, priority, textEdit, documentation, insertTextFormat) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(priority, name),
        textEdit         = textEdit,
        documentation = documentation,
        insertTextFormat = insertTextFormat,
        kind = CompletionItemKind.Class
      )

    case Completion.WithHandlerCompletion(name, textEdit) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.Highest, name),
        textEdit         = textEdit,
        documentation    = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.ImportCompletion(name, isPackage) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.Highest, name),
        textEdit         = TextEdit(context.range, name),
        documentation    = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = {
          if (isPackage) CompletionItemKind.Module
          else CompletionItemKind.Class
        }
      )

    case Completion.AutoImportCompletion(name, path, ap, labelDetails , priority) =>
      CompletionItem(
        label               = name,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(priority, name),
        textEdit            = TextEdit(context.range, name),
        insertTextFormat    = InsertTextFormat.PlainText,
        kind                = CompletionItemKind.Class,
        additionalTextEdits = List(Completion.mkTextEdit(ap, s"import $path"))
      )

    case Completion.SnippetCompletion(name, snippet, documentation) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.High, name),
        textEdit         = TextEdit(context.range, snippet),
        documentation    = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.MagicMatchCompletion(name, range, snippet, documentation) =>
      CompletionItem(
        label            = name,
        sortText         = Priority.toSortText(Priority.High, name),
        textEdit         = TextEdit(range, snippet),
        documentation    = Some(documentation),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.LocalVarCompletion(name) =>
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.High, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Variable
      )

    case Completion.LocalDeclarationCompletion(name) =>
        CompletionItem(
          label    = name,
          sortText = Priority.toSortText(Priority.High, name),
          textEdit = TextEdit(context.range, name),
          kind     = CompletionItemKind.Enum
        )

    case Completion.LocalJavaClassCompletion(name) =>
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.High, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Class
      )

    case Completion.LocalDefCompletion(sym, fparams) =>
      val snippet = sym.text + fparams.zipWithIndex.map{ case (fparam, idx) => s"$${${idx + 1}:${fparam.sym.text}}" }.mkString("(", ", ", ")")
      CompletionItem(
        label    = sym.text,
        sortText = Priority.toSortText(Priority.High, sym.text),
        textEdit = TextEdit(context.range, snippet),
        insertTextFormat = InsertTextFormat.Snippet,
        kind     = CompletionItemKind.Function
      )

    case Completion.DefCompletion(decl, ap, qualified, inScope) =>
      val qualifiedName = decl.sym.toString
      val label = if (qualified) qualifiedName else decl.sym.name
      val snippet = CompletionUtils.getApplySnippet(label, decl.spec.fparams)(context)
      val description = if(!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)), description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val priority = if (inScope) Priority.High else Priority.Lower
      CompletionItem(
        label               = label,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(priority, qualifiedName),
        filterText          = Some(CompletionUtils.getFilterTextForName(qualifiedName)),
        textEdit            = TextEdit(context.range, snippet),
        detail              = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation       = Some(decl.spec.doc.text),
        insertTextFormat    = InsertTextFormat.Snippet,
        kind                = CompletionItemKind.Function,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.EnumCompletion(enm, ap, qualified, inScope) =>
      val qualifiedName = enm.sym.toString
      val label = if (qualified) qualifiedName else enm.sym.name
      val description = if(!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val priority = if (inScope) Priority.High else Priority.Lower
      CompletionItem(
        label               = label,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(priority, qualifiedName),
        filterText          = Some(CompletionUtils.getFilterTextForName(qualifiedName)),
        textEdit            = TextEdit(context.range, label),
        documentation       = Some(enm.doc.text),
        kind                = CompletionItemKind.Enum,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.StructCompletion(struct, ap, qualified, inScope) =>
      val qualifiedName = struct.sym.toString
      val name = if (qualified) qualifiedName else struct.sym.name
      val label = name + CompletionUtils.formatTParams(struct.tparams)
      val snippet = name + CompletionUtils.formatTParamsSnippet(struct.tparams)
      val description = if(!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val priority: Priority = if (inScope) Priority.High else Priority.Lower
      CompletionItem(
        label               = label,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(priority, name),
        textEdit            = TextEdit(context.range, snippet),
        documentation       = Some(struct.doc.text),
        insertTextFormat    = InsertTextFormat.Snippet,
        kind                = CompletionItemKind.Struct,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.TraitCompletion(trt, ap, qualified, inScope) =>
      val qualifiedName = trt.sym.toString
      val name = if (qualified) qualifiedName else trt.sym.name
      val description = if(!qualified) {
        Some(if (inScope) qualifiedName else s"use $qualifiedName")
      } else None
      val labelDetails = CompletionItemLabelDetails(None, description)
      val additionalTextEdit = if (inScope) Nil else List(Completion.mkTextEdit(ap, s"use $qualifiedName"))
      val priority: Priority = if (inScope) Priority.High else Priority.Lower
      CompletionItem(
        label               = name,
        labelDetails        = Some(labelDetails),
        sortText            = Priority.toSortText(priority, name),
        textEdit            = TextEdit(context.range, name),
        documentation       = Some(trt.doc.text),
        kind                = CompletionItemKind.Struct,
        additionalTextEdits = additionalTextEdit
      )

    case Completion.SigCompletion(decl) =>
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      val labelDetails = CompletionItemLabelDetails(
        Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)),
        None)
      CompletionItem(
        label            = name,
        labelDetails     = Some(labelDetails),
        sortText         = Priority.toSortText(Priority.Lower, name),
        filterText       = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation    = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Interface
      )

    case Completion.OpCompletion(decl) =>
      // NB: priority is high because only an op can come after `do`
      val name = decl.sym.toString
      val snippet = CompletionUtils.getApplySnippet(name, decl.spec.fparams)(context)
      val labelDetails = CompletionItemLabelDetails(
        Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)),
        None)
      CompletionItem(
        label            = name,
        labelDetails     = Some(labelDetails),
        sortText         = Priority.toSortText(Priority.Lower, name),
        filterText       = Some(CompletionUtils.getFilterTextForName(name)),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)(flix)),
        documentation    = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Interface
      )

    case Completion.InstanceCompletion(trt, completion) =>
      val traitSym = trt.sym
      CompletionItem(
        label            = s"$traitSym[...]",
        sortText         = Priority.toSortText(Priority.Highest, traitSym.toString),
        textEdit         = TextEdit(context.range, completion),
        detail           = Some(InstanceCompleter.fmtTrait(trt)),
        documentation    = Some(trt.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Snippet
      )

    case Completion.UseEnumCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Enum
      )

    case Completion.UseEffCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Enum
      )

    case Completion.UseDefCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseEnumTagCompletion(sym, caze) =>
      val name = s"${sym.toString}.${caze.sym.name}"
      CompletionItem(
        label         = name,
        sortText      = Priority.toSortText(Priority.Lower, name),
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseOpCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.UseSignatureCompletion(name) =>
      CompletionItem(
        label         = name,
        sortText      = name,
        textEdit      = TextEdit(context.range, name),
        documentation = None,
        kind          = CompletionItemKind.Method
      )

    case Completion.EnumTagCompletion(enumSym, cas) =>
      val name = s"${enumSym.toString}.${cas.sym.name}"
      val args = (1 until cas.tpes.length + 1).map(i => s"?elem$i").mkString(", ")
      val snippet = if (args.isEmpty) name else s"$name($args)"
      val labelDetails = CompletionItemLabelDetails(
        Some(CompletionUtils.getParamsLabelForEnumTags(cas)),
        None)
      CompletionItem(
        label            = name,
        labelDetails     = Some(labelDetails),
        sortText         = Priority.toSortText(Priority.Default, name),
        textEdit         = TextEdit(context.range, snippet),
        detail           = Some(enumSym.name),
        documentation    = None,
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.EnumMember
      )

    case Completion.ModCompletion(modSym) =>
      val name = modSym.toString
      CompletionItem(
        label    = name,
        sortText = Priority.toSortText(Priority.Default, name),
        textEdit = TextEdit(context.range, name),
        kind     = CompletionItemKind.Module
      )

    case Completion.FieldCompletion(ident, field) =>
      val label = field.getName
      val text = field.getName
      val range = Range.from(ident.loc)

      CompletionItem(
        label            = label,
        sortText         = Priority.toSortText(Priority.Lowest, label),
        textEdit         = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.PlainText,
        kind             = CompletionItemKind.Method
      )

    case Completion.StructFieldCompletion(field, loc, tpe) =>
      CompletionItem(
        label    = field,
        sortText = Priority.toSortText(Priority.Lowest, field),
        textEdit = TextEdit(Range.from(loc), field),
        detail   = Some(FormatType.formatType(tpe)(flix)),
        kind     = CompletionItemKind.Property,
      )

    case Completion.MethodCompletion(ident, method) =>
      val argsWithName = method.getParameters.map(_.getName)
      val argsWithNameAndType = method.getParameters.map(p => p.getName + ": " + p.getType.getSimpleName)
      val returnType = method.getReturnType.getSimpleName
      val returnEffect = "IO"

      val label = method.getName
      val labelDetails = CompletionItemLabelDetails(
        Some( "(" + argsWithNameAndType.mkString(", ") + "): " + returnType + " \\ " + returnEffect),
        None
      )
      val text = method.getName + "(" + argsWithName.zipWithIndex.map {case (arg, i) => s"$${${i + 1}:$arg}" }.mkString(", ") + ")"
      val range = Range.from(ident.loc)

      CompletionItem(
        label            = label,
        labelDetails     = Some(labelDetails),
        sortText         = Priority.toSortText(Priority.Lowest, label),
        textEdit         = TextEdit(range, text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind             = CompletionItemKind.Method
      )

    case Completion.HoleCompletion(sym, decl, priority, loc) =>
      val name = decl.sym.toString
      val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
        case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.bnd.sym.text}}"
      } ::: sym.text :: Nil
      val params = args.mkString(", ")
      val snippet = s"$name($params)"
      val labelDetails = CompletionItemLabelDetails(
        Some(CompletionUtils.getLabelForSpec(decl.spec)(flix)),
        None)
      CompletionItem(
        label             = name,
        labelDetails      = Some(labelDetails),
        filterText        = Some(s"${sym.text}?$name"),
        sortText          = priority,
        textEdit          = TextEdit(Range.from(loc), snippet),
        detail            = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation     = Some(decl.spec.doc.text),
        insertTextFormat  = InsertTextFormat.Snippet,
        kind              = CompletionItemKind.Function)

  }
}

object Completion {

  /**
    * Represents an effect symbol completion.
    *
    * @param sym the effect symbol.
    * @param doc the documentation associated with the effect.
    */
  case class EffectCompletion(sym: Symbol.EffectSym, doc: String) extends Completion

  /**
    * Represents a keyword completion.
    *
    * @param name      the name of the keyword.
    * @param priority  the completion priority of the keyword.
    */
  case class KeywordCompletion(name: String, priority: Priority) extends Completion

  /**
    * Represents a keyword literal completion (i.e. `true`).
    *
    * The reason we differentiate bewteen normal keywords and these literals
    * is because completions for the former should include a trailing space
    * whereas completions for the latter we might not want one.
    *
    * To illustrate this consider the two following correct completions (where ˽ denotes a space)
    *
    * `de`      --->    `def˽`
    * `f(fal)`  --->    `f(false)`
    *
    * After the keyword `def` we *always* want a space but if we were
    * to add a trailing space after `false` we would get the unnatural completion
    *
    * `f(fal)`  --->    `f(false˽)`
    *
    * @param literal   the literal keyword text.
    * @param priority  the priority of the keyword.
    */
  case class KeywordLiteralCompletion(literal: String, priority: Priority) extends Completion

  /**
    * Represents a completion for a kind.
    *
    * @param kind the name of the kind.
    */
  case class KindCompletion(kind: String) extends Completion

  /**
    * Represents a label completion.
    *
    * @param label  the label.
    * @param prefix the prefix.
    */
  case class LabelCompletion(label: Name.Label, prefix: String) extends Completion

  /**
    * Represents a predicate completion.
    *
    * @param name   the name of the predicate.
    * @param arity  the arity of the predicate.
    * @param detail the type of the predicate.
    */
  case class PredicateCompletion(name: String, arity: Int, detail: String) extends Completion

  /**
    * Represents a type completion for builtin
    *
    * @param name             the name of the BuiltinType.
    * @param priority         the priority of the BuiltinType.
    */
  case class TypeBuiltinCompletion(name: String, priority: Priority) extends Completion

  /**
    * Represents a type completion for a builtin polymorphic type.
    *
    * @param name      the name of the type.
    * @param priority  the priority of the type.
    */
  case class TypeBuiltinPolyCompletion(name: String, edit: String, priority: Priority) extends Completion

  /**
    * Represents a type completion for alias
    *
    * @param aliasSym      the alias symbol.
    * @param nameSuffix    the suffix for the name of the AliasType.
    * @param priority      the priority of the AliasType.
    * @param textEdit      the edit which is applied to a document when selecting this completion.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class TypeAliasCompletion(aliasSym: TypeAliasSym, nameSuffix: String, priority: Priority, textEdit: TextEdit,
                                 documentation: Option[String]) extends Completion

  /**
    * Represents a With completion
    *
    * @param name             the name of the completion.
    * @param priority         the priority of the completion.
    * @param textEdit         the edit which is applied to a document when selecting this completion.
    * @param documentation    a human-readable string that represents a doc-comment.
    * @param insertTextFormat the format of the insert text.
    */
  case class WithCompletion(name: String, priority: Priority, textEdit: TextEdit, documentation: Option[String],
                            insertTextFormat: InsertTextFormat) extends Completion

  /**
    * Represents a WithHandler completion
    *
    * @param name             the name of the completion.
    * @param textEdit         the edit which is applied to a document when selecting this completion.
    */
  case class WithHandlerCompletion(name: String, textEdit: TextEdit) extends Completion

  /**
    * Represents a package, class, or interface completion.
    *
    * @param name       the name to be completed.
    * @param isPackage  whether the completion is a package.
    */
  case class ImportCompletion(name: String, isPackage: Boolean) extends Completion

  /**
    * Represents an auto-import completion.
    *
    * @param name          the name to be completed under cursor.
    * @param qualifiedName the path of the java class we will auto-import.
    * @param ap            the anchor position.
    * @param labelDetails  to show the namespace of class we are going to import
    * @param priority      the priority of the completion.
    */
  case class AutoImportCompletion(name:String, qualifiedName: String, ap: AnchorPosition, labelDetails: CompletionItemLabelDetails, priority: Priority) extends Completion

  /**
   * Represents an auto-import completion.
   *
   * @param eff           the effect to complete and use.
   * @param doc           the documentation associated with the effect.
   * @param ap            the anchor position for the use statement.
   */
  case class AutoUseEffCompletion(sym: Symbol.EffectSym, doc: String, ap: AnchorPosition) extends Completion

  /**
   * Represents an auto-import completion.
   *
   * @param enum          the enum to complete and use.
   * @param doc           the documentation associated with the effect.
   * @param ap            the anchor position for the use statement.
   */
  case class AutoUseEnumCompletion(sym: Symbol.EnumSym, doc: String, ap: AnchorPosition) extends Completion

  /**
    * Represents a Snippet completion
    *
    * @param name          the name of the snippet.
    * @param snippet       the snippet for TextEdit.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class SnippetCompletion(name: String, snippet: String, documentation: String) extends Completion

  /**
    * Represents a Snippet completion
    *
    * @param name          the name of the snippet.
    * @param range         the range for TextEdit.
    * @param snippet       the snippet for TextEdit.
    * @param documentation a human-readable string that represents a doc-comment.
    */
  case class MagicMatchCompletion(name: String, range: Range, snippet: String, documentation: String) extends Completion

  /**
    * Represents a Var completion
    *
    * @param name the name of the variable to complete.
    */
  case class LocalVarCompletion(name: String) extends Completion

  /**
    * Represents a Declaration completion
    *
    * @param name the name of the declaration to complete.
    */
  case class LocalDeclarationCompletion(name: String) extends Completion

  /**
    * Represents a Java Class completion
    *
    * @param name the name of the java class to complete.
    */
  case class LocalJavaClassCompletion(name: String) extends Completion

  /**
    * Represents a local def completion
    *
    * @param sym     the symbol of the local function
    * @param fparams the formal parameters of the local function
    */
  case class LocalDefCompletion(sym: Symbol.VarSym, fparams: List[ResolvedAst.FormalParam]) extends Completion

  /**
    * Represents a Def completion
    *
    * @param decl      the def decl.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the def is inScope.
    */
  case class DefCompletion(decl: TypedAst.Def, ap: AnchorPosition, qualified:Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a Enum completion
    *
    * @param enm      the enum construct.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the enum is inScope.
    */
  case class EnumCompletion(enm: TypedAst.Enum, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a struct completion
    *
    * @param struct    the struct construct.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the enum is inScope.
    */
  case class StructCompletion(struct: TypedAst.Struct, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a trait completion
    *
    * @param trt       trait struct construct.
    * @param ap        the anchor position for the use statement.
    * @param qualified indicate whether to use a qualified label.
    * @param inScope   indicate whether to the enum is inScope.
    */
  case class TraitCompletion(trt: TypedAst.Trait, ap: AnchorPosition, qualified: Boolean, inScope: Boolean) extends Completion

  /**
    * Represents a Signature completion
    *
    * @param decl the signature decl.
    */
  case class SigCompletion(decl: TypedAst.Sig) extends Completion

  /**
    * Represents a Op completion
    *
    * @param decl the op decl.
    */
  case class OpCompletion(decl: TypedAst.Op) extends Completion

  /**
    * Represents an Instance completion (based on traits)
    *
    * @param trt        the trait.
    * @param completion the completion string (used as information for TextEdit).
    */
  case class InstanceCompletion(trt: TypedAst.Trait, completion: String) extends Completion

  /**
    * Represents a Use Enum completion.
    *
    * @param name the name of the use enum completion.
    */
  case class UseEnumCompletion(name: String) extends Completion

  /**
    * Represents a Use Effect completion.
    *
    * @param name the name of the use effect completion.
    */
  case class UseEffCompletion(name: String) extends Completion

  /**
    * Represents a Use Def completion.
    *
    * @param name the name of the use def completion.
    */
  case class UseDefCompletion(name: String) extends Completion

  /**
    * Represents a Use Enum Tag completion
    *
    * @param enumSym the sym of the enum.
    * @param caze    the case of the enum.
    */
  case class UseEnumTagCompletion(enumSym: EnumSym, caze: TypedAst.Case) extends Completion

  /**
    * Represents a Use Op completion.
    *
    * @param name the name of the use op completion.
    */
  case class UseOpCompletion(name: String) extends Completion

  /**
    * Represents a Use Signature completion.
    *
    * @param name the name of the use signature completion.
    */
  case class UseSignatureCompletion(name: String) extends Completion

  /**
    * Represents an EnumTag completion
    *
    * @param enumSym the sym of the enum.
    * @param cas     the case (for that specific enum).
    */
  case class EnumTagCompletion(enumSym: EnumSym, cas: TypedAst.Case) extends Completion

  /**
   * Represents a struct field completion.
   *
   * @param field the candidate field.
   */
  case class StructFieldCompletion(field: String, symLoc: SourceLocation, tpe: Type) extends Completion

  /**
    * Represents a Module completion.
    *
    * @param modSym the module symbol.
    */
  case class ModCompletion(modSym: ModuleSym) extends Completion

  /**
   * Represents a Java field completion.
   *
   * @param ident  the partial field name.
   * @param field the candidate field.
   */
  case class FieldCompletion(ident: Name.Ident, field: Field) extends Completion

  /**
    * Represents a Java method completion.
    *
    * @param ident  the partial method name.
    * @param method the candidate method.
    */
  case class MethodCompletion(ident: Name.Ident, method: Method) extends Completion

  /**
    * Represents a hole completion.
    *
    * @param sym      the variable symbol being completed on.
    * @param decl     the proposed def declaration to call.
    * @param priority the priority of the completion (multiple suggestions are possible and they are ranked).
    * @param loc      the source location of the hole.
    */
  case class HoleCompletion(sym: Symbol.VarSym, decl: TypedAst.Def, priority: String, loc: SourceLocation) extends Completion

  /**
    * Returns a TextEdit that is inserted and indented according to the given `ap`.
    * This function will:
    *   - add leadingSpaces before the given text.
    *   - add leadingSpaces after each newline.
    *   - add a newline at the end.
    *
    * Example:
    *   Given text = "\ndef foo(): =\n", ap = AnchorPosition(line=1, col=0, spaces=4)
    *   The result will be:
    *   TextEdit(Range(Position(1, 0), Position(1, 0)), "    \n    def foo(): =\n    \n")
    */
  private def mkTextEdit(ap: AnchorPosition, text: String): TextEdit = {
    val insertPosition = Position(ap.line, ap.col)
    val leadingSpaces = " " * ap.spaces
    TextEdit(
      Range(insertPosition, insertPosition),
      leadingSpaces + text.replace("\n", s"\n$leadingSpaces") + "\n"
    )
  }
}
