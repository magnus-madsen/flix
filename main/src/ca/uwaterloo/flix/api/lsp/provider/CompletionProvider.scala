/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.api.lsp.provider.completion._
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.{ParseError, ResolutionError, TypeError, WeederError}
import ca.uwaterloo.flix.language.fmt.FormatScheme
import ca.uwaterloo.flix.language.phase.Lexer
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

/**
  * CompletionProvider
  *
  * Takes a source file, along with the position of the cursor within that file, and returns a list of CompletionItems.
  *
  * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
  *
  * This list is not displayed to the user as-is, the client always both sorts and filters the list (or at least,
  * VSCode does). Therefore we always need to provide both filterText (currently copied from label) and sortText.
  *
  * Note that we use textEdit rather than insertText to avoid relying on VSCode's tokenisation, so we can ensure that
  * we're consistent with Flix's parser.
  */
object CompletionProvider {

  /**
    * Process a completion request.
    */
  def autoComplete(uri: String, pos: Position, source: Option[String], currentErrors: List[CompilationMessage])(implicit flix: Flix, index: Index, root: TypedAst.Root): JObject = {
    val holeCompletions = getHoleExpCompletions(pos, uri, index, root)
    // If we are currently on a hole the only useful completion is a hole completion.
    if (holeCompletions.nonEmpty) {
      return ("status" -> ResponseStatus.Success) ~ ("result" -> CompletionList(isIncomplete = false, holeCompletions).toJSON)
    }

    //
    // To the best of my knowledge, completions should never be Nil. It could only happen if source was None
    // (what would having no source even mean?) or if the position represented by pos was invalid with respect
    // to the source (which would imply a bug in VSCode?).
    //
    val completions = source.flatMap(getContext(_, uri, pos, currentErrors)) match {
      case None => Nil
      case Some(context) =>
        // Get all completions
        val completions = getCompletions()(context, flix, index, root)
        completions.map(comp => comp.toCompletionItem(context))
    }

    ("status" -> ResponseStatus.Success) ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  /**
    * Gets completions for when the cursor position is on a hole expression with an expression
    */
  private def getHoleExpCompletions(pos: Position, uri: String, index: Index, root: TypedAst.Root)(implicit flix: Flix): Iterable[CompletionItem] = {
    val entity = index.query(uri, pos)
    entity match {
      case Some(Entity.Exp(TypedAst.Expr.HoleWithExp(TypedAst.Expr.Var(sym, sourceType, _), targetType, _, loc))) =>
        HoleCompletion.candidates(sourceType, targetType, root)
          .map(root.defs(_))
          .filter(_.spec.mod.isPublic)
          .zipWithIndex
          .map { case (decl, idx) => holeDefCompletion(f"$idx%09d", loc, sym, decl) }
      case _ => Nil
    }
  }

  /**
    * Creates a completion item from a hole with expression and a def.
    */
  private def holeDefCompletion(priority: String, loc: SourceLocation, sym: Symbol.VarSym, decl: TypedAst.Def)(implicit flix: Flix): CompletionItem = {
    val name = decl.sym.toString
    val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    } ::: sym.text :: Nil
    val params = args.mkString(", ")
    val snippet = s"$name($params)"
    CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      filterText = Some(s"${sym.text}?$name"),
      sortText = priority,
      textEdit = TextEdit(Range.from(loc), snippet),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Function)
  }

  private def getCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
    context.sctx match {
      //
      // Expressions.
      //
      case SyntacticContext.Expr.Constraint => PredicateCompleter.getCompletions(context)
      case SyntacticContext.Expr.Do => OpCompleter.getCompletions(context)
      case SyntacticContext.Expr.InvokeMethod(tpe, name) => InvokeMethodCompleter.getCompletions(tpe, name, context)
      case SyntacticContext.Expr.StaticFieldOrMethod(e) => GetStaticFieldCompleter.getCompletions(e) ++ InvokeStaticMethodCompleter.getCompletions(e)
      case SyntacticContext.Expr.StructAccess(e) => StructFieldCompleter.getCompletions(e, root)
      case _: SyntacticContext.Expr => ExprCompleter.getCompletions(context)

      //
      // Declarations.
      //
      case SyntacticContext.Decl.Trait => KeywordCompleter.getTraitKeywords
      case SyntacticContext.Decl.Enum => KeywordCompleter.getEnumKeywords
      case SyntacticContext.Decl.Instance => InstanceCompleter.getCompletions(context)
      case _: SyntacticContext.Decl => KeywordCompleter.getDeclKeywords ++ SnippetCompleter.getCompletions(context)

      //
      // Imports.
      //
      case SyntacticContext.Import => ImportCompleter.getCompletions(context)

      //
      // Types.
      //
      case SyntacticContext.Type.Eff => EffSymCompleter.getCompletions(context)
      case SyntacticContext.Type.OtherType => TypeCompleter.getCompletions(context) ++ EffSymCompleter.getCompletions(context)

      //
      // Patterns.
      //
      case _: SyntacticContext.Pat => ModuleCompleter.getCompletions(context) ++
        EnumCompleter.getCompletions(context) ++ EnumTagCompleter.getCompletions(context)

      //
      // Uses.
      //
      case SyntacticContext.Use => UseCompleter.getCompletions(context)

      //
      // With.
      //
      case SyntacticContext.WithClause =>
        // A with context could also be just a type context.
        TypeCompleter.getCompletions(context) ++ WithCompleter.getCompletions(context)

      //
      // Fallthrough.
      //
      case SyntacticContext.Unknown =>
        KeywordCompleter.getOtherKeywords ++ SnippetCompleter.getCompletions(context)
    }
  }

  /**
    * Characters that constitute a word.
    */
  private def isWordChar(c: Char) = isLetter(c) || Lexer.isMathNameChar(c) || Lexer.isGreekNameChar(c) || Lexer.isUserOp(c).isDefined

  /**
    * Characters that may appear in a word.
    */
  private def isLetter(c: Char) = c match {
      case c if c >= 'a' && c <= 'z' => true
      case c if c >= 'A' && c <= 'Z' => true
      case c if c >= '0' && c <= '9' => true
      // We also include some special symbols. This is more permissive than the lexer, but that's OK.
      case '_' => true
      case '!' => true
      case '@' => true
      case '/' => true
      case '.' => true
      case '#' => true
      case _ => false
    }

  /**
    * Returns the word at the end of a string, discarding trailing whitespace first
    */
  private def getLastWord(s: String): String = {
    s.reverse.dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Returns the second-to-last word at the end of a string, *not* discarding
    * trailing whitespace first.
    */
  private def getSecondLastWord(s: String): String = {
    s.reverse.dropWhile(isWordChar).dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getContext(source: String, uri: String, pos: Position, errors: List[CompilationMessage]): Option[CompletionContext] = {
    val x = pos.character - 1
    val y = pos.line - 1
    val lines = source.linesWithSeparators.toList
    for (line <- lines.slice(y, y + 1).headOption) yield {
      val (prefix, suffix) = line.splitAt(x)
      val wordStart = prefix.reverse.takeWhile(isWordChar).reverse
      val wordEnd = suffix.takeWhile(isWordChar)
      val word = wordStart + wordEnd
      val start = x - wordStart.length
      val end = x + wordEnd.length
      val prevWord = getSecondLastWord(prefix)
      val previousWord = if (prevWord.nonEmpty) {
        prevWord
      } else lines.slice(y - 1, y).headOption match {
        case None => ""
        case Some(s) => getLastWord(s)
      }
      val range = Range(Position(y, start), Position(y, end))
      val sctx = getSyntacticContext(uri, pos, errors)
      CompletionContext(uri, pos, range, sctx, word, previousWord, prefix, errors)
    }
  }

  /**
    * Optionally returns the syntactic context from the given list of errors.
    *
    * We have to check that the syntax error occurs after the position of the completion.
    */
  private def getSyntacticContext(uri: String, pos: Position, errors: List[CompilationMessage]): SyntacticContext =
    errors.filter({
      case err => pos.line <= err.loc.beginLine
    }).map({
      // We can have multiple errors, so we rank them, and pick the highest priority.
      case WeederError.UnqualifiedUse(_) => (1, SyntacticContext.Use)
      case ResolutionError.UndefinedJvmClass(_, _, _) => (1, SyntacticContext.Import)
      case ResolutionError.UndefinedName(_, _, _, isUse, _) => if (isUse) (1, SyntacticContext.Use) else (2, SyntacticContext.Expr.OtherExpr)
      case ResolutionError.UndefinedNameUnrecoverable(_, _, _, isUse, _) => if (isUse) (1, SyntacticContext.Use) else (2, SyntacticContext.Expr.OtherExpr)
      case ResolutionError.UndefinedType(_, _, _) => (1, SyntacticContext.Type.OtherType)
      case ResolutionError.UndefinedTag(_, _, _) => (1, SyntacticContext.Pat.OtherPat)
      case ResolutionError.UndefinedOp(_, _) => (1, SyntacticContext.Expr.Do)
      case WeederError.MalformedIdentifier(_, _) => (2, SyntacticContext.Import)
      case WeederError.UnappliedIntrinsic(_, _) => (5, SyntacticContext.Expr.OtherExpr)
      case err: ResolutionError.UndefinedJvmStaticField => (1, SyntacticContext.Expr.StaticFieldOrMethod(err))
      case err: TypeError.MethodNotFound => (1, SyntacticContext.Expr.InvokeMethod(err.tpe, err.methodName))
      case err: TypeError.FieldNotFound => (1, SyntacticContext.Expr.InvokeMethod(err.tpe, err.fieldName))
      case err: TypeError.UndefinedStructField => (1, SyntacticContext.Expr.StructAccess(err))
      case err: ParseError => (5, err.sctx)
      case _ => (999, SyntacticContext.Unknown)
    }).minByOption(_._1) match {
      case None => SyntacticContext.Unknown
      case Some((_, sctx)) => sctx
    }

}
