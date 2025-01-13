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
import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.api.lsp.provider.completion.*
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.getNamespaceAndIdentFromQName
import ca.uwaterloo.flix.api.lsp.provider.completion.semantic.{GetStaticFieldCompleter, InvokeStaticMethodCompleter}
import ca.uwaterloo.flix.api.lsp.provider.completion.syntactic.{ExprSnippetCompleter, KeywordCompleter}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SyntacticContext
import ca.uwaterloo.flix.language.errors.{ParseError, ResolutionError, TypeError, WeederError}
import ca.uwaterloo.flix.language.phase.Lexer
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

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

  def autoComplete(uri: String, pos: Position, source: String, currentErrors: List[CompilationMessage])(implicit flix: Flix, root: TypedAst.Root): JObject = {
    val completionItems = getCompletionContext(source, uri, pos, currentErrors).map {ctx =>
      errorsAt(ctx.uri, ctx.pos, currentErrors).flatMap({
        case WeederError.UnqualifiedUse(_) => UseCompleter.getCompletions(ctx)
        case WeederError.UndefinedAnnotation(_, _) => KeywordCompleter.getModKeywords ++ ExprSnippetCompleter.getCompletions()
        case ResolutionError.UndefinedUse(_, _, _, _) => UseCompleter.getCompletions(ctx)
        case ResolutionError.UndefinedTag(_, _, _, _) => ModuleCompleter.getCompletions(ctx) ++ EnumTagCompleter.getCompletions(ctx)
        case err: ResolutionError.UndefinedName =>
          val (namespace, ident) = getNamespaceAndIdentFromQName(err.qn)
          AutoImportCompleter.getCompletions(err) ++
            LocalScopeCompleter.getCompletions(err) ++
            AutoUseCompleter.getCompletions(err) ++
            ExprCompleter.getCompletions(ctx) ++
            DefCompleter.getCompletions(err, namespace, ident) ++
            EnumCompleter.getCompletions(err, namespace, ident)
        case err: ResolutionError.UndefinedType =>
          val (namespace, ident) = getNamespaceAndIdentFromQName(err.qn)
          AutoImportCompleter.getCompletions(err) ++
            LocalScopeCompleter.getCompletions(err) ++
            AutoUseCompleter.getCompletions(err) ++
            EffSymCompleter.getCompletions(err) ++
            TypeCompleter.getCompletions(ctx) ++
            EnumCompleter.getCompletions(err, namespace, ident) ++
            StructCompleter.getCompletions(err, namespace, ident)
        case err: ResolutionError.UndefinedJvmStaticField => GetStaticFieldCompleter.getCompletions(err) ++ InvokeStaticMethodCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedJvmClass => ImportCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedTrait =>
          val (namespace, ident) = getNamespaceAndIdentFromQName(err.qn)
          TraitCompleter.getCompletions(err, namespace, ident)
        case err: ResolutionError.UndefinedStructField => StructFieldCompleter.getCompletions(err, root)
        case err: ResolutionError.UndefinedKind => KindCompleter.getCompletions(err)
        case err: TypeError.FieldNotFound => MagicMatchCompleter.getCompletions(err) ++ InvokeMethodCompleter.getCompletions(err.tpe, err.fieldName)
        case err: TypeError.MethodNotFound => InvokeMethodCompleter.getCompletions(err.tpe, err.methodName)
        case err: ParseError => err.sctx match {
          // Expressions.
          case SyntacticContext.Expr.Constraint => PredicateCompleter.getCompletions(ctx) ++ KeywordCompleter.getConstraintKeywords
          case _: SyntacticContext.Expr => ExprCompleter.getCompletions(ctx)

          // Declarations.
          case SyntacticContext.Decl.Enum => KeywordCompleter.getEnumKeywords
          case SyntacticContext.Decl.Instance => InstanceCompleter.getCompletions(ctx) ++ KeywordCompleter.getInstanceKeywords
          case SyntacticContext.Decl.Module => KeywordCompleter.getModKeywords ++ ExprSnippetCompleter.getCompletions()
          case SyntacticContext.Decl.Struct => KeywordCompleter.getStructKeywords
          case SyntacticContext.Decl.Trait => KeywordCompleter.getTraitKeywords
          case SyntacticContext.Decl.Type => KeywordCompleter.getTypeKeywords

          // Types.
          case SyntacticContext.Type.OtherType => TypeCompleter.getCompletions(ctx)

          // Patterns.
          case _: SyntacticContext.Pat => ModuleCompleter.getCompletions(ctx) ++ EnumTagCompleter.getCompletions(ctx)

          // Uses.
          case SyntacticContext.Use => UseCompleter.getCompletions(ctx)

          // With.
          case SyntacticContext.WithClause =>
            // A with context could also be just a type context.
            TypeCompleter.getCompletions(ctx) ++ WithCompleter.getCompletions(ctx)

          // Try-with handler.
          case SyntacticContext.WithHandler => WithHandlerCompleter.getCompletions(ctx)

          // Unknown syntactic context. The program could be correct-- in which case it is hard to offer suggestions.
          case SyntacticContext.Unknown =>
            // Special case: A program with a hole is correct, but we should offer some completion suggestions.
            HoleCompletion.getHoleCompletion(ctx, root)

          case _ => Nil
        }

        case _ => HoleCompletion.getHoleCompletion(ctx, root)
      }).map(comp => comp.toCompletionItem(ctx))
    }.getOrElse(Nil)
    ("status" -> ResponseStatus.Success) ~ ("result" -> CompletionList(isIncomplete = true, completionItems).toJSON)
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getCompletionContext(source: String, uri: String, pos: Position, errors: List[CompilationMessage]): Option[CompletionContext] = {
    // Use zero-indexed lines and characters.
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
      // Remember positions are one-indexed.
      val range = Range(Position(y + 1, start + 1), Position(y + 1, end + 1))
      CompletionContext(uri, pos, range, word, previousWord, prefix)
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
    * Filters the list of errors to only those that occur at the given position.
    */
  private def errorsAt(uri: String, pos: Position, errors: List[CompilationMessage]): List[CompilationMessage] =
    errors.filter(err => uri == err.loc.source.name && pos.line <= err.loc.beginLine)

}
