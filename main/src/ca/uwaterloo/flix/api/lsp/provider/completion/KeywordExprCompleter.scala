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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.KeywordCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object KeywordExprCompleter extends Completer {

  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[KeywordCompletion] =
    List(
      "and",
      "as",
      "def",
      "discard",
      "do",
      "else",
      "false",
      "forA",
      "forM",
      "force",
      "foreach",
      "from",
      "if",
      "inject",
      "into",
      "lazy",
      "let",
      "match",
      "new",
      "not",
      "or",
      "par",
      "query",
      "region",
      "select",
      "solve",
      "spawn",
      "struct",
      "true",
      "try",
      "typematch",
      "unsafe",
      "use",
      "without",
      "yield"
    ) map (name => Completion.KeywordCompletion(name))
}
