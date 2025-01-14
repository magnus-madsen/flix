/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.api.lsp.provider.completion.syntactic.{KeywordCompleter, ExprSnippetCompleter}
import ca.uwaterloo.flix.language.ast.TypedAst

object ExprCompleter {

  def getCompletions(context: CompletionContext)(implicit flix: Flix, root: TypedAst.Root): Iterable[Completion] = {
      LabelCompleter.getCompletions(context) ++
      KeywordCompleter.getExprKeywords ++
      SignatureCompleter.getCompletions(context) ++
      EnumTagCompleter.getCompletions(context) ++
      ExprSnippetCompleter.getCompletions() ++
      ModuleCompleter.getCompletions(context) ++
      HoleCompletion.getHoleCompletion(context, root) ++
      OpCompleter.getCompletions(context)
  }

}
