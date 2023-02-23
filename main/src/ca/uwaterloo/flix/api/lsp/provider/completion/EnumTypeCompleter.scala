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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumTypeCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.TypedAst

object EnumTypeCompleter extends Completer {
  /**
    * Returns a List of Completion for enum types.
    */
  override def getCompletions(implicit context: CompletionContext, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[EnumTypeCompletion] = {
    if (root.isEmpty) {
      return Nil
    }

    root.get.enums.collect {
      case (_, t) if !t.ann.isInternal =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)
        Completion.EnumTypeCompletion(s"$name${formatTParams(t.tparams)}", priorityBoostForTypes(internalPriority(name)),
          TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"), Some(t.doc.text))
    }
  }
}
