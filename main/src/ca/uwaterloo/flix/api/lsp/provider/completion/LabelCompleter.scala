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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.LabelCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object LabelCompleter extends Completer {

  /**
    * Returns a list of [[LabelCompletion]]s.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[LabelCompletion] = {
    // Do not get label completions if we are importing or using.
    println("Entered LabelCompleter")
    if (context.prefix.contains("import") || context.prefix.contains("use")) {
      return Nil
    }

    println("Not in import or use context")

    val regex = raw"(.*)#.*".r

    println(s"context.word is: ${if (context.word.isBlank) "<the empty string>" else context.word}")
    println(s"regex matches word: ${regex.matches(context.word)}")
    println(s"regex matches prefix: ${regex.matches(context.prefix)}")

    context.word match {
      case regex(prefix) if isFirstCharLowerCase(prefix) =>
        println("Entered good suggestion path")
        index.labelDefs.m.concat(index.labelUses.m)
          .filter { case (_, locs) => locs.exists(loc => loc.source.name == context.uri) }
          .map {
            case (label, _) =>
              Completion.LabelCompletion(label, prefix)
          }
      case regex(prefix) =>
        println(s"case regex(prefix) => $prefix")
        Nil
      case _ =>
        println("Entered Nil suggestion path")
        Nil
    }
  }

  /**
    * Returns true if the first char of the string is lowerCase, false otherwise.
    */
  private def isFirstCharLowerCase(str: String): Boolean = str.headOption.exists(_.isLower)
}
