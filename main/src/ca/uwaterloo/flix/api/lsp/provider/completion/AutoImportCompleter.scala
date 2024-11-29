/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.AutoImportCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.shouldComplete
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope}
import ca.uwaterloo.flix.language.errors.ResolutionError

object AutoImportCompleter {

  /**
   * Returns a list of import completions to auto complete the class name and import the java class.
   *
   * Example:
   *  If we have an undefined name which is the prefix of an existing and unimported java class
   *
   *  {{{
   *    let s = Mat // undefined name error
   *  }}}
   *
   *  We propose to complete the name to `Math` and import the class `java.lang.Math`
   *
   *  {{{
   *    import java.lang.Math
   *    ...
   *    let s = Math
   *  }}}
   */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap, err.env)

  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap, err.env)

  /**
   * Gets completions from a java class prefix.
   *
   * Note: we will not propose completions for classes with a lowercase name.
   */
  private def javaClassCompletionsByClass(prefix: String, ap: AnchorPosition, env: LocalScope)(implicit root: Root): Iterable[AutoImportCompletion] = {
    if (!shouldComplete(prefix)) return Nil
    val availableClasses = root.availableClasses.byClass.m.filter(_._1.exists(_.isUpper))
    availableClasses.keys.filter(_.startsWith(prefix)).flatMap { className =>
      availableClasses(className).collect { case path if (!env.m.contains(className)) =>
        val completePath = path.mkString(".") + "." + className
        val label = s"$className (import $completePath)"
          AutoImportCompletion(label, className, completePath, ap, Some(completePath))
      }
    }
 }
}