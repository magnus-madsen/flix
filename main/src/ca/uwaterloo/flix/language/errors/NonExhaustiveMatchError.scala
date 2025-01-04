/*
 * Copyright 2017 Jason Mitterteiner
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

sealed trait PatMatchError extends CompilationMessage

/**
  * An error raised to indicate a non-exhaustive pattern match expression.
  */
case class NonExhaustiveMatchError(pat: String, loc: SourceLocation) extends PatMatchError {
  val kind = "Pattern Match"

  def summary: String = s"Non-exhaustive match. Missing case: '$pat'."

  def message(formatter: Formatter): String = {
    import formatter.*
    s""">> Non-Exhaustive Pattern. Missing case: ${red(pat)} in match expression.
       |
       |${code(loc, "incomplete pattern.")}
       |""".stripMargin
  }

  override def explain(formatter: Formatter): Option[String] = Some({
    s"""Flix requires every pattern match expression to be exhaustive, i.e. to cover all
       |possible cases. A wild card pattern, written with an underscore, can be used to
       |handle all other cases. For example:
       |
       |    case _ => // handle all other cases.
       |
       |""".stripMargin
  })

}

/**
  * An error raised to indicate a non-exhaustive pattern match expression.
  */
case class UselessPatternError(loc: SourceLocation) extends PatMatchError {
  val kind = "Pattern Match"

  def summary: String = s"Useless pattern."

  def message(formatter: Formatter): String = {
    import formatter.*
    s""">> Useless pattern. This case is unreachable.
       |
       |${code(loc, "unreachable case.")}
       |""".stripMargin
  }

  override def explain(formatter: Formatter): Option[String] = None // MATT say to remove it or something

}
