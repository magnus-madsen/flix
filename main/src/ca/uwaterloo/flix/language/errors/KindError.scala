/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation}
import ca.uwaterloo.flix.language.fmt.FormatKind.formatKind
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for kind errors.
  */
sealed trait KindError extends CompilationMessage {
  val kind: String = "Kind Error"
}

object KindError {

  /**
    * An error raised to indicate two incompatible kinds.
    *
    * @param k1  the first kind.
    * @param k2  the second kind.
    * @param loc the location where the error occurred.
    */
  case class MismatchedKinds(k1: Kind, k2: Kind, loc: SourceLocation) extends KindError with Unrecoverable {
    override def summary: String = s"Mismatched kinds: '${formatKind(k1)}' and '${formatKind(k2)}''"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> This type variable was used as both kind '${red(formatKind(k1))}' and kind '${red(formatKind(k2))}'.
         |
         |${code(loc, "mismatched kind.")}
         |
         |Kind One: ${cyan(formatKind(k1))}
         |Kind Two: ${magenta(formatKind(k2))}
         |""".stripMargin
    }
  }

  /**
    * An error describing a kind that doesn't match the expected kind.
    *
    * @param expectedKind the expected kind.
    * @param actualKind   the actual kind.
    * @param loc          the location where the error occurred.
    */
  case class UnexpectedKind(expectedKind: Kind, actualKind: Kind, loc: SourceLocation) extends KindError with Unrecoverable {
    override def summary: String = s"Kind ${formatKind(expectedKind)} was expected, but found ${formatKind(actualKind)}."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Expected kind '${red(formatKind(expectedKind))}' here, but kind '${red(formatKind(actualKind))}' is used.
         |
         |${code(loc, "unexpected kind.")}
         |
         |Expected kind: ${cyan(formatKind(expectedKind))}
         |Actual kind:   ${magenta(formatKind(actualKind))}
         |""".stripMargin
    }
  }

  /**
    * An error resulting from a type whose kind cannot be inferred.
    *
    * @param loc The location where the error occurred.
    */
  case class UninferrableKind(loc: SourceLocation) extends KindError with Recoverable {
    override def summary: String = "Unable to infer kind."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to infer kind.
         |
         |${code(loc, "uninferred kind.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip: ")} Add a kind annotation."
    })
  }

  // JOE TODO: Test error message quality of
  // bad parsing (e.g. new Struct {a = 3 @ rc
  // Nonexistent struct(e.g. new NonExistentStruct {a = 3} @ rc
  /**
   * An error raised to indicate a `new` struct expression provides too many fields
   *
   * @param fields the names of the extra fields
   * @param loc the location where the error occurred.
   */
  case class ExtraStructFields(fields: Set[String], loc: SourceLocation) extends KindError with Unrecoverable {
    override def summary: String = s"`new` struct expression provides too many fields"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> `new` struct expression provides fields not present in original declaration of struct type
         |
         |${code(loc, "extra fields")}
         |
         |Extra Fields: ${fields.init.foldLeft("")((field, acc) => acc + field + ", " ) + fields.last}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate a `new` struct expression is missing fields
   *
   * @param fields the names of the missing fields
   * @param loc the location where the error occurred.
   */
  case class UnprovidedStructFields(fields: Set[String], loc: SourceLocation) extends KindError with Unrecoverable {
    override def summary: String = s"`new` struct expression provides too few fields"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> `new` struct expression does not provide fields present in original declaration of struct type
         |
         |${code(loc, "missing fields")}
         |
         |Missing Fields: ${fields.init.foldLeft("")((field, acc) => acc + field + ", " ) + fields.last}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate a struct is missing a required field in `struct.field` or `struct.field = value` expression
   *
   * @param fields the names of the missing fields
   * @param loc the location where the error occurred.
   */
  case class NonExistentStructField(field: String, loc: SourceLocation) extends KindError with Unrecoverable {
    override def summary: String = s"Struct is missing a field"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> struct expression does not provide a field named ${field}
         |
         |${code(loc, "nonexistent field")}
         |""".stripMargin
    }
  }
}
