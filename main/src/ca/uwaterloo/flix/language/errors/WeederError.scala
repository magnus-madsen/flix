/*
 * Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation}

/**
 * A common super-type for weeding errors.
 */
sealed trait WeederError extends CompilationMessage {
  val kind = "Syntax Error"
}

object WeederError {

  /**
   * An error raised to indicate that the annotation `name` was used multiple times.
   *
   * @param name the name of the attribute.
   * @param loc1 the location of the first annotation.
   * @param loc2 the location of the second annotation.
   */
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Multiple occurrences of the annotation '${Format.red("@" + name)}'.
         |
         |${Format.code(loc1, "the first occurrence was here.")}
         |
         |${Format.code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain: String = s"${Format.underline("Tip:")} Remove one of the two annotations."

  }

  /**
   * An error raised to indicate that the formal parameter `name` was declared multiple times.
   *
   * @param name the name of the parameter.
   * @param loc1 the location of the first parameter.
   * @param loc2 the location of the second parameter.
   */
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple declarations of the formal parameter '$name'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Multiple declarations of the formal parameter '${Format.red(name)}'.
         |
         |${Format.code(loc1, "the first declaration was here.")}
         |
         |${Format.code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain: String = s"${Format.underline("Tip:")} Remove or rename one of the formal parameters to avoid the name clash."

  }

  /**
   * An error raised to indicate that the modifier `name` was used multiple times.
   *
   * @param name the name of the modifier.
   * @param loc1 the location of the first modifier.
   * @param loc2 the location of the second modifier.
   */
  case class DuplicateModifier(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate modifier '$name'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Multiple occurrences of the modifier '${Format.red(name)}'.
         |
         |${Format.code(loc1, "the first occurrence was here.")}
         |
         |${Format.code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

  }

  /**
   * An error raised to indicate that the tag `name` was declared multiple times.
   *
   * @param enumName the name of the enum.
   * @param tag      the name of the tag.
   * @param loc1     the location of the first tag.
   * @param loc2     the location of the second tag.
   */
  case class DuplicateTag(enumName: String, tag: Name.Tag, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate tag: '$tag'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Multiple declarations of the tag '${Format.red(tag.name)}' in the enum '${Format.cyan(enumName)}'.
         |
         |${Format.code(loc1, "the first declaration was here.")}
         |
         |${Format.code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain: String = s"${Format.underline("Tip:")} Remove or rename one of the tags to avoid the name clash."

  }

  /**
   * An error raised to indicate an illegal array length.
   *
   * @param loc the location where the illegal array length occurs.
   */
  case class IllegalArrayLength(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal array length"

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal array length.
         |
         |${Format.code(loc, "illegal array length.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal field name.
   *
   * @param loc the location where the illegal field name occurs.
   */
  case class IllegalFieldName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal field name"

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |
         |>> Illegal field name.
         |
         |${Format.code(loc, "illegal field name.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate that the formal parameter lacks a type declaration.
   *
   * @param name the name of the parameter.
   * @param loc  the location of the formal parameter.
   */
  case class IllegalFormalParameter(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = "The formal parameter must have a declared type."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |
         |>> The formal parameter '${Format.red(name)}' must have a declared type.
         |
         |${Format.code(loc, "has no declared type.")}
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Explicitly declare the type of the formal parameter."

  }

  /**
   * An error raised to indicate an illegal existential quantification expression.
   *
   * @param loc the location where the illegal expression occurs.
   */
  case class IllegalExistential(loc: SourceLocation) extends WeederError {
    def summary: String = "The existential quantifier does not declare any formal parameters."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> The existential quantifier does not declare any formal parameters.
         |
         |${Format.code(loc, "quantifier must declare at least one parameter.")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Add a formal parameter or remove the quantifier."

  }

  /**
   * An error raised to indicate an illegal universal quantification expression.
   *
   * @param loc the location where the illegal expression occurs.
   */
  case class IllegalUniversal(loc: SourceLocation) extends WeederError {
    def summary: String = "The universal quantifier does not declare any formal parameters."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> The universal quantifier does not declare any formal parameters.
         |
         |${Format.code(loc, "quantifier must declare at least one parameter.")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Add a formal parameter or remove the quantifier."

  }

  /**
   * An error raised to indicate that a float is out of bounds.
   *
   * @param loc the location where the illegal float occurs.
   */
  case class IllegalFloat(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal float."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal float.
         |
         |${Format.code(loc, "illegal float.")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Ensure that the literal is within bounds."

  }

  /**
   * An error raised to indicate that an int is out of bounds.
   *
   * @param loc the location where the illegal int occurs.
   */
  case class IllegalInt(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal int."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal int.
         |
         |${Format.code(loc, "illegal int.")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Ensure that the literal is within bounds."

  }

  /**
   * An error raised to indicate an illegal intrinsic.
   *
   * @param loc the location where the illegal intrinsic occurs.
   */
  case class IllegalIntrinsic(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal intrinsic"

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal intrinsic.
         |
         |${Format.code(loc, "illegal intrinsic.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal modifier.
   *
   * @param loc the location where the illegal modifier occurs.
   */
  case class IllegalModifier(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal modifier."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal modifier.
         |
         |${Format.code(loc, "illegal modifier.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal null pattern.
   *
   * @param loc the location where the illegal pattern occurs.
   */
  case class IllegalNullPattern(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal null pattern"

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal null pattern.
         |
         |${Format.code(loc, "illegal null pattern.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal jvm field or method name.
   *
   * @param loc the location of the name.
   */
  case class IllegalJvmFieldOrMethodName(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal jvm field or method name."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal jvm field or method name.
         |
         |${Format.code(loc, "illegal name.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal wildcard in an expression.
   *
   * @param loc the location where the illegal wildcard occurs.
   */
  case class IllegalWildcard(loc: SourceLocation) extends WeederError {
    def summary: String = "Wildcard not allowed here."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Wildcard not allowed here.
         |
         |${Format.code(loc, "illegal wildcard.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate a mismatched arity.
   *
   * @param expected the expected arity.
   * @param actual   the actual arity.
   * @param loc      the location where mismatch occurs.
   */
  case class MismatchedArity(expected: Int, actual: Int, loc: SourceLocation) extends WeederError {
    def summary: String = s"Mismatched arity: expected: $expected, actual: $actual."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Mismatched arity: expected: $expected, actual: $actual.
         |
         |${Format.code(loc, "mismatched arity.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
   *
   * @param name the name of the variable.
   * @param loc1 the location of the first use of the variable.
   * @param loc2 the location of the second use of the variable.
   */
  case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple occurrences of '$name' in pattern."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Multiple occurrences of '${Format.red(name)}'  in pattern.
         |
         |${Format.code(loc1, "the first occurrence was here.")}
         |
         |${Format.code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def loc: SourceLocation = loc1 min loc2

    override def explain: String = s"${Format.underline("Tip:")} A variable may only occur once in a pattern."

  }

  /**
   * An error raised to indicate an undefined annotation.
   *
   * @param name the name of the undefined annotation.
   * @param loc  the location of the annotation.
   */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Undefined annotation $name"

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Undefined annotation '${Format.red(name)}'.
         |
         |${Format.code(loc, "undefined annotation.")}
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an illegal private declaration.
   *
   * @param ident the name of the declaration.
   * @param loc   the location where the error occurred.
   */
  case class IllegalPrivateDeclaration(ident: Name.Ident, loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal private declaration '${ident.name}'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal private declaration '${Format.red(ident.name)}'.
         |
         |${Format.code(loc, "illegal private declaration")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Mark the declaration as 'pub'."

  }

  /**
   * An error raised to indicate an illegal type constraint parameter.
   *
   * @param loc the location where the error occurred.
   */
  case class IllegalTypeConstraintParameter(loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal type constraint parameter."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Illegal type constraint parameter.
         |
         |${Format.code(loc, "illegal type constraint parameter")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Type constraint parameters must be composed only of type variables."

  }

  /**
   * An error raised to indicate type params where some (but not all) are explicitly kinded.
   *
   * @param loc the location where the error occurred.
   */
  case class InconsistentTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Inconsistent type parameters.
         |
         |${Format.code(loc, "inconsistent type parameters")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Either all or none of the type parameters must be annotated with a kind."

  }

  /**
   * An error raised to indicate type params that are not kinded.
   *
   * @param loc the location where the error occurred.
   */
  case class UnkindedTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Type parameters here must be annotated with a kind."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Unkinded type parameters.
         |
         |${Format.code(loc, "unkinded type parameters")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Type parameters here must be annotated with a kind."

  }

  /**
   * An error raised to indicate a malformed unicode escape sequence.
   *
   * @param code the escape sequence
   * @param loc  the location where the error occurred.
   */
  case class MalformedUnicodeEscapeSequence(code: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed unicode escape sequence '$code'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Malformed unicode escape sequence.
         |
         |${Format.code(loc, "malformed unicode escape sequence")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")}" + " A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal."

  }

  /**
   * An error raised to indicate an invalid escape sequence.
   *
   * @param char the invalid escape character.
   * @param loc  the location where the error occurred.
   */
  case class InvalidEscapeSequence(char: Char, loc: SourceLocation) extends WeederError {
    def summary: String = s"Invalid escape sequence '\\$char'."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Invalid escape sequence.
         |
         |${Format.code(loc, "invalid escape sequence")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")}" + " The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\n', and '\\r'."

  }

  /**
   * An error raised to indicate a non-single character literal.
   *
   * @param chars the characters in the character literal.
   * @param loc   the location where the error occurred.
   */
  case class NonSingleCharacter(chars: String, loc: SourceLocation) extends WeederError {
    def summary: String = "Non-single-character literal."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Non-single-character literal.
         |
         |${Format.code(loc, "non-single-character literal")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} A character literal must consist of a single character."

  }

  /**
   * An error raised to indicate an empty interpolated expression (`"${}"`)
   *
   * @param loc the location where the error occurred.
   */
  case class EmptyInterpolatedExpression(loc: SourceLocation) extends WeederError {
    def summary: String = "Empty interpolated expression."

    def message: String = {
      s"""${Format.line(kind, source.format)}
         |>> Empty interpolated expression.
         |
         |${Format.code(loc, "empty interpolated expression")}
         |
         |""".stripMargin
    }

    override def explain: String = s"${Format.underline("Tip:")} Add an expression to the interpolation or remove the interpolation."

  }
}