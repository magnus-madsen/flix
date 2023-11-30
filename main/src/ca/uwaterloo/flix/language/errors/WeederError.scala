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
import ca.uwaterloo.flix.util.Formatter

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
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of the annotation '${red("@" + name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove one of the two annotations."
    })

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate that the formal parameter `name` was declared multiple times.
    *
    * @param name the name of the parameter.
    * @param loc1 the location of the first parameter.
    * @param loc2 the location of the second parameter.
    */
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Multiple declarations of the formal parameter '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple declarations of the formal parameter '${red(name)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove or rename one of the formal parameters to avoid the name clash."
    })

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate that the modifier `name` was used multiple times.
    *
    * @param name the name of the modifier.
    * @param loc1 the location of the first modifier.
    * @param loc2 the location of the second modifier.
    */
  case class DuplicateModifier(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Duplicate modifier '$name'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of the modifier '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None

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
  case class DuplicateTag(enumName: String, tag: Name.Ident, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate tag: '$tag'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple declarations of the tag '${red(tag.name)}' in the enum '${cyan(enumName)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Remove or rename one of the tags to avoid the name clash."
    })

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate that a loop does not contain any fragments.
    *
    * @param loc the location of the for-loop with no fragments.
    */
  case class EmptyForFragment(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "A loop must iterate over some collection."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Loop does not iterate over any collection.
         |
         |${code(loc, "Loop does not iterate over any collection.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""A loop must contain a collection comprehension.
         |
         |A minimal loop is written as follows:
         |
         |    foreach (x <- xs) yield x
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an empty interpolated expression (`"${}"`)
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyInterpolatedExpression(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Empty interpolated expression."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Empty interpolated expression.
         |
         |${code(loc, "empty interpolated expression")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Add an expression to the interpolation or remove the interpolation."
    })

  }

  /**
    * An error raised to indicate that a record pattern has shape the illegal shape `{ | r }`.
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyRecordExtensionPattern(loc: SourceLocation) extends WeederError with Unrecoverable {
    override def summary: String = "A record pattern must specify at least one field."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected record pattern.
         |
         |${code(loc, "A record pattern must specify at least one field.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
   * An error raised to indicate that an inner function is annotated with an illegal annotation.
   *
   * @param loc the location of the illegal annotation.
   */
  case class IllegalAnnotation(loc: SourceLocation) extends WeederError with Recoverable {
    override def summary: String = "Unexpected annotation on inner function."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected annotation on inner function.
         |
         |${code(loc, "unexpected annotation")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal effect set member.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectSetMember(loc: SourceLocation) extends WeederError with Unrecoverable {
    override def summary: String = "Illegal effect set member."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal effect set member.
         |
         |${code(loc, s"Effect sets may only contain variables and constants.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that type parameters are present on an effect or operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectTypeParams(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Unexpected effect type parameters."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected effect type parameters.
         |
         |${code(loc, "unexpected effect type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type parameters are not allowed on effects."
    })
  }

  /**
    * An error raised to indicate an enum using both singleton and multiton syntaxes.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEnum(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Unexpected enum format."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected enum format.
         |
         |${code(loc, "unexpected enum format")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""This enum uses both the singleton syntax and the case syntax.
         |
         |Only one of the enum forms may be used.
         |If you only need one case for the enum, use the singleton syntax:
         |
         |    enum E(Int32)
         |
         |If you need multiple cases, use the case syntax:
         |
         |    enum E {
         |        case C1(Int32)
         |        case C2(Bool)
         |    }
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an invalid escape sequence.
    *
    * @param char the invalid escape character.
    * @param loc  the location where the error occurred.
    */
  case class IllegalEscapeSequence(char: Char, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Invalid escape sequence '\\$char'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Invalid escape sequence.
         |
         |${code(loc, "invalid escape sequence")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")}" + " The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\${', '\\n', and '\\r'."
    })

  }

  /**
    * An error raised to indicate an ill-formed equality constraint.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEqualityConstraint(loc: SourceLocation) extends WeederError with Unrecoverable {
    override def summary: String = "Illegal equality constraint."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal equality constraint.
         |
         |${code(loc, s"Equality constraints must have the form: `Assoc[var] ~ Type`.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a loop does not iterate over any collection.
    *
    * @param loc the location of the for-loop in which the for-fragment appears.
    */
  case class IllegalForFragment(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "A foreach expression must start with a collection comprehension."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Loop does not start with collection comprehension.
         |
         |${code(loc, "Loop does not start with collection comprehension.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""A loop must start with collection comprehension where the collection
         |has an instance of the Iterable type class on it.
         |
         |A minimal loop is written as follows:
         |
         |    foreach (x <- xs) yield x
         |
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an illegal ascription on a formal parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalFormalParamAscription(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Unexpected type ascription. Type ascriptions are not permitted on effect handler cases."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected type ascription. Type ascriptions are not permitted on effect handler cases.
         |
         |${code(loc, "unexpected type ascription")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a negative atom is marked as fixed.
    *
    * @param loc the location where the illegal fixed atom occurs.
    */
  case class IllegalFixedAtom(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Illegal fixed atom"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |>> Illegal fixed atom. A negative atom is implicitly fixed.
         |
         |${code(loc, "Illegal fixed atom.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that an imported Java name is not a valid Flix identifier
    */
  case class IllegalJavaClass(name: String, loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "${name} is not a valid Flix identifier."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> ${red(name)} is not a valid Flix identifier.
         |
         |${code(loc, "identifier")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""Not every valid Java identifier is a valid Flix identifier.
         |
         |If you need to use such a class or interface, alias it during import, e.g.:
         |
         |    import java.util.{Locale$$Builder => Builder}
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an illegal jvm field or method name.
    *
    * @param loc the location of the name.
    */
  case class IllegalJavaFieldOrMethodName(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Illegal jvm field or method name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal jvm field or method name.
         |
         |${code(loc, "illegal name.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal modifier.
    *
    * @param loc the location where the illegal modifier occurs.
    */
  case class IllegalModifier(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Illegal modifier."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal modifier.
         |
         |${code(loc, "illegal modifier.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the name of a module does not begin with an uppercase symbol.
    *
    * @param name the part of the module name that does not begin with an uppercase symbol.
    * @param loc  the location where the error occurred
    */
  case class IllegalModuleName(name: String, loc: SourceLocation) extends WeederError with Recoverable {

    override def summary: String = s"Module name '$name' does not begin with an uppercase letter."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Lowercase module name.
         |
         |${code(loc, s"Module name '$name' does not begin with an uppercase letter.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "A module name must begin with an uppercase letter."
    })
  }

  /**
    * An error raised to indicate an illegal null pattern.
    *
    * @param loc the location where the illegal pattern occurs.
    */
  case class IllegalNullPattern(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Illegal null pattern"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal null pattern.
         |
         |${code(loc, "illegal null pattern.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal effect on an effect operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalOperationEffect(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Unexpected effect. Effect operations may not themselves have effects."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected effect. Effect operations may not themselves have effects.
         |
         |${code(loc, "unexpected effect")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal private declaration.
    *
    * @param ident the name of the declaration.
    * @param loc   the location where the error occurred.
    */
  case class IllegalPrivateDeclaration(ident: Name.Ident, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Declaration must be public: '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Declaration must be public: '${red(ident.name)}'.
         |
         |${code(loc, "illegal private declaration")}
         |
         |Mark the declaration as public with `pub'.
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

  }

  /**
    * An error raised to indicate that the extension of a record pattern is malformed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRecordExtensionPattern(loc: SourceLocation) extends WeederError with Unrecoverable {
    override def summary: String = "A record extension must be either a variable or wildcard."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected record extension pattern.
         |
         |${code(loc, "A record extension must be either a variable or wildcard.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal regex pattern.
    *
    * @param loc the location where the illegal regex pattern occurs.
    */
  case class IllegalRegexPattern(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Illegal regex pattern"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal regex pattern.
         |
         |${code(loc, "regex not allowed here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} A regex cannot be used as a pattern. It can be used in an `if` guard, e.g using `isMatch` or `isSubmatch`."
    })

  }

  /**
    * An error raised to indicate a use of resume outside an effect handler.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalResume(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Unexpected use of 'resume'. The 'resume' expression must occur in an effect handler."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected use of 'resume'. The 'resume' expression must occur in an effect handler.
         |
         |${code(loc, "unexpected use of 'resume'")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

  }

  /**
    * An error raised to indicate an illegal type constraint parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalTypeConstraintParameter(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = s"Illegal type constraint parameter."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal type constraint parameter.
         |
         |${code(loc, "illegal type constraint parameter")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type constraint parameters must be composed only of type variables."
    })

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param fromName the original name
    * @param toName   the alias
    * @param loc      the location where the error occurred
    */
  case class IllegalUseAlias(fromName: String, toName: String, loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = s"The case of '$fromName' does not match the case of '$toName'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Mismatched alias case.
         |
         |${code(loc, s"The case of '$fromName' does not match the case of '$toName'.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""An alias must match the case of the name it replaces.
         |
         |If a name is lowercase, the alias must be lowercase.
         |If a name is uppercase, the alias must be uppercase.
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate the presence of a guard in a restrictable choice rule.
    *
    * @param star whether the choose is of the star kind.
    * @param loc  the location where the error occurs.
    */
  case class IllegalRestrictableChooseGuard(star: Boolean, loc: SourceLocation) extends WeederError with Unrecoverable {
    private val operationName: String = if (star) "choose*" else "choose"

    def summary: String = s"cases of $operationName do not allow guards."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> $summary
         |
         |${code(loc, "Disallowed guard.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class MalformedChar(chars: String, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Malformed, non-single-character literal."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Malformed, non-single-character literal.
         |
         |${code(loc, "non-single-character literal")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} A character literal must consist of a single character."
    })

  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class MalformedFloat(loc: SourceLocation) extends WeederError with Recoverable  {
    def summary: String = "Malformed float."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Malformed float.
         |
         |${code(loc, "malformed float.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class MalformedInt(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Malformed int."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Malformed int.
         |
         |${code(loc, "malformed int.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param pat the invalid regular expression
    * @param loc the location where the error occurred
    */
  case class MalformedRegex(pat: String, err: String, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Malformed regular expression."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Malformed regular expression.
         |
         |${code(loc, "malformed regex.")}
         |
         |Pattern compilation error:
         |$err
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"A pattern literal must be a valid regular expression."
    })
  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param code the escape sequence
    * @param loc  the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(code: String, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Malformed unicode escape sequence."

    def message(formatter: Formatter): String = {
      import formatter.{line, code => fmtcode}
      s"""${line(kind, source.name)}
         |>> Malformed unicode escape sequence.
         |
         |${fmtcode(loc, "malformed unicode escape sequence")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")}" + " A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal."
    })

  }

  /**
    * An error raised to indicate type params where some (but not all) are explicitly kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class MismatchedTypeParameters(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Inconsistent type parameters.
         |
         |${code(loc, "inconsistent type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Either all or none of the type parameters must be annotated with a kind."
    })

  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class MissingFormalParamAscription(name: String, loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Missing type ascription. Type ascriptions are required for parameters here."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |>> The formal parameter '${red(name)}' must have a declared type.
         |
         |${code(loc, "has no declared type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None

  }

  /**
    * An error raised to indicate a mismatched arity.
    *
    * @param expected the expected arity.
    * @param actual   the actual arity.
    * @param loc      the location where mismatch occurs.
    */
  case class MismatchedArity(expected: Int, actual: Int, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Mismatched arity: expected: $expected, actual: $actual."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Mismatched arity: expected: $expected, actual: $actual.
         |
         |${code(loc, "mismatched arity.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that the variable `name` occurs multiple times in the same pattern.
    *
    * @param name the name of the variable.
    * @param loc1 the location of the first use of the variable.
    * @param loc2 the location of the second use of the variable.
    */
  case class NonLinearPattern(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = s"Multiple occurrences of '$name' in pattern."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Multiple occurrences of '${red(name)}' in pattern.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} A variable may only occur once in a pattern."
    })

    def loc: SourceLocation = loc1 min loc2

  }

  /**
    * An error raised to indicate a non-Unit return type of an effect operation.
    *
    * @param loc the location where the error occurred.
    */
  case class NonUnitOperationType(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Non-Unit return type. All effect operations must return Unit."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Non-Unit return type. All effect operations must return Unit.
         |
         |${code(loc, "non-Unit return type")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a non-unary associated type.
    *
    * @param numParams the number of parameters of the associated type.
    * @param loc       the location where the error occurred.
    */
  case class NonUnaryAssocType(numParams: Int, loc: SourceLocation) extends WeederError with Unrecoverable {
    override def summary: String = "Non-unary associated type signature."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Non-unary associated type signature.
         |
         |${code(loc, s"Associated types must have exactly one parameter, but $numParams are given here.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate that a newly defined name is reserved.
    *
    * @param ident the reserved name that conflicts.
    * @param loc   the location where the error occurred.
    */
  case class ReservedName(ident: Name.Ident, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Re-definition of a reserved name."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Re-definition of reserved name '${red(ident.name)}'.
         |
         |${code(loc, "re-definition of a reserved name")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Try to find a new name that doesn't match one that is reserved."
    })

  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Undefined annotation $name"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Undefined annotation '${red(name)}'.
         |
         |${code(loc, "undefined annotation.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class UndefinedIntrinsic(loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = "Illegal intrinsic"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal intrinsic.
         |
         |${code(loc, "illegal intrinsic.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate type params that are not kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class UnkindedTypeParameters(loc: SourceLocation) extends WeederError with Unrecoverable {
    def summary: String = "Type parameters here must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unkinded type parameters.
         |
         |${code(loc, "unkinded type parameters")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} Type parameters here must be annotated with a kind."
    })

  }

  /**
    * An error raised to indicate an unsupported restrictable choice rule pattern.
    *
    * @param star whether the choose is of the star kind.
    * @param loc  the location where the error occurs.
    */
  case class UnsupportedRestrictedChoicePattern(star: Boolean, loc: SourceLocation) extends WeederError with Unrecoverable {
    private val operationName: String = if (star) "choose*" else "choose"

    def summary: String = s"Unsupported $operationName pattern, only enums with variables are allowed."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> $summary
         |
         |${code(loc, "Unsupported pattern.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    def explain(formatter: Formatter): Option[String] = None
  }

}
