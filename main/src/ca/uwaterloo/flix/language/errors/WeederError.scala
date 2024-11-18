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
    * @param name the name of the annotation.
    * @param loc1 the location of the first annotation.
    * @param loc2 the location of the second annotation.
    */
  case class DuplicateAnnotation(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple occurrences of the annotation '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of the annotation '${red("@" + name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
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
  case class DuplicateFormalParam(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Multiple declarations of the formal parameter '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple declarations of the formal parameter '${red(name)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
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
  case class DuplicateModifier(name: String, loc1: SourceLocation, loc2: SourceLocation) extends WeederError {
    def summary: String = s"Duplicate modifier '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of the modifier '${red(name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate a struct contains duplicate fields
    *
    * @param structName the name of the struct
    * @param fieldName  the name of the field
    * @param field1Loc  the location of the first field
    * @param field2Loc  the location of the second field
    * @param loc        the location of the struct declaration
    */
  case class DuplicateStructField(structName: String, fieldName: String, field1Loc: SourceLocation, field2Loc: SourceLocation, loc: SourceLocation) extends WeederError {
    def summary: String = s"struct has duplicate fields"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Struct has duplicate fields
         |
         |${code(loc, "struct declaration has duplicate fields")}
         |
         |${code(field1Loc, "the first occurrence was here")}
         |
         |${code(field2Loc, "the second occurrence was here")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Remove one of the two fields."
    })
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
      import formatter.*
      s""">> Multiple declarations of the tag '${red(tag.name)}' in the enum '${cyan(enumName)}'.
         |
         |${code(loc1, "the first declaration was here.")}
         |
         |${code(loc2, "the second declaration was here.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Remove or rename one of the tags to avoid the name clash."
    })

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate that a loop does not contain any fragments.
    *
    * @param loc the location of the for-loop with no fragments.
    */
  case class EmptyForFragment(loc: SourceLocation) extends WeederError {
    def summary: String = "A loop must iterate over some collection."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop does not iterate over any collection.
         |
         |${code(loc, "Loop does not iterate over any collection.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
  case class EmptyInterpolatedExpression(loc: SourceLocation) extends WeederError {
    def summary: String = "Empty interpolated expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Empty interpolated expression.
         |
         |${code(loc, "empty interpolated expression")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Add an expression to the interpolation or remove the interpolation."
    })

  }

  /**
    * An error raised to indicate that a record pattern has shape the illegal shape `{ | r }`.
    *
    * @param loc the location where the error occurred.
    */
  case class EmptyRecordExtensionPattern(loc: SourceLocation) extends WeederError {
    override def summary: String = "A record pattern must specify at least one field."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record pattern.
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
  case class IllegalAnnotation(loc: SourceLocation) extends WeederError {
    override def summary: String = "Unexpected annotation on inner function."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected annotation on local function.
         |
         |${code(loc, "unexpected annotation")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some("Annotations are not allowed on local functions.")
  }

  /**
    * An error raised to indicate that type parameters are present on an effect or operation.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectTypeParams(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected effect type parameters."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected effect type parameters.
         |
         |${code(loc, "unexpected effect type parameters")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Type parameters are not allowed on effects."
    })
  }

  /**
    * An error raised to indicate an operation which itself has an effect.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEffectfulOperation(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected effect. Effect operations may not themselves have effects."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected effect. Effect operations may not themselves have effects.
         |
         |${code(loc, "unexpected effect")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an enum using both singleton and multiton syntaxes.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEnum(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected enum format."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected enum format.
         |
         |${code(loc, "unexpected enum format")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
    * An error raised to indicate an ill-formed equality constraint.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalEqualityConstraint(loc: SourceLocation) extends WeederError {
    override def summary: String = "Illegal equality constraint."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal equality constraint.
         |
         |${code(loc, s"Equality constraints must have the form: `Assoc[var] ~ Type`.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate an invalid escape sequence.
    *
    * @param char the invalid escape character.
    * @param loc  the location where the error occurred.
    */
  case class IllegalEscapeSequence(char: Char, loc: SourceLocation) extends WeederError {
    def summary: String = s"Invalid escape sequence '\\$char'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Invalid escape sequence.
         |
         |${code(loc, "invalid escape sequence")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")}" + " The valid escape sequences are '\\t', '\\\\', '\\\'', '\\\"', '\\${', '\\n', and '\\r'."
    })
  }

  /**
    * An error raised to indicate that a negative atom is marked as fixed.
    *
    * @param loc the location where the illegal fixed atom occurs.
    */
  case class IllegalFixedAtom(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal fixed atom"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal fixed atom. A negative atom is implicitly fixed.
         |
         |${code(loc, "Illegal fixed atom.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a loop does not iterate over any collection.
    *
    * @param loc the location of the for-loop in which the for-fragment appears.
    */
  case class IllegalForFragment(loc: SourceLocation) extends WeederError {
    def summary: String = s"A foreach expression must start with a collection comprehension."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop does not start with collection comprehension.
         |
         |${code(loc, "Loop does not start with collection comprehension.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
    * An error raised to indicate that a ForA-loop contains other ForFragments than Generators.
    *
    * @param loc the location of the for-loop in which the for-fragment appears.
    */
  case class IllegalForAFragment(loc: SourceLocation) extends WeederError {
    def summary: String = s"A forA loop may only contain comprehensions of the form `x <- xs`."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Loop contains bad for-comprehension.
         |
         |${code(loc, "Loop contains bad for-comprehension.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal ascription on a formal parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalFormalParamAscription(loc: SourceLocation) extends WeederError {
    def summary: String = "Unexpected type ascription. Type ascriptions are not permitted on effect handler cases."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type ascription. Type ascriptions are not permitted on effect handler cases.
         |
         |${code(loc, "unexpected type ascription")}
         |
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

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal modifier.
         |
         |${code(loc, "illegal modifier.")}
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

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal null pattern.
         |
         |${code(loc, "illegal null pattern.")}
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
    def summary: String = s"Declaration must be public: '${ident.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Declaration must be public: '${red(ident.name)}'.
         |
         |${code(loc, "illegal private declaration")}
         |
         |Mark the declaration as public with `pub'.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the extension of a record pattern is malformed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalRecordExtensionPattern(loc: SourceLocation) extends WeederError {
    override def summary: String = "A record extension must be either a variable or wildcard."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected record extension pattern.
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
  case class IllegalRegexPattern(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal regex pattern"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal regex pattern.
         |
         |${code(loc, "regex not allowed here.")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} A regex cannot be used as a pattern. It can be used in an `if` guard, e.g using `isMatch` or `isSubmatch`."
    })
  }

  /**
    * An error raised to indicate an illegal trait constraint parameter.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalTraitConstraintParameter(loc: SourceLocation) extends WeederError {
    def summary: String = s"Illegal type constraint parameter."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal type constraint parameter.
         |
         |${code(loc, "illegal type constraint parameter")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Type constraint parameters must be composed only of type variables."
    })

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param fromName the original name.
    * @param toName   the alias.
    * @param loc      the location where the error occurred.
    */
  case class IllegalUse(fromName: String, toName: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"The case of '$fromName' does not match the case of '$toName'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched alias case.
         |
         |${code(loc, s"The case of '$fromName' does not match the case of '$toName'.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""An alias must match the case of the name it replaces.
         |
         |If a name is lowercase, the alias must be lowercase.
         |If a name is uppercase, the alias must be uppercase.
         |""".stripMargin
    })
  }

  /**
    * An error raised to indicate an illegal qualified name.
    *
    * @param loc the location of the illegal qualified name.
    */
  case class IllegalQualifiedName(loc: SourceLocation) extends WeederError {
    override def summary: String = "Unexpected qualified name"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected qualified name. Java names must be imported, e.g., `import java.lang.Object`.
         |
         |${code(loc, "illegal qualified name")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error raised to indicate a non-single character literal.
    *
    * @param chars the characters in the character literal.
    * @param loc   the location where the error occurred.
    */
  case class MalformedChar(chars: String, loc: SourceLocation) extends WeederError {
    def summary: String = "Malformed, non-single-character literal."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed, non-single-character literal.
         |
         |${code(loc, "non-single-character literal")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} A character literal must consist of a single character."
    })

  }

  /**
    * An error raised to indicate that a float is out of bounds.
    *
    * @param loc the location where the illegal float occurs.
    */
  case class MalformedFloat(loc: SourceLocation) extends WeederError {
    def summary: String = "Malformed float."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed float.
         |
         |${code(loc, "malformed float.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate that a name is not a valid Flix identifier.
    */
  case class MalformedIdentifier(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed identifier: '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed identifier '${red(name)}'.
         |
         |${code(loc, "illegal identifier")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an int is out of bounds.
    *
    * @param loc the location where the illegal int occurs.
    */
  case class MalformedInt(loc: SourceLocation) extends WeederError {
    def summary: String = "Malformed int."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed int.
         |
         |${code(loc, "malformed int.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Ensure that the literal is within bounds."
    })

  }

  /**
    * An error raised to indicate that the case of an alias does not match the case of the original value.
    *
    * @param pat the invalid regular expression
    * @param loc the location where the error occurred
    */
  case class MalformedRegex(pat: String, err: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed regular expression."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Malformed regular expression.
         |
         |${code(loc, "malformed regex.")}
         |
         |Pattern compilation error:
         |$err
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"A pattern literal must be a valid regular expression."
    })
  }

  /**
    * An error raised to indicate a malformed unicode escape sequence.
    *
    * @param code the escape sequence
    * @param loc  the location where the error occurred.
    */
  case class MalformedUnicodeEscapeSequence(code: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Malformed unicode escape sequence."

    def message(formatter: Formatter): String = {
      s""">> Malformed unicode escape sequence.
         |
         |${formatter.code(loc, "malformed unicode escape sequence")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")}" + " A Unicode escape sequence must be of the form \\uXXXX where X is a hexadecimal."
    })
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

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched arity: expected: $expected, actual: $actual.
         |
         |${code(loc, "mismatched arity.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate type params where some (but not all) are explicitly kinded.
    *
    * @param loc the location where the error occurred.
    */
  case class MismatchedTypeParameters(loc: SourceLocation) extends WeederError {
    def summary: String = "Either all or none of the type parameters must be annotated with a kind."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Inconsistent type parameters.
         |
         |${code(loc, "inconsistent type parameters")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Either all or none of the type parameters must be annotated with a kind."
    })

  }

  /**
    * An error raised to indicate that the formal parameter lacks a type declaration.
    *
    * @param name the name of the parameter.
    * @param loc  the location of the formal parameter.
    */
  case class MissingFormalParamAscription(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = "Missing type ascription. Type ascriptions are required for parameters here."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The formal parameter '${red(name)}' must have a declared type.
         |
         |${code(loc, "has no declared type.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a type parameter is missing a kind.
    *
    * @param loc the location of the type parameter.
    */
  case class MissingTypeParamKind(loc: SourceLocation) extends WeederError {
    def summary: String = "Type parameter must be annotated with its kind."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing kind annotation. The type parameter must be annotated with its kind.
         |
         |${code(loc, "missing kind.")}
         |
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

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Multiple occurrences of '${red(name)}' in pattern.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |A variable may only occur once in a pattern.
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"""${underline("Tip:")} You can replace
         |
         |  case (x, x) => ...
         |
         |with a guard:
         |
         |  case (x, y) if x == y => ...
         |""".stripMargin
    })

    def loc: SourceLocation = loc1 min loc2

  }

  /**
    * An error raised to indicate a non-unary associated type.
    *
    * @param n   the number of parameters of the associated type.
    * @param loc the location where the error occurred.
    */
  case class NonUnaryAssocType(n: Int, loc: SourceLocation) extends WeederError {
    override def summary: String = "Non-unary associated type signature."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Associated types must have exactly one parameter, but $n are given here.
         |
         |${code(loc, s"too many parameters")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an undefined annotation.
    *
    * @param name the name of the undefined annotation.
    * @param loc  the location of the annotation.
    */
  case class UndefinedAnnotation(name: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Undefined annotation $name"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined annotation '${red(name)}'.
         |
         |${code(loc, "undefined annotation.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class UndefinedIntrinsic(loc: SourceLocation) extends WeederError {
    def summary: String = "Illegal intrinsic"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal intrinsic.
         |
         |${code(loc, "illegal intrinsic.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an unapplied intrinsic.
    *
    * @param intrinsic name of the intrinsic.
    * @param loc       the location where the illegal intrinsic occurs.
    */
  case class UnappliedIntrinsic(intrinsic: String, loc: SourceLocation) extends WeederError {
    def summary: String = s"Unapplied intrinsic '$intrinsic'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unapplied intrinsic '${red(intrinsic)}'.
         |
         |${code(loc, "unapplied intrinsic.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an invalid function call in a select rule.
    *
    * @param qname the name of the function being called
    */
  case class UnexpectedSelectChannelRuleFunction(qname: Name.QName) extends WeederError {

    val loc: SourceLocation = qname.loc

    override def summary: String = s"Unexpected channel function '$qname'."

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected channel function.
         |
         |${code(loc, s"select-rules must apply `Channel.recv` to the channel.")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an illegal intrinsic.
    *
    * @param loc the location where the illegal intrinsic occurs.
    */
  case class UnqualifiedUse(loc: SourceLocation) extends WeederError {
    def summary: String = "Unqualified use."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unqualified use.
         |
         |${code(loc, "unqualified use.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} A use must be qualified: It should have the form `use Foo.bar`"
    })
  }

  /**
    * An error raised to indicate an unsupported restrictable choice rule pattern.
    *
    * @param star whether the choose is of the star kind.
    * @param loc  the location where the error occurs.
    */
  case class UnsupportedRestrictedChoicePattern(star: Boolean, loc: SourceLocation) extends WeederError {
    private val operationName: String = if (star) "choose*" else "choose"

    def summary: String = s"Unsupported $operationName pattern, only enums with variables are allowed."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> $summary
         |
         |${code(loc, "Unsupported pattern.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an expression is nested too deeply.
    *
    * @param hint     Optional hint with more details about the error
    * @param loc      The source location of the expression that is nested too deeply.
    */
  case class NestedTooDeeply(hint: Option[String] = None, loc: SourceLocation) extends WeederError with Recoverable {
    def summary: String = s"Expression is nested too deeply."

    def message(fmt: Formatter): String = {
      val hintStr = hint.map(s"\nHint: " + _).getOrElse("")
      s""">> Expression is nested too deeply.
         |
         |${fmt.code(loc, s"Here")}$hintStr
         |""".stripMargin
    }
  }
}
