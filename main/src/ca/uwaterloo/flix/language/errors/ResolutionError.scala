/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, UnkindedType}
import ca.uwaterloo.flix.util.{Formatter, Grammar}

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.ResolutionError
}

object ResolutionError {

  /**
    * An error raise to indicate a cycle in the trait hierarchy.
    *
    * @param path the super trait path from a trait to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTraitHierarchy(path: List[Symbol.TraitSym], loc: SourceLocation) extends ResolutionError {
    private val fullCycle = path.last :: path

    override def summary: String = {
      val pathString = fullCycle.map(clazz => s"'${clazz.name}'").mkString(" extends ")
      "Cyclic inheritance: " + pathString
    }

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s"""${code(loc, "cyclic inheritance.")}
         |
         |The following traits are in the cycle:
         |
         |$cyclicTraits
         |""".stripMargin
    }

    private def cyclicTraits: String = {
      var res = ""
      for (case List(subTrait, superTrait) <- fullCycle.sliding(2)) {
        res += s"$subTrait extends $superTrait" + System.lineSeparator()
      }
      res
    }
  }

  /**
    * An error raise to indicate a cycle in type aliases.
    *
    * @param path the type reference path from a type alias to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTypeAliases(path: List[Symbol.TypeAliasSym], loc: SourceLocation) extends ResolutionError {
    private val fullCycle = path.last :: path

    def summary: String = {
      val pathString = fullCycle.map(alias => s"'${alias.name}'").mkString(" references ")
      "Cyclic type aliases: " + pathString
    }

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s"""${code(loc, "Cyclic type aliases.")}
         |
         |The following type aliases are in the cycle:
         |$appendCycles
         |""".stripMargin
    }

    private def appendCycles: String = {
      var res = ""
      for (case List(referrer, referee) <- fullCycle.sliding(2)) {
        res += s"$referrer references $referee" + System.lineSeparator()
      }
      res
    }
  }

  /**
    * An error raised to indicate a duplicate associated type definition.
    *
    * @param sym  the associated type symbol.
    * @param loc1 the location of the first associated type definition.
    * @param loc2 the location of the second associated type definition.
    */
  case class DuplicateAssocTypeDef(sym: Symbol.AssocTypeSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    override def summary: String = s"Duplicate associated type definition: $sym."

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Duplicate associated type definition: ${red(sym.name)}.
         |
         |${code(loc2, "duplicate associated type definition.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None

    val loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate a duplicate derivation.
    *
    * @param sym  the trait symbol of the duplicate derivation.
    * @param loc1 the location of the first occurrence.
    * @param loc2 the location of the second occurrence.
    */
  case class DuplicateDerivation(sym: Symbol.TraitSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    override def summary: String = s"Duplicate derivation: ${sym.name}"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Duplicate derivation '${red(sym.name)}'.
         |
         |${code(loc1, "the first occurrence was here.")}
         |
         |${code(loc2, "the second occurrence was here.")}
         |
         |""".stripMargin
    }

    override def loc: SourceLocation = loc1

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Remove one of the occurrences."
    })

  }

  /**
    * An error raised to indicate a `new` struct expression provides an extra unknown field.
    *
    * @param sym   the symbol of the struct.
    * @param field the name of the extra field.
    * @param loc   the location where the error occurred.
    */
  case class ExtraStructFieldInNew(sym: Symbol.StructSym, field: Name.Label, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Unexpected field '$field' in new struct expression"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Unexpected field '${red(field.toString)}' in new struct expression.
         |
         |>> The struct '${cyan(sym.toString)}' does not declare a '${red(field.toString)}' field.
         |
         |${code(loc, "unexpected field")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an associated type application is not allowed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalAssocTypeApplication(loc: SourceLocation) extends ResolutionError {
    override def summary: String = " Illegal associated type application."

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Illegal associated type application.
         |
         |${code(loc, "illegal associated type application.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "An associated type may only be applied to a variable."
    })
  }

  /**
    * An error raised to indicate a `new` struct expression initializes its fields in the wrong order.
    *
    * @param providedFields the order in which fields were initialized.
    * @param expectedFields the order in which fields were declared.
    * @param loc            the location where the error occurred
    */
  case class IllegalFieldOrderInNew(sym: Symbol.StructSym, providedFields: List[Name.Label], expectedFields: List[Name.Label], loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Struct fields must be initialized in their declaration order"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Struct fields must be initialized in their declaration order.
         |
         |Expected: ${expectedFields.mkString(", ")}
         |Actual  : ${providedFields.mkString(", ")}
         |
         |${code(loc, "incorrect order")}
         |""".stripMargin
    }
  }

  /**
    * Illegal Non-Java Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalNonJavaType(tpe: UnkindedType, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Illegal non-Java type. Expected class or interface type."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Unexpected non-Java type: '${red(tpe.toString)}'.
         |
         |${code(loc, "unexpected type.")}
         |
         |Expected a Java class or interface.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a signature does not include the class's type parameter.
    *
    * @param sym the symbol of the signature.
    * @param loc the location where the error occurred.
    */
  case class IllegalSignature(sym: Symbol.SigSym, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Unexpected signature which does not mention the type variable of the class."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Unexpected signature '${red(sym.name)}' which does not mention the type variable of the class.
         |
         |${code(loc, "unexpected signature.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "Every signature in a type class must mention the type variable of the class."
    })

  }

  /**
    * An error raised to indicate that a wildcard type is used in an illegal position.
    *
    * @param ident the name of the wildcard type.
    * @param loc   the location where the error occurred.
    */
  case class IllegalWildType(ident: Name.Ident, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Illegal wildcard type: '$ident'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Illegal wildcard type: '$ident'.
         |
         |${code(loc, "illegal wildcard type.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "Wildcard types (types starting with an underscore) are not allowed in this position."
    })
  }

  /**
    * An error raised to indicate a `put` struct expression attempts to modify an immutable field.
    *
    * @param field the immutable field.
    * @param loc   the location where the error occurred.
    */
  case class ImmutableField(field: Symbol.StructFieldSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Modification of immutable field `${field.name}`."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Modification of immutable field '${red(field.name)}' on ${cyan(field.structSym.toString)}'.
         |
         |${code(loc, "immutable field")}
         |
         |Mark the field as 'mut' in the declaration of the struct.
         |""".stripMargin
    }
  }

  /**
    * Inaccessible Trait Error.
    *
    * @param sym the trait symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTrait(sym: Symbol.TraitSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Trait '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible trait.")}
         |
         |""".stripMargin

    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the trait as public."
    })
  }

  /**
    * Inaccessible Def Error.
    *
    * @param sym the def symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleDef(sym: Symbol.DefnSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible definition.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Effect Error.
    *
    * @param sym the effect symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEffect(sym: Symbol.EffectSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Inaccessible alias ${sym.name}"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Effect '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible effect.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the effect as public."
    })

  }

  /**
    * Inaccessible Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEnum(sym: Symbol.EnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible enum.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Struct Error
    *
    * @param sym the struct symbol
    * @param ns  the namespace where the symbol is not accessible
    * @param loc the location where the error occurred
    */
  case class InaccessibleStruct(sym: Symbol.StructSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Struct '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible struct.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the definition as public."
    })
  }

  /**
    * Inaccessible Restrictable Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleRestrictableEnum(sym: Symbol.RestrictableEnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible enum.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Sig Error.
    *
    * @param sym the sig symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleSig(sym: Symbol.SigSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible definition.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the definition as public."
    })

  }

  /**
    * Inaccessible Type Alias Error.
    *
    * @param sym the type alias symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTypeAlias(sym: Symbol.TypeAliasSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Inaccessible type alias ${sym.name}"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Type alias '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${code(loc, "inaccessible type alias.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Mark the type alias as public."
    })

  }

  /**
    * An error indicating the number of effect operation arguments does not match the expected number.
    *
    * @param op       the effect operation symbol.
    * @param expected the expected number of arguments.
    * @param actual   the actual number of arguments.
    * @param loc      the location where the error occurred.
    */
  case class MismatchedOpArity(op: Symbol.OpSym, expected: Int, actual: Int, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Expected ${Grammar.n_things(expected, "parameter")} but found $actual."

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Mismatched arity.
         |
         |The operation $op expects ${Grammar.n_things(expected, "parameter")},
         |but ${Grammar.n_are(actual)} provided here.
         |
         |${code(loc, s"expected ${Grammar.n_things(expected, "parameter")} but found $actual")}
         |""".stripMargin
    }
  }

  /**
    * An error indicating that the number of arguments to a tag-pattern is mismatched.
    *
    * @param caze     the enum case symbol.
    * @param expected the expected number of terms.
    * @param actual   the actual number of patterns.
    * @param loc      the location where the error occurred.
    */
  case class MismatchedTagPatternArity(caze: Symbol.CaseSym, expected: Int, actual: Int, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Expected ${Grammar.n_things(expected, "argument")} but found $actual actual ${Grammar.n_things(expected, "argument")}."

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""" Expected ${Grammar.n_things(expected, "argument")} for ${cyan(caze.toString)} but found ${Grammar.n_things(actual, "argument")}.
         |
         |${code(loc, s"mismatched arguments")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a missing associated type definition.
    *
    * @param name the name of the missing associated type definition.
    * @param loc  the location of the instance symbol where the error occurred.
    */
  case class MissingAssocTypeDef(name: String, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Missing associated type definition: $name."

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Missing associated type definition: $name.
         |
         |${code(loc, s"missing associated type definition: $name.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a handler is missing a definition.
    *
    * @param sym the symbol of the missing definition.
    * @param loc the location where the error occurred.
    */
  case class MissingHandlerDef(sym: Symbol.OpSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Missing handler definition: ${sym.name}"

    override def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Missing handler definition '${red(sym.name)}' for effect ${cyan(sym.eff.name)}'.
         |
         |${code(loc, "missing handler definition")}
         |
         |Add a handler definition for ${sym.name}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a `new` struct expression is missing a field.
    *
    * @param sym   the symbol of the struct.
    * @param field the name of the missing fields.
    * @param loc   the location where the error occurred.
    */
  case class MissingStructFieldInNew(sym: Symbol.StructSym, field: Name.Label, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Missing struct field '$field' in new struct expression"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Missing struct field '${red(field.toString)}' in new struct expression for struct '${cyan(sym.toString)}'.
         |
         |${code(loc, "missing field")}
         |""".stripMargin
    }
  }

  /**
    * Sealed Class Error.
    *
    * @param sym the class symbol.
    * @param ns  the namespace from which the class is sealed.
    * @param loc the location where the error occurred.
    */
  case class SealedTrait(sym: Symbol.TraitSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = "Sealed."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Class '${red(sym.toString)}' is sealed from the module '${cyan(ns.toString)}'.
         |
         |${code(loc, "sealed class.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Move the instance or sub class to the class's module."
    })

  }

  /**
    * Undefined associated type error.
    *
    * @param qn  associated type.
    * @param loc the location where the error occurred.
    */
  case class UndefinedAssocType(qn: Name.QName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined associated type: '$qn'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined associated type'${red(qn.toString)}'.
         |
         |${code(loc, "associated type not found.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent associated type?"
    })
  }

  /**
    * Undefined Effect Error.
    *
    * @param qn  the unresolved effect.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedEffect(qn: Name.QName, ap: AnchorPosition, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined effect '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined effect '${red(qn.toString)}'.
         |
         |${code(loc, "effect not found")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent effect?"
    })

  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param ap   the anchor position.
    * @param msg  the Java error message.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmClass(name: String, ap: AnchorPosition, msg: String, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined Java class: '$name'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined Java class '${red(name)}'.
         |
         |${code(loc, "undefined class.")}
         |
         |$msg
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = {
      if (raw".*\.[A-Z].*\.[A-Z].*".r matches name)
        Some(s"Static nested classes should be specified using '$$', e.g. java.util.Locale$$Builder")
      else
        None
    }
  }

  /**
    * An error raised to indicate that a static field name was not found.
    *
    * @param clazz the class name.
    * @param field the field name.
    * @param loc   the location of the field access.
    */
  case class UndefinedJvmStaticField(clazz: Class[?], field: Name.Ident, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined static field."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined static field '${red(field.name)}' in class '${cyan(clazz.getName)}'.
         |
         |${code(loc, "undefined static field.")}
         |""".stripMargin
    }
  }

  /**
    * Undefined Kind Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedKind(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined kind: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined kind '${red(qn.toString)}'.
         |
         |${code(loc, "undefined kind.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent kind?"
    })
  }

  /**
    * Undefined Name Error.
    *
    * @param qn    the unresolved name.
    * @param ap    the anchor position.
    * @param env   the variables in the scope.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedName(qn: Name.QName, ap: AnchorPosition, env: LocalScope, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined name: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined name '${red(qn.toString)}'.
         |
         |${code(loc, "name not found")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent definition?"
    })

  }

  /**
    * Undefined Name Error (unrecoverable).
    *
    * @param qn    the unresolved name.
    * @param ns    the current namespace.
    * @param env   the variables in the scope.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedNameUnrecoverable(qn: Name.QName, ns: Name.NName, env: LocalScope, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined name: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined name '${red(qn.toString)}'.
         |
         |${code(loc, "name not found")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent definition?"
    })

  }

  /**
    * Undefined Op Error.
    *
    * @param qname the qualified name of the operation.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedOp(qname: Name.QName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined operation '${qname.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined operation '${red(qname.toString)}'.
         |
         |${code(loc, "operation not found")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent operation?"
    })
  }

  /**
    * Undefined Restrictable Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRestrictableTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined restrictable tag: '$tag'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined restrictable tag '${red(tag)}'.
         |
         |${code(loc, "tag not found.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent tag?"
    })

  }

  /**
    * Undefined Restrictable Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRestrictableType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined restrictable type: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined restrictable type '${red(qn.toString)}'.
         |
         |${code(loc, "type not found.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent type?"
    })

  }

  /**
    * Undefined Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(tag: String, ap: AnchorPosition, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined tag: '$tag'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined tag '${red(tag)}'.
         |
         |${code(loc, "tag not found.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent tag?"
    })

  }

  /**
    * Undefined Class Error.
    *
    * @param qn  the unresolved class.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTrait(qn: Name.QName, ap: AnchorPosition, env: LocalScope, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined trait: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined trait '${red(qn.toString)}'.
         |
         |${code(loc, "trait not found")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent class?"
    })

  }

  /**
    * Undefined Type Error.
    *
    * @param qn  the name.
    * @param ap  the enclosing module.
    * @param loc the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, ap: AnchorPosition, env: LocalScope, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined type: '${qn.toString}'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined type '${red(qn.toString)}'.
         |
         |${code(loc, "type not found.")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Possible typo or non-existent type?"
    })

  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined type variable '$name'."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined type variable '${red(name)}'.
         |
         |${code(loc, "undefined type variable.")}
         |""".stripMargin

    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "Flix cannot find the type variable. Maybe there is a typo?"
    })
  }

  /**
    * Undefined Use Error (unrecoverable).
    *
    * @param qn    the unresolved name.
    * @param ns    the current namespace.
    * @param env   the variables in the scope.
    * @param loc   the location where the error occurred.
    */
  case class UndefinedUse(qn: Name.QName, ns: Name.NName, env: Map[String, Symbol.VarSym], loc: SourceLocation) extends ResolutionError {
    def summary: String = s"Undefined '${qn.toString}' use."

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined '${red(qn.toString)}' use.
         |
         |${code(loc, "name not found")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the associated type.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Under-applied associated type: ${sym.name}"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Under-applied associated type '${red(sym.name)}'.
         |
         |${code(loc, "Under-applied associated type.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Associated types must be fully applied."
    })

  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the type alias.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Under-applied type alias: ${sym.name}"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Under-applied type alias '${red(sym.name)}'.
         |
         |${code(loc, "Under-applied type alias.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Type aliases must be fully applied."
    })

  }

  /**
    * An error raised to indicate an undefined struct in a `new S { ... } @ r` expression.
    *
    * @param name the name of the undefined struct.
    * @param loc  the location where the error occurred.
    */
  case class UndefinedStruct(name: Name.QName, ap: AnchorPosition, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Undefined struct"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined struct '${red(name.toString)}'.
         |
         |${code(loc, "undefined struct")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an undefined struct field in a struct get or struct put expression.
    *
    * @param struct the optional symbol of the struct.
    * @param field  the name of the missing field.
    * @param loc    the location where the error occurred.
    */
  case class UndefinedStructField(struct: Option[Symbol.StructSym], field: Name.Label, loc: SourceLocation) extends ResolutionError {
    override def summary: String = s"Undefined struct field '$field'$structMessage"

    def message(formatter: Formatter): String = messageWithLink {
      import formatter.*
      s""">> Undefined struct field '${red(field.toString)}'$structMessage.
         |
         |${code(loc, "undefined field")}
         |""".stripMargin
    }

    private def structMessage: String = struct match {
      case Some(sym) => s" on struct '$sym'."
      case None => ""
    }
  }

}
