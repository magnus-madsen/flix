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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.BroadEqualityConstraint
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.DocAst.Type.JvmConstructor
import ca.uwaterloo.flix.language.fmt.FormatEqualityConstraint.formatEqualityConstraint
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.util.{Formatter, Grammar}

import java.lang.reflect.{Method, Constructor};

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationMessage with Recoverable {
  val kind: String = "Type Error"
}

object TypeError {

  /**
    * Irreducible associated type error
    *
    * @param sym the associated type symbol.
    * @param tpe the argument to the associated type
    * @param loc the location where the error occurred.
    */
  case class IrreducibleAssocType(sym: Symbol.AssocTypeSym, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    private val assocType: Type = Type.AssocType(Ast.AssocTypeConstructor(sym, SourceLocation.Unknown), tpe, Kind.Wild, SourceLocation.Unknown)

    def summary: String = s"Irreducible associated type: ${formatType(assocType)}"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Irreducible associated type: ${formatType(assocType)}
         |
         |${code(loc, "irreducible associated type.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "Tip: Add an equality constraint to the function."
    })
  }

  /**
   * Java constructor not found type error.
   *
   * @param tpes    the types of the arguments.
   * @param methods a list of possible candidate constructors on the type of the receiver object.
   * @param renv    the rigidity environment.
   * @param loc     the location where the error occured.
   */
  case class ConstructorNotFound(clazz: Class[_], tpes: List[Type], constructors: List[JvmConstructor], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    // TODO INTEROP better comment, e.g., list possible constructors
    def summary: String = s"Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Java '${clazz.getName}' constructor not found")}
         |""".stripMargin
    }
  }

  /**
   * Java method not found type error.
   *
   * @param tpe  the type of the receiver object.
   * @param tpes the types of the arguments.
   * @param loc  the location where the error occurred.
   */
  case class MethodNotFound(methodName: Name.Ident, tpe: Type, tpes: List[Type], loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Java method '$methodName' in type '$tpe' with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Java method '$methodName' from type '${red(formatType(tpe, None))}' with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Java method '$methodName' not found")}
         |""".stripMargin
    }
  }

  /**
    * Java field not found type error.
    *
    * @param tpe  the type of the receiver object.
    * @param loc  the location where the error occurred.
    */
  case class FieldNotFound(fieldName: Name.Ident, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Java field '$fieldName' in type '$tpe' not found."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Java field '$fieldName' from type '${red(formatType(tpe, None))}' not found.
         |
         |${code(loc, s"Java field '$fieldName' not found")}
         |""".stripMargin
    }
  }

  /**
   * Static Java method not found type error.
   *
   * @param clazz   the Java class expected to contain the static method
   * @param tpes    the types of the arguments.
   * @param methods a list of possible candidate methods in the class.
   * @param renv    the rigidity environment.
   * @param loc     the location where the error occured.
   */
  case class StaticMethodNotFound(clazz: Class[_], methodName: Name.Ident, tpes: List[Type], methods: List[Method], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    // TODO INTEROP better comment, e.g., list possible methods
    def summary: String = s"Static Java method '$methodName' from class ${clazz.getName} with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Static Java method '$methodName' from class '${clazz.getName}' with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Static Java method '${methodName}' not found")}
         |""".stripMargin
    }
  }

  /**
   * Java ambiguous constructor type error.
   *
   * @param tpes    the types of the arguments.
   * @param constructors a list of possible candidate constructors on the type of the receiver object.
   * @param renv    the rigidity environment.
   * @param loc     the location where the error occured.
   */
  case class AmbiguousConstructor(clazz: Class[_], tpes: List[Type], constructors: List[Constructor[_]], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    // TODO INTEROP better comment with candidate constructors formatting
    def summary: String = s"Ambiguous Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")})."

    def message(formatter: Formatter): String = {
      import formatter._
      def constructorToStr(c: Constructor[_]) = {
        s"${c.getName}(${c.getParameterTypes.map(t => t.getName).mkString(", ")})"
      }
      s"""${line(kind, source.name)}
         |>> Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")}) is ambiguous.
         | Possible candidate constructors:
         |  ${constructors.map(m => constructorToStr(m)).mkString(", ")}
         |
         |${code(loc, s"Ambiguous Java '${clazz.getName}' constructor")}
         |""".stripMargin
    }
  }

  /**
   * Java ambiguous method type error.
   *
   * @param tpe0    the type of the receiver object.
   * @param tpes    the types of the arguments.
   * @param methods a list of possible candidate methods on the type of the receiver object.
   * @param renv    the rigidity environment.
   * @param loc     the location where the error occured.
   */
  case class AmbiguousMethod(methodName: Name.Ident, tpe0: Type, tpes: List[Type], methods: List[Method], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    // TODO INTEROP better comment with candidate methods formatting
    def summary: String = s"Ambiguous Java method '$methodName' in type '$tpe0' with arguments types (${tpes.mkString(", ")})."

    def message(formatter: Formatter): String = {
      import formatter._
      def methodToStr(m: Method) = {
        s"${m.getName}(${m.getParameterTypes.map(t => t.getName).mkString(", ")})"
      }
      s""">> Java method '$methodName' from type '${red(formatType(tpe0, Some(renv)))}' with arguments types (${tpes.mkString(", ")}) is ambiguous.
         | Possible candidate methods:
         |  ${methods.map(m => methodToStr(m)).mkString(", ")}
         |
         |${code(loc, s"Ambiguous Java method '${methodName}'")}
         |""".stripMargin
    }
  }

  /**
   * Java ambiguous static method type error.
   *
   * @param clazz   the Java class expected to contain the static method
   * @param tpes    the types of the arguments.
   * @param methods a list of possible candidate methods in the class
   * @param renv    the rigidity environment.
   * @param loc     the location where the error occured.
   */
  case class AmbiguousStaticMethod(clazz: Class[_], methodName: Name.Ident, tpes: List[Type], methods: List[Method], renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    // TODO INTEROP better comment with candidate methods formatting
    def summary: String = s"Ambiguous static Java method '$methodName' from class '${clazz.getName}' with arguments types (${tpes.mkString(", ")})."

    def message(formatter: Formatter): String = {
      import formatter._
      def methodToStr(m: Method) = {
        s"${m.getName}(${m.getParameterTypes.map(t => t.getName).mkString(", ")})"
      }
      s""">> Static Java method '$methodName' from class '${clazz.getName}' with arguments types (${tpes.mkString(", ")}) is ambiguous.
         | Possible candidate static methods:
         |  ${methods.map(m => methodToStr(m)).mkString(", ")}
         |
         |${code(loc, s"Ambiguous static Java method '${methodName}'")}
         |""".stripMargin
    }
  }

  /**
   * Unresolved constructor type error.
   * This is a dummy error used in Java constructor type reconstruction for InvokeConstructor2.
   */
  case class UnresolvedConstructor(loc: SourceLocation) extends TypeError with Recoverable {
    def summary: String = s"Unresolved constructor"
    def message(formatter: Formatter): String = s"Unresolved constructor"
  }

  /**
   * Unresolved method type error.
   * This is a dummy error used in Java method type reconstruction for InvokeMethod2.
   */
  case class UnresolvedMethod(loc: SourceLocation) extends TypeError with Recoverable {
    def summary: String = s"Unresolved method"

    def message(formatter: Formatter): String = s"Unresolved method"
  }

  /**
    * Unresolved field type error.
    * This is a dummy error used in Java field type reconstruction for GetField2.
    */
  case class UnresolvedField(loc: SourceLocation) extends TypeError with Recoverable {
    def summary: String = s"Unresolved field"

    def message(formatter: Formatter): String = s"Unresolved field"
  }


  /**
    * Mismatched Arity.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MismatchedArity(tpe1: Type, tpe2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the types '${formatType(tpe1, Some(renv))}' and '${formatType(tpe2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the types: '${red(formatType(tpe1, Some(renv)))}' and '${red(formatType(tpe2, Some(renv)))}'.
         |
         |${code(loc, "mismatched arity of types.")}
         |
         |""".stripMargin
    }
  }

  /**
    * Mismatched Pure and Effectful Arrows.
    *
    * @param baseType1 the first boolean formula.
    * @param baseType2 the second boolean formula.
    * @param fullType1 the first full type in which the first boolean formula occurs.
    * @param fullType2 the second full type in which the second boolean formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedArrowEffects(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Mismatched Pure and Effectful Functions."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Mismatched Pure and Effectful Functions.
         |
         |${code(loc, "mismatched pure and effectful functions.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Boolean Formulas.
    *
    * @param baseType1 the first boolean formula.
    * @param baseType2 the second boolean formula.
    * @param fullType1 the first full type in which the first boolean formula occurs.
    * @param fullType2 the second full type in which the second boolean formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedBools(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the Boolean formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the Boolean formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched Boolean formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Case Set Formulas.
    *
    * @param baseType1 the first case set formula.
    * @param baseType2 the second case set formula.
    * @param fullType1 the first full type in which the first case set formula occurs.
    * @param fullType2 the second full type in which the second case set formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedCaseSets(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the case set formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the case set formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched case set formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Effect Formulas.
    *
    * @param baseType1 the first effect formula.
    * @param baseType2 the second effect formula.
    * @param fullType1 the first full type in which the first effect formula occurs.
    * @param fullType2 the second full type in which the second effect formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedEffects(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the effect formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the effect formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched effect formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Types.
    *
    * @param baseType1 the first base type.
    * @param baseType2 the second base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedTypes(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the types '${formatType(fullType1, Some(renv))}' and '${formatType(fullType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the types: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv))}
         |Type Two: ${formatType(fullType2, Some(renv))}
         |""".stripMargin
    }
  }

  /**
    * Missing trait instance.
    *
    * @param trt  the trait of the instance.
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstance(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$trt' class for the type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> No instance of the '${cyan(trt.toString)}' class for the type '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }
  }

  /**
    * Missing trait instance for a function type.
    *
    * @param trt  the class of the instance.
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceArrow(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$trt' class for the function type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> No instance of the '${cyan(trt.toString)}' class for the ${magenta("function")} type '${red(formatType(tpe, Some(renv)))}'.
         |
         |>> Did you forget to apply the function to all of its arguments?
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }
  }

  /**
    * Missing `Eq` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceEq(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Equality is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Eq."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Equality is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Eq.
         |
         |${code(loc, s"missing Eq instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""To define equality on '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of Eq for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Eq for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Eq {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Missing `Order` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceOrder(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Order is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Order."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Order is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Order.
         |
         |${code(loc, s"missing Order instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""To define an order on '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of Order for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Order for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Eq, Order {
         |    case Red, Green, Blue
         |  }
         |
         |Note: To derive Order you must also derive Eq.
         |""".stripMargin
    })
  }

  /**
    * Missing `Sendable` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceSendable(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Sendable is not defined for '${formatType(tpe, Some(renv))}'. Define or derive instance of Sendable."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Sendable is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Sendable.
         |
         |${code(loc, s"missing Sendable instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""To mark '${formatType(tpe, Some(renv))}' as sendable, either:
         |
         |  (a) define an instance of Sendable for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Sendable for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Sendable {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Missing `ToString` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceToString(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"ToString is not defined for '${formatType(tpe, Some(renv))}'. Define or derive instance of ToString."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> ToString is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of ToString.
         |
         |${code(loc, s"missing ToString instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""To define a string representation of '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of ToString for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of ToString for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with ToString {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Unexpected non-record type error.
    *
    * @param tpe  the unexpected non-record type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class NonRecordType(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unexpected non-record type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unexpected non-record type: '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, "unexpected non-record type.")}
         |""".stripMargin
    }
  }

  /**
    * Unexpected non-schema type error.
    *
    * @param tpe  the unexpected non-schema type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class NonSchemaType(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unexpected non-schema type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unexpected non-schema type: '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, "unexpected non-schema type.")}
         |
         |""".stripMargin
    }
  }

  /**
    * Occurs Check.
    *
    * @param baseVar   the base type variable.
    * @param baseType  the base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class OccursCheck(baseVar: Type.Var, baseType: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the type variable '$baseVar' with the type '$baseType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unable to unify the type variable '${red(formatType(baseVar, Some(renv)))}' with the type '${red(formatType(baseType, Some(renv)))}'.
         |
         |>> The type variable occurs recursively within the type.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv))}
         |Type Two: ${formatType(fullType2, Some(renv))}
         |""".stripMargin
    }
  }

  /**
    * Unexpected type, but a checked type cast might work.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class PossibleCheckedTypeCast(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |
         |'${formatType(expected, Some(renv))}' appears to be assignable from '${formatType(inferred, Some(renv))}'.
         |Consider using 'checked_cast'?
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(
      s"""Flix does not support sub-typing nor sub-effecting.
         |
         |Nevertheless, 'checked_cast' is way to use sub-typing in a safe manner, for example:
         |
         |    let s = "Hello World";
         |    let o: ##java.lang.Object = checked_cast(s);
         |""".stripMargin
    )
  }

  /**
    * An error indicating that a region variable escapes its scope.
    *
    * @param rvar the region variable.
    * @param tpe  the type wherein the region variable escapes.
    * @param loc  the location where the error occurred.
    */
  case class RegionVarEscapes(rvar: Type.Var, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError with Recoverable {
    def summary: String = s"Region variable '${formatType(rvar)}' escapes its scope."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> The region variable '${red(formatType(rvar))}' escapes its scope.
         |
         |${code(loc, "region variable escapes.")}
         |
         |The escaping expression has type:
         |
         |  ${red(formatType(tpe))}
         |
         |which contains the region variable.
         |
         |The region variable was declared here:
         |
         |${code(rvar.loc, "region variable declared here.")}
         |""".stripMargin
    }
  }

  /**
    * A unification equation system was too complex to solve.
    *
    * @param loc the location where the error occurred.
    */
  case class TooComplex(loc: SourceLocation) extends TypeError {
    def summary: String = s"Type inference too complex."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> ${red("Type inference failed due to too complex unification.")}'.
         |
         |Try to break your function into smaller functions.
         |
         |${code(loc, "too complex constraints")}
         |
         |""".stripMargin
    }
  }

  /**
    * Undefined label error.
    *
    * @param label      the name of the missing label.
    * @param labelType  the type of the missing label.
    * @param recordType the record type where the label is missing.
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedLabel(label: Name.Label, labelType: Type, recordType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing label '$label' of type '$labelType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Missing label '${red(label.name)}' of type '${cyan(formatType(labelType, Some(renv)))}'.
         |
         |${code(loc, "missing label.")}
         |
         |The record type:
         |
         |  ${formatType(recordType, Some(renv))}
         |
         |does not contain the label '${red(label.name)}' of type ${cyan(formatType(labelType, Some(renv)))}.
         |""".stripMargin
    }
  }

  /**
    * Undefined predicate error.
    *
    * @param pred       the missing predicate.
    * @param predType   the type of the missing predicate.
    * @param schemaType the schema type where the predicate is missing.
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedPred(pred: Name.Pred, predType: Type, schemaType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing predicate '${pred.name}' of type '$predType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Missing predicate '${red(pred.name)}' of type '${cyan(formatType(predType, Some(renv)))}'.
         |
         |${code(loc, "missing predicate.")}
         |
         |The schema type:
         |
         |  ${formatType(schemaType, Some(renv))}
         |
         |does not contain the predicate '${red(pred.name)}' of type ${cyan(formatType(predType, Some(renv)))}.
         |""".stripMargin
    }
  }

  /**
    * An error indicating an unexpected argument.
    *
    * @param sym      the symbol.
    * @param ith      the index of the unexpected argument.
    * @param expected the expected type.
    * @param actual   the actual type.
    * @param loc      the location where the error occurred.
    */
  case class UnexpectedArg(sym: Symbol, ith: Int, expected: Type, actual: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'.
         |
         |${code(loc, s"expected: '${cyan(formatType(expected, Some(renv)))}'")}
         |
         |The function '${magenta(sym.toString)}' expects its ${Grammar.ordinal(ith)} argument to be of type '${formatType(expected, Some(renv))}'.
         |
         |Expected: ${formatType(expected, Some(renv))}
         |  Actual: ${formatType(actual, Some(renv))}
         |""".stripMargin
    }
  }

  /**
    * Unexpected Effect.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class UnexpectedEffect(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }
  }

  /**
    * Unexpected Type.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class UnexpectedType(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }
  }

  /**
    * Unsupported equality error.
    *
    * @param econstr the unsupported equality constraint.
    * @param loc     the location where the error occurred.
    */
  case class UnsupportedEquality(econstr: BroadEqualityConstraint, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unsupported type equality: ${formatEqualityConstraint(econstr)}"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Unsupported type equality: ${formatEqualityConstraint(econstr)}
         |
         |${code(loc, "unsupported type equality.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      "Tip: Add an equality constraint to the function."
    })
  }

  /**
    * Missing trait constraint.
    *
    * @param trt  the trait of the constraint.
    * @param tpe  the type of the constraint.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingTraitConstraint(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError with Recoverable {
    def summary: String = s"No constraint of the '$trt' trait for the type '${formatType(tpe, Some(renv))}'"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> No constraint of the '${cyan(trt.toString)}' trait for the type '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, s"missing constraint")}
         |
         |""".stripMargin
    }
  }

  /**
   * An error raised to indicate an undefined field in a `struct.field` or `struct.field = value` expression.
   *
   * @param field the name of the missing field.
   * @param loc   the location where the error occurred.
   */
  case class UndefinedStructField(tpe: Type, renv: RigidityEnv, field: Name.Label, loc: SourceLocation)(implicit flix: Flix) extends TypeError with Recoverable {
    override def summary: String = s"Undefined field '$field' for type `${formatType(tpe, Some(renv))}`"

    def message(formatter: Formatter): String = {
      import formatter._
      s""">> Undefined field '$field' for type `${formatType(tpe, Some(renv))}`
         |
         |${code(loc, "undefined field")}
         |""".stripMargin
    }
  }

}
