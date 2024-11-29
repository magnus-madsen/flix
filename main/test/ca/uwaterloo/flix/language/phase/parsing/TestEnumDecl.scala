package ca.uwaterloo.flix.language.phase.parsing

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.ParseError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestEnumDecl extends AnyFunSuite with TestUtils {

  test("Enum.01") {
    val input =
      """
        |enum Example[(Int32)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.02") {
    val input =
      """
        |enum Example[a(Int32)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.03") {
    val input =
      """
        |enum Example[a, (Int32)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.04") {
    val input =
      """
        |enum Example[{ case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.05") {
    val input =
      """
        |enum Example[a{ case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.06") {
    val input =
      """
        |enum Example[a, { case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1)
  }

  test("Enum.07") {
    val input =
      """
        |enum Example[ with ToString { case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1, derivations = 1)
  }

  test("Enum.08") {
    val input =
      """
        |enum Example[a with ToString { case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1, derivations = 1)
  }

  test("Enum.09") {
    val input =
      """
        |enum Example[a, with ToString { case A(Int32) }
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectEnum(result, "Example", cases = 1, derivations = 1)
  }

  /**
    * Asserts that `result` contains the described enum.
    * @param name the expected name of the enum (with namespace)
    * @param cases the expected number of defined cases
    * @param derivations the expected number of trait derivations
    */
  private def expectEnum(result: (Option[TypedAst.Root], List[CompilationMessage]), name: String, cases: Int, derivations: Int = 0): Unit = result match {
    case (Some(root), _) =>
      root.enums.find { case (sym, _) => sym.toString == name } match {
        case Some((_, enm)) =>
          if (enm.cases.sizeIs != cases)
            fail(s"Expected '$name' to have $cases cases, found ${enm.cases.size}.")
          if (enm.derives.traits.sizeIs != derivations)
            fail(s"Expected '$name' to have $derivations derivations, found ${enm.derives.traits.size}.")
        case None =>
          fail(s"Expected '$name' to be defined.")
        case _ => ()
      }
    case _ => fail(s"Expected '$name' to be defined.")
  }


}
