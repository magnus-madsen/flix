package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.api.effectlock.Serialization
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class EffectLockSuite extends AnyFunSuite {

  test("Reachable.01") {
    val input =
      """
        |def main(): Unit =
        |    g(f)
        |
        |def f(): Unit = ()
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
        |def a(): Unit = ()
        |
      """.stripMargin
    val result = reachableDefs(checkEffectLock(input))
    val expected = Set("main", "f", "g")
    assert(result == expected)
  }

  test("Reachable.02") {
    val input =
      """
        |use A.{q => f}
        |
        |mod A {
        |    pub def q(): Unit = ()
        |}
        |
        |def main(): Unit =
        |    g(f)
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
      """.stripMargin
    val result = reachableDefs(checkEffectLock(input))
    val expected = Set("main", "q", "g")
    assert(result == expected)
  }

  test("Reachable.03") {
    val input =
      """
        |use A.{q => f}
        |
        |mod A {
        |    pub def q(): Unit = ()
        |}
        |
        |def main(): Unit =
        |    g(f)
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
        |def a(): Unit = ()
        |
      """.stripMargin
    val result = reachableDefs(checkEffectLock(input))
    val expected = Set("main", "q", "g")
    assert(result == expected)
  }

  test("Serialization.01") {
    val input =
      """
        |pub def f(): Unit = ???
        |""".stripMargin
    val (tpe, ser) = checkSerializationType(input, "f")
    assert(Serialization.deserializeTpe(ser).get == tpe)
  }

  test("Serialization.02") {
    val input =
      """
        |pub def g(): a = ???
        |""".stripMargin
    val (tpe, ser) = checkSerializationType(input, "g")
    assert(Serialization.deserializeTpe(ser).get == tpe)
  }

  test("Serialization.03") {
    val input =
      """
        |pub def f(): Bool = ???
        |
        |pub def g(): Unit = ???
        |
        |pub def h(_: Bool): Int32 = ???
        |
        |""".stripMargin
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val (optRoot, _) = flix.check()
    val root = optRoot.get
    val funcs = List("f", "g", "h")
    val defs = root.defs.map {
      case (sym, defn) if funcs.contains(sym.text) =>
        val input = Input.FileInPackage(Path.of("."), "", "testPkg", sctx)
        val source = sym.loc.sp1.source.copy(input = input)
        val sp1 = sym.loc.sp1.copy(source)
        val sp2 = sym.loc.sp2.copy(source)
        val loc = sym.loc.copy(sp1 = sp1, sp2 = sp2)
        val sym1 = new Symbol.DefnSym(sym.id, sym.namespace, sym.text, loc)
        (sym1, defn.copy(sym = sym1))
      case sd => sd
    }
    val root1 = root.copy(defs = defs)
    val ser = Serialization.serialize(root1)
    val Some(actual) = Serialization.deserialize(ser)
    val expected = root1.defs.foldLeft(Map.empty[Serialization.Library, List[Serialization.NamedTypeScheme]]) {
      case (acc, (sym, defn)) =>
        val isPackage = sym.loc.sp1.source.input match {
          case Input.FileInPackage(_, _, _, _) => true
          case _ => false
        }
        if (isPackage)
          sym.loc.sp1.source.input match {
            case Input.FileInPackage(_, _, text, _) =>
              val defs = acc.getOrElse(text, List.empty)
              val nts = (defn.sym, defn.spec.declaredScheme)
              acc + (text -> (nts :: defs))
            case _ => ???
          }
        else
          acc
    }
    assert(expected == actual)
  }

  private def checkSerializationType(input: String, funSym: String): (Type, String) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val (optRoot, _) = flix.check()
    val root = optRoot.get
    val f = root.defs.filter(kv => kv._1.text == funSym).toList.head
    val tpe = f._2.spec.retTpe
    val ser = Serialization.serialize(tpe)
    (tpe, ser)
  }

  private def checkEffectLock(input: String): Option[TypedAst.Root] = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val result = flix.effectLockReachable()
    result
  }

  private def reachableDefs(root: Option[TypedAst.Root]): Set[String] = root match {
    case None => Set.empty
    case Some(r) => r.defs.keys.map(_.text).toSet
  }
}
