/*
 * Copyright 2020 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTyper extends AnyFunSuite with TestUtils {

  test("TestLeq01") {
    val input =
      """
        |def foo(): a = 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq02") {
    val input =
      """
        |def foo(): List[a] = 21 :: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq03") {
    val input =
      """
        |def foo(): Result[a, Int32] = Ok(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq04") {
    val input =
      """
        |def foo(): Result[Int32, a] = Err(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq05") {
    val input =
      """
        |def foo(): a -> a = x -> 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq06") {
    val input =
      """
        |def foo(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq07") {
    val input =
      """
        |def foo(): {x = Int32 | r} = {x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq08") {
    val input =
      """
        |def foo(): {x = Int32, y = Int32 | r} = {y = 42, x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestOccurs01") {
    val input = "def foo(a: #{A(Int32) | r}, b: #{B(Int32) | r}): #{A(Int32), B(Int32)} = solve (a <+> b)"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.01") {
    val input = "def foo(): {| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.02") {
    val input = "def foo(): #{| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.03") {
    val input = "def foo(): {a = Int32} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestMismatchedTypes.04") {
    val input = "def foo(): String = solve \"hello\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestOverApplied.01") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestOverApplied.02") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123, true)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestUnderApplied.01") {
    val input =
      """
        |def f(x: String, y: Int32): Bool = true
        |def under(): String = (f("hello"): String)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestUnderApplied.02") {
    val input =
      """
        |def f(x: String, y: Int32, z: Bool): Bool = true
        |def under(): String = (f("hello"): String)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.01") {
    val input = "def foo(a: _): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.02") {
    val input = "def foo(a: Int32): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.03") {
    val input = raw"def foo(a: Int32): Int32 \ _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestLeq.Wildcard.04") {
    val input = raw"def foo(g: Int32 -> Int32 \ _): Int32 \ _ = g(1)"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.01") {
    val input =
      """
        |trait C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: a): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.02") {
    val input =
      """
        |trait C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: Int32): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.03") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Float64]): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.04") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Int32]): String = C.foo(C.foo(x))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.05") {
    val input =
      """
        |trait C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): Int32 = x
        |}
        |
        |def bar(x: a, y: Int32): (Int32, Int32) = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.06") {
    // missing constraint on C[b]
    val input =
      """
        |trait C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |def bar(x: a, y: b): (Int32, Int32) with C[a] = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.07") {
    val input =
      """
        |trait C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |enum E[_: Eff] {
        |    case E(Int32)
        |}
        |
        |instance C[E[Pure]] {
        |    pub def foo(x: E[Pure]): Int32 = 1
        |}
        |
        |def bar(): Int32 = C.foo(E.E(123))    // E(123) has type E[_], not E[Pure]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("NoMatchingInstance.Relation.01") {
    val input =
      """
        |pub enum E {
        |   case E1
        |}
        |
        |pub def f(): Bool = {
        |   let _x = #{
        |     R(E.E1).
        |   };
        |   true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingEq.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x == y
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingSendable.01") {
    val input =
      """
        |enum NotSendable(Int32)
        |enum TrySendable[a](a) with Sendable
        |
        |def requiresSendable(x: a): a with Sendable[a] = x
        |
        |def foo(): TrySendable[NotSendable] = requiresSendable(TrySendable.TrySendable(NotSendable.NotSendable(42)))
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingSendable.02") {
    val input =
      """
        |enum NotSendable(Int32)
        |enum TrySendable[a, b](a, b) with Sendable
        |
        |def requiresSendable(x: a): a with Sendable[a] = x
        |
        |def foo(): TrySendable[NotSendable, NotSendable] = requiresSendable(TrySendable.TrySendable(NotSendable.NotSendable(42), NotSendable.NotSendable(43)))
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingOrder.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x <= y
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingToString.01") {
    val input =
      s"""
         |pub enum E {
         |   case E
         |}
         |
         |def foo(x: E): String = ToString.toString(x)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("MissingArrowInstance.01") {
    val input =
      """
        |def main(): Unit \ IO =
        |    println(x -> x + 41i32)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.01") {
    val input =
      """
        |pub def f(): Int32 = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.02") {
    val input =
      """
        |def f(): Int32 \ {} = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.03") {
    // Regression test. See https://github.com/flix/flix/issues/4062
    val input =
      """
        |def mkArray(): Array[Int32, Static] \ IO = Array#{} @ Static
        |
        |def zero(): Int32 \ {} = $ARRAY_LENGTH$(mkArray())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.04") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.05") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ {} = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.06") {
    val input =
      """
        |eff Print {
        |    pub def print(): Unit
        |}
        |
        |eff Exc {
        |    pub def raise(): Unit
        |}
        |
        |def f(): Unit =
        |    do Print.print();
        |    do Exc.raise()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.07") {
    // guards must be pure
    val input =
      """
        |eff E
        |def impureBool(): Bool \ E = checked_ecast(???)
        |
        |def foo(): Int32 \ E = {
        |    match 0 {
        |        case 0 if impureBool() => 0
        |        case _ => 1
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedEffect.08") {
    val input =
      """
        |import java.lang.Object
        |
        |eff IO
        |
        |def impureX(): String \ IO = checked_ecast("x")
        |
        |def f(): Object \ IO = {
        |    let x = new Object {
        |        def toString(_this: Object): String = impureX()
        |    };
        |    x
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.EffectGeneralizationError.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ ef = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.EffectGeneralizationError.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef1, h: Int32 -> Int32 \ ef2): Int32 \ {ef1, ef2} = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.01") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.02") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            (123, x)
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.03") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            _w -> x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.RegionVarEscapes.04") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = Ref.fresh(rc, 123);
        |            w -> {
        |                discard Ref.get(x);
        |                w
        |            }
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("RegionVarEscapes.05") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = Ref.fresh(Static, None);
        |    region rc {
        |        let x = Ref.fresh(rc, 123);
        |        Ref.put(Some(x), m);
        |        ()
        |    }
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("RegionVarEscapes.06") {
    val input =
      """
        |pub enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |pub def f(): Unit \ IO =
        |    let m = Ref.fresh(Static, None);
        |    region rc {
        |        let x = Ref.fresh(rc, 123);
        |        Ref.put(Some(_ -> x), m);
        |        ()
        |    }
    """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }


  test("Test.UnexpectedType.OpParam.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit \ E = do E.op(123)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.MismatchedEff.Without.01") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def foo(): Unit = do E.op() without E
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.MismatchedEff.Apply.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit = ???
        |
        |def foo(): Unit = disjoint(_ -> do E.op(), _ -> do E.op())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.GeneralizationError.Eff.01") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |eff F {
        |    pub def op(): Unit
        |}
        |
        |def doBoth(f: Unit -> Unit \ {ef - E}, g: Unit -> Unit \ {ef - F}): Unit \ {ef - E - F} = g(); f()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.PossibleCheckedTypeCast.01") {
    val input =
      """
        |import dev.flix.test.TestClassWithDefaultConstructor
        |import dev.flix.test.TestClassWithInheritedMethod
        |
        |def f(): TestClassWithDefaultConstructor \ IO =
        |    import java_new TestClassWithInheritedMethod(): TestClassWithInheritedMethod as newObj;
        |    let x: TestClassWithDefaultConstructor = newObj();
        |    x
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.01") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.02") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.03") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (a <- 1; b <- { unchecked_cast(1 as _ \ IO) }) yield (a, b);
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestParYield.04") {
    val input =
      """
        | def f(): Int32 =
        |     par (a <- true) yield a + 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.01") {
    val input =
      """
        |def f[m: Eff -> Type, a: Eff](_: m[a]): m[a] = ???
        |
        |enum Box[a](a)
        |
        |def g(): Box[Int32] = f(Box.Box(123))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def noE(f: Unit -> Unit \ ef - E): Unit = ???
        |
        |def foo(): Unit = noE(_ -> do E.op())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.03") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def mustE(f: Unit -> Unit \ {ef, E}): Unit = ???
        |
        |def foo(): Unit = mustE(x -> x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.04") {
    val input =
      """
        |trait A[a] {
        |    pub def f(x: Bool, y: a): Bool
        |    law l: forall (x: Int32, y: Bool) A.f(x, y)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.05") {
    // Regression test.
    // See https://github.com/flix/flix/issues/3634
    val input =
      """
        |enum E[a: Type, ef: Eff](Unit)
        |def f(g: E[Int32, Pure]): Bool = ???
        |def mkE(): E[Int32, Pure] \ ef = ???
        |
        |def g(): Bool = f(mkE)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("Test.UnexpectedArgument.06") {
    val input =
      """
        |def takesString(_: String): Unit = ()
        |
        |def f(): Unit = typematch 123 {
        |    case x: _ => takesString(x)
        |    case _: _ => ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestChoose.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = choose Expr.Cst {
        |    case Expr.Var(_) => true
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |     };
        |     let h = if (true) f else g;
        |     h(Expr.Var)
        | }
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let h = if (true) f else g;
        |
        |     let cstOrNotOrVar = if (true) open_variant Expr.Cst else if (true) open_variant Expr.Not else open_variant Expr.Var;
        |
        |     h(cstOrNotOrVar)
        | }
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def quack(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def liquorice(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Not(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.04") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def testChooseStar4(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Var(_) => true
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.05") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Not(_) => Expr.Not()
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.06") {
    val input =
      """
        |restrictable enum E[s] {
        |    case N(E[s])
        |    case C
        |}
        |
        |def n(e: E[s rvand <E.N>]): _ = ???
        |
        |def foo(e: E[s]): E[s] = choose* e {
        |    case E.N(x) => n(x)            // must have x <: <E.N> but this doesn't hold
        |    case E.C    => E.C
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.01") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Not all cases caught
        |def isRed(c: Color[s]): Bool = choose c {
        |    case Color.Red => true
        |    case Color.Green => false
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.02") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// forgot Green intro
        |def redToGreen(c: Color[s]): Color[s rvsub <Color.Red>] = choose* c {
        |    case Color.Red => Color.Green
        |    case Color.Green => Color.Green
        |    case Color.Blue => Color.Blue
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.03") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Wrong minus
        |def isRed(c: Color[s rvsub <Color.Blue>]): Bool = choose* c {
        |    case Color.Red => true
        |    case Color.Blue => false
        |}
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.04") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Wrong minus parsing
        |def isRed(c: Color[s rvsub <Color.Red> rvadd <Color.Green>]): Color[(s rvsub <Color.Red>) rvadd <Color.Green>] = c
        |""".stripMargin
    expectError[TypeError](compile(input, Options.TestWithLibNix))
  }

  test("TestLetRec.01") {
    val input =
      """
        |def f(): Int32 = {
        |    def g(): Bool = 123;
        |    g()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.01") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |def g(x: a): String with C[a] = C.f(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.02") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |    pub def f(x: a): C.T[a]
        |}
        |
        |def g(x: a): String with C[a] where C.T[a] ~ Int32 = C.f(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.03") {
    val input =
      """
        |pub enum Maybe[a] {
        |    case Just(a),
        |    case Nothing
        |}
        |
        |trait C[a] {
        |    type S : Type
        |    type T : Type -> Type
        |    pub def f(x: a): C.T[a][C.S[a]]
        |}
        |
        |instance C[Int32] {
        |    type S = Int32
        |    type T = Maybe
        |    pub def f(x: Int32): Maybe[Int64] = x
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestAssocType.04") {
    val input =
      """
        |trait A[a] {
        |    type A : Type -> Type -> Type
        |    type B : Type -> Type
        |    type C : Type
        |    pub def f(x: a): A.A[a][A.A[a][A.B[a][A.A[a][A.B[a][A.C[a]]][A.B[a][A.C[a]]]]][A.B[a][A.C[a]]]][A.A[a][A.B[a][A.C[a]]][A.C[a]]]
        |    pub def g(x: a): A.A[a][A.B[a][A.C[a]]][A.C[a]] = A.f(x)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.01") {
    val input =
      """
        |def f(): Bool = match { x = 1 } {
        |    case { x = false } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.02") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { x = _ } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.03") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { } => false
        |    case { x = _ | r } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestRecordPattern.04") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = false } {
        |    case { x = _ | r } => false
        |    case { x = _ } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.01") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    do Gen.gen();
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.02") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen()
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    do Gen.gen();
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  ignore("TestIOAndCustomEffect.03") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen();
        |        do AskTell.askTell(42)
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.04") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen();
        |        do AskTell.askTell(42)
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    do Gen.gen();
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.05") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen();
        |        do AskTell.askTell(42)
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    do AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestIOAndCustomEffect.06") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen();
        |        do AskTell.askTell(42)
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    do Gen.gen();
        |    do AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }
  test("TestIOAndCustomEffect.07") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: Int32): Int32
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen()
        |    } with Gen {
        |        def gen(k) = k("a")
        |    };
        |    do AskTell.askTell(42);
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  ignore("TestIOAndCustomEffect.08") {
    val input =
      """
        |eff Gen {
        |    pub def gen(): String
        |}
        |
        |eff AskTell {
        |    pub def askTell(x: String): String
        |}
        |
        |pub def f(): Unit \ IO =
        |    let _ = try {
        |        do Gen.gen()
        |    } with Gen {
        |        def gen(k) = do AskTell.askTell(k("a"))
        |    };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError](result)
  }

  test("TestTryCatch.01") {
    val input =
      """
        |import java.io.IOError
        |
        |enum Res { case Err(String), case Ok }
        |
        |pub def catchIO(f: Unit -> Unit \ ef): Res = {
        |    try {f(); Res.Ok} catch {
        |        case ex: IOError =>
        |            import java.lang.Throwable.getMessage(): String;
        |            Res.Err(getMessage(ex))
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }


  test("TypeError.MissingConstraint.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |def foo(): C.T[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingTraitConstraint](result)
  }

  test("TypeError.AmbiguousMethod.01") {
    val input =
      """
        |import java.lang.StringBuilder
        |
        |def main(): Unit \ IO =
        |    import java_new java.lang.StringBuilder(String): StringBuilder \ IO as newSB;
        |    let a = testInvokeMethod2_01(newSB(""));
        |    println(a.toString())
        |
        |def testInvokeMethod2_01(sb: StringBuilder): StringBuilder \ IO =
        |    sb.append(null)
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError.AmbiguousMethod](result)
  }

  test("TypeError.AmbiguousMethod.02") {
    val input =
      """
        |import java.io.PrintStream
        |
        |def main(): Unit \ IO =
        |    import java_new java.io.PrintStream(String): PrintStream \ IO as newPS;
        |    testInvokeMethod2_01(newPS(""))
        |
        |def testInvokeMethod2_01(ps: PrintStream): Unit \ IO =
        |    ps.println(null)
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError.AmbiguousMethod](result)
  }

  test("TypeError.NewStruct.01") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = 3, b = 4, c = "hello"};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.NewStruct.02") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = (), b = "hi", c = "hello"};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.NewStruct.03") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |
        |def Foo(): Unit = {
        |    region rc {
        |        new S @ rc {a = 3, b = "hi", c = new S @ rc {a = 4, b = 3, c = ()}};
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.01") {
    val input =
      """
        |struct S [v, r] {
        |    a: Int32,
        |    b: String,
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s = new S @ rc {a = 4, b = "hi", c = "hello"};
        |            s->a + s->b;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.02") {
    val input =
      """
        |struct S[v, r] {
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s1 = new S @ rc {c = 3};
        |            let s2 = new S @ rc {c = "hello"};
        |            s1->c + s2->c;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.03") {
    val input =
      """
        |mod S {
        |    struct S1[r] { field1: Int32 }
        |    struct S2[r] { field2: Int32 }
        |    def f(s: S1[r]): Unit = {
        |        s->field2;
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructGet.04") {
    val input =
      """
        |struct S[r] { }
        |def foo(s: S[r]): Int32 = s->field
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.01") {
    val input =
      """
        |struct S[v, r] {
        |    mut a: Int32,
        |    b: String,
        |    c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s = new S @ rc {a = 4, b = "hi", c = "hello"};
        |            s->a = s->b;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.02") {
    val input =
      """
        |struct S[v, r] {
        |    mut c: v
        |}
        |mod S {
        |    def Foo(): Unit = {
        |        region rc {
        |            let s1 = new S @ rc {c = 3};
        |            let s2 = new S @ rc {c = "hello"};
        |            s1->c = s2->c;
        |            ()
        |        }
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.03") {
    val input =
      """
        |mod S {
        |    struct S1[r] { field1: Int32 }
        |    struct S2[r] { field2: Int32 }
        |    def f(s: S1[r]): Unit = {
        |        s->field2 = 3;
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.StructPut.04") {
    val input =
      """
        |struct S[r] { }
        |def foo(s: S[r]): Int32 = s->field = 3
        |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError](result)
  }

  test("TypeError.MutateImmutableField.01") {
    val input = """
                  |mod S {
                  |    struct S[r] {f: Int32}
                  |    def f(rc: Region[r]): Unit \ r = {
                  |        let s = new S @ rc {f = 3};
                  |        s->f = 2;
                  |        ()
                  |    }
                  |}
                  |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError.UndefinedStructField](result)
  }

  test("TypeError.MutateImmutableField.02") {
    val input = """
                  |mod S {
                  |    struct S[r] {f1: Int32, mut f2: Int32, f3: Int32}
                  |    def f(rc: Region[r]): Unit \ r = {
                  |        let s = new S @ rc {f1 = 3, f2 = 4, f3 = 5};
                  |        s->f2 = 2;
                  |        s->f1 = 2;
                  |        ()
                  |    }
                  |}
                  |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError.UndefinedStructField](result)
  }

  test("TypeError.MutateImmutableField.03") {
    val input = """
                  |mod S {
                  |    struct S[v, r] {f: Int32, mut f2: v}
                  |    def f(rc: Region[r]): Unit \ r = {
                  |        let s = new S @ rc {f = 3, f2 = new S @ rc {f = 4, f2 = 5}};
                  |        s->f2->f = 2;
                  |        ()
                  |    }
                  |}
                  |""".stripMargin
    val result = compile(input, Options.Default)
    expectError[TypeError.UndefinedStructField](result)
  }
}
