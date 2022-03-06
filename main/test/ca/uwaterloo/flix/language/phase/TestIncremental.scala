/*
 * Copyright 2022 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Validation
import org.scalatest.{BeforeAndAfter, FunSuite}

class TestIncremental extends FunSuite with BeforeAndAfter {

  private val FileA = "FileA.flix"
  private val FileB = "FileB.flix"
  private val FileC = "FileC.flix"
  private val FileD = "FileD.flix"

  // A new Flix instance is created and initialized with some source code for each test.

  private var flix: Flix = _

  before {
    flix = new Flix()
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f(true));
         |    0
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cd(x: a): D[a] = DA(x)
         |    pub def cda(d: D[a]): a = match d {
         |        case DA(x) => x
         |    }
         |}
         |""".stripMargin)
    flix.addSourceCode(FileD,
      s"""
         |pub enum D[a] {
         |    case DA(a)
         |}
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.01") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int32): Int32 = x + 1i32
         |
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f(123));
         |    0
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: Int32, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |}
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.02") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: String): String = String.toUpperCase(x)
         |""".stripMargin)
    flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f("Hello World"));
         |    0
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: String, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |    pub def ch(x: a): a
         |}
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.03") {
    flix.addSourceCode(FileA,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |}
         |""".stripMargin)
    flix.addSourceCode(FileC,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin)
    flix.compile().get
  }

  test("Incremental.04") {
    flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int32): Bool = x == 0
         |
         |""".stripMargin)
    expectFailure(flix.compile())
  }

  /**
    * Helper function that handles compilation errors.
    *
    * @param res compilation result.
    * @tparam T type of success value.
    * @tparam E type of failure value.
    */
  private def expectFailure[T, E](res: => Validation[T, E]): Unit = res match {
    case Validation.Success(_) => throw new RuntimeException("Expected compilation, but compilation was successful")
    case Validation.Failure(_) => ()
  }
}
