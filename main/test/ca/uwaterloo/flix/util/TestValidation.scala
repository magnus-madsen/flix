/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.util.Validation._
import org.scalatest.FunSuite


class TestValidation extends FunSuite {

  test("map01") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Success("FOO"))(result)
  }

  test("map02") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }
    assertResult(Success("OOF"))(result)
  }

  test("map03") {
    val result = "foo".toSuccess[String, Exception].map {
      case x => x.toUpperCase
    }.map {
      case y => y.reverse
    }.map {
      case z => z + z
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("map04") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(Success(true))(result)
  }

  test("map05") {
    val result = "abc".toSuccess[String, Exception].map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(Success("e"))(result)
  }

  test("map06") {
    val result = SuccessWithFailures("abc", LazyList.empty[Exception]).map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(SuccessWithFailures(true, LazyList.empty[Exception]))(result)
  }

  test("map07") {
    val result = SuccessWithFailures("abc", LazyList.empty[Exception]).map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(SuccessWithFailures("e", LazyList.empty[Exception]))(result)
  }

  test("map08") {
    val ex = new RuntimeException()
    val result = SuccessWithFailures("abc", LazyList(ex)).map {
      case x => x.length
    }.map {
      case y => y < 5
    }
    assertResult(SuccessWithFailures(true, LazyList(ex)))(result)
  }

  test("map09") {
    val ex = new RuntimeException()
    val result = SuccessWithFailures("abc", LazyList(ex)).map {
      case x => x.charAt(1)
    }.map {
      case y => y + 3
    }.map {
      case z => z.toChar.toString
    }
    assertResult(SuccessWithFailures("e", LazyList(ex)))(result)
  }

  test("map10") {
    val ex = new RuntimeException()
    val result = ex.toFailure[String, Exception].map {
      case x => x.toUpperCase
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("mapN01") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception]) {
      case (x, y) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("mapN02") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SuccessWithFailures("abc", LazyList.empty)) {
      case (x, y, _) => x.toUpperCase.reverse + y.toUpperCase.reverse
    }
    assertResult(SuccessWithFailures("OOFOOF", LazyList.empty))(result)
  }

  test("mapN03") {
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SuccessWithFailures("abc", LazyList.empty)) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(SuccessWithFailures("OOFOOFCBA", LazyList.empty))(result)
  }

  test("mapN04") {
    val ex = new RuntimeException()
    val result = mapN("foo".toSuccess[String, Exception], "foo".toSuccess[String, Exception], SuccessWithFailures("abc", LazyList(ex))) {
      case (x, y, z) => x.toUpperCase.reverse + y.toUpperCase.reverse + z.toUpperCase.reverse
    }
    assertResult(SuccessWithFailures("OOFOOFCBA", LazyList(ex)))(result)
  }

  test("mapN05") {
    val result = mapN(SuccessWithFailures("foo", LazyList.empty[Exception])) {
      case x => x.toUpperCase.reverse
    }
    assertResult(SuccessWithFailures("OOF", LazyList.empty))(result)
  }

  test("mapN06") {
    val ex = new RuntimeException()
    val result = mapN(SuccessWithFailures("foo", LazyList(ex))) {
      case x => x.toUpperCase.reverse
    }
    assertResult(SuccessWithFailures("OOF", LazyList(ex)))(result)
  }

  test("mapN07") {
    val ex = new RuntimeException()
    val result = mapN(Failure(LazyList(ex)): Validation[String, Exception]) {
      case x => x.toUpperCase.reverse
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("mapN08") {
    val result = mapN("foo".toSuccess: Validation[String, Exception]) {
      case x => x.toUpperCase.reverse + x.toUpperCase.reverse
    }
    assertResult("OOFOOF".toSuccess)(result)
  }

  test("flatMapN01") {
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => x.toUpperCase.toSuccess
    }
    assertResult(Success("FOO"))(result)
  }

  test("flatMapN02") {
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(x.toUpperCase.toSuccess) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("flatMapN03") {
    val result = flatMapN(SuccessWithFailures("foo", LazyList.empty[Exception])) {
      case x => flatMapN(x.toUpperCase.toSuccess) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SuccessWithFailures("OOFOOF", LazyList.empty[Exception]))(result)
  }

  test("flatMapN04") {
    val ex = new RuntimeException()
    val result = flatMapN(SuccessWithFailures("foo", LazyList.empty[Exception])) {
      case x => flatMapN(SuccessWithFailures(x.toUpperCase, LazyList(ex))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SuccessWithFailures("OOFOOF", LazyList(ex)))(result)
  }

  test("flatMapN05") {
    val ex = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(SuccessWithFailures(x.toUpperCase, LazyList(ex))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case z => (z + z).toSuccess
        }
      }
    }
    assertResult(SuccessWithFailures("OOFOOF", LazyList(ex)))(result)
  }

  test("flatMapN06") {
    val ex1 = new RuntimeException()
    val ex2 = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => flatMapN(SuccessWithFailures(x.toUpperCase, LazyList(ex1))) {
        case y => flatMapN(y.reverse.toSuccess) {
          case _ => ex2.toFailure
        }
      }
    }
    assertResult(Failure(LazyList(ex1, ex2)))(result)
  }

  test("andThen03") {
    val ex = new RuntimeException()
    val result = flatMapN("foo".toSuccess[String, Exception]) {
      case x => ex.toFailure
    }
    assertResult(Failure(LazyList(ex)))(result)
  }

  test("andThen04") {
    val result = flatMapN("foo".toSuccess[String, Int]) {
      case x => flatMapN(Success(x.toUpperCase)) {
        case y => flatMapN(Success(y.reverse)) {
          case z => Success(z + z)
        }
      }
    }
    assertResult(Success("OOFOOF"))(result)
  }

  test("andThen05") {
    val result = flatMapN("foo".toSuccess[String, Int]) {
      case x => flatMapN(Success(x.toUpperCase)) {
        case y => flatMapN(Failure(LazyList(4, 5, 6))) {
          case z => Failure(LazyList(7, 8, 9))
        }
      }
    }
    assertResult(Failure(LazyList(4, 5, 6)))(result)
  }

  test("traverse01") {
    val result = traverse(List(1, 2, 3)) {
      case x => Success(x + 1)
    }

    assertResult(Success(List(2, 3, 4)))(result)
  }

  test("traverse02") {
    val result = traverse(List(1, 2, 3)) {
      case x => Failure(LazyList(42))
    }

    assertResult(Failure(LazyList(42, 42, 42)))(result)
  }

  test("traverse03") {
    val result = traverse(List(1, 2, 3)) {
      case x => if (x % 2 == 1) Success(x) else Failure(LazyList(x))
    }

    assertResult(Failure(LazyList(2)))(result)
  }

  test("foldRight01") {
    val result = foldRight(List(1, 1, 1))(Success(10)) {
      case (x, acc) => (acc - x).toSuccess
    }

    assertResult(Success(7))(result)
  }

}
