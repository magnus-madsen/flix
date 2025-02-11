/*
 * Copyright 2017 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.collection.ListMap

import java.util
import java.util.concurrent.{Callable, CountDownLatch, RecursiveTask}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

object ParOps {

  /**
    * The threshold at which `parAgg` switches from parallel to sequential evaluation.
    */
  private val SequentialThreshold: Int = 4

  /**
    * Returns true if the compiler is running on a single thread.
    */
  private def singleThreaded(implicit flix: Flix): Boolean = flix.options.threads == 1

  /**
    * Applies the function `f` to every element of `xs` in parallel.
    */
  def parMap[A, B: ClassTag](xs: Iterable[A])(f: A => B)(implicit flix: Flix): Iterable[B] = {
    // Just map if we're single-threaded.
    if (singleThreaded) {
      return xs.map(f)
    }

    // Compute the size of the input and construct a new empty array to hold the result.
    val size = xs.size
    val out: Array[B] = new Array(size)

    // Construct a new count down latch to track the number of threads.
    val latch = new CountDownLatch(size)

    // Construct a local volatile variable to hold a thrown exception (if any).
    // Multiple exceptions may be thrown, but we will just rethrow one of them.
    @volatile var exception: Throwable = null

    // Iterate through the elements of `xs`. Use a local variable to track the index.
    var idx = 0
    for (elm <- xs) {
      val i = idx // Ensure proper scope of i.
      flix.threadPool.execute(() => {
        try {
          out(i) = f(elm)
        } catch {
          case ex: Throwable =>
            ex.printStackTrace()
            exception = ex
        } finally {
          latch.countDown()
        }
      })
      idx = idx + 1
    }

    // Await all threads to finish and return the result.
    latch.await()

    // Rethrow the latest exception (if any).
    if (exception != null) throw exception

    out
  }

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    */
  def parMapValues[K, A, B](m: Map[K, A])(f: A => B)(implicit flix: Flix): Map[K, B] =
    parMap(m) {
      case (k, v) => (k, f(v))
    }.toMap

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    *
    * f will be applied to each value in the list.
    */
  def parMapValueList[K, A, B](m: ListMap[K, A])(f: A => B)(implicit flix: Flix): ListMap[K, B] =
    ListMap(
      parMap(m.m) {
        case (k, v) => (k, v.map(f))
      }.toMap
    )

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    *
    * f will be applied to the list of values.
    */
  def parMapValueList2[K, A, B](m: ListMap[K, A])(f: List[A] => List[B])(implicit flix: Flix): ListMap[K, B] =
    ListMap(
      parMap(m.m) {
        case (k, v) => (k, f(v))
      }.toMap
    )

  /**
    * Applies the function `f` to every element of `xs` in parallel. Aggregates the result using the applicative instance for [[Validation]].
    */
  def parTraverse[A, B, E](xs: Iterable[A])(f: A => Validation[B, E])(implicit flix: Flix): Validation[Iterable[B], E] = {
    val results = parMap(xs)(f)
    Validation.sequence(results)
  }

  /**
    * Aggregates the result of applying `seq` and `comb` to `xs`.
    */
  def parAgg[A: ClassTag, S](xs: Iterable[A], z: => S)(seq: (S, A) => S, comb: (S, S) => S)(implicit flix: Flix): S = {
    // Just fold if we're single-threaded.
    if (singleThreaded) {
      return xs.foldLeft(z)(seq)
    }
    /**
      * A ForkJoin task that operates on the array `a` from the interval `b` to `e`.
      */
    case class Task(a: Array[A], b: Int, e: Int) extends RecursiveTask[S] {
      override def compute(): S = {
        val span = e - b

        if (span < SequentialThreshold) {
          // Case: Sequential, fold over the `Limit` elements.
          (b until e).foldLeft(z) {
            case (acc, idx) => seq(acc, a(idx))
          }
        } else {
          // Case: Parallel, Fork-Join style.
          val m = span / 2
          val left = Task(a, b, b + m)
          left.fork()
          val right = Task(a, b + m, e)
          right.fork()
          comb(left.join(), right.join())
        }
      }
    }

    if (xs.isEmpty) {
      // Case 1: The iterable `xs` is empty. We simply return the neutral element z.
      z
    } else {
      // Case 2: We convert `xs` to an array and start a recursive task.
      val a = xs.toArray
      flix.threadPool.invoke(Task(a, 0, a.length))
    }
  }

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  def parReach[T](init: Set[T], next: T => Set[T])(implicit flix: Flix): Set[T] = {
    if (singleThreaded) {
      return seqReach(init, next)
    }
    // A wrapper for the next function.
    class NextCallable(t: T) extends Callable[Set[T]] {
      override def call(): Set[T] = next(t)
    }

    // Use global thread pool.
    val threadPool = flix.threadPool

    // A mutable variable that holds the currently reachable Ts.
    var reach = init

    // A mutable variable that holds the reachable Ts discovered in the last iteration.
    var delta = init

    // Iterate until the fixpoint is reached.
    while (delta.nonEmpty) {

      // Construct a collection of callables.
      val callables = new util.ArrayList[NextCallable]
      for (sym <- delta) {
        callables.add(new NextCallable(sym))
      }

      // Invoke all callables in parallel.
      val futures = threadPool.invokeAll(callables)

      // Compute the set of all inferred Ts in this iteration.
      // May include Ts discovered in previous iterations.
      val newReach = futures.asScala.foldLeft(Set.empty[T]) {
        case (acc, future) => acc ++ future.get()
      }

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Return the set of reachable Ts.
    reach
  }

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  private def seqReach[T](init: Set[T], next: T => Set[T]): Set[T] = {
    // A mutable variable that holds the currently reachable Ts.
    var reach = init

    // A mutable variable that holds the reachable Ts discovered in the last iteration.
    var delta = init

    // Iterate until the fixpoint is reached.
    while (delta.nonEmpty) {
      val newReach = delta.flatMap(next)

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Return the set of reachable Ts.
    reach
  }
}
