/*
 * Copyright 2021 Jonathan Lindegaard Starup
 * Copyright 2025 Chenahao Gao
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

package ca.uwaterloo.flix.util.collection

/**
  * Companion object for the [[ListMap]] class.
  */
object ListMap {
  /**
    * Returns the empty list map.
    */
  def empty[K, V]: ListMap[K, V] = ListMap(Map.empty[K, List[V]])

  /**
    * Returns a singleton list map with a mapping from `k` to `v`.
    */
  def singleton[K, V](k: K, v: V): ListMap[K, V] = empty + (k -> v)

  /**
    * Creates a ListMap from the given iterable of key-value pairs.
    *
    * Note that from takes a different type of iterable than `apply`.
    * If you want to create a ListMap from a list of key-valueList pairs, use `apply`.
    */
  def from[K, V](it: Iterable[(K, V)]): ListMap[K, V] = {
    it.iterator.foldLeft(ListMap.empty[K, V]) {
      case (acc, (k, v)) => acc + (k, v)
    }
  }

  /**
    * Creates a ListMap from the given key-valueList pairs.
    *
    * Note that apply takes a different type of iterable than `from`.
    * If you want to create a ListMap from a list of key-value pairs, use `from`.
    */
  def apply[K, V](elems: (K, List[V])*): ListMap[K, V] = {
    elems.foldLeft(ListMap.empty[K, V]) {
      case (acc, (k, vs)) => acc ++ (k -> vs)
    }
  }
}

/**
  * Represents a map from keys of type `K` to list of values of type `V`.
  * When a new mapping from `k` is added, it is prepended onto the list.
  */
case class ListMap[K, V](m: Map[K, List[V]]) {

  /**
    * Returns all the values in the list map flattened into a single iterable.
    */
  def values: Iterable[V] = m.values.flatten

  /**
    * Returns all the value lists in the list map.
    */
  def valueLists: Iterable[List[V]] = m.values

  /**
    * Returns all the keys in the list map.
    */
  def keys: Iterable[K] = m.keys

  /**
    * Returns the list of values that the key `k` maps to.
    * If there is no mapping for `k` returns an empty list.
    */
  def get(k: K): List[V] = m.getOrElse(k, Nil)

  /**
    * Returns the list of values that the key `k` maps to.
    */
  def apply(k: K): List[V] = m.getOrElse(k, List.empty)

  /**
    * Returns an iterable by applying `f` to each of the mappings in the list map.
    * The function `f` is expected to take a key `k` and a value `v`, not a list of values.
    */
  def map[A](f: ((K, V)) => A): Iterable[A] = {
    m.flatMap {
      case (k, vs) => vs.map(v => f(k, v))
    }
  }

  /**
    * Applies `f` to each of the mappings in the list map.
    * The function `f` is expected to take a key `k` and a value `v`, not a list of values.
    */
  def flatMap[A](f: ((K, V)) => Iterable[A]): Iterable[A] =
    m.flatMap {
      case (k, vs) => vs.flatMap(v => f(k, v))
    }

  /**
    * Required for pattern-matching in for-patterns.
    * For simplicity, we will filter eagerly.
    * We will return a Seq to store the filtered key-value pairs, as map will remove duplicates.
    */
  def withFilter(p: ((K, V)) => Boolean): Seq[(K, V)] = {
    for {
      (k, vs) <- m.toSeq
      v <- vs
      if (p(k, v))
    } yield (k, v)
  }

  /**
    * Folds the values in the list map using the given function `f`.
    * The function `f` takes an accumulator `z` and a tuple `(k, v)` where `v` is a value of type V instead of a list of V.
    */
  def foldLeft[A](z: A)(f: (A, (K, V)) => A): A = {
    m.foldLeft(z) {
      case (acc, (k, vs)) => vs.foldLeft(acc) {
        case (acc2, v) => f(acc2, (k, v))
      }
    }
  }

  /**
    * Returns `this` list map extended with an additional mapping from `k` to `v`.
    */
  def +(kv: (K, V)): ListMap[K, V] = {
    val (k, v) = kv
    val l = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (v :: l)))
  }

  /**
    * Returns `this` list map extended with additional mappings from `k`to the values in `vs`.
    */
  def ++(kvs: (K, List[V])): ListMap[K, V] = {
    val (k, vs) = kvs
    val l = m.getOrElse(k, List.empty)
    ListMap(m + (k -> (vs ++ l)))
  }

  /**
    * Returns `this` list map extended with all mappings in `that` list mapping.
    */
  def ++(that: ListMap[K, V]): ListMap[K, V] = {
    that.m.foldLeft(this) {
      case (macc, (k, vs)) => macc ++ (k -> vs)
    }
  }

  /**
    * Returns `this` list map without the mapping for `k`.
    */
  def -(k: K): ListMap[K, V] = ListMap(m - k)

  /**
    * Returns ´this´ list map without the mappings for ´ks´.
    */
  def --(ks: Iterable[K]): ListMap[K, V] = ListMap(m -- ks)
}
