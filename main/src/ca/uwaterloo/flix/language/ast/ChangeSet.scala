/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input}

sealed trait ChangeSet {

  /**
    * Returns a new change set with `i` marked as changed.
    *
    * Note: The input `i` is always marked as dirty itself.
    */
  def markChanged(i: Input, dg: DependencyGraph): ChangeSet = this match {
    case ChangeSet.Everything => ChangeSet.Dirty(dg.dirty(i))
    case ChangeSet.Dirty(s) => ChangeSet.Dirty(s ++ dg.dirty(i))
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`.
    *
    * A fresh key is one that can be reused.
    * A stale key is one that must be re-compiled.
    * A key that is neither fresh nor stale can be deleted.
    *
    * An entry is fresh if it is in `oldMap` and it is not dirty (i.e. has not changed).
    * An entry is stale if it is not fresh and it is in `newMap`.
    *
    * Note that the union of stale and fresh does not have to equal `newMap` or `oldMap`.
    * This happens if a key is deleted. Then it does not occur `newMap` but it occurs in `oldMap`.
    * However, it is neither fresh nor stale. It should simply be forgotten.
    */
  def partition[K <: Sourceable, V1, V2](newMap: Map[K, V1], oldMap: Map[K, V2]): (Map[K, V1], Map[K, V2]) = this match {
    case ChangeSet.Everything =>
      (newMap, Map.empty)

    case ChangeSet.Dirty(dirty) =>
      val fresh = oldMap.filter(kv => !dirty.contains(kv._1.src.input)).filter(kv => newMap.contains(kv._1))
      val stale = newMap.filter(kv => !fresh.contains(kv._1))

      (stale, fresh)
  }

  /**
    * Returns two maps: `stale` and `fresh` according to the given `newMap` and `oldMap`, the value of the map is a list of Sourceable.
    *
    * A fresh item in the value list is one that can be reused.
    * A stale item in the value list is one that must be re-compiled.
    * An item that is neither fresh nor stale can be deleted.
    * Actually, all fresh and stale items will be in the newMap
    *
    * An item from newMap is fresh if it is also in `oldMap`, and it is not dirty (i.e. has not changed).
    * Otherwise, it is stale.
    *
    * The type of the value in the map must be the same, since when checking stale, we need to check if the value is in the fresh map.
    */
  def partitionOnValues[K, V <: Sourceable](newMap: Map[K, List[V]], oldMap: Map[K, List[V]]): (Map[K, List[V]], Map[K, List[V]]) = this match {
    case ChangeSet.Everything =>
      (newMap, Map.empty)

    case ChangeSet.Dirty(dirty) =>
      newMap.foldLeft((Map.empty[K, List[V]], Map.empty[K, List[V]])){ case ((stale, fresh), (k, vList)) =>
        val oldList = oldMap.getOrElse(k, Nil)
        val (freshList, staleList) = vList.partition(v => oldList.contains(v) &&  !dirty.contains(v.src.input))
        val staleMap = if (staleList.nonEmpty) stale + (k -> staleList) else stale
        val freshMap = if (freshList.nonEmpty) fresh + (k -> freshList) else fresh
        (staleMap, freshMap)
      }
  }
}

object ChangeSet {

  /**
    * Represents a change set where everything is dirty (used for a complete re-compilation).
    */
  case object Everything extends ChangeSet

  /**
    * Represents a change set where everything in `s` is dirty (must be recompiled).
    */
  case class Dirty(s: Set[Input]) extends ChangeSet

}

