/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.shared.BoolSubstitution

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

object ZhegalkinCache {

  /**
    * Controls what caches are enabled.
    */
  var EnableUnionCache: Boolean = true
  var EnableInterCache: Boolean = true
  var EnableXorCache: Boolean = true
  var EnableSVECache: Boolean = false
  var EnableInterCstCache: Boolean = false

  /**
    * A cache that represents the union of the two given Zhegalkin expressions.
    */
  private val cachedUnion: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * A cache that represents the intersection of the two given Zhegalkin expressions.
    */
  private val cachedInter: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * A cache that represents the exclusive-or of the two given Zhegalkin expressions.
    */
  private val cachedXor: ConcurrentMap[(ZhegalkinExpr, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * A cache of SVE queries: a map from the query to its MGU (if it exists).
    */
  private val cachedSVE: ConcurrentMap[ZhegalkinExpr, BoolSubstitution[ZhegalkinExpr]] = new ConcurrentHashMap()

  /**
    * A cache that represents the intersection of the given Zhegalkin constant and expression.
    */
  private val cachedInterCst: ConcurrentMap[(ZhegalkinCst, ZhegalkinExpr), ZhegalkinExpr] = new ConcurrentHashMap()

  /**
    * Returns the union of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  @inline
  def lookupOrComputeUnion(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkUnion: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableUnionCache) {
      return mkUnion(e1, e2)
    }
    cachedUnion.computeIfAbsent((e1, e2), _ => mkUnion(e1, e2))
  }

  /**
    * Returns the intersection of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  @inline
  def lookupOrComputeInter(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkInter: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableInterCache) {
      return mkInter(e1, e2)
    }
    cachedInter.computeIfAbsent((e1, e2), _ => mkInter(e1, e2))
  }

  /**
    * Returns the exclusive-or of the two given Zhegalkin expressions `e1` and `e2`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  @inline
  def lookupOrComputeXor(e1: ZhegalkinExpr, e2: ZhegalkinExpr, mkXor: (ZhegalkinExpr, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableXorCache) {
      return mkXor(e1, e2)
    }

    cachedXor.computeIfAbsent((e1, e2), _ => mkXor(e1, e2))
  }

  /**
    * Returns the result of running the given `sve` algorithm on the given Zhegalkin expressions `q`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  @inline
  def lookupOrComputeSVE(q: ZhegalkinExpr, sve: ZhegalkinExpr => BoolSubstitution[ZhegalkinExpr]): BoolSubstitution[ZhegalkinExpr] = {
    if (!EnableSVECache) {
      return sve(q)
    }

    cachedSVE.computeIfAbsent(q, _ => sve(q))
  }

  /**
    * Returns the intersection of the given Zhegalkin constant `c` and the expression `e`.
    *
    * Performs a lookup in the cache or computes the result.
    */
  @inline
  def lookupOrComputeInterCst(c: ZhegalkinCst, e: ZhegalkinExpr, mkInter: (ZhegalkinCst, ZhegalkinExpr) => ZhegalkinExpr): ZhegalkinExpr = {
    if (!EnableInterCstCache) {
      return mkInter(c, e)
    }
    cachedInterCst.computeIfAbsent((c, e), _ => mkInter(c, e))
  }

  /**
    * Clears all caches.
    */
  def clearCaches(): Unit = {
    cachedUnion.clear()
    cachedInter.clear()
    cachedXor.clear()
    cachedSVE.clear()
    cachedInterCst.clear()
  }

  /**
    * Returns a human-readable string with statistics about the caches.
    */
  def stats: String = s"cachedUnion = ${cachedUnion.size()}, cachedInter = ${cachedInter.size()}, cachedInterCst = ${cachedInterCst.size()}, cachedXor = ${cachedXor.size()}, cachedSVE = ${cachedSVE.size()}"

}
