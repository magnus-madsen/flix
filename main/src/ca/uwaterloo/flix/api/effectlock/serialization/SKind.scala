/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SKind // only have star, eff, arrow

object SKind {

  case object Wild extends SKind

  case object WildCaseSet extends SKind

  case object Star extends SKind

  case object Eff extends SKind

  case object Bool extends SKind

  case object RecordRow extends SKind

  case object SchemaRow extends SKind

  case object Predicate extends SKind

  case object Jvm extends SKind

  // case class CaseSet(sym: SerializableSymbol.RestrictableEnumSym) extends SerializableKind

  case class Arrow(k1: SKind, k2: SKind) extends SKind

}
