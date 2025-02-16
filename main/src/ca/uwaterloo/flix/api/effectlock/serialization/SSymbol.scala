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

sealed trait SSymbol

object SSymbol {

  case class VarSym(id: Int, text: SVT, kind: SKind) extends SSymbol

  case class TypeAliasSym(namespace: List[String], name: String) extends SSymbol

  case class AssocTypeSym(trt: SSymbol.TraitSym, name: String) extends SSymbol

  case class TraitSym(namespace: List[String], name: String) extends SSymbol

  case class EnumSym(namespace: List[String], text: String) extends SSymbol

  case class EffectSym(namespace: List[String], name: String) extends SSymbol
}
