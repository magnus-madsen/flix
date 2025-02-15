/*
 * Copyright 2025 Matthew Lutze
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

// MATT docs
sealed trait RegionProperty extends Ordered[RegionProperty] {
  override def compare(that: RegionProperty): Int = (this, that) match {
    case (RegionProperty.Default, RegionProperty.Default) => 0
    case (RegionProperty.LowFidelity, RegionProperty.LowFidelity) => 0
    case (RegionProperty.Shared, RegionProperty.Shared) => 0
    case _ =>
      def ordinal(x: RegionProperty): Int = x match {
        case RegionProperty.Default => 0
        case RegionProperty.LowFidelity => 1
        case RegionProperty.Shared => 2
      }

      ordinal(this).compare(ordinal(that))
  }
}

object RegionProperty {
  case object Default extends RegionProperty
  case object LowFidelity extends RegionProperty
  case object Shared extends RegionProperty
}
