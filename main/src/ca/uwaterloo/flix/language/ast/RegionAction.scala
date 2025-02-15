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
sealed trait RegionAction extends Ordered[RegionAction] {
  override def compare(that: RegionAction): Int = {
    def ordinal(action: RegionAction): Int = action match {
      case RegionAction.Alloc => 0
      case RegionAction.Read => 1
      case RegionAction.Write => 2
    }

    ordinal(this).compare(ordinal(that))
  }
}

object RegionAction {
  case object Alloc extends RegionAction
  case object Read extends RegionAction
  case object Write extends RegionAction
}
