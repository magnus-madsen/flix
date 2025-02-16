/*
 * Copyright 2024 Andreas Stenbæk Larsen
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
package ca.uwaterloo.flix.tools.pkg

sealed trait Permissions

/**
  * Permissions for dependencies.
  */
object Permissions {

  case object FlixOnly extends Permissions {
    override def toString: String = "none"
  }

  case object Restricted extends Permissions {
    override def toString: String = "restricted"
  }

  case object All extends Permissions {
    override def toString: String = "all"
  }

  def fromString(s: String): Option[Permissions] = s.toLowerCase match {
    case "none" => Some(FlixOnly)
    case "nothing" => Some(FlixOnly)
    case "n" => Some(FlixOnly)
    case "1" => Some(FlixOnly)
    case "restricted" => Some(Restricted)
    case "r" => Some(Restricted)
    case "2" => Some(Restricted)
    case "all" => Some(All)
    case "a" => Some(All)
    case "3" => Some(All)
    case _ => None
  }
}
