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

package ca.uwaterloo.flix.api

object Version {
  /**
    * Represents the current version of Flix.
    */
  val CurrentVersion: Version = Version(major = 0, minor = 58, revision = 0)
}

/**
  * A case class to represent versions.
  */
case class Version(major: Int, minor: Int, revision: Int) {
  override val toString: String = s"$major.$minor.$revision"
}
