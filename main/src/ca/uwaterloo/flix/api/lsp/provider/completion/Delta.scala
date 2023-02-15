/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.{Name, Symbol}

/**
  * A common super-type for deltas (differences) between ASTs.
  */
sealed trait Delta {
  /**
    * Returns the timestamp (time since UNIX epoch) of when the change happened.
    */
  def timestamp: Long
}

object Delta {

  /**
    * Represents the addition of a new function.
    *
    * @param sym the symbol of the new function.
    */
  case class AddDef(sym: Symbol.DefnSym, timestamp: Long) extends Delta

  /**
    * Represents the addition of a new enum.
    *
    * @param sym the symbol of the new enum.
    */
  case class AddEnum(sym: Symbol.EnumSym, timestamp: Long) extends Delta

  /**
    * Represents the addition of a new field.
    *
    * @param field the name of the field.
    */
  case class AddField(field: Name.Field, timestamp: Long) extends Delta

  // TODO: ...

}
