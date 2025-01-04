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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.shared.Constant

object FormatConstant {

  /**
    * Returns a string representation of the given constant.
    */
  def formatConstant(cst: Constant): String = cst match {
    case Constant.Unit => "()"
    case Constant.Null => "null"
    case Constant.Bool(lit) => lit.toString
    case Constant.Char(lit) => "'" + lit.toString + "'"
    case Constant.Float32(lit) => s"${lit}f32"
    case Constant.Float64(lit) => s"${lit}f64"
    case Constant.BigDecimal(lit) => s"${lit}ff"
    case Constant.Int8(lit) => s"${lit}i8"
    case Constant.Int16(lit) => s"${lit}i16"
    case Constant.Int32(lit) => s"${lit}i32"
    case Constant.Int64(lit) => s"${lit}i64"
    case Constant.BigInt(lit) => s"${lit}ii"
    case Constant.Str(lit) => "\"" + lit + "\""
    case Constant.Regex(lit) => "regex\"" + lit.pattern() + "\""
  }

}
