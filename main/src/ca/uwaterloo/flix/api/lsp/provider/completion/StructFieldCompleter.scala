/*
 * Copyright 2024 Joseph Tan
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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.ast.{TypeConstructor, TypedAst}

import java.lang.reflect.{Field, Method, Modifier}

object StructFieldCompleter {
  def getCompletions(e: TypeError.UndefinedStructField, root: TypedAst.Root): Iterable[Completion.StructFieldCompletion] = {
    val fields = e.tpe.typeConstructor match {
      case Some(tc) => tc match {
        case TypeConstructor.Struct(sym, kind) => root.structs(sym).fields.values
        case _ => Nil
      }
      case None => Nil
    }
    val completions = fields.filter (_.sym.name.startsWith(e.field.name))
    completions.map(field => Completion.StructFieldCompletion(field.sym.name, e.field.loc, field.tpe))
  }
}
