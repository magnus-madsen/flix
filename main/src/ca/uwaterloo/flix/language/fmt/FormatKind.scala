/*
 * Copyright 2021 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.Kind

object FormatKind {

  /**
    * Create a string representation of the kind.
    */
  def formatKind(kind: Kind): String = kind match {
    case Kind.Wild => "???"
    case Kind.WildCaseSet => s"CaseSet[???]"
    case Kind.Star => "Type"
    case Kind.Eff => "Eff"
    case Kind.Region => "Region"
    case Kind.Bool => "Bool"
    case Kind.RecordRow => "RecordRow"
    case Kind.SchemaRow => "SchemaRow"
    case Kind.Predicate => "Predicate"
    case Kind.Jvm => "Jvm"
    case Kind.CaseSet(sym) => s"CaseSet[${sym.name}]"
    // parenthesize the left because `->` is right-associative
    case Kind.Arrow(k1: Kind.Arrow, k2) => s"(${formatKind(k1)}) -> ${formatKind(k2)}"
    case Kind.Arrow(k1, k2) => s"${formatKind(k1)} -> ${formatKind(k2)}"
    case Kind.Error => "Error"
  }

}
