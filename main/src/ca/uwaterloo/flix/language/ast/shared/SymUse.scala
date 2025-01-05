/*
 * Copyright 2024 Holger Dal Mogensen
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Name.{Ident, QName}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

object SymUse {

  /**
    * Represents a use of an associated type sym.
    */
  case class AssocTypeSymUse(sym: Symbol.AssocTypeSym, loc: SourceLocation)

  /**
    * Represents a use of an enum case sym.
    */
  case class CaseSymUse(sym: Symbol.CaseSym, loc: SourceLocation)

  /**
    * Represents a use of a defn sym.
    */
  case class DefSymUse(sym: Symbol.DefnSym, loc: SourceLocation)

  /**
    * Represents a use of an effect sym.
    *
    * For an occurrence of the form `Xxx.Yyy.Zzz`, `sym` is the [[Symbol]] of the accessed element `Zzz`
    * and `qname` represents the qualified name `Xxx.Yyy.Zzz` in its entirety, where [[QName.namespace]]
    * contains an [[Ident]] for `Xxx` and `Yyy` and [[QName.ident]] contains a [[Ident]] for `Zzz`.
    *
    * @param sym    The [[Symbol]] being used.
    * @param qname  The qualified name ([[QName]]) of the use of `sym`:
    */
  case class EffectSymUse(sym: Symbol.EffectSym, qname: QName)

  /**
    * Represents a use of a LocalDef sym.
    */
  case class LocalDefSymUse(sym: Symbol.VarSym, loc: SourceLocation)

  /**
    * Represents a use of an effect operation sym.
    *
    * For an occurrence of the form `Xxx.Yyy.zzz`, `sym` is the [[Symbol]] of the accessed element `zzz`
    * and `qname` represents the qualified name `Xxx.Yyy.zzz` in its entirety, where [[QName.namespace]]
    * contains an [[Ident]] for `Xxx` and `Yyy` and [[QName.ident]] contains a [[Ident]] for `zzz`.
    *
    * @param sym    The [[Symbol]] being used.
    * @param qname  The qualified name ([[QName]]) of the use of `sym`:
    */
  case class OpSymUse(sym: Symbol.OpSym, qname: QName)

  /**
    * Represents a use of a restrictable enum case sym.
    */
  case class RestrictableCaseSymUse(sym: Symbol.RestrictableCaseSym, loc: SourceLocation)

  /**
    * Represents a use of a restrictable enum sym.
    */
  case class RestrictableEnumSymUse(sym: Symbol.RestrictableEnumSym, loc: SourceLocation)

  /**
    * Represents a use of a sig sym.
    */
  case class SigSymUse(sym: Symbol.SigSym, loc: SourceLocation)

  /**
    * Represents a use of a struct field sym.
    */
  case class StructFieldSymUse(sym: Symbol.StructFieldSym, loc: SourceLocation)

  /**
    * Represents a use of a class sym.
    */
  case class TraitSymUse(sym: Symbol.TraitSym, loc: SourceLocation)

}
