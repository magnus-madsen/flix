/*
 * Copyright 2023 Anna Blume Jakobsen
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

import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol}
import ca.uwaterloo.flix.tools.pkg
import ca.uwaterloo.flix.tools.pkg.{ManifestError, PackageError}
import ca.uwaterloo.flix.util.Formatter

sealed trait BootstrapError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object BootstrapError {
  case class ManifestParseError(e: ManifestError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class FlixPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class MavenPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class JarPackageError(e: PackageError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class ReleaseError(e: pkg.ReleaseError) extends BootstrapError {
    override def message(f: Formatter): String = e.message(f)
  }

  case class FileError(e: String) extends BootstrapError {
    override def message(f: Formatter): String = e
  }

  case class GeneralError(e: List[String]) extends BootstrapError {
    override def message(f: Formatter): String = e.reduce[String] {
      case (acc, s) => acc + System.lineSeparator() + s
    }
  }

  case class EffectUpgradeError(sym: Symbol.DefnSym, uses: List[SourceLocation], originalScheme: Scheme, newScheme: Scheme) extends BootstrapError {
    override def message(f: Formatter): String = {

      s"""@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |@  WARNING! YOU MAY BE SUBJECT TO A SUPPLY CHAIN ATTACK!  @
         |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         |            ~~ Effect signatures have changed! ~~
         |
         |The following potentially harmful changes were detected:
         |$effectSets
         |
         |$useSites
         |""".stripMargin
    }

    private def effectSets: String = {
      val newEffectSet = newScheme.base.effects.mkString("*{", ", ", "}*")
      val effectSetDiff = newScheme.base.effects.diff(originalScheme.base.effects).mkString("*{", ", ", "}*")
      val congruentSentence = if (newScheme.base.effects.size == 1) "The new effect is" else "The new effects are"
      val newChanges = s"$congruentSentence: $effectSetDiff"
      s"+ `$sym` now uses $newEffectSet!"
    }

    private def useSites: String = {
      val congruentSentence = if (uses.length == 1) "The function is used in this place" else "The function is used in these places"
      s"""  $congruentSentence:
         |${uses.mkString("  - ", s"${System.lineSeparator()}  - ", "")}
         |""".stripMargin
    }
  }

  case class TrustError(loc: SourceLocation) extends BootstrapError {
    override def message(f: Formatter): String = s"TrustError at $loc"
  }
}
