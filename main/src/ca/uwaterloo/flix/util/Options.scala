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

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.Symbol

import java.nio.file.Path

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    lib = LibLevel.All,
    entryPoint = None,
    explain = false,
    githubToken = None,
    installDeps = false,
    incremental = true,
    json = false,
    output = None,
    progress = false,
    test = false,
    target = JvmTarget.Version21,
    threads = Runtime.getRuntime.availableProcessors(),
    loadClassFiles = true,
    assumeYes = false,
    xnoverify = false,
    xnooptimizer = false,
    inlinerRounds = 2,
    xnooptimizer1 = false,
    inliner1Rounds = 3,
    xprintphases = false,
    xnodeprecated = false,
    xsummary = false,
    xfuzzer = false,
    xprinttyper = None,
    xverifyeffects = false,
    xsubeffecting = Set.empty,
    XPerfN = None,
    XPerfFrontend = false,
    XPerfPar = false,
    xiterations = 5000
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(lib = LibLevel.All, progress = false, test = true, xnodeprecated = true)

  /**
    * Default test options with the standard library.
    */
  val TestWithLibAll: Options = DefaultTest

  /**
    * Default test options with the minimal library.
    */
  val TestWithLibMin: Options = DefaultTest.copy(lib = LibLevel.Min)

  /**
    * Default test options without any library.
    */
  val TestWithLibNix: Options = DefaultTest.copy(lib = LibLevel.Nix)
}

/**
  * General Flix options.
  *
  * @param lib                 selects the level of libraries to include.
  * @param entryPoint          specifies the main entry point.
  * @param explain             enables additional explanations.
  * @param githubToken         the API key to use for GitHub dependency resolution.
  * @param incremental         enables incremental compilation.
  * @param installDeps         enables automatic installation of dependencies.
  * @param json                enable json output.
  * @param output              the optional output directory where to place JVM bytecode.
  * @param progress            print progress during compilation.
  * @param test                enables test mode.
  * @param target              the target JVM.
  * @param threads             selects the number of threads to use.
  * @param loadClassFiles      loads the generated class files into the JVM.
  * @param assumeYes           run non-interactively and assume answer to all prompts is yes.
  */
case class Options(lib: LibLevel,
                   entryPoint: Option[Symbol.DefnSym],
                   explain: Boolean,
                   githubToken: Option[String],
                   incremental: Boolean,
                   installDeps: Boolean,
                   json: Boolean,
                   progress: Boolean,
                   output: Option[Path],
                   target: JvmTarget,
                   test: Boolean,
                   threads: Int,
                   loadClassFiles: Boolean,
                   assumeYes: Boolean,
                   xnoverify: Boolean,
                   xnooptimizer: Boolean,
                   inlinerRounds: Int,
                   xnooptimizer1: Boolean,
                   inliner1Rounds: Int,
                   xprintphases: Boolean,
                   xnodeprecated: Boolean,
                   xsummary: Boolean,
                   xfuzzer: Boolean,
                   xprinttyper: Option[String],
                   xverifyeffects: Boolean,
                   xsubeffecting: Set[Subeffecting],
                   XPerfFrontend: Boolean,
                   XPerfPar: Boolean,
                   XPerfN: Option[Int],
                   xiterations: Int,
                  )

/**
  * An option to control the version of emitted JVM bytecode.
  */
sealed trait JvmTarget

object JvmTarget {

  /**
    * Emit bytecode for Java 21.
    */
  object Version21 extends JvmTarget

}

sealed trait LibLevel

object LibLevel {

  /**
    * Do not include any libraries, even those essential for basic functionality.
    */
  case object Nix extends LibLevel

  /**
    * Only include essential libraries.
    */
  case object Min extends LibLevel

  /**
    * Include the full standard library.
    */
  case object All extends LibLevel

}

sealed trait Subeffecting

object Subeffecting {

  /**
    * Enable sub-effecting for module-level definitions.
    */
  case object ModDefs extends Subeffecting

  /**
    * Enable sub-effecting for instance-level defs.
    */
  case object InsDefs extends Subeffecting

  /**
    * Enable sub-effecting for lambda expressions.
    */
  case object Lambdas extends Subeffecting

}
