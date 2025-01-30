/*
 * Copyright 2024 Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.phase.Inliner1.Stats

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer1 {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    if (flix.options.xnooptimizer1) {
      root
    } else {
      var result = root
      var stats: Stats = null
      for (_ <- 1 to flix.options.inliner1Rounds) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
        val (afterInliner, stats1) = Inliner1.run(afterOccurrenceAnalyzer)
        stats = if (stats == null) stats1 else stats ++ stats1
        result = afterInliner
      }
      // println(stats)
      result
    }
  }
}
