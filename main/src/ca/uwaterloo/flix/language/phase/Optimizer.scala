/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.LiftedAst.Root
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Expr
import ca.uwaterloo.flix.language.dbg.AstPrinter.*

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner -> Reducer.
  */
object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Optimizer") {
    if (flix.options.xnooptimizer) {
      root
    } else {
      var result = root
      for (_ <- 1 to flix.options.inlinerRounds) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(result)
        val afterInliner = Inliner.run(afterOccurrenceAnalyzer)
        result = afterInliner.unsafeGet
      }
      result
    }
  }

  /**
    * returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  def isTrivialExp(exp0: Expr): Boolean = exp0 match {
    case Expr.Cst(_, _, _) => true
    case Expr.Var(_, _, _) => true
    case _ => false
  }
}
