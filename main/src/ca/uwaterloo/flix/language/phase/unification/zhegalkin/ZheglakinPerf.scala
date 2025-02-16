/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.api.{Flix, FlixEvent}
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.Phase
import ca.uwaterloo.flix.language.phase.unification.set.{Equation, SetUnification}
import ca.uwaterloo.flix.util.StatUtils.{average, median}
import ca.uwaterloo.flix.util.{FileOps, Options, Subeffecting}

import java.nio.file.Paths
import scala.collection.mutable

object ZheglakinPerf {

  private val RQ1 = "RQ1: Characteristics of the Boolean Equation Systems"
  private val RQ2 = "RQ2: Performance Gain of with Rewrite Rules"
  private val RQ3 = "RQ3: Performance Gain of Per-Operation Caching"
  private val RQ4 = "RQ4: Performance Gain of the Solve-and-Retry Strategy"

  private val DefaultN: Int = 2

  private val FullSubeffecting: Set[Subeffecting] = Set(Subeffecting.ModDefs, Subeffecting.InsDefs, Subeffecting.Lambdas)

  private case class Config(
                             rewriteRules: Boolean = false,
                             cacheInterCst: Boolean = false,
                             cacheUnion: Boolean = false,
                             cacheInter: Boolean = false,
                             cacheXor: Boolean = false,
                             cacheSVE: Boolean = false,
                             smartSubeffecting: Boolean = false,
                             opts: Options
                           )

  def run(n: Option[Int]): Unit = {
    val r = n.getOrElse(DefaultN)

    rq1(r)
    rq2()
    rq3(r)
    rq4(r)
  }


  private def rq1(n: Int): Unit = {
    println(RQ1)

    // TODO: What options should be used when we collect the constraints?

    // Collect all Boolean effect equation systems.
    val buffer = mutable.ArrayBuffer.empty[List[Equation]]

    EffUnification3.EnableSmartSubeffecting = false
    val flix = new Flix()
    flix.addListener {
      case FlixEvent.SolveEffEquations(eqns) =>
        if (eqns.nonEmpty) {
          buffer += eqns
        }
      case _ => // nop
    }
    val (_, errors) = flix.check()
    assert(buffer.nonEmpty)
    assert(errors.isEmpty)
    val allEquationSystems = buffer.toList

    val py1 =
      """import seaborn
        |import matplotlib.pyplot as plt
        |import pandas as pd
        |
        |seaborn.set(style = 'whitegrid')
        |
        |df = pd.read_csv('data.txt')
        |
        |seaborn.stripplot(data=df, x="Constraints", size=4.0, jitter=0.5, alpha=0.4)
        |plt.xlabel("Constraints")
        |plt.xlim(1, None)
        |plt.grid(True)
        |
        |plt.savefig('numberOfConstraintsPerSystem.png')
        |plt.show()
        |
        |
        |
        |seaborn.scatterplot(data=df, x="Constraints", y="FlexVars", alpha=0.2)
        |seaborn.scatterplot(data=df, x="Constraints", y="RigidVars", alpha=0.2)
        |seaborn.scatterplot(data=df, x="Constraints", y="Effects", alpha=0.2)
        |plt.ylabel("Constraints")
        |plt.ylabel("Quantity")
        |plt.xlim(1, 60)
        |plt.ylim(1, 60)
        |plt.grid(True)
        |
        |plt.savefig('numberOfVarsPerSystem.png')
        |plt.show()
        |
        |
        |
        |
        |seaborn.histplot(data=df[['RigidVars', 'Effects']], multiple='dodge', discrete=True, alpha=0.8, palette=seaborn.color_palette()[1:])
        |plt.ylabel("Quantity")
        |plt.grid(True)
        |
        |plt.savefig('histogram.png')
        |plt.show()
        |
        |
        |""".stripMargin

    val table = allEquationSystems.map {
      system =>
        val numberOfConstraints = system.length

        val flexVars = system.foldLeft(Set.empty[Int]) {
          (acc, eqn) => acc ++ eqn.varsOf
        }
        val numberOfFlexVariables = flexVars.size

        val rigidVars = system.foldLeft(Set.empty[Int]) {
          (acc, eqn) => acc ++ eqn.cstsOf
        }
        val numberOfRigidVariables = rigidVars.size

        val elements = system.foldLeft(Set.empty[Int]) {
          (acc, eqn) => acc ++ eqn.elmsOf
        }
        val numberOfElements = elements.size

        (numberOfConstraints, numberOfFlexVariables, numberOfRigidVariables, numberOfElements)
    }

    val data = "Constraints,FlexVars,RigidVars,Effects" + "\n" + table.map(t => s"${t._1},${t._2},${t._3},${t._4}").mkString("\n")
    FileOps.writeString(Paths.get("./data.txt"), data)
    FileOps.writeString(Paths.get("./plots.py"), py1)


    println("-" * 80)
    // Runs and Systems
    println(f"\\newcommand{\\Runs}{$n%,d}")
    println(f"\\newcommand{\\Systems}{${table.length}%,d}")
    // Constraints
    println(f"\\newcommand{\\ConstraintsTot}{${table.map(_._1).sum}%,d}")
    println(f"\\newcommand{\\ConstraintsMin}{${table.map(_._1).min}%,d}")
    println(f"\\newcommand{\\ConstraintsMax}{${table.map(_._1).max}%,d}")
    println(f"\\newcommand{\\ConstraintsAvg}{${average(table.map(_._1))}%1.1f}")
    println(f"\\newcommand{\\ConstraintsMed}{${median(table.map(_._1))}%1.1f}")
    // Flexible Variables
    println(f"\\newcommand{\\FlexVarsMin}{${table.map(_._2).min}%,d}")
    println(f"\\newcommand{\\FlexVarsMax}{${table.map(_._2).max}%,d}")
    println(f"\\newcommand{\\FlexVarsAvg}{${average(table.map(_._2))}%1.1f}")
    println(f"\\newcommand{\\FlexVarsMed}{${median(table.map(_._2))}%1.1f}")
    // Rigid Variables
    println(f"\\newcommand{\\RigidVarsMin}{${table.map(_._3).min}%,d}")
    println(f"\\newcommand{\\RigidVarsMax}{${table.map(_._3).max}%,d}")
    println(f"\\newcommand{\\RigidVarsAvg}{${average(table.map(_._3))}%1.1f}")
    println(f"\\newcommand{\\RigidVarsMed}{${median(table.map(_._3))}%1.1f}")
    // Elements
    println(f"\\newcommand{\\ElmsMin}{${table.map(_._4).min}%,d}")
    println(f"\\newcommand{\\ElmsMax}{${table.map(_._4).max}%,d}")
    println(f"\\newcommand{\\ElmsAvg}{${average(table.map(_._4))}%1.1f}")
    println(f"\\newcommand{\\ElmsMed}{${median(table.map(_._4))}%1.1f}")
    println("-" * 80)
    println()
  }

  private def rq2(): Unit = {
    println(RQ2)

    SetUnification.EnableStats = true
    val flix = new Flix()
    val (_, errors) = flix.check()
    assert(errors.isEmpty)
    SetUnification.EnableStats = false

    val m = SetUnification.ElimPerRule.toMap
    val trivial = m(Phase.Trivial)
    val constProp = m(Phase.ConstantPropagation)
    val varProp = m(Phase.VariablePropagation)
    val varAssign = m(Phase.VariableAssignment)
    val reflDupl = m(Phase.ReflexiveAndDuplicate)
    val sve = m(Phase.SuccessiveVariableElimination)

    println("-" * 80)
    println("  Trivial | ConstProp |  VarProp |  VarAssign |  ReflDupl |    SVE")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(f"$trivial%,9d & $constProp%,9d & $varProp%,8d & $varAssign%,10d & $reflDupl%,9d & $sve%,6d \\\\")
    println("-" * 80)
    println()
    println()
  }

  private def rq3(n: Int): Unit = {
    println(RQ3)

    val DefaultNoCaches = Config(rewriteRules = true, cacheInterCst = false, cacheUnion = false, cacheInter = false, cacheXor = false, cacheSVE = false, smartSubeffecting = false, opts = Options.Default.copy(xsubeffecting = FullSubeffecting))

    val m1 = runConfig(DefaultNoCaches, n).mdn
    val m2 = runConfig(DefaultNoCaches.copy(cacheInterCst = true), n).mdn
    val m3 = runConfig(DefaultNoCaches.copy(cacheUnion = true), n).mdn
    val m4 = runConfig(DefaultNoCaches.copy(cacheInter = true), n).mdn
    val m5 = runConfig(DefaultNoCaches.copy(cacheXor = true), n).mdn
    val m6 = runConfig(DefaultNoCaches.copy(cacheSVE = true), n).mdn
    val m7 = runConfig(DefaultNoCaches.copy(cacheInterCst = true, cacheUnion = true, cacheInter = true, cacheXor = true, cacheSVE = true), n).mdn

    println("-" * 80)
    println("  Baseline |  c1 ∩ z2 |  z1 ∪ z2 |  z1 ∩ z2 |  z1 ⨁ z2 |  SVE(z1, z2) |     All")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(f"$m1%,10d & $m2%,8d & $m3%,8d & $m4%,8d & $m5%,8d & $m6%,12d & $m7%,7d \\\\")
    println("-" * 80)
    println()
    println()
  }

  private def rq4(n: Int): Unit = {
    println(RQ4)

    val DefaultNoSubeffecting = Config(rewriteRules = true, cacheInterCst = false, cacheUnion = false, cacheInter = true, cacheXor = false, cacheSVE = false, opts = Options.Default.copy(xsubeffecting = Set.empty))

    val baseline = runConfig(DefaultNoSubeffecting.copy(smartSubeffecting = false, opts = Options.Default.copy(xsubeffecting = FullSubeffecting)), n).mdn
    val solveAndRetry = runConfig(DefaultNoSubeffecting.copy(smartSubeffecting = true, opts = Options.Default.copy(xsubeffecting = FullSubeffecting)), n).mdn
    val optimal = runConfig(DefaultNoSubeffecting, n).mdn

    println("-" * 80)
    println("  Baseline |  Solve-and-Retry |  Optimal")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(f"$baseline%,10d & $solveAndRetry%,16d & $optimal%,8d \\\\")
    println("-" * 80)
    println()
    println()
  }

  /**
    * Run a specific configuration.
    */
  private def runConfig(c: Config, n: Int): Throughput = {
    val baseLine = aggregate(runN(n, c))

    // Find the number of lines of source code.
    val lines = baseLine.lines.toLong

    // Find the timings of each run.
    val timings = baseLine.times

    // Find the throughput of each run.
    val throughputs = timings.map(throughput(lines, _))

    // Compute the minimum throughput (per second).
    val min = throughputs.min

    // Compute the maximum throughput (per second).
    val max = throughputs.max

    // Compute the average throughput (per second).
    val avg = average(throughputs.map(_.toLong)).toInt

    // Compute the median throughput (per second).
    val mdn = median(throughputs.map(_.toLong)).toInt

    Throughput(min, max, avg, mdn)
  }

  /**
    * Runs Flix multiple times.
    */
  private def runN(N: Int, c: Config): IndexedSeq[Run] = {
    SetUnification.EnableRewriteRules = c.rewriteRules

    ZhegalkinCache.EnableInterCstCache = c.cacheInterCst
    ZhegalkinCache.EnableUnionCache = c.cacheUnion
    ZhegalkinCache.EnableInterCache = c.cacheInter
    ZhegalkinCache.EnableXorCache = c.cacheXor
    ZhegalkinAlgebra.EnableSVECache = c.cacheSVE

    EffUnification3.EnableSmartSubeffecting = c.smartSubeffecting

    (0 until N).map { _ =>
      val flix = new Flix()
      flix.setOptions(c.opts)
      runSingle(flix)
    }
  }

  /**
    * Runs Flix once.
    */
  private def runSingle(flix: Flix): Run = {
    // Clear caches.
    ZhegalkinCache.clearCaches()

    // Run the Flix compiler
    val (root, errors) = flix.check()
    if (errors.nonEmpty) {
      throw new RuntimeException(s"Errors were present after compilation: ${errors.mkString(", ")}")
    }
    val totalLines = root.get.sources.foldLeft(0) {
      case (acc, (_, sl)) => acc + sl.endLine
    }

    val totalTime = flix.getTotalTime

    Run(totalLines, totalTime)
  }

  /**
    * Merges a sequences of runs `l`.
    */
  private def aggregate(l: IndexedSeq[Run]): Runs = {
    if (l.isEmpty) {
      return Runs(0, List(0))
    }

    val lines = l.head.lines
    val times = l.map(_.time).toList
    Runs(lines, times)
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

  private case class Run(lines: Int, time: Long)

  private case class Runs(lines: Int, times: List[Long])

  private case class Throughput(min: Int, max: Int, avg: Int, mdn: Int) {
    override def toString: String = f"min: $min%,7d, max: $max%,7d, avg: $avg%,7d, median: $mdn%,7d"
  }

}
