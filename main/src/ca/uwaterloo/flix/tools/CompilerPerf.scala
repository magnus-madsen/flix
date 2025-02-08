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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{Flix, PhaseTime}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.ZhegalkinCache
import ca.uwaterloo.flix.util.StatUtils.{average, median}
import ca.uwaterloo.flix.util.{FileOps, LocalResource, Options, StatUtils}
import org.json4s.JValue
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods

import java.nio.file.Path
import java.util.UUID

object CompilerPerf {

  /**
    * The default number of compilations.
    */
  private val DefaultN: Int = 7

  /**
    * The number of threads to use for the single-thread experiment.
    */
  private val MinThreads: Int = 1

  /**
    * The number of threads to use for the multi-threaded experiment.
    */
  private val MaxThreads: Int = Runtime.getRuntime.availableProcessors()

  /**
    * The Python program used to generate graphs.
    */
  private val Python =
    """
      |# $ pip install pandas
      |
      |import json
      |import matplotlib
      |import matplotlib.pyplot as plt
      |
      |with open('speedupWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    minThreads = data['minThreads']
      |    maxThreads = data['maxThreads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['speedup'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Parallel Speedup ({minThreads} vs. {maxThreads} threads, non-incremental)')
      |    ax.set_ylabel('Speedup')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1, 10)
      |
      |    plt.savefig('speedupWithPar.json.png')
      |
      |with open('incrementalism.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['ratio'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    bars = ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Incrementalism (1.0x is best) ({threads} threads)')
      |    ax.set_ylabel('Incremental Ratio')
      |    ax.bar_label(bars, fmt='\n%.1fx')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(0.0, 1.1)
      |
      |    plt.savefig('incrementalism.json.png')
      |
      |with open('throughput.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughput.json.png')
      |
      |with open('throughputWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputWithPar.json.png')
      |
      |with open('throughputWithParInc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    maxy = data['plot']['maxy']
      |    xvalues = list(map(lambda obj: obj['i'], data['results']))
      |    yvalues = list(map(lambda obj: obj['throughput'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Throughput ({threads} threads, incremental)')
      |    ax.set_ylabel('Throughput (lines/sec)')
      |
      |    plt.xticks(rotation=90)
      |    plt.ylim(1, maxy)
      |
      |    plt.savefig('throughputWithParInc.json.png')
      |
      |with open('time.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('time.json.png')
      |
      |with open('timeWithPar.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, non-incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('timeWithPar.json.png')
      |
      |with open('timeWithParInc.json', 'r') as file:
      |    data = json.load(file)
      |    threads = data['threads']
      |    xvalues = list(map(lambda obj: obj['phase'], data['results']))
      |    yvalues = list(map(lambda obj: obj['time'], data['results']))
      |
      |    fig, ax = plt.subplots()
      |    ax.bar(xvalues, yvalues)
      |
      |    ax.set_title(f'Time per Phase ({threads} threads, incremental)')
      |    ax.set_ylabel('Time (ms)')
      |
      |    plt.xticks(rotation=90)
      |    plt.subplots_adjust(bottom=0.30)
      |    plt.ylim(1)
      |
      |    plt.savefig('timeWithParInc.json.png')
      |
      |""".stripMargin

  case class Run(lines: Int, time: Long, phases: List[(String, Long)])

  case class Runs(lines: Int, times: List[Long], phases: List[(String, List[Long])])

  /**
    * Run compiler performance experiments.
    */
  def run(opts: Options): Unit = {
    // Options
    val o = opts.copy(progress = false, loadClassFiles = false)

    // The number of iterations.
    val N = o.XPerfN.getOrElse(DefaultN)

    // Run the experiments.
    val baseline = aggregate(if (o.XPerfPar) IndexedSeq.empty else perfBaseLine(N, o))
    val baselineWithPar = aggregate(perfBaseLineWithPar(N, o))
    val baselineWithParInc = aggregate(if (o.XPerfPar) IndexedSeq.empty else perfBaseLineWithParInc(N, o))

    // Find the number of lines of source code.
    val lines = baselineWithPar.lines.toLong

    // Find the timings of each run.
    val timings = baselineWithPar.times

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

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

    // Best observed throughput.
    val maxObservedThroughput = throughput(lines,
      Math.min(baseline.times.min,
        Math.min(baselineWithPar.times.min, baselineWithParInc.times.min)))

    // Timestamp (in seconds) when the experiment was run.
    val timestamp = System.currentTimeMillis() / 1000

    //
    // The combine function uses to merge data from different runs.
    //
    def combine[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double =
      if (N <= 4)
        numeric.toDouble(xs.last)
      else
        StatUtils.median(xs)

    //
    // Speedup
    //
    val speedupPar =
      ("timestamp" -> timestamp) ~
        ("minThreads" -> MinThreads) ~
        ("maxThreads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baseline.phases.zip(baselineWithPar.phases).map {
          case ((phase, times1), (_, times2)) =>
            ("phase" -> phase) ~ ("speedup" -> combine(times1.zip(times2).map(p => p._1.toDouble / p._2.toDouble)))
        })
    writeFile("speedupWithPar.json", speedupPar)

    //
    // Incrementalism
    //
    val incrementalism =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithPar.phases.zip(baselineWithParInc.phases).map {
          case ((phase, times1), (_, times2)) =>
            ("phase" -> phase) ~ ("ratio" -> combine(times1.zip(times2).map(p => Math.max(0.0, 1.toDouble - (p._2.toDouble / p._1.toDouble)))))
        })
    writeFile("incrementalism.json", incrementalism)

    //
    // Throughput
    //
    val throughoutBaseLine =
      ("timestamp" -> timestamp) ~
        ("threads" -> MinThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baseline.times.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughput.json", throughoutBaseLine)

    val throughputPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithPar.times.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughputWithPar.json", throughputPar)

    val throughputParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("plot" -> ("maxy" -> maxObservedThroughput)) ~
        ("results" -> baselineWithParInc.times.zipWithIndex.map({
          case (time, i) => ("i" -> s"Run $i") ~ ("throughput" -> throughput(lines, time))
        }))
    writeFile("throughputWithParInc.json", throughputParInc)

    //
    // Time
    //
    val timeBaseline =
      ("timestamp" -> timestamp) ~
        ("threads" -> MinThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baseline.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
        })
    writeFile("time.json", timeBaseline)

    val timeWithPar =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> false) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithPar.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
        })
    writeFile("timeWithPar.json", timeWithPar)

    val timeWithParInc =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("incremental" -> true) ~
        ("lines" -> lines) ~
        ("results" -> baselineWithParInc.phases.map {
          case (phase, times) => ("phase" -> phase) ~ ("time" -> milliseconds(combine(times)))
        })
    writeFile("timeWithParInc.json", timeWithParInc)

    //
    // Summary
    //
    val summaryJSON =
      ("timestamp" -> timestamp) ~
        ("threads" -> MaxThreads) ~
        ("lines" -> lines) ~
        ("iterations" -> N) ~
        ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> mdn))
    val s = JsonMethods.pretty(JsonMethods.render(summaryJSON))
    writeFile("summary.json", s)

    //
    // Python Plot
    //
    FileOps.writeString(Path.of("./build/").resolve("perf/").resolve("plots.py"), Python)

    println("~~~~ Flix Compiler Performance ~~~~")
    println()
    println(f"Throughput (best): $max%,6d lines/sec (with $MaxThreads threads.)")
    println()
    println(f"  min: $min%,6d, max: $max%,6d, avg: $avg%,6d, median: $mdn%,6d")
    println()
    println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")

  }

  /**
    * Runs Flix with one thread and non-incremental.
    */
  private def perfBaseLine(N: Int, o: Options): IndexedSeq[Run] = {
    // Note: The Flix object is created _for every iteration._
    (0 until N).map { _ =>
      val flix = new Flix()
      flix.setOptions(o.copy(threads = MinThreads, incremental = false))

      addInputs(flix)
      runSingle(flix)
    }
  }

  /**
    * Runs Flix with n threads and non-incremental.
    */
  private def perfBaseLineWithPar(N: Int, o: Options): IndexedSeq[Run] = {
    // Note: The Flix object is created _for every iteration._
    (0 until N).map { _ =>
      val flix = new Flix()
      flix.setOptions(o.copy(threads = MaxThreads, incremental = false))

      addInputs(flix)
      runSingle(flix)
    }
  }

  /**
    * Runs Flix with n threads and incrementally.
    */
  private def perfBaseLineWithParInc(N: Int, o: Options): IndexedSeq[Run] = {
    // We create on single Flix object.
    val flix: Flix = new Flix()
    flix.setOptions(o.copy(threads = MaxThreads, incremental = true))

    // We add all inputs.
    addInputs(flix)

    // We compile once.
    runSingle(flix)

    // And then we perform N incremental compilations with no changes to the input.
    (0 until N).map { _ =>
      runSingle(flix)
    }
  }

  /**
    * Runs Flix once.
    */
  private def runSingle(flix: Flix): Run = {
    // Clear caches.
    ZhegalkinCache.clearCaches()

    val frontendOnly = flix.options.XPerfFrontend
    val totalLines =
      if (frontendOnly) {
        val (optRoot, errors) = flix.check()
        if (errors.nonEmpty) {
          throw new RuntimeException(s"Errors were present after compilation: ${errors.mkString(", ")}")
        }
        optRoot.get.sources.foldLeft(0) {
          case (acc, (_, sl)) => acc + sl.endLine
        }
      } else {
        flix.compile().unsafeGet.getTotalLines
      }

    val phases = flix.phaseTimers.map {
      case PhaseTime(phase, time) => phase -> time
    }
    val totalTime = flix.getTotalTime

    Run(totalLines, totalTime, phases.toList)
  }

  /**
    * Merges a sequences of runs `l`.
    */
  private def aggregate(l: IndexedSeq[Run]): Runs = {
    if (l.isEmpty) {
      return Runs(0, List(0), Nil)
    }

    val lines = l.head.lines
    val times = l.map(_.time).toList
    val phases = l.head.phases.map(_._1)

    val phaseMatrix = l.map(_.phases.map(_._2))
    val transposedMatrix = phaseMatrix.transpose.map(_.toList).toList
    val transposedWithPhases = phases.zip(transposedMatrix)

    Runs(lines, times, transposedWithPhases)
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

  /**
    * Returns the given time `l` in milliseconds.
    */
  private def milliseconds(l: Double): Double = l / 1_000_000.0

  /**
    * Adds test code to the benchmarking suite.
    */
  private def addInputs(flix: Flix): Unit = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    flix.addSourceCode("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addSourceCode("TestChain.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChain.flix"))
    flix.addSourceCode("TestIterator.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addSourceCode("TestDelayList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestDelayList.flix"))
    flix.addSourceCode("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addSourceCode("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addSourceCode("TestMutDeque.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addSourceCode("TestMutList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addSourceCode("TestMutMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutMap.flix"))
    flix.addSourceCode("TestMutSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutSet.flix"))
    flix.addSourceCode("TestNel.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addSourceCode("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addSourceCode("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addSourceCode("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addSourceCode("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addSourceCode("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))
  }

  /**
    * Writes the given `json` to the given `file`.
    */
  private def writeFile(file: String, json: JValue): Unit = {
    val directory = Path.of("./build/").resolve("perf/")
    val filePath = directory.resolve(s"$file")
    FileOps.writeJSON(filePath, json)
  }

}
