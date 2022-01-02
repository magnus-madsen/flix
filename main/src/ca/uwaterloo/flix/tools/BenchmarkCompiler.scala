package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{LocalResource, Options, StatUtils}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompiler {

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val N = 14

  /**
    * Outputs statistics about the size of the generated JVM code.
    */
  def benchmarkCodeSize(o: Options): Unit = {
    val flix = newFlix(o)
    val result = flix.compile().get
    val codeSize = result.codeSize

    // Find the number of lines of source code.
    val lines = result.getTotalLines.toLong

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("codeSize" -> codeSize) ~
          ("lines" -> lines)
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Generated Code Size ======================")
      println()
      println(s"Generated ${java.text.NumberFormat.getIntegerInstance.format(codeSize)} Bytes of code from $lines lines of source code.")
    }
  }

  /**
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    //
    // Collect data from N iterations.
    //
    val r = (0 until N).map { _ =>
      val flix = newFlix(o)
      val compilationResult = flix.compile().get
      (compilationResult, flix.phaseTimers.toList)
    }

    //
    // Split into compilation results and phase results.
    //
    val results = r.map(_._1).toList
    val phases = r.map(_._2)

    // Compute a map from phase -> list of times.
    val phaseTimings = phases.flatten.groupMap(_.phase)(_.time)

    // Compute a map from phase -> minimum time.
    val phaseMinTime = phaseTimings.map {
      case (phase, times) => (phase, times.min)
    }.toList

    // Compute the sum of all the minimum times.
    val totalMinTime = phaseMinTime.map(_._2).sum

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.getTotalLines.toLong

    // Find the timings of each run.
    val timings = results.map(_.getTotalTime)

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("threads" -> threads) ~
          ("lines" -> lines) ~
          ("iterations" -> N) ~
          ("phases" -> phaseMinTime.map {
            case (phase, time) => ("phase" -> phase) ~ ("time" -> time)
          })
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Compiler Phases ======================")
      println()
      println("Best runtime per phase (low to high):")
      for ((phase, time) <- phaseMinTime.sortBy(_._2)) {
        val msec = time.toDouble / 1_000_000.toDouble
        val percent = 100.0 * (time.toDouble / totalMinTime.toDouble)
        println(f"  $phase%-30s $msec%5.1f ms ($percent%04.1f%%)")
      }
      println()
      println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")
    }
  }

  /**
    * Computes the throughput of the compiler.
    */
  def benchmarkThroughput(o: Options): Unit = {
    //
    // Collect data from N iterations.
    //
    val results = (0 until N).map { _ =>
      val flix = newFlix(o)
      flix.compile().get
    }

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.getTotalLines.toLong

    // Find the timings of each run.
    val timings = results.map(_.getTotalTime).toList

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Find the throughput of each run.
    val throughputs = timings.map(throughput(lines, _))

    // Compute the minimum throughput (per second).
    val min = throughputs.min

    // Compute the maximum throughput (per second).
    val max = throughputs.max

    // Compute the average throughput (per second).
    val avg = StatUtils.avg(throughputs.map(_.toLong)).toInt

    // Compute the median throughput (per second).
    val median = StatUtils.median(throughputs.map(_.toLong)).toInt

    // Compute the fastest iteration.
    val bestIter = timings.indexOf(timings.min)

    // Compute the ration between the slowest and fastest run.
    val bestWorstRatio = timings.max.toDouble / timings.min.toDouble

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("threads" -> threads) ~
          ("lines" -> lines) ~
          ("iterations" -> N) ~
          ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Compiler Throughput ======================")
      println()
      println(f"Throughput (best): $max%,6d lines/sec (with $threads threads.)")
      println()
      println(f"  min: $min%,6d, max: $max%,6d, avg: $avg%,6d, median: $median%,6d")
      println()
      println(f"  The highest throughput was in iteration: $bestIter (out of $N).")
      println(f"  The ratio between the best and worst iteration was: $bestWorstRatio%1.1fx.")
      println()
      println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")
    }
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

  /**
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(o: Options): Flix = {
    val flix = new Flix()

    flix.setOptions(opts = o.copy(loadClassFiles = false, writeClassFiles = false))

    // NB: We only use unit tests from the standard library because we want to test real code.

    flix.addSourceCode("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addSourceCode("TestBigInt.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestBigInt.flix"))
    flix.addSourceCode("TestChain.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChain.flix"))
    flix.addSourceCode("TestChar.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChar.flix"))
    flix.addSourceCode("TestDelayMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestDelayMap.flix"))
    flix.addSourceCode("TestFloat32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat32.flix"))
    flix.addSourceCode("TestFloat64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat64.flix"))
    flix.addSourceCode("TestFromString.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFromString.flix"))
    flix.addSourceCode("TestGetOpt.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestGetOpt.flix"))
    flix.addSourceCode("TestHash.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestHash.flix"))
    flix.addSourceCode("TestInt8.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt8.flix"))
    flix.addSourceCode("TestInt16.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt16.flix"))
    flix.addSourceCode("TestInt32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt32.flix"))
    flix.addSourceCode("TestInt64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt64.flix"))
    flix.addSourceCode("TestIterator.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addSourceCode("TestLazyList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestLazyList.flix"))
    flix.addSourceCode("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addSourceCode("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addSourceCode("TestMutList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addSourceCode("TestMutDeque.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addSourceCode("TestNel.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addSourceCode("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addSourceCode("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addSourceCode("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addSourceCode("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addSourceCode("TestString.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestString.flix"))
    flix.addSourceCode("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))

    flix
  }

}
