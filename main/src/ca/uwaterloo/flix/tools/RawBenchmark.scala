package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.CompilerPerf.{DefaultN, Run, perfBaseLine}
import ca.uwaterloo.flix.util.Options

import java.util.UUID

object RawBenchmark {

  case class Record(id: String, phase: String, lines: Int, time: Long)

  def runToRecords(id: String, run: Run): List[Record] = run match {
    case Run(lines, _, phases) =>
      phases.map {
        case (phase, time) => Record(id, phase, lines, time)
      }
  }

  val tsvHeader: String = Record("", "", 0, 0).productElementNames.mkString("\t")
  def recordToTsv(record: Record): String = record.productIterator.mkString("\t")

  def runRaw(opts: Options): Unit = {
    // Options
    val o = opts.copy(progress = false, loadClassFiles = false)

    // The number of iterations.
    val N = o.XPerfN.getOrElse(DefaultN)
    val runs = perfBaseLine(N, o)

    val id = UUID.randomUUID().toString

    println(tsvHeader)
    for {
      run <- runs
      record <- runToRecords(id, run)
    } {
      println(recordToTsv(record))
    }
  }

}
