package ca.uwaterloo.flix
import ca.uwaterloo.flix.MutationTester.TestRes
import ca.uwaterloo.flix.language.ast.TypedAst.MutationType

import java.io.{File, FileWriter}
import scala.collection.immutable.HashMap

object MutationDataHandler {

  def processData(operatorResults: List[(MutationType, TestRes)],module:  String): Unit = {
    val empty: Map[String, Map[String,DataPoints]] = HashMap.empty
    val sortedData = empty.updated(module, sortData(operatorResults, createEmptyDataMap()))
    writeDataToFile(sortedData)
  }
  private def writeDataToFile(stringToPoints: Map[String, Map[String, DataPoints]]): Unit = {
    val stringToWrite = stringToPoints.foldLeft("") {
      case (acc, (module, results)) => results.foldLeft("") {
        case (acc2, (operator, dPoints)) =>
        s"$operator:${dPoints.total}:${dPoints.killed}:${dPoints.surviving}:${dPoints.unknown}:${dPoints.equivalent}:$module\n$acc2"
      } ++ acc
    }

    val fileWriter = new FileWriter("MutStats.txt", true)
    fileWriter.write(stringToWrite)
    fileWriter.close()
  }




  def writeTTBToFile(times: String): Unit = {
    val fileWriter = new FileWriter("TTBData.txt", true)
    fileWriter.write(times)
    fileWriter.close()
  }

  private case class DataPoints(total: Int, killed: Int, surviving: Int, unknown: Int, equivalent: Int)

  private def sortData(tuples: List[(MutationType, TestRes)],emptyMap: Map[String, DataPoints]): Map[String, DataPoints] = {
    def updateDataPoints(dataPoints: DataPoints, testRes: TestRes): DataPoints = testRes match {
      case TestRes.MutantKilled => dataPoints.copy(total = dataPoints.total + 1, killed = dataPoints.killed + 1)
      case TestRes.MutantSurvived => dataPoints.copy(total = dataPoints.total + 1, surviving = dataPoints.surviving + 1)
      case TestRes.Unknown => dataPoints.copy(total = dataPoints.total + 1, unknown = dataPoints.unknown + 1)
      case TestRes.Equivalent => dataPoints.copy(total = dataPoints.total + 1, equivalent = dataPoints.equivalent + 1)
    }
    def helper(opRes: (MutationType, TestRes), hMap: Map[String, DataPoints])  = opRes._1 match {
      case MutationType.CstMut(_) =>
        val dPoints = hMap.apply("CstMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CstMut", newDPoints)
      case MutationType.SigMut(_) =>
        val dPoints = hMap.apply("SigMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("SigMut", newDPoints)
      case MutationType.IfMut(_) =>
        val dPoints = hMap.apply("IfMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("IfMut", newDPoints)
      case MutationType.CompMut(_) =>
        val dPoints = hMap.apply("CompMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CompMut", newDPoints)
      case MutationType.CaseSwitch(_, _) =>
        val dPoints = hMap.apply("CaseSwitch")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CaseSwitch", newDPoints)
      case MutationType.VarMut(_, _) =>
        val dPoints = hMap.apply("VarMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("VarMut", newDPoints)
      case MutationType.CaseDeletion(_) =>
        val dPoints = hMap.apply("CaseDeletion")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CaseDeletion", newDPoints)
      case MutationType.RecordSelectMut(_) =>
        val dPoints = hMap.apply("RecordSelectMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("RecordSelectMut", newDPoints)
      case MutationType.ListMut() =>
        val dPoints = hMap.apply("ListMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("ListMut", newDPoints)
    }


    tuples.foldLeft(emptyMap)((acc, t) => helper(t, acc))
  }

  private def createEmptyDataMap(): HashMap[String, DataPoints] = {
    val emptyMap: HashMap[String, DataPoints] = HashMap.empty
    val emptyDP = DataPoints(0,0,0,0,0)
    val m1 =  emptyMap.updated("CstMut", emptyDP)
    val m2 =  m1.updated("ListMut", emptyDP)
    val m3 =  m2.updated("RecordSelectMut", emptyDP)
    val m4 =  m3.updated("CaseDeletion", emptyDP)
    val m5 =  m4.updated("VarMut", emptyDP)
    val m6 =  m5.updated("CaseSwitch", emptyDP)
    val m7 =  m6.updated("CompMut", emptyDP)
    val m8 =  m7.updated("IfMut", emptyDP)
    m8.updated("SigMut", emptyDP)
  }
  def createEmptyDataSetMap(module: String): Unit = {
    val empty: Map[String, Map[String, DataPoints]] = Map.empty
    writeDataToFile(empty.updated(module, createEmptyDataMap()))
  }
}
