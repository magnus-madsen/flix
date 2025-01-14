package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.SecurityContext.AllPermissions
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input, Source}
import ca.uwaterloo.flix.util.collection.MultiMap
import org.scalatest.funsuite.AnyFunSuite

class TestChangeSet extends AnyFunSuite {
  case class MySourceable(input: Input) extends Sourceable {
    override def src: Source = Source(input, Array.emptyCharArray)
  }

  private def mkInput(name: String): Input = Input.Text(name, "", AllPermissions)

  private val input1 = mkInput("input1")
  private val input2 = mkInput("input2")
  private val input3 = mkInput("input3")
  private val input4 = mkInput("input4")
  private val input5 = mkInput("input5")

  private val src1 = MySourceable(input1)
  private val src2 = MySourceable(input2)
  private val src3 = MySourceable(input3)
  private val src4 = MySourceable(input4)
  private val src5 = MySourceable(input5)

  private val dg1 = DependencyGraph.empty
  private val dg2 = DependencyGraph(MultiMap(Map(
    input1 -> Set(input2, input3), input2 -> Set(input4), input3 -> Set(input5)
  )))

  test("ChangeSet.Everything.partition should make everything stale") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src3 -> 3, src4 -> 4)

    val cs = ChangeSet.Everything
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == newMap)
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partition should put anything changed into staleMap.01") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src1 -> 3, src2 -> 2)

    val cs = ChangeSet.Everything.markChanged(input1, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src1 -> 3))
    assert(freshMap == Map(src2 -> 2))
  }

  test("ChangeSet.partition should put anything changed into staleMap.02") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5)
    val newMap = Map(src1 -> 3, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5)

    val cs = ChangeSet.Everything.markChanged(input1, dg2)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src1 -> 3, src2 -> 2, src3 -> 3, src4 -> 4, src5 -> 5))
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partition should put anything new into staleMap") {
    val oldMap = Map(src1 -> 1, src2 -> 2)
    val newMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)

    val cs = ChangeSet.Everything.markChanged(input3, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src3 -> 3))
    assert(freshMap == Map(src1 -> 1, src2 -> 2))
  }

  test("ChangeSet.partition should ignore anything deleted") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)
    val newMap = Map(src1 -> 1, src2 -> 2)

    val cs = ChangeSet.Everything.markChanged(input3, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map.empty)
    assert(freshMap == Map(src1 -> 1, src2 -> 2))
  }

  test("ChangeSet.partition test with both newed and deleted input") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3)
    val newMap = Map(src2 -> 2, src3 -> 3, src4 -> 4)

    val cs = ChangeSet.Everything.markChanged(input1, dg1).markChanged(input4, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src4 -> 4))
    assert(freshMap == Map(src2 -> 2, src3 -> 3))
  }

  test("ChangeSet.partition test with new, deleted and changed input") {
    val oldMap = Map(src1 -> 1, src2 -> 2, src3 -> 3, src5 -> 5)
    val newMap = Map(src2 -> 2, src3 -> 30, src4 -> 4, src5 -> 5)

    val cs = ChangeSet.Everything.markChanged(input1, dg1).markChanged(input3, dg2).markChanged(input4, dg1)
    val (staleMap, freshMap) = cs.partition(newMap, oldMap)

    assert(staleMap == Map(src3 -> 30, src4 -> 4, src5 -> 5))
    assert(freshMap == Map(src2 -> 2))
  }

  test("ChangeSet.Everything.partitionOnValues should make everything stale") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = Map(3 -> List(src3), 4 -> List(src4, src5))

    val cs = ChangeSet.Everything
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == newMap)
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partitionOnValues should put anything changed into staleMap.01") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2))

    val cs = ChangeSet.Everything.markChanged(input1, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map(1 -> List(src1)))
    assert(freshMap == Map(1 -> List(src2), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues should put anything changed into staleMap.02") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5))

    val cs = ChangeSet.Everything.markChanged(input1, dg2)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src3), 4 -> List(src4), 5 -> List(src5)))
    assert(freshMap == Map.empty)
  }

  test("ChangeSet.partitionOnValues should put anything new into staleMap") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))

    val cs = ChangeSet.Everything.markChanged(input3, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map(2 -> List(src3), 3 -> List(src4)))
    assert(freshMap == Map(1 -> List(src1, src2), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues should ignore anything deleted") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2))

    val cs = ChangeSet.Everything.markChanged(input3, dg1).markChanged(input4, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map.empty)
    assert(freshMap == Map(1 -> List(src1, src2), 2 -> List(src2)))
  }

  test("ChangeSet.partitionOnValues test with both newed and deleted input") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src4))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src4, src5))

    val cs = ChangeSet.Everything.markChanged(input3, dg1).markChanged(input5, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map(3 -> List(src5)))
    assert(freshMap == Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src4)))
  }

  test("ChangeSet.partitionOnValues test with new, deleted and changed input") {
    val oldMap = Map(1 -> List(src1, src2), 2 -> List(src2, src3), 3 -> List(src5))
    val newMap = Map(1 -> List(src1, src2), 2 -> List(src2), 3 -> List(src4, src5))

    val cs = ChangeSet.Everything.markChanged(input1, dg1).markChanged(input3, dg1).markChanged(input4, dg1)
    val (staleMap, freshMap) = cs.partitionOnValues(newMap, oldMap)

    assert(staleMap == Map(1 -> List(src1), 3 -> List(src4)))
    assert(freshMap == Map(1 -> List(src2), 2 -> List(src2), 3 -> List(src5)))
  }
}
