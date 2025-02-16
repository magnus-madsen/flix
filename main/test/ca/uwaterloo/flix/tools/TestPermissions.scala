package ca.uwaterloo.flix.tools

import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Permissions

class TestPermissions extends AnyFunSuite {
  test("toString-ofString-none") {
    val perm = Permissions.FlixOnly
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-restricted") {
    val perm = Permissions.Restricted
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-all") {
    val perm = Permissions.All
    val res = Permissions.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }
}
