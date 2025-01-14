package ca.uwaterloo.flix.language.ast

import org.scalatest.Suites

class AstSuite extends Suites(
  new SourcePositionSuite,
  new SourceLocationSuite,
  new TestChangeSet,
)

