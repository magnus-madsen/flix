package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.term.Term
import ca.uwaterloo.flix.runtime.solver.api.symbol.{TableSym, VarSym}

class AtomPredicate(sym: TableSym, positive: Boolean, terms: Array[Term], index2sym: Array[VarSym]) extends Predicate {

  /**
    * Returns the table symbol of the atom.
    */
  def getSym: TableSym = sym

  /**
    * Returns `true` if this atom is un-negated.
    */
  def isPositive: Boolean = positive

  /**
    * Returns `true` if this atom is negated.
    */
  def isNegative: Boolean = !positive

  /**
    * Returns the terms of the atom.
    */
  def getTerms: Array[Term] = terms

  // TODO: Deprecated.
  def getIndex2SymTEMPORARY: Array[VarSym] = index2sym

}
