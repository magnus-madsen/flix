package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.predicate._

import scala.collection.mutable

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(strata: Array[Stratum]) {

  // TODO: Replace stratum by a number and then just do group by.

  /**
    * Returns all the relation values in the constraint set.
    */
  def getRelations(): Array[Relation] = getAllRelations()

  /**
    * Returns all the lattice values in the constraint set.
    */
  def getLattices(): Array[Lattice] = getAllLattices()

  /**
    * Returns the strata in the constraint set.
    */
  def getStrata(): Array[Stratum] = strata

  /**
    * Returns the union of `this` constraint set with `that` constraint set.
    */
  def union(that: ConstraintSet): ConstraintSet = {
    // TODO: Correctness. This is just a hack for now.

    // TODO: What about duplicates?
    val newStrata = (this.getStrata() zip that.getStrata()) map {
      case (stratum1, stratum2) => new Stratum(stratum1.getConstraints() ++ stratum2.getConstraints())
    }

    new ConstraintSet(newStrata)
  }


  /**
    * Returns a new constraint set without any place holders.
    */
  // TODO: Move
  def complete(): ConstraintSet = {
    // TODO: Cleanup.
    val relationPlaceholders = getRelationPlaceholders().groupBy(_.getName())
    val latticePlaceholders = getLatticePlaceholders().groupBy(_.getName())

    // Introduce a proper relation for each relation placeholder.
    val newRelations = relationPlaceholders map {
      case (name, placeholders) => {
        val placeholder = placeholders(0) // guaranteed to be non-empty.
        val attr = placeholder.attr
        name -> new Relation(name, attr)
      }
    }

    val newLattices = latticePlaceholders map {
      case (name, placeholders) => {
        val placeholder = placeholders(0)
        name -> new Lattice(name, placeholder.keys, placeholder.value, placeholder.ops)
      }
    }

    def replace(c: Constraint): Constraint = {
      val head = replacePredicate(c.getHeadPredicate())
      val body = c.getBodyPredicates().map(replacePredicate)
      new Constraint(c.getParams(), head, body)
    }

    def replacePredicate(p0: Predicate): Predicate = p0 match {
      case p: AtomPredicate =>
        val sym = p.getSym() match {
          case r: RelationPlaceholder => newRelations(r.getName())
          case l: LatticePlaceholder => newLattices(l.getName())
          case _ => p.getSym()
        }
        new AtomPredicate(sym, p.isPositive(), p.getTerms(), p.getIndex2SymTEMPORARY)
      case _ => p0
    }

    val newStrata = strata map {
      stratum => new Stratum(stratum.getConstraints().map(replace))
    }

    new ConstraintSet(newStrata)
  }

  /**
    * Returns a human readable representation the constraint set.
    */
  override def toString: String = strata.mkString(", ")

  /**
    * Computes all relations in the constraint set.
    */
  private def getAllRelations(): Array[Relation] = getTables() collect {
    case r: Relation => r
  }

  /**
    * Computes all lattices in the constraint set.
    */
  private def getAllLattices(): Array[Lattice] = getTables() collect {
    case l: Lattice => l
  }

  /**
    * Computes all placeholder relations in the constraint set.
    */
  private def getRelationPlaceholders(): Array[RelationPlaceholder] = getTables() collect {
    case r: RelationPlaceholder => r
  }

  /**
    * Computes all lattice placeholders in the constraint set.
    */
  private def getLatticePlaceholders(): Array[LatticePlaceholder] = getTables() collect {
    case l: LatticePlaceholder => l
  }

  /**
    * Returns all tables in the constraint set.
    */
  private def getTables(): Array[Table] =
    getAtomPredicates().map(_.getSym()).distinct

  /**
    * Returns all predicates in the constraint set.
    */
  private def getAtomPredicates(): Array[AtomPredicate] = {
    val constraints = strata.flatMap(_.getConstraints())
    // TODO: Introduce better helper.
    constraints.flatMap {
      case c => c.getHeadPredicate() match {
        case h: AtomPredicate => h +: c.getAtoms()
        case _ => c.getAtoms()
      }
    }
  }

}
