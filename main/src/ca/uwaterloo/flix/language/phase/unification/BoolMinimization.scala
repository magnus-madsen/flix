/*
 *  Copyright 2022 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object BoolMinimization {

  sealed trait Formula

  object Formula {

    case object True extends Formula

    case object False extends Formula

    case class Var(v: Type.KindedVar) extends Formula

    case class Not(term: Formula) extends Formula

    case class And(terms: List[Formula]) extends Formula

    case class Or(terms: List[Formula]) extends Formula

  }

  /**
    * Sort terms `True` -> `False` -> `Var`s (by name then negation) ->
    * complex negation -> conjunctions -> disjunctions.
    */
  def sortTerms(terms: List[Formula]): List[Formula] = {
    import Formula._
    terms.sortWith {
      case (True, True) => false
      case (True, False | Var(_) | Not(_) | And(_) | Or(_)) => true

      case (False, True | False) => false
      case (False, Var(_) | Not(_) | And(_) | Or(_)) => true

      case (Var(_), True | False) => false

      case (Var(v1), Var(v2)) => v1 < v2
      case (Var(v1), Not(Var(v2))) if v1 == v2 => true
      case (Var(v1), Not(Var(v2))) => v1 < v2
      case (Not(Var(v1)), Var(v2)) if v1 == v2 => false
      case (Not(Var(v1)), Var(v2)) => v1 < v2
      case (Not(Var(v1)), Not(Var(v2))) => v1 < v2
      case (Var(_), Not(_) | And(_) | Or(_)) => true

      case (Not(_), True | False | Var(_)) => false
      case (Not(_), Not(_)) => false
      case (Not(_), And(_) | Or(_)) => true

      case (And(_), True | False | Var(_) | Not(_)) => false
      case (And(_), And(_)) => false
      case (And(_), Or(_)) => true

      case (Or(_), True | False | Var(_) | Not(_) | And(_) | Or(_)) => false
    }
  }

  /**
    * Reduces things like `y ∨ y` to `y` or `¬x ∧ ¬x` to `x`. Returns `None` if
    * the list contains both `x` and `¬x` for some `x`.
    * Assumes the list is sorted.
    */
  def removeSimpleDuplicates(terms: List[BoolMinimization.Formula]): Option[List[Formula]] = {
    import Formula._
    terms match {
      case Nil =>
        Some(Nil)
      case _ :: Nil =>
        Some(terms)
      case Var(v1) :: Var(v2) :: rest if v1 == v2 =>
        removeSimpleDuplicates(Var(v2) :: rest)
      case Not(Var(v1)) :: Not(Var(v2)) :: rest if v1 == v2 =>
        removeSimpleDuplicates(Not(Var(v2)) :: rest)
      case Var(v1) :: Not(Var(v2)) :: _ if v1 == v2 =>
        None
      case other :: rest =>
        removeSimpleDuplicates(rest).map(other :: _)
    }
  }

  def unitPropagation(terms: List[Formula]): List[Formula] = {
    import Formula._
    def removeVars(f: Formula, trues: Set[Var]): Option[Formula] = f match {
      case True => Some(True)
      case False => Some(False)
      case v: Var if trues.contains(v) => None
      case v: Var => Some(v)
      case Not(v: Var) if trues.contains(v) => None
      case Not(_) => throw InternalCompilerException(s"Found complex negation after NNF ${show(f)}")
      case And(_) => throw InternalCompilerException(s"Found conjunction in input ${show(f)}")
      case Or(terms) => ???
    }

    def aux(terms0: List[Formula], trues: Set[Var]): List[Formula] = terms0 match {
      case Nil => Nil
      case (v: Var) :: rest => v :: aux(rest, trues + v)
      case f :: rest if trues.nonEmpty => ??? // remove(f, trues)
      case _ => ???
    }

    aux(terms, Set.empty)
  }

  /**
    * Build `Not(f)` and performs shallow minimization.
    */
  def mkNot(f: Formula): Formula = {
    import Formula._
    f match {
      case True => False
      case False => True
      case Var(v) => Not(Var(v))
      case Not(term) => term
      case And(terms) => Not(And(terms))
      case Or(terms) => Not(Or(terms))
    }
  }

  /**
    * Merges the conjunctions if arguments are conjunctions themselves and
    * performs shallow minimization.
    */
  def mkAnd(f: Formula*): Formula = {
    mkAnd(f.toList)
  }

  /**
    * Merges the conjunctions in `f` into the new conjunction and performs
    * shallow minimization.
    */
  def mkAnd(f: List[Formula]): Formula = {
    import Formula._
    val baseValue: Option[List[Formula]] = Some(Nil)
    val hoistedTerms = f.foldRight(baseValue) {
      case (term, opt) => opt.flatMap(
        acc => term match {
          case True => Some(acc)
          case False => None
          case v: Var => Some(v :: acc)
          case n: Not => Some(n :: acc)
          case And(terms) => Some(terms ++ acc)
          case or: Or => Some(or :: acc)
        }
      )
    }
    hoistedTerms match {
      case None => False
      case Some(term :: Nil) => term
      case Some(terms) =>
        normalizeTerms(terms) match {
          case None => False
          case Some(normalizedTerms) => And(normalizedTerms)
        }
    }
  }

  /**
    * Sorts terms, reduces simple dublicates and returns `None` if the terms
    * contains a simple contradiction.
    */
  def normalizeTerms(terms: List[Formula]): Option[List[Formula]] = {
    removeSimpleDuplicates(sortTerms(terms))
  }

  /**
    * Merges the disjunctions if arguments are disjunctions themselves and
    * perfoms shallow minimization.
    */
  def mkOr(f: Formula*): Formula = {
    mkOr(f.toList)
  }

  /**
    * Merges the disjunctions in `f` into the new disjunction and performs
    * shallow minimization.
    */
  def mkOr(f: List[Formula]): Formula = {
    import Formula._
    val baseValue: Option[List[Formula]] = Some(Nil)
    val hoistedTerms = f.foldRight(baseValue) {
      case (term, opt) => opt.flatMap(
        acc => term match {
          case True => None
          case False => Some(acc)
          case v: Var => Some(v :: acc)
          case n: Not => Some(n :: acc)
          case and: And => Some(and :: acc)
          case Or(terms) => Some(terms ++ acc)
        }
      )
    }
    hoistedTerms match {
      case None => True
      case Some(term :: Nil) => term
      case Some(terms) =>
        normalizeTerms(terms) match {
          case None => True
          case Some(normalizedTerms) => Or(normalizedTerms)
        }
    }
  }

  /**
    * Transform `f` to an equivalent formula where negation only appears
    * on variables directly.
    */
  def toNNF(f: Formula): Formula = {
    import Formula._
    f match {
      case True => True

      case False => False

      case Var(v) => Var(v)

      case not: Not => not.term match {
        case True => False
        case False => True
        case Var(v) => mkNot(Var(v))
        case Not(term) => term
        case And(terms) => toNNF(mkOr(terms.map(t => mkNot(t))))
        case Or(terms) => toNNF(mkAnd(terms.map(t => mkNot(t))))
      }

      case And(terms) => mkAnd(terms.map(toNNF))

      case Or(terms) => mkOr(terms.map(toNNF))

    }
  }

  /**
    * Transform `f` to an equivalent formula that is a conjunction of
    * disjunctions of either variables or negated variables.
    * Ex. `x ∧ (x ∨ y) ∧ (z ∨ ¬y ∨ ¬z)`
    */
  def toCNF(f: Formula): Formula = {
    def aux(f0: Formula): Formula = {
      import Formula._
      f0 match {
        case True => True
        case False => False
        case Var(v) => Var(v)
        case Not(Var(v)) => Not(Var(v))
        case Not(_) => throw InternalCompilerException(s"Found complex negation after NNF ${show(f0)}")
        case And(terms) => mkAnd(terms.map(toCNF))
        case Or(terms) =>
          // Note: this code relies on the ordering of simple terms before conjunctions
          terms.foldLeft(False: Formula)(
            (cnf, term) => term match {
              case True => mkOr(cnf, True)
              case False => mkOr(cnf, False)
              case Var(v) => mkOr(cnf, Var(v))
              case Not(Var(v)) => mkOr(cnf, Not(Var(v)))
              case Not(_) => throw InternalCompilerException(s"Found complex negation after NNF ${show(term)}")
              case And(terms0) => toCNF(mkAnd(terms0.map(t => mkOr(cnf, t))))
              case Or(_) => throw InternalCompilerException(s"Found directly nested Or: ${show(f0)}")
            })
      }
    }

    aux(toNNF(f))
  }

  /**
    * Transform `f` to an equivalent formula that is a disjunction of
    * conjunctions of either variables or negated variables.
    * Ex. `x ∨ (x ∧ y) ∨ (z ∧ ¬y ∧ ¬z)`
    */
  def toDNF(f: Formula): Formula = ???

  def main(args: Array[String]): Unit = {
    import Formula._
    def mkVar(s: String): Var = Var(
      Type.KindedVar(
        new Symbol.KindedTypeVarSym(
          s.hashCode,
          Some(s),
          Kind.Bool,
          Rigidity.Flexible,
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      )
    )

    val formulas = List(
      mkAnd(mkVar("x"), mkVar("y")),
      mkOr(mkVar("y"), mkVar("z")),
      mkAnd(mkOr(mkVar("q"), mkAnd(mkVar("q"), mkVar("h"))), mkNot(mkVar("x")), mkVar("y"), mkVar("x")),
      mkAnd(mkOr(mkVar("x"), mkVar("y")), mkOr(mkVar("x"), mkVar("z"))),
      mkNot(mkAnd(mkOr(mkVar("x"), mkNot(mkVar("y"))), mkNot(mkOr(mkVar("x"), mkVar("z"))))),
      mkAnd(mkOr(mkVar("z"), mkNot(mkAnd(mkVar("z"), mkVar("Q")))), mkNot(mkAnd(mkOr(mkVar("x"), mkNot(mkVar("y"))), mkNot(mkOr(mkVar("x"), mkVar("z"))))))
    )

    def run(msg: String, t: Formula => Formula): Unit = {
      println(msg)
      val delim = "----"
      formulas.foreach(f => {
        println(delim)
        println(show(f))
        println(show(t(f)))
      })
      println(delim)
    }

    run("NNF", toNNF)
    run("CNF", toCNF)
  }

  /**
    * Creates a string representation of the formula.
    */
  def show(f: Formula): String = {
    def showAux(f: Formula): (String, Boolean) = {
      def showTermBraces(term: Formula): String = {
        val (termStr, paran) = showAux(term)
        if (paran) s"($termStr)" else termStr
      }

      f match {
        case Formula.True => ("T", false)
        case Formula.False => ("F", false)
        case Formula.Var(v) => (v.sym.text.getOrElse("???"), false)
        case Formula.Not(term) =>
          val (termStr, paran) = showAux(term)
          val rep = if (paran) s"($termStr)" else termStr
          (s"¬$rep", false)
        case Formula.And(terms) =>
          val rep = terms.map(showTermBraces).mkString(" ∧ ")
          (rep, true)
        case Formula.Or(terms) =>
          val rep = terms.map(showTermBraces).mkString(" ∨ ")
          (rep, true)
      }
    }

    showAux(f)._1
  }

}
