/*
 *  Copyright 2024 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.Progress
import ca.uwaterloo.flix.language.phase.unification.set.Equation.Status
import ca.uwaterloo.flix.language.phase.unification.set.{Equation, SetFormula, SetSubstitution, SetUnification}
import ca.uwaterloo.flix.util.collection.SortedBimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object EffUnification3 {

  /**
    * Tries to solve the system of effect equality constraints `eqs`.
    *
    * Returns `(eqs', subst)` where `eqs'` are the remaining equations that were not solved and
    * `subst` is a most general unifier of the equations that were solved. Equations in `eqs'` are
    * not always unsolvable, maybe they just need more resolution of associated types for example.
    */
  def unifyAll(
                eqs: List[(Type, Type, SourceLocation)],
                scope: Scope, renv: RigidityEnv,
                opts: SetUnification.Options,
                progress: Progress,
              )(implicit flix: Flix): (List[(Type, Type, SourceLocation)], Substitution) = {
    // Add to implicit context.
    implicit val scopeImplicit: Scope = scope
    implicit val renvImplicit: RigidityEnv = renv
    implicit val optsImplicit: SetUnification.Options = SetUnification.Options.default

    // Listen for any progress made in SetUnification
    implicit val listener: SetUnification.SolverListener = new SetUnification.SolverListener {
      override def onExitPhase(state: SetUnification.State, progressMade: Boolean): Unit = {
        if (progressMade) {
          progress.markProgress()
        }
      }
    }

    // Choose a unique number for each atom.
    implicit val bimap: SortedBimap[Atom, Int] = mkBidirectionalVarMap(getAtomsFromEquations(eqs))

    // Convert type equations into formula equations.
    val equations = try {
      eqs.map(toEquation)
    } catch {
      // If any equation is not convertible to formulas, then give up for the whole system.
      // A more lenient approach would be to solve the convertible equations and give the rest back
      // unchanged.
      case InvalidType => return (eqs, Substitution.empty)
    }

    // Solve the equations and convert them back.
    val (setEqs, setSubst) = SetUnification.solve(equations)
    val typeEqs = setEqs.map(eq =>
      (fromSetFormula(eq.f1, eq.loc), fromSetFormula(eq.f2, eq.loc), eq.loc)
    )
    val subst = fromSetSubst(setSubst)
    (typeEqs, subst)
  }

  /**
    * Tries to unify the two effects: `eff1` and `eff2`.
    *
    *   - Returns [[Result.Err]] if the two effects are not unifiable (e.g. `Pure ~ IO`)
    *   - Returns [[Result.Ok]] of [[None]] if the two effects are not simplified enough to solve
    *     (e.g. `MyTrait.MyType[x] ~ Pure` where `x` is a flexible variable)
    *   - Returns [[Result.Ok]] of [[Some]] of a substitution if the two effects could be unified.
    *     The returned substitution is a most general unifier.
    */
  def unify(eff1: Type, eff2: Type, scope: Scope, renv: RigidityEnv)(implicit flix: Flix): Result[Option[Substitution], UnificationError] = {
    // Add to implicit context.
    implicit val scopeImplicit: Scope = scope
    implicit val renvImplicit: RigidityEnv = renv
    implicit val optsImplicit: SetUnification.Options = SetUnification.Options.default
    implicit val listener: SetUnification.SolverListener = SetUnification.SolverListener.DoNothing

    // Choose a unique number for each atom.
    implicit val bimap: SortedBimap[Atom, Int] = mkBidirectionalVarMap(Atom.getAtoms(eff1) ++ Atom.getAtoms(eff2))

    // Convert the type equation into a formula equation.
    val equation = try {
      toEquation(eff1, eff2, eff1.loc)
    } catch {
      // If `eff1` and `eff2` are not convertible then we cannot make progress.
      case InvalidType => return Result.Ok(None)
    }

    SetUnification.solve(List(equation)) match {
      case (Nil, subst) =>
        // The equation was solved, return the substitution.
        Result.Ok(Some(fromSetSubst(subst)))

      case (eq :: _, _) =>
        // The equation wasn't (completely) solved, return an error for the first unsolved equation.
        val Equation(f1, f2, status, loc) = eq
        status match {
          case Status.Pending =>
            // `SetUnification.solve` says it never returns pending equations.
            throw InternalCompilerException(s"Unexpected pending formula $eq", eq.loc)
          case Status.Unsolvable =>
            Result.Err(UnificationError.MismatchedEffects(fromSetFormula(f1, loc), fromSetFormula(f2, loc)))
          case Status.Timeout(msg) =>
            Result.Err(UnificationError.TooComplex(msg, eq.loc))
        }
    }
  }

  /** Returns a [[SortedBimap]] with each [[Atom]] having a unique number. */
  private def mkBidirectionalVarMap(atoms: SortedSet[Atom]): SortedBimap[Atom, Int] =
    SortedBimap.from(atoms.toList.zipWithIndex)

  /** Returns the union of [[Atom]]s for each [[Type]] in `eqs` using [[Atom.getAtoms]]. */
  private def getAtomsFromEquations(eqs: List[(Type, Type, SourceLocation)])(implicit scope: Scope, renv: RigidityEnv): SortedSet[Atom] = {
    eqs.foldLeft(SortedSet.empty[Atom]) {
      case (acc, (t1, t2, _)) => acc ++ Atom.getAtoms(t1) ++ Atom.getAtoms(t2)
    }
  }

  /**
    * Translates `eq` into a equation of [[SetFormula]].
    *
    * Throws [[InvalidType]] for types not convertible to [[SetFormula]].
    */
  private def toEquation(eq: (Type, Type, SourceLocation))(implicit scope: Scope, renv: RigidityEnv, m: SortedBimap[Atom, Int]): Equation = {
    val (tpe1, tpe2, loc) = eq
    Equation.mk(toSetFormula(tpe1), toSetFormula(tpe2), loc)
  }

  /**
    * Returns `t` as a [[SetFormula]].
    *
    * Throws [[InvalidType]] if `t` is not valid.
    */
  private def toSetFormula(t: Type)(implicit scope: Scope, renv: RigidityEnv, m: SortedBimap[Atom, Int]): SetFormula = t match {
    case Type.Univ => SetFormula.Univ
    case Type.Pure => SetFormula.Empty

    case tpe@Type.Var(_, _) =>
      val atom = Atom.fromType(tpe)
      m.getForward(atom) match {
        case None => throw InternalCompilerException(s"Unexpected unbound type variable: '$tpe'.", tpe.loc)
        case Some(x) => atom match {
          case Atom.VarFlex(_) => SetFormula.Var(x) // A flexible variable is a real variable.
          case Atom.VarRigid(_) => SetFormula.Cst(x) // A rigid variable is a constant.
          case _ => throw InternalCompilerException(s"Unexpected atom representation ($atom) of variable ($tpe)", tpe.loc)
        }
      }

    case tpe@Type.Cst(TypeConstructor.Effect(_), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.mkElemSet(x)
    }

    case tpe@Type.AssocType(_, _, _, _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound associated type: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.Cst(x)
    }

    case tpe@Type.Cst(TypeConstructor.Error(_, _), _) => m.getForward(Atom.fromType(tpe)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound error type: '$tpe'.", tpe.loc)
      case Some(x) => SetFormula.Var(x)
    }

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) =>
      SetFormula.mkCompl(toSetFormula(tpe1))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) =>
      SetFormula.mkUnion(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) =>
      SetFormula.mkInter(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), tpe1, _), tpe2, _) =>
      SetFormula.mkDifference(toSetFormula(tpe1), toSetFormula(tpe2))

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), tpe1, _), tpe2, _) =>
      val f1 = toSetFormula(tpe1)
      val f2 = toSetFormula(tpe2)
      SetFormula.mkXorDirect(f1, f2)

    case Type.Alias(_, _, tpe, _) => toSetFormula(tpe)

    case _ => throw InvalidType
  }

  /** Returns [[Substitution]] where each mapping in `s` is converted to [[Type]]. */
  private def fromSetSubst(s: SetSubstitution)(implicit m: SortedBimap[Atom, Int]): Substitution = {
    Substitution(s.m.foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type]) {
      case (macc, (k, SetFormula.Var(x))) if k == x => macc
      case (macc, (k, v)) =>
        m.getBackward(k) match {
          // A proper var. Add it to the substitution.
          case Some(Atom.VarFlex(sym)) =>
            macc + (sym -> fromSetFormula(v, sym.loc))
          // An error type. Don't add it to the substitution.
          case Some(Atom.Error(_)) =>
            macc
          // An invalid atom. Crash.
          case Some(other) => throw InternalCompilerException(s"unexpected non-variable mapped to variable: $other", SourceLocation.Unknown)
          // An unbound identifier. Crash.
          case None => throw InternalCompilerException(s"Unexpected unbound substitution identifier '$k'", SourceLocation.Unknown)
        }
    })
  }

  /**
    * Returns `f` as a type with location `loc`.
    *
    * Both constants and variables are mapped back to generic type variables. The rigidity
    * environment, in the type world, distinguishes their rigidity or flexibility.
    */
  private def fromSetFormula(f: SetFormula, loc: SourceLocation)(implicit m: SortedBimap[Atom, Int]): Type = f match {
    case SetFormula.Univ => Type.Univ
    case SetFormula.Empty => Type.Pure
    case SetFormula.Cst(c) => m.getBackward(c) match {
      case Some(atom) => Atom.toType(atom, loc)
      case None => throw InternalCompilerException(s"Unexpected unbound constant identifier '$c'", loc)
    }
    case SetFormula.Var(x) => m.getBackward(x) match {
      case Some(atom) => Atom.toType(atom, loc)
      case None => throw InternalCompilerException(s"Unexpected unbound variable identifier '$x'", loc)
    }
    case SetFormula.ElemSet(s) =>
      val elementTypes = s.toList.map(e => m.getBackward(e) match {
        case Some(atom) => Atom.toType(atom, loc)
        case None => throw InternalCompilerException(s"Unexpected unbound element identifier '$e'", loc)
      })
      Type.mkUnion(elementTypes, loc)
    case SetFormula.Compl(f1) => Type.mkComplement(fromSetFormula(f1, loc), loc)
    case inter@SetFormula.Inter(_, _, _, _, _, _, _) =>
      Type.mkIntersection(inter.mapSubformulas(fromSetFormula(_, loc)), loc)
    case union@SetFormula.Union(_) =>
      Type.mkUnion(union.mapSubformulas(fromSetFormula(_, loc)), loc)
    case SetFormula.Xor(other) =>
      Type.mkSymmetricDiff(other.map(fromSetFormula(_, loc)), loc)
  }

  /**
    * Atomic effects that can be represented as atoms in [[SetFormula]]. Atomic effects are
    * variables, effects, errors, or simple associated effects, described in the grammar below.
    *
    * atom ::= VarFlex | VarRigid | Eff | Error | atomAssoc
    * atomAssoc ::= Assoc atomAssoc | VarRigid
    */
  private sealed trait Atom extends Ordered[Atom] {
    override def compare(that: Atom): Int = (this, that) match {
      case (Atom.VarFlex(sym1), Atom.VarFlex(sym2)) => sym1.id - sym2.id
      case (Atom.VarRigid(sym1), Atom.VarRigid(sym2)) => sym1.id - sym2.id
      case (Atom.Eff(sym1), Atom.Eff(sym2)) => sym1.compare(sym2)
      case (Atom.Assoc(sym1, arg1), Atom.Assoc(sym2, arg2)) =>
        val symCmp = sym1.compare(sym2)
        if (symCmp != 0) symCmp else arg1.compare(arg2)
      case (Atom.Error(id1), Atom.Error(id2)) => id1 - id2
      case _ =>
        def ordinal(a: Atom): Int = a match {
          case Atom.VarFlex(_) => 0
          case Atom.VarRigid(_) => 1
          case Atom.Eff(_) => 2
          case Atom.Assoc(_, _) => 3
          case Atom.Error(_) => 4
        }

        ordinal(this) - ordinal(that)
    }
  }

  private object Atom {
    /** Representing a flexible variable. */
    case class VarFlex(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing a rigid variable. */
    case class VarRigid(sym: Symbol.KindedTypeVarSym) extends Atom

    /** Representing an effect constant. */
    case class Eff(sym: Symbol.EffectSym) extends Atom

    /** Represents an associated effect. */
    case class Assoc(sym: Symbol.AssocTypeSym, arg: Atom) extends Atom

    /** Represents an error type. */
    case class Error(id: Int) extends Atom

    /** Returns the [[Atom]] representation of `t` or throws [[InvalidType]]. */
    @tailrec
    def fromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.VarRigid(sym)
      case Type.Var(sym, _) => Atom.VarFlex(sym)
      case Type.Cst(TypeConstructor.Effect(sym), _) => Atom.Eff(sym)
      case assoc@Type.AssocType(_, _, _, _) => assocFromType(assoc)
      case Type.Cst(TypeConstructor.Error(id, _), _) => Atom.Error(id)
      case Type.Alias(_, _, tpe, _) => fromType(tpe)
      case _ => throw InvalidType
    }

    /** Returns the [[Atom]] representation of `t` or throws [[InvalidType]]. */
    private def assocFromType(t: Type)(implicit scope: Scope, renv: RigidityEnv): Atom = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Atom.VarRigid(sym)
      case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) => Atom.Assoc(sym, assocFromType(arg))
      case Type.Alias(_, _, tpe, _) => assocFromType(tpe)
      case _ => throw InvalidType
    }

    /**
      * Returns the set of valid [[Atom]]s that occur in `t` (according to [[Atom.fromType]]).
      * Invalid or unrelated types are ignored.
      *
      * The validity of atoms are checked top-down, so even though `MyTrait.MyType[x]` is a valid
      * atom where `x` is rigid, `getAtoms(MyTrait.MyType[MyTrait.MyType[x] ∪ IO])` will return
      * `Set.empty` since the outermost associated type is not valid. This behaviour aligns with the
      * needs of [[toSetFormula]].
      *
      * Examples:
      *   - `getAtoms(Crash ∪ ef) = Set(Eff(Crash), VarFlex(ef))` (if [[RigidityEnv.isRigid]] is
      *     false for `ef`)
      *   - `getAtoms(Indexable.Aef[Error]) = Set.empty`
      */
    def getAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): SortedSet[Atom] = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => SortedSet(Atom.VarRigid(sym))
      case Type.Var(sym, _) => SortedSet(Atom.VarFlex(sym))
      case Type.Cst(TypeConstructor.Effect(sym), _) => SortedSet(Atom.Eff(sym))
      case Type.Cst(TypeConstructor.Error(id, _), _) => SortedSet(Atom.Error(id))
      case Type.Apply(tpe1, tpe2, _) => getAtoms(tpe1) ++ getAtoms(tpe2)
      case Type.Alias(_, _, tpe, _) => getAtoms(tpe)
      case assoc@Type.AssocType(_, _, _, _) => SortedSet.from(getAssocAtoms(assoc))
      case _ => SortedSet.empty
    }

    /**
      * Returns the [[Atom]] of `t` if it is a valid associated [[Atom]] (according to
      * [[Atom.assocFromType]]). Invalid or unrelated types return [[None]].
      */
    private def getAssocAtoms(t: Type)(implicit scope: Scope, renv: RigidityEnv): Option[Atom] = t match {
      case Type.Var(sym, _) if renv.isRigid(sym) => Some(Atom.VarRigid(sym))
      case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) =>
        getAssocAtoms(arg).map(Atom.Assoc(sym, _))
      case Type.Alias(_, _, tpe, _) => getAssocAtoms(tpe)
      case _ => None
    }

    /**
      * Returns the [[Type]] represented by `atom` with location `loc`. The kind of errors and
      * associated types are set to be [[Kind.Eff]].
      */
    def toType(atom: Atom, loc: SourceLocation)(implicit m: SortedBimap[Atom, Int]): Type = atom match {
      case Atom.Eff(sym) => Type.Cst(TypeConstructor.Effect(sym), loc)
      case Atom.VarRigid(sym) => Type.Var(sym, loc)
      case Atom.VarFlex(sym) => Type.Var(sym, loc)
      case Atom.Assoc(sym, arg0) =>
        Type.AssocType(AssocTypeSymUse(sym, loc), toType(arg0, loc), Kind.Eff, loc)
      case Atom.Error(id) => Type.Cst(TypeConstructor.Error(id, Kind.Eff), loc)
    }
  }

  /**
    * An exception used for partial functions that convert [[Type]] into [[Atom]].
    *
    * This exception should not leak outside this phase - it should always be caught. It is used to
    * avoid having [[Option]] types on recursive functions.
    */
  private case object InvalidType extends RuntimeException

}
