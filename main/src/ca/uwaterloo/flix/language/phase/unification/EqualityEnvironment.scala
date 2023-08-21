/*
 * Copyright 2023 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, LevelEnv, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object EqualityEnvironment {

  /**
    * Checks that the given `econstrs` entail the given `econstr`.
    */
  def entail(econstrs: List[Ast.EqualityConstraint], econstr: Ast.BroadEqualityConstraint, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, UnificationError] = {
    // create assoc-type substitution using econstrs
    val subst = toSubst(econstrs)

    // extract the types
    val Ast.BroadEqualityConstraint(tpe1, tpe2) = econstr

    // apply the substitution to them
    val newTpe1 = subst(tpe1)
    val newTpe2 = subst(tpe2)

    val res1 = reduceType2(newTpe1, eqEnv)
    val res2 = reduceType2(newTpe2, eqEnv)
    // check that econstr becomes tautological (according to global instance map)
    for {
//      res1 <- reduceType(newTpe1, eqEnv) // TODO ASSOC-TYPES
//      res2 <- reduceType(newTpe2, eqEnv)
      res <- unifyWithHiddenAssocTypes(res1, res2, renv, LevelEnv.Top) match {
        case Result.Ok((subst, Nil)) => Result.Ok(subst): Result[Substitution, UnificationError]
        case Result.Ok((_, _ :: _)) => Result.Err(UnificationError.UnsupportedEquality(res1, res2)): Result[Substitution, UnificationError]
        case Result.Err(_) => Result.Err(UnificationError.UnsupportedEquality(res1, res2): UnificationError): Result[Substitution, UnificationError]
      }
      // TODO ASSOC-TYPES weird typing hack
    } yield res
  }.toValidation

  // MATT docs
  def entailAll(econstrs1: List[Ast.EqualityConstraint], econstrs2: List[Ast.BroadEqualityConstraint], renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, UnificationError] = {
    // split by kind
    val (effEconstrs, tpeEconstrs) = econstrs2.partition {
      case Ast.BroadEqualityConstraint(tpe1, _) => tpe1.kind == Kind.Bool
    }

    // We process the tpe econstrs first to be sure that vars in eff econstrs are fully resolved
    Validation.fold(tpeEconstrs ::: effEconstrs, Substitution.empty) {
      case (subst, econstr) =>
        val newEconstrs1 = econstrs1.map(subst.apply)
        val newEconstr = subst.apply(econstr)
        entail(newEconstrs1, newEconstr, renv, eqEnv)
    }
  }

  // MATT docs
  def unifyWithHiddenAssocTypes(tpe10: Type, tpe20: Type, renv0: RigidityEnv, lenv: LevelEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    val assocs = getAssocTypes(tpe10) ++ getAssocTypes(tpe20)

    val rigidAssocs = assocs.filter {
      case (_, tvar, _) => renv0.isRigid(tvar)
    }

    val aliases = rigidAssocs.map {
      case (assoc, tvar, kind) => (assoc, tvar, Type.freshVar(kind, SourceLocation.Unknown))
    }

    val renv = aliases.foldLeft(renv0) {
      case (renv1, (_, _, alias)) => renv1.markRigid(alias.sym)
    }

    val assocSubst = aliases.foldLeft(AssocTypeSubstitution.empty) {
      case (acc, (assoc, tvar, alias)) => acc ++ AssocTypeSubstitution.singleton(assoc, tvar, alias)
    }

    val backSubst = aliases.foldLeft(Substitution.empty) {
      case (acc, (assoc, tvar, alias)) =>
        acc ++ Substitution.singleton(alias.sym, Type.AssocType(Ast.AssocTypeConstructor(assoc, SourceLocation.Unknown), Type.Var(tvar, SourceLocation.Unknown), alias.kind, SourceLocation.Unknown))
    }

    val tpe1 = assocSubst(tpe10)
    val tpe2 = assocSubst(tpe20)

    Unification.unifyTypes(tpe1, tpe2, renv, lenv).map {
      case (subst, econstrs) => (backSubst @@ subst, econstrs.map(backSubst.apply))
    }
  }

  // MATT docs
  def getAssocTypes(tpe: Type): Set[(Symbol.AssocTypeSym, Symbol.KindedTypeVarSym, Kind)] = tpe match {
    case Type.Var(_, _) => Set.empty
    case Type.Cst(_, _) => Set.empty
    case Type.Apply(tpe1, tpe2, _) => getAssocTypes(tpe1) ++ getAssocTypes(tpe2)
    case Type.Alias(_, _, tpe, _) => getAssocTypes(tpe) // TODO ASSOC-TYPES handle aliases
    case Type.AssocType(Ast.AssocTypeConstructor(assoc, _), Type.Var(tvar, _), k, _) => Set((assoc, tvar, k))
    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("unexpected non-HNF assoc type", loc)
  }

  /**
    * Converts the given EqualityConstraint into a BroadEqualityConstraint.
    */
  def narrow(econstr: Ast.BroadEqualityConstraint): Ast.EqualityConstraint = econstr match {
    case Ast.BroadEqualityConstraint(Type.AssocType(cst, tpe1, _, _), tpe2) =>
      Ast.EqualityConstraint(cst, tpe1, tpe2, SourceLocation.Unknown)
    case _ => throw InternalCompilerException("unexpected broad equality constraint", SourceLocation.Unknown)
  }

  /**
    * Converts the given Equality
    */
  def broaden(econstr: Ast.EqualityConstraint): Ast.BroadEqualityConstraint = econstr match {
    case Ast.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      Ast.BroadEqualityConstraint(Type.AssocType(cst, tpe1, Kind.Wild, loc), tpe2)
  }

  /**
    * Converts the list of equality constraints to a substitution.
    */
  private def toSubst(econstrs: List[Ast.EqualityConstraint]): AssocTypeSubstitution = {
    econstrs.foldLeft(AssocTypeSubstitution.empty) {
      case (acc, Ast.EqualityConstraint(Ast.AssocTypeConstructor(sym, _), Type.Var(tvar, _), tpe2, _)) =>
        acc ++ AssocTypeSubstitution.singleton(sym, tvar, tpe2)
      case (_, Ast.EqualityConstraint(cst, tpe1, tpe2, loc)) => throw InternalCompilerException("unexpected econstr", loc) // TODO ASSOC-TYPES
    }
  }

  /**
    * Reduces the associated type in the equality environment.
    *
    * Only performs one reduction step. The result may itself contain associated types.
    */
  def reduceAssocTypeStep(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
    val renv = arg.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
    val insts = eqEnv(cst.sym)
    insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
      inst =>
        Unification.unifyTypes(arg, inst.arg, renv, LevelEnv.Top).toOption.map { // TODO level env?
          case (subst, econstrs) => subst(inst.ret) // TODO ASSOC-TYPES consider econstrs
        }
    }.nextOption() match {
      case None => Result.Err(UnificationError.IrreducibleAssocType(cst.sym, arg))
      case Some(t) => Result.Ok(t)
    }
  }

  /**
    * Reduces the associated type in the equality environment.
    *
    * Only performs one reduction step. The result may itself contain associated types.
    */
  def reduceAssocTypeStep2(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Option[Type] = { // TODO ASSOC-TYPES better thing than option
    val renv = arg.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
    val insts = eqEnv(cst.sym)
    insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
      inst =>
        Unification.unifyTypes(arg, inst.arg, renv, LevelEnv.Top).toOption.map { // TODO level env?
          case (subst, econstrs) => subst(inst.ret) // TODO ASSOC-TYPES consider econstrs
        }
    }.nextOption()
  }

  /**
    * Fully reduces the given associated type.
    */
  def reduceAssocType(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
    for {
      tpe <- reduceAssocTypeStep(cst, arg, eqEnv)
      res <- reduceType(tpe, eqEnv)
    } yield res
  }

  /**
    * Fully reduces the given associated type.
    */
  def reduceAssocType2(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Option[Type] = {
    reduceAssocTypeStep2(cst, arg, eqEnv) map (reduceType2(_, eqEnv))
  }

  /**
    * Reduces associated types in the equality environment.
    */
  def reduceType(t0: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
    // TODO ASSOC-TYPE require that AssocTypeDefs which themselves include assoc types are supported by tconstrs
    def visit(t: Type): Result[Type, UnificationError] = t match {
      case t: Type.Var => Result.Ok(t)
      case t: Type.Cst => Result.Ok(t)
      case Type.Apply(tpe1, tpe2, loc) =>
        visit(tpe1)
        for {
          t1 <- visit(tpe1)
          t2 <- visit(tpe2)
        } yield Type.Apply(t1, t2, loc)
      case Type.Alias(cst, args0, tpe0, loc) =>
        for {
          args <- Result.traverse(args0)(visit)
          tpe <- visit(tpe0)
        } yield Type.Alias(cst, args, tpe, loc)
      case Type.AssocType(cst, arg0, kind, loc) =>
        for {
          arg <- visit(arg0)
          res0 <- reduceAssocTypeStep(cst, arg, eqEnv)
          res <- visit(res0)
        } yield res
    }

    visit(t0)
  }

  /**
    * Reduces associated types in the equality environment.
    */
  def reduceType2(t0: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Type = {
    // TODO ASSOC-TYPE require that AssocTypeDefs which themselves include assoc types are supported by tconstrs
    def visit(t: Type): Type = t match {
      case t: Type.Var => t
      case t: Type.Cst => t
      case Type.Apply(tpe1, tpe2, loc) =>
        Type.Apply(visit(tpe1), visit(tpe2), loc)
      case Type.Alias(cst, args0, tpe0, loc) =>
        Type.Alias(cst, args0.map(visit), visit(tpe0), loc)
      case Type.AssocType(cst, arg0, kind, loc) =>
        val arg = visit(arg0)
        reduceAssocTypeStep2(cst, arg, eqEnv) match {
          case None => Type.AssocType(cst, arg, kind, loc)
          case Some(reduced) => visit(reduced)
        }
    }

    visit(t0)
  }

}
