/*
 * Copyright 2024 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeConstructor, AssocTypeDef, EqualityConstraint, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{KindedAst, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.{Substitution, TraitEnv}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.collection.ListMap

import scala.annotation.tailrec

/**
  * Interface to [[ConstraintSolver2]].
  */
object ConstraintSolverInterface {

  /**
    * Resolves constraints in the given definition using the given inference result.
    */
  def visitDef(defn: KindedAst.Def, infResult: InfResult2, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], tenv0: TraitEnv, eqEnv0: ListMap[Symbol.AssocTypeSym, AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = defn match {
    case KindedAst.Def(sym, spec, _, _) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      visitSpec(spec, defn.loc, infResult, renv0, tconstrs0, tenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given signature using the given inference result.
    */
  def visitSig(sig: KindedAst.Sig, infResult: InfResult2, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], tenv0: TraitEnv, eqEnv0: ListMap[Symbol.AssocTypeSym, AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = sig match {
    case KindedAst.Sig(_, _, None, _) => (SubstitutionTree.empty, Nil)
    case KindedAst.Sig(sym, spec, Some(_), _) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      visitSpec(spec, sig.loc, infResult, renv0, tconstrs0, tenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given spec using the given inference result.
    */
  def visitSpec(spec: KindedAst.Spec, loc: SourceLocation, infResult: InfResult2, renv0: RigidityEnv, tconstrs0: List[TraitConstraint], tenv0: TraitEnv, eqEnv0: ListMap[Symbol.AssocTypeSym, AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): (SubstitutionTree, List[TypeError]) = spec match {
    case KindedAst.Spec(_, _, _, _, fparams, _, tpe, eff, tconstrs, econstrs) =>

      val InfResult2(infConstrs, infTpe, infEff, infRenv) = infResult

      // The initial substitution maps from formal parameters to their types
      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(tpe)(Scope.Top, flix))
      }

      // Wildcard tparams are not counted in the tparams, so we need to traverse the types to get them.
      val allTparams = tpe.typeVars ++ eff.typeVars ++ fparams.flatMap(_.tpe.typeVars) ++ econstrs.flatMap(_.tpe2.typeVars)

      // The rigidity environment is made up of:
      // 1. rigid variables from the context (e.g. from instance type parameters)
      // 2. rigid variables from type inference (e.g. regions)
      // 3. rigid variables from declared function type parameters
      val renv = allTparams.foldLeft(infRenv ++ renv0) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }

      // The trait and equality environments are made up of:
      // 1. constraints from the context (e.g. constraints on instances and traits, plus global constraints)
      // 2. constraints from the function signature
      val tenv = expandTraitEnv(tenv0, tconstrs ++ tconstrs0)
      val eenv = expandEqualityEnv(eqEnv0, econstrs) // TODO ASSOC-TYPES allow econstrs on instances

      // We add extra constraints for the declared type and effect
      val declaredTpeConstr = TypeConstraint2.Equality(tpe, infTpe, loc)
      val declaredEffConstr = TypeConstraint2.Equality(eff, infEff, loc)
      val constrs0 = declaredTpeConstr :: declaredEffConstr :: infConstrs

      // Apply the initial substitution to all the constraints
      val initialTree = SubstitutionTree(initialSubst, Map.empty)
      val constrs = constrs0.map(initialTree.apply)

      ///////////////////////////////////////////////////////////////////
      //             This is where the stuff happens!                  //
      // We resolve the constraints under the environments we created. //
      ///////////////////////////////////////////////////////////////////

      val (leftovers, tree) = ConstraintSolver2.solveAll(constrs, initialTree)(Scope.Top, renv, tenv, eenv, flix)
      leftovers match {
        case Nil => (tree, Nil)
        case errs@(_ :: _) => (tree, errs.map(toTypeError(_, renv))) // TODO use getErrorsFromTypeConstraints
      }
  }

  /**
    * Converts the unresolved type constraint into an appropriate error.
    */
  private def toTypeError(constr: TypeConstraint2, renv: RigidityEnv)(implicit flix: Flix): TypeError = constr match {
    case TypeConstraint2.Equality(tpe1, tpe2, loc) => TypeError.MismatchedTypes(tpe1, tpe2, tpe1, tpe2, renv, loc)
    case TypeConstraint2.Trait(sym, tpe, loc) => TypeError.MissingInstance(sym, tpe, renv, loc)
    case TypeConstraint2.Purification(sym, eff1, eff2, nested, loc) => toTypeError(nested.head, renv)
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit scope: Scope, flix: Flix): Type = {
    @tailrec
    def transformRow(tpe: Type, acc: Type => Type): Type = tpe match {
      case Type.Cst(TypeConstructor.SchemaRowEmpty, loc) =>
        acc(Type.freshVar(TypeConstructor.SchemaRowEmpty.kind, loc))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), rest, loc3) =>
        transformRow(rest, inner =>
          // copy into acc, just replacing `rest` with `inner`
          acc(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), inner, loc3))
        )
      case other => acc(other)
    }

    tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), row, loc2) =>
        Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), transformRow(row, x => x), loc2)
      case other => other
    }
  }

  /**
    * Adds the given type constraints as assumptions to the trait environment.
    *
    * Transitively adds the supertraits of the constraints.
    * For example, given the trait environment:
    *
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    * }}}
    *
    * If we add
    * {{{
    *   instance Order[b]
    * }}}
    *
    * then we get
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    *
    *   instance Eq[b]
    *   instance Order[b]
    * }}}
    */
  private def expandTraitEnv(tenv: TraitEnv, tconstrs: List[TraitConstraint]): TraitEnv = {
    tconstrs.foldLeft(tenv) {
      case (acc, TraitConstraint(TraitSymUse(sym, _), arg, loc)) =>
        acc.addInstance(sym, arg)
    }
  }

  /**
    * Adds the given equality constraints as assumptions to the equality environment.
    *
    * For example, given the equality environment:
    * {{{
    *   Elm[List[a]] ~ a
    * }}}
    *
    * If we add
    * {{{
    *   Elm[b] ~ String
    * }}}
    *
    * then we get
    * {{{
    *   Elm[List[a]] ~ a
    *   Elm[b] ~ String
    * }}}
    */
  private def expandEqualityEnv(eqEnv: ListMap[Symbol.AssocTypeSym, AssocTypeDef], econstrs: List[EqualityConstraint]): ListMap[Symbol.AssocTypeSym, AssocTypeDef] = {
    econstrs.foldLeft(eqEnv) {
      case (acc, EqualityConstraint(AssocTypeConstructor(sym, _), tpe1, tpe2, _)) =>
        val assoc = AssocTypeDef(tpe1, tpe2)
        acc + (sym -> assoc)
    }
  }

}
