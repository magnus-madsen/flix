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
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeDef, EqualityConstraint, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{KindedAst, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint2.Provenance
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
      val declaredTpeConstr = TypeConstraint2.Equality(tpe, infTpe, prov = Provenance.Expect(expected = tpe, actual = infTpe, loc), loc)
      val declaredEffConstr = TypeConstraint2.Equality(eff, infEff, prov = Provenance.Expect(expected = eff, actual = infEff, loc), loc)
      val constrs0 = declaredTpeConstr :: declaredEffConstr :: infConstrs

      // Apply the initial substitution to all the constraints
      val initialTree = SubstitutionTree.shallow(initialSubst)
      val constrs = constrs0.map(initialTree.apply)

      ///////////////////////////////////////////////////////////////////
      //             This is where the stuff happens!                  //
      // We resolve the constraints under the environments we created. //
      ///////////////////////////////////////////////////////////////////

      val (leftovers, tree) = ConstraintSolver2.solveAll(constrs, initialTree)(Scope.Top, renv, tenv, eenv, flix)
      leftovers match {
        case Nil => (tree, Nil)
        case errs@(_ :: _) => (tree, errs.flatMap(toTypeErrors(_, renv, tree))) // TODO use getErrorsFromTypeConstraints
      }
  }

  /**
    * Converts the unresolved type constraint into an appropriate error.
    */
  private def toTypeErrors(constr: TypeConstraint2, renv: RigidityEnv, subst: SubstitutionTree)(implicit flix: Flix): List[TypeError] = constr match {
    case TypeConstraint2.Equality(Type.UnresolvedJvmType(member, _), _, _, loc) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, loc))
    case TypeConstraint2.Equality(_, Type.UnresolvedJvmType(member, _), _, loc) =>
      List(mkErrorFromUnresolvedJvmMember(member, renv, loc))

    case TypeConstraint2.Equality(_, _, Provenance.Expect(expected, actual, loc), _) =>
      List(TypeError.UnexpectedType(expected = subst(expected), inferred = subst(actual), renv, loc))

    case TypeConstraint2.Equality(_, _, Provenance.ExpectArgument(expected, actual, sym, num, loc), _) =>
      List(TypeError.UnexpectedArg(sym, num, expected = subst(expected), actual = subst(actual), renv, loc))

    case TypeConstraint2.Equality(tpe1, tpe2, Provenance.Match(baseTpe1, baseTpe2, loc), _) =>
      List(TypeError.MismatchedTypes(baseTpe1, baseTpe2, tpe1, tpe2, renv, loc))

    case TypeConstraint2.Equality(tpe1, tpe2, _, loc) =>
      List(TypeError.MismatchedTypes(tpe1, tpe2, tpe1, tpe2, renv, loc))

    case TypeConstraint2.Trait(sym, tpe, loc) => List(TypeError.MissingInstance(sym, tpe, renv, loc))

    case TypeConstraint2.Purification(sym, _, _, nested, _) =>
      nested.flatMap(toTypeErrors(_, renv, subst.branches.getOrElse(sym, SubstitutionTree.empty)))
  }

  /**
    * Creates an appropriate error from the unresolved Jvm member.
    */
  private def mkErrorFromUnresolvedJvmMember(member: Type.JvmMember, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): TypeError = member match {
    case JvmMember.JvmConstructor(clazz, tpes) => TypeError.ConstructorNotFound(clazz, tpes, renv, loc)
    case JvmMember.JvmField(base, tpe, name) => TypeError.FieldNotFound(base, name, tpe, loc)
    case JvmMember.JvmMethod(tpe, name, tpes) => TypeError.MethodNotFound(name, tpe, tpes, loc)
    case JvmMember.JvmStaticMethod(clazz, name, tpes) => TypeError.StaticMethodNotFound(clazz, name, tpes, renv, loc)
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
  def expandTraitEnv(tenv: TraitEnv, tconstrs: List[TraitConstraint]): TraitEnv = {
    tconstrs.foldLeft(tenv) {
      case (acc, TraitConstraint(TraitSymUse(sym, _), tpe, _)) =>
        acc.addInstance(sym, tpe)
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
      case (acc, EqualityConstraint(AssocTypeSymUse(sym, _), tpe1, tpe2, _)) =>
        // we set tparams to Nil because we are adding econstrs (with rigid parameters) rather than instances
        val tparams = Nil
        val assoc = AssocTypeDef(tparams, tpe1, tpe2)
        acc + (sym -> assoc)
    }
  }

}
