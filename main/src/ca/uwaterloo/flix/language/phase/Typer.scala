/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Constant, Denotation, Stratification}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.inference.RestrictableChooseInference
import ca.uwaterloo.flix.language.phase.unification.InferMonad.{seqM, traverseM}
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization.minimizeScheme
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, mapN, traverse}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

import java.io.PrintWriter
import scala.annotation.tailrec

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Typer") {
    val classEnv = mkClassEnv(root.classes, root.instances)
    val eqEnv = mkEqualityEnv(root.classes, root.instances)
    val classesVal = visitClasses(root, classEnv, eqEnv, oldRoot, changeSet)
    val instancesVal = visitInstances(root, classEnv, eqEnv)
    val defsVal = visitDefs(root, classEnv, eqEnv, oldRoot, changeSet)
    val enums = visitEnums(root)
    val restrictableEnums = visitRestrictableEnums(root)
    val effsVal = visitEffs(root)
    val typeAliases = visitTypeAliases(root)

    Validation.mapN(classesVal, instancesVal, defsVal, effsVal) {
      case (classes, instances, defs, effs) =>
        val sigs = classes.values.flatMap(_.signatures).map(sig => sig.sym -> sig).toMap
        val modules = collectModules(root)
        TypedAst.Root(modules, classes, instances, sigs, defs, enums, restrictableEnums, effs, typeAliases, root.uses, root.entryPoint, root.sources, classEnv, eqEnv, root.names)
    }
  }

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(classes, _, defs, enums, restrictableEnums, effects, typeAliases, _, _, _, loc) =>
      val sigs = classes.values.flatMap { clazz => clazz.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms0 = classes.keys ++ defs.keys ++ enums.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      // collect namespaces from prefixes of other symbols
      // TODO this should be done in resolver once the duplicate namespace issue is managed
      val namespaces = syms0.collect {
        case sym: Symbol.DefnSym => sym.namespace
        case sym: Symbol.EnumSym => sym.namespace
        case sym: Symbol.RestrictableEnumSym => sym.namespace
        case sym: Symbol.ClassSym => sym.namespace
        case sym: Symbol.TypeAliasSym => sym.namespace
        case sym: Symbol.EffectSym => sym.namespace
      }.flatMap {
        fullNs =>
          fullNs.inits.collect {
            case ns@(_ :: _) => new Symbol.ModuleSym(ns)
          }
      }.toSet
      val syms = syms0 ++ namespaces

      val groups = syms.groupBy {
        case sym: Symbol.DefnSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.ClassSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EffectSym => new Symbol.ModuleSym(sym.namespace)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name)
        case sym: Symbol.AssocTypeSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)

        case sym: Symbol.ModuleSym => new Symbol.ModuleSym(sym.ns.init)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RestrictableCaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol: $sym", SourceLocation.Unknown)
        case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
      }

      groups.map {
        case (k, v) => (k, v.toList)
      }

  }

  /**
    * Creates a class environment from the classes and instances in the root.
    */
  private def mkClassEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): Map[Symbol.ClassSym, Ast.ClassContext] =
    flix.subphase("ClassEnv") {
      classes0.map {
        case (classSym, clazz) =>
          val instances = instances0.getOrElse(classSym, Nil)
          val envInsts = instances.map {
            case KindedAst.Instance(_, _, _, _, tpe, tconstrs, _, _, _, _) => Ast.Instance(tpe, tconstrs)
          }
          // ignore the super class parameters since they should all be the same as the class param
          val superClasses = clazz.superClasses.map(_.head.sym)
          (classSym, Ast.ClassContext(superClasses, envInsts))
      }
    }

  /**
    * Creates an equality environment from the classes and instances in the root.
    */
  private def mkEqualityEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] =
    flix.subphase("EqualityEnv") {

      val assocs = for {
        (classSym, _) <- classes0.iterator
        inst <- instances0.getOrElse(classSym, Nil)
        assoc <- inst.assocs
      } yield (assoc.sym.sym, Ast.AssocTypeDef(assoc.arg, assoc.tpe))


      assocs.foldLeft(ListMap.empty[Symbol.AssocTypeSym, Ast.AssocTypeDef]) {
        case (acc, (sym, defn)) => acc + (sym -> defn)
      }
    }

  /**
    * Performs type inference and reassembly on all classes in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitClasses(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.ClassSym, TypedAst.Class], TypeError] =
    flix.subphase("Classes") {
      // Compute the stale and fresh classes.
      val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)

      // Process the stale classes in parallel.
      val results = ParOps.parMap(staleClasses.values)(visitClass(_, root, classEnv, eqEnv))

      // Sequence the results using the freshClasses as the initial value.
      Validation.sequence(results) map {
        case xs => xs.foldLeft(freshClasses) {
          case (acc, clazzPair) => acc + clazzPair
        }
      }
    }

  /**
    * Reassembles a single class.
    */
  private def visitClass(clazz: KindedAst.Class, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[(Symbol.ClassSym, TypedAst.Class), TypeError] = clazz match {
    case KindedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs0, sigs0, laws0, loc) =>
      val tparams = getTypeParams(List(tparam))
      val tconstr = Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, sym.loc), Type.Var(tparam.sym, tparam.loc), sym.loc)
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(doc, mod, sym, tp, kind, loc) =>
          val tps = getTypeParams(List(tp))
          TypedAst.AssocTypeSig(doc, mod, sym, tps.head, kind, loc) // TODO ASSOC-TYPES trivial
      }
      val sigsVal = traverse(sigs0.values)(visitSig(_, List(tconstr), root, classEnv, eqEnv))
      val lawsVal = traverse(laws0)(visitDefn(_, List(tconstr), root, classEnv, eqEnv))
      mapN(sigsVal, lawsVal) {
        case (sigs, laws) => (sym, TypedAst.Class(doc, ann, mod, sym, tparams.head, superClasses, assocs, sigs, laws, loc))
      }
  }

  /**
    * Performs type inference and reassembly on all instances in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitInstances(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Map[Symbol.ClassSym, List[TypedAst.Instance]], TypeError] =
    flix.subphase("Instances") {
      val results = ParOps.parMap(root.instances.values.flatten)(visitInstance(_, root, classEnv, eqEnv))
      Validation.sequence(results) map {
        insts => insts.groupBy(inst => inst.clazz.sym)
      }
    }

  /**
    * Reassembles a single instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Instance, TypeError] = inst match {
    case KindedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs0, defs0, ns, loc) =>
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) => TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      val defsVal = traverse(defs0)(visitDefn(_, tconstrs, root, classEnv, eqEnv))
      mapN(defsVal) {
        case defs => TypedAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
      }
  }

  /**
    * Performs type inference and reassembly on all effects in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitEffs(root: KindedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.EffectSym, TypedAst.Effect], TypeError] = {
    val results = ParOps.parMap(root.effects.values)(visitEff(_, root))

    // Sequence the results using the freshDefs as the initial value.
    Validation.sequence(results) map {
      case xs => xs.foldLeft(Map.empty[Symbol.EffectSym, TypedAst.Effect]) {
        case (acc, eff) => acc + (eff.sym -> eff)
      }
    }
  }

  /**
    * Performs type inference and reassembly on the given effect `eff`.
    */
  private def visitEff(eff: KindedAst.Effect, root: KindedAst.Root)(implicit flix: Flix): Validation[TypedAst.Effect, TypeError] = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val opsVal = traverse(ops0)(visitOp(_, root))
      mapN(opsVal) {
        case ops => TypedAst.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs type inference and reassembly on the given definition `defn`.
    */
  private def visitDefn(defn: KindedAst.Def, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Def, TypeError] = defn match {
    case KindedAst.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      mapN(typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, eqEnv, sym.loc)) {
        case (spec, impl) => TypedAst.Def(sym, spec, impl)
      } recoverOne {
        case err: TypeError =>
          //
          // We recover from a type error by replacing the expression body with [[Expression.Error]].
          //
          // We use the declared type, purity, and effect as stand-ins.
          //
          val tpe = spec0.tpe
          val eff = spec0.eff
          val exp = TypedAst.Expr.Error(err, tpe, eff)
          val spec = visitSpec(spec0, root, Substitution.empty)
          val impl = TypedAst.Impl(exp, spec.declaredScheme)
          TypedAst.Def(sym, spec, impl)
      }
  }

  /**
    * Performs type inference and reassembly on the given signature `sig`.
    */
  private def visitSig(sig: KindedAst.Sig, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[TypedAst.Sig, TypeError] = sig match {
    case KindedAst.Sig(sym, spec0, Some(exp0)) =>
      typeCheckDecl(spec0, exp0, assumedTconstrs, root, classEnv, eqEnv, sym.loc) map {
        case (spec, exp) => TypedAst.Sig(sym, spec, Some(exp))
      }
    case KindedAst.Sig(sym, spec0, None) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Sig(sym, spec, None).toSuccess
  }

  /**
    * Performs type inference and reassembly on the given effect operation `op`
    */
  private def visitOp(op: KindedAst.Op, root: KindedAst.Root)(implicit flix: Flix): Validation[TypedAst.Op, TypeError] = op match {
    case KindedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Op(sym, spec).toSuccess
  }

  /**
    * Performs type inference and reassembly on the given Spec `spec`.
    */
  private def visitSpec(spec: KindedAst.Spec, root: KindedAst.Root, subst: Substitution)(implicit flix: Flix): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, tpe, eff, tconstrs, loc) =>
      val tparams = getTypeParams(tparams0)
      val fparams = getFormalParams(fparams0, subst)
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, loc)
  }

  /**
    * Performs type inference and reassembly on all definitions in the given AST root.
    *
    * Returns [[Err]] if a definition fails to type check.
    */
  private def visitDefs(root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[Map[Symbol.DefnSym, TypedAst.Def], TypeError] =
    flix.subphase("Defs") {
      // Compute the stale and fresh definitions.
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)

      // println(s"Stale = ${staleDefs.keySet}")
      // println(s"Fresh = ${freshDefs.keySet.size}")

      // Process the stale defs in parallel.
      val results = ParOps.parMap(staleDefs.values)(visitDefn(_, Nil, root, classEnv, eqEnv))

      // Sequence the results using the freshDefs as the initial value.
      Validation.sequence(results) map {
        case xs => xs.foldLeft(freshDefs) {
          case (acc, defn) => acc + (defn.sym -> defn)
        }
      }
    }

  /**
    * Infers the type of the given definition `defn0`.
    */
  private def typeCheckDecl(spec0: KindedAst.Spec, exp0: KindedAst.Expr, assumedTconstrs: List[Ast.TypeConstraint], root: KindedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], loc: SourceLocation)(implicit flix: Flix): Validation[(TypedAst.Spec, TypedAst.Impl), TypeError] = spec0 match {
    case KindedAst.Spec(_, _, _, _, fparams0, sc, tpe, eff, _, _) =>

      ///
      /// Infer the type of the expression `exp0`.
      ///
      val result = for {
        (inferredConstrs, inferredTyp, inferredEff) <- inferExpectedExp(exp0, tpe, eff, root)
      } yield (inferredConstrs, Type.mkUncurriedArrowWithEffect(fparams0.map(_.tpe), inferredEff, inferredTyp, loc))


      // Add the assumed constraints to the declared scheme
      val declaredScheme = sc.copy(tconstrs = sc.tconstrs ++ assumedTconstrs)

      ///
      /// Pattern match on the result to determine if type inference was successful.
      ///
      result match {
        case InferMonad(run) =>

          ///
          /// NB: We *DO NOT* run the type inference under the empty environment (as you would expect).
          /// Instead, we pre-populate the environment with the types from the formal parameters.
          /// This is required because we have expressions such as `x + y` where we must know the type of `x`
          /// (or y) to determine the type of floating-point or integer operations.
          ///
          val initialSubst = getSubstFromParams(fparams0)
          val initialRenv = getRigidityFromParams(fparams0)
          val initialLenv = LevelEnv.Top

          run(initialSubst, Nil, initialRenv, initialLenv) match { // TODO ASSOC-TYPES initial econstrs?
            case Ok((subst, partialEconstrs, renv0, _, (partialTconstrs, partialType))) => // TODO ASSOC-TYPES check econstrs

              ///
              /// The partial type returned by the inference monad does not have the substitution applied.
              ///
              val inferredTconstrs = partialTconstrs.map(subst.apply)
              val inferredEconstrs = partialEconstrs.map(subst.apply)
              val inferredType = subst(partialType)

              ///
              /// Check that the inferred type is at least as general as the declared type.
              ///
              /// NB: Because the inferredType is always a function type, the purect is always implicitly accounted for.
              ///
              val inferredSc = Scheme.generalize(inferredTconstrs, inferredEconstrs, inferredType, renv0)
              Scheme.checkLessThanEqual(inferredSc, declaredScheme, classEnv, eqEnv) match {
                // Case 1: no errors, continue
                case Validation.Success(_) => // noop
                case failure =>
                  val instanceErrs = failure.errors.collect {
                    case UnificationError.NoMatchingInstance(tconstr) =>
                      tconstr.arg.typeConstructor match {
                        case Some(tc: TypeConstructor.Arrow) =>
                          TypeError.MissingArrowInstance(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                        case _ =>
                          if (tconstr.head.sym.name == "Eq")
                            TypeError.MissingEq(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Order")
                            TypeError.MissingOrder(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "ToString")
                            TypeError.MissingToString(tconstr.arg, renv0, tconstr.loc)
                          else if (tconstr.head.sym.name == "Sendable")
                            TypeError.MissingSendable(tconstr.arg, renv0, tconstr.loc)
                          else
                            TypeError.MissingInstance(tconstr.head.sym, tconstr.arg, renv0, tconstr.loc)
                      }
                  }
                  // Case 2: non instance error
                  if (instanceErrs.isEmpty) {
                    //
                    // Determine the most precise type error to emit.
                    //
                    val inferredEff = inferredSc.base.arrowEffectType
                    val declaredEff = declaredScheme.base.arrowEffectType

                    if (declaredEff == Type.Pure && inferredEff == Type.Impure) {
                      // Case 1: Declared as pure, but impure.
                      return TypeError.ImpureDeclaredAsPure(loc).toFailure
                    } else if (declaredEff == Type.Pure && inferredEff != Type.Pure) {
                      // Case 2: Declared as pure, but effectful.
                      return TypeError.EffectfulDeclaredAsPure(inferredEff, loc).toFailure
                    } else {
                      // Case 3: Check if it is the effect that cannot be generalized.
                      val inferredEffScheme = Scheme(inferredSc.quantifiers, Nil, Nil, inferredEff)
                      val declaredEffScheme = Scheme(declaredScheme.quantifiers, Nil, Nil, declaredEff)
                      Scheme.checkLessThanEqual(inferredEffScheme, declaredEffScheme, classEnv, eqEnv) match {
                        case Validation.Success(_) =>
                        // Case 3.1: The effect is not the problem. Regular generalization error.
                        // Fall through to below.

                        case _failure =>
                          // Case 3.2: The effect cannot be generalized.
                          return TypeError.EffectGeneralizationError(declaredEff, inferredEff, loc).toFailure
                      }

                      return TypeError.GeneralizationError(declaredScheme, minimizeScheme(inferredSc), loc).toFailure
                    }
                  } else {
                    // Case 3: instance error
                    return Validation.Failure(instanceErrs)
                  }
              }

              ///
              /// Compute the expression, type parameters, and formal parameters with the substitution applied everywhere.
              ///
              val exp = reassembleExp(exp0, root, subst)
              val spec = visitSpec(spec0, root, subst)

              ///
              /// Compute a type scheme that matches the type variables that appear in the expression body.
              ///
              /// NB: It is very important to understand that: The type scheme a function is declared with must match the inferred type scheme.
              /// However, we require an even stronger property for the implementation to work. The inferred type scheme used in the rest of the
              /// compiler must *use the same type variables* in the scheme as in the body expression. Otherwise monomorphization et al. will break.
              ///
              val finalInferredType = subst(partialType)
              val finalInferredTconstrs = partialTconstrs.map(subst.apply)
              val finalInferredEconstrs = partialEconstrs.map(subst.apply)
              val inferredScheme = Scheme(finalInferredType.typeVars.toList.map(_.sym), finalInferredTconstrs, finalInferredEconstrs, finalInferredType)

              (spec, TypedAst.Impl(exp, inferredScheme)).toSuccess

            case Err(e) => Validation.Failure(LazyList(e))
          }
      }
  }

  /**
    * Performs type inference and reassembly on all enums in the given AST root.
    */
  private def visitEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EnumSym, TypedAst.Enum] =
    flix.subphase("Enums") {
      // Visit every enum in the ast.
      val result = root.enums.toList.map {
        case (_, enum) => visitEnum(enum, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Performs type resolution on the given enum and its cases.
    */
  private def visitEnum(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.EnumSym, TypedAst.Enum) = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, tpe, loc) =>
      val tparams = getTypeParams(tparams0)
      val cases = cases0 map {
        case (name, KindedAst.Case(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.Case(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, tpe, loc)
  }

  /**
    * Performs type inference and reassembly on all restrictable enums in the given AST root.
    */
  private def visitRestrictableEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] =
    flix.subphase("Restrictble Enums") {
      // Visit every restrictable enum in the ast.
      val result = root.restrictableEnums.toList.map {
        case (_, re) => visitRestrictableEnum(re, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Performs type resolution on the given restrictable enum and its cases.
    */
  private def visitRestrictableEnum(enum0: KindedAst.RestrictableEnum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum) = enum0 match {
    case KindedAst.RestrictableEnum(doc, ann, mod, enumSym, index0, tparams0, derives, cases0, tpe, loc) =>
      val index = TypedAst.TypeParam(index0.name, index0.sym, index0.loc)
      val tparams = getTypeParams(tparams0)
      val cases = cases0 map {
        case (name, KindedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.RestrictableEnum(doc, ann, mod, enumSym, index, tparams, derives, cases, tpe, loc)
  }

  /**
    * Performs typing on the type aliases in the given `root`.
    */
  private def visitTypeAliases(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] =
    flix.subphase("TypeAliases") {
      def visitTypeAlias(alias: KindedAst.TypeAlias): (Symbol.TypeAliasSym, TypedAst.TypeAlias) = alias match {
        case KindedAst.TypeAlias(doc, mod, sym, tparams0, tpe, loc) =>
          val tparams = getTypeParams(tparams0)
          sym -> TypedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }

      root.typeAliases.values.map(visitTypeAlias).toMap
    }

  /**
    * Infers the type of the given expression `exp0`.
    */
  def inferExp(exp0: KindedAst.Expr, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {

    /**
      * Infers the type of the given expression `exp0` inside the inference monad.
      */
    def visitExp(e0: KindedAst.Expr): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = e0 match {

      case KindedAst.Expr.Var(sym, loc) =>
        liftM(List.empty, sym.tvar, Type.Pure)

      case KindedAst.Expr.Def(sym, tvar, loc) =>
        val defn = root.defs(sym)
        val (tconstrs0, defType) = Scheme.instantiate(defn.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, defType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Sig(sym, tvar, loc) =>
        // find the declared signature corresponding to this symbol
        val sig = root.classes(sym.clazz).sigs(sym)
        val (tconstrs0, sigType) = Scheme.instantiate(sig.spec.sc, loc.asSynthetic)
        for {
          resultTyp <- unifyTypeM(tvar, sigType, loc)
          tconstrs = tconstrs0.map(_.copy(loc = loc))
        } yield (tconstrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Hole(_, tvar, _) =>
        liftM(List.empty, tvar, Type.Pure)

      case KindedAst.Expr.HoleWithExp(exp, tvar, pvar, loc) =>
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
          // result type is whatever is needed for the hole
          resultTpe = tvar
          // effect type is AT LEAST the inner expression's effect
          atLeastEff = Type.mkUnion(eff, Type.freshVar(Kind.Eff, loc.asSynthetic), loc.asSynthetic)
          resultEff <- unifyTypeM(atLeastEff, pvar, loc)
        } yield (tconstrs, resultTpe, resultEff)

      case e: KindedAst.Expr.OpenAs => RestrictableChooseInference.inferOpenAs(e, root)

      case KindedAst.Expr.Use(_, alias, exp, _) => visitExp(exp)

      case KindedAst.Expr.Cst(Ast.Constant.Unit, loc) =>
        liftM(List.empty, Type.mkUnit(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Null, loc) =>
        liftM(List.empty, Type.mkNull(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Bool(_), loc) =>
        liftM(List.empty, Type.mkBool(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Char(_), loc) =>
        liftM(List.empty, Type.mkChar(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Float32(_), loc) =>
        liftM(List.empty, Type.mkFloat32(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Float64(_), loc) =>
        liftM(List.empty, Type.mkFloat64(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.BigDecimal(_), loc) =>
        liftM(List.empty, Type.mkBigDecimal(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int8(_), loc) =>
        liftM(List.empty, Type.mkInt8(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int16(_), loc) =>
        liftM(List.empty, Type.mkInt16(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int32(_), loc) =>
        liftM(List.empty, Type.mkInt32(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Int64(_), loc) =>
        liftM(List.empty, Type.mkInt64(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.BigInt(_), loc) =>
        liftM(List.empty, Type.mkBigInt(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Str(_), loc) =>
        liftM(List.empty, Type.mkString(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Cst(Ast.Constant.Regex(_), loc) =>
        liftM(List.empty, Type.mkRegex(loc.asSynthetic), Type.Pure)

      case KindedAst.Expr.Lambda(fparam, exp, loc) =>
        val argType = fparam.tpe
        val argTypeVar = fparam.sym.tvar
        for {
          (constrs, bodyType, bodyEff) <- visitExp(exp)
          _ <- unifyTypeM(argType, argTypeVar, loc)
          resultTyp = Type.mkArrowWithEffect(argType, bodyEff, bodyType, loc)
        } yield (constrs, resultTyp, Type.Pure)

      case KindedAst.Expr.Apply(exp, exps, tvar, pvar, loc) =>
        //
        // Determine if there is a direct call to a Def or Sig.
        //
        val knownTarget = exp match {
          case KindedAst.Expr.Def(sym, tvar2, loc2) =>
            // Case 1: Lookup the sym and instantiate its scheme.
            val defn = root.defs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(defn.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case KindedAst.Expr.Sig(sym, tvar2, loc2) =>
            // Case 2: Lookup the sym and instantiate its scheme.
            val sig = root.classes(sym.clazz).sigs(sym)
            val (tconstrs1, declaredType) = Scheme.instantiate(sig.spec.sc, loc2.asSynthetic)
            val constrs1 = tconstrs1.map(_.copy(loc = loc))
            Some((sym, tvar2, constrs1, declaredType))

          case _ =>
            // Case 3: Unknown target.
            None
        }

        knownTarget match {
          case Some((sym, tvar2, constrs1, declaredType)) =>
            //
            // Special Case: We are applying a Def or Sig and we break apart its declared type.
            //
            val declaredEff = declaredType.arrowEffectType
            val declaredArgumentTypes = declaredType.arrowArgTypes
            val declaredResultType = declaredType.arrowResultType

            for {
              (constrs2, tpes, effs) <- traverseM(exps)(visitExp).map(_.unzip3)
              _ <- expectTypeArguments(sym, declaredArgumentTypes, tpes, exps.map(_.loc), loc)
              _ <- unifyTypeM(tvar2, declaredType, loc)
              resultTyp <- unifyTypeM(tvar, declaredResultType, loc)
              resultEff <- unifyEffM(pvar, Type.mkUnion(declaredEff :: effs, loc), loc)
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)

          case None =>
            //
            // Default Case: Apply.
            //
            val lambdaBodyType = Type.freshVar(Kind.Star, loc)
            val lambdaBodyEff = Type.freshVar(Kind.Eff, loc)
            for {
              (constrs1, tpe, eff) <- visitExp(exp)
              (constrs2, tpes, effs) <- traverseM(exps)(visitExp).map(_.unzip3)
              _ <- expectTypeM(tpe, Type.mkUncurriedArrowWithEffect(tpes, lambdaBodyEff, lambdaBodyType, loc), loc)
              resultTyp <- unifyTypeM(tvar, lambdaBodyType, loc)
              resultEff <- unifyEffM(pvar, Type.mkUnion(lambdaBodyEff :: eff :: effs, loc), loc)
              _ <- unbindVar(lambdaBodyType) // NB: Safe to unbind since the variable is not used elsewhere.
              _ <- unbindVar(lambdaBodyEff) // NB: Safe to unbind since the variable is not used elsewhere.
            } yield (constrs1 ++ constrs2.flatten, resultTyp, resultEff)
        }

      case KindedAst.Expr.Unary(sop, exp, tvar, loc) => sop match {
        case SemanticOp.BoolOp.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Bool, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Float32Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Float64Op.Neg =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Float64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int8Op.Neg | SemanticOp.Int8Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int8, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int16Op.Neg | SemanticOp.Int16Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int16, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int32Op.Neg | SemanticOp.Int32Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int32, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case SemanticOp.Int64Op.Neg | SemanticOp.Int64Op.Not =>
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            resultTyp <- expectTypeM(expected = Type.Int64, actual = tpe, bind = tvar, exp.loc)
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected unary operator: '$sop'.", loc)
      }

      case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) => sop match {

        case SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Bool, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Float32Op.Add | SemanticOp.Float32Op.Sub | SemanticOp.Float32Op.Mul | SemanticOp.Float32Op.Div
             | SemanticOp.Float32Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float32, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Float64Op.Add | SemanticOp.Float64Op.Sub | SemanticOp.Float64Op.Mul | SemanticOp.Float64Op.Div
             | SemanticOp.Float64Op.Exp =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Float64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Float64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Float64, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int8Op.Add | SemanticOp.Int8Op.Sub | SemanticOp.Int8Op.Mul | SemanticOp.Int8Op.Div
             | SemanticOp.Int8Op.Rem | SemanticOp.Int8Op.Exp
             | SemanticOp.Int8Op.And | SemanticOp.Int8Op.Or | SemanticOp.Int8Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int8, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int8, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int8, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int16Op.Add | SemanticOp.Int16Op.Sub | SemanticOp.Int16Op.Mul | SemanticOp.Int16Op.Div
             | SemanticOp.Int16Op.Rem | SemanticOp.Int16Op.Exp
             | SemanticOp.Int16Op.And | SemanticOp.Int16Op.Or | SemanticOp.Int16Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int16, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int16, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int16, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int32Op.Add | SemanticOp.Int32Op.Sub | SemanticOp.Int32Op.Mul | SemanticOp.Int32Op.Div
             | SemanticOp.Int32Op.Rem | SemanticOp.Int32Op.Exp
             | SemanticOp.Int32Op.And | SemanticOp.Int32Op.Or | SemanticOp.Int32Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int32, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int32, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int64Op.Add | SemanticOp.Int64Op.Sub | SemanticOp.Int64Op.Mul | SemanticOp.Int64Op.Div
             | SemanticOp.Int64Op.Rem | SemanticOp.Int64Op.Exp
             | SemanticOp.Int64Op.And | SemanticOp.Int64Op.Or | SemanticOp.Int64Op.Xor =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Int64, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Int64, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Int64, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.Int8Op.Shl | SemanticOp.Int8Op.Shr
             | SemanticOp.Int16Op.Shl | SemanticOp.Int16Op.Shr
             | SemanticOp.Int32Op.Shl | SemanticOp.Int32Op.Shr
             | SemanticOp.Int64Op.Shl | SemanticOp.Int64Op.Shr =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- unifyTypeM(tvar, tpe1, loc)
            rhs <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, lhs, resultEff)

        case SemanticOp.BoolOp.Eq | SemanticOp.BoolOp.Neq
             | SemanticOp.CharOp.Eq | SemanticOp.CharOp.Neq
             | SemanticOp.Float32Op.Eq | SemanticOp.Float32Op.Neq
             | SemanticOp.Float64Op.Eq | SemanticOp.Float64Op.Neq
             | SemanticOp.Int8Op.Eq | SemanticOp.Int8Op.Neq
             | SemanticOp.Int16Op.Eq | SemanticOp.Int16Op.Neq
             | SemanticOp.Int32Op.Eq | SemanticOp.Int32Op.Neq
             | SemanticOp.Int64Op.Eq | SemanticOp.Int64Op.Neq =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.CharOp.Lt | SemanticOp.CharOp.Le | SemanticOp.CharOp.Gt | SemanticOp.CharOp.Ge
             | SemanticOp.Float32Op.Lt | SemanticOp.Float32Op.Le | SemanticOp.Float32Op.Gt | SemanticOp.Float32Op.Ge
             | SemanticOp.Float64Op.Lt | SemanticOp.Float64Op.Le | SemanticOp.Float64Op.Gt | SemanticOp.Float64Op.Ge
             | SemanticOp.Int8Op.Lt | SemanticOp.Int8Op.Le | SemanticOp.Int8Op.Gt | SemanticOp.Int8Op.Ge
             | SemanticOp.Int16Op.Lt | SemanticOp.Int16Op.Le | SemanticOp.Int16Op.Gt | SemanticOp.Int16Op.Ge
             | SemanticOp.Int32Op.Lt | SemanticOp.Int32Op.Le | SemanticOp.Int32Op.Gt | SemanticOp.Int32Op.Ge
             | SemanticOp.Int64Op.Lt | SemanticOp.Int64Op.Le | SemanticOp.Int64Op.Gt | SemanticOp.Int64Op.Ge =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            valueType <- unifyTypeM(tpe1, tpe2, loc)
            resultTyp <- unifyTypeM(tvar, Type.Bool, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case SemanticOp.StringOp.Concat =>
          for {
            (constrs1, tpe1, eff1) <- visitExp(exp1)
            (constrs2, tpe2, eff2) <- visitExp(exp2)
            lhs <- expectTypeM(expected = Type.Str, actual = tpe1, exp1.loc)
            rhs <- expectTypeM(expected = Type.Str, actual = tpe2, exp2.loc)
            resultTyp <- unifyTypeM(tvar, Type.Str, loc)
            resultEff = Type.mkUnion(eff1, eff2, loc)
          } yield (constrs1 ++ constrs2, resultTyp, resultEff)

        case _ => throw InternalCompilerException(s"Unexpected binary operator: '$sop'.", loc)
      }

      case KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          condType <- expectTypeM(expected = Type.Bool, actual = tpe1, exp1.loc)
          resultTyp <- unifyTypeM(tpe2, tpe3, loc)
          resultEff = Type.mkUnion(eff1, eff2, eff3, loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.Stm(exp1, exp2, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Discard(exp, loc) =>
        for {
          (constrs, _, eff) <- visitExp(exp)
          resultTyp = Type.Unit
        } yield (constrs, resultTyp, eff)

      case KindedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
        // Note: The call to unify on sym.tvar occurs immediately after we have inferred the type of exp1.
        // This ensures that uses of sym inside exp2 are type checked according to this type.
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          boundVar <- unifyTypeM(sym.tvar, tpe1, loc)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.LetRec(sym, mod, exp1, exp2, loc) =>
        // Ensure that `exp1` is a lambda.
        val a = Type.freshVar(Kind.Star, loc)
        val b = Type.freshVar(Kind.Star, loc)
        val p = Type.freshVar(Kind.Eff, loc)
        val expectedType = Type.mkArrowWithEffect(a, p, b, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          arrowTyp <- unifyTypeM(expectedType, tpe1, exp1.loc)
          boundVar <- unifyTypeM(sym.tvar, tpe1, exp1.loc)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp = tpe2
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Region(tpe, _) =>
        liftM(Nil, tpe, Type.Pure)

      case KindedAst.Expr.Scope(sym, regionVar, exp, pvar, loc) =>
        for {
          // don't make the region var rigid if the --Xflexible-regions flag is set
          _ <- if (flix.options.xflexibleregions) InferMonad.point(()) else rigidifyM(regionVar)
          _ <- enterScopeM(regionVar.sym)
          _ <- unifyTypeM(sym.tvar, Type.mkRegion(regionVar, loc), loc)
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- exitScopeM(regionVar.sym)
          resultEff <- unifyTypeM(pvar, eff, loc)
          _ <- noEscapeM(regionVar, tpe)
          resultTyp = tpe
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ScopeExit(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        val p = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = Type.mkUncurriedArrowWithEffect(Type.Unit :: Nil, p, Type.Unit, loc.asSynthetic), actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.mkUnion(Type.Impure, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Match(exp, rules, loc) =>
        val patterns = rules.map(_.pat)
        val guards = rules.flatMap(_.guard)
        val bodies = rules.map(_.exp)
        val guardLocs = guards.map(_.loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          patternType <- unifyTypeM(tpe :: patternTypes, loc)
          (guardConstrs, guardTypes, guardEffs) <- traverseM(guards)(visitExp).map(_.unzip3)
          guardType <- traverseM(guardTypes.zip(guardLocs)) { case (gTpe, gLoc) => expectTypeM(expected = Type.Bool, actual = gTpe, loc = gLoc) }
          (bodyConstrs, bodyTypes, bodyEffs) <- traverseM(bodies)(visitExp).map(_.unzip3)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultEff = Type.mkUnion(eff :: guardEffs ::: bodyEffs, loc)
        } yield (constrs ++ guardConstrs.flatten ++ bodyConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.TypeMatch(exp, rules, loc) =>
        val bodies = rules.map(_.exp)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          // rigidify all the type vars in the rules
          _ <- traverseM(rules.flatMap(rule => rule.tpe.typeVars.toList))(rigidifyM)
          // unify each rule's variable with its type
          _ <- traverseM(rules)(rule => unifyTypeM(rule.sym.tvar, rule.tpe, rule.sym.loc))
          (bodyConstrs, bodyTypes, bodyEffs) <- traverseM(bodies)(visitExp).map(_.unzip3)
          resultTyp <- unifyTypeM(bodyTypes, loc)
          resultEff = Type.mkUnion(eff :: bodyEffs, loc)
        } yield (constrs ++ bodyConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.RelationalChoose(star, exps0, rules0, tvar, loc) =>

        /**
          * Performs type inference on the given match expressions `exps` and nullity `vars`.
          *
          * Returns a pair of lists of the types and purects of the match expressions.
          */
        def visitMatchExps(exps: List[KindedAst.Expr], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitMatchExp(exp: KindedAst.Expr, isAbsentVar: Type.Var, isPresentVar: Type.Var): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
            val freshElmVar = Type.freshVar(Kind.Star, loc)
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              _ <- unifyTypeM(tpe, Type.mkChoice(freshElmVar, isAbsentVar, isPresentVar, loc), loc)
            } yield (constrs, freshElmVar, eff)
          }

          traverseM(exps.zip(isAbsentVars.zip(isPresentVars))) {
            case (matchExp, (isAbsentVar, isPresentVar)) => visitMatchExp(matchExp, isAbsentVar, isPresentVar)
          }.map(_.unzip3)
        }

        /**
          * Performs type inference of the given null rules `rs`.
          *
          * Returns a pair of list of the types and purects of the rule expressions.
          */
        def visitRuleBodies(rs: List[KindedAst.RelationalChooseRule]): InferMonad[(List[List[Ast.TypeConstraint]], List[Type], List[Type])] = {
          def visitRuleBody(r: KindedAst.RelationalChooseRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = r match {
            case KindedAst.RelationalChooseRule(_, exp0) => visitExp(exp0)
          }

          traverseM(rs)(visitRuleBody).map(_.unzip3)
        }

        /**
          * Returns a transformed result type that encodes the Boolean constraint of each row pattern in the result type.
          *
          * NB: Requires that the `ts` types are Choice-types.
          */
        def transformResultTypes(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], rs: List[KindedAst.RelationalChooseRule], ts: List[Type], loc: SourceLocation): InferMonad[Type] = {
          def visitRuleBody(r: KindedAst.RelationalChooseRule, resultType: Type): InferMonad[(Type, Type, Type)] = r match {
            case KindedAst.RelationalChooseRule(r, exp0) =>
              val cond = mkOverApprox(isAbsentVars, isPresentVars, r)
              val innerType = Type.freshVar(Kind.Star, exp0.loc)
              val isAbsentVar = Type.freshVar(Kind.Bool, exp0.loc)
              val isPresentVar = Type.freshVar(Kind.Bool, exp0.loc)
              for {
                choiceType <- unifyTypeM(resultType, Type.mkChoice(innerType, isAbsentVar, isPresentVar, loc), loc)
              } yield (Type.mkAnd(cond, isAbsentVar, loc), Type.mkAnd(cond, isPresentVar, loc), innerType)
          }

          ///
          /// Simply compute the mgu of the `ts` types if this is not a star relational_choose.
          ///
          if (!star) {
            return unifyTypeM(ts, loc)
          }

          ///
          /// Otherwise construct a new Choice type with isAbsent and isPresent conditions that depend on each pattern row.
          ///
          for {
            (isAbsentConds, isPresentConds, innerTypes) <- traverseM(rs.zip(ts))(p => visitRuleBody(p._1, p._2)).map(_.unzip3)
            isAbsentCond = Type.mkOr(isAbsentConds, loc)
            isPresentCond = Type.mkOr(isPresentConds, loc)
            innerType <- unifyTypeM(innerTypes, loc)
            resultType = Type.mkChoice(innerType, isAbsentCond, isPresentCond, loc)
          } yield resultType
        }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an under-approximation.
          *
          * If a pattern is a wildcard it *must* always match.
          * If a pattern is `Absent`  its corresponding `isPresentVar` must be `false` (i.e. to prevent the value from being `Present`).
          * If a pattern is `Present` its corresponding `isAbsentVar`  must be `false` (i.e. to prevent the value from being `Absent`).
          */
        def mkUnderApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoosePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.RelationalChoosePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.RelationalChoosePattern.Present(_, _, _))) =>
              // Case 2: A `Present` pattern forces the `isAbsentVar` to be equal to `false`.
              Type.mkAnd(acc, Type.mkEquiv(isAbsentVar, Type.False, loc), loc)
            case (acc, ((_, isPresentVar), KindedAst.RelationalChoosePattern.Absent(_))) =>
              // Case 3: An `Absent` pattern forces the `isPresentVar` to be equal to `false`.
              Type.mkAnd(acc, Type.mkEquiv(isPresentVar, Type.False, loc), loc)
          }

        /**
          * Constructs a Boolean constraint for the given choice rule `r` which is an over-approximation.
          *
          * If a pattern is a wildcard it *may* always match.
          * If a pattern is `Absent` it *may* match if its corresponding `isAbsent` is `true`.
          * If a pattern is `Present` it *may* match if its corresponding `isPresentVar`is `true`.
          */
        def mkOverApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoosePattern]): Type =
          isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
            case (acc, (_, KindedAst.RelationalChoosePattern.Wild(_))) =>
              // Case 1: No constraint is generated for a wildcard.
              acc
            case (acc, ((isAbsentVar, _), KindedAst.RelationalChoosePattern.Absent(_))) =>
              // Case 2: An `Absent` pattern may match if the `isAbsentVar` is `true`.
              Type.mkAnd(acc, isAbsentVar, loc)
            case (acc, ((_, isPresentVar), KindedAst.RelationalChoosePattern.Present(_, _, _))) =>
              // Case 3: A `Present` pattern may match if the `isPresentVar` is `true`.
              Type.mkAnd(acc, isPresentVar, loc)
          }

        /**
          * Constructs a disjunction of the constraints of each choice rule.
          */
        def mkOuterDisj(m: List[List[KindedAst.RelationalChoosePattern]], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): Type =
          m.foldLeft(Type.False) {
            case (acc, rule) => Type.mkOr(acc, mkUnderApprox(isAbsentVars, isPresentVars, rule), loc)
          }

        /**
          * Performs type inference and unification with the `matchTypes` against the given choice rules `rs`.
          */
        def unifyMatchTypesAndRules(matchTypes: List[Type], rs: List[KindedAst.RelationalChooseRule]): InferMonad[List[List[Type]]] = {
          def unifyWithRule(r: KindedAst.RelationalChooseRule): InferMonad[List[Type]] = {
            traverseM(matchTypes.zip(r.pat)) {
              case (matchType, KindedAst.RelationalChoosePattern.Wild(_)) =>
                // Case 1: The pattern is wildcard. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.RelationalChoosePattern.Absent(_)) =>
                // Case 2: The pattern is a `Absent`. No variable is bound and there is type to constrain.
                liftM(matchType)
              case (matchType, KindedAst.RelationalChoosePattern.Present(sym, tvar, loc)) =>
                // Case 3: The pattern is `Present`. Must constraint the type of the local variable with the type of the match expression.
                unifyTypeM(matchType, sym.tvar, tvar, loc)
            }
          }

          traverseM(rs)(unifyWithRule)
        }

        //
        // Introduce an isAbsent variable for each match expression in `exps`.
        //
        val isAbsentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

        //
        // Introduce an isPresent variable for each math expression in `exps`.
        //
        val isPresentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

        //
        // Extract the choice pattern match matrix.
        //
        val matrix = rules0.map(_.pat)

        //
        // Compute the saturated pattern match matrix..
        //
        val saturated = ChoiceMatch.saturate(matrix)

        //
        // Build the Boolean formula.
        //
        val formula = mkOuterDisj(saturated, isAbsentVars, isPresentVars)

        //
        // Put everything together.
        //
        for {
          _ <- unifyBoolM(formula, Type.True, loc)
          (matchConstrs, matchTyp, matchEff) <- visitMatchExps(exps0, isAbsentVars, isPresentVars)
          _ <- unifyMatchTypesAndRules(matchTyp, rules0)
          (ruleBodyConstrs, ruleBodyTyp, ruleBodyEff) <- visitRuleBodies(rules0)
          resultTypes <- transformResultTypes(isAbsentVars, isPresentVars, rules0, ruleBodyTyp, loc)
          resultTyp <- unifyTypeM(tvar, resultTypes, loc)
          resultEff = Type.mkUnion(matchEff ::: ruleBodyEff, loc)
        } yield (matchConstrs.flatten ++ ruleBodyConstrs.flatten, resultTyp, resultEff)

      case exp@KindedAst.Expr.RestrictableChoose(_, _, _, _, _) => RestrictableChooseInference.infer(exp, root)

      case KindedAst.Expr.Tag(symUse, exp, tvar, loc) =>
        if (symUse.sym.enumSym == Symbol.mkEnumSym("Choice")) {
          //
          // Special Case 1: Absent or Present Tag
          //
          if (symUse.sym.name == "Absent") {
            // Case 1.1: Absent Tag.
            val elmVar = Type.freshVar(Kind.Star, loc)
            val isAbsent = Type.True
            val isPresent = Type.freshVar(Kind.Bool, loc)
            for {
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(elmVar, isAbsent, isPresent, loc), loc)
              resultEff = Type.Pure
            } yield (List.empty, resultTyp, resultEff)
          }
          else if (symUse.sym.name == "Present") {
            // Case 1.2: Present Tag.
            val isAbsent = Type.freshVar(Kind.Bool, loc)
            val isPresent = Type.True
            for {
              (constrs, tpe, eff) <- visitExp(exp)
              resultTyp <- unifyTypeM(tvar, Type.mkChoice(tpe, isAbsent, isPresent, loc), loc)
              resultEff = eff
            } yield (constrs, resultTyp, resultEff)
          } else {
            // Case 1.3: Unknown tag.
            throw InternalCompilerException(s"Unexpected choice tag: '${symUse.sym}'.", loc)
          }
        } else {
          //
          // General Case:
          //

          // Lookup the enum declaration.
          val decl = root.enums(symUse.sym.enumSym)

          // Lookup the case declaration.
          val caze = decl.cases(symUse.sym)

          // Instantiate the type scheme of the case.
          val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

          //
          // The tag type is a function from the type of variant to the type of the enum.
          //
          for {
            (constrs, tpe, eff) <- visitExp(exp)
            _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
            resultTyp = tvar
            resultEff = eff
          } yield (constrs, resultTyp, resultEff)
        }

      case exp@KindedAst.Expr.RestrictableTag(_, _, _, _, _) =>
        RestrictableChooseInference.inferRestrictableTag(exp, root)

      case KindedAst.Expr.Tuple(elms, loc) =>
        for {
          (elementConstrs, elementTypes, elementEffs) <- traverseM(elms)(visitExp).map(_.unzip3)
          resultEff = Type.mkUnion(elementEffs, loc)
        } yield (elementConstrs.flatten, Type.mkTuple(elementTypes, loc), resultEff)

      case KindedAst.Expr.RecordEmpty(loc) =>
        liftM(List.empty, Type.mkRecord(Type.RecordRowEmpty, loc), Type.Pure)

      case KindedAst.Expr.RecordSelect(exp, field, tvar, loc) =>
        //
        // r : { field = tpe | row }
        // -------------------------
        //       r.field : tpe
        //
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(field, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, expectedRecordType, loc)
          resultEff = eff
        } yield (constrs, tvar, resultEff)

      case KindedAst.Expr.RecordExtend(field, exp1, exp2, tvar, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { field = exp1 | exp2 } : { field  :: tpe | r }
        //
        val restRow = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe2, Type.mkRecord(restRow, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(Type.mkRecordRowExtend(field, tpe1, restRow, loc), loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.RecordRestrict(field, exp, tvar, loc) =>
        //
        //  exp : { field  :: t | r }
        // -------------------------
        // { -field | exp } : {| r }
        //
        val freshFieldType = Type.freshVar(Kind.Star, loc)
        val freshRowVar = Type.freshVar(Kind.RecordRow, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          recordType <- unifyTypeM(tpe, Type.mkRecord(Type.mkRecordRowExtend(field, freshFieldType, freshRowVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRecord(freshRowVar, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLit(exps, exp, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, elmTypes, eff1) <- traverseM(exps)(visitExp).map(_.unzip3)
          (constrs2, tpe2, eff2) <- visitExp(exp)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp.loc)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(elmTyp, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(Type.mkUnion(eff1, loc), eff2, regionVar, loc), loc)
        } yield (constrs1.flatten ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = regionType, actual = tpe1, loc)
          _lenType <- expectTypeM(expected = Type.Int32, actual = tpe3, exp3.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkArray(tpe2, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, eff3, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkArray(elmVar, regionVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultEff = eff
          _ <- unbindVar(elmVar)
          _ <- unbindVar(regionVar)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkArray(tvar, regionVar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(regionVar, eff1, eff2, loc), loc)
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case KindedAst.Expr.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          (constrs3, tpe3, eff3) <- visitExp(exp3)
          _ <- expectTypeM(expected = arrayType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe3, exp3.loc)
          resultTyp = Type.Unit
          resultEff <- unifyTypeM(pvar, Type.mkUnion(List(regionVar, eff1, eff2, eff3), loc), loc)
        } yield (constrs1 ++ constrs2 ++ constrs3, resultTyp, resultEff)

      case KindedAst.Expr.VectorLit(exps, tvar, pvar, loc) =>
        for {
          (constrs, elmTypes, eff) <- traverseM(exps)(visitExp).map(_.unzip3)
          elmTyp <- unifyTypeAllowEmptyM(elmTypes, Kind.Star, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkVector(elmTyp, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff, loc), loc)
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.VectorLoad(exp1, exp2, tvar, pvar, loc) =>
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          arrayType <- expectTypeM(expected = Type.mkVector(tvar, loc), actual = tpe1, exp1.loc)
          indexType <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, loc), loc)
        } yield (constrs1 ++ constrs2, tvar, resultEff)

      case KindedAst.Expr.VectorLength(exp, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(Type.mkVector(elmVar, loc), tpe, exp.loc)
          resultTyp = Type.Int32
          resultEff = eff
          _ <- unbindVar(elmVar)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Ref(exp1, exp2, tvar, pvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(tpe2, regionType, exp2.loc)
          resultTyp <- unifyTypeM(tvar, Type.mkRef(tpe1, regionVar, loc), loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Deref(exp, tvar, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = refType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff, regionVar, loc), loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Assign(exp1, exp2, pvar, loc) =>
        val elmVar = Type.freshVar(Kind.Star, loc)
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val refType = Type.mkRef(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = refType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff <- unifyTypeM(pvar, Type.mkUnion(eff1, eff2, regionVar, loc), loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Ascribe(exp, expectedTyp, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        for {
          (constrs, actualTyp, actualPur) <- visitExp(exp)
          resultTyp <- expectTypeM(expected = expectedTyp.getOrElse(Type.freshVar(Kind.Star, loc)), actual = actualTyp, bind = tvar, loc)
          resultEff <- expectTypeM(expected = expectedEff.getOrElse(Type.freshVar(Kind.Eff, loc)), actual = actualPur, loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.InstanceOf(exp, className, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.Bool
          resultEff <- expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.CheckedCast(cast, exp, tvar, pvar, loc) =>
        cast match {
          case CheckedCastType.TypeCast =>
            for {
              // Ignore the inferred type of exp.
              (constrs, _, eff) <- visitExp(exp)
            } yield (constrs, tvar, eff)

          case CheckedCastType.EffectCast =>
            for {
              // We simply union the purity and effect with a fresh variable.
              (constrs, tpe, eff) <- visitExp(exp)
              resultEff = Type.mkUnion(eff, pvar, loc)
            } yield (constrs, tpe, resultEff)
        }

      case KindedAst.Expr.UncheckedCast(exp, declaredTyp, declaredEff, tvar, loc) =>
        // A cast expression is unsound; the type system assumes the declared type is correct.
        for {
          (constrs, actualTyp, actualEff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tvar, declaredTyp.getOrElse(actualTyp), loc)
          resultEff = declaredEff.getOrElse(actualEff)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.UncheckedMaskingCast(exp, _) =>
        // A mask expression is unsound; the type system assumes the expression is pure.
        for {
          (constrs, tpe, eff) <- visitExp(exp)
        } yield (constrs, tpe, Type.Pure)

      case KindedAst.Expr.Without(exp, effUse, loc) =>
        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        //        val expected = Type.mkDifference(Type.freshVar(Kind.Bool, loc), effType, loc)
        // TODO EFF-MIGRATION use expected
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
        } yield (tconstrs, tpe, eff)

      case KindedAst.Expr.TryCatch(exp, rules, loc) =>
        val rulesType = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            visitExp(body)
        }

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          (ruleConstrs, ruleTypes, ruleEffs) <- seqM(rulesType).map(_.unzip3)
          ruleType <- unifyTypeM(ruleTypes, loc)
          resultTyp <- unifyTypeM(tpe, ruleType, loc)
          resultEff = Type.mkUnion(eff :: ruleEffs, loc)
        } yield (constrs ++ ruleConstrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.TryWith(exp, effUse, rules, tvar, loc) =>
        val effect = root.effects(effUse.sym)
        val ops = effect.ops.map(op => op.sym -> op).toMap

        def unifyFormalParams(op: Symbol.OpSym, expected: List[KindedAst.FormalParam], actual: List[KindedAst.FormalParam]): InferMonad[Unit] = {
          if (expected.length != actual.length) {
            InferMonad.errPoint(TypeError.InvalidOpParamCount(op, expected = expected.length, actual = actual.length, loc))
          } else {
            traverseM(expected zip actual) {
              case (ex, ac) =>
                for {
                  _ <- expectTypeM(expected = ex.tpe, actual = ac.tpe, ac.loc)
                } yield ()
            }.map(_ => ())
          }
        }

        def visitHandlerRule(rule: KindedAst.HandlerRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = rule match {
          case KindedAst.HandlerRule(op, actualFparams, body, opTvar) =>
            // Don't need to generalize since ops are monomorphic
            // Don't need to handle unknown op because resolver would have caught this
            ops(op.sym) match {
              case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, expectedPur, _, _)) =>
                for {
                  _ <- unifyFormalParams(op.sym, expected = expectedFparams, actual = actualFparams)
                  (actualTconstrs, actualTpe, actualEff) <- visitExp(body)

                  // unify the operation return type with its tvar
                  _ <- unifyTypeM(opTpe, opTvar, body.loc)

                  // unify the handler result type with the whole block's tvar
                  resultTpe <- expectTypeM(expected = tvar, actual = actualTpe, body.loc)
                  resultEff <- expectTypeM(expected = expectedPur, actual = actualEff, body.loc) // MATT improve error message for this
                } yield (actualTconstrs, resultTpe, resultEff)
            }
        }

        val effType = Type.Cst(TypeConstructor.Effect(effUse.sym), effUse.loc)
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
          (tconstrss, _, effs) <- traverseM(rules)(visitHandlerRule).map(_.unzip3)
          resultTconstrs = (tconstrs :: tconstrss).flatten
          resultTpe <- unifyTypeM(tvar, tpe, loc)
          resultEff = Type.mkUnion(Type.mkDifference(eff, effType, loc) :: effs, loc)
        } yield (resultTconstrs, resultTpe, resultEff)

      case KindedAst.Expr.Do(op, args, tvar, loc) =>
        val effect = root.effects(op.sym.eff)
        val operation = effect.ops.find(_.sym == op.sym)
          .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $op in effect ${op.sym.eff}", loc))
        val effTpe = Type.Cst(TypeConstructor.Effect(op.sym.eff), loc)

        def visitArg(arg: KindedAst.Expr, fparam: KindedAst.FormalParam): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
          for {
            (tconstrs, tpe, eff) <- visitExp(arg)
            _ <- expectTypeM(expected = fparam.tpe, tpe, arg.loc)
          } yield (tconstrs, tpe, eff)
        }

        if (operation.spec.fparams.length != args.length) {
          InferMonad.errPoint(TypeError.InvalidOpParamCount(op.sym, expected = operation.spec.fparams.length, actual = args.length, loc))
        } else {
          val argM = (args zip operation.spec.fparams) map {
            case (arg, fparam) => visitArg(arg, fparam)
          }
          for {
            (tconstrss, _, effs) <- seqM(argM).map(_.unzip3)
            resultTconstrs = tconstrss.flatten
            resultTpe <- unifyTypeM(operation.spec.tpe, tvar, loc)
            resultEff = Type.mkUnion(effTpe :: operation.spec.eff :: effs, loc)
          } yield (resultTconstrs, resultTpe, resultEff)
        }

      case KindedAst.Expr.Resume(exp, argTvar, retTvar, loc) =>
        for {
          (tconstrs, tpe, eff) <- visitExp(exp)
          resultTconstrs = tconstrs
          _ <- expectTypeM(expected = argTvar, actual = tpe, exp.loc)
          resultTpe = retTvar
          resultEff = eff
        } yield (resultTconstrs, resultTpe, resultEff)

      case KindedAst.Expr.InvokeConstructor(constructor, args, loc) =>
        val classType = getFlixType(constructor.getDeclaringClass)
        for {
          (constrs, _, _) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = classType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.InvokeMethod(method, clazz, exp, args, loc) =>
        val classType = getFlixType(clazz)
        val returnType = getFlixType(method.getReturnType)
        for {
          (baseConstrs, baseTyp, _) <- visitExp(exp)
          objectTyp <- unifyTypeM(baseTyp, classType, loc)
          (constrs, tpes, effs) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = getFlixType(method.getReturnType)
          resultEff = Type.Impure
        } yield (baseConstrs ++ constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.InvokeStaticMethod(method, args, loc) =>
        val returnType = getFlixType(method.getReturnType)
        for {
          (constrs, tpes, effs) <- traverseM(args)(visitExp).map(_.unzip3)
          resultTyp = returnType
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)

      case KindedAst.Expr.GetField(field, clazz, exp, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs, tpe, _) <- visitExp(exp)
          objectTyp <- expectTypeM(expected = classType, actual = tpe, exp.loc)
          resultTyp = fieldType
          resultEff = Type.Impure
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.PutField(field, clazz, exp1, exp2, loc) =>
        val fieldType = getFlixType(field.getType)
        val classType = getFlixType(clazz)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = classType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = fieldType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.GetStaticField(field, loc) =>
        val fieldType = getFlixType(field.getType)
        val resultTyp = fieldType
        val resultEff = Type.Impure
        liftM(List.empty, resultTyp, resultEff)

      case KindedAst.Expr.PutStaticField(field, exp, loc) =>
        for {
          (valueConstrs, valueTyp, _) <- visitExp(exp)
          fieldTyp <- expectTypeM(expected = getFlixType(field.getType), actual = valueTyp, exp.loc)
          resultTyp = Type.Unit
          resultEff = Type.Impure
        } yield (valueConstrs, resultTyp, resultEff)

      case KindedAst.Expr.NewObject(_, clazz, methods, loc) =>

        /**
          * Performs type inference on the given JVM `method`.
          */
        def inferJvmMethod(method: KindedAst.JvmMethod): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = method match {
          case KindedAst.JvmMethod(ident, fparams, exp, returnTpe, eff, loc) =>

            /**
              * Constrains the given formal parameter to its declared type.
              */
            def inferParam(fparam: KindedAst.FormalParam): InferMonad[Unit] = fparam match {
              case KindedAst.FormalParam(sym, _, tpe, _, loc) =>
                unifyTypeM(sym.tvar, tpe, loc).map(_ => ())
            }

            for {
              _ <- traverseM(fparams)(inferParam)
              (constrs, bodyTpe, bodyEff) <- visitExp(exp)
              _ <- expectTypeM(expected = returnTpe, actual = bodyTpe, exp.loc)
            } yield (constrs, returnTpe, bodyEff)
        }

        for {
          (constrs, _, _) <- traverseM(methods)(inferJvmMethod).map(_.unzip3)
          resultTyp = getFlixType(clazz)
          resultEff = Type.Impure
        } yield (constrs.flatten, resultTyp, resultEff)


      case KindedAst.Expr.NewChannel(exp1, exp2, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = Type.Int32, actual = tpe2, exp2.loc)
          resultTyp <- liftM(tvar)
          resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.GetChannel(exp, tvar, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkReceiver(elmVar, regionVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- expectTypeM(expected = channelType, actual = tpe, exp.loc)
          resultTyp <- unifyTypeM(tvar, elmVar, loc)
          resultEff = Type.mkUnion(eff, regionVar, loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.PutChannel(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val elmVar = Type.freshVar(Kind.Star, loc)
        val channelType = Type.mkSender(elmVar, regionVar, loc)

        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- expectTypeM(expected = channelType, actual = tpe1, exp1.loc)
          _ <- expectTypeM(expected = elmVar, actual = tpe2, exp2.loc)
          resultTyp = Type.mkUnit(loc)
          resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.SelectChannel(rules, default, tvar, loc) =>

        val regionVar = Type.freshVar(Kind.Eff, loc)

        /**
          * Performs type inference on the given select rule `sr0`.
          */
        def inferSelectRule(sr0: KindedAst.SelectChannelRule): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          sr0 match {
            case KindedAst.SelectChannelRule(sym, chan, body) => for {
              (chanConstrs, chanType, eff1) <- visitExp(chan)
              (bodyConstrs, bodyType, eff2) <- visitExp(body)
              _ <- unifyTypeM(chanType, Type.mkReceiver(sym.tvar, regionVar, sym.loc), sym.loc)
              resultCon = chanConstrs ++ bodyConstrs
              resultTyp = bodyType
              resultEff = Type.mkUnion(eff1, eff2, regionVar, loc)
            } yield (resultCon, resultTyp, resultEff)
          }

        /**
          * Performs type inference on the given optional default expression `exp0`.
          */
        def inferDefaultRule(exp0: Option[KindedAst.Expr]): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
          exp0 match {
            case None => liftM(Nil, Type.freshVar(Kind.Star, loc), Type.Pure)
            case Some(exp) => visitExp(exp)
          }

        for {
          (ruleConstrs, ruleTypes, ruleEffs) <- traverseM(rules)(inferSelectRule).map(_.unzip3)
          (defaultConstrs, defaultType, eff2) <- inferDefaultRule(default)
          resultCon = ruleConstrs.flatten ++ defaultConstrs
          resultTyp <- unifyTypeM(tvar :: defaultType :: ruleTypes, loc)
          resultEff = Type.mkUnion(regionVar :: eff2 :: ruleEffs, loc)
        } yield (resultCon, resultTyp, resultEff)

      case KindedAst.Expr.Spawn(exp1, exp2, loc) =>
        val regionVar = Type.freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegion(regionVar, loc)
        for {
          (constrs1, tpe1, _) <- visitExp(exp1)
          (constrs2, tpe2, _) <- visitExp(exp2)
          _ <- expectTypeM(expected = regionType, actual = tpe2, exp2.loc)
          resultTyp = Type.Unit
          resultEff = Type.mkUnion(Type.Impure, regionVar, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.ParYield(frags, exp, loc) =>
        val patterns = frags.map(_.pat)
        val parExps = frags.map(_.exp)
        val patLocs = frags.map(_.loc)
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          patternTypes <- inferPatterns(patterns, root)
          (fragConstrs, fragTypes, fragEffs) <- seqM(parExps map visitExp).map(_.unzip3)
          _ <- seqM(patternTypes.zip(fragTypes).zip(patLocs).map { case ((patTpe, expTpe), l) => unifyTypeM(List(patTpe, expTpe), l) })
          _ <- seqM(fragEffs.zip(patLocs) map { case (p, l) => expectTypeM(expected = Type.Pure, actual = p, l) })
        } yield (constrs ++ fragConstrs.flatten, tpe, eff)

      case KindedAst.Expr.Lazy(exp, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp = Type.mkLazy(tpe, loc)
          resultEff <- expectTypeM(expected = Type.Pure, actual = eff, exp.loc)
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.Force(exp, tvar, loc) =>
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          lazyTyp <- expectTypeM(expected = Type.mkLazy(tvar, loc), actual = tpe, exp.loc)
          resultTyp = tvar
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointConstraintSet(cs, tvar, loc) =>
        for {
          (constrs, constraintTypes) <- traverseM(cs)(visitConstraint).map(_.unzip)
          schemaRow <- unifyTypeAllowEmptyM(constraintTypes, Kind.SchemaRow, loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(schemaRow, loc), loc)
        } yield (constrs.flatten, resultTyp, Type.Pure)

      case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>

        def mkRowExtend(pparam: KindedAst.PredicateParam, restRow: Type): Type = pparam match {
          case KindedAst.PredicateParam(pred, tpe, loc) => Type.mkSchemaRowExtend(pred, tpe, restRow, tpe.loc)
        }

        def mkFullRow(baseRow: Type): Type = pparams.foldRight(baseRow)(mkRowExtend)

        val expectedRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))
        val resultRowType = mkFullRow(Type.freshVar(Kind.SchemaRow, loc))

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          _ <- unifyTypeM(tpe, Type.mkSchema(expectedRowType, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(resultRowType, loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
        //
        //  exp1 : #{...}    exp2 : #{...}
        //  ------------------------------
        //  exp1 <+> exp2 : #{...}
        //
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          resultTyp <- unifyTypeM(tpe1, tpe2, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.FixpointSolve(exp, loc) =>
        //
        //  exp : #{...}
        //  ---------------
        //  solve exp : tpe
        //
        for {
          (constrs, tpe, eff) <- visitExp(exp)
          resultTyp <- unifyTypeM(tpe, Type.mkSchema(mkAnySchemaRowType(loc), loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
        //
        //  exp1 : tpe    exp2 : #{ P : a  | b }
        //  -------------------------------------------
        //  project P exp2 : #{ P : a | c }
        //
        val freshPredicateTypeVar = Type.freshVar(Kind.Predicate, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)
        val freshResultSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshRestSchemaTypeVar, loc), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, freshPredicateTypeVar, freshResultSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
        //
        //  exp : F[freshElmType] where F is Foldable
        //  -------------------------------------------
        //  project exp into A: #{A(freshElmType) | freshRestSchemaType}
        //
        val freshTypeConstructorVar = Type.freshVar(Kind.Star ->: Kind.Star, loc)
        val freshElmTypeVar = Type.freshVar(Kind.Star, loc)
        val freshRestSchemaTypeVar = Type.freshVar(Kind.SchemaRow, loc)

        // Require Order and Foldable instances.
        val orderSym = PredefinedClasses.lookupClassSym("Order", root)
        val foldableSym = PredefinedClasses.lookupClassSym("Foldable", root)
        val order = Ast.TypeConstraint(Ast.TypeConstraint.Head(orderSym, loc), freshElmTypeVar, loc)
        val foldable = Ast.TypeConstraint(Ast.TypeConstraint.Head(foldableSym, loc), freshTypeConstructorVar, loc)

        for {
          (constrs, tpe, eff) <- visitExp(exp)
          expectedType <- unifyTypeM(tpe, Type.mkApply(freshTypeConstructorVar, List(freshElmTypeVar), loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.mkRelation(List(freshElmTypeVar), loc), freshRestSchemaTypeVar, loc), loc), loc)
          resultEff = eff
        } yield (order :: foldable :: constrs, resultTyp, resultEff)

      case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
        //
        //  exp1: {$Result(freshRelOrLat, freshTupleVar) | freshRestSchemaVar }
        //  exp2: freshRestSchemaVar
        //  --------------------------------------------------------------------
        //  FixpointQuery pred, exp1, exp2 : Array[freshTupleVar]
        //
        val freshRelOrLat = Type.freshVar(Kind.Star ->: Kind.Predicate, loc)
        val freshTupleVar = Type.freshVar(Kind.Star, loc)
        val freshRestSchemaVar = Type.freshVar(Kind.SchemaRow, loc)
        val expectedSchemaType = Type.mkSchema(Type.mkSchemaRowExtend(pred, Type.Apply(freshRelOrLat, freshTupleVar, loc), freshRestSchemaVar, loc), loc)
        for {
          (constrs1, tpe1, eff1) <- visitExp(exp1)
          (constrs2, tpe2, eff2) <- visitExp(exp2)
          _ <- unifyTypeM(tpe1, expectedSchemaType, loc)
          _ <- unifyTypeM(tpe2, Type.mkSchema(freshRestSchemaVar, loc), loc)
          resultTyp <- unifyTypeM(tvar, Type.mkVector(freshTupleVar, loc), loc)
          resultEff = Type.mkUnion(eff1, eff2, loc)
        } yield (constrs1 ++ constrs2, resultTyp, resultEff)

      case KindedAst.Expr.Error(m, tvar, pvar) =>
        InferMonad.point((Nil, tvar, pvar))

    }

    /**
      * Infers the type of the given constraint `con0` inside the inference monad.
      */
    def visitConstraint(con0: KindedAst.Constraint): InferMonad[(List[Ast.TypeConstraint], Type)] = {
      val KindedAst.Constraint(cparams, head0, body0, loc) = con0
      //
      //  A_0 : tpe, A_1: tpe, ..., A_n : tpe
      //  -----------------------------------
      //  A_0 :- A_1, ..., A_n : tpe
      //
      for {
        (constrs1, headPredicateType) <- inferHeadPredicate(head0, root)
        (constrs2, bodyPredicateTypes) <- traverseM(body0)(b => inferBodyPredicate(b, root)).map(_.unzip)
        bodyPredicateType <- unifyTypeAllowEmptyM(bodyPredicateTypes, Kind.SchemaRow, loc)
        resultType <- unifyTypeM(headPredicateType, bodyPredicateType, loc)
      } yield (constrs1 ++ constrs2.flatten, resultType)
    }

    visitExp(exp0)
  }

  /**
    * Infers the type and effect of the expression, and checks that they match the expected type and effect.
    */
  private def inferExpectedExp(exp: KindedAst.Expr, tpe0: Type, eff0: Type, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type, Type)] = {
    for {
      (tconstrs, tpe, eff) <- inferExp(exp, root)
      _ <- expectTypeM(expected = tpe0, actual = tpe, exp.loc)
      _ <- expectTypeM(expected = eff0, actual = eff, exp.loc)
    } yield (tconstrs, tpe, eff)
  }

  /**
    * Applies the given substitution `subst0` to the given expression `exp0`.
    */
  private def reassembleExp(exp0: KindedAst.Expr, root: KindedAst.Root, subst0: Substitution): TypedAst.Expr = {
    /**
      * Applies the given substitution `subst0` to the given expression `exp0`.
      */
    def visitExp(exp0: KindedAst.Expr, subst0: Substitution): TypedAst.Expr = exp0 match {

      case KindedAst.Expr.Var(sym, loc) =>
        TypedAst.Expr.Var(sym, subst0(sym.tvar), loc)

      case KindedAst.Expr.Def(sym, tvar, loc) =>
        TypedAst.Expr.Def(sym, subst0(tvar), loc)

      case KindedAst.Expr.Sig(sym, tvar, loc) =>
        TypedAst.Expr.Sig(sym, subst0(tvar), loc)

      case KindedAst.Expr.Hole(sym, tpe, loc) =>
        TypedAst.Expr.Hole(sym, subst0(tpe), loc)

      case KindedAst.Expr.HoleWithExp(exp, tvar, pvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expr.HoleWithExp(e, subst0(tvar), subst0(pvar), loc)

      case KindedAst.Expr.OpenAs(sym, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expr.OpenAs(sym, e, subst0(tvar), loc)

      case KindedAst.Expr.Use(sym, alias, exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expr.Use(sym, alias, e, loc)

      case KindedAst.Expr.Cst(Ast.Constant.Null, loc) =>
        TypedAst.Expr.Cst(Ast.Constant.Null, Type.Null, loc)

      case KindedAst.Expr.Cst(cst, loc) => TypedAst.Expr.Cst(cst, constantType(cst), loc)

      case KindedAst.Expr.Apply(exp, exps, tvar, pvar, loc) =>
        val e = visitExp(exp, subst0)
        val es = exps.map(visitExp(_, subst0))
        TypedAst.Expr.Apply(e, es, subst0(tvar), subst0(pvar), loc)

      case KindedAst.Expr.Lambda(fparam, exp, loc) =>
        val p = visitFormalParam(fparam)
        val e = visitExp(exp, subst0)
        val t = Type.mkArrowWithEffect(p.tpe, e.eff, e.tpe, loc)
        TypedAst.Expr.Lambda(p, e, t, loc)

      case KindedAst.Expr.Unary(sop, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.Unary(sop, e, subst0(tvar), eff, loc)

      case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expr.Binary(sop, e1, e2, subst0(tvar), eff, loc)

      case KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = e2.tpe
        val eff = Type.mkUnion(e1.eff, e2.eff, e3.eff, loc)
        TypedAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case KindedAst.Expr.Stm(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expr.Stm(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.Discard(exp, loc) =>
        val e = visitExp(exp, subst0)
        TypedAst.Expr.Discard(e, e.eff, loc)

      case KindedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expr.Let(sym, mod, e1, e2, tpe, eff, loc)

      case KindedAst.Expr.LetRec(sym, mod, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e2.tpe
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expr.LetRec(sym, mod, e1, e2, tpe, eff, loc)

      case KindedAst.Expr.Region(tpe, loc) =>
        TypedAst.Expr.Region(tpe, loc)

      case KindedAst.Expr.Scope(sym, regionVar, exp, pvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = subst0(pvar)
        TypedAst.Expr.Scope(sym, regionVar, e, tpe, eff, loc)

      case KindedAst.Expr.ScopeExit(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expr.ScopeExit(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.Match(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case KindedAst.MatchRule(pat, guard, exp) =>
            val p = reassemblePattern(pat, root, subst0)
            val g = guard.map(visitExp(_, subst0))
            val b = visitExp(exp, subst0)
            TypedAst.MatchRule(p, g, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkUnion(g.map(_.eff).toList ::: List(b.eff, acc), loc)
        }
        TypedAst.Expr.Match(e1, rs, tpe, eff, loc)

      case KindedAst.Expr.TypeMatch(matchExp, rules, loc) =>
        val e1 = visitExp(matchExp, subst0)
        val rs = rules map {
          case KindedAst.TypeMatchRule(sym, tpe0, exp) =>
            val t = subst0(tpe0)
            val b = visitExp(exp, subst0)
            TypedAst.TypeMatchRule(sym, t, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = rs.foldLeft(e1.eff) {
          case (acc, TypedAst.TypeMatchRule(_, _, b)) => Type.mkUnion(b.eff, acc, loc)
        }
        TypedAst.Expr.TypeMatch(e1, rs, tpe, eff, loc)

      case KindedAst.Expr.RelationalChoose(_, exps, rules, tvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val rs = rules.map {
          case KindedAst.RelationalChooseRule(pat0, exp) =>
            val pat = pat0.map {
              case KindedAst.RelationalChoosePattern.Wild(loc) => TypedAst.RelationalChoosePattern.Wild(loc)
              case KindedAst.RelationalChoosePattern.Absent(loc) => TypedAst.RelationalChoosePattern.Absent(loc)
              case KindedAst.RelationalChoosePattern.Present(sym, tvar, loc) => TypedAst.RelationalChoosePattern.Present(sym, subst0(tvar), loc)
            }
            TypedAst.RelationalChooseRule(pat, visitExp(exp, subst0))
        }
        val tpe = subst0(tvar)
        val eff = Type.mkAnd(rs.map(_.exp.eff), loc)
        TypedAst.Expr.RelationalChoose(es, rs, tpe, eff, loc)

      case KindedAst.Expr.RestrictableChoose(star, exp, rules, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules.map {
          case KindedAst.RestrictableChooseRule(pat0, body0) =>
            val pat = pat0 match {
              case KindedAst.RestrictableChoosePattern.Tag(sym, pats, tvar, loc) =>
                val ps = pats.map {
                  case KindedAst.RestrictableChoosePattern.Wild(tvar, loc) => TypedAst.RestrictableChoosePattern.Wild(subst0(tvar), loc)
                  case KindedAst.RestrictableChoosePattern.Var(sym, tvar, loc) => TypedAst.RestrictableChoosePattern.Var(sym, subst0(tvar), loc)
                }
                TypedAst.RestrictableChoosePattern.Tag(sym, ps, subst0(tvar), loc)
            }
            val body = visitExp(body0, subst0)
            TypedAst.RestrictableChooseRule(pat, body)
        }
        val eff = Type.mkUnion(rs.map(_.exp.eff), loc)
        TypedAst.Expr.RestrictableChoose(star, e, rs, subst0(tvar), eff, loc)

      case KindedAst.Expr.Tag(sym, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.Tag(sym, e, subst0(tvar), eff, loc)

      case KindedAst.Expr.RestrictableTag(sym, exp, _, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.RestrictableTag(sym, e, subst0(tvar), eff, loc)

      case KindedAst.Expr.Tuple(elms, loc) =>
        val es = elms.map(visitExp(_, subst0))
        val tpe = Type.mkTuple(es.map(_.tpe), loc)
        val eff = Type.mkUnion(es.map(_.eff), loc)
        TypedAst.Expr.Tuple(es, tpe, eff, loc)

      case KindedAst.Expr.RecordEmpty(loc) =>
        TypedAst.Expr.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

      case KindedAst.Expr.RecordSelect(exp, field, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.RecordSelect(e, field, subst0(tvar), eff, loc)

      case KindedAst.Expr.RecordExtend(field, value, rest, tvar, loc) =>
        val v = visitExp(value, subst0)
        val r = visitExp(rest, subst0)
        val eff = Type.mkUnion(v.eff, r.eff, loc)
        TypedAst.Expr.RecordExtend(field, v, r, subst0(tvar), eff, loc)

      case KindedAst.Expr.RecordRestrict(field, rest, tvar, loc) =>
        val r = visitExp(rest, subst0)
        val eff = r.eff
        TypedAst.Expr.RecordRestrict(field, r, subst0(tvar), eff, loc)

      case KindedAst.Expr.ArrayLit(exps, exp, tvar, pvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.ArrayLit(es, e, tpe, eff, loc)

      case KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)

      case KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.ArrayLoad(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val e3 = visitExp(exp3, subst0)
        val eff = subst0(pvar)
        TypedAst.Expr.ArrayStore(e1, e2, e3, eff, loc)

      case KindedAst.Expr.ArrayLength(exp, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.ArrayLength(e, eff, loc)

      case KindedAst.Expr.VectorLit(exps, tvar, pvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.VectorLit(es, tpe, eff, loc)

      case KindedAst.Expr.VectorLoad(exp1, exp2, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.VectorLength(e, loc)

      case KindedAst.Expr.Ref(exp1, exp2, tvar, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.Ref(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.Deref(exp, tvar, pvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.Deref(e, tpe, eff, loc)

      case KindedAst.Expr.Assign(exp1, exp2, pvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = subst0(pvar)
        TypedAst.Expr.Assign(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.Ascribe(exp, _, _, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.Ascribe(e, subst0(tvar), eff, loc)

      case KindedAst.Expr.InstanceOf(exp, clazz, loc) =>
        val e1 = visitExp(exp, subst0)
        TypedAst.Expr.InstanceOf(e1, clazz, loc)

      case KindedAst.Expr.CheckedCast(cast, exp, tvar, pvar, loc) =>
        cast match {
          case CheckedCastType.TypeCast =>
            val e = visitExp(exp, subst0)
            val tpe = subst0(tvar)
            TypedAst.Expr.CheckedCast(cast, e, tpe, e.eff, loc)
          case CheckedCastType.EffectCast =>
            val e = visitExp(exp, subst0)
            val eff = Type.mkUnion(e.eff, subst0(pvar), loc)
            TypedAst.Expr.CheckedCast(cast, e, e.tpe, eff, loc)
        }

      case KindedAst.Expr.UncheckedCast(KindedAst.Expr.Cst(Ast.Constant.Null, _), _, _, tvar, loc) =>
        val t = subst0(tvar)
        TypedAst.Expr.Cst(Ast.Constant.Null, t, loc)

      case KindedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val dt = declaredType.map(tpe => subst0(tpe))
        val dp = declaredEff.map(eff => subst0(eff))
        val tpe = subst0(tvar)
        val eff = declaredEff.getOrElse(e.eff)
        TypedAst.Expr.UncheckedCast(e, dt, dp, tpe, eff, loc)

      case KindedAst.Expr.UncheckedMaskingCast(exp, loc) =>
        // We explicitly mark a `Mask` expression as Impure.
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = Type.Impure
        TypedAst.Expr.UncheckedMaskingCast(e, tpe, eff, loc)

      case KindedAst.Expr.Without(exp, effUse, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = e.eff
        TypedAst.Expr.Without(e, effUse, tpe, eff, loc)

      case KindedAst.Expr.TryCatch(exp, rules, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case KindedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body, subst0)
            TypedAst.CatchRule(sym, clazz, b)
        }
        val tpe = rs.head.exp.tpe
        val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
        TypedAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case KindedAst.Expr.TryWith(exp, effUse, rules, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val rs = rules map {
          case KindedAst.HandlerRule(op, fparams, hexp, htvar) =>
            val fps = fparams.map(visitFormalParam)
            val he = visitExp(hexp, subst0)
            TypedAst.HandlerRule(op, fps, he)
        }
        val tpe = subst0(tvar)
        val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
        TypedAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case KindedAst.Expr.Do(op, exps, tvar, loc) =>
        val es = exps.map(visitExp(_, subst0))
        val tpe = subst0(tvar)
        val eff1 = Type.Cst(TypeConstructor.Effect(op.sym.eff), op.loc.asSynthetic)
        val eff = Type.mkUnion(eff1 :: es.map(_.eff), loc)
        TypedAst.Expr.Do(op, es, tpe, eff, loc)

      case KindedAst.Expr.Resume(exp, _, retTvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(retTvar)
        TypedAst.Expr.Resume(e, tpe, loc)

      case KindedAst.Expr.InvokeConstructor(constructor, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(constructor.getDeclaringClass)
        val eff = Type.Impure
        TypedAst.Expr.InvokeConstructor(constructor, as, tpe, eff, loc)

      case KindedAst.Expr.InvokeMethod(method, _, exp, args, loc) =>
        val e = visitExp(exp, subst0)
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expr.InvokeMethod(method, e, as, tpe, eff, loc)

      case KindedAst.Expr.InvokeStaticMethod(method, args, loc) =>
        val as = args.map(visitExp(_, subst0))
        val tpe = getFlixType(method.getReturnType)
        val eff = Type.Impure
        TypedAst.Expr.InvokeStaticMethod(method, as, tpe, eff, loc)

      case KindedAst.Expr.GetField(field, _, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expr.GetField(field, e, tpe, eff, loc)

      case KindedAst.Expr.PutField(field, _, exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expr.PutField(field, e1, e2, tpe, eff, loc)

      case KindedAst.Expr.GetStaticField(field, loc) =>
        val tpe = getFlixType(field.getType)
        val eff = Type.Impure
        TypedAst.Expr.GetStaticField(field, tpe, eff, loc)

      case KindedAst.Expr.PutStaticField(field, exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expr.PutStaticField(field, e, tpe, eff, loc)

      case KindedAst.Expr.NewObject(name, clazz, methods, loc) =>
        val tpe = getFlixType(clazz)
        val eff = Type.Impure
        val ms = methods map visitJvmMethod
        TypedAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)

      case KindedAst.Expr.NewChannel(exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val eff = Type.Impure
        TypedAst.Expr.NewChannel(e1, e2, subst0(tvar), eff, loc)

      case KindedAst.Expr.GetChannel(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = Type.Impure
        TypedAst.Expr.GetChannel(e, subst0(tvar), eff, loc)

      case KindedAst.Expr.PutChannel(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.mkUnit(loc)
        val eff = Type.Impure
        TypedAst.Expr.PutChannel(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.SelectChannel(rules, default, tvar, loc) =>
        val rs = rules map {
          case KindedAst.SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, subst0)
            val b = visitExp(exp, subst0)
            TypedAst.SelectChannelRule(sym, c, b)
        }
        val d = default.map(visitExp(_, subst0))
        val eff = Type.Impure
        TypedAst.Expr.SelectChannel(rs, d, subst0(tvar), eff, loc)

      case KindedAst.Expr.Spawn(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = Type.Unit
        val eff = Type.Impure
        TypedAst.Expr.Spawn(e1, e2, tpe, eff, loc)

      case KindedAst.Expr.ParYield(frags, exp, loc) =>
        val e = visitExp(exp, subst0)
        val fs = frags map {
          case KindedAst.ParYieldFragment(pat, e0, l0) =>
            val p = reassemblePattern(pat, root, subst0)
            val e1 = visitExp(e0, subst0)
            TypedAst.ParYieldFragment(p, e1, l0)
        }
        val tpe = e.tpe
        val eff = fs.foldLeft(e.eff) {
          case (acc, TypedAst.ParYieldFragment(_, e1, _)) => Type.mkUnion(acc, e1.eff, loc)
        }
        TypedAst.Expr.ParYield(fs, e, tpe, eff, loc)

      case KindedAst.Expr.Lazy(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = Type.mkLazy(e.tpe, loc)
        TypedAst.Expr.Lazy(e, tpe, loc)

      case KindedAst.Expr.Force(exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = e.eff
        TypedAst.Expr.Force(e, tpe, eff, loc)

      case KindedAst.Expr.FixpointConstraintSet(cs0, tvar, loc) =>
        val cs = cs0.map(visitConstraint)
        TypedAst.Expr.FixpointConstraintSet(cs, Stratification.empty, subst0(tvar), loc)

      case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>
        val ps = pparams.map(visitPredicateParam)
        val e = visitExp(exp, subst0)
        val tpe = subst0(tvar)
        val eff = e.eff
        TypedAst.Expr.FixpointLambda(ps, e, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val tpe = e1.tpe
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)
        TypedAst.Expr.FixpointMerge(e1, e2, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expr.FixpointSolve(exp, loc) =>
        val e = visitExp(exp, subst0)
        val tpe = e.tpe
        val eff = e.eff
        TypedAst.Expr.FixpointSolve(e, Stratification.empty, tpe, eff, loc)

      case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.FixpointFilter(pred, e, subst0(tvar), eff, loc)

      case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
        val e = visitExp(exp, subst0)
        val eff = e.eff
        TypedAst.Expr.FixpointInject(e, pred, subst0(tvar), eff, loc)

      case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        val stf = Stratification.empty
        val tpe = subst0(tvar)
        val eff = Type.mkUnion(e1.eff, e2.eff, loc)

        // Note: This transformation should happen in the Weeder but it is here because
        // `#{#Result(..)` | _} cannot be unified with `#{A(..)}` (a closed row).
        // See Weeder for more details.
        val mergeExp = TypedAst.Expr.FixpointMerge(e1, e2, stf, e1.tpe, eff, loc)
        val solveExp = TypedAst.Expr.FixpointSolve(mergeExp, stf, e1.tpe, eff, loc)
        TypedAst.Expr.FixpointProject(pred, solveExp, tpe, eff, loc)

      case KindedAst.Expr.Error(m, tvar, pvar) =>
        val tpe = subst0(tvar)
        val eff = subst0(pvar)
        TypedAst.Expr.Error(m, tpe, eff)

    }

    /**
      * Applies the substitution to the given constraint.
      */
    def visitConstraint(c0: KindedAst.Constraint): TypedAst.Constraint = {
      // Pattern match on the constraint.
      val KindedAst.Constraint(cparams0, head0, body0, loc) = c0

      // Unification was successful. Reassemble the head and body predicates.
      val head = reassembleHeadPredicate(head0, root, subst0)
      val body = body0.map(b => reassembleBodyPredicate(b, root, subst0))

      // Reassemble the constraint parameters.
      val cparams = cparams0.map {
        case KindedAst.ConstraintParam(sym, l) =>
          TypedAst.ConstraintParam(sym, subst0(sym.tvar), l)
      }

      // Reassemble the constraint.
      TypedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Applies the substitution to the given list of formal parameters.
      */
    def visitFormalParam(fparam: KindedAst.FormalParam): TypedAst.FormalParam =
      TypedAst.FormalParam(fparam.sym, fparam.mod, subst0(fparam.tpe), fparam.src, fparam.loc)

    /**
      * Applies the substitution to the given list of predicate parameters.
      */
    def visitPredicateParam(pparam: KindedAst.PredicateParam): TypedAst.PredicateParam =
      TypedAst.PredicateParam(pparam.pred, subst0(pparam.tpe), pparam.loc)

    /**
      * Applies the substitution to the given jvm method.
      */
    def visitJvmMethod(method: KindedAst.JvmMethod): TypedAst.JvmMethod = {
      method match {
        case KindedAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
          val fparams = getFormalParams(fparams0, subst0)
          val exp = visitExp(exp0, subst0)
          TypedAst.JvmMethod(ident, fparams, exp, tpe, eff, loc)
      }
    }

    visitExp(exp0, subst0)
  }

  /**
    * Infers the type of the given pattern `pat0`.
    */
  private def inferPattern(pat0: KindedAst.Pattern, root: KindedAst.Root)(implicit flix: Flix): InferMonad[Type] = {

    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.Pattern): InferMonad[Type] = p match {
      case KindedAst.Pattern.Wild(tvar, loc) => liftM(tvar)

      case KindedAst.Pattern.Var(sym, tvar, loc) => unifyTypeM(sym.tvar, tvar, loc)

      case KindedAst.Pattern.Cst(Ast.Constant.Unit, loc) => liftM(Type.Unit)

      case KindedAst.Pattern.Cst(Ast.Constant.Bool(b), loc) => liftM(Type.Bool)

      case KindedAst.Pattern.Cst(Ast.Constant.Char(c), loc) => liftM(Type.Char)

      case KindedAst.Pattern.Cst(Ast.Constant.Float32(i), loc) => liftM(Type.Float32)

      case KindedAst.Pattern.Cst(Ast.Constant.Float64(i), loc) => liftM(Type.Float64)

      case KindedAst.Pattern.Cst(Ast.Constant.BigDecimal(i), loc) => liftM(Type.BigDecimal)

      case KindedAst.Pattern.Cst(Ast.Constant.Int8(i), loc) => liftM(Type.Int8)

      case KindedAst.Pattern.Cst(Ast.Constant.Int16(i), loc) => liftM(Type.Int16)

      case KindedAst.Pattern.Cst(Ast.Constant.Int32(i), loc) => liftM(Type.Int32)

      case KindedAst.Pattern.Cst(Ast.Constant.Int64(i), loc) => liftM(Type.Int64)

      case KindedAst.Pattern.Cst(Ast.Constant.BigInt(i), loc) => liftM(Type.BigInt)

      case KindedAst.Pattern.Cst(Ast.Constant.Str(s), loc) => liftM(Type.Str)

      case KindedAst.Pattern.Cst(Ast.Constant.Regex(s), loc) => liftM(Type.Regex)

      case KindedAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)

      case KindedAst.Pattern.Tag(symUse, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.enums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, tagType) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        for {
          tpe <- visit(pat)
          _ <- unifyTypeM(tagType, Type.mkPureArrow(tpe, tvar, loc), loc)
          resultTyp = tvar
        } yield resultTyp

      case KindedAst.Pattern.Tuple(elms, loc) =>
        for {
          elementTypes <- traverseM(elms)(visit)
        } yield Type.mkTuple(elementTypes, loc)

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def inferPatterns(pats0: List[KindedAst.Pattern], root: KindedAst.Root)(implicit flix: Flix): InferMonad[List[Type]] = {
    traverseM(pats0)(inferPattern(_, root))
  }

  /**
    * Applies the substitution `subst0` to the given pattern `pat0`.
    */
  private def reassemblePattern(pat0: KindedAst.Pattern, root: KindedAst.Root, subst0: Substitution): TypedAst.Pattern = {
    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.Pattern): TypedAst.Pattern = p match {
      case KindedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst0(tvar), loc)
      case KindedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst0(tvar), loc)
      case KindedAst.Pattern.Cst(cst, loc) => TypedAst.Pattern.Cst(cst, constantType(cst), loc)

      case KindedAst.Pattern.Tag(sym, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, visit(pat), subst0(tvar), loc)

      case KindedAst.Pattern.Tuple(elms, loc) =>
        val es = elms.map(visit)
        val tpe = Type.mkTuple(es.map(_.tpe), loc)
        TypedAst.Pattern.Tuple(es, tpe, loc)

    }

    visit(pat0)
  }

  /**
    * Infers the type of the given head predicate.
    */
  private def inferHeadPredicate(head: KindedAst.Predicate.Head, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = head match {
    case KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc) =>
      // Adds additional type constraints if the denotation is a lattice.
      val restRow = Type.freshVar(Kind.SchemaRow, loc)
      for {
        (termConstrs, termTypes, termEffs) <- traverseM(terms)(inferExp(_, root)).map(_.unzip3)
        pureTermEffs <- unifyEffM(Type.Pure, Type.mkUnion(termEffs, loc), loc)
        predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
        tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
      } yield (termConstrs.flatten ++ tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))
  }

  /**
    * Applies the given substitution `subst0` to the given head predicate `head0`.
    */
  private def reassembleHeadPredicate(head0: KindedAst.Predicate.Head, root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case KindedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => reassembleExp(t, root, subst0))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst0(tvar), loc)
  }

  /**
    * Infers the type of the given body predicate.
    */
  private def inferBodyPredicate(body0: KindedAst.Predicate.Body, root: KindedAst.Root)(implicit flix: Flix): InferMonad[(List[Ast.TypeConstraint], Type)] = {

    body0 match {
      case KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, tvar, loc) =>
        val restRow = Type.freshVar(Kind.SchemaRow, loc)
        for {
          termTypes <- traverseM(terms)(inferPattern(_, root))
          predicateType <- unifyTypeM(tvar, mkRelationOrLatticeType(pred.name, den, termTypes, root, loc), loc)
          tconstrs = getTermTypeClassConstraints(den, termTypes, root, loc)
        } yield (tconstrs, Type.mkSchemaRowExtend(pred, predicateType, restRow, loc))

      case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
        val tupleType = Type.mkTuplish(outVars.map(_.tvar), loc)
        val expectedType = Type.mkVector(tupleType, loc)
        for {
          (constrs, tpe, eff) <- inferExp(exp, root)
          expTyp <- unifyTypeM(expectedType, tpe, loc)
          expEff <- unifyEffM(Type.Pure, eff, loc)
        } yield (constrs, mkAnySchemaRowType(loc))

      case KindedAst.Predicate.Body.Guard(exp, loc) =>
        for {
          (constrs, tpe, eff) <- inferExp(exp, root)
          expEff <- unifyEffM(Type.Pure, eff, loc)
          expTyp <- unifyTypeM(Type.Bool, tpe, loc)
        } yield (constrs, mkAnySchemaRowType(loc))
    }
  }

  /**
    * Applies the given substitution `subst0` to the given body predicate `body0`.
    */
  private def reassembleBodyPredicate(body0: KindedAst.Predicate.Body, root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, terms, tvar, loc) =>
      val ts = terms.map(t => reassemblePattern(t, root, subst0))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, ts, subst0(tvar), loc)

    case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Functional(outVars, e, loc)

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      val e = reassembleExp(exp, root, subst0)
      TypedAst.Predicate.Body.Guard(e, loc)

  }

  /**
    * Returns the relation or lattice type of `name` with the term types `ts`.
    */
  private def mkRelationOrLatticeType(name: String, den: Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation)(implicit flix: Flix): Type = den match {
    case Denotation.Relational => Type.mkRelation(ts, loc)
    case Denotation.Latticenal => Type.mkLattice(ts, loc)
  }

  /**
    * Returns the type class constraints for the given term types `ts` with the given denotation `den`.
    */
  private def getTermTypeClassConstraints(den: Ast.Denotation, ts: List[Type], root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = den match {
    case Denotation.Relational =>
      ts.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc))
    case Denotation.Latticenal =>
      ts.init.flatMap(mkTypeClassConstraintsForRelationalTerm(_, root, loc)) ::: mkTypeClassConstraintsForLatticeTerm(ts.last, root, loc)
  }

  /**
    * Constructs the type class constraints for the given relational term type `tpe`.
    */
  private def mkTypeClassConstraintsForRelationalTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Constructs the type class constraints for the given lattice term type `tpe`.
    */
  private def mkTypeClassConstraintsForLatticeTerm(tpe: Type, root: KindedAst.Root, loc: SourceLocation): List[Ast.TypeConstraint] = {
    val classes = List(
      PredefinedClasses.lookupClassSym("Eq", root),
      PredefinedClasses.lookupClassSym("Order", root),
      PredefinedClasses.lookupClassSym("PartialOrder", root),
      PredefinedClasses.lookupClassSym("LowerBound", root),
      PredefinedClasses.lookupClassSym("JoinLattice", root),
      PredefinedClasses.lookupClassSym("MeetLattice", root),
    )
    classes.map(clazz => Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe, loc))
  }

  /**
    * Returns a substitution from formal parameters to their declared types.
    *
    * Performs type resolution of the declared type of each formal parameters.
    */
  private def getSubstFromParams(params: List[KindedAst.FormalParam])(implicit flix: Flix): Substitution = {
    // Compute the substitution by mapping the symbol of each parameter to its declared type.
    val declaredTypes = params.map(_.tpe)
    (params zip declaredTypes).foldLeft(Substitution.empty) {
      case (macc, (KindedAst.FormalParam(sym, _, _, _, _), declaredType)) =>
        macc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(declaredType))
    }
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit flix: Flix): Type = {
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
    * Collects all the type variables from the formal params and sets them as rigid.
    */
  private def getRigidityFromParams(params: List[KindedAst.FormalParam])(implicit flix: Flix): RigidityEnv = {
    params.flatMap(_.tpe.typeVars).foldLeft(RigidityEnv.empty) {
      case (renv, tvar) => renv.markRigid(tvar.sym)
    }
  }

  /**
    * Returns the typed version of the given type parameters `tparams0`.
    */
  private def getTypeParams(tparams0: List[KindedAst.TypeParam]): List[TypedAst.TypeParam] = tparams0.map {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Returns the typed version of the given formal parameters `fparams0`.
    */
  private def getFormalParams(fparams0: List[KindedAst.FormalParam], subst0: Substitution): List[TypedAst.FormalParam] = fparams0.map {
    case KindedAst.FormalParam(sym, mod, tpe, src, loc) => TypedAst.FormalParam(sym, mod, subst0(tpe), src, sym.loc)
  }

  /**
    * Returns an open schema type.
    */
  private def mkAnySchemaRowType(loc: SourceLocation)(implicit flix: Flix): Type = Type.freshVar(Kind.SchemaRow, loc)

  /**
    * Returns the type of the given constant.
    */
  private def constantType(cst: Ast.Constant): Type = cst match {
    case Constant.Unit => Type.Unit
    case Constant.Null => Type.Null
    case Constant.Bool(_) => Type.Bool
    case Constant.Char(_) => Type.Char
    case Constant.Float32(_) => Type.Float32
    case Constant.Float64(_) => Type.Float64
    case Constant.BigDecimal(_) => Type.BigDecimal
    case Constant.Int8(_) => Type.Int8
    case Constant.Int16(_) => Type.Int16
    case Constant.Int32(_) => Type.Int32
    case Constant.Int64(_) => Type.Int64
    case Constant.BigInt(_) => Type.BigInt
    case Constant.Str(_) => Type.Str
    case Constant.Regex(_) => Type.Regex
  }


  /**
    * Computes and prints statistics about the given substitution.
    */
  private def printStatistics(m: Map[Symbol.DefnSym, Substitution]): Unit = {
    val t = new AsciiTable().
      withTitle("Substitution Statistics").
      withCols("Def", "Type Vars", "Mean Type Size", "Median Type Size", "Total Type Size")

    for ((sym, subst) <- m) {
      val size = subst.m.size
      val sizes = subst.m.values.map(_.size.toLong).toList
      val mean = StatUtils.avg(sizes)
      val median = StatUtils.median(sizes)
      val total = sizes.sum
      t.mkRow(List(sym.toString, size, f"$mean%2.1f", median, total))
    }
    t.write(new PrintWriter(System.out))
  }
}
