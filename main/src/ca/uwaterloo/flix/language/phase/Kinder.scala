/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Kind.WildCaseSet
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.{Denotation, Scope}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.EqualityEnvironment
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unify
import ca.uwaterloo.flix.util.Validation.{flatMapN, fold, mapN, traverse, traverseOpt}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import ca.uwaterloo.flix.language.phase.unification.Substitution

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * Attributes kinds to the types in the AST.
  *
  * For enums, structs, traits, instances, and type aliases:
  * Either:
  *   - type parameters are not annotated and are then assumed all to be Star, or
  *   - type parameters are all annotated with their kinds.
  *
  * For defs:
  * Either:
  *   - type parameters are all annotated with their kinds, or
  *   - type parameters are not annotated and their kinds are inferred from their
  *     use in the formal parameters, return type and effect, and type constraints.
  *     This inference uses the following rules:
  *       - If the type variable is the type of a formal parameter, it is ascribed kind Star.
  *       - If the type variable is the return type of the function, it is ascribed kind Star.
  *       - If the type variable is the purity type of the function, it is ascribed kind Eff.
  *       - If the type variable is an argument to a type constraint, it is ascribed the trait's parameter kind
  *       - If the type variable is an argument to a type constructor, it is ascribed the type constructor's parameter kind.
  *       - If the type variable is used as an type constructor, it is ascribed the kind Star -> Star ... -> Star -> X,
  *         where X is the kind inferred from enacting these rules in the place of the fully-applied type.
  *       - If there is an inconsistency among these kinds, an error is raised.
  *
  * In inferring types, variable type constructors are assumed to have kind * -> * -> * -> ???.
  *
  */
object Kinder {

  def run(root: ResolvedAst.Root, oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[KindedAst.Root, KindError] = flix.phase("Kinder") {
    implicit val sctx: SharedContext = SharedContext.mk()

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    flatMapN(visitTypeAliases(root.taOrder, root)) {
      taenv =>

        val enumsVal = ParOps.parTraverseValues(root.enums)(visitEnum(_, taenv, root))

        val structsVal = ParOps.parTraverseValues(root.structs)(visitStruct(_, taenv, root))

        val restrictableEnumsVal = ParOps.parTraverseValues(root.restrictableEnums)(visitRestrictableEnum(_, taenv, root))

        val traitsVal = visitTraits(root, taenv, oldRoot, changeSet)

        val defsVal = visitDefs(root, taenv, oldRoot, changeSet)

        val instancesVal = ParOps.parTraverseValues(root.instances)(traverse(_)(i => visitInstance(i, taenv, root)))

        val effectsVal = ParOps.parTraverseValues(root.effects)(visitEffect(_, taenv, root))

        mapN(enumsVal, structsVal, restrictableEnumsVal, traitsVal, defsVal, instancesVal, effectsVal) {
          case (enums, structs, restrictableEnums, traits, defs, instances, effects) =>
            KindedAst.Root(traits, instances, defs, enums, structs, restrictableEnums, effects, taenv, root.uses, root.entryPoint, root.sources, root.names)
        }
    }.withSoftFailures(sctx.errors.asScala)
  }(DebugValidation())

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum0: ResolvedAst.Declaration.Enum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Enum, KindError] = enum0 match {
    case ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives, cases0, loc) =>
      val kenv = getKindEnvFromTypeParams(tparams0)

      val tparamsVal = traverse(tparams0)(visitTypeParam(_, kenv))

      flatMapN(tparamsVal) {
        case tparams =>
          val targs = tparams.map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
          val tpe = Type.mkApply(Type.Cst(TypeConstructor.Enum(sym, getEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
          val casesVal = traverse(cases0) {
            case case0 => mapN(visitCase(case0, tparams, tpe, kenv, taenv, root)) {
              caze => caze.sym -> caze
            }
          }
          mapN(casesVal) {
            case cases => KindedAst.Enum(doc, ann, mod, sym, tparams, derives, cases.toMap, tpe, loc)
          }
      }
  }

  /**
    * Performs kinding on the given struct.
    */
  private def visitStruct(struct0: ResolvedAst.Declaration.Struct, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Struct, KindError] = struct0 match {
    case ResolvedAst.Declaration.Struct(doc, ann, mod, sym, tparams0, fields0, loc) =>
      // In the case in which the user doesn't supply any type params,
      // the parser will have already notified the user of this error
      // The recovery step here is to simply add a single type param that is never used
      val tparams1 = if (tparams0.isEmpty) {
        val regionTparam = ResolvedAst.TypeParam.Unkinded(Name.Ident("$rc", loc), Symbol.freshUnkindedTypeVarSym(Ast.VarText.Absent, isRegion = false, loc)(Scope.Top, flix), loc)
        List(regionTparam)
      } else {
        tparams0
      }
      val kenv1 = getKindEnvFromTypeParams(tparams1.init)
      val kenv2 = getKindEnvFromRegion(tparams1.last)
      // The last add is simply to verify that the last tparam was marked as Eff
      val kenvVal = KindEnv.disjointAppend(kenv1, kenv2) + (tparams1.last.sym -> Kind.Eff)
      flatMapN(kenvVal) {
        case kenv =>
          val tparamsVal = traverse(tparams1)(visitTypeParam(_, kenv))

          flatMapN(tparamsVal) {
            case kindedTparams =>
              val fieldsVal = traverse(fields0)(visitStructField(_, kindedTparams, kenv, taenv, root))
              mapN(fieldsVal) {
                case fields =>
                  val targs = kindedTparams.map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
                  val sc = Scheme(kindedTparams.map(_.sym), List(), List(), Type.mkStruct(sym, targs, loc))
                  KindedAst.Struct(doc, ann, mod, sym, kindedTparams, sc, fields, loc)
              }
          }
      }
  }

  /**
    * Performs kinding on the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: ResolvedAst.Declaration.RestrictableEnum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.RestrictableEnum, KindError] = enum0 match {
    case ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives, cases0, loc) =>
      val kenvIndex = getKindEnvFromIndex(index0, sym)
      val kenvTparams = getKindEnvFromTypeParams(tparams0)
      val kenv = KindEnv.disjointAppend(kenvIndex, kenvTparams)

      val indexVal = visitIndex(index0, sym, kenv)
      val tparamsVal = traverse(tparams0)(visitTypeParam(_, kenv))

      flatMapN(indexVal, tparamsVal) {
        case (index, tparams) =>
          val targs = (index :: tparams).map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
          val tpe = Type.mkApply(Type.Cst(TypeConstructor.RestrictableEnum(sym, getRestrictableEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
          val casesVal = traverse(cases0) {
            case case0 => mapN(visitRestrictableCase(case0, index, tparams, tpe, kenv, taenv, root)) {
              caze => caze.sym -> caze
            }
          }
          mapN(casesVal) {
            case cases => KindedAst.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases.toMap, tpe, loc)
          }
      }

  }

  /**
    * Performs kinding on the given type alias.
    * Returns the kind of the type alias.
    */
  private def visitTypeAlias(alias: ResolvedAst.Declaration.TypeAlias, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.TypeAlias, KindError] = alias match {
    case ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParams(tparams0)

      val tparamsVal = traverse(tparams0)(visitTypeParam(_, kenv))
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, taenv, root)

      mapN(tparamsVal, tpeVal) {
        case (tparams, tpe) => KindedAst.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type aliases.
    * The aliases must be sorted topologically.
    */
  private def visitTypeAliases(aliases: List[Symbol.TypeAliasSym], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], KindError] = {
    fold(aliases, Map.empty[Symbol.TypeAliasSym, KindedAst.TypeAlias]) {
      case (taenv, sym) =>
        val alias = root.typeAliases(sym)
        mapN(visitTypeAlias(alias, taenv, root)) {
          case kind => taenv + (sym -> kind)
        }
    }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitCase(caze0: ResolvedAst.Declaration.Case, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Declaration.Case(sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          val quants = tparams.map(_.sym)
          val sc = Scheme(quants, Nil, Nil, Type.mkPureArrow(tpe, resTpe, sym.loc.asSynthetic))
          KindedAst.Case(sym, tpe, sc, loc)
      }
  }

  /**
    * Performs kinding on the given struct field under the given kind environment.
    */
  private def visitStructField(field0: ResolvedAst.Declaration.StructField, tparams: List[KindedAst.TypeParam], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.StructField, KindError] = field0 match {
    case ResolvedAst.Declaration.StructField(mod, sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          KindedAst.StructField(mod, sym, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitRestrictableCase(caze0: ResolvedAst.Declaration.RestrictableCase, index: KindedAst.TypeParam, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.RestrictableCase, KindError] = caze0 match {
    case ResolvedAst.Declaration.RestrictableCase(sym, tpe0, loc) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      mapN(tpeVal) {
        case tpe =>
          val quants = (index :: tparams).map(_.sym)
          val sc = Scheme(quants, Nil, Nil, Type.mkPureArrow(tpe, resTpe, sym.loc.asSynthetic))
          KindedAst.RestrictableCase(sym, tpe, sc, loc) // TODO RESTR-VARS the scheme is different for these. REVISIT
      }
  }

  /**
    * Performs kinding on the all the traits in the given root.
    */
  private def visitTraits(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit sctx: SharedContext, flix: Flix): Validation[Map[Symbol.TraitSym, KindedAst.Trait], KindError] = {
    val (staleTraits, freshTraits) = changeSet.partition(root.traits, oldRoot.traits)

    val result = ParOps.parTraverseValues(staleTraits)(visitTrait(_, taenv, root))
    mapN(result)(freshTraits ++ _)
  }

  /**
    * Performs kinding on the given trait.
    */
  private def visitTrait(trt: ResolvedAst.Declaration.Trait, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Trait, KindError] = trt match {
    case ResolvedAst.Declaration.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParam(tparam0)

      val tparamsVal = visitTypeParam(tparam0, kenv)
      val superTraitsVal = traverse(superTraits0)(visitTraitConstraint(_, kenv, taenv, root))
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, kenv, taenv, root))
      flatMapN(tparamsVal, superTraitsVal, assocsVal) {
        case (tparam, superTraits, assocs) =>
          val sigsVal = traverse(sigs0) {
            case (sigSym, sig0) => mapN(visitSig(sig0, tparam, superTraits, kenv, taenv, root))(sig => sigSym -> sig)
          }
          val lawsVal = traverse(laws0)(visitDef(_, Nil, kenv, taenv, root)) // TODO ASSOC-TYPES need to include super traits?
          mapN(sigsVal, lawsVal) {
            case (sigs, laws) => KindedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs.toMap, laws, loc)
          }

      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Declaration.Instance, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Declaration.Instance(doc, ann, mod, trt, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val kind = getTraitKind(root.traits(trt.sym))

      val kenvVal = inferType(tpe0, kind, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val tpeVal = visitType(tpe0, kind, kenv, taenv, root)
          val tconstrsVal = traverse(tconstrs0)(visitTraitConstraint(_, kenv, taenv, root))
          val assocsVal = traverse(assocs0)(visitAssocTypeDef(_, kind, kenv, taenv, root))
          flatMapN(tpeVal, tconstrsVal, assocsVal) {
            case (tpe, tconstrs, assocs) =>
              val defsVal = traverse(defs0)(visitDef(_, tconstrs, kenv, taenv, root))
              mapN(defsVal) {
                case defs => KindedAst.Instance(doc, ann, mod, trt, tpe, tconstrs, assocs, defs, ns, loc)
              }
          }
      }
  }

  /**
    * Performs kinding on the given effect declaration.
    */
  private def visitEffect(eff: ResolvedAst.Declaration.Effect, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Effect, KindError] = eff match {
    case ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      val opsVal = traverse(ops0)(visitOp(_, taenv, root))
      mapN(opsVal) {
        case ops => KindedAst.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs kinding on the all the definitions in the given root.
    */
  private def visitDefs(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit sctx: SharedContext, flix: Flix): Validation[Map[Symbol.DefnSym, KindedAst.Def], KindError] = {
    val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)

    val result = ParOps.parTraverseValues(staleDefs)(visitDef(_, Nil, KindEnv.empty, taenv, root))
    mapN(result)(freshDefs ++ _)
  }

  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Declaration.Def, extraTconstrs: List[Ast.TraitConstraint], kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Declaration.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val henv = None
          val specVal = visitSpec(spec0, Nil, extraTconstrs, kenv, taenv, root)
          val expVal = visitExp(exp0, kenv, taenv, henv, root)(Scope.Top, sctx, flix)
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Def(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Declaration.Sig, traitTparam: KindedAst.TypeParam, traitConstraints: List[Ast.TraitConstraint], kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Declaration.Sig(sym, spec0, exp0) =>
      val kenvVal = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      flatMapN(kenvVal) {
        kenv =>
          val henv = None
          val specVal = visitSpec(spec0, List(traitTparam.sym), traitConstraints, kenv, taenv, root)
          val expVal = traverseOpt(exp0)(visitExp(_, kenv, taenv, henv, root)(Scope.Top, sctx, flix))
          mapN(specVal, expVal) {
            case (spec, exp) => KindedAst.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs kinding on the given effect operation under the given kind environment.
    */
  private def visitOp(op: ResolvedAst.Declaration.Op, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Op, KindError] = op match {
    case ResolvedAst.Declaration.Op(sym, spec0) =>
      val kenvVal = inferSpec(spec0, KindEnv.empty, taenv, root)
      flatMapN(kenvVal) {
        case kenv =>
          val specVal = visitSpec(spec0, Nil, Nil, kenv, taenv, root)
          mapN(specVal) {
            case spec => KindedAst.Op(sym, spec)
          }
      }
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    *
    * Adds `quantifiers` to the generated scheme's quantifier list.
    */
  private def visitSpec(spec0: ResolvedAst.Spec, quantifiers: List[Symbol.KindedTypeVarSym], extraTconstrs: List[Ast.TraitConstraint], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparamsVal = traverse(tparams0)(visitTypeParam(_, kenv))
      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, taenv, root))
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val effVal = visitEffectDefaultPure(eff0, kenv, taenv, root)
      val tconstrsVal = traverse(tconstrs0)(visitTraitConstraint(_, kenv, taenv, root))
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint(_, kenv, taenv, root))

      mapN(tparamsVal, fparamsVal, tpeVal, effVal, tconstrsVal, econstrsVal) {
        case (tparams, fparams, tpe, eff, tconstrs, econstrs) =>
          val allQuantifiers = quantifiers ::: tparams.map(_.sym)
          val base = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), eff, tpe, tpe.loc)
          val sc = Scheme(allQuantifiers, tconstrs, econstrs.map(EqualityEnvironment.broaden), base)
          KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc)
      }
  }

  /**
    * Performs kinding on the given associated type signature under the given kind environment.
    */
  private def visitAssocTypeSig(s0: ResolvedAst.Declaration.AssocTypeSig, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.AssocTypeSig, KindError] = s0 match {
    case ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam0, kind, tpe0, loc) =>
      val tparamVal = visitTypeParam(tparam0, kenv)
      val tpeVal = traverseOpt(tpe0)(visitType(_, kind, kenv, taenv, root))

      mapN(tparamVal, tpeVal) {
        case (tparam, tpe) => KindedAst.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given associated type definition under the given kind environment.
    */
  private def visitAssocTypeDef(d0: ResolvedAst.Declaration.AssocTypeDef, trtKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.AssocTypeDef, KindError] = d0 match {
    case ResolvedAst.Declaration.AssocTypeDef(doc, mod, symUse, arg0, tpe0, loc) =>
      val trt = root.traits(symUse.sym.trt)
      val assocSig = trt.assocs.find(assoc => assoc.sym == symUse.sym).get
      val tpeKind = assocSig.kind
      val argVal = visitType(arg0, trtKind, kenv, taenv, root)
      val tpeVal = visitType(tpe0, tpeKind, kenv, taenv, root)

      mapN(argVal, tpeVal) {
        case (args, tpe) => KindedAst.AssocTypeDef(doc, mod, symUse, args, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExp(exp00: ResolvedAst.Expr, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv0: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.Expr, KindError] = exp00 match {

    case ResolvedAst.Expr.Var(sym, loc) => Validation.success(KindedAst.Expr.Var(sym, loc))

    case ResolvedAst.Expr.Def(sym, loc) => Validation.success(KindedAst.Expr.Def(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc))

    case ResolvedAst.Expr.Sig(sym, loc) => Validation.success(KindedAst.Expr.Sig(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc))

    case ResolvedAst.Expr.Hole(sym, loc) => Validation.success(KindedAst.Expr.Hole(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc))

    case ResolvedAst.Expr.HoleWithExp(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.HoleWithExp(exp, tvar, evar, loc)
      }

    case ResolvedAst.Expr.OpenAs(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          KindedAst.Expr.OpenAs(sym, exp, tvar, loc)
      }

    case ResolvedAst.Expr.Use(sym, alias, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expr.Use(sym, alias, exp, loc)
      }

    case ResolvedAst.Expr.Cst(cst, loc) => Validation.success(KindedAst.Expr.Cst(cst, loc))

    case ResolvedAst.Expr.Apply(exp0, exps0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expVal, expsVal) {
        case (exp, exps) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Apply(exp, exps, tvar, evar, loc)
      }

    case ResolvedAst.Expr.Lambda(fparam0, exp0, loc) =>
      val fparamVal = visitFormalParam(fparam0, kenv0, taenv, root)
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(fparamVal, expVal) {
        case (fparam, exp) => KindedAst.Expr.Lambda(fparam, exp, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.Unary(sop, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.Unary(sop, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.Binary(sop, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.Binary(sop, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.IfThenElse(exp10, exp20, exp30, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      val exp3Val = visitExp(exp30, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val, exp3Val) {
        case (exp1, exp2, exp3) => KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc)
      }

    case ResolvedAst.Expr.Stm(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.Stm(exp1, exp2, loc)
      }

    case ResolvedAst.Expr.Discard(exp, loc) =>
      mapN(visitExp(exp, kenv0, taenv, henv0, root)) {
        case e => KindedAst.Expr.Discard(e, loc)
      }

    case ResolvedAst.Expr.Let(sym, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.Let(sym, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expr.LetRec(sym, ann, mod, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.LetRec(sym, ann, mod, exp1, exp2, loc)
      }

    case ResolvedAst.Expr.Region(tpe, loc) =>
      Validation.success(KindedAst.Expr.Region(tpe, loc))

    case ResolvedAst.Expr.Scope(sym, regionVar, exp0, loc) =>
      val rv = Type.Var(regionVar.withKind(Kind.Eff), loc)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      flatMapN(kenv0 + (regionVar -> Kind.Eff)) {
        case kenv =>
          // Record that we enter the new scope.
          val newScope = scope.enter(rv.sym)
          val expVal = visitExp(exp0, kenv, taenv, henv0, root)(newScope, sctx, flix)
          mapN(expVal) {
            exp => KindedAst.Expr.Scope(sym, rv, exp, evar, loc)
          }
      }

    case ResolvedAst.Expr.Match(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitMatchRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expr.Match(exp, rules, loc)
      }

    case ResolvedAst.Expr.TypeMatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitTypeMatchRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expr.TypeMatch(exp, rules, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.RestrictableChoose(star, exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitRestrictableChooseRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expr.RestrictableChoose(star, exp, rules, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.Tag(sym, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.Tag(sym, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.RestrictableTag(sym, exp0, isOpen, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.RestrictableTag(sym, exp, isOpen, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(elmsVal) {
        elms => KindedAst.Expr.Tuple(elms, loc)
      }

    case ResolvedAst.Expr.RecordEmpty(loc) => Validation.success(KindedAst.Expr.RecordEmpty(loc))

    case ResolvedAst.Expr.RecordSelect(exp0, label, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.RecordSelect(exp, label, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.RecordExtend(label, value0, rest0, loc) =>
      val valueVal = visitExp(value0, kenv0, taenv, henv0, root)
      val restVal = visitExp(rest0, kenv0, taenv, henv0, root)
      mapN(valueVal, restVal) {
        case (value, rest) => KindedAst.Expr.RecordExtend(label, value, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.RecordRestrict(label, rest0, loc) =>
      val restVal = visitExp(rest0, kenv0, taenv, henv0, root)
      mapN(restVal) {
        rest => KindedAst.Expr.RecordRestrict(label, rest, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.ArrayLit(exps, exp, loc) =>
      val esVal = traverse(exps)(visitExp(_, kenv0, taenv, henv0, root))
      val eVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(esVal, eVal) {
        case (es, e) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.ArrayLit(es, e, tvar, evar, loc)
      }

    case ResolvedAst.Expr.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      val e3Val = visitExp(exp3, kenv0, taenv, henv0, root)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.ArrayNew(e1, e2, e3, tvar, evar, loc)
      }

    case ResolvedAst.Expr.ArrayLoad(base0, index0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, taenv, henv0, root)
      mapN(baseVal, indexVal) {
        case (base, index) =>
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.ArrayLoad(base, index, Type.freshVar(Kind.Star, loc.asSynthetic), evar, loc)
      }

    case ResolvedAst.Expr.ArrayStore(base0, index0, elm0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      val indexVal = visitExp(index0, kenv0, taenv, henv0, root)
      val elmVal = visitExp(elm0, kenv0, taenv, henv0, root)
      mapN(baseVal, indexVal, elmVal) {
        case (base, index, elm) =>
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.ArrayStore(base, index, elm, evar, loc)
      }

    case ResolvedAst.Expr.ArrayLength(base0, loc) =>
      val baseVal = visitExp(base0, kenv0, taenv, henv0, root)
      mapN(baseVal) {
        base =>
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.ArrayLength(base, evar, loc)
      }

    case ResolvedAst.Expr.StructNew(sym, fields, region, loc) =>
      val fieldsVal = traverse(fields) {
        case (field, exp) => mapN(visitExp(exp, kenv0, taenv, henv0, root)) {
          case e => (field, e)
        }
      }
      val regionVal = visitExp(region, kenv0, taenv, henv0, root)
      mapN(fieldsVal, regionVal) {
        case (fs, r) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.StructNew(sym, fs, r, tvar, evar, loc)
      }

    case ResolvedAst.Expr.StructGet(e, field, loc) =>
      val expVal = visitExp(e, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(Symbol.mkSigSym(Symbol.mkTraitSym(Deriver.structFieldGetTraitName(field.name)),
              Name.Ident(Deriver.GetMethodName, loc)), Type.freshVar(Kind.Star, loc), loc),
            List(exp), tvar, evar, loc
          )
      }

    case ResolvedAst.Expr.StructPut(e1, field, e2, loc) =>
      val exp1Val = visitExp(e1, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(e2, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(Symbol.mkSigSym(Symbol.mkTraitSym(Deriver.structFieldPutTraitName(field.name)),
              Name.Ident(Deriver.PutMethodName, loc)), Type.freshVar(Kind.Star, loc), loc),
            List(exp1, exp2), tvar, evar, loc
          )
      }

    case ResolvedAst.Expr.VectorLit(exps, loc) =>
      val expsVal = traverse(exps)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expsVal) {
        case es =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.VectorLit(es, tvar, evar, loc)
      }

    case ResolvedAst.Expr.VectorLoad(exp1, exp2, loc) =>
      val exp1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (e1, e2) =>
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.VectorLoad(e1, e2, Type.freshVar(Kind.Star, loc.asSynthetic), evar, loc)
      }

    case ResolvedAst.Expr.VectorLength(exp, loc) =>
      val expVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(expVal) {
        e => KindedAst.Expr.VectorLength(e, loc)
      }

    case ResolvedAst.Expr.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val expectedTypeVal = traverseOpt(expectedType0)(visitType(_, Kind.Star, kenv0, taenv, root))
      val expectedEffVal = traverseOpt(expectedEff0)(visitType(_, Kind.Eff, kenv0, taenv, root))
      mapN(expVal, expectedTypeVal, expectedEffVal) {
        case (exp, expectedType, expectedEff) =>
          KindedAst.Expr.Ascribe(exp, expectedType, expectedEff, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.InstanceOf(exp0, clazz, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.InstanceOf(exp, clazz, loc)
      }

    case ResolvedAst.Expr.CheckedCast(cast, exp, loc) =>
      mapN(visitExp(exp, kenv0, taenv, henv0, root)) {
        case e =>
          val tvar = Type.freshVar(Kind.Star, loc)
          val evar = Type.freshVar(Kind.Eff, loc)
          KindedAst.Expr.CheckedCast(cast, e, tvar, evar, loc)
      }

    case ResolvedAst.Expr.UncheckedCast(exp0, declaredType0, declaredEff0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val declaredTypeVal = traverseOpt(declaredType0)(visitType(_, Kind.Star, kenv0, taenv, root))
      val declaredeffVal = traverseOpt(declaredEff0)(visitType(_, Kind.Eff, kenv0, taenv, root))
      mapN(expVal, declaredTypeVal, declaredeffVal) {
        case (exp, declaredType, declaredEff) =>
          KindedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.UncheckedMaskingCast(exp, loc) =>
      val eVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(eVal) {
        case e => KindedAst.Expr.UncheckedMaskingCast(e, loc)
      }

    case ResolvedAst.Expr.Without(exp0, eff, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expr.Without(exp, eff, loc)
      }

    case ResolvedAst.Expr.TryCatch(exp0, rules0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val rulesVal = traverse(rules0)(visitCatchRule(_, kenv0, taenv, henv0, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expr.TryCatch(exp, rules, loc)
      }

    case ResolvedAst.Expr.Throw(exp0, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc)
      val evar = Type.freshVar(Kind.Eff, loc)
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp => KindedAst.Expr.Throw(exp, tvar, evar, loc)
      }

    case ResolvedAst.Expr.TryWith(exp0, eff, rules0, loc) =>
      // create a fresh type variable for the handling block (same as resume result)
      // and for the operation result (same as resume argument)
      // and set the handled env
      val tvar = Type.freshVar(Kind.Star, loc)

      // use the old henv for the handled expression
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)

      // use the new henv for the handler
      val rulesVal = traverse(rules0)(visitHandlerRule(_, kenv0, taenv, tvar, root))
      mapN(expVal, rulesVal) {
        case (exp, rules) => KindedAst.Expr.TryWith(exp, eff, rules, tvar, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.Do(op, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      val tvar = Type.freshVar(Kind.Star, loc)
      mapN(argsVal) {
        case args => KindedAst.Expr.Do(op, args, tvar, loc)
      }

    case ResolvedAst.Expr.InvokeConstructor2(clazz, exps0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expsVal) {
        exps =>
          val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.InvokeConstructor2(clazz, exps, jvar, evar, loc)
      }

    case ResolvedAst.Expr.InvokeMethod2(exp0, methodName, exps0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expVal, expsVal) {
        case (exp, exps) =>
          val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.InvokeMethod2(exp, methodName, exps, jvar, tvar, evar, loc)
      }

    case ResolvedAst.Expr.InvokeStaticMethod2(clazz, methodName, exps0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expsVal) {
        exps =>
          val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.InvokeStaticMethod2(clazz, methodName, exps, jvar, tvar, evar, loc)
      }

    case ResolvedAst.Expr.GetField2(exp0, fieldName, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        case exp =>
          val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.GetField2(exp, fieldName, jvar, tvar, evar, loc)
      }

    case ResolvedAst.Expr.InvokeConstructorOld(constructor, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expr.InvokeConstructorOld(constructor, args, loc)
      }

    case ResolvedAst.Expr.InvokeMethodOld(method, clazz, exp0, args0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(expVal, argsVal) {
        case (exp, args) => KindedAst.Expr.InvokeMethodOld(method, clazz, exp, args, loc)
      }

    case ResolvedAst.Expr.InvokeStaticMethodOld(method, args0, loc) =>
      val argsVal = traverse(args0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(argsVal) {
        args => KindedAst.Expr.InvokeStaticMethodOld(method, args, loc)
      }

    case ResolvedAst.Expr.GetFieldOld(field, clazz, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.GetFieldOld(field, clazz, exp, loc)
      }

    case ResolvedAst.Expr.PutField(field, clazz, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.PutField(field, clazz, exp1, exp2, loc)
      }

    case ResolvedAst.Expr.GetStaticField(field, loc) => Validation.success(KindedAst.Expr.GetStaticField(field, loc))

    case ResolvedAst.Expr.PutStaticField(field, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.PutStaticField(field, exp, loc)
      }

    case ResolvedAst.Expr.NewObject(name, clazz, methods, loc) =>
      mapN(traverse(methods)(visitJvmMethod(_, kenv0, taenv, henv0, root))) {
        methods => KindedAst.Expr.NewObject(name, clazz, methods, loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.NewChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.NewChannel(exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.GetChannel(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.GetChannel(exp, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.PutChannel(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.PutChannel(exp1, exp2, Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.SelectChannel(rules0, default0, loc) =>
      val rulesVal = traverse(rules0)(visitSelectChannelRule(_, kenv0, taenv, henv0, root))
      val defaultVal = traverseOpt(default0)(visitExp(_, kenv0, taenv, henv0, root))
      mapN(rulesVal, defaultVal) {
        case (rules, default) => KindedAst.Expr.SelectChannel(rules, default, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1Val = visitExp(exp1, kenv0, taenv, henv0, root)
      val e2Val = visitExp(exp2, kenv0, taenv, henv0, root)
      mapN(e1Val, e2Val) {
        case (e1, e2) => KindedAst.Expr.Spawn(e1, e2, loc)
      }

    case ResolvedAst.Expr.ParYield(frags, exp0, loc) =>
      val fragsVal = traverse(frags) {
        case ResolvedAst.ParYieldFragment(pat, exp1, l0) =>
          val patVal = visitPattern(pat, kenv0, root)
          val expVal = visitExp(exp1, kenv0, taenv, henv0, root)
          mapN(patVal, expVal) {
            case (p, e) => KindedAst.ParYieldFragment(p, e, l0)
          }
      }

      mapN(fragsVal, visitExp(exp0, kenv0, taenv, henv0, root)) {
        case (fs, exp) => KindedAst.Expr.ParYield(fs, exp, loc)
      }

    case ResolvedAst.Expr.Lazy(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.Lazy(exp, loc)
      }

    case ResolvedAst.Expr.Force(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.Force(exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.FixpointConstraintSet(cs0, loc) =>
      val csVal = traverse(cs0)(visitConstraint(_, kenv0, taenv, henv0, root))
      mapN(csVal) {
        cs => KindedAst.Expr.FixpointConstraintSet(cs, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(visitPredicateParam(_, kenv0, taenv, root))
      val expVal = visitExp(exp, kenv0, taenv, henv0, root)
      mapN(psVal, expVal) {
        case (ps, e) => KindedAst.Expr.FixpointLambda(ps, e, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }.recoverOne {
        case err: KindError =>
          val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
          val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
          KindedAst.Expr.Error(err, tvar, evar)
      }

    case ResolvedAst.Expr.FixpointMerge(exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.FixpointMerge(exp1, exp2, loc)
      }

    case ResolvedAst.Expr.FixpointSolve(exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.FixpointSolve(exp, loc)
      }

    case ResolvedAst.Expr.FixpointFilter(pred, exp0, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.FixpointFilter(pred, exp, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.FixpointInject(exp0, pred, loc) =>
      val expVal = visitExp(exp0, kenv0, taenv, henv0, root)
      mapN(expVal) {
        exp => KindedAst.Expr.FixpointInject(exp, pred, Type.freshVar(Kind.Star, loc.asSynthetic), Type.freshVar(Kind.Eff, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.FixpointProject(pred, exp10, exp20, loc) =>
      val exp1Val = visitExp(exp10, kenv0, taenv, henv0, root)
      val exp2Val = visitExp(exp20, kenv0, taenv, henv0, root)
      mapN(exp1Val, exp2Val) {
        case (exp1, exp2) => KindedAst.Expr.FixpointProject(pred, exp1, exp2, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Expr.Error(m) =>
      val tvar = Type.freshVar(Kind.Star, m.loc)
      val evar = Type.freshVar(Kind.Eff, m.loc)
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.success(KindedAst.Expr.Error(m, tvar, evar))
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      val patVal = visitPattern(pat0, kenv, root)
      val guardVal = traverseOpt(guard0)(visitExp(_, kenv, taenv, henv, root))
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(patVal, guardVal, expVal) {
        case (pat, guard, exp) => KindedAst.MatchRule(pat, guard, exp)
      }
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitTypeMatchRule(rule0: ResolvedAst.TypeMatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.TypeMatchRule, KindError] = rule0 match {
    case ResolvedAst.TypeMatchRule(sym, tpe0, exp0) =>
      val tpeVal = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(tpeVal, expVal) {
        case (tpe, exp) => KindedAst.TypeMatchRule(sym, tpe, exp)
      }
  }

  /**
    * Performs kinding on the given relational choice rule under the given kind environment.
    */
  private def visitRestrictableChooseRule(rule0: ResolvedAst.RestrictableChooseRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.RestrictableChooseRule, KindError] = rule0 match {
    case ResolvedAst.RestrictableChooseRule(pat0, exp0) =>
      val patVal = visitRestrictableChoosePattern(pat0)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(patVal, expVal) {
        case (pat, exp) => KindedAst.RestrictableChooseRule(pat, exp)
      }
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.CatchRule(sym, clazz, exp)
      }
  }

  /**
    * Performs kinding on the given handler rule under the given kind environment.
    */
  private def visitHandlerRule(rule0: ResolvedAst.HandlerRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], hTvar: Type.Var, root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.HandlerRule, KindError] = rule0 match {
    case ResolvedAst.HandlerRule(op, fparams0, exp0) =>
      // create a new type variable for the op return type (same as resume argument type)
      val tvar = Type.freshVar(Kind.Star, exp0.loc)

      val henv = Some((tvar, hTvar))

      val fparamsVal = traverse(fparams0)(visitFormalParam(_, kenv, taenv, root))
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(fparamsVal, expVal) {
        case (fparams, exp) => KindedAst.HandlerRule(op, fparams, exp, tvar)
      }
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      val chanVal = visitExp(chan0, kenv, taenv, henv, root)
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(chanVal, expVal) {
        case (chan, exp) => KindedAst.SelectChannelRule(sym, chan, exp)
      }
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def visitPattern(pat00: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit scope: Scope, flix: Flix): Validation[KindedAst.Pattern, KindError] = pat00 match {
    case ResolvedAst.Pattern.Wild(loc) => Validation.success(KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc.asSynthetic), loc))
    case ResolvedAst.Pattern.Var(sym, loc) => Validation.success(KindedAst.Pattern.Var(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc))
    case ResolvedAst.Pattern.Cst(cst, loc) => Validation.success(KindedAst.Pattern.Cst(cst, loc))
    case ResolvedAst.Pattern.Tag(sym, pat0, loc) =>
      val patVal = visitPattern(pat0, kenv, root)
      mapN(patVal) {
        pat => KindedAst.Pattern.Tag(sym, pat, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      val elmsVal = traverse(elms0)(visitPattern(_, kenv, root))
      mapN(elmsVal) {
        elms => KindedAst.Pattern.Tuple(elms, loc)
      }
    case ResolvedAst.Pattern.Record(pats, pat, loc) =>
      val psVal = traverse(pats) {
        case ResolvedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
          val tvar = Type.freshVar(Kind.Star, loc1.asSynthetic)
          mapN(visitPattern(pat1, kenv, root)) {
            case p => KindedAst.Pattern.Record.RecordLabelPattern(label, tvar, p, loc1)
          }
      }
      val pVal = visitPattern(pat, kenv, root)
      mapN(psVal, pVal) {
        case (ps, p) => KindedAst.Pattern.Record(ps, p, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }

    case ResolvedAst.Pattern.RecordEmpty(loc) => Validation.success(KindedAst.Pattern.RecordEmpty(loc))

    case ResolvedAst.Pattern.Error(loc) => Validation.success(KindedAst.Pattern.Error(Type.freshVar(Kind.Star, loc.asSynthetic), loc))
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoosePattern(pat00: ResolvedAst.RestrictableChoosePattern)(implicit scope: Scope, flix: Flix): Validation[KindedAst.RestrictableChoosePattern, KindError] = pat00 match {
    case ResolvedAst.RestrictableChoosePattern.Tag(sym, pat0, loc) =>
      val patVal = traverse(pat0)(visitRestrictableChoosePatternVarOrWild)
      mapN(patVal) {
        case pat => KindedAst.RestrictableChoosePattern.Tag(sym, pat, Type.freshVar(Kind.Star, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoosePatternVarOrWild(pat0: ResolvedAst.RestrictableChoosePattern.VarOrWild)(implicit scope: Scope, flix: Flix): Validation[KindedAst.RestrictableChoosePattern.VarOrWild, KindError] = pat0 match {
    case ResolvedAst.RestrictableChoosePattern.Wild(loc) => Validation.success(KindedAst.RestrictableChoosePattern.Wild(Type.freshVar(Kind.Star, loc.asSynthetic), loc))
    case ResolvedAst.RestrictableChoosePattern.Var(sym, loc) => Validation.success(KindedAst.RestrictableChoosePattern.Var(sym, Type.freshVar(Kind.Star, loc.asSynthetic), loc))
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparams = cparams0.map(visitConstraintParam)
      val headVal = visitHeadPredicate(head0, kenv, taenv, henv, root)
      val bodyVal = traverse(body0)(visitBodyPredicate(_, kenv, taenv, henv, root))
      mapN(headVal, bodyVal) {
        case (head, body) => KindedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Performs kinding on the given constraint param under the given kind environment.
    */
  private def visitConstraintParam(cparam0: ResolvedAst.ConstraintParam): KindedAst.ConstraintParam = cparam0 match {
    case ResolvedAst.ConstraintParam(sym, loc) => KindedAst.ConstraintParam(sym, loc)
  }

  /**
    * Performs kinding on the given head predicate under the given kind environment.
    */
  private def visitHeadPredicate(pred0: ResolvedAst.Predicate.Head, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred0 match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      val termsVal = traverse(terms0)(visitExp(_, kenv, taenv, henv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Head.Atom(pred, den, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred0: ResolvedAst.Predicate.Body, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms0, loc) =>
      val termsVal = traverse(terms0)(visitPattern(_, kenv, root))
      mapN(termsVal) {
        terms => KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, Type.freshVar(Kind.Predicate, loc.asSynthetic), loc)
      }

    case ResolvedAst.Predicate.Body.Functional(outVars, exp0, loc) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Functional(outVars, exp, loc)
      }

    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      val expVal = visitExp(exp0, kenv, taenv, henv, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Guard(exp, loc)
      }
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVar(tvar: UnkindedType.Var, expectedKind: Kind, kenv: KindEnv)(implicit sctx: SharedContext): Validation[Type.Var, KindError] = tvar match {
    case UnkindedType.Var(sym0, loc) =>
      val sym = visitTypeVarSym(sym0, expectedKind, kenv, loc)
      Validation.success(Type.Var(sym, loc))
  }

  /**
    * Performs kinding on the given type variable symbol under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVarSym(sym: Symbol.UnkindedTypeVarSym, expectedKind: Kind, kenv: KindEnv, loc: SourceLocation)(implicit sctx: SharedContext): Symbol.KindedTypeVarSym = {
    kenv.map.get(sym) match {
      // Case 1: we don't know about this kind, just ascribe it with what the context expects
      case None => sym.withKind(expectedKind)
      // Case 2: we know about this kind, make sure it's behaving as we expect
      case Some(actualKind) =>
        unify(expectedKind, actualKind) match {
          case Some(kind) => sym.withKind(kind)
          case None =>
            val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc = loc)
            sctx.errors.add(e)
            sym.withKind(Kind.Error)
        }
    }
  }


  /**
    * Performs kinding on the given type under the given kind environment, with `expectedKind` expected from context.
    * This is roughly analogous to the reassembly of expressions under a type environment, except that:
    *   - Kind errors may be discovered here as they may not have been found during inference (or inference may not have happened at all).
    */
  private def visitType(tpe0: UnkindedType, expectedKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => visitTypeVar(tvar, expectedKind, kenv)

    case UnkindedType.Cst(cst, loc) =>
      val kind = cst.kind
      unify(expectedKind, kind) match {
        case Some(_) => Validation.success(Type.Cst(cst, loc))
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.Apply(t10, t20, loc) =>
      val t2Val = visitType(t20, Kind.Wild, kenv, taenv, root)
      flatMapN(t2Val) {
        t2 =>
          val k1 = Kind.Arrow(t2.kind, expectedKind)
          val t1Val = visitType(t10, k1, kenv, taenv, root)
          mapN(t1Val) {
            t1 => mkApply(t1, t2, loc)
          }
      }

    case UnkindedType.Ascribe(t, k, loc) =>
      unify(k, expectedKind) match {
        case Some(kind) => visitType(t, kind, kenv, taenv, root)
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.Alias(cst, args0, t0, loc) =>
      taenv(cst.sym) match {
        case KindedAst.TypeAlias(_, _, _, _, tparams, tpe, _) =>
          val argsVal = traverse(tparams.zip(args0)) {
            case (tparam, arg) => visitType(arg, tparam.sym.kind, kenv, taenv, root)
          }
          val tpeVal = visitType(t0, tpe.kind, kenv, taenv, root)
          flatMapN(argsVal, tpeVal) {
            case (args, t) => unify(t.kind, expectedKind) match {
              case Some(_) => Validation.success(Type.Alias(cst, args, t, loc))
              case None =>
                val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc)
                Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
            }
          }
      }

    case UnkindedType.AssocType(cst, arg0, loc) =>
      val trt = root.traits(cst.sym.trt)
      // TODO ASSOC-TYPES maybe have dedicated field in root for assoc types
      trt.assocs.find(_.sym == cst.sym).get match {
        case ResolvedAst.Declaration.AssocTypeSig(_, _, _, _, k0, _, _) =>
          // TODO ASSOC-TYPES for now assuming just one type parameter
          // check that the assoc type kind matches the expected
          unify(k0, expectedKind) match {
            case Some(kind) =>
              val innerExpectedKind = getTraitKind(trt)
              val argVal = visitType(arg0, innerExpectedKind, kenv, taenv, root)
              mapN(argVal) {
                case arg => Type.AssocType(cst, arg, kind, loc)
              }
            case None =>
              val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k0, loc)
              Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
          }
      }

    case UnkindedType.Arrow(eff0, arity, loc) =>
      val kind = Kind.mkArrow(arity)
      unify(kind, expectedKind) match {
        case Some(_) =>
          val effVal = visitEffectDefaultPure(eff0, kenv, taenv, root)
          mapN(effVal) {
            case eff => Type.mkApply(Type.Cst(TypeConstructor.Arrow(arity), loc), List(eff), loc)
          }
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.Enum(sym, loc) =>
      val kind = getEnumKind(root.enums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Validation.success(Type.Cst(TypeConstructor.Enum(sym, k), loc))
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.Struct(sym, loc) =>
      val kind = getStructKind(root.structs(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Validation.success(Type.Cst(TypeConstructor.Struct(sym, k), loc))
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.RestrictableEnum(sym, loc) =>
      val kind = getRestrictableEnumKind(root.restrictableEnums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Validation.success(Type.Cst(TypeConstructor.RestrictableEnum(sym, k), loc))
        case None =>
          val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc)
          Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
      }

    case UnkindedType.CaseSet(cases, loc) =>
      // Infer the kind from the cases.
      val actualKindVal: Validation[Kind, KindError] = fold(cases, Kind.WildCaseSet: Kind) {
        case (kindAcc, sym) =>
          val symKind = Kind.CaseSet(sym.enumSym)
          unify(kindAcc, symKind) match {
            // Case 1: The kinds unify. Update the kind.
            case Some(k) => Validation.success(k)
            // Case 2: The kinds do not unify. Error.
            case None =>
              val e = KindError.MismatchedKinds(kindAcc, symKind, loc)
              Validation.toSoftFailure(Kind.Error, e)
          }
      }

      // Check against the expected kind.
      flatMapN(actualKindVal) {
        case actualKind =>
          unify(actualKind, expectedKind) match {
            // Case 1:  We have an explicit case kind.
            case Some(Kind.CaseSet(sym)) => Validation.success(Type.Cst(TypeConstructor.CaseSet(cases.to(SortedSet), sym), loc))
            // Case 2: We have a generic case kind. Error.
            case Some(Kind.WildCaseSet) => Validation.toSoftFailure(Type.freshError(Kind.Error, loc), KindError.UninferrableKind(loc))
            // Case 3: Unexpected kind. Error.
            case None =>
              val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc)
              Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)

            case Some(k) if Kind.hasError(k) =>
              val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc)
              Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)

            case Some(_) => throw InternalCompilerException("unexpected non-case set kind", loc)
          }
      }


    case UnkindedType.CaseComplement(t0, loc) =>
      val tVal = visitType(t0, Kind.WildCaseSet, kenv, taenv, root)
      flatMapN(tVal) {
        t =>
          unify(t.kind, expectedKind) match {
            case Some(Kind.CaseSet(enumSym)) => Validation.success(Type.mkCaseComplement(t, enumSym, loc))
            case None =>
              val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc)
              Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
            case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
          }
      }

    case UnkindedType.CaseUnion(t1, t2, loc) =>
      // Get the component types.
      val t1Val = visitType(t1, Kind.WildCaseSet, kenv, taenv, root)
      val t2Val = visitType(t2, Kind.WildCaseSet, kenv, taenv, root)

      flatMapN(t1Val, t2Val) {
        case (t1, t2) =>
          val actualKindVal: Validation[Kind, KindError] = unify(t1.kind, t2.kind) match {
            // Case 1: The kinds unify.
            case Some(k) => Validation.success(k)
            // Case 2: The kinds do not unify. Error.
            case None =>
              val e = KindError.MismatchedKinds(t1.kind, t2.kind, loc)
              Validation.toSoftFailure(Kind.Error, e)
          }

          flatMapN(actualKindVal) {
            case actualKind =>
              unify(actualKind, expectedKind) match {
                case Some(Kind.CaseSet(enumSym)) => Validation.success(Type.mkCaseUnion(t1, t2, enumSym, loc))
                case None =>
                  val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc)
                  Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
                case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
              }
          }
      }

    case UnkindedType.CaseIntersection(t1, t2, loc) =>
      // Get the component types.
      val t1Val = visitType(t1, Kind.WildCaseSet, kenv, taenv, root)
      val t2Val = visitType(t2, Kind.WildCaseSet, kenv, taenv, root)

      flatMapN(t1Val, t2Val) {
        case (t1, t2) =>
          val actualKindVal: Validation[Kind, KindError] = unify(t1.kind, t2.kind) match {
            // Case 1: The kinds unify.
            case Some(k) => Validation.success(k)
            // Case 2: The kinds do not unify. Error.
            case None =>
              val e = KindError.MismatchedKinds(t1.kind, t2.kind, loc)
              Validation.toSoftFailure(Kind.Error, e)
          }

          flatMapN(actualKindVal) {
            case actualKind =>
              unify(actualKind, expectedKind) match {
                case Some(Kind.CaseSet(enumSym)) => Validation.success(Type.mkCaseIntersection(t1, t2, enumSym, loc))
                case None =>
                  val e = KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc)
                  Validation.toSoftFailure(Type.freshError(Kind.Error, loc), e)
                case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
              }
          }
      }


    case UnkindedType.Error(loc) => Validation.success(Type.freshError(expectedKind, loc))

    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe0.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe0.loc)


  }

  /**
    * Performs kinding on the given effect, assuming it to be Pure if it is absent.
    */
  private def visitEffectDefaultPure(tpe: Option[UnkindedType], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Type, KindError] = tpe match {
    case None => Validation.success(Type.mkPure(SourceLocation.Unknown))
    case Some(t) => visitType(t, Kind.Eff, kenv, taenv, root)
  }

  /**
    * Performs kinding on the given trait constraint under the given kind environment.
    */
  private def visitTraitConstraint(tconstr: ResolvedAst.TraitConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Ast.TraitConstraint, KindError] = tconstr match {
    case ResolvedAst.TraitConstraint(head, tpe0, loc) =>
      val traitKind = getTraitKind(root.traits(head.sym))
      mapN(visitType(tpe0, traitKind, kenv, taenv, root)) {
        tpe => Ast.TraitConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given equality constraint under the given kind environment.
    */
  private def visitEqualityConstraint(econstr: ResolvedAst.EqualityConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[Ast.EqualityConstraint, KindError] = econstr match {
    case ResolvedAst.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      val t1Val = visitType(tpe1, Kind.Wild, kenv, taenv, root)
      val t2Val = visitType(tpe2, Kind.Wild, kenv, taenv, root)
      mapN(t1Val, t2Val) {
        case (t1, t2) => Ast.EqualityConstraint(cst, t1, t2, loc)
      }
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv)(implicit sctx: SharedContext): Validation[KindedAst.TypeParam, KindError] = {
    val (name, sym0, loc) = tparam match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
      case ResolvedAst.TypeParam.Implicit(iName, iSym, iLoc) => (iName, iSym, iLoc)
    }
    val sym = visitTypeVarSym(sym0, Kind.Wild, kenv, loc)
    Validation.success(KindedAst.TypeParam(name, sym, loc))
  }

  /**
    * Performs kinding on the given index parameter of the given enum sym under the given kind environment.
    */
  private def visitIndex(index: ResolvedAst.TypeParam, `enum`: Symbol.RestrictableEnumSym, kenv: KindEnv)(implicit sctx: SharedContext): Validation[KindedAst.TypeParam, KindError] = {
    val (name, sym0, loc) = index match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
      case ResolvedAst.TypeParam.Implicit(iName, iSym, iLoc) => (iName, iSym, iLoc)
    }

    val sym = visitTypeVarSym(sym0, Kind.CaseSet(`enum`), kenv, loc)
    Validation.success(KindedAst.TypeParam(name, sym, loc))
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      val (tpeVal, src) = tpe0 match {
        case None => (Validation.success(sym.tvar), Ast.TypeSource.Inferred)
        case Some(tpe) => (visitType(tpe, Kind.Star, kenv, taenv, root), Ast.TypeSource.Ascribed)
      }
      mapN(tpeVal) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, src, loc)
      }
  }

  /**
    * Performs kinding on the given predicate param under the given kind environment.
    */
  private def visitPredicateParam(pparam0: ResolvedAst.PredicateParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): Validation[KindedAst.PredicateParam, KindError] = pparam0 match {
    case ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      val tpe = Type.freshVar(Kind.Predicate, loc)
      Validation.success(KindedAst.PredicateParam(pred, tpe, loc))

    case ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, Kind.Star, kenv, taenv, root))) {
        case ts =>
          val tpe = den match {
            case Denotation.Relational => Type.mkRelation(ts, pred.loc.asSynthetic)
            case Denotation.Latticenal => Type.mkLattice(ts, pred.loc.asSynthetic)
          }
          KindedAst.PredicateParam(pred, tpe, loc)
      }

  }

  /**
    * Performs kinding on the given JVM method.
    */
  private def visitJvmMethod(method: ResolvedAst.JvmMethod, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], henv: Option[(Type.Var, Type.Var)], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix) = method match {
    case ResolvedAst.JvmMethod(_, fparams, exp, tpe0, eff0, loc) =>
      val fparamsVal = traverse(fparams)(visitFormalParam(_, kenv, taenv, root))
      val expVal = visitExp(exp, kenv, taenv, henv, root)
      val effVal = visitEffectDefaultPure(eff0, kenv, taenv, root)
      val tpeVal = visitType(tpe0, Kind.Wild, kenv, taenv, root)
      mapN(fparamsVal, expVal, tpeVal, effVal) {
        case (f, e, tpe, eff) => KindedAst.JvmMethod(method.ident, f, e, tpe, eff, loc)
      }
  }

  /**
    * Infers a kind environment from the given spec.
    * A KindEnvironment is provided in case some subset of of kinds have been declared (and therefore should not be inferred),
    * as in the case of a trait type parameter used in a sig or law.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, eff0, tconstrs, econstrs, _) =>
      val fparamKenvsVal = traverse(fparams)(inferFormalParam(_, kenv, taenv, root))
      val tpeKenvVal = inferType(tpe, Kind.Star, kenv, taenv, root)
      val effKenvsVal = traverse(eff0)(inferType(_, Kind.Eff, kenv, taenv, root))
      val tconstrsKenvsVal = traverse(tconstrs)(inferTraitConstraint(_, kenv, taenv, root))
      val econstrsKenvsVal = traverse(econstrs)(inferEqualityConstraint(_, kenv, taenv, root))

      flatMapN(fparamKenvsVal, tpeKenvVal, effKenvsVal, tconstrsKenvsVal, econstrsKenvsVal) {
        case (fparamKenvs, tpeKenv, effKenvs, tconstrKenvs, econstrsKenvs) =>
          KindEnv.merge(fparamKenvs ::: tpeKenv :: effKenvs ::: tconstrKenvs ::: econstrsKenvs)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => tpe0 match {
      case None => Validation.success(KindEnv.empty)
      case Some(tpe) => inferType(tpe, Kind.Star, kenv, taenv, root)
    }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTraitConstraint(tconstr: ResolvedAst.TraitConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tconstr match {
    case ResolvedAst.TraitConstraint(head, tpe, _) =>
      val kind = getTraitKind(root.traits(head.sym))
      inferType(tpe, kind, kenv: KindEnv, taenv, root)
  }

  /**
    * Infers a kind environment from the given equality constraint.
    */
  private def inferEqualityConstraint(econstr: ResolvedAst.EqualityConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = econstr match {
    case ResolvedAst.EqualityConstraint(Ast.AssocTypeConstructor(sym, _), tpe1, tpe2, _) =>
      val trt = root.traits(sym.trt)
      val kind1 = getTraitKind(trt)
      val kind2 = trt.assocs.find(_.sym == sym).get.kind
      val kenv1Val = inferType(tpe1, kind1, kenv, taenv, root)
      val kenv2Val = inferType(tpe2, kind2, kenv, taenv, root)
      flatMapN(kenv1Val, kenv2Val) {
        case (kenv1, kenv2) => kenv1 ++ kenv2
      }
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    * The inference is roughly analogous to the inference of types for expressions.
    * The primary differences are:
    *   - There are no kind variables; kinds that cannot be determined are instead marked with [[Kind.Wild]].
    *   - Subkinding may allow a variable to be ascribed with two different kinds; the most specific is used in the returned environment.
    */
  private def inferType(tpe: UnkindedType, expectedKind: Kind, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: UnkindedType.Var =>
      val tyconKind = kenv0.map.get(tvar.sym) match {
        // Case 1.1: the type is not in the kenv: guess that it is Star -> Star -> ... -> ???.
        case None =>
          tpe.typeArguments.foldLeft(expectedKind) {
            case (acc, _) => Kind.Star ->: acc
          }
        // Case 1.2: the type is in the kenv: use it.
        case Some(k) => k
      }

      val args = Kind.kindArgs(tyconKind)
      fold(tpe.typeArguments.zip(args), KindEnv.singleton(tvar.sym -> tyconKind)) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Cst(cst, loc) =>
      val args = Kind.kindArgs(cst.kind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Ascribe(t, k, _) => inferType(t, k, kenv0, taenv, root)

    case UnkindedType.Alias(cst, args, _, _) =>
      val alias = taenv(cst.sym)
      val tparamKinds = alias.tparams.map(_.sym.kind)

      fold(args.zip(tparamKinds), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.AssocType(cst, arg, _) =>
      val trt = root.traits(cst.sym.trt)
      val kind = getTraitKind(trt)
      inferType(arg, kind, kenv0, taenv, root)

    case UnkindedType.Arrow(eff, _, _) =>
      val effKenvsVal = traverse(eff)(inferType(_, Kind.Eff, kenv0, taenv, root))
      val argKenvVal = fold(tpe.typeArguments, KindEnv.empty) {
        case (acc, targ) => flatMapN(inferType(targ, Kind.Star, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }
      flatMapN(effKenvsVal, argKenvVal) {
        case (effKenvs, argKenv) => KindEnv.merge(effKenvs :+ argKenv)
      }

    case UnkindedType.Enum(sym, _) =>
      val tyconKind = getEnumKind(root.enums(sym))
      val args = Kind.kindArgs(tyconKind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.Struct(sym, _) =>
      val tyconKind = getStructKind(root.structs(sym))
      val args = Kind.kindArgs(tyconKind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.RestrictableEnum(sym, _) =>
      val tyconKind = getRestrictableEnumKind(root.restrictableEnums(sym))
      val args = Kind.kindArgs(tyconKind)

      fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, kind, kenv0, taenv, root)) {
          kenv => acc ++ kenv
        }
      }

    case UnkindedType.CaseSet(_, _) => Validation.success(KindEnv.empty)

    case UnkindedType.CaseComplement(t, _) =>
      // Expected kind for t is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      inferType(t, expected, kenv0, taenv, root)

    case UnkindedType.CaseUnion(t1, t2, _) =>
      // Expected kind for t1 and t2 is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      val kenv1Val = inferType(t1, expected, kenv0, taenv, root)
      val kenv2Val = inferType(t2, expected, kenv0, taenv, root)
      flatMapN(kenv1Val, kenv2Val) {
        case (kenv1, kenv2) => kenv1 ++ kenv2
      }

    case UnkindedType.CaseIntersection(t1, t2, _) =>
      // Expected kind for t1 and t2 is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }

      val kenv1Val = inferType(t1, expected, kenv0, taenv, root)
      val kenv2Val = inferType(t2, expected, kenv0, taenv, root)
      flatMapN(kenv1Val, kenv2Val) {
        case (kenv1, kenv2) => kenv1 ++ kenv2
      }

    case UnkindedType.Error(_) => Validation.success(KindEnv.empty)

    case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", tpe.loc)
    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe.loc)
  }

  /**
    * Gets a kind environment from the type params, defaulting to Star kind if they are unkinded.
    */
  private def getKindEnvFromTypeParams(tparams0: List[ResolvedAst.TypeParam])(implicit flix: Flix): KindEnv = {
    val kenvs = tparams0.map(getKindEnvFromTypeParam)
    KindEnv.disjointMerge(kenvs)
  }

  /**
    * Gets a kind environment from the type param, defaulting to Star kind if it is unkinded.
    */
  private def getKindEnvFromTypeParam(tparam0: ResolvedAst.TypeParam)(implicit flix: Flix): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.Star)
    case ResolvedAst.TypeParam.Implicit(_, _, _) => KindEnv.empty
  }

  /**
    * Gets a kind environment from the type param, defaulting the to kind of the given enum's tags if it is unkinded.
    */
  private def getKindEnvFromIndex(index0: ResolvedAst.TypeParam, sym: Symbol.RestrictableEnumSym)(implicit flix: Flix): KindEnv = index0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.CaseSet(sym))
    case ResolvedAst.TypeParam.Implicit(_, _, _) => KindEnv.empty
  }

  /**
    * Gets a kind environment from the type param, defaulting to `Kind.Eff` if it is unspecified
    */
  private def getKindEnvFromRegion(tparam0: ResolvedAst.TypeParam)(implicit flix: Flix): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.Eff)
    case ResolvedAst.TypeParam.Implicit(_, tvar, _) => KindEnv.singleton(tvar -> Kind.Eff)
  }

  /**
    * Gets a kind environment from the spec.
    */
  private def getKindEnvFromSpec(spec0: ResolvedAst.Spec, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _, _) =>
      // first get the kenv from the declared tparams
      val kenv1 = getKindEnvFromTypeParams(tparams0)

      // merge it from the kenv from the context
      val kenv2Val = kenv0 ++ kenv1

      // Finally do inference on the spec under the new kenv
      flatMapN(kenv2Val) {
        case kenv2 =>
          inferSpec(spec0, kenv2, taenv, root)
      }
  }

  /**
    * Gets the kind of the enum.
    */
  private def getEnumKind(enum0: ResolvedAst.Declaration.Enum)(implicit flix: Flix): Kind = enum0 match {
    case ResolvedAst.Declaration.Enum(_, _, _, _, tparams, _, _, _) =>
      val kenv = getKindEnvFromTypeParams(tparams)
      tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the struct.
    */
  private def getStructKind(struct0: ResolvedAst.Declaration.Struct)(implicit flix: Flix): Kind = struct0 match {
    case ResolvedAst.Declaration.Struct(_, _, _, _, tparams0, _, _) =>
      // tparams default to zero except for the region param
      val kenv1 = getKindEnvFromTypeParams(tparams0.init)
      val kenv2 = getKindEnvFromRegion(tparams0.last)
      // The last add is simply to verify that the last tparam was marked as Eff
      val kenv = KindEnv.disjointAppend(kenv1, kenv2)
      tparams0.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the restrictable enum.
    */
  private def getRestrictableEnumKind(enum0: ResolvedAst.Declaration.RestrictableEnum)(implicit flix: Flix): Kind = enum0 match {
    case ResolvedAst.Declaration.RestrictableEnum(_, _, _, sym, index, tparams, _, _, _) =>
      val kenvIndex = getKindEnvFromIndex(index, sym)
      val kenvTparams = getKindEnvFromTypeParams(tparams)

      val kenv = KindEnv.disjointAppend(kenvIndex, kenvTparams)

      (index :: tparams).foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the trait.
    */
  private def getTraitKind(trt: ResolvedAst.Declaration.Trait): Kind = trt.tparam match {
    case ResolvedAst.TypeParam.Kinded(_, _, kind, _) => kind
    case _: ResolvedAst.TypeParam.Unkinded => Kind.Star
    case ResolvedAst.TypeParam.Implicit(_, _, loc) => throw InternalCompilerException("unexpected implicit type parameter for trait", loc)
  }

  /**
    * Creates the type application `t1[t2]`, while simplifying trivial boolean formulas.
    */
  private def mkApply(t1: Type, t2: Type, loc: SourceLocation): Type = t1 match {
    case Type.Apply(Type.Cst(TypeConstructor.Union, _), arg, _) => Type.mkUnion(arg, t2, loc)
    case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), arg, _) => Type.mkIntersection(arg, t2, loc)
    case Type.Cst(TypeConstructor.Complement, _) => Type.mkComplement(t2, loc)

    case t => Type.Apply(t, t2, loc)
  }

  /**
    * A mapping from type variables to kinds.
    */
  private object KindEnv {
    /**
      * The empty kind environment.
      */
    val empty: KindEnv = KindEnv(Map.empty)

    /**
      * Returns a kind environment consisting of a single mapping.
      */
    def singleton(pair: (Symbol.UnkindedTypeVarSym, Kind)): KindEnv = KindEnv(Map(pair))

    /**
      * Merges all the given kind environments.
      */
    def merge(kenvs: List[KindEnv]): Validation[KindEnv, KindError] = {
      fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
    }

    /**
      * Merges the given kind environments.
      *
      * The environments must be disjoint.
      */
    def disjointAppend(kenv1: KindEnv, kenv2: KindEnv): KindEnv = {
      KindEnv(kenv1.map ++ kenv2.map)
    }

    /**
      * Merges all the given kind environments.
      *
      * The environments must be disjoint.
      */
    def disjointMerge(kenvs: List[KindEnv]): KindEnv = {
      kenvs.fold(KindEnv.empty)(disjointAppend)
    }
  }

  private case class KindEnv(map: Map[Symbol.UnkindedTypeVarSym, Kind]) {
    /**
      * Adds the given mapping to the kind environment.
      */
    def +(pair: (Symbol.UnkindedTypeVarSym, Kind)): Validation[KindEnv, KindError] = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) => unify(kind0, kind) match {
          case Some(minKind) => Validation.success(KindEnv(map + (tvar -> minKind)))
          case None =>
            val e = KindError.MismatchedKinds(kind0, kind, tvar.loc)
            Validation.toSoftFailure(KindEnv(map + (tvar -> kind0)), e)
        }
        case None => Validation.success(KindEnv(map + (tvar -> kind)))
      }
    }

    /**
      * Merges the given kind environment into this kind environment.
      */
    def ++(other: KindEnv): Validation[KindEnv, KindError] = {
      fold(other.map, this) {
        case (acc, pair) => acc + pair
      }
    }
  }

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[KindError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[KindError])


  /**
    * Instantiates the scheme of the struct in corresponding to `sym` in `structs`
    * Returns a map from field name to its instantiated type, the type of the instantiated struct, and the instantiated struct's region variable
    *
    * For example, for the struct `struct S [v, r] { a: v, b: Int32 }` where `v` instantiates to `v'` and `r` instantiates to `r'`
    *   The first element of the return tuple would be a map with entries `a -> v'` and `b -> Int32`
    *   The second element of the return tuple would be(locations omitted) `Apply(Apply(Cst(Struct(S)), v'), r')`
    *   The third element of the return tuple would be `r'`
    */
  def instantiateStruct(sym: Symbol.StructSym, structs: Map[Symbol.StructSym, KindedAst.Struct])(implicit scope: Scope, flix: Flix) : (Map[Symbol.StructFieldSym, Type], Type, Type.Var) = {
    val struct = structs(sym)
    val fields = struct.fields
    val (_, _, tpe, substMap) = Scheme.instantiate(struct.sc, struct.loc)
    val subst = Substitution(substMap)
    val instantiatedFields = fields.map(f => f match {
      case KindedAst.StructField(_, fieldSym, tpe, _) =>
        fieldSym -> subst(tpe)
    })
    (instantiatedFields.toMap, tpe, substMap(struct.tparams.last.sym))
  }
}
