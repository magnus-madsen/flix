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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Kind.WildCaseSet
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, DefSymUse, SigSymUse}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unify
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
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

  def run(root: ResolvedAst.Root, oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (KindedAst.Root, List[KindError]) = flix.phaseNew("Kinder") {
    implicit val sctx: SharedContext = SharedContext.mk()

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    val taenv = visitTypeAliases(root.taOrder, root)

    val enums = ParOps.parMapValues(root.enums)(visitEnum(_, taenv, root))

    val structs = ParOps.parMapValues(root.structs)(visitStruct(_, taenv, root))

    val restrictableEnums = ParOps.parMapValues(root.restrictableEnums)(visitRestrictableEnum(_, taenv, root))

    val traits = visitTraits(root, taenv, oldRoot, changeSet)

    val defs = visitDefs(root, taenv, oldRoot, changeSet)

    val instances = ParOps.parMapValueList(root.instances)(visitInstance(_, taenv, root))

    val effects = ParOps.parMapValues(root.effects)(visitEffect(_, taenv, root))

    val newRoot = KindedAst.Root(traits, instances, defs, enums, structs, restrictableEnums, effects, taenv, root.uses, root.mainEntryPoint, root.sources, root.availableClasses, root.tokens)

    (newRoot, sctx.errors.asScala.toList)
  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum0: ResolvedAst.Declaration.Enum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Enum = enum0 match {
    case ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives, cases0, loc) =>
      val kenv = getKindEnvFromTypeParams(tparams0)
      val tparams = tparams0.map(visitTypeParam(_, kenv))
      val targs = tparams.map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
      val t = Type.mkApply(Type.Cst(TypeConstructor.Enum(sym, getEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
      val cases = cases0.map(visitCase(_, tparams, t, kenv, taenv, root)).map(caze => caze.sym -> caze).toMap
      KindedAst.Enum(doc, ann, mod, sym, tparams, derives, cases, t, loc)
  }

  /**
    * Performs kinding on the given struct.
    */
  private def visitStruct(struct0: ResolvedAst.Declaration.Struct, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Struct = struct0 match {
    case ResolvedAst.Declaration.Struct(doc, ann, mod, sym, tparams0, fields0, loc) =>
      // In the case in which the user doesn't supply any type params,
      // the parser will have already notified the user of this error
      // The recovery step here is to simply add a single type param that is never used
      val tparams1 = if (tparams0.isEmpty) {
        val regionTparam = ResolvedAst.TypeParam.Unkinded(Name.Ident("$rc", loc), Symbol.freshUnkindedTypeVarSym(VarText.Absent, loc)(Scope.Top, flix), loc)
        List(regionTparam)
      } else {
        tparams0
      }
      val kenv1 = getKindEnvFromTypeParams(tparams1.init)
      val kenv2 = getKindEnvFromRegion(tparams1.last)
      // The last add is simply to verify that the last tparam was marked as Region
      val kenv = KindEnv.disjointAppend(kenv1, kenv2) + (tparams1.last.sym -> Kind.Region)
      val tparams = tparams1.map(visitTypeParam(_, kenv))
      val fields = fields0.map(visitStructField(_, kenv, taenv, root))
      val targs = tparams.map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
      val sc = Scheme(tparams.map(_.sym), List(), List(), Type.mkStruct(sym, targs, loc))
      KindedAst.Struct(doc, ann, mod, sym, tparams, sc, fields, loc)
  }

  /**
    * Performs kinding on the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: ResolvedAst.Declaration.RestrictableEnum, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.RestrictableEnum = enum0 match {
    case ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives, cases0, loc) =>
      val kenvIndex = getKindEnvFromIndex(index0, sym)
      val kenvTparams = getKindEnvFromTypeParams(tparams0)
      val kenv = KindEnv.disjointAppend(kenvIndex, kenvTparams)
      val index = visitIndex(index0, sym, kenv)
      val tparams = tparams0.map(visitTypeParam(_, kenv))
      val targs = (index :: tparams).map(tparam => Type.Var(tparam.sym, tparam.loc.asSynthetic))
      val t = Type.mkApply(Type.Cst(TypeConstructor.RestrictableEnum(sym, getRestrictableEnumKind(enum0)), sym.loc.asSynthetic), targs, sym.loc.asSynthetic)
      val cases = cases0.map(visitRestrictableCase(_, index, tparams, t, kenv, taenv, root)).map(caze => caze.sym -> caze).toMap
      KindedAst.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, t, loc)
  }

  /**
    * Performs kinding on the given type alias.
    * Returns the kind of the type alias.
    */
  private def visitTypeAlias(alias: ResolvedAst.Declaration.TypeAlias, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.TypeAlias = alias match {
    case ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams0, tpe0, loc) =>
      val kenv = getKindEnvFromTypeParams(tparams0)
      val tparams = tparams0.map(visitTypeParam(_, kenv))
      val t = visitType(tpe0, Kind.Wild, kenv, taenv, root)
      KindedAst.TypeAlias(doc, ann, mod, sym, tparams, t, loc)
  }

  /**
    * Performs kinding on the given type aliases.
    * The aliases must be sorted topologically.
    */
  private def visitTypeAliases(aliases: List[Symbol.TypeAliasSym], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Map[Symbol.TypeAliasSym, KindedAst.TypeAlias] = {
    aliases.foldLeft(Map.empty[Symbol.TypeAliasSym, KindedAst.TypeAlias]) {
      case (taenv, sym) =>
        val alias = root.typeAliases(sym)
        val kind = visitTypeAlias(alias, taenv, root)
        taenv + (sym -> kind)
    }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitCase(caze0: ResolvedAst.Declaration.Case, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Case = caze0 match {
    case ResolvedAst.Declaration.Case(sym, tpes0, loc) =>
      val ts = tpes0.map(visitType(_, Kind.Star, kenv, taenv, root))
      val quants = tparams.map(_.sym)
      val schemeBase = Type.mkPureUncurriedArrow(ts, resTpe, sym.loc.asSynthetic)
      val sc = Scheme(quants, Nil, Nil, schemeBase)
      KindedAst.Case(sym, ts, sc, loc)
  }

  /**
    * Performs kinding on the given struct field under the given kind environment.
    */
  private def visitStructField(field0: ResolvedAst.Declaration.StructField, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.StructField = field0 match {
    case ResolvedAst.Declaration.StructField(mod, sym, tpe0, loc) =>
      val t = visitType(tpe0, Kind.Star, kenv, taenv, root)
      KindedAst.StructField(mod, sym, t, loc)
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def visitRestrictableCase(caze0: ResolvedAst.Declaration.RestrictableCase, index: KindedAst.TypeParam, tparams: List[KindedAst.TypeParam], resTpe: Type, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.RestrictableCase = caze0 match {
    case ResolvedAst.Declaration.RestrictableCase(sym, tpes0, loc) =>
      val ts = tpes0.map(visitType(_, Kind.Star, kenv, taenv, root))
      val quants = (index :: tparams).map(_.sym)
      val schemeBase = Type.mkPureUncurriedArrow(ts, resTpe, sym.loc.asSynthetic)
      val sc = Scheme(quants, Nil, Nil, schemeBase)
      KindedAst.RestrictableCase(sym, ts, sc, loc) // TODO RESTR-VARS the scheme is different for these. REVISIT
  }

  /**
    * Performs kinding on the all the traits in the given root.
    */
  private def visitTraits(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit sctx: SharedContext, flix: Flix): Map[Symbol.TraitSym, KindedAst.Trait] = {
    val res = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait(_, taenv, root)))
    res
  }

  /**
    * Performs kinding on the given trait.
    */
  private def visitTrait(trt: ResolvedAst.Declaration.Trait, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Trait = trt match {
    case ResolvedAst.Declaration.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, sigs0, laws0, loc) =>
      val kenv = getKindEnvFromTypeParam(tparam0)
      val tparam = visitTypeParam(tparam0, kenv)
      val superTraits = superTraits0.map(visitTraitConstraint(_, kenv, taenv, root))
      val assocs = assocs0.map(visitAssocTypeSig(_, kenv, taenv, root))
      val sigs = sigs0.map {
        case (sigSym, sig0) =>
          val sig = visitSig(sig0, tparam, kenv, taenv, root)
          sigSym -> sig
      }
      val laws = laws0.map(visitDef(_, kenv, taenv, root)) // TODO ASSOC-TYPES need to include super traits?
      KindedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Declaration.Instance, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Instance = inst match {
    case ResolvedAst.Declaration.Instance(doc, ann, mod, symUse, tparams0, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val kind = getTraitKind(root.traits(symUse.sym))
      val kenv = inferType(tpe0, kind, KindEnv.empty, taenv, root)
      val tparams = tparams0.map(visitTypeParam(_, kenv))
      val t = visitType(tpe0, kind, kenv, taenv, root)
      val tconstrs = tconstrs0.map(visitTraitConstraint(_, kenv, taenv, root))
      val assocs = assocs0.map(visitAssocTypeDef(_, kind, kenv, taenv, root))
      val defs = defs0.map(visitDef(_, kenv, taenv, root))
      KindedAst.Instance(doc, ann, mod, symUse, tparams, t, tconstrs, assocs, defs, ns, loc)
  }

  /**
    * Performs kinding on the given effect declaration.
    */
  private def visitEffect(eff: ResolvedAst.Declaration.Effect, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Effect = eff match {
    case ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp(_, taenv, root))
      KindedAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Performs kinding on the all the definitions in the given root.
    */
  private def visitDefs(root: ResolvedAst.Root, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], oldRoot: KindedAst.Root, changeSet: ChangeSet)(implicit sctx: SharedContext, flix: Flix): Map[Symbol.DefnSym, KindedAst.Def] = {
    changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef(_, KindEnv.empty, taenv, root)))
  }

  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Declaration.Def, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Def = def0 match {
    case ResolvedAst.Declaration.Def(sym, spec0, exp0, loc) =>
      flix.subtask(sym.toString, sample = true)
      val kenv = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      val henv = None
      val spec = visitSpec(spec0, Nil, kenv, taenv, root)
      val exp = visitExp(exp0, kenv, taenv, root)(Scope.Top, sctx, flix)
      KindedAst.Def(sym, spec, exp, loc)
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Declaration.Sig, traitTparam: KindedAst.TypeParam, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Sig = sig0 match {
    case ResolvedAst.Declaration.Sig(sym, spec0, exp0, loc) =>
      val kenv = getKindEnvFromSpec(spec0, kenv0, taenv, root)
      val henv = None
      val spec = visitSpec(spec0, List(traitTparam.sym), kenv, taenv, root)
      val exp = exp0.map(visitExp(_, kenv, taenv, root)(Scope.Top, sctx, flix))
      KindedAst.Sig(sym, spec, exp, loc)
  }

  /**
    * Performs kinding on the given effect operation under the given kind environment.
    */
  private def visitOp(op: ResolvedAst.Declaration.Op, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Op = op match {
    case ResolvedAst.Declaration.Op(sym, spec0, loc) =>
      val kenv = inferSpec(spec0, KindEnv.empty, taenv, root)
      val spec = visitSpec(spec0, Nil, kenv, taenv, root)
      KindedAst.Op(sym, spec, loc)
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    *
    * Adds `quantifiers` to the generated scheme's quantifier list.
    */
  private def visitSpec(spec0: ResolvedAst.Spec, quantifiers: List[Symbol.KindedTypeVarSym], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.Spec = spec0 match {
    case ResolvedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, eff0, tconstrs0, econstrs0) =>
      val tparams = tparams0.map(visitTypeParam(_, kenv))
      val fparams = fparams0.map(visitFormalParam(_, kenv, taenv, root))
      val tpe = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val eff = visitEffectDefaultPure(eff0, kenv, taenv, root)
      val tconstrs = tconstrs0.map(visitTraitConstraint(_, kenv, taenv, root))
      val econstrs = econstrs0.map(visitEqualityConstraint(_, kenv, taenv, root))
      val allQuantifiers = quantifiers ::: tparams.map(_.sym)
      val base = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), eff, tpe, tpe.loc)
      val sc = Scheme(allQuantifiers, tconstrs, econstrs, base)
      KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs)
  }

  /**
    * Performs kinding on the given associated type signature under the given kind environment.
    */
  private def visitAssocTypeSig(s0: ResolvedAst.Declaration.AssocTypeSig, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.AssocTypeSig = s0 match {
    case ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam0, kind, tpe0, loc) =>
      val tparam = visitTypeParam(tparam0, kenv)
      val tpe = tpe0.map(visitType(_, kind, kenv, taenv, root))
      KindedAst.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc)
  }

  /**
    * Performs kinding on the given associated type definition under the given kind environment.
    */
  private def visitAssocTypeDef(d0: ResolvedAst.Declaration.AssocTypeDef, trtKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.AssocTypeDef = d0 match {
    case ResolvedAst.Declaration.AssocTypeDef(doc, mod, symUse, arg0, tpe0, loc) =>
      val trt = root.traits(symUse.sym.trt)
      val assocSig = trt.assocs.find(assoc => assoc.sym == symUse.sym).get
      val tpeKind = assocSig.kind
      val args = visitType(arg0, trtKind, kenv, taenv, root)
      val tpe = visitType(tpe0, tpeKind, kenv, taenv, root)
      KindedAst.AssocTypeDef(doc, mod, symUse, args, tpe, loc)
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  private def visitExp(exp00: ResolvedAst.Expr, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.Expr = exp00 match {
    case ResolvedAst.Expr.Var(sym, loc) =>
      KindedAst.Expr.Var(sym, loc)

    case ResolvedAst.Expr.Hole(sym, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshEffSlackVar(loc.asSynthetic)
      KindedAst.Expr.Hole(sym, tvar, evar, loc)

    case ResolvedAst.Expr.HoleWithExp(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshEffSlackVar(loc.asSynthetic)
      KindedAst.Expr.HoleWithExp(exp, tvar, evar, loc)

    case ResolvedAst.Expr.OpenAs(symUse, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.OpenAs(symUse, exp, tvar, loc)

    case ResolvedAst.Expr.Use(sym, alias, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.Use(sym, alias, exp, loc)

    case ResolvedAst.Expr.Cst(cst, loc) =>
      KindedAst.Expr.Cst(cst, loc)

    case ResolvedAst.Expr.ApplyClo(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ApplyClo(exp1, exp2, tvar, evar, loc)

    case ResolvedAst.Expr.ApplyDef(DefSymUse(sym, loc1), exps0, loc2) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val itvar = Type.freshVar(Kind.Star, loc1.asSynthetic)
      val tvar = Type.freshVar(Kind.Star, loc2.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc2.asSynthetic)
      KindedAst.Expr.ApplyDef(DefSymUse(sym, loc1), exps, itvar, tvar, evar, loc2)

    case ResolvedAst.Expr.ApplyLocalDef(symUse, exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val arrowTvar = Type.freshVar(Kind.Star, loc.asSynthetic) // use loc of symuse
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ApplyLocalDef(symUse, exps, arrowTvar, tvar, evar, loc)

    case ResolvedAst.Expr.ApplySig(SigSymUse(sym, loc1), exps0, loc2) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val itvar = Type.freshVar(Kind.Star, loc1.asSynthetic)
      val tvar = Type.freshVar(Kind.Star, loc2.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc2.asSynthetic)
      KindedAst.Expr.ApplySig(SigSymUse(sym, loc1), exps, itvar, tvar, evar, loc2)

    case ResolvedAst.Expr.Lambda(fparam0, exp0, allowSubeffecting, loc) =>
      val fparam = visitFormalParam(fparam0, kenv0, taenv, root)
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.Lambda(fparam, exp, allowSubeffecting, loc)

    case ResolvedAst.Expr.Unary(sop, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.Unary(sop, exp, tvar, loc)

    case ResolvedAst.Expr.Binary(sop, exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc)

    case ResolvedAst.Expr.IfThenElse(exp10, exp20, exp30, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val exp3 = visitExp(exp30, kenv0, taenv, root)
      KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc)

    case ResolvedAst.Expr.Stm(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.Stm(exp1, exp2, loc)

    case ResolvedAst.Expr.Discard(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.Discard(exp, loc)

    case ResolvedAst.Expr.Let(sym, exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.Let(sym, exp1, exp2, loc)

    case ResolvedAst.Expr.LocalDef(sym, fparams0, exp10, exp20, loc) =>
      // we must infer the formal parameters because the may contain wildcard types
      // which would not appear in the function's kenv
      val fparamKenvs = fparams0.map(inferFormalParam(_, kenv0, taenv, root))
      val kenv1 = KindEnv.merge(kenv0 :: fparamKenvs)
      val fparams = fparams0.map(visitFormalParam(_, kenv1, taenv, root))
      val exp1 = visitExp(exp10, kenv1, taenv, root)
      // We visit exp2 outside the new kenv since it's not in the def's scope
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.LocalDef(sym, fparams, exp1, exp2, loc)

    case ResolvedAst.Expr.Region(tpe, loc) =>
      KindedAst.Expr.Region(tpe, loc)

    case ResolvedAst.Expr.Scope(sym, regSym, exp0, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      // Record that we enter the new scope.
      val newScope = scope.enter(regSym)
      val exp = visitExp(exp0, kenv0, taenv, root)(newScope, sctx, flix)
      KindedAst.Expr.Scope(sym, regSym, exp, tvar, evar, loc)

    case ResolvedAst.Expr.Match(exp0, rules0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val rules = rules0.map(visitMatchRule(_, kenv0, taenv, root))
      KindedAst.Expr.Match(exp, rules, loc)

    case ResolvedAst.Expr.TypeMatch(exp0, rules0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val rules = rules0.map(visitTypeMatchRule(_, kenv0, taenv, root))
      KindedAst.Expr.TypeMatch(exp, rules, loc)

    case ResolvedAst.Expr.RestrictableChoose(star, exp0, rules0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val rules = rules0.map(visitRestrictableChooseRule(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.RestrictableChoose(star, exp, rules, tvar, loc)

    case ResolvedAst.Expr.Tag(symUse, exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.Tag(symUse, exps, tvar, loc)

    case ResolvedAst.Expr.RestrictableTag(symUse, exps0, isOpen, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.RestrictableTag(symUse, exps, isOpen, tvar, evar, loc)

    case ResolvedAst.Expr.Tuple(exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      KindedAst.Expr.Tuple(exps, loc)

    case ResolvedAst.Expr.RecordSelect(exp0, label, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.RecordSelect(exp, label, tvar, loc)

    case ResolvedAst.Expr.RecordExtend(label, exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.RecordExtend(label, exp1, exp2, tvar, loc)

    case ResolvedAst.Expr.RecordRestrict(label, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.RecordRestrict(label, exp, tvar, loc)

    case ResolvedAst.Expr.ArrayLit(exps0, exp0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ArrayLit(exps, exp, tvar, evar, loc)

    case ResolvedAst.Expr.ArrayNew(exp10, exp20, exp30, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val exp3 = visitExp(exp30, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, evar, loc)

    case ResolvedAst.Expr.ArrayLoad(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, evar, loc)

    case ResolvedAst.Expr.ArrayStore(exp10, exp20, exp30, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val exp3 = visitExp(exp30, kenv0, taenv, root)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ArrayStore(exp1, exp2, exp3, evar, loc)

    case ResolvedAst.Expr.ArrayLength(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.ArrayLength(exp, evar, loc)

    case ResolvedAst.Expr.StructNew(sym, exps0, region0, loc) =>
      val fields = exps0.map {
        case (symUse, fieldExp0) =>
          val exp = visitExp(fieldExp0, kenv0, taenv, root)
          (symUse, exp)
      }
      val region = visitExp(region0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.StructNew(sym, fields, region, tvar, evar, loc)

    case ResolvedAst.Expr.StructGet(exp0, symUse, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.StructGet(exp, symUse, tvar, evar, loc)

    case ResolvedAst.Expr.StructPut(exp10, symUse, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.StructPut(exp1, symUse, exp2, tvar, evar, loc)

    case ResolvedAst.Expr.VectorLit(exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.VectorLit(exps, tvar, evar, loc)

    case ResolvedAst.Expr.VectorLoad(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.VectorLoad(exp1, exp2, tvar, evar, loc)

    case ResolvedAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, kenv0, taenv, root)
      KindedAst.Expr.VectorLength(e, loc)

    case ResolvedAst.Expr.Ascribe(exp0, expectedType0, expectedEff0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)

      // We must infer for the ascriptions because they may have wildcard types,
      // which won't be found in the kenv of the function
      val kenvTpe = expectedType0.map(inferType(_, Kind.Star, kenv0, taenv, root)).getOrElse(KindEnv.empty)
      val kenvEff = expectedEff0.map(inferType(_, Kind.Eff, kenv0, taenv, root)).getOrElse(KindEnv.empty)
      val kenv = KindEnv.merge(List(kenv0, kenvTpe, kenvEff))
      val expectedType = expectedType0.map(visitType(_, Kind.Star, kenv, taenv, root))
      val expectedEff = expectedEff0.map(visitType(_, Kind.Eff, kenv, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.Ascribe(exp, expectedType, expectedEff, tvar, loc)

    case ResolvedAst.Expr.InstanceOf(exp0, clazz, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.InstanceOf(exp, clazz, loc)

    case ResolvedAst.Expr.CheckedCast(cast, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc)
      val evar = Type.freshVar(Kind.Eff, loc)
      KindedAst.Expr.CheckedCast(cast, exp, tvar, evar, loc)

    case ResolvedAst.Expr.UncheckedCast(exp0, declaredType0, declaredEff0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val declaredType = declaredType0.map(visitType(_, Kind.Star, kenv0, taenv, root))
      val declaredEff = declaredEff0.map(visitType(_, Kind.Eff, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tvar, loc)

    case ResolvedAst.Expr.Unsafe(exp0, eff0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val eff = visitType(eff0, Kind.Eff, kenv0, taenv, root)
      KindedAst.Expr.Unsafe(exp, eff, loc)

    case ResolvedAst.Expr.Without(exp0, symUse, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.Without(exp, symUse, loc)

    case ResolvedAst.Expr.TryCatch(exp0, rules0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val rules = rules0.map(visitCatchRule(_, kenv0, taenv, root))
      KindedAst.Expr.TryCatch(exp, rules, loc)

    case ResolvedAst.Expr.Throw(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc)
      val evar = Type.freshVar(Kind.Eff, loc)
      KindedAst.Expr.Throw(exp, tvar, evar, loc)

    case ResolvedAst.Expr.Handler(symUse, rules0, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc)
      val evar1 = Type.freshVar(Kind.Eff, loc)
      val evar2 = Type.freshVar(Kind.Eff, loc)
      val rules = rules0.map(visitHandlerRule(_, kenv0, taenv, tvar, root))
      KindedAst.Expr.Handler(symUse, rules, tvar, evar1, evar2, loc)

    case ResolvedAst.Expr.RunWith(exp10, exp20, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc)
      val evar = Type.freshVar(Kind.Eff, loc)

      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.RunWith(exp1, exp2, tvar, evar, loc)

    case ResolvedAst.Expr.Do(symUse, exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc)
      KindedAst.Expr.Do(symUse, exps, tvar, loc)

    case ResolvedAst.Expr.InvokeConstructor(clazz, exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.InvokeConstructor(clazz, exps, jvar, evar, loc)

    case ResolvedAst.Expr.InvokeMethod(exp0, methodName, exps0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.InvokeMethod(exp, methodName, exps, jvar, tvar, evar, loc)

    case ResolvedAst.Expr.InvokeStaticMethod(clazz, methodName, exps0, loc) =>
      val exps = exps0.map(visitExp(_, kenv0, taenv, root))
      val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.InvokeStaticMethod(clazz, methodName, exps, jvar, tvar, evar, loc)

    case ResolvedAst.Expr.GetField(exp0, fieldName, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val jvar = Type.freshVar(Kind.Jvm, loc.asSynthetic)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.GetField(exp, fieldName, jvar, tvar, evar, loc)

    case ResolvedAst.Expr.PutField(field, clazz, exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.PutField(field, clazz, exp1, exp2, loc)

    case ResolvedAst.Expr.GetStaticField(field, loc) =>
      KindedAst.Expr.GetStaticField(field, loc)

    case ResolvedAst.Expr.PutStaticField(field, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.PutStaticField(field, exp, loc)

    case ResolvedAst.Expr.NewObject(name, clazz, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod(_, kenv0, taenv, root))
      KindedAst.Expr.NewObject(name, clazz, methods, loc)

    case ResolvedAst.Expr.NewChannel(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.NewChannel(exp, tvar, loc)

    case ResolvedAst.Expr.GetChannel(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.GetChannel(exp, tvar, evar, loc)

    case ResolvedAst.Expr.PutChannel(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.PutChannel(exp1, exp2, evar, loc)

    case ResolvedAst.Expr.SelectChannel(rules0, exp0, loc) =>
      val rules = rules0.map(visitSelectChannelRule(_, kenv0, taenv, root))
      val exp = exp0.map(visitExp(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.SelectChannel(rules, exp, tvar, evar, loc)

    case ResolvedAst.Expr.Spawn(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.Spawn(exp1, exp2, loc)

    case ResolvedAst.Expr.ParYield(frags0, exp0, loc) =>
      val frags = frags0.map {
        case ResolvedAst.ParYieldFragment(pat1, exp1, l0) =>
          val pat = visitPattern(pat1, kenv0, root)
          val exp = visitExp(exp1, kenv0, taenv, root)
          KindedAst.ParYieldFragment(pat, exp, l0)
      }
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.ParYield(frags, exp, loc)

    case ResolvedAst.Expr.Lazy(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.Lazy(exp, loc)

    case ResolvedAst.Expr.Force(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.Force(exp, tvar, loc)

    case ResolvedAst.Expr.FixpointConstraintSet(cs0, loc) =>
      val cs = cs0.map(visitConstraint(_, kenv0, taenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.FixpointConstraintSet(cs, tvar, loc)

    case ResolvedAst.Expr.FixpointLambda(pparams0, exp0, loc) =>
      val pparams = pparams0.map(visitPredicateParam(_, kenv0, taenv, root))
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc)

    case ResolvedAst.Expr.FixpointMerge(exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      KindedAst.Expr.FixpointMerge(exp1, exp2, loc)

    case ResolvedAst.Expr.FixpointSolve(exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      KindedAst.Expr.FixpointSolve(exp, loc)

    case ResolvedAst.Expr.FixpointFilter(pred, exp0, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc)

    case ResolvedAst.Expr.FixpointInject(exp0, pred, loc) =>
      val exp = visitExp(exp0, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      val evar = Type.freshVar(Kind.Eff, loc.asSynthetic)
      KindedAst.Expr.FixpointInject(exp, pred, tvar, evar, loc)

    case ResolvedAst.Expr.FixpointProject(pred, exp10, exp20, loc) =>
      val exp1 = visitExp(exp10, kenv0, taenv, root)
      val exp2 = visitExp(exp20, kenv0, taenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc)

    case ResolvedAst.Expr.Error(m) =>
      val tvar = Type.freshVar(Kind.Star, m.loc)
      val evar = Type.freshEffSlackVar(m.loc)
      KindedAst.Expr.Error(m, tvar, evar)
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitMatchRule(rule0: ResolvedAst.MatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.MatchRule = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      val pat = visitPattern(pat0, kenv, root)
      val guard = guard0.map(visitExp(_, kenv, taenv, root))
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def visitTypeMatchRule(rule0: ResolvedAst.TypeMatchRule, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.TypeMatchRule = rule0 match {
    case ResolvedAst.TypeMatchRule(sym, tpe0, exp0) =>
      val kenv = inferType(tpe0, Kind.Star, kenv0, taenv, root)
      val tpe = visitType(tpe0, Kind.Star, kenv, taenv, root)
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.TypeMatchRule(sym, tpe, exp)
  }

  /**
    * Performs kinding on the given relational choice rule under the given kind environment.
    */
  private def visitRestrictableChooseRule(rule0: ResolvedAst.RestrictableChooseRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.RestrictableChooseRule = rule0 match {
    case ResolvedAst.RestrictableChooseRule(pat0, exp0) =>
      val pat = visitRestrictableChoosePattern(pat0)
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.RestrictableChooseRule(pat, exp)
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def visitCatchRule(rule0: ResolvedAst.CatchRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.CatchRule = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.CatchRule(sym, clazz, exp)
  }

  /**
    * Performs kinding on the given handler rule under the given kind environment.
    */
  private def visitHandlerRule(rule0: ResolvedAst.HandlerRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], hTvar: Type.Var, root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.HandlerRule = rule0 match {
    case ResolvedAst.HandlerRule(symUse, fparams0, exp0) =>
      // create a new type variable for the op return type (same as resume argument type)
      val tvar = Type.freshVar(Kind.Star, exp0.loc)

      val henv = Some((tvar, hTvar))

      val fparams = fparams0.map(visitFormalParam(_, kenv, taenv, root))
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.HandlerRule(symUse, fparams, exp, tvar)
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def visitSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.SelectChannelRule = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      val chan = visitExp(chan0, kenv, taenv, root)
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.SelectChannelRule(sym, chan, exp)
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def visitPattern(pat00: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root)(implicit scope: Scope, flix: Flix): KindedAst.Pattern = pat00 match {
    case ResolvedAst.Pattern.Wild(loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Pattern.Wild(tvar, loc)

    case ResolvedAst.Pattern.Var(sym, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Pattern.Var(sym, tvar, loc)

    case ResolvedAst.Pattern.Cst(cst, loc) => KindedAst.Pattern.Cst(cst, loc)
    case ResolvedAst.Pattern.Tag(symUse, pats0, loc) =>
      val pats = pats0.map(visitPattern(_, kenv, root))
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Pattern.Tag(symUse, pats, tvar, loc)

    case ResolvedAst.Pattern.Tuple(pats0, loc) =>
      val pats = pats0.map(visitPattern(_, kenv, root))
      KindedAst.Pattern.Tuple(pats, loc)

    case ResolvedAst.Pattern.Record(pats0, pat0, loc) =>
      val pats = pats0.map {
        case ResolvedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
          val tvar = Type.freshVar(Kind.Star, loc1.asSynthetic)
          val pat = visitPattern(pat1, kenv, root)
          KindedAst.Pattern.Record.RecordLabelPattern(label, pat, tvar, loc1)
      }
      val pat = visitPattern(pat0, kenv, root)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Pattern.Record(pats, pat, tvar, loc)

    case ResolvedAst.Pattern.Error(loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.Pattern.Error(tvar, loc)
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoosePattern(pat00: ResolvedAst.RestrictableChoosePattern)(implicit scope: Scope, flix: Flix): KindedAst.RestrictableChoosePattern = pat00 match {
    case ResolvedAst.RestrictableChoosePattern.Tag(symUse, pats0, loc) =>
      val pats = pats0.map(visitRestrictableChoosePatternVarOrWild)
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.RestrictableChoosePattern.Tag(symUse, pats, tvar, loc)

    case ResolvedAst.RestrictableChoosePattern.Error(loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.RestrictableChoosePattern.Error(tvar, loc)
  }

  /**
    * Performs kinding on the given restrictable choice pattern under the given kind environment.
    */
  private def visitRestrictableChoosePatternVarOrWild(pat0: ResolvedAst.RestrictableChoosePattern.VarOrWild)(implicit scope: Scope, flix: Flix): KindedAst.RestrictableChoosePattern.VarOrWild = pat0 match {
    case ResolvedAst.RestrictableChoosePattern.Wild(loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.RestrictableChoosePattern.Wild(tvar, loc)

    case ResolvedAst.RestrictableChoosePattern.Var(sym, loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.RestrictableChoosePattern.Var(sym, tvar, loc)

    case ResolvedAst.RestrictableChoosePattern.Error(loc) =>
      val tvar = Type.freshVar(Kind.Star, loc.asSynthetic)
      KindedAst.RestrictableChoosePattern.Error(tvar, loc)
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def visitConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.Constraint = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparams = cparams0.map(visitConstraintParam)
      val head = visitHeadPredicate(head0, kenv, taenv, root)
      val body = body0.map(visitBodyPredicate(_, kenv, taenv, root))
      KindedAst.Constraint(cparams, head, body, loc)
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
  private def visitHeadPredicate(pred0: ResolvedAst.Predicate.Head, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.Predicate.Head = pred0 match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, loc) =>
      val terms = terms0.map(visitExp(_, kenv, taenv, root))
      val pvar = Type.freshVar(Kind.Predicate, loc.asSynthetic)
      KindedAst.Predicate.Head.Atom(pred, den, terms, pvar, loc)
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def visitBodyPredicate(pred0: ResolvedAst.Predicate.Body, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.Predicate.Body = pred0 match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms0, loc) =>
      val terms = terms0.map(visitPattern(_, kenv, root))
      val pvar = Type.freshVar(Kind.Predicate, loc.asSynthetic)
      KindedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, pvar, loc)

    case ResolvedAst.Predicate.Body.Functional(syms, exp0, loc) =>
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.Predicate.Body.Functional(syms, exp, loc)

    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      val exp = visitExp(exp0, kenv, taenv, root)
      KindedAst.Predicate.Body.Guard(exp, loc)
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `expectedKind` expected from context.
    */
  private def visitTypeVar(tvar: UnkindedType.Var, expectedKind: Kind, kenv: KindEnv)(implicit sctx: SharedContext): Type.Var = tvar match {
    case UnkindedType.Var(sym0, loc) =>
      val sym = visitTypeVarSym(sym0, expectedKind, kenv, loc)
      Type.Var(sym, loc)
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
  private def visitType(tpe0: UnkindedType, expectedKind: Kind, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Type = tpe0 match {
    case tvar: UnkindedType.Var => visitTypeVar(tvar, expectedKind, kenv)

    case UnkindedType.Cst(cst, loc) =>
      val kind = cst.kind
      unify(expectedKind, kind) match {
        case Some(_) => Type.Cst(cst, loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.Apply(t10, t20, loc) =>
      val t2 = visitType(t20, Kind.Wild, kenv, taenv, root)
      val k1 = Kind.Arrow(t2.kind, expectedKind)
      val t1 = visitType(t10, k1, kenv, taenv, root)
      mkApply(t1, t2, loc)

    case UnkindedType.Ascribe(t, k, loc) =>
      unify(k, expectedKind) match {
        case Some(kind) => visitType(t, kind, kenv, taenv, root)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.Alias(cst, args0, t0, loc) =>
      taenv(cst.sym) match {
        case KindedAst.TypeAlias(_, _, _, _, tparams, tpe, _) =>
          val args = tparams.zip(args0).map { case (tparam, arg) => visitType(arg, tparam.sym.kind, kenv, taenv, root) }
          val t = visitType(t0, tpe.kind, kenv, taenv, root)
          unify(t.kind, expectedKind) match {
            case Some(_) => Type.Alias(cst, args, t, loc)
            case None =>
              sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc))
              Type.freshError(Kind.Error, loc)
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
              val arg = visitType(arg0, innerExpectedKind, kenv, taenv, root)
              Type.AssocType(cst, arg, kind, loc)
            case None =>
              sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = k0, loc))
              Type.freshError(Kind.Error, loc)
          }
      }

    case UnkindedType.Arrow(eff0, arity, loc) =>
      val kind = Kind.mkArrow(arity)
      unify(kind, expectedKind) match {
        case Some(_) =>
          val eff = visitEffectDefaultPure(eff0, kenv, taenv, root)
          Type.mkApply(Type.Cst(TypeConstructor.Arrow(arity), loc), List(eff), loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.Enum(sym, loc) =>
      val kind = getEnumKind(root.enums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.Enum(sym, k), loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.Struct(sym, loc) =>
      val kind = getStructKind(root.structs(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.Struct(sym, k), loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.RestrictableEnum(sym, loc) =>
      val kind = getRestrictableEnumKind(root.restrictableEnums(sym))
      unify(kind, expectedKind) match {
        case Some(k) => Type.Cst(TypeConstructor.RestrictableEnum(sym, k), loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = kind, loc))
          Type.freshError(Kind.Error, loc)
      }

    case UnkindedType.CaseSet(cases, loc) =>
      // Infer the kind from the cases.
      val actualKind: Kind = cases.foldLeft(Kind.WildCaseSet: Kind) {
        case (kindAcc, sym) =>
          val symKind = Kind.CaseSet(sym.enumSym)
          unify(kindAcc, symKind) match {
            // Case 1: The kinds unify. Update the kind.
            case Some(k) => k
            // Case 2: The kinds do not unify. Error.
            case None =>
              sctx.errors.add(KindError.MismatchedKinds(kindAcc, symKind, loc))
              Kind.Error
          }
      }

      // Check against the expected kind.
      unify(actualKind, expectedKind) match {
        // Case 1:  We have an explicit case kind.
        case Some(Kind.CaseSet(sym)) => Type.Cst(TypeConstructor.CaseSet(cases.to(SortedSet), sym), loc)
        // Case 2: We have a generic case kind. Error.
        case Some(Kind.WildCaseSet) =>
          sctx.errors.add(KindError.UninferrableKind(loc))
          Type.freshError(Kind.Error, loc)
        // Case 3: Unexpected kind. Error.
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc))
          Type.freshError(Kind.Error, loc)

        case Some(k) if Kind.hasError(k) =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc))
          Type.freshError(Kind.Error, loc)

        case Some(_) => throw InternalCompilerException("unexpected non-case set kind", loc)
      }


    case UnkindedType.CaseComplement(t0, loc) =>
      val t = visitType(t0, Kind.WildCaseSet, kenv, taenv, root)
      unify(t.kind, expectedKind) match {
        case Some(Kind.CaseSet(enumSym)) => Type.mkCaseComplement(t, enumSym, loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = t.kind, loc))
          Type.freshError(Kind.Error, loc)
        case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
      }

    case UnkindedType.CaseUnion(t10, t20, loc) =>
      // Get the component types.
      val t1 = visitType(t10, Kind.WildCaseSet, kenv, taenv, root)
      val t2 = visitType(t20, Kind.WildCaseSet, kenv, taenv, root)

      val actualKind: Kind = unify(t1.kind, t2.kind) match {
        // Case 1: The kinds unify.
        case Some(k) => k
        // Case 2: The kinds do not unify. Error.
        case None =>
          sctx.errors.add(KindError.MismatchedKinds(t1.kind, t2.kind, loc))
          Kind.Error
      }

      unify(actualKind, expectedKind) match {
        case Some(Kind.CaseSet(enumSym)) => Type.mkCaseUnion(t1, t2, enumSym, loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc))
          Type.freshError(Kind.Error, loc)
        case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
      }


    case UnkindedType.CaseIntersection(t10, t20, loc) =>
      // Get the component types.
      val t1 = visitType(t10, Kind.WildCaseSet, kenv, taenv, root)
      val t2 = visitType(t20, Kind.WildCaseSet, kenv, taenv, root)

      val actualKind: Kind = unify(t1.kind, t2.kind) match {
        // Case 1: The kinds unify.
        case Some(k) => k
        // Case 2: The kinds do not unify. Error.
        case None =>
          sctx.errors.add(KindError.MismatchedKinds(t1.kind, t2.kind, loc))
          Kind.Error
      }

      unify(actualKind, expectedKind) match {
        case Some(Kind.CaseSet(enumSym)) => Type.mkCaseIntersection(t1, t2, enumSym, loc)
        case None =>
          sctx.errors.add(KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, loc))
          Type.freshError(Kind.Error, loc)
        case Some(_) => throw InternalCompilerException("unexpected failed kind unification", loc)
      }


    case UnkindedType.Error(loc) => Type.freshError(expectedKind, loc)

    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe0.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe0.loc)


  }

  /**
    * Performs kinding on the given effect, assuming it to be Pure if it is absent.
    */
  private def visitEffectDefaultPure(tpe: Option[UnkindedType], kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): Type = tpe match {
    case None => Type.mkPure(SourceLocation.Unknown)
    case Some(t) => visitType(t, Kind.Eff, kenv, taenv, root)
  }

  /**
    * Performs kinding on the given trait constraint under the given kind environment.
    */
  private def visitTraitConstraint(tconstr: ResolvedAst.TraitConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): TraitConstraint = tconstr match {
    case ResolvedAst.TraitConstraint(head, tpe0, loc) =>
      val traitKind = getTraitKind(root.traits(head.sym))
      val t = visitType(tpe0, traitKind, kenv, taenv, root)
      TraitConstraint(head, t, loc)
  }

  /**
    * Performs kinding on the given equality constraint under the given kind environment.
    */
  private def visitEqualityConstraint(econstr: ResolvedAst.EqualityConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): EqualityConstraint = econstr match {
    case ResolvedAst.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1, Kind.Wild, kenv, taenv, root)
      val t2 = visitType(tpe2, Kind.Wild, kenv, taenv, root)
      EqualityConstraint(cst, t1, t2, loc)
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def visitTypeParam(tparam: ResolvedAst.TypeParam, kenv: KindEnv)(implicit sctx: SharedContext): KindedAst.TypeParam = {
    val (name, sym0, loc) = tparam match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
      case ResolvedAst.TypeParam.Implicit(iName, iSym, iLoc) => (iName, iSym, iLoc)
    }
    val sym = visitTypeVarSym(sym0, Kind.Wild, kenv, loc)
    KindedAst.TypeParam(name, sym, loc)
  }

  /**
    * Performs kinding on the given index parameter of the given enum sym under the given kind environment.
    */
  private def visitIndex(index: ResolvedAst.TypeParam, `enum`: Symbol.RestrictableEnumSym, kenv: KindEnv)(implicit sctx: SharedContext): KindedAst.TypeParam = {
    val (name, sym0, loc) = index match {
      case ResolvedAst.TypeParam.Kinded(kName, kSym, _, kLoc) => (kName, kSym, kLoc)
      case ResolvedAst.TypeParam.Unkinded(uName, uSym, uLoc) => (uName, uSym, uLoc)
      case ResolvedAst.TypeParam.Implicit(iName, iSym, iLoc) => (iName, iSym, iLoc)
    }

    val sym = visitTypeVarSym(sym0, Kind.CaseSet(`enum`), kenv, loc)
    KindedAst.TypeParam(name, sym, loc)
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def visitFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindedAst.FormalParam = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      val (t, src) = tpe0 match {
        case None => (sym.tvar, TypeSource.Inferred)
        case Some(tpe) => (visitType(tpe, Kind.Star, kenv, taenv, root), TypeSource.Ascribed)
      }
      KindedAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Performs kinding on the given predicate param under the given kind environment.
    */
  private def visitPredicateParam(pparam0: ResolvedAst.PredicateParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.PredicateParam = pparam0 match {
    case ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      val t = Type.freshVar(Kind.Predicate, loc)
      KindedAst.PredicateParam(pred, t, loc)

    case ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      val ts = tpes.map(visitType(_, Kind.Star, kenv, taenv, root))
      val t = den match {
        case Denotation.Relational => Type.mkRelation(ts, pred.loc.asSynthetic)
        case Denotation.Latticenal => Type.mkLattice(ts, pred.loc.asSynthetic)
      }
      KindedAst.PredicateParam(pred, t, loc)
  }

  /**
    * Performs kinding on the given JVM method.
    */
  private def visitJvmMethod(method: ResolvedAst.JvmMethod, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit scope: Scope, sctx: SharedContext, flix: Flix): KindedAst.JvmMethod = method match {
    case ResolvedAst.JvmMethod(_, fparams0, exp0, tpe0, eff0, loc) =>
      val fparams = fparams0.map(visitFormalParam(_, kenv, taenv, root))
      val exp = visitExp(exp0, kenv, taenv, root)
      val eff = visitEffectDefaultPure(eff0, kenv, taenv, root)
      val tpe = visitType(tpe0, Kind.Wild, kenv, taenv, root)
      KindedAst.JvmMethod(method.ident, fparams, exp, tpe, eff, loc)
  }

  /**
    * Infers a kind environment from the given spec.
    * A KindEnvironment is provided in case some subset of of kinds have been declared (and therefore should not be inferred),
    * as in the case of a trait type parameter used in a sig or law.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindEnv = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, eff0, tconstrs, econstrs) =>
      val fparamKenvs = fparams.map(inferFormalParam(_, kenv, taenv, root))
      val tpeKenv = inferType(tpe, Kind.Star, kenv, taenv, root)
      val effKenvs = eff0.map(inferType(_, Kind.Eff, kenv, taenv, root)).toList
      val tconstrsKenvs = tconstrs.map(inferTraitConstraint(_, kenv, taenv, root))
      val econstrsKenvs = econstrs.map(inferEqualityConstraint(_, kenv, taenv, root))
      KindEnv.merge(fparamKenvs ::: tpeKenv :: effKenvs ::: tconstrsKenvs ::: econstrsKenvs)
  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFormalParam(fparam0: ResolvedAst.FormalParam, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindEnv = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => tpe0 match {
      case None => KindEnv.empty
      case Some(tpe) => inferType(tpe, Kind.Star, kenv, taenv, root)
    }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTraitConstraint(tconstr: ResolvedAst.TraitConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext): KindEnv = tconstr match {
    case ResolvedAst.TraitConstraint(head, tpe, _) =>
      val kind = getTraitKind(root.traits(head.sym))
      inferType(tpe, kind, kenv: KindEnv, taenv, root)
  }

  /**
    * Infers a kind environment from the given equality constraint.
    */
  private def inferEqualityConstraint(econstr: ResolvedAst.EqualityConstraint, kenv: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext): KindEnv = econstr match {
    case ResolvedAst.EqualityConstraint(AssocTypeSymUse(sym, _), tpe1, tpe2, _) =>
      val trt = root.traits(sym.trt)
      val kind1 = getTraitKind(trt)
      val kind2 = trt.assocs.find(_.sym == sym).get.kind
      val kenv1 = inferType(tpe1, kind1, kenv, taenv, root)
      val kenv2 = inferType(tpe2, kind2, kenv, taenv, root)
      kenv1 ++ kenv2
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    * The inference is roughly analogous to the inference of types for expressions.
    * The primary differences are:
    *   - There are no kind variables; kinds that cannot be determined are instead marked with [[Kind.Wild]].
    *   - Subkinding may allow a variable to be ascribed with two different kinds; the most specific is used in the returned environment.
    */
  private def inferType(tpe: UnkindedType, expectedKind: Kind, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext): KindEnv = tpe.baseType match {
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
      tpe.typeArguments.zip(args).foldLeft(KindEnv.singleton(tvar.sym -> tyconKind)) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.Cst(cst, _) =>
      val args = Kind.kindArgs(cst.kind)
      tpe.typeArguments.zip(args).foldLeft(KindEnv.empty) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.Ascribe(t, k, _) => inferType(t, k, kenv0, taenv, root)

    case UnkindedType.Alias(cst, args, _, _) =>
      val alias = taenv(cst.sym)
      val tparamKinds = alias.tparams.map(_.sym.kind)
      args.zip(tparamKinds).foldLeft(KindEnv.empty) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.AssocType(cst, arg, _) =>
      val trt = root.traits(cst.sym.trt)
      val kind = getTraitKind(trt)
      inferType(arg, kind, kenv0, taenv, root)

    case UnkindedType.Arrow(eff, _, _) =>
      val effKenvs = eff.map(inferType(_, Kind.Eff, kenv0, taenv, root)).toList
      val argKenv = tpe.typeArguments.foldLeft(KindEnv.empty) {
        case (acc, targ) => acc ++ inferType(targ, Kind.Star, kenv0, taenv, root)
      }
      KindEnv.merge(effKenvs :+ argKenv)

    case UnkindedType.Enum(sym, _) =>
      val tyconKind = getEnumKind(root.enums(sym))
      val args = Kind.kindArgs(tyconKind)
      tpe.typeArguments.zip(args).foldLeft(KindEnv.empty) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.Struct(sym, _) =>
      val tyconKind = getStructKind(root.structs(sym))
      val args = Kind.kindArgs(tyconKind)
      tpe.typeArguments.zip(args).foldLeft(KindEnv.empty) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.RestrictableEnum(sym, _) =>
      val tyconKind = getRestrictableEnumKind(root.restrictableEnums(sym))
      val args = Kind.kindArgs(tyconKind)
      tpe.typeArguments.zip(args).foldLeft(KindEnv.empty) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, kind, kenv0, taenv, root)
      }

    case UnkindedType.CaseSet(_, _) => KindEnv.empty

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

      val kenv1 = inferType(t1, expected, kenv0, taenv, root)
      val kenv2 = inferType(t2, expected, kenv0, taenv, root)
      kenv1 ++ kenv2

    case UnkindedType.CaseIntersection(t1, t2, _) =>
      // Expected kind for t1 and t2 is GenericCaseSet, but if we have a more specific kind we use that.
      val expected = unify(expectedKind, WildCaseSet) match {
        case Some(k) => k
        // This case will be an error in visitType
        case None => WildCaseSet
      }
      val kenv1 = inferType(t1, expected, kenv0, taenv, root)
      val kenv2 = inferType(t2, expected, kenv0, taenv, root)
      kenv1 ++ kenv2

    case UnkindedType.Error(_) => KindEnv.empty

    case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", tpe.loc)
    case _: UnkindedType.UnappliedAlias => throw InternalCompilerException("unexpected unapplied alias", tpe.loc)
    case _: UnkindedType.UnappliedAssocType => throw InternalCompilerException("unexpected unapplied associated type", tpe.loc)
  }

  /**
    * Gets a kind environment from the type params, defaulting to Star kind if they are unkinded.
    */
  private def getKindEnvFromTypeParams(tparams0: List[ResolvedAst.TypeParam]): KindEnv = {
    val kenvs = tparams0.map(getKindEnvFromTypeParam)
    KindEnv.disjointMerge(kenvs)
  }

  /**
    * Gets a kind environment from the type param, defaulting to Star kind if it is unkinded.
    */
  private def getKindEnvFromTypeParam(tparam0: ResolvedAst.TypeParam): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, sym, kind, _) => KindEnv.singleton(sym -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, sym, _) => KindEnv.singleton(sym -> Kind.Star)
    case ResolvedAst.TypeParam.Implicit(_, _, _) => KindEnv.empty
  }

  /**
    * Gets a kind environment from the type param, defaulting the to kind of the given enum's tags if it is unkinded.
    */
  private def getKindEnvFromIndex(index0: ResolvedAst.TypeParam, sym: Symbol.RestrictableEnumSym): KindEnv = index0 match {
    case ResolvedAst.TypeParam.Kinded(_, kSym, kind, _) => KindEnv.singleton(kSym -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, uSym, _) => KindEnv.singleton(uSym -> Kind.CaseSet(sym))
    case ResolvedAst.TypeParam.Implicit(_, _, _) => KindEnv.empty
  }

  /**
    * Gets a kind environment from the type param, defaulting to `Kind.Region` if it is unspecified
    */
  private def getKindEnvFromRegion(tparam0: ResolvedAst.TypeParam): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, sym, kind, _) => KindEnv.singleton(sym -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, sym, _) => KindEnv.singleton(sym -> Kind.Region)
    case ResolvedAst.TypeParam.Implicit(_, sym, _) => KindEnv.singleton(sym -> Kind.Region)
  }

  /**
    * Gets a kind environment from the spec.
    */
  private def getKindEnvFromSpec(spec0: ResolvedAst.Spec, kenv0: KindEnv, taenv: Map[Symbol.TypeAliasSym, KindedAst.TypeAlias], root: ResolvedAst.Root)(implicit sctx: SharedContext, flix: Flix): KindEnv = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _) =>
      // first get the kenv from the declared tparams
      val kenv1 = getKindEnvFromTypeParams(tparams0)

      // merge it from the kenv from the context
      val kenv2 = kenv0 ++ kenv1

      // Finally do inference on the spec under the new kenv
      inferSpec(spec0, kenv2, taenv, root)
  }

  /**
    * Gets the kind of the enum.
    */
  private def getEnumKind(enum0: ResolvedAst.Declaration.Enum): Kind = enum0 match {
    case ResolvedAst.Declaration.Enum(_, _, _, _, tparams, _, _, _) =>
      val kenv = getKindEnvFromTypeParams(tparams)
      tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the struct.
    */
  private def getStructKind(struct0: ResolvedAst.Declaration.Struct): Kind = struct0 match {
    case ResolvedAst.Declaration.Struct(_, _, _, _, tparams0, _, _) =>
      // tparams default to zero except for the region param
      val kenv1 = getKindEnvFromTypeParams(tparams0.init)
      val kenv2 = getKindEnvFromRegion(tparams0.last)
      val kenv = KindEnv.disjointAppend(kenv1, kenv2)
      tparams0.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.sym) ->: acc
      }
  }

  /**
    * Gets the kind of the restrictable enum.
    */
  private def getRestrictableEnumKind(enum0: ResolvedAst.Declaration.RestrictableEnum): Kind = enum0 match {
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
    case Type.Apply(Type.Cst(TypeConstructor.Difference, _), arg, _) => Type.mkDifference(arg, t2, loc)
    case Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), arg, _) => Type.mkSymmetricDiff(arg, t2, loc)
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
    def merge(kenvs: List[KindEnv])(implicit sctx: SharedContext): KindEnv = {
      kenvs.foldLeft(KindEnv.empty)(_ ++ _)
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
    def +(pair: (Symbol.UnkindedTypeVarSym, Kind))(implicit sctx: SharedContext): KindEnv = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) => unify(kind0, kind) match {
          case Some(minKind) => KindEnv(map + (tvar -> minKind))
          case None =>
            val e = KindError.MismatchedKinds(kind0, kind, tvar.loc)
            sctx.errors.add(e)
            KindEnv(map + (tvar -> kind0))
        }
        case None => KindEnv(map + (tvar -> kind))
      }
    }

    /**
      * Merges the given kind environment into this kind environment.
      */
    def ++(other: KindEnv)(implicit sctx: SharedContext): KindEnv = {
      other.map.foldLeft(this)(_ + _)
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

}
