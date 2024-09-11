/*
 *  Copyright 2019, 2023 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.CheckedCastType
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.language.errors.RedundancyError._
import ca.uwaterloo.flix.language.phase.unification.TraitEnvironment
import ca.uwaterloo.flix.util.{ParOps, Validation}

import java.util.concurrent.ConcurrentHashMap
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * The Redundancy phase checks that declarations and expressions within the AST are used in a meaningful way.
  *
  * For example, the redundancy phase ensures that there are no:
  *
  *   - unused local variables.
  *   - unused enums, definitions, ...
  *   - useless expressions.
  *
  * and so on.
  *
  * The phase performs no AST rewrites; it can be disabled without affecting the runtime semantics.
  */
object Redundancy {

  /**
    * Checks the given AST `root` for redundancies.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, RedundancyError] = flix.phase("Redundancy") {
    implicit val sctx: SharedContext = SharedContext.mk()

    val errorsFromDefs = ParOps.parAgg(root.defs, Used.empty)({
      case (acc, (_, decl)) => acc ++ visitDef(decl)(sctx, root, flix)
    }, _ ++ _).errors.toList

    val errorsFromSigs = ParOps.parAgg(root.sigs, Used.empty)({
      case (acc, (_, decl)) => acc ++ visitSig(decl)(sctx, root, flix)
    }, _ ++ _).errors.toList

    val errorsFromInst = ParOps.parAgg(TypedAstOps.instanceDefsOf(root), Used.empty)({
      case (acc, decl) => acc ++ visitDef(decl)(sctx, root, flix)
    }, _ ++ _).errors.toList

    // Check for unused symbols.
    val errors = {
      errorsFromDefs ++
        errorsFromInst ++
        errorsFromSigs ++
        checkUnusedDefs()(sctx, root) ++
        checkUnusedEffects()(sctx, root) ++
        checkUnusedEnumsAndTags()(sctx, root) ++
        checkUnusedTypeParamsEnums()(root) ++
        checkUnusedStructsAndFields()(sctx, root) ++
        checkUnusedTypeParamsStructs()(root) ++
        checkRedundantTraitConstraints()(root, flix)
    }

    // Determine whether to return success or soft failure.
    Validation.toSuccessOrSoftFailure(root, errors)
  }(DebugValidation())

  /**
    * Checks for unused definition symbols.
    */
  private def checkUnusedDefs()(implicit sctx: SharedContext, root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, defn) <- root.defs) {
      if (deadDef(defn)) {
        result += UnusedDefSym(defn.sym)
      }
    }
    result.toList
  }

  /**
    * Checks for unused effect symbols.
    */
  private def checkUnusedEffects()(implicit sctx: SharedContext, root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, eff) <- root.effects) {
      if (deadEffect(eff)) {
        result += UnusedEffectSym(eff.sym)
      }
    }
    result.toList
  }

  /**
    * Checks for unused enum symbols and tags.
    */
  private def checkUnusedEnumsAndTags()(implicit sctx: SharedContext, root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, enm) <- root.enums) {
      if (deadEnum(enm)) {
        result += UnusedEnumSym(enm.sym)
      }

      for ((tag, _) <- enm.cases) {
        if (deadTag(enm, tag)) {
          result += UnusedEnumTag(enm.sym, tag)
        }
      }
    }
    result.toList
  }

  /**
    * Checks for unused type parameters in enums.
    */
  private def checkUnusedTypeParamsEnums()(implicit root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, decl) <- root.enums) {
      val usedTypeVars = decl.cases.foldLeft(Set.empty[Symbol.KindedTypeVarSym]) {
        case (sacc, (_, Case(_, tpe, _, _))) => sacc ++ tpe.typeVars.map(_.sym)
      }
      val unusedTypeParams = decl.tparams.filter {
        tparam =>
          !usedTypeVars.contains(tparam.sym) &&
            !tparam.name.name.startsWith("_")
      }
      result ++= unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name, tparam.loc))
    }
    result.toList
  }

  /**
    * Checks for unused struct symbols and tags.
    */
  private def checkUnusedStructsAndFields()(implicit sctx: SharedContext, root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, struct) <- root.structs) {
      if (deadStruct(struct)) {
        result += UnusedStructSym(struct.sym)
      }
    }

    result.toList
  }

  /**
   * Checks for unused type parameters in structs.
   */
  private def checkUnusedTypeParamsStructs()(implicit root: Root): List[RedundancyError] = {
    val result = new ListBuffer[RedundancyError]
    for ((_, decl) <- root.structs) {
      val usedTypeVars = decl.fields.foldLeft(Set.empty[Symbol.KindedTypeVarSym]) {
        case (acc, (name, field)) =>
          acc ++ field.tpe.typeVars.map(_.sym)
      }
      val unusedTypeParams = decl.tparams.init.filter { // the last tparam is implicitly used for the region
        tparam =>
          !usedTypeVars.contains(tparam.sym) &&
            !tparam.name.name.startsWith("_")
      }
      result ++= unusedTypeParams.map(tparam => UnusedTypeParam(tparam.name, tparam.loc))
    }
    result.toList
  }

  /**
    * Checks for redundant type constraints in the given `root`.
    */
  private def checkRedundantTraitConstraints()(implicit root: Root, flix: Flix): List[RedundancyError] = {
    val defErrors = ParOps.parMap(root.defs.values)(defn => redundantTraitConstraints(defn.spec.declaredScheme.tconstrs))
    val classErrors = ParOps.parMap(root.traits.values)(trt => redundantTraitConstraints(trt.superTraits))
    val instErrors = ParOps.parMap(root.instances.values.flatten)(inst => redundantTraitConstraints(inst.tconstrs))
    val sigErrors = ParOps.parMap(root.sigs.values)(sig => redundantTraitConstraints(sig.spec.declaredScheme.tconstrs))

    (defErrors.flatten ++ classErrors.flatten ++ instErrors.flatten ++ sigErrors.flatten).toList
  }

  /**
    * Finds redundant trait constraints in `tconstrs`.
    */
  private def redundantTraitConstraints(tconstrs: List[Ast.TraitConstraint])(implicit root: Root, flix: Flix): List[RedundancyError] = {
    for {
      (tconstr1, i1) <- tconstrs.zipWithIndex
      (tconstr2, i2) <- tconstrs.zipWithIndex
      // don't compare a constraint against itself
      if i1 != i2 && TraitEnvironment.entails(tconstr1, tconstr2, root.traitEnv)
    } yield RedundancyError.RedundantTraitConstraint(tconstr1, tconstr2, tconstr2.loc)
  }

  /**
    * Checks for unused symbols in the given definition and returns all used symbols.
    */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, root: Root, flix: Flix): Used = {
    // Create fresh local context.
    implicit val lctx: LocalContext = LocalContext.mk()

    // Compute the used symbols inside the definition.
    val usedExp = visitExp(defn.exp, Env.empty ++ defn.spec.fparams.map(_.sym), RecursionContext.ofDef(defn.sym))

    val unusedFormalParams = findUnusedFormalParameters(defn.spec.fparams, usedExp)
    val unusedTypeParams = findUnusedTypeParameters(defn.spec)

    // Check for unused parameters and remove all variable symbols.
    val usedAll = (usedExp ++
      unusedFormalParams ++
      unusedTypeParams).copy(varSyms = Set.empty)

    // If the expression has no holes nor errors then we return usedAll.
    // Otherwise, we discard all unused variable errors.
    if (lctx.holeSyms.isEmpty && lctx.errorLocs.isEmpty)
      usedAll
    else
      usedAll.withoutUnusedVars
  }

  /**
    * Checks for unused symbols in the given signature and returns all used symbols.
    */
  private def visitSig(sig: Sig)(implicit sctx: SharedContext, root: Root, flix: Flix): Used = {
    // Create fresh local context.
    implicit val lctx: LocalContext = LocalContext.mk()

    // Compute the used symbols inside the signature.
    val usedExp = sig.exp match {
      case None => Used.empty
      case Some(exp) =>
        visitExp(exp, Env.empty ++ sig.spec.fparams.map(_.sym), RecursionContext.ofSig(sig.sym))
    }

    // Check for unused parameters and remove all variable symbols.
    val unusedFormalParams = sig.exp.toList.flatMap(_ => findUnusedFormalParameters(sig.spec.fparams, usedExp))
    val unusedTypeParams = findUnusedTypeParameters(sig.spec)

    val usedAll = (usedExp ++
      unusedFormalParams ++
      unusedTypeParams).copy(varSyms = Set.empty)

    // If the expression has no holes nor errors then we return usedAll.
    // Otherwise, we discard all unused variable errors.
    if (lctx.holeSyms.isEmpty && lctx.errorLocs.isEmpty)
      usedAll
    else
      usedAll.withoutUnusedVars
  }

  /**
    * Finds unused formal parameters.
    */
  private def findUnusedFormalParameters(fparams: List[FormalParam], used: Used): List[UnusedFormalParam] = {
    fparams.collect {
      case fparam if deadVarSym(fparam.sym, used) => UnusedFormalParam(fparam.sym)
    }
  }

  /**
    * Finds unused type parameters.
    */
  private def findUnusedTypeParameters(spec: Spec): List[UnusedTypeParam] = spec match {
    case Spec(_, _, _, tparams, fparams, _, tpe, eff, tconstrs, econstrs, _) =>
      val tpes = fparams.map(_.tpe) ::: tpe :: eff :: tconstrs.map(_.arg) ::: econstrs.map(_.tpe1) ::: econstrs.map(_.tpe2)
      val used = tpes.flatMap { t => t.typeVars.map(_.sym) }.toSet
      tparams.collect {
        case tparam if deadTypeVar(tparam.sym, used) => UnusedTypeParam(tparam.name, tparam.loc)
      }
  }


  /**
    * Returns the symbols used in the given expression `e0` under the given environment `env0`.
    */
  private def visitExp(e0: Expr, env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): Used = e0 match {
    case Expr.Cst(_, _, _) => Used.empty

    case Expr.Var(sym, _, loc) => (sym.isWild, rc.vars.contains(sym)) match {
      // Case 1: Non-wild, non-recursive use of sym.
      case (false, false) => Used.of(sym)
      // Case 2: Non-wild, recursive use of sym. Does not count as a use.
      case (false, true) => Used.empty
      // Case 3: Wild, non-recursive use of sym.
      case (true, false) => Used.empty + HiddenVarSym(sym, loc)
      // Case 4: Wild, recursive use of sym.
      case (true, true) => Used.empty + HiddenVarSym(sym, loc)
    }

    case Expr.Def(sym, _, _) =>
      // Recursive calls do not count as uses.
      if (!rc.defn.contains(sym)) {
        sctx.defSyms.put(sym, ())
        Used.empty
      } else
        Used.empty

    case Expr.Sig(sym, _, _) =>
      // Recursive calls do not count as uses.
      if (!rc.sig.contains(sym)) {
        sctx.sigSyms.put(sym, ())
        Used.empty
      } else
        Used.empty

    case Expr.Hole(sym, _, _) =>
      lctx.holeSyms += sym
      Used.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Use(_, alias, exp, _) =>
      // Check if the alias is shadowing
      val shadowedName = shadowing(alias.name, alias.loc, env0)

      // Add the name to the environment
      val env = env0 + (alias.name -> alias.loc)

      // Visit the expression with the extended environment
      val innerUsed = visitExp(exp, env, rc)

      // TODO NS-REFACTOR check for unused syms
      innerUsed ++ shadowedName

    case Expr.Lambda(fparam, exp, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + fparam.sym

      // Visit the expression with the extended environment.
      val innerUsed = visitExp(exp, env1, rc)

      // Check if the formal parameter is shadowing.
      val shadowedVar = shadowing(fparam.sym.text, fparam.sym.loc, env0)

      // Check if the lambda parameter symbol is dead.
      if (deadVarSym(fparam.sym, innerUsed))
        innerUsed ++ shadowedVar - fparam.sym + UnusedFormalParam(fparam.sym)
      else
        innerUsed ++ shadowedVar - fparam.sym

    case Expr.Apply(exp, exps, _, _, _) =>
      val us1 = visitExp(exp, env0, rc)
      val us2 = visitExps(exps, env0, rc)
      us1 ++ us2

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.Let(sym, _, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions one under the original environment and one under the extended environment.
      val innerUsed1 = visitExp(exp1, env0, rc)
      val innerUsed2 = visitExp(exp2, env1, rc)

      // Check for shadowing.
      val shadowedVar = shadowing(sym.text, sym.loc, env0)

      // Check if the let-bound variable symbol is dead in exp2.
      if (deadVarSym(sym, innerUsed2))
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed1 ++ innerUsed2 ++ shadowedVar) - sym

    case Expr.LetRec(sym, _, _, exp1, exp2, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the two expressions under the extended environment.
      // Add the variable to the recursion context only in the first expression.
      val innerUsed1 = visitExp(exp1, env1, rc.withVar(sym))
      val innerUsed2 = visitExp(exp2, env1, rc)
      val used = innerUsed1 ++ innerUsed2

      // Check for shadowing.
      val shadowedVar = shadowing(sym.text, sym.loc, env0)

      // Check if the let-bound variable symbol is dead in exp1 + exp2.
      if (deadVarSym(sym, used))
        (used ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (used ++ shadowedVar) - sym

    case Expr.Region(_, _) =>
      Used.empty

    case Expr.Scope(sym, _, exp, _, _, _) =>
      // Extend the environment with the variable symbol.
      val env1 = env0 + sym

      // Visit the expression under the extended environment.
      val innerUsed = visitExp(exp, env1, rc)

      // Check for shadowing.
      val shadowedVar = shadowing(sym.text, sym.loc, env0)

      // Check if the let-bound variable symbol is dead in exp.
      if (deadVarSym(sym, innerUsed))
        (innerUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
      else
        (innerUsed ++ shadowedVar) - sym

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      val us3 = visitExp(exp3, env0, rc)
      us1 ++ us2 ++ us3

    case Expr.Stm(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)

      // Check for useless pure expressions.
      if (isUnderAppliedFunction(exp1)) {
        // `isUnderAppliedFunction` implies `isUselessExpression` so this must be checked first.
        (us1 ++ us2) + UnderAppliedFunction(exp1.tpe, exp1.loc)
      } else if (isUselessExpression(exp1)) {
        (us1 ++ us2) + UselessExpression(exp1.tpe, exp1.loc)
      } else if (isMustUse(exp1)(root) && !isHole(exp1)) {
        (us1 ++ us2) + UnusedMustUseValue(exp1.tpe, exp1.loc)
      } else {
        us1 ++ us2
      }

    case Expr.Discard(exp, _, _) =>
      val us = visitExp(exp, env0, rc)

      if (isPure(exp))
        us + DiscardedPureValue(exp.loc)
      else if (exp.tpe == Type.Unit)
        us + RedundantDiscard(exp.loc)
      else
        us

    case Expr.Match(exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0, rc)

      // Visit each match rule.
      val usedRules = rules map {
        case MatchRule(pat, guard, body) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the pattern, guard and body.
          val usedPat = visitPat(pat)
          val usedGuard = guard.map(visitExp(_, extendedEnv, rc)).getOrElse(Used.empty)
          val usedBody = visitExp(body, extendedEnv, rc)
          val usedPatGuardAndBody = usedPat ++ usedGuard ++ usedBody

          // Check for unused variable symbols.
          val unusedVarSyms = findUnusedVarSyms(fvs, usedPatGuardAndBody)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = findShadowedVarSyms(fvs, env0)

          // Combine everything together.
          (usedPatGuardAndBody -- fvs) ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedMatch ++ usedRules.reduceLeft(_ ++ _)

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0, rc)

      // Visit each match rule.
      val usedRules = rules map {
        case TypeMatchRule(sym, _, body) =>
          // Get the free var from the sym
          val fvs = Set(sym)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the pattern, guard and body.
          val usedBody = visitExp(body, extendedEnv, rc)

          // Check for unused variable symbols.
          val unusedVarSyms = findUnusedVarSyms(fvs, usedBody)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = findShadowedVarSyms(fvs, env0)

          // Combine everything together.
          (usedBody -- fvs) ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedMatch ++ usedRules.reduceLeft(_ ++ _)

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      // Visit the match expression.
      val usedMatch = visitExp(exp, env0, rc)

      // Visit each match rule.
      val usedRules = rules map {
        case RestrictableChooseRule(pat, body) =>
          // Compute the free variables in the pattern.
          val fvs = freeVars(pat)

          // Extend the environment with the free variables.
          val extendedEnv = env0 ++ fvs

          // Visit the pattern, guard and body.
          val usedPat = Used.empty
          val usedBody = visitExp(body, extendedEnv, rc)
          val usedPatAndBody = usedPat ++ usedBody

          // Check for unused variable symbols.
          val unusedVarSyms = findUnusedVarSyms(fvs, usedPatAndBody)

          // Check for shadowed variable symbols.
          val shadowedVarSyms = findShadowedVarSyms(fvs, env0)

          // Combine everything together.
          (usedPatAndBody -- fvs) ++ unusedVarSyms ++ shadowedVarSyms
      }

      usedMatch ++ usedRules.reduceLeft(_ ++ _)


    case Expr.Tag(Ast.CaseSymUse(sym, _), exp, _, _, _) =>
      val us = visitExp(exp, env0, rc)
      sctx.enumSyms.put(sym.enumSym, ())
      sctx.caseSyms.put(sym, ())
      us

    case Expr.RestrictableTag(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Tuple(elms, _, _, _) =>
      visitExps(elms, env0, rc)

    case Expr.RecordEmpty(_, _) =>
      Used.empty

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      val us1 = visitExp(value, env0, rc)
      val us2 = visitExp(rest, env0, rc)
      us1 ++ us2

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest, env0, rc)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps, env0, rc) ++ visitExp(exp, env0, rc)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      val us3 = visitExp(exp3, env0, rc)
      us1 ++ us2 ++ us3

    case Expr.ArrayLoad(base, index, _, _, _) =>
      val us1 = visitExp(base, env0, rc)
      val us2 = visitExp(index, env0, rc)
      us1 ++ us2

    case Expr.ArrayLength(base, _, _) =>
      visitExp(base, env0, rc)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      val us1 = visitExp(base, env0, rc)
      val us2 = visitExp(index, env0, rc)
      val us3 = visitExp(elm, env0, rc)
      us1 ++ us2 ++ us3

    case Expr.StructNew(sym, fields, region, _, _, _) =>
      sctx.structSyms.put(sym, ())
      visitExps(fields.map {case (k, v) => v}, env0, rc) ++ visitExp(region, env0, rc)

    case Expr.StructGet(e, field, _, _, _) =>
      sctx.structFieldSyms.put(field, ())
      visitExp(e, env0, rc)

    case Expr.StructPut(e1, field, e2, _, _, _) =>
      sctx.structFieldSyms.put(field, ())
      visitExp(e1, env0, rc) ++ visitExp(e2, env0, rc)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps, env0, rc)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.VectorLength(exp, _) =>
      visitExp(exp, env0, rc)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      cast match {
        case CheckedCastType.TypeCast =>
          if (exp.tpe == tpe)
            visitExp(exp, env0, rc) + RedundantCheckedTypeCast(loc)
          else
            visitExp(exp, env0, rc)
        case CheckedCastType.EffectCast =>
          if (exp.eff == eff)
            visitExp(exp, env0, rc) + RedundantCheckedEffectCast(loc)
          else
            visitExp(exp, env0, rc)
      }

    case Expr.UncheckedCast(exp, _, declaredEff, _, _, loc) =>
      declaredEff match {
        // Don't capture redundant purity casts if there's also a set effect
        case Some(eff) =>
          (eff, exp.eff) match {
            case (Type.Pure, Type.Pure) =>
              visitExp(exp, env0, rc) + RedundantUncheckedEffectCast(loc)
            case (Type.Var(eff1, _), Type.Var(eff2, _))
              if eff1 == eff2 =>
              visitExp(exp, env0, rc) + RedundantUncheckedEffectCast(loc)
            case _ => visitExp(exp, env0, rc)
          }
        case _ => visitExp(exp, env0, rc)
      }

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Without(exp, effUse, _, _, _) =>
      sctx.effSyms.put(effUse.sym, ())
      visitExp(exp, env0, rc)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      val usedExp = visitExp(exp, env0, rc)
      val usedRules = rules.foldLeft(Used.empty) {
        case (acc, CatchRule(sym, _, body)) =>
          val usedBody = visitExp(body, env0, rc)
          if (deadVarSym(sym, usedBody))
            acc ++ usedBody + UnusedVarSym(sym)
          else
            acc ++ usedBody
      }
      usedExp ++ usedRules

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.TryWith(exp, effUse, rules, _, _, _) =>
      sctx.effSyms.put(effUse.sym, ())
      val usedExp = visitExp(exp, env0, rc)
      val usedRules = rules.foldLeft(Used.empty) {
        case (acc, HandlerRule(_, fparams, body)) =>
          val usedBody = visitExp(body, env0, rc)
          val syms = fparams.map(_.sym)
          val dead = syms.filter(deadVarSym(_, usedBody))
          acc ++ usedBody ++ dead.map(UnusedVarSym.apply)
      }
      usedExp ++ usedRules

    case Expr.Do(opUse, exps, _, _, _) =>
      sctx.effSyms.put(opUse.sym.eff, ())
      visitExps(exps, env0, rc)

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args, env0, rc)

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp, env0, rc) ++ visitExps(args, env0, rc)

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args, env0, rc)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1, env0, rc) ++ visitExp(exp2, env0, rc)

    case Expr.GetStaticField(_, _, _, _) =>
      Used.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Used.empty) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) =>
          // Extend the environment with the formal parameter symbols
          val env1 = env0 ++ fparams.map(_.sym)
          val used = visitExp(exp, env1, rc)
          val unusedFParams = findUnusedFormalParameters(fparams, used)
          acc ++ used ++ unusedFParams
      }

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.SelectChannel(rules, defaultOpt, _, _, _) =>
      val defaultUsed = defaultOpt match {
        case None => Used.empty
        case Some(default) => visitExp(default, env0, rc)
      }

      val rulesUsed = rules map {
        case SelectChannelRule(sym, chan, body) =>
          // Extend the environment with the symbol.
          val env1 = env0 + sym

          // Check for shadowing.
          val shadowedVar = shadowing(sym.text, sym.loc, env0)

          // Visit the channel and body expressions.
          val chanUsed = visitExp(chan, env1, rc)
          val bodyUsed = visitExp(body, env1, rc)

          // Check if the variable symbol is dead in the body.
          if (deadVarSym(sym, bodyUsed))
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym + UnusedVarSym(sym)
          else
            (chanUsed ++ bodyUsed ++ shadowedVar) - sym
      }

      rulesUsed.foldLeft(defaultUsed) {
        case (acc, used) => acc ++ used
      }

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.ParYield(frags, exp, _, _, _) =>
      val (used, env1, fvs) = visitParYieldFragments(frags, env0, rc)
      val usedYield = visitExp(exp, env1, rc)
      val unusedVarSyms = findUnusedVarSyms(fvs, usedYield)
      (usedYield -- fvs) ++ unusedVarSyms ++ used

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Force(exp, _, _, _) => visitExp(exp, env0, rc)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Used.empty) {
        case (used, con) => used ++ visitConstraint(con, env0, rc: RecursionContext)
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      val us1 = visitExp(exp1, env0, rc)
      val us2 = visitExp(exp2, env0, rc)
      us1 ++ us2

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp, env0, rc)

    case Expr.Error(_, _, _) =>
      lctx.errorLocs += e0.loc
      Used.empty

  }

  /**
    * Visits the [[ParYieldFragment]]s `frags`.
    *
    * Returns a tuple of three entries:
    *   1. The used variables
    *   1. An updated environment with the free variables
    *   1. All the free variables.
    */
  private def visitParYieldFragments(frags: List[ParYieldFragment], env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): (Used, Env, Set[Symbol.VarSym]) = {
    frags.foldLeft((Used.empty, env0, Set.empty[Symbol.VarSym])) {
      case ((usedAcc, envAcc, fvsAcc), ParYieldFragment(p, e, _)) =>
        // Find free vars in pattern
        val fvs = freeVars(p)

        // Check that the free vars don't shadow any previous par yield vars or anything else
        val shadowedVars = findShadowedVarSyms(fvs, envAcc)

        // Extend env
        val extendedEnv = envAcc ++ fvs

        // Visit pattern
        val usedPat = visitPat(p)

        // Visit exp under env0 since each exp should be independent
        val usedExp = visitExp(e, env0, rc)

        // Combine everything
        val allUsed = usedAcc ++ usedPat ++ usedExp ++ shadowedVars

        (allUsed, extendedEnv, fvsAcc ++ fvs)
    }
  }

  /**
    * Returns the symbols that the free variables shadow in env0.
    */
  private def findShadowedVarSyms(freeVars: Set[Symbol.VarSym], env0: Env): Used = {
    freeVars.map(sym => shadowing(sym.text, sym.loc, env0)).foldLeft(Used.empty)(_ ++ _)
  }

  /**
    * Returns the set of unused vars from `freeVars` in `usedSyms`.
    * I.e. if an element in `freeVars` is **not** used in `usedSyms` then
    * it is a member of the returned set.
    */
  private def findUnusedVarSyms(freeVars: Set[Symbol.VarSym], usedSyms: Used): Set[UnusedVarSym] = {
    freeVars.filter(sym => deadVarSym(sym, usedSyms)).map(UnusedVarSym.apply)
  }

  /**
    * Returns the symbols used in the given list of expressions `es` under the given environment `env0`.
    */
  private def visitExps(es: List[Expr], env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): Used =
    es.foldLeft(Used.empty) {
      case (acc, exp) => acc ++ visitExp(exp, env0, rc)
    }

  /**
    * Returns the symbols used in the given pattern `pat`.
    */
  private def visitPat(pat0: Pattern)(implicit sctx: SharedContext): Used = pat0 match {
    case Pattern.Wild(_, _) => Used.empty
    case Pattern.Var(_, _, _) => Used.empty
    case Pattern.Cst(_, _, _) => Used.empty
    case Pattern.Tag(Ast.CaseSymUse(sym, _), _, _, _) =>
      sctx.enumSyms.put(sym.enumSym, ())
      sctx.caseSyms.put(sym, ())
      Used.empty
    case Pattern.Tuple(elms, _, _) => visitPats(elms)
    case Pattern.Record(pats, pat, _, _) =>
      visitPats(pats.map(_.pat)) ++ visitPat(pat)
    case Pattern.RecordEmpty(_, _) => Used.empty
    case Pattern.Error(_, _) => Used.empty
  }

  /**
    * Returns the symbols used in the given list of pattern `ps`.
    */
  private def visitPats(ps: List[Pattern])(implicit sctx: SharedContext): Used = ps.foldLeft(Used.empty) {
    case (acc, pat) => acc ++ visitPat(pat)
  }

  /**
    * Returns the symbols used in the given constraint `c0` under the given environment `env0`.
    */
  private def visitConstraint(c0: Constraint, env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): Used = {
    val head = visitHeadPred(c0.head, env0, rc: RecursionContext)
    val body = c0.body.foldLeft(Used.empty) {
      case (acc, b) => acc ++ visitBodyPred(b, env0, rc: RecursionContext)
    }
    val total = head ++ body

    total -- c0.cparams.map(_.sym)
  }

  /**
    * Returns the symbols used in the given head predicate `h0` under the given environment `env0`.
    */
  private def visitHeadPred(h0: Predicate.Head, env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): Used = h0 match {
    case Head.Atom(_, _, terms, _, _) =>
      visitExps(terms, env0, rc)
  }

  /**
    * Returns the symbols used in the given body predicate `h0` under the given environment `env0`.
    */
  private def visitBodyPred(b0: Predicate.Body, env0: Env, rc: RecursionContext)(implicit lctx: LocalContext, sctx: SharedContext, root: Root, flix: Flix): Used = b0 match {
    case Body.Atom(_, _, _, _, terms, _, _) =>
      terms.foldLeft(Used.empty) {
        case (acc, term) => acc ++ Used.of(freeVars(term))
      }

    case Body.Functional(outVars, exp, _) =>
      outVars.foldLeft(visitExp(exp, env0, rc: RecursionContext)) {
        case (acc, varSym) => acc ++ Used.of(varSym)
      }

    case Body.Guard(exp, _) =>
      visitExp(exp, env0, rc)

  }

  /**
    * Returns true if the expression is pure and of impure function type.
    */
  private def isUnderAppliedFunction(exp: Expr): Boolean = {
    val isPure = exp.eff == Type.Pure
    val isNonPureFunction = exp.tpe.typeConstructor match {
      case Some(TypeConstructor.Arrow(_)) =>
        curriedArrowPurityType(exp.tpe) != Type.Pure
      case _ => false
    }
    isPure && isNonPureFunction
  }

  /**
    * Returns the purity type of `this` curried arrow type.
    *
    * For example,
    *
    * {{{
    * Int32                                        =>     throw
    * Int32 -> String -> Int32 \ Pure              =>     Pure
    * (Int32, String) -> String -> Bool \ IO       =>     IO
    * }}}
    *
    * NB: Assumes that `this` type is an arrow.
    */
  @tailrec
  private def curriedArrowPurityType(tpe: Type): Type = {
    val resType = tpe.arrowResultType
    resType.typeConstructor match {
      case Some(TypeConstructor.Arrow(_)) => curriedArrowPurityType(resType)
      case _ => tpe.arrowEffectType
    }
  }

  /**
    * Returns true if the expression is pure.
    */
  private def isPure(exp: Expr): Boolean =
    exp.eff == Type.Pure

  /**
    * Returns true if the expression is pure.
    */
  private def isUselessExpression(exp: Expr): Boolean =
    isPure(exp) && !exp.isInstanceOf[Expr.UncheckedMaskingCast]

  /**
    * Returns `true` if the expression must be used.
    */
  private def isMustUse(exp: Expr)(implicit root: Root): Boolean =
    isMustUseType(exp.tpe) && !exp.isInstanceOf[Expr.UncheckedMaskingCast]

  /**
    * Returns `true` if the given type `tpe` is marked as `@MustUse` or is intrinsically `@MustUse`.
    */
  private def isMustUseType(tpe: Type)(implicit root: Root): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => true
    case Some(TypeConstructor.Enum(sym, _)) => root.enums(sym).ann.isMustUse
    case _ => false
  }

  /**
    * Returns true if the expression is a hole.
    */
  private def isHole(exp: Expr): Boolean = exp match {
    case Expr.Hole(_, _, _) => true
    case Expr.HoleWithExp(_, _, _, _) => true
    case _ => false
  }

  /**
    * Returns the free variables in the pattern `p0`.
    */
  private def freeVars(p0: Pattern): Set[Symbol.VarSym] = p0 match {
    case Pattern.Wild(_, _) => Set.empty
    case Pattern.Var(sym, _, _) => Set(sym)
    case Pattern.Cst(_, _, _) => Set.empty
    case Pattern.Tag(_, pat, _, _) => freeVars(pat)
    case Pattern.Tuple(pats, _, _) => pats.foldLeft(Set.empty[Symbol.VarSym]) {
      case (acc, pat) => acc ++ freeVars(pat)
    }
    case Pattern.Record(pats, pat, _, _) =>
      val patsVal = pats.foldLeft(Set.empty[Symbol.VarSym]) {
        case (acc, rfp) => acc ++ freeVars(rfp.pat)
      }
      val patVal = freeVars(pat)
      patsVal ++ patVal
    case Pattern.RecordEmpty(_, _) => Set.empty
    case Pattern.Error(_, _) => Set.empty
  }

  /**
    * Returns the free variables in the restrictable pattern `p`.
    */
  private def freeVars(p: RestrictableChoosePattern): Set[Symbol.VarSym] = p match {
    case RestrictableChoosePattern.Tag(_, pat, _, _) => pat.flatMap(freeVars).toSet
  }

  /**
    * Returns the free variables in the VarOrWild.
    */
  private def freeVars(v: RestrictableChoosePattern.VarOrWild): Option[Symbol.VarSym] = v match {
    case RestrictableChoosePattern.Wild(_, _) => None
    case RestrictableChoosePattern.Var(sym, _, _) => Some(sym)
  }

  /**
    * Checks whether the variable symbol `sym` shadows another variable in the environment `env`.
    */
  private def shadowing(name: String, loc: SourceLocation, env: Env): Used =
    env.names.get(name) match {
      case None =>
        Used.empty
      case Some(shadowed) =>
        if (Name.isWild(name))
          Used.empty
        else
          Used.empty +
            ShadowedName(name, shadowing = loc, shadowed = shadowed) +
            ShadowingName(name, shadowing = loc, shadowed = shadowed)
    }

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def deadDef(decl: Def)(implicit sctx: SharedContext, root: Root): Boolean =
    !decl.spec.ann.isTest &&
      !decl.spec.mod.isPublic &&
      !decl.spec.ann.isExport &&
      !isMain(decl.sym) &&
      !decl.sym.name.startsWith("_") &&
      !sctx.defSyms.containsKey(decl.sym)

  /**
    * Returns `true` if the given symbol `sym` either is `main` or is an entry point.
    */
  private def isMain(sym: Symbol.DefnSym)(implicit root: Root): Boolean =
    sym.toString == "main" || root.entryPoint.contains(sym)

  /**
    * Returns `true` if the given definition `decl` is unused according to `used`.
    */
  private def deadEffect(decl: Effect)(implicit ctx: SharedContext): Boolean =
    !decl.mod.isPublic &&
      !decl.sym.name.startsWith("_") &&
      !ctx.effSyms.containsKey(decl.sym)

  /**
    * Returns `true` if the given `enm` is unused according to `used`.
    */
  private def deadEnum(enm: Enum)(implicit sctx: SharedContext): Boolean =
    !enm.sym.name.startsWith("_") &&
      !enm.mod.isPublic &&
      !sctx.enumSyms.containsKey(enm.sym)

  /**
    * Returns `true` if the given `struct` is unused according to `used`.
    */
  private def deadStruct(struct: Struct)(implicit sctx: SharedContext): Boolean =
    !struct.sym.name.startsWith("_") &&
      !struct.mod.isPublic &&
      !sctx.structSyms.containsKey(struct.sym)

  /**
    * Returns `true` if the given `tag` of the given `enm` is unused according to `used`.
    */
  private def deadTag(enm: Enum, tag: Symbol.CaseSym)(implicit ctx: SharedContext): Boolean =
    !enm.sym.name.startsWith("_") &&
      !enm.mod.isPublic &&
      !tag.name.startsWith("_") &&
      !ctx.caseSyms.containsKey(tag)

  /**
    * Returns `true` if the type variable `tvar` is unused according to the argument `used`.
    */
  private def deadTypeVar(tvar: Symbol.KindedTypeVarSym, used: Set[Symbol.KindedTypeVarSym]): Boolean = {
    !tvar.isWild &&
      !used.contains(tvar)
  }

  /**
    * Returns `true` if the local variable `tvar` is unused according to the argument `used`.
    */
  private def deadVarSym(sym: Symbol.VarSym, used: Used): Boolean =
    !sym.text.startsWith("_") &&
      !used.varSyms.contains(sym)

  /**
    * Companion object for the [[Env]] class.
    */
  private object Env {
    /**
      * Represents the empty environment.
      */
    val empty: Env = Env(Map.empty)

    /**
      * Returns an environment with the given variable symbols `varSyms` in it.
      */
    def of(varSyms: Iterable[Symbol.VarSym]): Env = varSyms.foldLeft(Env.empty) {
      case (acc, sym) => acc + sym
    }
  }

  /**
    * Represents a name environment.
    */
  private case class Env(names: Map[String, SourceLocation]) {
    /**
      * Updates `this` environment with a new variable symbol `sym`.
      */
    def +(sym: Symbol.VarSym): Env = {
      copy(names = names + (sym.text -> sym.loc))
    }

    /**
      * Updates `this` environment with a set of new variable symbols `varSyms`.
      */
    def ++(vs: Iterable[Symbol.VarSym]): Env = vs.foldLeft(this) {
      case (acc, sym) => acc + sym
    }

    /**
      * Updates `this` environment with a new `name`.
      */
    def +(nameAndLoc: (String, SourceLocation)): Env = {
      copy(names = names + nameAndLoc)
    }
  }

  /**
    * Companion object for the [[Used]] class.
    */
  private object Used {

    /**
      * Represents the empty set of used symbols.
      */
    val empty: Used = Used(Set.empty, Set.empty)

    /**
      * Returns an object where the given variable symbol `sym` is marked as used.
      */
    def of(sym: Symbol.VarSym): Used = empty.copy(varSyms = Set(sym))

    /**
      * Returns an object where the given variable symbols `syms` are marked as used.
      */
    def of(syms: Set[Symbol.VarSym]): Used = empty.copy(varSyms = syms)

  }

  /**
    * A representation of used symbols.
    */
  private case class Used(varSyms: Set[Symbol.VarSym], errors: Set[RedundancyError]) {

    /**
      * Merges `this` and `that` where one of the two branches is executed
      */
    def ++(that: Used): Used =
      if (this eq that) {
        this
      } else if (this eq Used.empty) {
        that
      } else if (that eq Used.empty) {
        this
      } else {
        Used(
          this.varSyms ++ that.varSyms,
          this.errors ++ that.errors
        )
      }

    /**
      * Adds the given redundancy error `e` to `this` object.
      */
    def +(e: RedundancyError): Used = copy(errors = errors + e)

    /**
      * Adds the given traversable of redundancy errors `es` to `this` object.
      */
    def ++(es: Iterable[RedundancyError]): Used =
      if (es.isEmpty) this else copy(errors = errors ++ es)

    /**
      * Marks the given variable symbol `sym` as used.
      */
    def -(sym: Symbol.VarSym): Used = copy(varSyms = varSyms - sym)

    /**
      * Marks all the given variable symbols `syms` as used.
      */
    def --(syms: Iterable[Symbol.VarSym]): Used =
      if (syms.isEmpty)
        this
      else
        copy(varSyms = varSyms -- syms)

    /**
      * Returns `this` without any unused variable errors.
      */
    def withoutUnusedVars: Used = copy(errors = errors.filter {
      case _: RedundancyError.UnusedFormalParam => false
      case _: RedundancyError.UnusedVarSym => false
      case _ => true
    })

  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {
    def mk(): LocalContext = new LocalContext(mutable.Set.empty, mutable.Set.empty)
  }

  /**
    * A local non-shared context. Does not need to be thread-safe.
    *
    * @param holeSyms  the hole symbols in the def or sig.
    * @param errorLocs the source locations of the error expressions.
    */
  private case class LocalContext(holeSyms: mutable.Set[Symbol.HoleSym], errorLocs: mutable.Set[SourceLocation])

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {
    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(
      new ConcurrentHashMap(),
      new ConcurrentHashMap(),
      new ConcurrentHashMap(),
      new ConcurrentHashMap(),
      new ConcurrentHashMap(),
      new ConcurrentHashMap(),
      new ConcurrentHashMap())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param defSyms  the def symbols used anywhere in the program.
    * @param sigSyms  the sig symbols used anywhere in the program.
    * @param effSyms  the eff symbols used anywhere in the program.
    * @param enumSyms the enum symbols used anywhere in the program.
    * @param caseSyms the case symbols used anywhere in the program.
    */
  private case class SharedContext(defSyms: ConcurrentHashMap[Symbol.DefnSym, Unit],
                                   sigSyms: ConcurrentHashMap[Symbol.SigSym, Unit],
                                   effSyms: ConcurrentHashMap[Symbol.EffectSym, Unit],
                                   structSyms: ConcurrentHashMap[Symbol.StructSym, Unit],
                                   structFieldSyms: ConcurrentHashMap[Symbol.StructFieldSym, Unit],
                                   enumSyms: ConcurrentHashMap[Symbol.EnumSym, Unit],
                                   caseSyms: ConcurrentHashMap[Symbol.CaseSym, Unit])

  /**
    * Companion object for [[RecursionContext]].
    */
  private object RecursionContext {
    /**
      * Initializes a context under the given definition.
      */
    def ofDef(defn: Symbol.DefnSym): RecursionContext = RecursionContext(defn = Some(defn), sig = None, vars = Set.empty)

    /**
      * Initializes a context under the given signature.
      */
    def ofSig(sig: Symbol.SigSym): RecursionContext = RecursionContext(defn = None, sig = Some(sig), vars = Set.empty)
  }

  /**
    * Tracks the context of the explored expression, recalling the definition, signature,
    * or recursive variable under which it is defined.
    */
  private case class RecursionContext(defn: Option[Symbol.DefnSym], sig: Option[Symbol.SigSym], vars: Set[Symbol.VarSym]) {
    /**
      * Adds the given variable to the context.
      */
    def withVar(v: Symbol.VarSym): RecursionContext = this.copy(vars = this.vars + v)
  }
}
