/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.MonoAst.Expr
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, OccurrenceAst1, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{CofiniteEffSet, InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The inliner optionally performs beta-reduction at call-sites.
  * TODO: Improve this documentation
  */
object Inliner1 {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): (MonoAst.Root, Stats) = {
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(root, sctx, flix))
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val newRoot = MonoAst.Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
    val stats = Stats.from(sctx)
    (newRoot, stats)
  }

  private type InVar = Symbol.VarSym

  private type OutVar = Symbol.VarSym

  private type InExpr = OccurrenceAst1.Expr

  private type OutExpr = MonoAst.Expr

  private type InCase = OccurrenceAst1.Case

  private type OutCase = MonoAst.Case

  sealed private trait SubstRange

  private object SubstRange {

    case class SuspendedExp(exp: InExpr) extends SubstRange

    case class DoneExp(exp: OutExpr) extends SubstRange

  }

  private sealed trait Const

  private object Const {

    case class Lit(lit: Constant) extends Const

    case class Tag(tag: Symbol.CaseSym) extends Const

  }

  private sealed trait Definition

  private object Definition {

    case object CaseOrLambda extends Definition

    case class LetBound(expr: OutExpr, occur: Occur) extends Definition


  }

  private type VarSubst = Map[InVar, OutVar]

  private type Subst = Map[InVar, SubstRange]

  private type InScopeSet = Map[OutVar, Definition]

  private sealed trait Context

  private object Context {

    case object Start extends Context

    case object Stop extends Context

    case class Application(inExpr: InExpr, subst: Subst, context: Context) extends Context

    case class Argument(continuation: OutExpr => OutExpr) extends Context

    case class Match(sym: InVar, cases: List[InCase], subst: Subst, context: Context) extends Context

  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst1]] to [[MonoAst]].
    */
  private def visitDef(def0: OccurrenceAst1.Def)(implicit root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): MonoAst.Def = def0 match {
    case OccurrenceAst1.Def(sym, fparams, spec, exp, ctx, loc) =>
      if (ctx.occur != Dangerous) {
        val e = visitExp(exp, Map.empty, Map.empty, Map.empty, Context.Start)(sym, root, sctx, flix)
        val sp = visitSpec(spec, fparams.map { case (fp, _) => fp })
        MonoAst.Def(sym, sp, e, loc)
      } else {
        val e = toMonoAstExpr(exp)
        val sp = visitSpec(spec, fparams.map { case (fp, _) => fp })
        MonoAst.Def(sym, sp, e, loc)
      }
  }

  private def visitSpec(spec0: OccurrenceAst1.Spec, fparams0: List[OccurrenceAst1.FormalParam]): MonoAst.Spec = spec0 match {
    case OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff) =>
      val fps = fparams0.map(visitFormalParam)
      MonoAst.Spec(doc, ann, mod, fps, functionType, retTpe, eff)
  }

  private def visitEnum(enum0: OccurrenceAst1.Enum): MonoAst.Enum = enum0 match {
    case OccurrenceAst1.Enum(doc, ann, mod, sym, tparams, cases, loc) =>
      val tps = tparams.map(visitTypeParam)
      val cs = cases.map { case (sym, caze) => sym -> visitEnumCase(caze) }
      MonoAst.Enum(doc, ann, mod, sym, tps, cs, loc)
  }

  private def visitEnumCase(case0: OccurrenceAst1.Case): MonoAst.Case = case0 match {
    case OccurrenceAst1.Case(sym, tpe, loc) => MonoAst.Case(sym, tpe, loc)
  }

  private def visitStruct(struct0: OccurrenceAst1.Struct): MonoAst.Struct = struct0 match {
    case OccurrenceAst1.Struct(doc, ann, mod, sym, tparams, fields, loc) =>
      val tps = tparams.map(visitTypeParam)
      val fs = fields.map(visitStructField)
      MonoAst.Struct(doc, ann, mod, sym, tps, fs, loc)
  }

  private def visitStructField(field0: OccurrenceAst1.StructField): MonoAst.StructField = field0 match {
    case OccurrenceAst1.StructField(sym, tpe, loc) =>
      MonoAst.StructField(sym, tpe, loc)
  }

  private def visitEffect(effect0: OccurrenceAst1.Effect): MonoAst.Effect = effect0 match {
    case OccurrenceAst1.Effect(doc0, ann0, mod0, sym0, ops0, loc0) =>
      val ops = ops0.map(visitEffectOp)
      MonoAst.Effect(doc0, ann0, mod0, sym0, ops, loc0)
  }

  private def visitEffectOp(op0: OccurrenceAst1.Op): MonoAst.Op = op0 match {
    case OccurrenceAst1.Op(sym0, fparams0, spec0, loc) =>
      val spec = visitSpec(spec0, fparams0)
      MonoAst.Op(sym0, spec, loc)
  }

  private def visitTypeParam(tparam0: OccurrenceAst1.TypeParam): MonoAst.TypeParam = tparam0 match {
    case OccurrenceAst1.TypeParam(name, sym, loc) => MonoAst.TypeParam(name, sym, loc)
  }

  /**
    * Translates the given formal parameter `fparam` from [[OccurrenceAst1.FormalParam]] into a [[MonoAst.FormalParam]].
    */
  private def visitFormalParam(fparam0: OccurrenceAst1.FormalParam): MonoAst.FormalParam = fparam0 match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[OccurrenceAst1.Expr]].
    * Returns a [[MonoAst.Expr]]
    */
  private def visitExp(exp00: OccurrenceAst1.Expr, varSubst0: VarSubst, subst0: Subst, inScopeSet0: InScopeSet, context0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): MonoAst.Expr = {

    def visit(exp0: OccurrenceAst1.Expr): MonoAst.Expr = exp0 match {
      case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
        MonoAst.Expr.Cst(cst, tpe, loc)

      case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
        // Check for renamed local binder
        varSubst0.get(sym) match {
          case Some(freshVarSym) => subst0.get(freshVarSym) match {
            // Case 1:
            // The variable `sym` is not in the substitution map and will not be inlined.
            case None => MonoAst.Expr.Var(freshVarSym, tpe, loc)
            // Case 2:
            // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
            case Some(e1) =>
              sctx.inlinedVars.add((sym0, sym))
              e1 match {
                // If `e1` is a `LiftedExp` then `e1` has already been visited
                case SubstRange.DoneExp(e) =>
                  //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym' into '$sym0'")
                  e
                // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
                case SubstRange.SuspendedExp(exp) =>
                  val e = visit(exp)
                  //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym' into '$sym0'")
                  e
              }
          }
          case None => // Function parameter occurrence
            MonoAst.Expr.Var(sym, tpe, loc)
        }

      case OccurrenceAst1.Expr.Lambda((fparam, occur), exp, tpe, loc) => // TODO: Make parameter wild if dead
        val (fps, varSubst1) = freshFormalParam(fparam)
        val varSubst2 = varSubst0 ++ varSubst1
        val e = visitExp(exp, varSubst2, subst0, inScopeSet0, context0)
        MonoAst.Expr.Lambda(fps, e, tpe, loc)

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        // TODO: Refactor this to use Context, so inlining and beta reduction cases are moved to Var and Lambda exprs respectively.
        val e2 = visit(exp2)

        def maybeInline(sym1: OutVar): MonoAst.Expr.ApplyClo = {
          inScopeSet0.get(sym1) match {
            case Some(Definition.LetBound(lambda, Occur.OnceInAbstraction)) =>
              sctx.inlinedVars.add((sym0, sym1))
              //assert(lambda.tpe.arrowResultType == tpe, s"expected '$tpe', got '${lambda.tpe.arrowResultType}' at $loc, inlining '$sym1' into '$sym0'")
              //assert(validSubEff(lambda.tpe.arrowEffectType, eff), s"expected '$eff', got '${lambda.tpe.arrowEffectType}' at $loc, inlining '$sym1' into '$sym0'")
              val e1 = refreshBinders(lambda)(Map.empty, flix)
              MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

            case Some(Definition.LetBound(lambda, Occur.Once)) =>
              sctx.inlinedVars.add((sym0, sym1))
              //assert(lambda.tpe.arrowResultType == tpe, s"expected '$tpe', got '${lambda.tpe.arrowResultType}' at $loc, inlining '$sym1' into '$sym0'")
              //assert(validSubEff(lambda.tpe.arrowEffectType, eff), s"expected '$eff', got '${lambda.tpe.arrowEffectType}' at $loc, inlining '$sym1' into '$sym0'")
              val e1 = refreshBinders(lambda)(Map.empty, flix)
              MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

            case _ =>
              val e1 = visit(exp1)
              MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)
          }
        }

        exp1 match {
          case OccurrenceAst1.Expr.Var(sym, _, _) =>
            varSubst0.get(sym) match {
              case Some(freshVarSym) => maybeInline(freshVarSym)

              case None =>
                // If it is the inScopeSet then we have added it via a let-binding so the varSubst should contain sym.
                // Thus, this is only possible if `sym` is a parameter of the top-level def.
                val e1 = visit(exp1)
                MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)
            }

          case OccurrenceAst1.Expr.Lambda(fparam, body, _, _) =>
            // Direct application, e.g., (x -> x)(1)
            sctx.betaReductions.add((sym0, 1))
            val e = inlineLocalAbstraction(body, List(fparam), List(e2))
            //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining lambda into '$sym0'")
            //assert(validSubEff(e.eff, eff), s"expected '$eff', got '${e.eff}' at $loc, inlining lambda into '$sym0'")
            e

          case _ =>
            val e1 = visit(exp1)
            MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visit)
        val def1 = root.defs.apply(sym)
        // If `def1` is a single non-self call or is trivial
        // then inline the body of `def1`
        if (canInlineDef(def1.context, context0)) {
          sctx.inlinedDefs.add((sym0, sym))
          val e = inlineDef(def1.exp, def1.fparams, es)
          //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym' into '$sym0'")
          //assert(validSubEff(e.eff, eff), s"expected '$eff', got '${e.eff}' at $loc, inlining '$sym' into '$sym0'")
          e
        } else {
          MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        varSubst0.get(sym) match {
          case Some(freshVarSym) =>
            val es = exps.map(visit)
            MonoAst.Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

          case None => throw InternalCompilerException("unexpected stale local def symbol", loc)
        }

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
        if (isDead(occur)) {
          if (isPure(exp1.eff)) {
            // Case 1:
            // If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
            // Both code size and runtime are reduced
            sctx.eliminatedVars.add((sym0, sym))
            visit(exp2)
          } else {
            // Case 2:
            // If `sym` is never used (it is `Dead`) so it is safe to make a Stm.
            sctx.eliminatedVars.add((sym0, sym))
            val e1 = visitExp(exp1, varSubst0, subst0, inScopeSet0, context0)
            MonoAst.Expr.Stm(e1, visit(exp2), tpe, eff, loc)
          }
        } else {
          val freshVarSym = Symbol.freshVarSym(sym)
          val varSubst1 = varSubst0 + (sym -> freshVarSym)

          // Case 3:
          // If `exp1` occurs once, and it is pure, then it is safe to inline.
          // There is a small decrease in code size and runtime.
          val wantToPreInline = isUsedOnceAndPure(occur, exp1.eff)
          if (wantToPreInline) {
            sctx.eliminatedVars.add((sym0, sym))
            val subst1 = subst0 + (freshVarSym -> SubstRange.SuspendedExp(exp1))
            visitExp(exp2, varSubst1, subst1, inScopeSet0, context0)
          } else {
            val e1 = visitExp(exp1, varSubst0, subst0, inScopeSet0, context0)
            // Case 4:
            // If `e1` is trivial and pure, then it is safe to inline.
            // Code size and runtime are not impacted, because only trivial expressions are inlined
            val wantToPostInline = isTrivialAndPure(e1, exp1.eff) && occur != DontInline && occur != Dangerous
            if (wantToPostInline) {
              // If `e1` is to be inlined:
              // Add map `sym` to `e1` and return `e2` without constructing the let expression.
              sctx.eliminatedVars.add((sym0, sym))
              val subst1 = subst0 + (freshVarSym -> SubstRange.DoneExp(e1))
              visitExp(exp2, varSubst1, subst1, inScopeSet0, context0)
            } else {
              // Case 5:
              // If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
              // Code size and runtime are not impacted
              val inScopeSet1 = inScopeSet0 + (freshVarSym -> Definition.LetBound(e1, occur))
              val e2 = visitExp(exp2, varSubst1, subst0, inScopeSet1, context0)
              MonoAst.Expr.Let(freshVarSym, e1, e2, tpe, eff, loc)
            }
          }
        }

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
        // TODO: Treat like let-binding above, except it is never trivial so some checks can be omitted
        if (isDead(occur)) { // Probably never happens
          sctx.eliminatedVars.add((sym0, sym))
          visit(exp2)
        } else {
          val freshVarSym = Symbol.freshVarSym(sym)
          val varSubst1 = varSubst0 + (sym -> freshVarSym)
          val e2 = visitExp(exp2, varSubst1, subst0, inScopeSet0, context0)
          val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
          val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
          val e1 = visitExp(exp1, varSubst2, subst0, inScopeSet0, context0)
          MonoAst.Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val varSubst1 = varSubst0 + (sym -> freshVarSym)
        val e = visitExp(exp, varSubst1, subst0, inScopeSet0, context0)
        MonoAst.Expr.Scope(freshVarSym, rvar, e, tpe, eff, loc)

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visit(exp1)
        e1 match {
          case MonoAst.Expr.Cst(Constant.Bool(true), _, _) =>
            sctx.simplifiedIfThenElse.add((sym0, 1))
            visit(exp2)
          case MonoAst.Expr.Cst(Constant.Bool(false), _, _) =>
            sctx.simplifiedIfThenElse.add((sym0, 1))
            visit(exp3)
          case _ => MonoAst.Expr.IfThenElse(e1, visit(exp2), visit(exp3), tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        // Case 1:
        // If `exp1` is pure, so it has no side effects, then it is safe to remove
        // Both code size and runtime are reduced
        if (isPure(exp1.eff)) {
          sctx.eliminatedStms.add((sym0, 1))
          visit(exp2)
        } else {
          val e1 = visit(exp1)
          val e2 = visit(exp2)
          MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
            val (p, varSubst1) = visitPattern(pat)
            val varSubst2 = varSubst0 ++ varSubst1
            val g = guard.map(visitExp(_, varSubst2, subst0, inScopeSet0, context0))
            val e1 = visitExp(exp1, varSubst2, subst0, inScopeSet0, context0)
            MonoAst.MatchRule(p, g, e1)
        }
        MonoAst.Expr.Match(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.VectorLit(es, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
        val e = visit(exp)
        MonoAst.Expr.VectorLength(e, loc)

      case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Ascribe(e, tpe, eff, loc)

      case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
            val freshVarSym = Symbol.freshVarSym(sym)
            val varSubst1 = varSubst0 + (sym -> freshVarSym)
            val e1 = visitExp(exp1, varSubst1, subst0, inScopeSet0, context0)
            MonoAst.CatchRule(freshVarSym, clazz, e1)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.HandlerRule(op, fparams, exp1) =>
            val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
            val varSubst1 = varSubsts.fold(varSubst0)(_ ++ _)
            val e1 = visitExp(exp1, varSubst1, subst0, inScopeSet0, context0)
            MonoAst.HandlerRule(op, fps, e1)
        }
        MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.Do(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
        val methods = methods0.map {
          case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
            val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
            val varSubst1 = varSubsts.fold(varSubst0)(_ ++ _)
            val e = visitExp(exp, varSubst1, subst0, inScopeSet0, context0)
            MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
        }
        MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

    }

    /**
      * Recursively bind each argument in `args` to a let-expression with a fresh symbol
      * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
      */
    def inlineLocalAbstraction(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, Occur)], args: List[OutExpr])(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = {
      bind(exp0, symbols, args, varSubst0, subst0, inScopeSet0, context0)
    }


    visit(exp00)
  }

  private def validSubEff(eff1: Type, eff2: Type): Boolean = {
    if (eff1 == Type.Pure) {
      true
    }
    else {
      eff1 == eff2
    }
  }

  private def visitPattern(pattern00: OccurrenceAst1.Pattern)(implicit flix: Flix): (MonoAst.Pattern, VarSubst) = {

    // TODO: Figure out what to do with occurrence information in Pattern.Var
    def visit(pattern0: OccurrenceAst1.Pattern): (MonoAst.Pattern, VarSubst) = pattern0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        (MonoAst.Pattern.Wild(tpe, loc), Map.empty)

      case OccurrenceAst1.Pattern.Var(sym, tpe, occur, loc) =>
        if (isDead(occur)) {
          (MonoAst.Pattern.Wild(tpe, loc), Map.empty)
        } else {
          val freshVarSym = Symbol.freshVarSym(sym)
          (MonoAst.Pattern.Var(freshVarSym, tpe, loc), Map(sym -> freshVarSym))
        }

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        (MonoAst.Pattern.Cst(cst, tpe, loc), Map.empty)

      case OccurrenceAst1.Pattern.Tag(sym, pats, tpe, loc) =>
        val (ps, varSubsts) = pats.map(visit).unzip
        val varSubst = varSubsts.foldLeft(Map.empty[InVar, OutVar])(_ ++ _)
        (MonoAst.Pattern.Tag(sym, ps, tpe, loc), varSubst)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val (ps, varSubsts) = pats.map(visit).unzip
        val varSubst = varSubsts.foldLeft(Map.empty[InVar, OutVar])(_ ++ _)
        (MonoAst.Pattern.Tuple(ps, tpe, loc), varSubst)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val (ps, varSubsts) = pats.map(visitRecordLabelPattern).unzip
        val (p, varSubst1) = visit(pat)
        val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
        (MonoAst.Pattern.Record(ps, p, tpe, loc), varSubst2)

      case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
        (MonoAst.Pattern.RecordEmpty(tpe, loc), Map.empty)
    }

    def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): (MonoAst.Pattern.Record.RecordLabelPattern, VarSubst) = pattern0 match {
      case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val (p, subst) = visit(pat)
        (MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
    }

    visit(pattern00)
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    */
  private def inlineDef(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, Occur)], args: List[OutExpr])(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): MonoAst.Expr = {
    bind(exp0, symbols, args, Map.empty, Map.empty, Map.empty, Context.Stop)
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    */
  private def bind(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, Occur)], args: List[OutExpr], varSubst0: VarSubst, subst0: Subst, inScopeSet0: InScopeSet, context0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): MonoAst.Expr = {
    def bnd(syms: List[(OccurrenceAst1.FormalParam, Occur)], as: List[OutExpr], env: VarSubst): MonoAst.Expr = (syms, as) match {
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1.eff) =>
        // If the parameter is unused and the argument is pure, then throw it away.
        bnd(nextSymbols, nextExpressions, env)

      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
        // If the parameter is unused and the argument is NOT pure, then put it in a statement.
        val nextLet = bnd(nextSymbols, nextExpressions, env)
        val eff = canonicalEffect(Type.mkUnion(e1.eff, nextLet.eff, e1.loc))
        MonoAst.Expr.Stm(e1, nextLet, nextLet.tpe, eff, exp0.loc)

      case ((OccurrenceAst1.FormalParam(sym, _, _, _, _), _) :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(sym)
        val env1 = env + (sym -> freshVar)
        val nextLet = bnd(nextSymbols, nextExpressions, env1)
        val eff = canonicalEffect(Type.mkUnion(e1.eff, nextLet.eff, e1.loc))
        MonoAst.Expr.Let(freshVar, e1, nextLet, nextLet.tpe, eff, exp0.loc)

      case _ => visitExp(exp0, varSubst0 ++ env, subst0, inScopeSet0, context0)
    }

    bnd(symbols, args, Map.empty)
  }

  private def freshFormalParam(fp0: OccurrenceAst1.FormalParam)(implicit flix: Flix): (MonoAst.FormalParam, VarSubst) = fp0 match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst = Map(sym -> freshVarSym)
      (MonoAst.FormalParam(freshVarSym, mod, tpe, src, loc), subst)
  }

  private def refreshFormalParam(fp0: MonoAst.FormalParam)(implicit flix: Flix): (MonoAst.FormalParam, VarSubst) = fp0 match {
    case MonoAst.FormalParam(sym, mod, tpe, src, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst = Map(sym -> freshVarSym)
      (MonoAst.FormalParam(freshVarSym, mod, tpe, src, loc), subst)
  }

  private def refreshFormalParams(fparams0: List[MonoAst.FormalParam])(implicit flix: Flix): (List[MonoAst.FormalParam], VarSubst) = {
    val (fps, substs) = fparams0.map(refreshFormalParam).unzip
    val subst = substs.reduceLeft(_ ++ _)
    (fps, subst)
  }

  private def refreshBinders(expr0: OutExpr)(implicit subst0: VarSubst, flix: Flix): OutExpr = expr0 match {
    case MonoAst.Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case MonoAst.Expr.Var(sym, tpe, loc) =>
      val freshVarSym = subst0.getOrElse(sym, sym)
      Expr.Var(freshVarSym, tpe, loc)

    case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (fp, subst1) = refreshFormalParam(fparam)
      val subst2 = subst0 ++ subst1
      val e = refreshBinders(exp)(subst2, flix)
      Expr.Lambda(fp, e, tpe, loc)

    case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val freshVarSym = subst0.getOrElse(sym, sym)
      val es = exps.map(refreshBinders)
      Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

    case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)(subst1, flix)
      Expr.Let(freshVarSym, e1, e2, tpe, eff, loc)

    case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e2 = refreshBinders(exp2)(subst1, flix)
      val (fps, subst2) = refreshFormalParams(fparams)
      val subst3 = subst1 ++ subst2
      val e1 = refreshBinders(exp1)(subst3, flix)
      Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, loc)

    case MonoAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e = refreshBinders(exp)(subst1, flix)
      Expr.Scope(freshVarSym, regionVar, e, tpe, eff, loc)

    case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      val e3 = refreshBinders(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case MonoAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.Stm(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.Discard(exp, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Discard(e, eff, loc)

    case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case MonoAst.MatchRule(pat, guard, exp) =>
          val (p, subst1) = refreshPattern(pat)
          val subst2 = subst0 ++ subst1
          val g = guard.map(refreshBinders(_)(subst2, flix))
          val e = refreshBinders(exp)(subst2, flix)
          MonoAst.MatchRule(p, g, e)
      }
      Expr.Match(e, rs, tpe, eff, loc)

    case MonoAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.VectorLit(es, tpe, eff, loc)

    case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case MonoAst.Expr.VectorLength(exp, loc) =>
      val e = refreshBinders(exp)
      Expr.VectorLength(e, loc)

    case MonoAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Ascribe(e, tpe, eff, loc)

    case MonoAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case MonoAst.CatchRule(sym, clazz, exp1) =>
          val freshVarSym = Symbol.freshVarSym(sym)
          val subst1 = subst0 + (sym -> freshVarSym)
          val e1 = refreshBinders(exp1)(subst1, flix)
          MonoAst.CatchRule(freshVarSym, clazz, e1)
      }
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case MonoAst.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case MonoAst.HandlerRule(op, fparams, exp1) =>
          val (fps, subst1) = refreshFormalParams(fparams)
          val subst2 = subst0 ++ subst1
          val e1 = refreshBinders(exp1)(subst2, flix)
          MonoAst.HandlerRule(op, fps, e1)
      }
      Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case MonoAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.Do(op, es, tpe, eff, loc)

    case MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val ms = methods.map {
        case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val (fps, subst1) = refreshFormalParams(fparams)
          val subst2 = subst0 ++ subst1
          val e = refreshBinders(exp)(subst2, flix)
          MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
      }
      Expr.NewObject(name, clazz, tpe, eff, ms, loc)
  }

  private def refreshPattern(pattern0: MonoAst.Pattern)(implicit flix: Flix): (MonoAst.Pattern, VarSubst) = {
    def refreshRecordLabelPattern(pattern0: MonoAst.Pattern.Record.RecordLabelPattern)(implicit flix: Flix): (MonoAst.Pattern.Record.RecordLabelPattern, VarSubst) = pattern0 match {
      case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val (p, subst) = refreshPattern(pat)
        (MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
    }

    pattern0 match {
      case MonoAst.Pattern.Wild(tpe, loc) =>
        (MonoAst.Pattern.Wild(tpe, loc), Map.empty)

      case MonoAst.Pattern.Var(sym, tpe, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        (MonoAst.Pattern.Var(freshVarSym, tpe, loc), Map(sym -> freshVarSym))

      case MonoAst.Pattern.Cst(cst, tpe, loc) =>
        (MonoAst.Pattern.Cst(cst, tpe, loc), Map.empty)

      case MonoAst.Pattern.Tag(sym, pats, tpe, loc) =>
        val (ps, substs) = pats.map(refreshPattern).unzip
        val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
        (MonoAst.Pattern.Tag(sym, ps, tpe, loc), subst)

      case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
        val (ps, substs) = pats.map(refreshPattern).unzip
        val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
        (MonoAst.Pattern.Tuple(ps, tpe, loc), subst)

      case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
        val (ps, substs) = pats.map(refreshRecordLabelPattern).unzip
        val (p, subst) = refreshPattern(pat)
        val subst1 = substs.foldLeft(subst)(_ ++ _)
        (MonoAst.Pattern.Record(ps, p, tpe, loc), subst1)

      case MonoAst.Pattern.RecordEmpty(tpe, loc) =>
        (MonoAst.Pattern.RecordEmpty(tpe, loc), Map.empty)
    }
  }

  /**
    * A function below the soft threshold is typically inlined.
    */
  private val SoftInlineThreshold: Int = 4

  /**
    * A function above the hard threshold is never inlined.
    */
  private val HardInlineThreshold: Int = 8

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(ctx: DefContext, context: Context): Boolean = {
    val mayInline = ctx.occur != DontInline && ctx.occur != Dangerous && !ctx.isSelfRecursive && context != Context.Stop
    val isSomewhereOnce = ctx.occur == Once || ctx.occur == OnceInAbstraction
    val belowSoft = ctx.size - ctx.localDefs * 8 < SoftInlineThreshold
    val belowHard = ctx.size - ctx.localDefs * 8 < HardInlineThreshold
    val shouldInline = belowSoft || (ctx.isDirectCall && belowHard) || (isSomewhereOnce && belowHard)
    mayInline && shouldInline
  }

  private def isPure(eff0: Type): Boolean = {
    eff0 == Type.Pure
  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst1.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `eff` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Once => isPure(eff0)
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `eff` is pure
    */
  private def isTrivialAndPure(exp0: MonoAst.Expr, eff0: Type): Boolean = {
    isPure(eff0) && isTrivialExp(exp0)
  }

  /**
    * Checks if `occur` is dead and `exp` is pure.
    */
  private def isDeadAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Dead => isPure(eff0)
    case _ => false
  }

  /** Returns a canonical effect type equivalent to `eff` */
  private def canonicalEffect(eff: Type): Type = {
    evalToType(eval(eff), eff.loc)
  }

  /** Evaluates a ground, simplified effect type */
  private def eval(eff: Type): CofiniteEffSet = eff match {
    case Type.Univ => CofiniteEffSet.universe
    case Type.Pure => CofiniteEffSet.empty
    case Type.Cst(TypeConstructor.Effect(sym), _) =>
      CofiniteEffSet.mkSet(sym)
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), y, _) =>
      CofiniteEffSet.complement(eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _) =>
      CofiniteEffSet.union(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _) =>
      CofiniteEffSet.intersection(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y, _) =>
      CofiniteEffSet.difference(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y, _) =>
      CofiniteEffSet.xor(eval(x), eval(y))
    case other => throw InternalCompilerException(s"Unexpected effect $other", other.loc)
  }

  /** Returns the [[Type]] representation of `set` with `loc`. */
  private def evalToType(set: CofiniteEffSet, loc: SourceLocation): Type = set match {
    case CofiniteEffSet.Set(s) => Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc)
    case CofiniteEffSet.Compl(s) => Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc), loc)
  }


  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: MonoAst.Expr): Boolean = exp0 match {
    case MonoAst.Expr.Cst(_, _, _) => true
    case MonoAst.Expr.Var(_, _, _) => true
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Unary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Binary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Tuple, exps, _, _, _) => exps.forall(isTrivialExp)
    case _ => false
  }

  def toMonoAstExpr(exp0: OccurrenceAst1.Expr): MonoAst.Expr = exp0 match {
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
      MonoAst.Expr.Var(sym, tpe, loc)

    case OccurrenceAst1.Expr.Lambda((fparam, _), exp, tpe, loc) =>
      val fps = visitFormalParam(fparam)
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.Lambda(fps, e, tpe, loc)

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(toMonoAstExpr)
      MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = toMonoAstExpr(exp1)
      val es = toMonoAstExpr(exp2)
      MonoAst.Expr.ApplyClo(e1, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(toMonoAstExpr)
      MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(toMonoAstExpr)
      MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
      val e1 = toMonoAstExpr(exp1)
      val e2 = toMonoAstExpr(exp2)
      MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      if (isDead(occur)) {
        toMonoAstExpr(exp2)
      } else {
        val fps = fparams.map(visitFormalParam)
        val e1 = toMonoAstExpr(exp1)
        val e2 = toMonoAstExpr(exp2)
        MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)
      }

    case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = toMonoAstExpr(exp1)
      val e2 = toMonoAstExpr(exp2)
      val e3 = toMonoAstExpr(exp3)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = toMonoAstExpr(exp1)
      val e2 = toMonoAstExpr(exp2)
      MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.Discard(e, eff, loc)

    case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
          val p = toMonoAstPattern(pat)
          val g = guard.map(toMonoAstExpr)
          val e1 = toMonoAstExpr(exp1)
          MonoAst.MatchRule(p, g, e1)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(toMonoAstExpr)
      MonoAst.Expr.VectorLit(es, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = toMonoAstExpr(exp1)
      val e2 = toMonoAstExpr(exp2)
      MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.VectorLength(e, loc)

    case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.Ascribe(e, tpe, eff, loc)

    case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
          val e1 = toMonoAstExpr(exp1)
          MonoAst.CatchRule(sym, clazz, e1)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = toMonoAstExpr(exp)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp1) =>
          val fps = fparams.map(visitFormalParam)
          val e1 = toMonoAstExpr(exp1)
          MonoAst.HandlerRule(op, fps, e1)
      }
      MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(toMonoAstExpr)
      MonoAst.Expr.Do(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val fps = fparams.map(visitFormalParam)
          val e = toMonoAstExpr(exp)
          MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
      }
      MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

  }


  private def toMonoAstPattern(pattern00: OccurrenceAst1.Pattern): MonoAst.Pattern = {

    def visit(pattern0: OccurrenceAst1.Pattern): MonoAst.Pattern = pattern0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        MonoAst.Pattern.Wild(tpe, loc)

      case OccurrenceAst1.Pattern.Var(sym, tpe, occur, loc) =>
        MonoAst.Pattern.Var(sym, tpe, loc)

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        MonoAst.Pattern.Cst(cst, tpe, loc)

      case OccurrenceAst1.Pattern.Tag(sym, pats, tpe, loc) =>
        val ps = pats.map(visit)
        MonoAst.Pattern.Tag(sym, ps, tpe, loc)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visit)
        MonoAst.Pattern.Tuple(ps, tpe, loc)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map(visitRecordLabelPattern)
        val p = visit(pat)
        MonoAst.Pattern.Record(ps, p, tpe, loc)

      case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
        MonoAst.Pattern.RecordEmpty(tpe, loc)
    }

    def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): MonoAst.Pattern.Record.RecordLabelPattern = pattern0 match {
      case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val p = visit(pat)
        MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
    }

    visit(pattern00)
  }


  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    *
    * inlinedDefs is a map where each def `def1` points to a set of defs that have been inlined into `def1`.
    * inlinedVars is a map where each def `def1` points to a set of vars that have been inlined at their use sites in `def1`.
    * betaReductions is a map where each def `def1` points to the number of beta reductions that have been performed in `def1`.
    * eliminatedVars is a map where each def `def1` points to the vars that have been removed from `def1`.
    * simplifiedIfThenElse is a map where each def `def1` points to the number of simplifications of `if (constant) e1 else e2` in `def1`.
    * eliminatedStms is a map where each def `def1` points to the number of removed Stms that were pure `def1`.
    */
  private case class SharedContext(inlinedDefs: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.DefnSym)], inlinedVars: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.VarSym)], betaReductions: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)], eliminatedVars: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.VarSym)], simplifiedIfThenElse: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)], eliminatedStms: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)])

  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = SharedContext(new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue())
  }


  case class Stats(inlinedDefs: Map[Symbol.DefnSym, Set[Symbol.DefnSym]],
                   inlinedVars: Map[Symbol.DefnSym, Set[Symbol.VarSym]],
                   betaReductions: Map[Symbol.DefnSym, Int],
                   eliminatedVars: Map[Symbol.DefnSym, Set[Symbol.VarSym]],
                   simplifiedIfThenElse: Map[Symbol.DefnSym, Int],
                   eliminatedStms: Map[Symbol.DefnSym, Int]) {
    def ++(that: Stats): Stats = {
      val inlinedDefs1 = Stats.merge(inlinedDefs, that.inlinedDefs)(_ ++ _)
      val inlinedVars1 = Stats.merge(inlinedVars, that.inlinedVars)(_ ++ _)
      val betaReductions1 = Stats.merge(betaReductions, that.betaReductions)(_ + _)
      val eliminatedVars1 = Stats.merge(eliminatedVars, that.eliminatedVars)(_ ++ _)
      val simplifiedIfThenElse1 = Stats.merge(simplifiedIfThenElse, that.simplifiedIfThenElse)(_ + _)
      val eliminatedStms1 = Stats.merge(eliminatedStms, that.eliminatedStms)(_ + _)
      Stats(inlinedDefs1, inlinedVars1, betaReductions1, eliminatedVars1, simplifiedIfThenElse1, eliminatedStms1)
    }

    override def toString: String = {
      s"""====== STATISTICS ======
         |
         |Inlined Defs: $inlinedDefs
         |
         |Inlined Vars: $inlinedVars
         |
         |Eliminated Vars: $eliminatedVars
         |
         |Beta Reductions: $betaReductions
         |
         |Eliminated Stms: $eliminatedStms
         |
         |Eliminated Ifs: $simplifiedIfThenElse
         |""".stripMargin
    }
  }

  private object Stats {
    private def merge[A, B](m1: Map[A, B], m2: Map[A, B])(combine: (B, B) => B): Map[A, B] = {
      val smallest = if (m1.size < m2.size) m1 else m2
      val biggest = if (m1.size >= m2.size) m1 else m2
      smallest.foldLeft(biggest) {
        case (acc, (k, v1)) => acc.get(k) match {
          case Some(v2) => acc + (k -> combine(v1, v2))
          case None => acc + (k -> v1)
        }
      }
    }

    private def toMapSet[A, B](iterable: Iterable[(A, B)]): Map[A, Set[B]] = {
      iterable.foldLeft(Map.empty[A, Set[B]]) {
        case (m, (k, v)) => m.get(k) match {
          case Some(set) => m + (k -> (set + v))
          case None => m + (k -> Set(v))
        }
      }
    }

    private def toCount[A](iterable: Iterable[(A, Int)]): Map[A, Int] = {
      iterable.foldLeft(Map.empty[A, Int]) {
        case (m, (k, i)) => m.get(k) match {
          case Some(j) => m + (k -> (i + j))
          case None => m + (k -> i)
        }
      }
    }

    def from(sctx: SharedContext): Stats = {
      val inlinedDefs = toMapSet(sctx.inlinedDefs.asScala)
      val inlinedVars = toMapSet(sctx.inlinedVars.asScala)
      val betaReductions = toCount(sctx.betaReductions.asScala)
      val eliminatedVars = toMapSet(sctx.eliminatedVars.asScala)
      val simplifiedIfThenElse = toCount(sctx.simplifiedIfThenElse.asScala)
      val eliminatedStms = toCount(sctx.eliminatedStms.asScala)
      Stats(inlinedDefs, inlinedVars, betaReductions, eliminatedVars, simplifiedIfThenElse, eliminatedStms)
    }
  }

}
