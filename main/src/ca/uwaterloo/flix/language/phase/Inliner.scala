/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.*
import ca.uwaterloo.flix.language.ast.Purity.Pure
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, LiftedAst, OccurrenceAst, Purity, Symbol}
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * The inliner replaces closures and functions by their code to improve performance.
  */
object Inliner {

  sealed trait Expr

  object Expr {
    case class LiftedExp(exp: LiftedAst.Expr) extends Expr

    case class OccurrenceExp(exp: OccurrenceAst.Expr) extends Expr
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
  private def canInlineDef(def0: OccurrenceAst.Def): Boolean = {
    val mayInline = def0.context.occur != DontInline && def0.context.occur != Dangerous && !def0.context.isSelfRecursive
    val belowSoft = def0.context.size < SoftInlineThreshold
    val belowHard = def0.context.size < HardInlineThreshold
    val shouldInline = belowSoft || (def0.context.isDirectCall && belowHard) || (def0.context.occur == Once && belowHard)
    mayInline && shouldInline
  }

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationMessage] = {
    val defs = ParOps.parMapValues(root.defs)(d => visitDef(d)(flix, root))
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    Validation.Success(LiftedAst.Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources))
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from OccurrenceAst to LiftedAst.
    */
  private def visitDef(def0: OccurrenceAst.Def)(implicit flix: Flix, root: OccurrenceAst.Root): LiftedAst.Def = {
    val convertedExp = if (def0.context.occur != Dangerous) visitExp(def0.exp, Map.empty)(root, flix) else toLiftedExpr(def0.exp)
    val cparams = def0.cparams.map {
      case (OccurrenceAst.FormalParam(sym, mod, tpe, loc), _) => LiftedAst.FormalParam(sym, mod, tpe, loc)
    }
    val fparams = def0.fparams.map {
      case (OccurrenceAst.FormalParam(sym, mod, tpe, loc), _) => LiftedAst.FormalParam(sym, mod, tpe, loc)
    }
    LiftedAst.Def(def0.ann, def0.mod, def0.sym, cparams, fparams, convertedExp, def0.tpe, def0.loc)
  }

  private def visitEnum(enm: OccurrenceAst.Enum): LiftedAst.Enum = enm match {
    case OccurrenceAst.Enum(ann, mod, sym, tparams0, cases0, loc) =>
      val tparams = tparams0.map(param => LiftedAst.TypeParam(param.name, param.sym, param.loc))
      val cases = MapOps.mapValues(cases0)(visitEnumCase)
      LiftedAst.Enum(ann, mod, sym, tparams, cases, loc)
  }

  private def visitEnumCase(caze: OccurrenceAst.Case): LiftedAst.Case = caze match {
    case OccurrenceAst.Case(sym, tpes, loc) => LiftedAst.Case(sym, tpes, loc)
  }

  private def visitStruct(struct: OccurrenceAst.Struct): LiftedAst.Struct = struct match {
    case OccurrenceAst.Struct(ann, mod, sym, tparams0, fields0, loc) =>
      val tparams = tparams0.map(param => LiftedAst.TypeParam(param.name, param.sym, param.loc))
      val fields = fields0.map(visitStructField)
      LiftedAst.Struct(ann, mod, sym, tparams, fields, loc)
  }

  private def visitStructField(field: OccurrenceAst.StructField): LiftedAst.StructField = field match {
    case OccurrenceAst.StructField(sym, tpe, loc) => LiftedAst.StructField(sym, tpe, loc)
  }

  private def visitEffect(effect: OccurrenceAst.Effect): LiftedAst.Effect = effect match {
    case OccurrenceAst.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffectOp)
      LiftedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitEffectOp(op: OccurrenceAst.Op): LiftedAst.Op = op match {
    case OccurrenceAst.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      LiftedAst.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` of Monotype OccurrenceAst.Expression.
    * Returns an expression of Monotype Expression
    */
  private def visitExp(exp0: OccurrenceAst.Expr, subst0: Map[Symbol.VarSym, Expr])(implicit root: OccurrenceAst.Root, flix: Flix): LiftedAst.Expr = exp0 match {
    case OccurrenceAst.Expr.Cst(cst, tpe, loc) => LiftedAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst.Expr.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        // Case 1:
        // The variable `sym` is not in the substitution map and will not be inlined.
        case None => LiftedAst.Expr.Var(sym, tpe, loc)
        // Case 2:
        // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
        case Some(e1) =>
          e1 match {
            // If `e1` is a `LiftedExp` then `e1` has already been visited
            case Expr.LiftedExp(e) =>
              //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym'")
              e
            // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
            case Expr.OccurrenceExp(exp) =>
              val e = visitExp(exp, subst0)
              //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym'")
              e
          }
      }

    case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      e1 match {
        case LiftedAst.Expr.ApplyAtomic(AtomicOp.Closure(sym), closureArgs, _, _, _) =>
          val def1 = root.defs.apply(sym)
          // If `def1` is a single non-self call or
          // it is trivial
          // then inline the body of `def1`
          if (canInlineDef(def1)) {
            // Map for substituting formal parameters of a function with the closureArgs currently in scope
            val e = bindFormals(def1.exp, def1.cparams ++ def1.fparams, closureArgs :+ e2, Map.empty)
            //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym'")
            //assert(e.purity == purity, s"expected '$purity', got '${e.purity}' at $loc, inlining '$sym'")
            e
          } else {
            LiftedAst.Expr.ApplyClo(e1, e2, tpe, purity, loc)
          }
        case _ => LiftedAst.Expr.ApplyClo(e1, e2, tpe, purity, loc)
      }

    case OccurrenceAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or
      // it is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1)) {
        val e = bindFormals(def1.exp, def1.cparams ++ def1.fparams, es, Map.empty)
        //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym'")
        //assert(e.purity == purity, s"expected '$purity', got '${e.purity}' at $loc, inlining '$sym'")
        e
      } else {
        LiftedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)
      }

    case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      e1 match {
        case LiftedAst.Expr.Cst(Constant.Bool(true), _, _) => e2
        case LiftedAst.Expr.Cst(Constant.Bool(false), _, _) => e3
        case _ => LiftedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)
      }

    case OccurrenceAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      LiftedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst.Expr.JumpTo(sym, tpe, purity, loc) => LiftedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst.Expr.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      if (isDead(occur)) {
        if (Purity.isPure(exp1.purity)) {
          /// Case 1:
          /// If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
          /// Both code size and runtime are reduced
          visitExp(exp2, subst0)
        } else {
          /// Case 2:
          /// If `sym` is never used (it is `Dead`) so it is safe to make a Stmt.
          LiftedAst.Expr.Stm(visitExp(exp1, subst0), visitExp(exp2, subst0), tpe, purity, loc)
        }
      } else {
        /// Case 3:
        /// If `exp1` occurs once and it is pure, then it is safe to inline.
        /// There is a small decrease in code size and runtime.
        val wantToPreInline = isUsedOnceAndPure(occur, exp1.purity)
        if (wantToPreInline) {
          val subst1 = subst0 + (sym -> Expr.OccurrenceExp(exp1))
          visitExp(exp2, subst1)
        } else {
          val e1 = visitExp(exp1, subst0)
          /// Case 4:
          /// If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, exp1.purity) && occur != DontInline
          if (wantToPostInline) {
            /// If `e1` is to be inlined:
            /// Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = subst0 + (sym -> Expr.LiftedExp(e1))
            visitExp(exp2, subst1)
          } else {
            /// Case 5:
            /// If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            /// Code size and runtime are not impacted
            val e2 = visitExp(exp2, subst0)
            LiftedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)
          }
        }
      }

    case OccurrenceAst.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      /// Case 1:
      /// If `exp1` is pure, so it has no side effects, then it is safe to remove
      /// Both code size and runtime are reduced
      if (exp1.purity == Purity.Pure) {
        visitExp(exp2, subst0)
      } else {
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        LiftedAst.Expr.Stm(e1, e2, tpe, purity, loc)
      }

    case OccurrenceAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          LiftedAst.CatchRule(sym, clazz, e)
      }
      LiftedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExp(exp, subst0)
          LiftedAst.HandlerRule(op, fps, e)
      }
      LiftedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      LiftedAst.Expr.Do(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = visitExp(clo, subst0)
          LiftedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `purity` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Once, Purity.Pure) => true
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `purity` is pure
    */
  private def isTrivialAndPure(exp0: LiftedAst.Expr, purity: Purity): Boolean = purity match {
    case Purity.Pure => isTrivialExp(exp0)
    case _ => false
  }

  /**
    * Checks if `occur` is dead and  `exp` is pure.
    */
  private def isDeadAndPure(occur: OccurrenceAst.Occur, exp: LiftedAst.Expr): Boolean = (occur, exp.purity) match {
    case (Dead, Pure) => true
    case _ => false
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    * Substitute variables in `exp0` via the filled substitution map `env0`
    */
  private def bindFormals(exp0: OccurrenceAst.Expr, symbols: List[(OccurrenceAst.FormalParam, OccurrenceAst.Occur)], args: List[LiftedAst.Expr], env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: OccurrenceAst.Root, flix: Flix): LiftedAst.Expr = {
    (symbols, args) match {
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1) =>
        // if the parameter is unused and the argument is pure, then throw it away.
        bindFormals(exp0, nextSymbols, nextExpressions, env0)
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
        // if the parameter is unused and the argument is NOT pure, then put it in a statement.
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env0)
        val purity = Purity.combine(e1.purity, nextLet.purity)
        LiftedAst.Expr.Stm(e1, nextLet, exp0.tpe, purity, exp0.loc)
      case ((formal, _) :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(formal.sym)
        val env1 = env0 + (formal.sym -> freshVar)
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env1)
        val purity = Purity.combine(e1.purity, nextLet.purity)
        LiftedAst.Expr.Let(freshVar, e1, nextLet, exp0.tpe, purity, exp0.loc)
      case _ => substituteExp(exp0, env0)
    }
  }

  /**
    * returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: LiftedAst.Expr): Boolean = exp0 match {
    case LiftedAst.Expr.Cst(_, _, _) => true
    case LiftedAst.Expr.Var(_, _, _) => true
    case _ => false
  }

  /**
    * Substitute variables in `exp0` for new fresh variables in `env0`
    */
  private def substituteExp(exp0: OccurrenceAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: OccurrenceAst.Root, flix: Flix): LiftedAst.Expr = exp0 match {
    case OccurrenceAst.Expr.Cst(cst, tpe, loc) => LiftedAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst.Expr.Var(sym, tpe, loc) => LiftedAst.Expr.Var(env0.getOrElse(sym, sym), tpe, loc)

    case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(substituteExp(_, env0))
      LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      LiftedAst.Expr.ApplyClo(e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(substituteExp(_, env0))
      LiftedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)

    case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      val e3 = substituteExp(exp3, env0)
      LiftedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val bs = branches.map {
        case (sym, br) => sym -> substituteExp(br, env0)
      }
      LiftedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst.Expr.JumpTo(sym, tpe, purity, loc) => LiftedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst.Expr.Let(sym, exp1, exp2, _, tpe, purity, loc) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshVar)
      val e1 = substituteExp(exp1, env1)
      val e2 = substituteExp(exp2, env1)
      LiftedAst.Expr.Let(freshVar, e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      LiftedAst.Expr.Stm(e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      LiftedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshVar)
          val e = substituteExp(exp, env1)
          LiftedAst.CatchRule(freshVar, clazz, e)
      }
      LiftedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val rs = rules.map {
        case OccurrenceAst.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = substituteExp(exp, env0)
          LiftedAst.HandlerRule(op, fps, e)
      }
      LiftedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(substituteExp(_, env0))
      LiftedAst.Expr.Do(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = substituteExp(clo, env0)
          LiftedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

  }

  /**
    * Translates the given formal parameter `fparam` from OccurrenceAst.FormalParam into a lifted formal parameter.
    */
  private def visitFormalParam(fparam: OccurrenceAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def toLiftedExpr(exp0: OccurrenceAst.Expr): LiftedAst.Expr = exp0 match {
    case OccurrenceAst.Expr.Cst(cst, tpe, loc) => LiftedAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst.Expr.Var(sym, tpe, loc) =>
      LiftedAst.Expr.Var(sym, tpe, loc)

    case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(toLiftedExpr)
      LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, purity, loc) =>
      val e1 = toLiftedExpr(exp1)
      val e2 = toLiftedExpr(exp2)
      LiftedAst.Expr.ApplyClo(e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(toLiftedExpr)
      LiftedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)

    case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = toLiftedExpr(exp1)
      val e2 = toLiftedExpr(exp2)
      val e3 = toLiftedExpr(exp3)
      LiftedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = toLiftedExpr(exp)
      val bs = branches.map {
        case (sym, br) => sym -> toLiftedExpr(br)
      }
      LiftedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst.Expr.JumpTo(sym, tpe, purity, loc) => LiftedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst.Expr.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      val e1 = toLiftedExpr(exp1)
      val e2 = toLiftedExpr(exp2)
      LiftedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      val e1 = toLiftedExpr(exp1)
      val e2 = toLiftedExpr(exp2)
      LiftedAst.Expr.Stm(e1, e2, tpe, purity, loc)

    case OccurrenceAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = toLiftedExpr(exp)
      LiftedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = toLiftedExpr(exp)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = toLiftedExpr(exp)
          LiftedAst.CatchRule(sym, clazz, e)
      }
      LiftedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = toLiftedExpr(exp)
      val rs = rules.map {
        case OccurrenceAst.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = toLiftedExpr(exp)
          LiftedAst.HandlerRule(op, fps, e)
      }
      LiftedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case OccurrenceAst.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(toLiftedExpr)
      LiftedAst.Expr.Do(op, es, tpe, purity, loc)

    case OccurrenceAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = toLiftedExpr(exp)
          LiftedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

  }


}
