/*
 * Copyright 2017 Magnus Madsen, 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

object Reachability {

  /**
    * Returns the root with only reachable functions.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, ListMap[Symbol, SourceLocation]) = {
    implicit val sctx: SharedContext = SharedContext.mk()

    // Entry points are always reachable.
    val initReach: Set[ReachableSym] = root.entryPoints.map(ReachableSym.DefnSym.apply)

    // Compute the symbols that are transitively reachable.
    val next = (sym: ReachableSym) => visitSym(sym, root)(sctx)
    val allReachable = ParOps.parReach(initReach, next)

    // Filter the reachable definitions.
    val reachableDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.DefnSym(sym))
    }

    val uses = sctx.useSites.asScala.foldLeft(ListMap.empty[Symbol, SourceLocation]) {
      case (acc, (sym, symUse)) => acc + (sym -> symUse)
    }

    // Reassemble the AST.
    (root.copy(defs = reachableDefs), uses)
  }

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    *
    * This includes three types of symbols:
    *   - (a) The function or signature symbols in the implementation / body expression of a reachable function symbol
    *   - (b) The trait symbol of a reachable sig symbol.
    *   - (c) Every expression in a trait instance of a reachable trait symbol is reachable.
    */
  private def visitSym(sym: ReachableSym, root: TypedAst.Root)(implicit sctx: SharedContext): Set[ReachableSym] = sym match {
    case ReachableSym.DefnSym(defnSym) =>
      visitExp(root.defs(defnSym).exp)(defnSym, sctx)

    case ReachableSym.SigSym(sigSym) =>
      val sig = root.sigs(sigSym)
      implicit val sym0: Symbol = sig.sym
      Set(ReachableSym.TraitSym(sig.sym.trt)) ++
        sig.exp.map(visitExp).getOrElse(Set.empty)

    case ReachableSym.TraitSym(traitSym) =>
      root.instances(traitSym).foldLeft(Set.empty[ReachableSym]) {
        case (acc, s) => s.defs.flatMap(defn => visitExp(defn.exp)(defn.sym, sctx)).toSet ++ acc
      }
  }

  /**
    * Returns the function and signature symbols reachable from the given expression `exp0`.
    *
    * Implicit parameter `sym0` is the visited function.
    */
  private def visitExp(exp0: TypedAst.Expr)(implicit sym0: Symbol, sctx: SharedContext): Set[ReachableSym] = exp0 match {
    case Expr.Cst(_, _, _) =>
      Set.empty

    case Expr.Var(_, _, _) =>
      Set.empty

    case Expr.Hole(_, _, _, _) =>
      Set.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ApplyDef(SymUse.DefSymUse(sym, loc), exps, _, _, _, _) =>
      sctx.useSites.add(sym -> loc)
      Set(ReachableSym.DefnSym(sym)) ++ visitExps(exps)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      visitExps(exps)

    case Expr.ApplySig(SymUse.SigSymUse(sym, loc), exps, _, _, _, _) =>
      sctx.useSites.add(sym -> loc)
      Set(ReachableSym.SigSym(sym)) ++ visitExps(exps)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Region(_, _) =>
      Set.empty

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      val exps = rules.flatMap(r => r.exp :: r.guard.toList)
      visitExp(exp) ++ visitExps(exps)

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      val exps = rules.map(_.exp)
      visitExp(exp) ++ visitExps(exps)

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      val exps = rules.map(_.exp)
      visitExp(exp) ++ visitExps(exps)

    case Expr.Tag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.StructNew(_, fields, _, _, _, _) =>
      visitExps(fields.map(_._2))

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      val exps = rules.map(_.exp)
      visitExp(exp) ++ visitExps(exps)

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      val exps = rules.map(_.exp)
      visitExps(exps)

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Do(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      visitExps(exps)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      Set.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      visitExps(methods.map(_.exp))

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val exps = rules.flatMap(r => r.exp :: r.chan :: Nil)
      visitExps(exps) ++ visitExps(default.toList)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      val exps = frags.map(_.exp)
      visitExps(exps) ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      val exps = cs.flatMap {
        case TypedAst.Constraint(_, TypedAst.Predicate.Head.Atom(_, _, exps1, _, _), body, _) =>
          val exps2 = body.flatMap {
            case Body.Atom(_, _, _, _, _, _, _) => List.empty
            case Body.Functional(_, exp, _) => List(exp)
            case Body.Guard(exp, _) => List(exp)
          }
          exps1 ::: exps2
      }
      visitExps(exps)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      Set.empty

  }

  /**
    * Returns the function symbols reachable from `exps`.
    */
  private def visitExps(exps: List[TypedAst.Expr])(implicit sym0: Symbol, sctx: SharedContext): Set[ReachableSym] = exps.map(visitExp).fold(Set())(_ ++ _)

  /**
    * A common super-type for reachable symbols (defs, traits, sigs)
    */
  private sealed trait ReachableSym

  private object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }

  private case class SharedContext(useSites: ConcurrentLinkedQueue[(Symbol, SourceLocation)])

  private object SharedContext {
    def mk(): SharedContext = SharedContext(new ConcurrentLinkedQueue())
  }
}
