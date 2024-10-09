/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, LabelSym, VarSym}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst1, MonoAst, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * The occurrence analyzer collects information on variable and function usage and calculates the weight of the expressions
  * Marks a variable or function as Dead if it is not used, Once if it is used exactly once and Many otherwise
  */
object OccurrenceAnalyzer1 {

  object OccurInfo {

    /**
      * Occurrence information for an empty sequence of expressions
      */
    val Empty: OccurInfo = OccurInfo(Map.empty, Map.empty, 0)

    /**
      * The initial occurrence information for an expression of size 1, i.e. an expression without subexpressions.
      */
    val One: OccurInfo = OccurInfo(Map.empty, Map.empty, 1)
  }

  /**
    * The occurrence of `defs` and `vars` inside the body of a `def`
    * `size` represents the number of expressions in the body of a `def`
    */
  case class OccurInfo(defs: Map[DefnSym, Occur], vars: Map[VarSym, Occur], size: Int) { // TODO: Maybe add LocalDefs
    /**
      * Increments number of expressions by one
      */
    def increaseSizeByOne(): OccurInfo = this.copy(size = this.size + 1)
  }

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): OccurrenceAst1.Root = {
    val defs = visitDefs(root.defs)
    val structs = visitStructs(root.structs)
    val effects = visitEffects(root.effects)
    OccurrenceAst1.Root(defs, structs, effects, root.entryPoint, root.reachable, root.sources)
  }

  /**
    * Performs occurrence analysis on every entry in `defs0` in parallel.
    * Sets the occurrence of each `def` based on the occurrence found in `defOccur`. TODO: What does this mean?
    */
  private def visitDefs(defs0: Map[DefnSym, MonoAst.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst1.Def] = {
    val (ds, os) = ParOps.parMap(defs0.values)((d: MonoAst.Def) => visitDef(d)).unzip

    // Combine all `defOccurrences` into one map.
    val defOccur = combineAll(os)

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    ds.foldLeft(Map.empty[DefnSym, OccurrenceAst1.Def]) {
      case (macc, defn) => macc + (defn.sym -> defn.copy(context = defn.context.copy(occur = defOccur.getOrElse(defn.sym, Dead))))
    }
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn0: MonoAst.Def): (OccurrenceAst1.Def, OccurInfo) = {
    val (e, oi) = visitExp(defn0.sym, defn0.exp)
    val fparams = defn0.spec.fparams.map(visitFormalParam).map(p => p -> oi.vars.getOrElse(p.sym, Dead))
    // Def consists of a single direct call to a def
    val isDirectCall = e match { // TODO: Refactor into function, these are base cases along with ApplyLocalDef, add recursive case for LocalDef
      case OccurrenceAst1.Expr.ApplyDef(_, _, _, _, _, _) => true
      case OccurrenceAst1.Expr.ApplyClo(clo, _, _, _, _) =>
        clo match {
          case OccurrenceAst1.Expr.ApplyAtomic(AtomicOp.Closure(_), _, _, _, _) => true
          case _ => false
        }
      case _ => false

    }
    val isSelfRecursive = oi.defs.get(defn0.sym) match {
      case None => false
      case Some(o) => o match {
        case Occur.Dead => false
        case Occur.Once => true
        case Occur.Many => true
        case Occur.ManyBranch => true
        case Occur.DontInline => false
      }
    }
    val defContext = DefContext(isDirectCall, oi.defs.getOrElse(defn0.sym, Dead), oi.size, isSelfRecursive)
    val spec = visitSpec(defn0.spec)
    (OccurrenceAst1.Def(defn0.sym, fparams, spec, e, defContext), oi)
  }

  private def visitEffects(effects0: Map[Symbol.EffectSym, MonoAst.Effect]): Map[Symbol.EffectSym, OccurrenceAst1.Effect] = {
    effects0.map { case (k, v) => k -> visitEffect(v) }
  }

  private def visitEffect(effect0: MonoAst.Effect): OccurrenceAst1.Effect = effect0 match {
    case MonoAst.Effect(doc, ann, mod, sym, ops, loc) =>
      val os = ops.map(visitEffectOp)
      OccurrenceAst1.Effect(doc, ann, mod, sym, os, loc)
  }

  private def visitEffectOp(op0: MonoAst.Op): OccurrenceAst1.Op = op0 match {
    case MonoAst.Op(sym, spec) =>
      val sp = visitSpec(spec)
      val fps = spec.fparams.map(visitFormalParam)
      OccurrenceAst1.Op(sym, fps, sp)
  }

  private def visitSpec(spec0: MonoAst.Spec): OccurrenceAst1.Spec = spec0 match {
    case MonoAst.Spec(doc, ann, mod, _, functionType, retTpe, eff, loc) =>
      OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff, loc)
  }

  private def visitStructs(structs0: Map[Symbol.StructSym, MonoAst.Struct]): Map[Symbol.StructSym, OccurrenceAst1.Struct] = ???

  /**
    * Translates the given formal param `p` to the OccurrenceAst1.
    */
  private def visitFormalParam(fparam0: MonoAst.FormalParam): OccurrenceAst1.FormalParam = fparam0 match {
    case MonoAst.FormalParam(sym, mod, tpe, src, loc) =>
      OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Performs occurrence analysis on `exp0`
    */
  private def visitExp(sym0: Symbol.DefnSym, exp0: MonoAst.Expr): (OccurrenceAst1.Expr, OccurInfo) = exp0 match { // TODO (refactor): Add local function `visit` that captures `sym0`
    case Expr.Cst(cst, tpe, loc) => (OccurrenceAst1.Expr.Cst(cst, tpe, loc), OccurInfo.One)

    case Expr.Var(sym, tpe, loc) => (OccurrenceAst1.Expr.Var(sym, tpe, loc), OccurInfo(Map.empty, Map(sym -> Once), 1))

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      // TODO: Map every Once occurrence to OnceInLambda
      val fps = visitFormalParam(fparam)
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst1.Expr.Lambda(fps, e, tpe, loc), o)

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val (es, o) = visitExps(sym0, exps)
      val o1 = op match {
        case AtomicOp.Is(sym) if sym.name == "Choice" => o.copy(defs = o.defs + (sym0 -> DontInline)).increaseSizeByOne()
        case _ => o.increaseSizeByOne()
      }
      (OccurrenceAst1.Expr.ApplyAtomic(op, es, tpe, purity, loc), o1)

    case Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (es, o2) = visitExps(sym0, exps)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expr.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _) =>
          val o4 = OccurInfo(Map(sym -> Once), Map.empty, 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst1.Expr.ApplyClo(e, es, tpe, purity, loc), o5.increaseSizeByOne())
        case _ => (OccurrenceAst1.Expr.ApplyClo(e, es, tpe, purity, loc), o3.increaseSizeByOne())
      }

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      val o2 = OccurInfo(Map(sym -> Once), Map.empty, 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), o3.increaseSizeByOne())

    case Expr.ApplyLocalDef(sym, exps, tpe, purity, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      val o2 = OccurInfo(Map.empty, Map(sym -> Once), 1)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.ApplyLocalDef(sym, es, tpe, purity, loc), o3)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val (e3, o3) = visitExp(sym0, exp3)
      val o4 = combineAllSeq(o1, combineAllBranch(o2, o3))
      (OccurrenceAst1.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc), o4.increaseSizeByOne())

    case Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.vars.getOrElse(sym, Dead)
      val o4 = o3.copy(vars = o3.vars - sym)
      (OccurrenceAst1.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), o4.increaseSizeByOne())

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      // TODO: Figure out if this is correct...
      val fps = fparams.map(visitFormalParam)
      val (e1, o1) = visitExp(sym0, exp1) // TODO: Map every Once to OnceInLocalDef unless they are bound by formal params
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.vars.getOrElse(sym, Dead)
      val o4 = o3.copy(vars = o3.vars - sym)
      (OccurrenceAst1.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), o4.increaseSizeByOne())

    case Expr.Stm(exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.Stm(e1, e2, tpe, purity, loc), o3.increaseSizeByOne())

    case Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val withSym0 = o1.defs + (sym0 -> DontInline)
      val o2 = o1.copy(defs = withSym0).increaseSizeByOne()
      (OccurrenceAst1.Expr.Scope(sym, rvar, e, tpe, eff, loc), o2)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (rs, o2) = rules.map { // TODO: add helper function
        case MonoAst.CatchRule(sym, clazz, exp) =>
          val (e, o3) = visitExp(sym0, exp)
          (OccurrenceAst1.CatchRule(sym, clazz, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      val withSym0 = o4.defs + (sym0 -> DontInline)
      val o5 = o4.copy(defs = withSym0).increaseSizeByOne()
      (OccurrenceAst1.Expr.TryCatch(e, rs, tpe, purity, loc), o5)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (rs, o2) = rules.map { // TODO: add helper function
        case MonoAst.HandlerRule(op, fparams, exp) =>
          val (e, o3) = visitExp(sym0, exp)
          val fps = fparams.map(visitFormalParam)
          (OccurrenceAst1.HandlerRule(op, fps, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      (OccurrenceAst1.Expr.TryWith(e, effUse, rs, tpe, purity, loc), o4.copy(defs = o4.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      (OccurrenceAst1.Expr.Do(op, es, tpe, eff, loc), o1.increaseSizeByOne())

    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val (ms, o1) = methods.map { // TODO: add helper function
        case MonoAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map(visitFormalParam)
          val (c, o) = visitExp(sym0, clo)
          (OccurrenceAst1.JvmMethod(ident, f, c, retTpe, purity, loc), o.increaseSizeByOne())

      }.unzip
      val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
      (OccurrenceAst1.Expr.NewObject(name, clazz, tpe, purity, ms, loc), o2.increaseSizeByOne())

  }

  /**
    * Performs occurrence analysis on a list of expressions `exps`` and merges occurrences.
    */
  private def visitExps(sym0: Symbol.DefnSym, exps: List[MonoAst.Expr]): (List[OccurrenceAst1.Expr], OccurInfo) = {
    val (es, o1) = exps.map(visitExp(sym0, _)).unzip
    val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
    (es, o2)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAllBranch(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineBranch)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAllSeq(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineSeq)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAll(o1: OccurInfo, o2: OccurInfo, combine: (Occur, Occur) => Occur): OccurInfo = {
    val varMap = combineMaps(o1.vars, o2.vars, combine)
    val defMap = combineMaps(o1.defs, o2.defs, combine)
    val size = o1.size + o2.size
    OccurInfo(defMap, varMap, size)
  }

  /**
    * Combines maps `m1` and `m2` of the type (A -> Occur) into a single map of the same type.
    */
  private def combineMaps[A](m1: Map[A, Occur], m2: Map[A, Occur], combine: (Occur, Occur) => Occur): Map[A, Occur] = {
    val (smallest, largest) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    smallest.foldLeft[Map[A, Occur]](largest) {
      case (acc, (k, v)) =>
        val occur = combine(v, acc.getOrElse(k, Dead))
        acc + (k -> occur)
    }
  }

  /**
    * Combines all [[OccurInfo]] in `os` and maps each [[DefnSym]] to its corresponding [[OccurInfo]].
    */
  private def combineAll(os: Iterable[OccurInfo]): Map[DefnSym, Occur] = {
    os.foldLeft(Map.empty[DefnSym, Occur])((acc, o) => combineMaps(acc, o.defs, combineSeq))
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence.
    */
  private def combineSeq(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence based on ManyBranches logic.
    * ManyBranches can be IfThenElse, Branches, and SelectChannel
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case (Once, Once) => ManyBranch
    case (Once, ManyBranch) => ManyBranch
    case (ManyBranch, Once) => ManyBranch
    case (ManyBranch, ManyBranch) => ManyBranch
    case _ => Many
  }
}


