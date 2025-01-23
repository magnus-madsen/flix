package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.SymUse

import java.nio.file.{Files, Path}

object EffectLock {

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getEffectLockFile(p: Path): Path = p.resolve("./effect.lock").normalize()

  def effectLock(path: Path, bootstrap: Bootstrap, flix: Flix): Validation[Unit, BootstrapError] = {
    // TODO: Refactor this to callee (Main) and pass the root and Flix instance to this function instead
    bootstrap.check(flix) match {
      case Validation.Failure(errors) => Validation.Failure(errors)
      case Validation.Success(root) =>
        val outputStream = Files.newOutputStream(getEffectLockFile(path))
        val signatures = mkEffectLockSignatures(root).mkString("\n") // TODO: make JSON
        Validation.Success(outputStream.write(signatures.getBytes))
    }
  }


  def mkEffectLockSignatures(root: TypedAst.Root): List[String] = {
    root.defs.map {
      // TODO: Consider types / type schemes and only reachable functions / visible functions
      case (sym, defn) => s"$sym:${defn.spec}"
    }.toList
  }


  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = {
    // Entry points are always reachable.
    val initReach: Set[ReachableSym] = root.entryPoints.map(ReachableSym.DefnSym.apply)

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReach(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val reachableDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.DefnSym(sym))
    }

    // Reassemble the AST.
    root.copy(defs = reachableDefs)
  }

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    *
    * This includes three types of symbols:
    *   - (a) The function or signature symbols in the implementation / body expression of a reachable function symbol
    *   - (b) The trait symbol of a reachable sig symbol.
    *   - (c) Every expression in a trait instance of a reachable trait symbol is reachable.
    */
  private def visitSym(sym: ReachableSym, root: TypedAst.Root): Set[ReachableSym] = sym match {
    case ReachableSym.DefnSym(defnSym) =>
      visitExp(root.defs(defnSym).exp)

    case ReachableSym.SigSym(sigSym) =>
      val sig = root.sigs(sigSym)
      Set(ReachableSym.TraitSym(sig.sym.trt)) ++
        sig.exp.map(visitExp).getOrElse(Set.empty)

    case ReachableSym.TraitSym(traitSym) =>
      root.instances(traitSym).foldLeft(Set.empty[ReachableSym]) {
        case (acc, s) => visitExps(s.defs.map(_.exp)) ++ acc
      }
  }

  /**
    * Returns the function and signature symbols reachable from the given expression `exp0`.
    */
  private def visitExp(exp0: TypedAst.Expr): Set[ReachableSym] = exp0 match {
    case Expr.Cst(_, _, _) => Set.empty
    case Expr.Var(sym, tpe, loc) => Set.empty
    case Expr.Hole(sym, tpe, eff, loc) => Set.empty
    case Expr.HoleWithExp(exp, tpe, eff, loc) => visitExp(exp)
    case Expr.OpenAs(symUse, exp, tpe, loc) => visitExp(exp)
    case Expr.Use(sym, alias, exp, loc) => visitExp(exp)
    case Expr.Lambda(fparam, exp, tpe, loc) => visitExp(exp)
    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)

    case Expr.ApplyDef(SymUse.DefSymUse(sym, _), exps, _, _, _, _) =>
      Set(ReachableSym.DefnSym(sym)) ++ visitExps(exps)

    case Expr.ApplyLocalDef(_, exps, arrowTpe, tpe, eff, loc) =>
      visitExps(exps)

    case Expr.ApplySig(SymUse.SigSymUse(sym, _), exps, itpe, tpe, eff, loc) =>
      Set(ReachableSym.SigSym(sym)) ++ visitExps(exps)

    case Expr.Unary(sop, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.Region(tpe, loc) => Set.empty
    case Expr.Scope(bnd, regionVar, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.Discard(exp, eff, loc) => visitExp(exp)
    case Expr.Match(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.map(???)
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.map(???)
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.map(???)
    case Expr.Tag(sym, exps, tpe, eff, loc) => visitExps(exps)
    case Expr.RestrictableTag(sym, exps, tpe, eff, loc) => visitExps(exps)
    case Expr.Tuple(exps, tpe, eff, loc) => visitExps(exps)
    case Expr.RecordSelect(exp, label, tpe, eff, loc) => visitExp(exp)
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => visitExps(exps) ++ visitExp(exp)
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.ArrayLength(exp, eff, loc) => visitExp(exp)
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
    case Expr.StructNew(sym, fields, region, tpe, eff, loc) => visitExps(fields.map(_._2))
    case Expr.StructGet(exp, sym, tpe, eff, loc) => visitExp(exp)
    case Expr.StructPut(exp1, sym, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.VectorLit(exps, tpe, eff, loc) => visitExps(exps)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.VectorLength(exp, loc) => visitExp(exp)
    case Expr.Ascribe(exp, tpe, eff, loc) => visitExp(exp)
    case Expr.InstanceOf(exp, clazz, loc) => visitExp(exp)
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => visitExp(exp)
    case Expr.Unsafe(exp, runEff, tpe, eff, loc) => visitExp(exp)
    case Expr.Without(exp, sym, tpe, eff, loc) => visitExp(exp)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ rules.map(???)
    case Expr.Throw(exp, tpe, eff, loc) => visitExp(exp)
    case Expr.Handler(sym, rules, bodyType, bodyEff, handledEff, tpe, loc) => visitExps(rules.map(???))
    case Expr.RunWith(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.Do(op, exps, tpe, eff, loc) => visitExps(exps)
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => visitExps(exps)
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => visitExp(exp) ++ visitExps(exps)
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => visitExps(exps)
    case Expr.GetField(field, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
    case Expr.GetStaticField(field, tpe, eff, loc) => Set.empty
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => visitExp(exp)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => visitExps(methods.map(_.exp))
    case Expr.NewChannel(exp, tpe, eff, loc) => ???
    case Expr.GetChannel(exp, tpe, eff, loc) => ???
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => ???
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ParYield(frags, exp, tpe, eff, loc) => ???
    case Expr.Lazy(exp, tpe, loc) => ???
    case Expr.Force(exp, tpe, eff, loc) => ???
    case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => ???
    case Expr.FixpointSolve(exp, tpe, eff, loc) => ???
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => ???
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => ???
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => ???
    case Expr.Error(m, tpe, eff) => ???
  }

  /**
    * Returns the function symbols reachable from `exps`.
    */
  private def visitExps(exps: List[TypedAst.Expr]): Set[ReachableSym] = exps.map(visitExp).fold(Set())(_ ++ _)


  /**
    * A common super-type for reachable symbols (defs, traits, sigs)
    */
  sealed trait ReachableSym

  object ReachableSym {

    case class DefnSym(sym: Symbol.DefnSym) extends ReachableSym

    case class TraitSym(sym: Symbol.TraitSym) extends ReachableSym

    case class SigSym(sym: Symbol.SigSym) extends ReachableSym

  }
}
