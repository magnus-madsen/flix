package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.CallByValueAst.{Expr, Stmt}
import ca.uwaterloo.flix.language.ast.{CallByValueAst, ReducedAst, Type}
import ca.uwaterloo.flix.util.collection.MapOps


object Undo {

  def run(root: CallByValueAst.Root)(implicit flix: Flix): ReducedAst.Root = {
    val defs = MapOps.mapValues(root.defs)(visitDef)
    val enums = MapOps.mapValues(root.enums)(visitEnum)
    ReducedAst.Root(defs, enums, root.entryPoint, root.sources)
  }

  private def visitDef(d: CallByValueAst.Def): ReducedAst.Def = {
    val CallByValueAst.Def(ann, mod, sym, cparams0, fparams0, exp, tpe, loc) = d
    val cparams = cparams0.map(visitFormalParam)
    val fparams = fparams0.map(visitFormalParam)
    val body = visitStmt(exp)
    val e = ReducedAst.Stmt.Ret(body, body.tpe, body.loc)
    val purity = Type.Pure // TODO PURITY
    ReducedAst.Def(ann, mod, sym, cparams, fparams, e, tpe, purity, loc)
  }

  private def visitExp(expr: CallByValueAst.Expr): ReducedAst.Expr = expr match {
    case Expr.Cst(cst, tpe, loc) =>
      ReducedAst.Expr.Cst(cst, tpe, loc)
    case Expr.Var(sym, tpe, loc) =>
      ReducedAst.Expr.Var(sym, tpe, loc)
    case Expr.Closure(sym, closureArgs0, tpe, loc) =>
      val closureArgs = closureArgs0.map(visitExp)
      ??? // TODO CLOSURE?
//      ReducedAst.Expr.Closure(sym, closureArgs, tpe, loc)
    case Expr.TryCatch(exp, rules0, tpe, purity, loc) =>
      val e = visitStmt(exp)
      val rules = rules0.map(visitCatchRule)
      ReducedAst.Expr.TryCatch(e, rules, tpe, purity, loc)
    case Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod)
      ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)
    case Expr.ApplyAtomic(op, exps0, tpe, purity, loc) =>
      val exps = exps0.map(visitExp)
      ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc)
  }

  private def visitStmt(stmt: CallByValueAst.Stmt): ReducedAst.Expr = stmt match {
    case Stmt.Ret(exp, _, _, _) =>
      visitExp(exp)
    case Stmt.IfThenElse(exp, stmt1, stmt2, tpe, purity, loc) =>
      val e1 = visitExp(exp)
      val e2 = visitStmt(stmt1)
      val e3 = visitStmt(stmt2)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case Stmt.Branch(stmt, branches0, tpe, purity, loc) =>
      val e = visitStmt(stmt)
      val branches = MapOps.mapValues(branches0)(visitStmt)
      ReducedAst.Expr.Branch(e, branches, tpe, purity, loc)
    case Stmt.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)
    case Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc) =>
      val e1 = visitStmt(stmt1)
      val e2 = visitStmt(stmt2)
      ReducedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)
    case Stmt.LetRec(varSym, index, defSym, exp, stmt, tpe, purity, loc) =>
      val e1 = visitExp(exp)
      val e2 = visitStmt(stmt)
      ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)
    case Stmt.Scope(sym, stmt, tpe, purity, loc) =>
      val e = visitStmt(stmt)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)
    case Stmt.ApplyClo(exp, exps0, ct, tpe, purity, loc) =>
      val e = visitExp(exp)
      val exps = exps0.map(visitExp)
      ReducedAst.Expr.ApplyClo(e, exps, ct, tpe, purity, loc)
    case Stmt.ApplyDef(sym, exps0, ct, tpe, purity, loc) =>
      val exps = exps0.map(visitExp)
      ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc)
    case Stmt.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExp)
      ReducedAst.Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
  }

  private def visitFormalParam(p: CallByValueAst.FormalParam): ReducedAst.FormalParam = {
    val CallByValueAst.FormalParam(sym, mod, tpe, loc) = p
    ReducedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def visitCatchRule(rule: CallByValueAst.CatchRule): ReducedAst.CatchRule = rule match {
    case CallByValueAst.CatchRule(sym, clazz, exp) =>
      val e = visitStmt(exp)
      ReducedAst.CatchRule(sym, clazz, e)
  }

  private def visitJvmMethod(m: CallByValueAst.JvmMethod): ReducedAst.JvmMethod = {
    val CallByValueAst.JvmMethod(ident, fparams0, clo0, retTpe, purity, loc) = m
    val fparams = fparams0.map(visitFormalParam)
    val clo = visitExp(clo0)
    ReducedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc)
  }

  private def visitEnum(e: CallByValueAst.Enum): ReducedAst.Enum = {
    val CallByValueAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) = e
    val cases = MapOps.mapValues(cases0)(visitEnumCase)
    ReducedAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  private def visitEnumCase(c: CallByValueAst.Case): ReducedAst.Case = {
    val CallByValueAst.Case(sym, tpeDeprecated, loc) = c
    ReducedAst.Case(sym, tpeDeprecated, loc)
  }

}
