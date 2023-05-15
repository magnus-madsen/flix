package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.Expr
import ca.uwaterloo.flix.language.ast.{Ast, CallByValueAst, ReducedAst, Purity, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object ControlSeparator {

  private def todo: Nothing = throw InternalCompilerException("WIP", SourceLocation.Unknown)

  def run(root: ReducedAst.Root)(implicit flix: Flix): CallByValueAst.Root = flix.phase("ControlSeparator") {
    val ReducedAst.Root(defs0, enums0, entryPoint, sources) = root
    val defs = defs0.view.mapValues(visitDef).toMap
    val enums = enums0.view.mapValues(visitEnum).toMap
    CallByValueAst.Root(defs, enums, entryPoint, sources)
  }

  def visitDef(defn: ReducedAst.Def)(implicit flix: Flix): CallByValueAst.Def = {
    val ReducedAst.Def(ann, mod, sym, fparams0, ReducedAst.Stmt.Ret(exp, _, _), tpe, loc) = defn
    val fparams = fparams0.map(visitFormalParam)
    // important! reify bindings
    implicit val ctx: Context = new Context()
    val stmt = insertBindings(_ => visitExpAsStmt(exp))
    CallByValueAst.Def(ann, mod, sym, fparams, stmt, tpe, loc)
  }

  def visitEnum(e: ReducedAst.Enum): CallByValueAst.Enum = {
    val ReducedAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) = e
    val cases = cases0.view.mapValues(visitEnumCase).toMap
    CallByValueAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  def visitEnumCase(c: ReducedAst.Case): CallByValueAst.Case = {
    val ReducedAst.Case(sym, tpeDeprecated, loc) = c
    CallByValueAst.Case(sym, tpeDeprecated, loc)
  }

  // invariant: context will be unchanged
  def visitExpAsStmt(exp: ReducedAst.Expr)(implicit ctx: Context, flix: Flix): CallByValueAst.Stmt = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      insertBindings(_ => ret(CallByValueAst.Expr.Cst(cst, tpe, loc)))
    case Expr.Var(sym, tpe, loc) =>
      ret(CallByValueAst.Expr.Var(sym, tpe, loc))
    case Expr.Closure(sym, closureArgs0, tpe, loc) =>
      insertBindings(_ => {
        val closureArgs = closureArgs0.map(visitExpAsExpr)
        ret(CallByValueAst.Expr.Closure(sym, closureArgs, tpe, loc))
      })
    case Expr.ApplyAtomic(op, exps0, tpe, purity, loc) =>
      insertBindings{ _ =>
        val exps = exps0.map(visitExpAsExpr)
        ret(CallByValueAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc))
      }
    case Expr.ApplyClo(exp, exps0, ct, tpe, purity, loc) =>
      insertBindings(_ => {
        val e = visitExpAsExpr(exp)
        val exps = exps0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplyClo(e, exps, ct, tpe, purity, loc)
      })
    case Expr.ApplyDef(sym, exps0, ct, tpe, purity, loc) =>
      insertBindings(_ => {
        val exps = exps0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplyDef(sym, exps, ct, tpe, purity, loc)
      })
    case Expr.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      insertBindings(_ => {
        val formals = formals0.map(visitFormalParam)
        val actuals = actuals0.map(visitExpAsExpr)
        CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
      })
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      insertBindings(_ => {
        val e1 = visitExpAsExpr(exp1)
        val e2 = visitExpAsStmt(exp2)
        val e3 = visitExpAsStmt(exp3)
        CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      })
    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      todo
    case Expr.JumpTo(sym, tpe, purity, loc) =>
      todo
    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val stmt1 = visitExpAsStmt(exp1)
      val stmt2 = visitExpAsStmt(exp2)
      CallByValueAst.Stmt.LetVal(sym, stmt1, stmt2, tpe, purity, loc)
    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      todo
    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      todo
    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      todo
    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      todo
  }

  sealed trait Binding

  object Binding {
    case class Val(sym: Symbol.VarSym, binding: CallByValueAst.Stmt, tpe: Type, purity: Purity, loc: SourceLocation) extends Binding
  }

  /**
    * Knows how to bind.
    */
  class Context() {

    type Stack[a] = List[a]

    private var l: Stack[Binding] = Nil

    def bind(stmt: CallByValueAst.Stmt)(implicit flix: Flix): CallByValueAst.Expr.Var = {
      val loc = SourceLocation.Unknown
      val sym = Symbol.freshVarSym("cbv", Ast.BoundBy.Let, loc)
      l = Binding.Val(sym, stmt, stmt.tpe, stmt.purity, stmt.loc.asSynthetic) :: l
      CallByValueAst.Expr.Var(sym, stmt.tpe, stmt.loc)
    }

    def withBindings[R](f: Unit => R): (R, Stack[Binding]) = {
      // track fresh bindings
      val old = l
      l = Nil
      val res = f(())
      val bindings = l
      l = old
      (res, bindings)
    }

    def reifyBindings(stmt: CallByValueAst.Stmt, bindings: Stack[Binding]): CallByValueAst.Stmt = {
      bindings.foldLeft(stmt) {
        case (acc, Binding.Val(sym, binding, tpe, purity, loc)) =>
          CallByValueAst.Stmt.LetVal(sym, binding, acc, tpe, purity, loc)
      }
    }

  }

  def insertBindings(f: Unit => CallByValueAst.Stmt)(implicit ctx: Context): CallByValueAst.Stmt = {
    val (s, bindings) = ctx.withBindings(f)
    ctx.reifyBindings(s, bindings)
  }

  /**
    * Translates the lifted expression into a CBV expression.
    * This often entail let-binding.
    */
  def visitExpAsExpr(exp: ReducedAst.Expr)(implicit ctx: Context, flix: Flix): CallByValueAst.Expr = exp match {
    case ReducedAst.Expr.Cst(cst, tpe, loc) =>
      CallByValueAst.Expr.Cst(cst, tpe, loc)
    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      CallByValueAst.Expr.Var(sym, tpe, loc)
    case ReducedAst.Expr.Closure(sym, closureArgs0, tpe, loc) =>
      val closureArgs = closureArgs0.map(visitExpAsExpr)
      CallByValueAst.Expr.Closure(sym, closureArgs, tpe, loc)
    case ReducedAst.Expr.ApplyAtomic(op, exps0, tpe, purity, loc) =>
      val exps = exps0.map(visitExpAsExpr)
      CallByValueAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc)
    case ReducedAst.Expr.ApplyClo(exp, exps0, ct, tpe, purity, loc) =>
      val e = visitExpAsExpr(exp)
      val exps = exps0.map(visitExpAsExpr)
      val ac = CallByValueAst.Stmt.ApplyClo(e, exps, ct, tpe, purity, loc)
      ctx.bind(ac)
    case ReducedAst.Expr.ApplyDef(sym, exps0, ct, tpe, purity, loc) =>
      val exps = exps0.map(visitExpAsExpr)
      val ad = CallByValueAst.Stmt.ApplyDef(sym, exps, ct, tpe, purity, loc)
      ctx.bind(ad)
    case ReducedAst.Expr.ApplySelfTail(sym, formals0, actuals0, tpe, purity, loc) =>
      val formals = formals0.map(visitFormalParam)
      val actuals = actuals0.map(visitExpAsExpr)
      val adt = CallByValueAst.Stmt.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)
      ctx.bind(adt)
    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpAsExpr(exp1)
      val e2 = visitExpAsStmt(exp2)
      val e3 = visitExpAsStmt(exp3)
      val ite = CallByValueAst.Stmt.IfThenElse(e1, e2, e3, tpe, purity, loc)
      ctx.bind(ite)
    case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) => todo
    case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) => todo
    case ReducedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) => todo
    case ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => todo
    case ReducedAst.Expr.Scope(sym, exp, tpe, purity, loc) => todo
    case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) => todo
    case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) => todo
  }

  private def visitFormalParam(p: ReducedAst.FormalParam): CallByValueAst.FormalParam = {
    val ReducedAst.FormalParam(sym, mod, tpe, loc) = p
    CallByValueAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * Wrap an expression in a return statement.
    */
  private def ret(e: CallByValueAst.Expr): CallByValueAst.Stmt.Ret = {
    CallByValueAst.Stmt.Ret(e, e.tpe, e.purity, e.loc)
  }

}
