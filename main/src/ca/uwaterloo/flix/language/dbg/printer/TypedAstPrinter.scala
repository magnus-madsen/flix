package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.dbg.DocAst

object TypedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: TypedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case TypedAst.Enum(_, ann, mod, sym, tparams, _, cases, _, _) =>
        DocAst.Enum(ann, mod, sym, tparams.map(printTypeParam), cases.values.map(printCase).toList)
    }.toList
    val defs = root.defs.values.map {
      case TypedAst.Def(sym, TypedAst.Spec(_, ann, mod, _, fparams, _, retTpe, eff, _, _, _), exp) =>
        DocAst.Def(ann, mod, sym, fparams.map(printFormalParam), TypePrinter.print(retTpe), TypePrinter.printAsEffect(eff), print(exp))
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  private def print(e: TypedAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => printVar(sym)
    case Expr.Def(sym, _, _) => DocAst.Expr.Def(sym)
    case Expr.Sig(sym, tpe, loc) => DocAst.Expr.Unknown
    case Expr.Hole(sym, _, _) => DocAst.Expr.Hole(sym)
    case Expr.HoleWithExp(exp, _, _, _) => DocAst.Expr.HoleWithExp(print(exp))
    case Expr.OpenAs(symUse, exp, tpe, loc) => DocAst.Expr.Unknown
    case Expr.Use(sym, alias, exp, loc) => DocAst.Expr.Unknown
    case Expr.Lambda(fparam, exp, _, _) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.Apply(exp, exps, _, _, _) => DocAst.Expr.App(print(exp), exps.map(print))
    case Expr.Unary(sop, exp, _, _, _) => DocAst.Expr.Unary(OpPrinter.print(sop), print(exp))
    case Expr.Binary(sop, exp1, exp2, _, _, _) => DocAst.Expr.Binary(print(exp1), OpPrinter.print(sop), print(exp2))
    case Expr.Let(sym, _, exp1, exp2, _, _, _) => DocAst.Expr.Let(printVar(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.LetRec(sym, _, _, exp1, exp2, _, _, _) => DocAst.Expr.LetRec(printVar(sym), Some(TypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.Region(_, _) => DocAst.Expr.Region
    case Expr.Scope(sym, _, exp, _, _, _) => DocAst.Expr.Scope(printVar(sym), print(exp))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, _, _) => DocAst.Expr.Discard(print(exp))
    case Expr.Match(exp, rules, _, _, _) => DocAst.Expr.Match(print(exp), rules.map(printMatchRule))
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Tag(sym, exp, tpe, _, _) => DocAst.Expr.Tag(sym.sym, List(print(exp)))
    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Tuple(elms, _, _, _) => DocAst.Expr.Tuple(elms.map(print))
    case Expr.RecordEmpty(_, _) => DocAst.Expr.RecordEmpty
    case Expr.RecordSelect(exp, label, _, _, _) => DocAst.Expr.RecordSelect(label, print(exp))
    case Expr.RecordExtend(label, exp1, exp2, _, _, _) => DocAst.Expr.RecordExtend(label, print(exp1), print(exp2))
    case Expr.RecordRestrict(label, exp, _, _, _) => DocAst.Expr.RecordRestrict(label, print(exp))
    case Expr.ArrayLit(exps, exp, _, _, _) => DocAst.Expr.InRegion(DocAst.Expr.ArrayLit(exps.map(print)), print(exp))
    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) => DocAst.Expr.InRegion(DocAst.Expr.ArrayNew(print(exp1), print(exp2)), print(exp3))
    case Expr.ArrayLoad(exp1, exp2, _, _, _) => DocAst.Expr.ArrayLoad(print(exp1), print(exp2))
    case Expr.ArrayLength(exp, _, _) => DocAst.Expr.ArrayLength(print(exp))
    case Expr.ArrayStore(exp1, exp2, exp3, _, _) => DocAst.Expr.ArrayStore(print(exp1), print(exp2), print(exp3))
    case Expr.StructNew(sym, fields, region, tpe, _, _) => DocAst.Expr.StructNew(sym, fields.map {case (k, v) => (k.sym, print(v))}, print(region))
    case Expr.StructGet(exp, field, tpe, _, _) => DocAst.Expr.StructGet(print(exp), field)
    case Expr.StructPut(exp1, field, exp2, tpe, _, _) => DocAst.Expr.StructPut(print(exp1), field, print(exp2))
    case Expr.VectorLit(exps, _, _, _) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, _, _, _) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, _) => DocAst.Expr.VectorLength(print(exp))
    case Expr.Ascribe(exp, tpe, _, _) => DocAst.Expr.Ascription(print(exp), TypePrinter.print(tpe))
    case Expr.InstanceOf(exp, clazz, _) => DocAst.Expr.InstanceOf(print(exp), clazz)
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Without(exp, effUse, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map(printCatchRule))
    case Expr.Throw(exp, _, _, _) => DocAst.Expr.Throw(print(exp))
    case Expr.TryWith(exp, _Use, rules, _, _, _) => DocAst.Expr.TryWith(print(exp), _Use.sym, rules.map(printHandlerRule))
    case Expr.Do(op, exps, _, _, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.InvokeConstructor(constructor, exps, _, _, _) => DocAst.Expr.JavaInvokeConstructor(constructor, exps.map(print))
    case Expr.InvokeMethod(method, exp, exps, _, _, _) => DocAst.Expr.JavaInvokeMethod(method, print(exp), exps.map(print))
    case Expr.InvokeStaticMethod(method, exps, _, _, _) => DocAst.Expr.JavaInvokeStaticMethod(method, exps.map(print))
    case Expr.GetField(field, exp, _, _, _) => DocAst.Expr.JavaGetField(field, print(exp))
    case Expr.PutField(field, exp1, exp2, _, _, _) => DocAst.Expr.JavaPutField(field, print(exp1), print(exp2))
    case Expr.GetStaticField(field, _, _, _) => DocAst.Expr.JavaGetStaticField(field)
    case Expr.PutStaticField(field, exp, _, _, _) => DocAst.Expr.JavaPutStaticField(field, print(exp))
    case Expr.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expr.NewObject(name, clazz, TypePrinter.print(tpe), methods.map(printJvmMethod))
    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.GetChannel(exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Spawn(exp1, exp2, _, _, _) => DocAst.Expr.Spawn(print(exp1), print(exp2))
    case Expr.ParYield(frags, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Lazy(exp, _, _) => DocAst.Expr.Lazy(print(exp))
    case Expr.Force(exp, _, _, _) => DocAst.Expr.Force(print(exp))
    case Expr.FixpointConstraintSet(cs, tpe, loc) => DocAst.Expr.Unknown
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.FixpointSolve(exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => DocAst.Expr.Unknown
    case Expr.Error(_, _, _) => DocAst.Expr.Error
  }

  /**
    * Returns the [[DocAst.JvmMethod]] representation of `method`.
    */
  private def printJvmMethod(method: TypedAst.JvmMethod): DocAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
      DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printHandlerRule(rule: TypedAst.HandlerRule): (Symbol.OpSym, List[DocAst.Expr.Ascription], DocAst.Expr) = rule match {
    case TypedAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam), print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printCatchRule(rule: TypedAst.CatchRule): (Symbol.VarSym, Class[_], DocAst.Expr) = rule match {
    case TypedAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printMatchRule(rule: TypedAst.MatchRule): (DocAst.Expr, Option[DocAst.Expr], DocAst.Expr) = rule match {
    case TypedAst.MatchRule(pat, guard, exp) => (printPattern(pat), guard.map(print), print(exp))
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `pattern`.
    */
  private def printPattern(pattern: TypedAst.Pattern): DocAst.Expr = pattern match {
    case Pattern.Wild(_, _) => DocAst.Expr.Wild
    case Pattern.Var(sym, _, _) => printVar(sym)
    case Pattern.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Pattern.Tag(sym, pat, _, _) => DocAst.Expr.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(elms, _, _) => DocAst.Expr.Tuple(elms.map(printPattern))
    case Pattern.Record(pats, pat, _, _) => printRecordPattern(pats, pat)
    case Pattern.RecordEmpty(_, _) => DocAst.Expr.RecordEmpty
    case Pattern.Error(_, _) => DocAst.Expr.Error
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `pats`.
    */
  private def printRecordPattern(pats: List[Record.RecordLabelPattern], pat: Pattern): DocAst.Expr = {
    pats.foldRight(printPattern(pat)) {
      case (TypedAst.Pattern.Record.RecordLabelPattern(label, _, pat, _), acc) =>
        DocAst.Expr.RecordExtend(label, printPattern(pat), acc)
    }
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVar(sym: Symbol.VarSym): DocAst.Expr = {
    DocAst.Expr.Var(sym)
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: TypedAst.FormalParam): DocAst.Expr.Ascription = {
    val TypedAst.FormalParam(sym, _, tpe, _, _) = fp
    DocAst.Expr.Ascription(DocAst.Expr.Var(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Case]] representation of `caze`.
    */
  private def printCase(caze: TypedAst.Case): DocAst.Case = caze match {
    case TypedAst.Case(sym, tpe, _, _) => DocAst.Case(sym, TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: TypedAst.TypeParam): DocAst.TypeParam = tp match {
    case TypedAst.TypeParam(_, sym, _) => DocAst.TypeParam(sym)
  }

}
