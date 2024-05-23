package ca.uwaterloo.flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern}

import scala.annotation.tailrec

object MutationReporter {

  @tailrec
  def reportNonKilledMutation(exp: TypedAst.Expr): String = {
    exp match {
      case Expr.Mutated(mutExp, originalExp, tpe, eff, loc) => mutExp match {
        case Expr.Mutated(_, _, _, _, _) => reportNonKilledMutation(mutExp)
        case e => s"surviving mutation: ${prettyPrint(e)},\nloc: ${e.loc}"
      }
      case e => e.toString
    }
  }

  private def prettyPrint(exp: Expr): String = {
      exp match {
        case Expr.Cst(cst, _, loc) => cst.toString
        case Expr.Var(sym, tpe, loc) => sym.text
        case Expr.Def(sym, tpe, loc) => sym.name
        case Expr.Sig(sym, tpe, loc) => sym.name
        case Expr.Hole(sym, tpe, loc) => "Hole :("
        case Expr.HoleWithExp(exp, tpe, eff, loc) => "HoleWithExp :("
        case Expr.OpenAs(symUse, exp, tpe, loc) => "OpenAs :("
        case Expr.Use(sym, alias, exp, loc) => s"Use ${prettyPrint(exp)}"
        case Expr.Lambda(fparam, exp, tpe, loc) =>
          s"(${fparam.sym.text} -> ${prettyPrint(exp)})"
        case Expr.Apply(exp, exps, tpe, eff, loc) =>
          val params = exps.map(e => s"${prettyPrint(e)}, ").mkString
          s"${prettyPrint(exp)}($params)"
        case Expr.Unary(sop, exp, tpe, eff, loc) => "Unary :("
        case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => "Binary :("
        case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => s"let ${sym.toString} = ${prettyPrint(exp2)}" // idk if it should be exp1 or exp2
        case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
          s"def ${sym.text} = {${prettyPrint(exp1)}}\n\t${prettyPrint(exp2)}"
        case Expr.Region(tpe, loc) => "Region :("
        case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => s"${sym.toString} {${prettyPrint(exp)}}"
        case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => s"if (${prettyPrint(exp1)}) ${prettyPrint(exp2)} else ${prettyPrint(exp3)}"
        case Expr.Stm(exp1, exp2, tpe, eff, loc) => s"{${prettyPrint(exp1)}; ${prettyPrint(exp2)}"
        case Expr.Discard(exp, eff, loc) => "Discard :("
        case Expr.Match(exp, rules, tpe, eff, loc) =>
          s"\nmatch ${prettyPrint(exp)} {\n${rules.map(r => s"${printMatchRule(r)}\n").mkString("")}}"
        case Expr.TypeMatch(exp, rules, tpe, eff, loc) => "typeMatch"
        case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => "RestrictableChoose :("
        case Expr.Tag(sym, exp, tpe, eff, loc) => s"${sym.toString}(${prettyPrint(exp)})"
        case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => "RestrictableTag :("
        case Expr.Tuple(elms, tpe, eff, loc) => s"(${elms.map(e => s"${prettyPrint(e)},")})"
        case Expr.RecordEmpty(tpe, loc) => ""
        case Expr.RecordSelect(exp, label, tpe, eff, loc) => s"${prettyPrint(exp)}.${label.toString}"
        case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => s"recordExtend: ${label.toString}, exp1: ${prettyPrint(exp1)}, exp2: ${prettyPrint(exp2)}"
        case Expr.RecordRestrict(label, exp, tpe, eff, loc) => "RecordRestrict :("
        case Expr.ArrayLit(exps, exp, tpe, eff, loc) => s"ArrayLit: exps: ${exps.map{e => prettyPrint(e)}}, exp: ${prettyPrint(exp)}"
        case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => "ArrayNew :("
        case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => "ArrayLoad :("
        case Expr.ArrayLength(exp, eff, loc) => "ArrayLength :("
        case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => "ArrayStore :("
        case Expr.VectorLit(exps, tpe, eff, loc) => s"VectorLit: ${exps.map(e => prettyPrint(e))}"
        case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => "VectorLoad :("
        case Expr.VectorLength(exp, loc) => "VectorLength :("
        case Expr.Ref(exp1, exp2, tpe, eff, loc) => "Ref :("
        case Expr.Deref(exp, tpe, eff, loc) => "Deref :("
        case Expr.Assign(exp1, exp2, tpe, eff, loc) => "Assign :("
        case Expr.Ascribe(exp, tpe, eff, loc) => "Ascribe :("
        case Expr.InstanceOf(exp, clazz, loc) => "InstanceOf :("
        case Expr.CheckedCast(cast, exp, tpe, eff, loc) => "CheckedCast :("
        case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => "UncheckedCast :("
        case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => "UncheckedMaskingCast :("
        case Expr.Without(exp, effUse, tpe, eff, loc) => "Without :("
        case Expr.TryCatch(exp, rules, tpe, eff, loc) => "TryCatch :("
        case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => "TryWith :("
        case Expr.Do(op, exps, tpe, eff, loc) => "Do :("
        case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => "InvokeConstructor :("
        case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => "InvokeMethod :("
        case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => "InvokeStaticMethod :("
        case Expr.GetField(field, exp, tpe, eff, loc) => "GetField :("
        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => "PutField :("
        case Expr.GetStaticField(field, tpe, eff, loc) => "GetStaticField :("
        case Expr.PutStaticField(field, exp, tpe, eff, loc) => "PutStaticField :("
        case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => "NewObject :("
        case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => "NewChannel :("
        case Expr.GetChannel(exp, tpe, eff, loc) => "GetChannel :("
        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => "PutChannel :("
        case Expr.SelectChannel(rules, default, tpe, eff, loc) => "SelectChannel :("
        case Expr.Spawn(exp1, exp2, tpe, eff, loc) => "Spawn :("
        case Expr.ParYield(frags, exp, tpe, eff, loc) => "ParYield :("
        case Expr.Lazy(exp, tpe, loc) => "Lazy :("
        case Expr.Force(exp, tpe, eff, loc) => "Force :("
        case Expr.FixpointConstraintSet(cs, tpe, loc) => "FixpointConstraintSet :("
        case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => "FixpointLambda :("
        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => "FixpointMerge :("
        case Expr.FixpointSolve(exp, tpe, eff, loc) => "FixpointSolve :("
        case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => "FixpointFilter :("
        case Expr.FixpointInject(exp, pred, tpe, eff, loc) => "FixpointInject :("
        case Expr.FixpointProject(pred, exp, tpe, eff, loc) => "FixPointProject :("
        case Expr.Error(m, tpe, eff) => "Error :("
        case Expr.Mutated(mut,_,_,_,_) =>  prettyPrint(mut)
      }
  }
  private def printMatchRule(rule: TypedAst.MatchRule): String = {
    val pattern = printPattern(rule.pat)
    val exp = prettyPrint(rule.exp)
    s"\tcase $pattern => $exp"
  }
  private def printPattern(pattern: TypedAst.Pattern): String = {
    pattern match {
      case Pattern.Wild(tpe, loc) => "_"
      case Pattern.Var(sym, tpe, loc) => sym.text
      case Pattern.Cst(cst, tpe, loc) => cst.toString
      case Pattern.Tag(sym, pat, tpe, loc) => s"${sym.sym.name}(${printPattern(pat)})"
      case Pattern.Tuple(elms, tpe, loc) => s"(${elms.map(e => s"(${printPattern(e)})").mkString("")}"
      case Pattern.Record(pats, pat, tpe, loc) => tpe.toString
      case Pattern.RecordEmpty(tpe, loc) => tpe.toString
      case Pattern.Error(tpe, loc) => tpe.toString
    }
  }
}
