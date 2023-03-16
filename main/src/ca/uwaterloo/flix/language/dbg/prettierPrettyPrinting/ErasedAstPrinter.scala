package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.Symbol._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers.OperatorPrinter

import scala.annotation.tailrec


object ErasedAstPrinter {

  /**
    * More principled than `pretty` but causes stackoverflow somehow.
    */
  private def doc(root: Root)(implicit i: Indent): Doc = {
    val defs = root.
      defs.
      toList.
      sortBy { case (sym, _) => sym.toString }.
      map { case (_, defn) => doc(defn) }
    group(fold(_ <> breakWith("") <> breakWith("") <> _, defs))
  }

  def pretty(root: Root, width: Int)(implicit i: Indent): String = {
    val defs = root.
      defs.
      toList.
      sortBy { case (sym, _) => sym.toString }.
      map { case (_, defn) => doc(defn) }.
      map(Doc.pretty(width, _))
    defs.mkString("\n\n")
  }

  def doc(defn: Def)(implicit i: Indent): Doc = {
    defnf(
      defn.sym.toString,
      defn.formals.map(doc),
      returnTypeDoc(defn.tpe),
      doc(defn.exp, paren = false)
    )
  }

  def doc(f: FormalParam)(implicit i: Indent): Doc = {
    paramf(text(f.sym.toString) <> text("%") <> text(f.sym.getStackOffset.toString), MonoTypePrinter.doc(f.tpe))
  }

  def doc(sym: VarSym): Doc = text(sym.toString) <> text("%") <> text(sym.getStackOffset.toString)

  def doc(exp: Expression, paren: Boolean = true)(implicit i: Indent): Doc = {
    def par(d: Doc): Doc = if (paren) parens(d) else d

    exp match {
      case Expression.Var(sym, _, _) =>
        val output = doc(sym)
        output
      case Expression.Unary(sop, exp, _, _) =>
        ???
      case Expression.Binary(sop, _, exp1, exp2, _, _) =>
        ???
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        val output = itef(
          doc(exp1, paren = false),
          doc(exp2, paren = false),
          doc(exp3, paren = false)
        )
        par(output)
      case Expression.Branch(exp, branches, tpe, loc) => text("unknown")
      case Expression.JumpTo(sym, tpe, loc) => text("unknown")
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val output = letf(doc(sym), Some(MonoTypePrinter.doc(tpe)), doc(exp1, paren = false), doc(exp2))
        output
      case Expression.LetRec(varSym, _, _, exp1, exp2, tpe, _) =>
        val output = letrecf(doc(varSym), Some(MonoTypePrinter.doc(tpe)), doc(exp1, paren = false), doc(exp2))
        output
      case Expression.Scope(sym, exp, tpe, loc) => text("unknown")
      case Expression.ScopeExit(exp1, exp2, tpe, loc) => text("unknown")
      case Expression.TryCatch(exp, rules, tpe, loc) => text("unknown")
      case Expression.NewObject(name, clazz, tpe, methods, loc) => text("unknown")
      case Expression.Intrinsic0(op, tpe, loc) => text("unknown")
      case Expression.Intrinsic1(op, exp, tpe, loc) => text("unknown")
      case Expression.Intrinsic2(op, exp1, exp2, tpe, loc) => text("unknown")
      case Expression.Intrinsic3(op, exp1, exp2, exp3, tpe, loc) => text("unknown")
      case Expression.IntrinsicN(op, exps, tpe, loc) => text("unknown")
      case Expression.Intrinsic1N(op, exp, exps, tpe, loc) => text("unknown")
    }


//    exp match {
//      case Expression.Cst(cst, _, _) =>
//        val output = ConstantPrinter.doc(cst)
//        output
//      case Expression.Closure(sym, closureArgs, _, _) =>
//        val output = applyf(doc(sym) <> metaText("buildclo"), closureArgs.map(doc(_)))
//        output
//      case Expression.ApplyClo(exp, args, _, _) =>
//        val output = applyf(doc(exp) <> metaText("clo"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyDef(sym, args, _, _) =>
//        val output = applyf(doc(sym) <> metaText("def"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyCloTail(exp, args, _, _) =>
//        val output = applyf(doc(exp) <> metaText("clotail"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplyDefTail(sym, args, _, _) =>
//        val output = applyf(doc(sym) <> metaText("deftail"), args.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.ApplySelfTail(sym, _, actuals, _, _) =>
//        val output = applyf(doc(sym) <> metaText("selftail"), actuals.map(a => doc(a, paren = false)))
//        par(output)
//      case Expression.Branch(exp, branches, tpe, loc) =>
//        val output = metaText("Branch")
//        output
//      case Expression.JumpTo(sym, tpe, loc) =>
//        val output = metaText("JumpTo")
//        output
//      case Expression.Region(_, _) =>
//        val output = metaText("Region")
//        output
//      case Expression.Scope(sym, exp, _, _) =>
//        val output = scopef(text(sym.toString), doc(exp, paren = false, inBlock = true))
//        par(output)
//      case Expression.Is(sym, exp, loc) => metaText("Is")
//      case Expression.Tag(sym, exp, tpe, loc) =>
//        val output = applyf(doc(sym), List(doc(exp)))
//        output
//      case Expression.Untag(sym, exp, tpe, loc) => metaText("Untag")
//      case Expression.Index(base, offset, _, _) =>
//        val output = tupleIndexf(doc(base), offset)
//        par(output)
//      case Expression.Tuple(elms, _, _) =>
//        val output = tuplef(elms.map(doc(_, paren = false)))
//        output
//      case Expression.RecordEmpty(_, _) =>
//        val output = emptyRecordf()
//        output
//      case Expression.RecordSelect(exp, field, _, _) =>
//        val output = recordSelectf(doc(exp), text(field.name))
//        par(output)
//      case e@Expression.RecordExtend(_, _, _, _, _) =>
//
//        @tailrec
//        def recordDoc(exp: Expression, fields: List[(Doc, Doc)]): Doc = exp match {
//          case Expression.RecordExtend(field, value, rest, _, _) =>
//            recordDoc(rest, (text(field.name), doc(value, paren = false)) :: fields)
//          case Expression.RecordEmpty(_, _) =>
//            recordExtendf(fields.reverse, None)
//          case other =>
//            recordExtendf(fields.reverse, Some(doc(other, paren = false)))
//        }
//
//        recordDoc(e, Nil)
//      case Expression.RecordRestrict(field, rest, tpe, loc) =>
//        val output = metaText("RecordRestrict")
//        output
//      case Expression.ArrayLit(elms, _, _) =>
//        val output = arrayListf(elms.map(doc(_, paren = false)))
//        output
//      case Expression.ArrayNew(elm, len, tpe, loc) => metaText("ArrayNew")
//      case Expression.ArrayLoad(base, index, tpe, loc) => metaText("ArrayLoad")
//      case Expression.ArrayStore(base, index, elm, tpe, loc) => metaText("ArrayStore")
//      case Expression.ArrayLength(base, tpe, loc) => metaText("ArrayLength")
//      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => metaText("ArraySlice")
//      case Expression.Ref(exp, _, _) =>
//        val output = par(text("ref") <+> doc(exp))
//        par(output)
//      case Expression.Deref(exp, _, _) =>
//        val output = par(text("deref") <+> doc(exp))
//        par(output)
//      case Expression.Assign(exp1, exp2, _, _) =>
//        val output = assignf(doc(exp1), doc(exp2))
//        par(output)
//      case Expression.Cast(exp, tpe, _) =>
//        val output = castf(doc(exp, paren = false), MonoTypePrinter.doc(tpe))
//        par(output)
//      case Expression.TryCatch(exp, rules, tpe, loc) => metaText("TryCatch")
//      case Expression.InvokeConstructor(constructor, args, tpe, loc) => metaText("InvokeConstructor")
//      case Expression.InvokeMethod(method, exp, args, _, _) =>
//        val output = applyJavaf(method, doc(exp), args.map(doc(_, paren = false)))
//        par(output)
//      case Expression.InvokeStaticMethod(method, args, _, _) =>
//        val output = applyStaticJavaf(method, args.map(doc(_, paren = false)))
//        output
//      case Expression.GetField(field, exp, tpe, loc) => metaText("GetField")
//      case Expression.PutField(field, exp1, exp2, tpe, loc) => metaText("PutField")
//      case Expression.GetStaticField(field, tpe, loc) => metaText("GetStaticField")
//      case Expression.PutStaticField(field, exp, tpe, loc) => metaText("PutStaticField")
//      case Expression.NewObject(name, clazz, tpe, methods, loc) => metaText("NewObject")
//      case Expression.Spawn(exp1, exp2, _, _) =>
//        val output = spawnf(doc(exp1, paren = false), doc(exp2))
//        par(output)
//      case Expression.Lazy(exp, _, _) =>
//        val output = text("lazy") <+> doc(exp)
//        par(output)
//      case Expression.Force(exp, _, _) =>
//        val output = text("force") <+> doc(exp)
//        par(output)
//      case Expression.HoleError(sym, _, _) =>
//        doc(sym)
//      case Expression.MatchError(_, _) => metaText("MatchError")
//      case Expression.BoxBool(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxInt8(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxInt16(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxInt32(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxInt64(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxChar(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxFloat32(exp, _) => text("box") <+> doc(exp)
//      case Expression.BoxFloat64(exp, _) => text("box") <+> doc(exp)
//      case Expression.UnboxBool(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxInt8(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxInt16(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxInt32(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxInt64(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxChar(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxFloat32(exp, _) => text("unbox") <+> doc(exp)
//      case Expression.UnboxFloat64(exp, _) => text("unbox") <+> doc(exp)
//      case _ => text("unknown")
//    }
  }

  def returnTypeDoc(tpe: MonoType)(implicit i: Indent): Doc = tpe match {
    case MonoType.Arrow(_, result) => MonoTypePrinter.doc(result)
    case _ => metaText("NoReturnType")
  }

  def doc(sym: HoleSym): Doc = text(sym.toString)

  def doc(sym: DefnSym): Doc = text(sym.toString)

  def doc(sym: CaseSym): Doc = text(sym.toString)

}
