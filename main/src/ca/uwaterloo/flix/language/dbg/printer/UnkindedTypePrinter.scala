package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{TypeConstructor, UnkindedType}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Type

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object UnkindedTypePrinter {

  /** Returns the [[DocAst.Type]] representation of `tpe`. */
  def print(tpe: UnkindedType): DocAst.Type = {
    val (base, args) = collectApp(tpe)
    // Make the well-kinded types pretty.
    (base, args) match {
      case (UnkindedType.Var(sym, _), _) => mkApp(Type.Var(sym), args.map(print))
      case (UnkindedType.Cst(TypeConstructor.Arrow(arity), _), _) if args.lengthIs == arity + 1 && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as `List(ef, a1, a2, .., b)`
        // safe match because of the case guard
        val (arrowEff :: arrowArgs, List(arrowRes)) = args.splitAt(args.length - 1)
        Type.ArrowEff(arrowArgs.map(print), print(arrowRes), print(arrowEff))
      case (UnkindedType.Cst(TypeConstructor.ArrowWithoutEffect(arity), _), _) if args.lengthIs == arity && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as `List(ef, a1, a2, .., b)`
        // safe match because of the case guard
        val (arrowArgs, List(arrowRes)) = args.splitAt(args.length - 1)
        Type.Arrow(arrowArgs.map(print), print(arrowRes))
      case (UnkindedType.Cst(TypeConstructor.RecordRowExtend(label), _), List(arg0, arg1)) =>
        Type.RecordRowExtend(label.toString, print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Record, _), List(arg0)) =>
        DocAst.Type.RecordOf(print(arg0))
      case (UnkindedType.Cst(TypeConstructor.SchemaRowExtend(pred), _), List(arg0, arg1)) =>
        DocAst.Type.SchemaRowExtend(pred.toString, print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Schema, _), List(arg0)) =>
        DocAst.Type.SchemaOf(print(arg0))
      case (UnkindedType.Cst(TypeConstructor.Tuple(l), _), _) if args.lengthIs == l =>
        DocAst.Type.Tuple(args.map(print))
      case (UnkindedType.Cst(TypeConstructor.Not, _), List(arg0)) =>
        DocAst.Type.Not(print(arg0))
      case (UnkindedType.Cst(TypeConstructor.And, _), List(arg0, arg1)) =>
        DocAst.Type.And(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Or, _), List(arg0, arg1)) =>
        DocAst.Type.Or(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Complement, _), List(arg0)) =>
        DocAst.Type.Complement(print(arg0))
      case (UnkindedType.Cst(TypeConstructor.Union, _), List(arg0, arg1)) =>
        DocAst.Type.Union(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Intersection, _), List(arg0, arg1)) =>
        DocAst.Type.Intersection(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Difference, _), List(arg0, arg1)) =>
        DocAst.Type.Difference(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.SymmetricDiff, _), List(arg0, arg1)) =>
        DocAst.Type.SymmetricDiff(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.CaseComplement(_), _), List(arg0)) =>
        DocAst.Type.CaseComplement(print(arg0))
      case (UnkindedType.Cst(TypeConstructor.CaseUnion(_), _), List(arg0, arg1)) =>
        DocAst.Type.CaseUnion(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.CaseIntersection(_), _), List(arg0, arg1)) =>
        DocAst.Type.CaseIntersection(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.Difference, _), List(arg0, arg1)) =>
        DocAst.Type.Difference(print(arg0), print(arg1))
      case (UnkindedType.Cst(TypeConstructor.SymmetricDiff, _), List(arg0, arg1)) =>
        DocAst.Type.SymmetricDiff(print(arg0), print(arg1))
      case (UnkindedType.Cst(tc, _), _) => mkApp(TypeConstructorPrinter.print(tc), args.map(print))
      case (UnkindedType.Enum(sym, _), _) => Type.AsIs(sym.toString)
      case (UnkindedType.Struct(sym, _), _) => Type.AsIs(sym.toString)
      case (UnkindedType.RestrictableEnum(sym, _), _) => Type.AsIs(sym.toString)
      case (UnkindedType.UnappliedAlias(sym, _), _) => Type.AsIs(sym.toString)
      case (UnkindedType.UnappliedAssocType(sym, _), _) => Type.AsIs(sym.toString)
      case (UnkindedType.UnappliedGetEff(action, _), _) => Type.AsIs("Get" + action.toString)
      case (UnkindedType.Arrow(eff0, arity, _), _) if args.lengthIs == arity && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as `List(a1, a2, .., b)`
        // safe match because of the case guard
        val (arrowArgs, List(arrowRes)) = args.splitAt(args.length - 1)
        eff0.map(print) match {
          case Some(eff) => Type.ArrowEff(arrowArgs.map(print), print(arrowRes), eff)
          case None => Type.Arrow(arrowArgs.map(print), print(arrowRes))
        }
      case (UnkindedType.Arrow(eff, arity, _), _) => mkApp(Type.AsIs(s"Arrow($arity)"), (eff.toList ::: args).map(print))
      case (UnkindedType.CaseSet(cases, _), _) => mkApp(Type.CaseSet(SortedSet.from(cases)), args.map(print))
      case (UnkindedType.CaseComplement(tpe, _), _) => mkApp(Type.CaseComplement(print(tpe)), args.map(print))
      case (UnkindedType.CaseUnion(tpe1, tpe2, _), _) => mkApp(Type.CaseUnion(print(tpe1), print(tpe2)), args.map(print))
      case (UnkindedType.CaseIntersection(tpe1, tpe2, _), _) => mkApp(Type.CaseIntersection(print(tpe1), print(tpe2)), args.map(print))
      case (UnkindedType.Ascribe(tpe, kind, _), _) => mkApp(Type.Ascribe(print(tpe), KindPrinter.print(kind)), args.map(print))
      case (UnkindedType.Alias(cst, aliasArgs, _, _), _) => mkApp(Type.Alias(cst.sym, aliasArgs.map(print)), args.map(print))
      case (UnkindedType.AssocType(cst, arg, _), _) => mkApp(Type.AssocType(cst.sym, print(arg)), args.map(print))
      case (UnkindedType.GetEff(action, tpe, _), _) => mkApp(mkApp(Type.AsIs("Get" + action.toString), List(print(tpe))), args.map(print))
      case (UnkindedType.Error(_), _) => Type.Error
      case (UnkindedType.Apply(_, _, _), _) =>
        // `collectApp` does not return Apply as base.
        DocAst.Type.Meta("bug in UnkindedTypePrinter")
    }
  }

  /** Constructs [[DocAst.Type.App]] unless `args` is empty */
  private def mkApp(base: DocAst.Type, args: List[DocAst.Type]): DocAst.Type = args match {
    case Nil => base
    case other => Type.App(base, other)
  }

  /** Returns e.g. `App(App(Tuple, Char), Char)` as `(Tuple, List(Char, Char))`. */
  private def collectApp(tpe: UnkindedType): (UnkindedType, List[UnkindedType]) = {
    @tailrec
    def helper(tpe0: UnkindedType, acc: List[UnkindedType]): (UnkindedType, List[UnkindedType]) = tpe0 match {
      case UnkindedType.Apply(tpe1, tpe2, _) => helper(tpe1, tpe2 :: acc)
      case _ => (tpe0, acc)
    }

    helper(tpe, Nil)
  }
}
