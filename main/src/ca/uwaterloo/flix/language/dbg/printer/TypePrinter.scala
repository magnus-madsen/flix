/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatType}

import scala.annotation.tailrec

object TypePrinter {

  /** Returns the [[DocAst.Type]] representation of `tpe`. */
  def print(tpe: Type): DocAst.Type = {
    val (base, args) = tpe.fullApply
    // Make the well-kinded types pretty.
    (base, args) match {
      case (Type.Var(sym, _), _) => mkApp(DocAst.Type.Var(sym), args.map(print))
      case (Type.Cst(TypeConstructor.Arrow(arity), _), _) if args.lengthIs == arity + 1 && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as `List(ef, a1, a2, .., b)`
        // safe match because of the case guard
        val (arrowEff :: arrowArgs, List(arrowRes)) = args.splitAt(args.length-1)
        DocAst.Type.ArrowEff(arrowArgs.map(print), print(arrowRes), print(arrowEff))
      case (Type.Cst(TypeConstructor.ArrowWithoutEffect(arity), _), _) if args.lengthIs == arity && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as `List(ef, a1, a2, .., b)`
        // safe match because of the case guard
        val (arrowArgs, List(arrowRes)) = args.splitAt(args.length - 1)
        DocAst.Type.Arrow(arrowArgs.map(print), print(arrowRes))
      case (Type.Cst(TypeConstructor.RecordRowExtend(label), _), List(arg0, arg1)) =>
        DocAst.Type.RecordRowExtend(label.toString, print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Record, _), List(arg0)) =>
        DocAst.Type.RecordOf(print(arg0))
      case (Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), List(arg0, arg1)) =>
        DocAst.Type.SchemaRowExtend(pred.toString, print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Schema, _), List(arg0)) =>
        DocAst.Type.SchemaOf(print(arg0))
      case (Type.Cst(TypeConstructor.Tuple(l), _), _) if args.lengthIs == l =>
        DocAst.Type.Tuple(args.map(print))
      case (Type.Cst(TypeConstructor.Not, _), List(arg0)) =>
        DocAst.Type.Not(print(arg0))
      case (Type.Cst(TypeConstructor.And, _), List(arg0, arg1)) =>
        DocAst.Type.And(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Or, _), List(arg0, arg1)) =>
        DocAst.Type.Or(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Complement, _), List(arg0)) =>
        DocAst.Type.Complement(print(arg0))
      case (Type.Cst(TypeConstructor.Union, _), List(arg0, arg1)) =>
        DocAst.Type.Union(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Intersection, _), List(arg0, arg1)) =>
        DocAst.Type.Intersection(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Difference, _), List(arg0, arg1)) =>
        DocAst.Type.Difference(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.SymmetricDiff, _), List(arg0, arg1)) =>
        DocAst.Type.SymmetricDiff(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.CaseComplement(_), _), List(arg0)) =>
        DocAst.Type.CaseComplement(print(arg0))
      case (Type.Cst(TypeConstructor.CaseUnion(_), _), List(arg0, arg1)) =>
        DocAst.Type.CaseUnion(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.CaseIntersection(_), _), List(arg0, arg1)) =>
        DocAst.Type.CaseIntersection(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Difference, _), List(arg0, arg1)) =>
        DocAst.Type.Difference(print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.SymmetricDiff, _), List(arg0, arg1)) =>
        DocAst.Type.SymmetricDiff(print(arg0), print(arg1))
      case (Type.Cst(tc, _), _) => mkApp(TypeConstructorPrinter.print(tc), args.map(print))
      case (Type.Alias(cst, aliasArgs, _, _), _) => mkApp(DocAst.Type.Alias(cst.sym, aliasArgs.map(print)), args.map(print))
      case (Type.AssocType(cst, arg, _, _), _) => mkApp(DocAst.Type.AssocType(cst.sym, print(arg)), args.map(print))
      case (Type.JvmToType(tpe, _), _) => mkApp(mkApp(DocAst.Type.AsIs("JvmToType"), List(print(tpe))), args.map(print))
      case (Type.JvmToEff(tpe, _), _) => mkApp(mkApp(DocAst.Type.AsIs("JvmToEff"), List(print(tpe))), args.map(print))
      case (Type.UnresolvedJvmType(member, _), _) => mkApp(mkApp(printJvmMember(member), List(print(tpe))), args.map(print))
      case (Type.Apply(_, _, _), _) =>
        // `Type.fullApply` does not return Apply as base.
        DocAst.Type.Meta("bug in TypePrinter")
    }
  }

  /** Constructs [[DocAst.Type.App]] unless `args` is empty */
  private def mkApp(base: DocAst.Type, args: List[DocAst.Type]): DocAst.Type = args match {
    case Nil => base
    case other => DocAst.Type.App(base, other)
  }

  private def printJvmMember(member: Type.JvmMember): DocAst.Type = DocAst.Type.Meta("JvmMember")

}
