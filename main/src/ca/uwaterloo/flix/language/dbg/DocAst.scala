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

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol}

import java.lang.reflect.{Constructor, Field, Method}

sealed trait DocAst

object DocAst {

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, parameters: List[Expr.Ascription], resType: Type, effect: Eff, body: Expr)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], cases: List[Case])

  case class Case(sym: Symbol.CaseSym, tpe: Type)

  case class TypeParam(sym: Symbol.KindedTypeVarSym)

  case class Program(enums: List[Enum], defs: List[Def])

  case class JvmMethod(ident: Name.Ident, fparams: List[Expr.Ascription], clo: Expr, tpe: Type)


  sealed trait Expr

  object Expr {

    /** A [[Expr]] atom that doesn't need parenthesis */
    sealed trait Atom extends Expr

    /** A [[Expr]] that sometimes needs parenthesis */
    sealed trait Composite extends Expr

    sealed trait LetBinder extends Atom

    sealed trait RecordOp extends Atom

    case object Unit extends Atom

    case class Tuple(elms: List[Expr]) extends Atom

    case class Tag(sym: Symbol.CaseSym, args: List[Expr]) extends Atom

    /** inserted string printed as-is (assumed not to require parenthesis) */
    case class AsIs(s: String) extends Atom

    /** inserted string printed as-is, enclosed with special meta symbols */
    case class Meta(s: String) extends Atom

    case object RecordEmpty extends Atom

    case class RecordExtend(label: Name.Label, value: Expr, rest: Expr) extends RecordOp

    case class RecordRestrict(label: Name.Label, value: Expr) extends RecordOp

    case class Keyword(word: String, d: Expr) extends Composite

    case class DoubleKeyword(word1: String, d1: Expr, word2: String, d2: Either[Expr, Type]) extends Composite

    case class Unary(op: String, d: Expr) extends Composite

    /** e.g. `arr?` */
    case class UnaryRightAfter(d: Expr, op: String) extends Atom

    case class Binary(d1: Expr, op: String, d2: Expr) extends Composite

    case class IfThenElse(cond: Expr, thn: Expr, els: Expr) extends Composite

    case class Branch(d: Expr, branches: Map[Symbol.LabelSym, Expr]) extends Atom

    case class Match(d: Expr, branches: List[(Expr, Option[Expr], Expr)]) extends Atom

    case class TypeMatch(d: Expr, branches: List[(Expr, Type, Expr)]) extends Atom

    /** e.g. `r.x` */
    case class Dot(d1: Expr, d2: Expr) extends Atom

    /** e.g. `r..toString()`. It is used for java "dots" */
    case class DoubleDot(d1: Expr, d2: Expr) extends Atom

    /**
      * e.g. `r#x` for [[RecordSelect]].
      */
    case class Hash(d1: Expr, d2: Expr) extends Atom

    case class TryCatch(d: Expr, rules: List[(Symbol.VarSym, Class[_], Expr)]) extends Atom

    case class TryWith(d1: Expr, eff: Symbol.EffectSym, rules: List[(Symbol.OpSym, List[Ascription], Expr)]) extends Atom

    case class Stm(d1: Expr, d2: Expr) extends LetBinder

    case class Let(v: Expr, tpe: Option[Type], bind: Expr, body: Expr) extends LetBinder

    case class LetRec(v: Expr, tpe: Option[Type], bind: Expr, body: Expr) extends LetBinder

    case class Scope(v: Expr, d: Expr) extends Atom

    case class App(f: Expr, args: List[Expr]) extends Atom

    case class SquareApp(f: Expr, args: List[Expr]) extends Atom

    case class DoubleSquareApp(f: Expr, args: List[Expr]) extends Atom

    case class Assign(d1: Expr, d2: Expr) extends Composite

    case class Ascription(v: Expr, tpe: Type) extends Composite

    case class NewObject(name: String, clazz: Class[_], tpe: Type, methods: List[JvmMethod]) extends Composite

    case class Lambda(fparams: List[Expr.Ascription], body: Expr) extends Composite

    case class Native(clazz: Class[_]) extends Atom

    val Unknown: Expr =
      Meta("unknown exp")

    def Var(sym: Symbol.VarSym): Expr =
      AsIs(sym.toString)

    val Wild: Expr =
      AsIs("_")

    /** e.g. `x_2` */
    def VarWithOffset(sym: Symbol.VarSym): Expr =
      AsIs(sym.toString + "_" + sym.getStackOffset(0).toString)

    def Hole(sym: Symbol.HoleSym): Expr =
      AsIs("?" + sym.toString)

    def HoleWithExp(exp: Expr): Expr =
      UnaryRightAfter(exp, "?")

    def HoleError(sym: Symbol.HoleSym): Expr =
      AsIs(sym.toString)

    /** the region value */
    val Region: Expr =
      Meta("region")

    val MatchError: Expr =
      AsIs("?matchError")

    /** represents the error ast node when compiling partial programs */
    val Error: Expr =
      AsIs("?astError")

    def Untag(sym: Symbol.CaseSym, d: Expr): Expr =
      Keyword("untag", d)

    def Is(sym: Symbol.CaseSym, d: Expr): Expr =
      Binary(d, "is", AsIs(sym.toString))

    /** The control separated return statement */
    def Ret(d: Expr): Expr =
      Keyword("ret", d)

    def Discard(d: Expr): Expr =
      Keyword("discard", d)

    def Def(sym: Symbol.DefnSym): Expr =
      AsIs(sym.toString)

    def Sig(sym: Symbol.SigSym): Expr =
      AsIs(sym.toString)

    /** e.g. `something @ rc` */
    def InRegion(d1: Expr, d2: Expr): Expr =
      Binary(d1, "@", d2)

    def ArrayNew(d1: Expr, d2: Expr): Expr =
      SquareApp(AsIs(""), List(Binary(d1, ";", d2)))

    def ArrayLit(ds: List[Expr]): Expr =
      SquareApp(AsIs(""), ds)

    def ArrayLength(d: Expr): Expr =
      DoubleDot(d, AsIs("length"))

    def ArrayLoad(d1: Expr, index: Expr): Expr =
      SquareApp(d1, List(index))

    def ArrayStore(d1: Expr, index: Expr, d2: Expr): Expr =
      Assign(SquareApp(d1, List(index)), d2)

    def StructNew(sym: Symbol.StructSym, exps: List[(Symbol.StructFieldSym, Expr)], d2: Expr): Expr = {
      val beforeRecord = "new " + sym.toString
      val record = exps.foldRight(RecordEmpty: Expr) { case (cur, acc) => RecordExtend(Name.Label(cur._1.name, cur._1.loc), cur._2, acc)}
      DoubleKeyword(beforeRecord, record, "@", Left(d2))
    }

    def StructGet(d1: Expr, field: Symbol.StructFieldSym): Expr =
      Dot(d1, AsIs(field.name))

    def StructPut(d1: Expr, field: Symbol.StructFieldSym, d2: Expr): Expr =
      Assign(Dot(d1, AsIs(field.name)), d2)

    def VectorLit(ds: List[Expr]): Expr =
      DoubleSquareApp(AsIs(""), ds)

    def VectorLoad(d1: Expr, index: Expr): Expr =
      DoubleSquareApp(d1, List(index))

    def VectorLength(d: Expr): Expr =
      DoubleDot(d, AsIs("length"))

    def Lazy(d: Expr): Expr =
      Keyword("lazy", d)

    def Force(d: Expr): Expr =
      Keyword("force", d)

    def Throw(d: Expr): Expr =
      Keyword("throw", d)

    def Index(idx: Int, d: Expr): Expr =
      Dot(d, AsIs(s"_$idx"))

    def InstanceOf(d: Expr, clazz: Class[_]): Expr =
      Binary(d, "instanceof", Native(clazz))

    def ClosureLifted(sym: Symbol.DefnSym, ds: List[Expr]): Expr = {
      val defName = AsIs(sym.toString)
      if (ds.isEmpty) defName else App(defName, ds)
    }

    def Spawn(d1: Expr, d2: Expr): Expr =
      InRegion(Keyword("spawn", d1), d2)

    def Cast(d: Expr, tpe: Type): Expr =
      DoubleKeyword("cast", d, "as", Right(tpe))

    def Unbox(d: Expr, tpe: Type): Expr =
      DoubleKeyword("unbox", d, "as", Right(tpe))

    def Box(d: Expr): Expr =
      Keyword("box", d)

    def Without(d: Expr, sym: Symbol.EffectSym): Expr =
      Binary(d, "without", AsIs(sym.toString))

    def Cst(cst: Ast.Constant): Expr =
      printer.ConstantPrinter.print(cst)

    def ApplyClo(d: Expr, ds: List[Expr], ct: Option[Ast.ExpPosition]): Expr =
      App(d, ds)

    def ApplySelfTail(sym: Symbol.DefnSym, ds: List[Expr]): Expr =
      App(AsIs(sym.toString), ds)

    def ApplyDef(sym: Symbol.DefnSym, ds: List[Expr], ct: Option[Ast.ExpPosition]): Expr =
      App(AsIs(sym.toString), ds)

    def Do(sym: Symbol.OpSym, ds: List[Expr]): Expr =
      Keyword("do", App(AsIs(sym.toString), ds))

    def JavaInvokeMethod2(d: Expr, methodName: Name.Ident, ds: List[Expr]): Expr =
      App(DoubleDot(d, AsIs(methodName.name)), ds)

    def JavaInvokeMethod(m: Method, d: Expr, ds: List[Expr]): Expr =
      App(DoubleDot(d, AsIs(m.getName)), ds)

    def JavaInvokeStaticMethod(m: Method, ds: List[Expr]): Expr = {
      App(Dot(Native(m.getDeclaringClass), AsIs(m.getName)), ds)
    }

    def JavaGetStaticField(f: Field): Expr = {
      Dot(Native(f.getDeclaringClass), AsIs(f.getName))
    }

    def JavaInvokeConstructor(c: Constructor[_], ds: List[Expr]): Expr = {
      App(Native(c.getDeclaringClass), ds)
    }

    def JavaGetField(f: Field, d: Expr): Expr =
      DoubleDot(d, AsIs(f.getName))

    def JavaPutField(f: Field, d1: Expr, d2: Expr): Expr =
      Assign(DoubleDot(d1, AsIs(f.getName)), d2)

    def JavaPutStaticField(f: Field, d: Expr): Expr =
      Assign(Dot(Native(f.getDeclaringClass), AsIs(f.getName)), d)

    def JumpTo(sym: Symbol.LabelSym): Expr =
      Keyword("goto", AsIs(sym.toString))

    def RecordSelect(label: Name.Label, d: Expr): Expr =
      Hash(d, AsIs(label.name))

    def Regex(p: java.util.regex.Pattern): Expr =
      App(AsIs("Regex"), List(AsIs(s""""${p.toString}"""")))

    val Absent: Expr =
      AsIs("Absent")

    def Present(d: Expr): Expr =
      App(AsIs("Present"), List(d))

  }

  sealed trait Type

  object Type {

    sealed trait Atom extends Type

    sealed trait Composite extends Type

    case object Unit extends Atom

    case class AsIs(s: String) extends Atom

    case class App(obj: String, args: List[Type]) extends Atom

    case class Tuple(elms: List[Type]) extends Atom

    case class Arrow(args: List[Type], res: Type) extends Composite

    case object RecordEmpty extends Atom

    case class RecordExtend(label: String, value: Type, rest: Type) extends Atom

    case object SchemaEmpty extends Atom

    case class SchemaExtend(name: String, tpe: Type, rest: Type) extends Atom

    case class Native(clazz: Class[_]) extends Atom

    case class JvmConstructor(constructor: Constructor[_]) extends Atom

    case class JvmMethod(method: Method) extends Atom

    /** inserted string printed as-is (assumed not to require parenthesis) */
    case class Meta(s: String) extends Atom

    val Void: Type = AsIs("Void")

    val AnyType: Type = AsIs("AnyType")

    val Unknown: Type = Meta("unknown type")

    val Bool: Type = AsIs("Bool")

    val Char: Type = AsIs("Char")

    val Float32: Type = AsIs("Float32")

    val Float64: Type = AsIs("Float64")

    val BigDecimal: Type = AsIs("BigDecimal")

    val Int8: Type = AsIs("Int8")

    val Int16: Type = AsIs("Int16")

    val Int32: Type = AsIs("Int32")

    val Int64: Type = AsIs("Int64")

    val BigInt: Type = AsIs("BigInt")

    val Str: Type = AsIs("String")

    val Regex: Type = AsIs("Regex")

    val Region: Type = AsIs("Region")

    val Null: Type = AsIs("Null")

    def Array(t: Type): Type = App("Array", List(t))

    def Lazy(t: Type): Type = App("Lazy", List(t))

    def Enum(sym: Symbol.EnumSym, args: List[Type]): Type = App(sym.toString, args)

    def Struct(sym: Symbol.StructSym, args: List[Type]): Type = App(sym.toString, args)

    def Var(id: Int): Type = AsIs(s"var$id")
  }

  sealed trait Eff

  object Eff {

    case object Pure extends Eff

    /** Represents the union of IO and all regions. */
    case object Impure extends Eff

    /** Represents Impure and all algebraic effect. */
    case object ControlImpure extends Eff

    case class AsIs(s: String) extends Eff

    /** Represents the top effect. */
    def Univ: Eff = AsIs("Univ")

  }

}


