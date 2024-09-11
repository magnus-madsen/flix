/*
 *  Copyright 2021 Matthew Lutze
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Denotation, Fixity, Polarity, Source}
import ca.uwaterloo.flix.language.{CompilationMessage, ast}
import ca.uwaterloo.flix.util.collection.MultiMap

import java.lang.reflect.{Constructor, Field, Method}

object KindedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Map.empty, MultiMap.empty)

  case class Root(traits: Map[Symbol.TraitSym, Trait],
                  instances: Map[Symbol.TraitSym, List[Instance]],
                  defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, RestrictableEnum],
                  effects: Map[Symbol.EffectSym, Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypeAlias],
                  uses: Map[Symbol.ModuleSym, List[Ast.UseOrImport]],
                  entryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  names: MultiMap[List[String], String])

  case class Trait(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[Ast.TraitConstraint], assocs: List[AssocTypeSig], sigs: Map[Symbol.SigSym, Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, trt: Ast.TraitSymUse, tpe: Type, tconstrs: List[Ast.TraitConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], sc: Scheme, tpe: Type, eff: Type, tconstrs: List[Ast.TraitConstraint], econstrs: List[Ast.EqualityConstraint], loc: SourceLocation)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Ast.Derivations, cases: Map[Symbol.CaseSym, Case], tpe: Type, loc: SourceLocation)

  case class Struct(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], sc: Scheme, fields: List[StructField], loc: SourceLocation)

  case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Ast.Derivations, cases: Map[Symbol.RestrictableCaseSym, RestrictableCase], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  sealed trait Expr {
    def loc: SourceLocation
  }

  object Expr {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Expr

    case class Def(sym: Symbol.DefnSym, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Sig(sym: Symbol.SigSym, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Hole(sym: Symbol.HoleSym, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: Ast.RestrictableEnumSymUse, exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Expr

    case class Apply(exp: Expr, exps: List[Expr], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class Lambda(fparam: FormalParam, exp: Expr, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, ann: Ast.Annotations, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Region(tpe: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp1: Expr, evar: Type.Var, loc: SourceLocation) extends Expr

    case class Match(exp: Expr, rules: List[MatchRule], loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Tag(sym: Ast.CaseSymUse, exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class RestrictableTag(sym: Ast.RestrictableCaseSymUse, exp: Expr, isOpen: Boolean, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], loc: SourceLocation) extends Expr

    case class RecordEmpty(loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, label: Name.Label, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, value: Expr, rest: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, rest: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class ArrayLoad(base: Expr, index: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class ArrayStore(base: Expr, index: Expr, elm: Expr, evar: Type.Var, loc: SourceLocation) extends Expr

    case class ArrayLength(base: Expr, evar: Type.Var, loc: SourceLocation) extends Expr

    case class StructNew(sym: Symbol.StructSym, fields: List[(Ast.StructFieldSymUse, Expr)], region: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, sym: Symbol.StructFieldSym, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, sym: Symbol.StructFieldSym, exp2: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr

    case class Ascribe(exp: Expr, expectedType: Option[Type], expectedPur: Option[Type], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: java.lang.Class[_], loc: SourceLocation) extends Expr

    case class CheckedCast(cast: Ast.CheckedCastType, exp: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class UncheckedMaskingCast(exp: Expr, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, eff: Ast.EffectSymUse, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, eff: Ast.EffectSymUse, rules: List[HandlerRule], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class InvokeConstructor2(clazz: Class[_], exps: List[Expr], jvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class InvokeMethod2(exp: Expr, methodName: Name.Ident, exps: List[Expr], jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class InvokeStaticMethod2(clazz: Class[_], methodName: Name.Ident, exps: List[Expr], jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class GetField2(exp: Expr, fieldName: Name.Ident, jvar: Type.Var, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class InvokeConstructorOld(constructor: Constructor[_], exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeMethodOld(method: Method, clazz: java.lang.Class[_], exp: Expr, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeStaticMethodOld(method: Method, exps: List[Expr], loc: SourceLocation) extends Expr

    case class GetFieldOld(field: Field, clazz: java.lang.Class[_], exp: Expr, loc: SourceLocation) extends Expr

    case class PutField(field: Field, clazz: java.lang.Class[_], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetStaticField(field: Field, loc: SourceLocation) extends Expr

    case class PutStaticField(field: Field, exp: Expr, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp1: Expr, exp2: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, evar: Type.Var, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Expr], tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ParYield(frags: List[ParYieldFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class Lazy(exp: Expr, loc: SourceLocation) extends Expr

    case class Force(exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class FixpointConstraintSet(cs: List[Constraint], tvar: Type.Var, loc: SourceLocation) extends Expr

    case class FixpointLambda(pparams: List[PredicateParam], exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class FixpointMerge(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class FixpointSolve(exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointFilter(pred: Name.Pred, exp: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class FixpointInject(exp: Expr, pred: Name.Pred, tvar: Type.Var, evar: Type.Var, loc: SourceLocation) extends Expr

    case class FixpointProject(pred: Name.Pred, exp1: Expr, exp2: Expr, tvar: Type.Var, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage, tvar: Type.Var, evar: Type.Var) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends Pattern

    case class RecordEmpty(loc: SourceLocation) extends Pattern

    case class Error(tvar: ast.Type.Var, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tvar: ast.Type.Var, pat: Pattern, loc: SourceLocation)
    }
  }

  sealed trait RestrictableChoosePattern {
    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, tvar: Type.Var, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: Ast.RestrictableCaseSymUse, pat: List[VarOrWild], tvar: Type.Var, loc: SourceLocation) extends RestrictableChoosePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], tvar: ast.Type.Var, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class StructField(mod: Ast.Modifiers, sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr, tvar: Type.Var)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
