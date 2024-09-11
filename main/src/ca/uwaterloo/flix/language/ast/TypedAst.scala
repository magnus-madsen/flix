/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.LabelledPrecedenceGraph
import ca.uwaterloo.flix.language.ast.shared.{Denotation, Fixity, Polarity, Source}
import ca.uwaterloo.flix.util.collection.{ListMap, MultiMap}

import java.lang.reflect.{Constructor, Field, Method}

object TypedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, Set.empty, Map.empty, Map.empty, ListMap.empty, MultiMap.empty, LabelledPrecedenceGraph.empty)

  case class Root(modules: Map[Symbol.ModuleSym, List[Symbol]],
                  traits: Map[Symbol.TraitSym, Trait],
                  instances: Map[Symbol.TraitSym, List[Instance]],
                  sigs: Map[Symbol.SigSym, Sig],
                  defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, RestrictableEnum],
                  effects: Map[Symbol.EffectSym, Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, TypeAlias],
                  uses: Map[Symbol.ModuleSym, List[Ast.UseOrImport]],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  traitEnv: Map[Symbol.TraitSym, Ast.TraitContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef],
                  names: MultiMap[List[String], String],
                  precedenceGraph: LabelledPrecedenceGraph)

  case class Trait(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[Ast.TraitConstraint], assocs: List[AssocTypeSig], sigs: List[Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, trt: Ast.TraitSymUse, tpe: Type, tconstrs: List[Ast.TraitConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr)

  case class Spec(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[Ast.TraitConstraint], econstrs: List[Ast.EqualityConstraint], loc: SourceLocation)

  case class Enum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Ast.Derivations, cases: Map[Symbol.CaseSym, Case], tpe: Type, loc: SourceLocation)

  case class Struct(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], sc: Scheme, fields: Map[Symbol.StructFieldSym, StructField], loc: SourceLocation)

  case class RestrictableEnum(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Ast.Derivations, cases: Map[Symbol.RestrictableCaseSym, RestrictableCase], tpe: Type, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Ast.Doc, mod: Ast.Modifiers, sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Ast.Doc, ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  sealed trait Expr extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Def(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Sig(sym: Symbol.SigSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Hole(sym: Symbol.HoleSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class HoleWithExp(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: Ast.RestrictableEnumSymUse, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff
    }

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr {
      def tpe: Type = exp.tpe

      def eff: Type = exp.eff
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Apply(exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LetRec(sym: Symbol.VarSym, ann: Ast.Annotations, mod: Ast.Modifiers, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Region(tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tag(sym: Ast.CaseSymUse, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableTag(sym: Ast.RestrictableCaseSymUse, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class RecordSelect(exp: Expr, label: Name.Label, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLength(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.Int32
    }

    case class ArrayStore(exp1: Expr, exp2: Expr, exp3: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.Unit
    }

    case class StructNew(sym: Symbol.StructSym, fields: List[(Ast.StructFieldSymUse, Expr)], region: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, sym: Symbol.StructFieldSym, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, sym: Symbol.StructFieldSym, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: java.lang.Class[_], loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Bool
    }

    case class CheckedCast(cast: Ast.CheckedCastType, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class UncheckedMaskingCast(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, effUse: Ast.EffectSymUse, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeConstructor(constructor: Constructor[_], exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeMethod(method: Method, exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeStaticMethod(method: Method, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetField(field: Field, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutField(field: Field, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutStaticField(field: Field, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[_], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ParYield(frags: List[ParYieldFragment], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Lazy(exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Force(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointConstraintSet(cs: List[Constraint], tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class FixpointLambda(pparams: List[PredicateParam], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointMerge(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointSolve(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointFilter(pred: Name.Pred, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointInject(exp: Expr, pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointProject(pred: Name.Pred, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage, tpe: Type, eff: Type) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Ast.Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tpe: Type, pat: Pattern, loc: SourceLocation)
    }

    case class Error(tpe: Type, loc: SourceLocation) extends Pattern
  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: Ast.RestrictableCaseSymUse, pat: List[VarOrWild], tpe: Type, loc: SourceLocation) extends RestrictableChoosePattern

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], tpe: Type, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], tpe: Type, loc: SourceLocation) extends Predicate.Body

      case class Functional(outVars: List[Symbol.VarSym], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Case(sym: Symbol.CaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpe: Type, sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, src: Ast.TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
