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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.util.collection.{ListMap, MultiMap}

import java.lang.reflect.{Constructor, Field, Method}

object TypedAst {

  val empty: Root = Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, None, None, Map.empty, Map.empty, ListMap.empty, AvailableClasses.empty, LabelledPrecedenceGraph.empty, DependencyGraph.empty)

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
                  uses: Map[Symbol.ModuleSym, List[UseOrImport]],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  entryPoints: Option[Set[Symbol.DefnSym]],
                  sources: Map[Source, SourceLocation],
                  traitEnv: Map[Symbol.TraitSym, TraitContext],
                  eqEnv: ListMap[Symbol.AssocTypeSym, shared.AssocTypeDef],
                  availableClasses: AvailableClasses,
                  precedenceGraph: LabelledPrecedenceGraph,
                  dependencyGraph: DependencyGraph)

  case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[TraitConstraint], assocs: List[AssocTypeSig], sigs: List[Sig], laws: List[Def], loc: SourceLocation)

  case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, trt: TraitSymUse, tpe: Type, tconstrs: List[TraitConstraint], assocs: List[AssocTypeDef], defs: List[Def], ns: Name.NName, loc: SourceLocation) extends Sourceable {
    override def src: Source = loc.source
  }

  case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr], loc: SourceLocation)

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr, loc: SourceLocation)

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], declaredScheme: Scheme, retTpe: Type, eff: Type, tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint])

  case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Derivations, cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], sc: Scheme, fields: Map[Symbol.StructFieldSym, StructField], loc: SourceLocation)

  case class RestrictableEnum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Derivations, cases: Map[Symbol.RestrictableCaseSym, RestrictableCase], loc: SourceLocation)

  case class TypeAlias(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: Type, loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeSig(doc: Doc, mod: Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

  // TODO ASSOC-TYPES can probably be combined with KindedAst.AssocTypeSig
  case class AssocTypeDef(doc: Doc, mod: Modifiers, sym: AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec, loc: SourceLocation)

  sealed trait Expr extends Product {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: RestrictableEnumSymUse, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff
    }

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr {
      def tpe: Type = exp.tpe

      def eff: Type = exp.eff
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyClo(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(symUse: DefSymUse, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(symUse: LocalDefSymUse, exps: List[Expr], arrowTpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplySig(symUse: SigSymUse, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(bnd: Binder, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LocalDef(bnd: Binder, fparams: List[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Region(tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Scope(bnd: Binder, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tag(sym: CaseSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableTag(sym: RestrictableCaseSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

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

    case class StructNew(sym: Symbol.StructSym, fields: List[(StructFieldSymUse, Expr)], region: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, sym: StructFieldSymUse, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, sym: StructFieldSymUse, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: java.lang.Class[?], loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Bool
    }

    case class CheckedCast(cast: CheckedCastType, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Unsafe(exp: Expr, runEff: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, sym: EffectSymUse, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Handler(sym: EffectSymUse, rules: List[HandlerRule], bodyType: Type, bodyEff: Type, handledEff: Type, tpe: Type, loc: SourceLocation) extends Expr {
      override def eff: Type = Type.Pure
    }

    case class RunWith(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeConstructor(constructor: Constructor[?], exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeMethod(method: Method, exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InvokeStaticMethod(method: Method, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetField(field: Field, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutField(field: Field, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutStaticField(field: Field, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

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

    case class Var(bnd: Binder, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: CaseSymUse, pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, tpe: Type, loc: SourceLocation)
    }

    case class Error(tpe: Type, loc: SourceLocation) extends Pattern
  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Var(bnd: Binder, tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Tag(sym: RestrictableCaseSymUse, pat: List[VarOrWild], tpe: Type, loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(tpe: Type, loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

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

      case class Functional(outBnds: List[Binder], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Binder(sym: Symbol.VarSym, tpe: Type)

  case class Case(sym: Symbol.CaseSym, tpes: List[Type], sc: Scheme, loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpes: List[Type], sc: Scheme, loc: SourceLocation)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(bnd: Binder, tpe: Type, loc: SourceLocation)

  case class FormalParam(bnd: Binder, mod: Modifiers, tpe: Type, src: TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(bnd: Binder, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(bnd: Binder, tpe: Type, exp: Expr)

  case class SelectChannelRule(bnd: Binder, chan: Expr, exp: Expr)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
