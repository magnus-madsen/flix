/*
 *  Copyright 2017 Magnus Madsen
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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.util.collection.ListMap

import java.lang.reflect.Field

object ResolvedAst {

  val empty: Root = Root(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, ListMap.empty, List.empty, None, Map.empty, AvailableClasses.empty, Map.empty)

  case class Root(traits: Map[Symbol.TraitSym, Declaration.Trait],
                  instances: ListMap[Symbol.TraitSym, Declaration.Instance],
                  defs: Map[Symbol.DefnSym, Declaration.Def],
                  enums: Map[Symbol.EnumSym, Declaration.Enum],
                  structs: Map[Symbol.StructSym, Declaration.Struct],
                  restrictableEnums: Map[Symbol.RestrictableEnumSym, Declaration.RestrictableEnum],
                  effects: Map[Symbol.EffectSym, Declaration.Effect],
                  typeAliases: Map[Symbol.TypeAliasSym, Declaration.TypeAlias],
                  uses: ListMap[Symbol.ModuleSym, UseOrImport],
                  taOrder: List[Symbol.TypeAliasSym],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation],
                  availableClasses: AvailableClasses,
                  tokens: Map[Source, Array[Token]])

  // TODO use Law for laws
  case class CompilationUnit(usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {
    case class Namespace(sym: Symbol.ModuleSym, usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation) extends Declaration

    case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TraitSym, tparam: TypeParam, superTraits: List[TraitConstraint], assocs: List[Declaration.AssocTypeSig], sigs: Map[Symbol.SigSym, Declaration.Sig], laws: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, symUse: TraitSymUse, tparams: List[TypeParam], tpe: UnkindedType, tconstrs: List[TraitConstraint], assocs: List[Declaration.AssocTypeDef], defs: List[Declaration.Def], ns: Name.NName, loc: SourceLocation) extends Declaration

    case class Sig(sym: Symbol.SigSym, spec: Spec, exp: Option[Expr], loc: SourceLocation) extends Declaration

    case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr, loc: SourceLocation) extends Declaration

    case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], derives: Derivations, cases: List[Declaration.Case], loc: SourceLocation) extends Declaration

    case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation) extends Declaration

    case class StructField(mod: Modifiers, sym: Symbol.StructFieldSym, tpe: UnkindedType, loc: SourceLocation)

    case class RestrictableEnum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.RestrictableEnumSym, index: TypeParam, tparams: List[TypeParam], derives: Derivations, cases: List[Declaration.RestrictableCase], loc: SourceLocation) extends Declaration

    case class Case(sym: Symbol.CaseSym, tpes: List[UnkindedType], loc: SourceLocation) extends Declaration

    case class RestrictableCase(sym: Symbol.RestrictableCaseSym, tpes: List[UnkindedType], loc: SourceLocation) extends Declaration

    case class TypeAlias(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.TypeAliasSym, tparams: List[TypeParam], tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class AssocTypeSig(doc: Doc, mod: Modifiers, sym: Symbol.AssocTypeSym, tparam: TypeParam, kind: Kind, tpe: Option[UnkindedType], loc: SourceLocation) extends Declaration

    case class AssocTypeDef(doc: Doc, mod: Modifiers, symUse: AssocTypeSymUse, arg: UnkindedType, tpe: UnkindedType, loc: SourceLocation) extends Declaration

    case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Declaration.Op], loc: SourceLocation) extends Declaration

    case class Op(sym: Symbol.OpSym, spec: Spec, loc: SourceLocation) extends Declaration
  }

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, tparams: List[TypeParam], fparams: List[FormalParam], tpe: UnkindedType, eff: Option[UnkindedType], tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint])

  sealed trait Expr {
    def loc: SourceLocation

    def setLoc(loc: SourceLocation): Expr = this match {
      case Expr.Var(sym, _) => Expr.Var(sym, loc)
      case Expr.Hole(sym, _) => Expr.Hole(sym, loc)
      case Expr.HoleWithExp(exp, _) => Expr.HoleWithExp(exp, loc)
      case Expr.OpenAs(symUse, exp, _) => Expr.OpenAs(symUse, exp, loc)
      case Expr.Use(sym, alias, exp, _) => Expr.Use(sym, alias, exp, loc)
      case Expr.Cst(cst, _) => Expr.Cst(cst, loc)
      case Expr.ApplyClo(exp1, exp2, _) => Expr.ApplyClo(exp1, exp2, loc)
      case Expr.ApplyDef(symUse, exps, _) => Expr.ApplyDef(symUse, exps, loc)
      case Expr.ApplyLocalDef(symUse, exps, _) => Expr.ApplyLocalDef(symUse, exps, loc)
      case Expr.ApplySig(symUse, exps, _) => Expr.ApplySig(symUse, exps, loc)
      case Expr.Lambda(fparam, exp, allowSubeffecting, _) => Expr.Lambda(fparam, exp, allowSubeffecting, loc)
      case Expr.Unary(sop, exp, _) => Expr.Unary(sop, exp, loc)
      case Expr.Binary(sop, exp1, exp2, _) => Expr.Binary(sop, exp1, exp2, loc)
      case Expr.IfThenElse(exp1, exp2, exp3, _) => Expr.IfThenElse(exp1, exp2, exp3, loc)
      case Expr.Stm(exp1, exp2, _) => Expr.Stm(exp1, exp2, loc)
      case Expr.Discard(exp, _) => Expr.Discard(exp, loc)
      case Expr.Let(sym, exp1, exp2, _) => Expr.Let(sym, exp1, exp2, loc)
      case Expr.LocalDef(sym, fparams, exp1, exp2, _) => Expr.LocalDef(sym, fparams, exp1, exp2, loc)
      case Expr.Region(tpe, _) => Expr.Region(tpe, loc)
      case Expr.Scope(sym, regionVar, exp, _) => Expr.Scope(sym, regionVar, exp, loc)
      case Expr.Match(exp, rules, _) => Expr.Match(exp, rules, loc)
      case Expr.TypeMatch(exp, rules, _) => Expr.TypeMatch(exp, rules, loc)
      case Expr.RestrictableChoose(star, exp, rules, _) => Expr.RestrictableChoose(star, exp, rules, loc)
      case Expr.Tag(sym, exps, _) => Expr.Tag(sym, exps, loc)
      case Expr.RestrictableTag(sym, exps, isOpen, _) => Expr.RestrictableTag(sym, exps, isOpen, loc)
      case Expr.Tuple(exps, _) => Expr.Tuple(exps, loc)
      case Expr.RecordEmpty(_) => Expr.RecordEmpty(loc)
      case Expr.RecordSelect(exp, label, _) => Expr.RecordSelect(exp, label, loc)
      case Expr.RecordExtend(label, value, rest, _) => Expr.RecordExtend(label, value, rest, loc)
      case Expr.RecordRestrict(label, rest, _) => Expr.RecordRestrict(label, rest, loc)
      case Expr.ArrayLit(exps, exp, _) => Expr.ArrayLit(exps, exp, loc)
      case Expr.ArrayNew(exp1, exp2, exp3, _) => Expr.ArrayNew(exp1, exp2, exp3, loc)
      case Expr.ArrayLoad(base, index, _) => Expr.ArrayLoad(base, index, loc)
      case Expr.ArrayStore(base, index, elm, _) => Expr.ArrayStore(base, index, elm, loc)
      case Expr.ArrayLength(base, _) => Expr.ArrayLength(base, loc)
      case Expr.StructNew(sym, exps, region, _) => Expr.StructNew(sym, exps, region, loc)
      case Expr.StructGet(e, sym, _) => Expr.StructGet(e, sym, loc)
      case Expr.StructPut(exp1, sym, exp2, _) => Expr.StructPut(exp1, sym, exp2, loc)
      case Expr.VectorLit(exps, _) => Expr.VectorLit(exps, loc)
      case Expr.VectorLoad(exp1, exp2, _) => Expr.VectorLoad(exp1, exp2, loc)
      case Expr.VectorLength(exp, _) => Expr.VectorLength(exp, loc)
      case Expr.Ascribe(exp, expectedType, expectedEff, _) => Expr.Ascribe(exp, expectedType, expectedEff, loc)
      case Expr.InstanceOf(exp, clazz, _) => Expr.InstanceOf(exp, clazz, loc)
      case Expr.CheckedCast(cast, exp, _) => Expr.CheckedCast(cast, exp, loc)
      case Expr.UncheckedCast(exp, declaredType, declaredEff, _) => Expr.UncheckedCast(exp, declaredType, declaredEff, loc)
      case Expr.Without(exp, eff, _) => Expr.Without(exp, eff, loc)
      case Expr.TryCatch(exp, rules, _) => Expr.TryCatch(exp, rules, loc)
      case Expr.Throw(exp, _) => Expr.Throw(exp, loc)
      case Expr.TryWith(exp, eff, rules, _) => Expr.TryWith(exp, eff, rules, loc)
      case Expr.Do(op, exps, _) => Expr.Do(op, exps, loc)
      case Expr.InvokeConstructor(clazz, exps, _) => Expr.InvokeConstructor(clazz, exps, loc)
      case Expr.InvokeMethod(exp, methodName, exps, _) => Expr.InvokeMethod(exp, methodName, exps, loc)
      case Expr.InvokeStaticMethod(clazz, methodName, exps, _) => Expr.InvokeStaticMethod(clazz, methodName, exps, loc)
      case Expr.GetField(exp, fieldName, _) => Expr.GetField(exp, fieldName, loc)
      case Expr.PutField(field, clazz, exp1, exp2, _) => Expr.PutField(field, clazz, exp1, exp2, loc)
      case Expr.GetStaticField(field, _) => Expr.GetStaticField(field, loc)
      case Expr.PutStaticField(field, exp, _) => Expr.PutStaticField(field, exp, loc)
      case Expr.NewObject(name, clazz, methods, _) => Expr.NewObject(name, clazz, methods, loc)
      case Expr.NewChannel(exp, _) => Expr.NewChannel(exp, loc)
      case Expr.GetChannel(exp, _) => Expr.GetChannel(exp, loc)
      case Expr.PutChannel(exp1, exp2, _) => Expr.PutChannel(exp1, exp2, loc)
      case Expr.SelectChannel(rules, default, _) => Expr.SelectChannel(rules, default, loc)
      case Expr.Spawn(exp1, exp2, _) => Expr.Spawn(exp1, exp2, loc)
      case Expr.ParYield(frags, exp, _) => Expr.ParYield(frags, exp, loc)
      case Expr.Lazy(exp, _) => Expr.Lazy(exp, loc)
      case Expr.Force(exp, _) => Expr.Force(exp, loc)
      case Expr.FixpointConstraintSet(cs, _) => Expr.FixpointConstraintSet(cs, loc)
      case Expr.FixpointLambda(pparams, exp, _) => Expr.FixpointLambda(pparams, exp, loc)
      case Expr.FixpointMerge(exp1, exp2, _) => Expr.FixpointMerge(exp1, exp2, loc)
      case Expr.FixpointSolve(exp, _) => Expr.FixpointSolve(exp, loc)
      case Expr.FixpointFilter(pred, exp, _) => Expr.FixpointFilter(pred, exp, loc)
      case Expr.FixpointInject(exp, pred, _) => Expr.FixpointInject(exp, pred, loc)
      case Expr.FixpointProject(pred, exp1, exp2, _) => Expr.FixpointProject(pred, exp1, exp2, loc)
      case Expr.Error(m) => Expr.Error(m)
    }
  }

  object Expr {

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Expr

    case class Hole(sym: Symbol.HoleSym, loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: RestrictableEnumSymUse, exp: Expr, loc: SourceLocation) extends Expr

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr

    case class Cst(cst: Constant, loc: SourceLocation) extends Expr

    case class ApplyClo(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ApplyDef(symUse: DefSymUse, exps: List[Expr], loc: SourceLocation) extends Expr

    case class ApplyLocalDef(symUse: LocalDefSymUse, exps: List[Expr], loc: SourceLocation) extends Expr

    case class ApplySig(symUse: SigSymUse, exps: List[Expr], loc: SourceLocation) extends Expr

    case class Lambda(fparam: FormalParam, exp: Expr, allowSubeffecting: Boolean, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    // MATT why was this a full type
    case class Region(tpe: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regSym: Symbol.RegionSym, exp: Expr, loc: SourceLocation) extends Expr

    case class Match(exp: Expr, rules: List[MatchRule], loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], loc: SourceLocation) extends Expr

    case class Tag(symUse: CaseSymUse, exps: List[Expr], loc: SourceLocation) extends Expr

    case class RestrictableTag(symUse: RestrictableCaseSymUse, exps: List[Expr], isOpen: Boolean, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, label: Name.Label, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, value: Expr, rest: Expr, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, rest: Expr, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class ArrayLoad(base: Expr, index: Expr, loc: SourceLocation) extends Expr

    case class ArrayStore(base: Expr, index: Expr, elm: Expr, loc: SourceLocation) extends Expr

    case class ArrayLength(base: Expr, loc: SourceLocation) extends Expr

    case class StructNew(sym: Symbol.StructSym, exps: List[(StructFieldSymUse, Expr)], region: Expr, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, symUse: StructFieldSymUse, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, symUse: StructFieldSymUse, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr

    case class Ascribe(exp: Expr, expectedType: Option[UnkindedType], expectedEff: Option[UnkindedType], loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: java.lang.Class[?], loc: SourceLocation) extends Expr

    case class CheckedCast(cast: CheckedCastType, exp: Expr, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[UnkindedType], declaredEff: Option[UnkindedType], loc: SourceLocation) extends Expr

    case class Unsafe(exp: Expr, eff: UnkindedType, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, symUse: EffectSymUse, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, loc: SourceLocation) extends Expr

    case class Handler(symUse: EffectSymUse, rules: List[HandlerRule], loc: SourceLocation) extends Expr

    case class RunWith(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Do(symUse: OpSymUse, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeConstructor(clazz: Class[?], exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeMethod(exp: Expr, methodName: Name.Ident, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeStaticMethod(clazz: Class[?], methodName: Name.Ident, exps: List[Expr], loc: SourceLocation) extends Expr

    case class GetField(exp: Expr, fieldName: Name.Ident, loc: SourceLocation) extends Expr

    case class PutField(field: Field, clazz: java.lang.Class[?], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class GetStaticField(field: Field, loc: SourceLocation) extends Expr

    case class PutStaticField(field: Field, exp: Expr, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Expr], loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ParYield(frags: List[ParYieldFragment], exp: Expr, loc: SourceLocation) extends Expr

    case class Lazy(exp: Expr, loc: SourceLocation) extends Expr

    case class Force(exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointConstraintSet(cs: List[Constraint], loc: SourceLocation) extends Expr

    case class FixpointLambda(pparams: List[PredicateParam], exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointMerge(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class FixpointSolve(exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointFilter(pred: Name.Pred, exp: Expr, loc: SourceLocation) extends Expr

    case class FixpointInject(exp: Expr, pred: Name.Pred, loc: SourceLocation) extends Expr

    case class FixpointProject(pred: Name.Pred, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, loc: SourceLocation) extends Pattern

    case class Tag(symUse: CaseSymUse, pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, loc: SourceLocation) extends Pattern

    case class Error(loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, loc: SourceLocation)
    }

  }

  sealed trait RestrictableChoosePattern

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends VarOrWild

    case class Tag(symUse: RestrictableCaseSymUse, pats: List[VarOrWild], loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], loc: SourceLocation) extends Predicate.Body

      case class Functional(syms: List[Symbol.VarSym], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(sym: Symbol.VarSym, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: Option[UnkindedType], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Denotation, tpes: List[UnkindedType], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: UnkindedType, eff: Option[UnkindedType], loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(symUse: OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: UnkindedType, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  sealed trait TypeParam {
    val name: Name.Ident
    val sym: Symbol.UnkindedTypeVarSym
  }

  object TypeParam {
    case class Kinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam

    case class Implicit(name: Name.Ident, sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends TypeParam
  }

  case class TraitConstraint(symUse: TraitSymUse, tpe: UnkindedType, loc: SourceLocation)

  case class EqualityConstraint(assocTypeSymUse: AssocTypeSymUse, tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}
