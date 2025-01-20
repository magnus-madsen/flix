/*
 * Copyright 2023 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.shared.{Annotations, AvailableClasses, CheckedCastType, Constant, Denotation, Doc, Fixity, Modifiers, Polarity, Source}
import ca.uwaterloo.flix.util.collection.MultiMap

object DesugaredAst {

  val empty: Root = Root(Map.empty, None, AvailableClasses.empty)

  case class Root(units: Map[Source, CompilationUnit], mainEntryPoint: Option[Symbol.DefnSym], availableClasses: AvailableClasses)

  case class CompilationUnit(usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation)

  sealed trait Declaration

  object Declaration {

    case class Namespace(ident: Name.Ident, usesAndImports: List[UseOrImport], decls: List[Declaration], loc: SourceLocation) extends Declaration

    // TODO change laws to Law
    case class Trait(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparam: TypeParam, superTraits: List[TraitConstraint], assocs: List[Declaration.AssocTypeSig], sigs: List[Declaration.Sig], laws: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Instance(doc: Doc, ann: Annotations, mod: Modifiers, trt: Name.QName, tpe: Type, tconstrs: List[TraitConstraint], assocs: List[Declaration.AssocTypeDef], defs: List[Declaration.Def], loc: SourceLocation) extends Declaration

    case class Sig(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], fparams: List[FormalParam], exp: Option[Expr], tpe: Type, eff: Option[Type], tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint], loc: SourceLocation)

    case class Def(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Option[Type], tconstrs: List[TraitConstraint], constrs: List[EqualityConstraint], loc: SourceLocation) extends Declaration

    case class Law(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Type, tconstrs: List[TraitConstraint], loc: SourceLocation) extends Declaration

    case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], derives: Derivations, cases: List[Case], loc: SourceLocation) extends Declaration

    case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation) extends Declaration

    case class RestrictableEnum(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, index: TypeParam, tparams: List[TypeParam], derives: Derivations, cases: List[RestrictableCase], loc: SourceLocation) extends Declaration

    case class TypeAlias(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, tparams: List[TypeParam], tpe: Type, loc: SourceLocation) extends Declaration

    case class AssocTypeSig(doc: Doc, mod: Modifiers, ident: Name.Ident, tparam: TypeParam, kind: Kind, tpe: Option[Type], loc: SourceLocation)

    case class AssocTypeDef(doc: Doc, mod: Modifiers, ident: Name.Ident, arg: Type, tpe: Type, loc: SourceLocation)

    case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, ops: List[Declaration.Op], loc: SourceLocation) extends Declaration

    case class Op(doc: Doc, ann: Annotations, mod: Modifiers, ident: Name.Ident, fparams: List[FormalParam], tpe: Type, tconstrs: List[TraitConstraint], loc: SourceLocation)

  }

  sealed trait UseOrImport

  object UseOrImport {

    case class Use(qname: Name.QName, alias: Name.Ident, loc: SourceLocation) extends UseOrImport

    case class Import(name: Name.JavaName, alias: Name.Ident, loc: SourceLocation) extends UseOrImport
  }


  sealed trait Expr {
    def loc: SourceLocation
  }

  object Expr {

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Expr

    case class Open(qname: Name.QName, loc: SourceLocation) extends Expr

    case class OpenAs(qname: Name.QName, exp: Expr, loc: SourceLocation) extends Expr

    case class Hole(name: Option[Name.Ident], loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, loc: SourceLocation) extends Expr

    case class Use(uses: List[UseOrImport], exp: Expr, loc: SourceLocation) extends Expr

    case class Cst(cst: Constant, loc: SourceLocation) extends Expr

    case class Apply(exp: Expr, exps: List[Expr], loc: SourceLocation) extends Expr

    case class Lambda(fparam: FormalParam, exp: Expr, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, loc: SourceLocation) extends Expr

    case class Let(ident: Name.Ident, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class LocalDef(ident: Name.Ident, fparams: List[FormalParam], exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class Region(tpe: ca.uwaterloo.flix.language.ast.Type, loc: SourceLocation) extends Expr

    case class Scope(ident: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr

    case class Match(exp: Expr, rules: List[MatchRule], loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, label: Name.Label, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class ArrayLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class ArrayLength(exp: Expr, loc: SourceLocation) extends Expr

    case class ArrayStore(exp1: Expr, exp2: Expr, exp3: Expr, loc: SourceLocation) extends Expr

    case class StructNew(name: Name.QName, exps: List[(Name.Label, Expr)], region: Expr, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, name: Name.Label, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, name: Name.Label, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr

    case class Ascribe(exp: Expr, expectedType: Option[Type], expectedEff: Option[Type], loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, className: Name.Ident, loc: SourceLocation) extends Expr

    case class CheckedCast(cast: CheckedCastType, exp: Expr, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], loc: SourceLocation) extends Expr

    case class Unsafe(exp: Expr, eff: Type, loc: SourceLocation) extends Expr

    case class Without(exp: Expr, eff: Name.QName, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, loc: SourceLocation) extends Expr

    case class Handler(eff: Name.QName, rules: List[HandlerRule], loc: SourceLocation) extends Expr

    case class RunWith(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class InvokeConstructor(clazzName: Name.Ident, exps: List[Expr], loc: SourceLocation) extends Expr

    case class InvokeMethod(exp: Expr, methodName: Name.Ident, exps: List[Expr], loc: SourceLocation) extends Expr

    case class GetField(exp: Expr, fieldName: Name.Ident, loc: SourceLocation) extends Expr

    case class NewObject(tpe: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], exp: Option[Expr], loc: SourceLocation) extends Expr

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

    case class Var(ident: Name.Ident, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, loc: SourceLocation) extends Pattern

    case class Tag(qname: Name.QName, pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, loc: SourceLocation) extends Pattern

    case class Error(loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Option[Pattern], loc: SourceLocation)
    }

  }

  sealed trait RestrictableChoosePattern {
    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild

    case class Wild(loc: SourceLocation) extends VarOrWild

    case class Var(ident: Name.Ident, loc: SourceLocation) extends VarOrWild

    case class Tag(qname: Name.QName, pat: List[VarOrWild], loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

  }


  sealed trait Predicate

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, exps: List[Expr], loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], loc: SourceLocation) extends Predicate.Body

      case class Functional(idents: List[Name.Ident], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  sealed trait Type

  object Type {

    case class Var(ident: Name.Ident, loc: SourceLocation) extends Type

    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Type

    case class Unit(loc: SourceLocation) extends Type

    case class Tuple(tpes: List[Type], loc: SourceLocation) extends Type

    case class RecordRowEmpty(loc: SourceLocation) extends Type

    case class RecordRowExtend(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation) extends Type

    case class Record(row: Type, loc: SourceLocation) extends Type

    case class SchemaRowEmpty(loc: SourceLocation) extends Type

    case class SchemaRowExtendByAlias(qname: Name.QName, targs: List[Type], rest: Type, loc: SourceLocation) extends Type

    case class SchemaRowExtendByTypes(name: Name.Ident, den: Denotation, tpes: List[Type], rest: Type, loc: SourceLocation) extends Type

    case class Schema(row: Type, loc: SourceLocation) extends Type

    case class Arrow(tparams: List[Type], eff: Option[Type], tresult: Type, loc: SourceLocation) extends Type

    case class Apply(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class True(loc: SourceLocation) extends Type

    case class False(loc: SourceLocation) extends Type

    case class Not(tpe: Type, loc: SourceLocation) extends Type

    case class And(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Or(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Complement(tpe: Type, loc: SourceLocation) extends Type

    case class Union(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Intersection(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Difference(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class Pure(loc: SourceLocation) extends Type

    case class CaseSet(cases: List[Name.QName], loc: SourceLocation) extends Type

    case class CaseUnion(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class CaseIntersection(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Type

    case class CaseComplement(tpe: Type, loc: SourceLocation) extends Type

    case class Ascribe(tpe: Type, kind: Kind, loc: SourceLocation) extends Type

    case class Error(loc: SourceLocation) extends Type

  }

  sealed trait Kind

  object Kind {
    case class Ambiguous(qname: Name.QName, loc: SourceLocation) extends Kind

    case class Arrow(k1: Kind, k2: Kind, loc: SourceLocation) extends Kind
  }

  case class Case(ident: Name.Ident, tpes: List[Type], loc: SourceLocation)

  case class StructField(mod: Modifiers, name: Name.Label, tpe: Type, loc: SourceLocation)

  case class RestrictableCase(ident: Name.Ident, tpes: List[Type], loc: SourceLocation)

  case class FormalParam(ident: Name.Ident, mod: Modifiers, tpe: Option[Type], loc: SourceLocation)

  sealed trait PredicateParam

  object PredicateParam {

    case class PredicateParamUntyped(pred: Name.Pred, loc: SourceLocation) extends PredicateParam

    case class PredicateParamWithType(pred: Name.Pred, den: Denotation, tpes: List[Type], loc: SourceLocation) extends PredicateParam

  }

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, tpe: Type, eff: Option[Type], loc: SourceLocation)

  case class CatchRule(ident: Name.Ident, className: Name.Ident, exp: Expr)

  case class HandlerRule(op: Name.Ident, fparams: List[FormalParam], exp: Expr)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class TraitConstraint(trt: Name.QName, tpe: Type, loc: SourceLocation)

  case class EqualityConstraint(qname: Name.QName, tpe1: Type, tpe2: Type, loc: SourceLocation)

  case class Constraint(head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class MatchRule(pat: Pattern, exp1: Option[Expr], exp2: Expr)

  case class TypeMatchRule(ident: Name.Ident, tpe: Type, exp: Expr)

  case class SelectChannelRule(ident: Name.Ident, exp1: Expr, exp2: Expr)

  sealed trait TypeParam

  object TypeParam {

    case class Unkinded(ident: Name.Ident) extends TypeParam

    case class Kinded(ident: Name.Ident, kind: Kind) extends TypeParam

  }

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

  case class Derivations(traits: List[Name.QName], loc: SourceLocation)

}
