/*
 * Copyright 2024 Magnus Madsen, Christian Bonde, Patrick Lundvig, Anna Krogh
 * 2024 Jakob Schneider Villumsen
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

import ca.uwaterloo.flix.language.ast.Ast.EliminatedBy
import ca.uwaterloo.flix.language.ast.shared.{Annotations, Constant, Denotation, Doc, Fixity, Modifiers, Polarity, Source}
import ca.uwaterloo.flix.language.phase.Monomorpher

object OccurrenceAst1 {

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffectSym, Effect],
                  entryPoint: Option[Symbol.DefnSym],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(sym: Symbol.DefnSym, spec: Spec, exp: Expr, context: DefContext)

  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, fparams: List[FormalParam], functionType: Type, retTpe: Type, eff: Type, loc: SourceLocation)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, spec: Spec)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[Symbol.KindedTypeVarSym], fields: List[StructField], loc: SourceLocation)

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

    /**
      * It now uses a DefSymUse since it has been specialized.
      * This AST Node will be removed entirely once we have `ApplySig`.
      */
    @EliminatedBy(Monomorpher.getClass)
    case class Sig(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyClo(exp: Expr, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(sym: Symbol.VarSym, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TypeMatch(exp: Expr, rules: List[TypeMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Cast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: Ast.EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: Ast.OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: Ast.CaseSymUse, pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, tpe: Type, pat: Pattern, loc: SourceLocation)
    }
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

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam {
    def sym: Symbol.VarSym

    def tpe: Type

    def loc: SourceLocation
  }

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, occur: Occur, loc: SourceLocation) extends ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, occur: Occur, loc: SourceLocation) extends ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: Type, src: Ast.TypeSource, occur: Occur, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: Ast.OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)


  sealed trait Occur

  object Occur {

    /**
      * Represents a variable that is not used in an expression.
      */
    case object Dead extends Occur

    /**
      * Represents a variables that occur exactly once in an expression.
      */
    case object Once extends Occur

    /**
      * Represents a variable that occur in expressions more than once.
      */
    case object Many extends Occur

    /**
      * Represents a variable that occur in more than one branch, e.g. match cases.
      */
    case object ManyBranch extends Occur

    /**
      * Represents a variable that we explicitly do not want to inline.
      */
    case object DontInline extends Occur
  }

  /**
    * `OccurDef` contains information that indicates whether or not a def should be inlined
    * A def is `isDirectCall` if
    * the expression consist of a single (non-self) call with trivial arguments
    * `occur` represents the number of times a def is references in the entire program.
    * `size` denotes the cumulative weight of each expression in the body of the def
    */
  case class DefContext(isDirectCall: Boolean, occur: Occur, size: Int, isSelfRecursive: Boolean)

}


