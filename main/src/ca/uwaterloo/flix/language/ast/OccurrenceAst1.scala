/*
 * Copyright 2024 Jakob Schneider Villumsen
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

import ca.uwaterloo.flix.language.ast.shared.SymUse.{CaseSymUse, EffectSymUse, OpSymUse}
import ca.uwaterloo.flix.language.ast.shared.*

object OccurrenceAst1 {

  case class Root(defs: Map[Symbol.DefnSym, Def],
                  enums: Map[Symbol.EnumSym, Enum],
                  structs: Map[Symbol.StructSym, Struct],
                  effects: Map[Symbol.EffectSym, Effect],
                  mainEntryPoint: Option[Symbol.DefnSym],
                  entryPoints: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  case class Def(sym: Symbol.DefnSym, fparams: List[(FormalParam, Occur)], spec: Spec, exp: Expr, context: DefContext, loc: SourceLocation)

  /** Formal parameters have been moved out since [[Def]] and [[Op]] now have different types of formal parameters. */
  case class Spec(doc: Doc, ann: Annotations, mod: Modifiers, functionType: Type, retTpe: Type, eff: Type)

  case class Effect(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EffectSym, ops: List[Op], loc: SourceLocation)

  case class Op(sym: Symbol.OpSym, fparams: List[FormalParam], spec: Spec, loc: SourceLocation)

  case class Enum(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], cases: Map[Symbol.CaseSym, Case], loc: SourceLocation)

  case class Struct(doc: Doc, ann: Annotations, mod: Modifiers, sym: Symbol.StructSym, tparams: List[TypeParam], fields: List[StructField], loc: SourceLocation)

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

    case class Lambda(fparam: (FormalParam, Occur), exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyAtomic(op: AtomicOp, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyClo(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(sym: Symbol.DefnSym, exps: List[Expr], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(sym: Symbol.VarSym, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(sym: Symbol.VarSym, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, occur: Occur, loc: SourceLocation) extends Expr

    case class LocalDef(sym: Symbol.VarSym, fparams: List[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, occur: Occur, loc: SourceLocation) extends Expr

    case class Scope(sym: Symbol.VarSym, regionVar: Type.Var, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Cast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryWith(exp: Expr, effUse: EffectSymUse, rules: List[HandlerRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Do(op: OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(name: String, clazz: java.lang.Class[?], tpe: Type, eff: Type, methods: List[JvmMethod], loc: SourceLocation) extends Expr

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, occur: Occur, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(sym: CaseSymUse, pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    case class RecordEmpty(tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, tpe: Type, loc: SourceLocation)
    }
  }

  case class Case(sym: Symbol.CaseSym, tpes: List[Type], loc: SourceLocation)

  case class StructField(sym: Symbol.StructFieldSym, tpe: Type, loc: SourceLocation)

  case class FormalParam(sym: Symbol.VarSym, mod: Modifiers, tpe: Type, src: TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmMethod(ident: Name.Ident, fparams: List[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[?], exp: Expr)

  case class HandlerRule(op: OpSymUse, fparams: List[FormalParam], exp: Expr)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr)

  case class TypeMatchRule(sym: Symbol.VarSym, tpe: Type, exp: Expr)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: Expr, exp: Expr)

  case class TypeParam(name: Name.Ident, sym: Symbol.KindedTypeVarSym, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)


  sealed trait Occur

  object Occur {

    /**
      * Represents a variable that is not used in an expression.
      */
    case object Dead extends Occur

    /**
      * Represents a variable that occurs exactly once in an expression.
      */
    case object Once extends Occur

    /**
      * Represents a variable that occurs exactly once in the body of a lambda abstraction or local def.
      */
    case object OnceInAbstraction extends Occur

    /**
      * Represents a variable that occurs in expressions more than once.
      */
    case object Many extends Occur

    /**
      * Represents a variable that occurs in more than one branch, e.g., match cases, but never inside a lambda abstraction or local def.
      */
    case object ManyBranch extends Occur

    /**
      * Represents a variable that we explicitly do not want to inline.
      */
    case object DontInline extends Occur

    /**
      * Represents a function that should not be inlined and excluded from any rewriting internally.
      * This is because casts are dangerous.
      */
    case object Dangerous extends Occur
  }

  /**
    * // TODO: Before doing the TODO below, check that the invariant is maintained in [[ca.uwaterloo.flix.language.phase.Inliner1]].
    * // TODO: Either update this documentation or maintain the invariant in [[ca.uwaterloo.flix.language.phase.OccurrenceAnalyzer1]].
    * `OccurDef` contains information that indicates whether or not a def should be inlined
    * A def is `isDirectCall` if
    * the expression consist of a single (non-self) call with trivial arguments
    * `occur` represents the number of times a def is references in the entire program.
    * `size` denotes the cumulative weight of each expression in the body of the def
    */
  case class DefContext(isDirectCall: Boolean, occur: Occur, size: Int, localDefs: Int, isSelfRecursive: Boolean)

}


