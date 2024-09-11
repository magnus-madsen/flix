/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.*
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.{Denotation, Fixity, Polarity, Scope}
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, Kind, LoweredAst, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugLoweredAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  */

// TODO: Long-term improvements:
// - Return a [[Validation]] from visitExp etc.
// - Decide which expressions to allow as head and body terms.

object Lowering {

  private object Defs {
    lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Boxable.box")
    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Solver.run")
    lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Solver.union")
    lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Solver.projectSym")
    lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Solver.rename")

    def ProjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint.Solver.injectInto$arity")

    def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint.Solver.facts$arity")

    lazy val DebugWithPrefix: Symbol.DefnSym = Symbol.mkDefnSym("Debug.debugWithPrefix")

    lazy val ChannelNew: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannel")
    lazy val ChannelNewTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannelTuple")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")
    lazy val ChannelMpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.mpmcAdmin")
    lazy val ChannelSelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.selectFrom")
    lazy val ChannelUnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.unsafeGetAndUnlock")

    /**
      * Returns the definition associated with the given symbol `sym`.
      */
    def lookup(sym: Symbol.DefnSym)(implicit root: TypedAst.Root, flix: Flix): TypedAst.Def = root.defs.get(sym) match {
      case None => throw InternalCompilerException(s"Symbol '$sym' not found. Missing library?", sym.loc)
      case Some(d) => d
    }
  }

  private object Enums {
    lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.Datalog")
    lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.Constraint")

    lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.HeadPredicate")
    lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.BodyPredicate")

    lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.HeadTerm")
    lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.BodyTerm")

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Shared.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.VarSym")

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Shared.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.Polarity")
    lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.Datalog.Fixity")
    lazy val SourceLocation: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Ast.SourceLocation")

    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym("Fixpoint.Boxed")

    lazy val Comparison: Symbol.EnumSym = Symbol.mkEnumSym("Comparison")

    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")

    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
    lazy val ChannelMpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.MpmcAdmin")

    lazy val ConcurrentReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
  }

  private object Types {
    //
    // Data Types
    //
    lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Boxed :: Nil, SourceLocation.Unknown)

    lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Boxed :: Nil, SourceLocation.Unknown)
    lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Boxed :: Nil, SourceLocation.Unknown)

    lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Boxed :: Nil, SourceLocation.Unknown)
    lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Boxed :: Nil, SourceLocation.Unknown)

    lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil, SourceLocation.Unknown)
    lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil, SourceLocation.Unknown)

    lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil, SourceLocation.Unknown)
    lazy val Fixity: Type = Type.mkEnum(Enums.Fixity, Nil, SourceLocation.Unknown)
    lazy val SL: Type = Type.mkEnum(Enums.SourceLocation, Nil, SourceLocation.Unknown)

    lazy val Comparison: Type = Type.mkEnum(Enums.Comparison, Nil, SourceLocation.Unknown)
    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil, SourceLocation.Unknown)

    lazy val ChannelMpmcAdmin: Type = Type.mkEnum(Enums.ChannelMpmcAdmin, Nil, SourceLocation.Unknown)
    lazy val ChannelMpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)

    lazy val ConcurrentReentrantLock: Type = Type.mkEnum(Enums.ConcurrentReentrantLock, Nil, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog, SourceLocation.Unknown)
    lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog, SourceLocation.Unknown)
    lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog, SourceLocation.Unknown)
    lazy val RenameType: Type = Type.mkPureUncurriedArrow(List(mkList(PredSym, SourceLocation.Unknown), Datalog), Datalog, SourceLocation.Unknown)
  }

  /**
    * Translates internal Datalog constraints into Flix Datalog constraints.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("Lowering") {
    implicit val r: TypedAst.Root = root

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val sigs = ParOps.parMapValues(root.sigs)(visitSig)
    val instances = ParOps.parMapValues(root.instances)(insts => insts.map(visitInstance))
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val restrictableEnums = ParOps.parMapValues(root.restrictableEnums)(visitRestrictableEnum)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val aliases = ParOps.parMapValues(root.typeAliases)(visitTypeAlias)

    // TypedAst.Sigs are shared between the `sigs` field and the `classes` field.
    // Instead of visiting twice, we visit the `sigs` field and then look up the results when visiting traits.
    val traits = ParOps.parMapValues(root.traits)(t => visitTrait(t, sigs))

    val newEnums = enums ++ restrictableEnums.map {
      case (_, v) => v.sym -> v
    }

    LoweredAst.Root(traits, instances, sigs, defs, newEnums, structs, effects, aliases, root.entryPoint, root.reachable, root.sources, root.traitEnv, root.eqEnv)
  }

  /**
    * Lowers the given definition `defn0`.
    */
  private def visitDef(defn0: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Def = defn0 match {
    case TypedAst.Def(sym, spec0, exp0) =>
      val spec = visitSpec(spec0)
      val exp = visitExp(exp0)(Scope.Top, root, flix)
      LoweredAst.Def(sym, spec, exp)
  }

  /**
    * Lowers the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Sig = sig0 match {
    case TypedAst.Sig(sym, spec0, exp0) =>
      val spec = visitSpec(spec0)
      val impl = exp0.map(visitExp(_)(Scope.Top, root, flix))
      LoweredAst.Sig(sym, spec, impl)
  }

  /**
    * Lowers the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Instance = inst0 match {
    case TypedAst.Instance(doc, ann, mod, sym, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = visitType(tpe0)
      val tconstrs = tconstrs0.map(visitTraitConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) => LoweredAst.AssocTypeDef(doc, mod, sym, args, tpe, loc)
      }
      val defs = defs0.map(visitDef)
      LoweredAst.Instance(doc, ann, mod, sym, tpe, tconstrs, assocs, defs, ns, loc)
  }

  /**
    * Lowers the given enum `enum0`.
    */
  private def visitEnum(enum0: TypedAst.Enum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.Enum(doc, ann, mod, sym, tparams0, derives, cases0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val cases = cases0.map {
        case (_, TypedAst.Case(caseSym, caseTpeDeprecated0, caseSc0, loc)) =>
          val caseTpeDeprecated = visitType(caseTpeDeprecated0)
          val caseSc = visitScheme(caseSc0)
          (caseSym, LoweredAst.Case(caseSym, caseTpeDeprecated, caseSc, loc))
      }
      LoweredAst.Enum(doc, ann, mod, sym, tparams, derives, cases, tpe, loc)
  }

  /**
    * Lowers the given struct `struct0`.
    */
  private def visitStruct(struct0: TypedAst.Struct)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Struct = struct0 match {
    case TypedAst.Struct(doc, ann, mod, sym, tparams0, _, fields0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val fields = fields0.map {
        case (fieldSym, field) =>
          LoweredAst.StructField(fieldSym, visitType(field.tpe), loc)
      }
      LoweredAst.Struct(doc, ann, mod, sym, tparams, fields.toList, loc)
  }

  /**
    * Lowers the given enum `enum0` from a restrictable enum into a regular enum.
    */
  private def visitRestrictableEnum(enum0: TypedAst.RestrictableEnum)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Enum = enum0 match {
    case TypedAst.RestrictableEnum(doc, ann, mod, sym0, index0, tparams0, derives, cases0, tpe0, loc) =>
      // index is erased since related checking has concluded.
      // Restrictable tag is lowered into a regular tag
      val index = visitTypeParam(index0)
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      val cases = cases0.map {
        case (_, TypedAst.RestrictableCase(caseSym0, caseTpeDeprecated0, caseSc0, loc)) =>
          val caseTpeDeprecated = visitType(caseTpeDeprecated0)
          val caseSc = visitScheme(caseSc0)
          val caseSym = visitRestrictableCaseSym(caseSym0)
          (caseSym, LoweredAst.Case(caseSym, caseTpeDeprecated, caseSc, loc))
      }
      val sym = visitRestrictableEnumSym(sym0)
      LoweredAst.Enum(doc, ann, mod, sym, index :: tparams, derives, cases, tpe, loc)
  }

  /**
    * Lowers `sym` from a restrictable case sym into a regular case sym.
    */
  private def visitRestrictableCaseSym(sym: Symbol.RestrictableCaseSym): Symbol.CaseSym = {
    val enumSym = visitRestrictableEnumSym(sym.enumSym)
    new Symbol.CaseSym(enumSym, sym.name, sym.loc)
  }

  /**
    * Lowers `sym` from a restrictable case sym use into a regular case sym use.
    */
  private def visitRestrictableCaseSymUse(sym: Ast.RestrictableCaseSymUse): Ast.CaseSymUse = {
    Ast.CaseSymUse(visitRestrictableCaseSym(sym.sym), sym.sym.loc)
  }

  /**
    * Lowers `sym` from a restrictable enum sym into a regular enum sym.
    */
  private def visitRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): Symbol.EnumSym =
    new Symbol.EnumSym(sym.namespace, sym.name, sym.loc)

  /**
    * Lowers the given `effect`.
    */
  private def visitEffect(effect: TypedAst.Effect)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Effect = effect match {
    case TypedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp)
      LoweredAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given `op`.
    */
  private def visitOp(op: TypedAst.Op)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Op = op match {
    case TypedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0)
      LoweredAst.Op(sym, spec)
  }

  /**
    * Lowers the given type `alias`.
    */
  private def visitTypeAlias(alias: TypedAst.TypeAlias)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeAlias = alias match {
    case TypedAst.TypeAlias(doc, _, mod, sym, tparams0, tpe0, loc) =>
      val tparams = tparams0.map(visitTypeParam)
      val tpe = visitType(tpe0)
      LoweredAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
  }

  /**
    * Lowers the given type constraint `tconstr0`.
    */
  private def visitTraitConstraint(tconstr0: Ast.TraitConstraint)(implicit root: TypedAst.Root, flix: Flix): Ast.TraitConstraint = tconstr0 match {
    case Ast.TraitConstraint(head, tpe0, loc) =>
      val tpe = visitType(tpe0)
      Ast.TraitConstraint(head, tpe, loc)
  }

  /**
    * Lowers the given trait `trt0`, with the given lowered sigs `sigs`.
    */
  private def visitTrait(trt0: TypedAst.Trait, sigs: Map[Symbol.SigSym, LoweredAst.Sig])(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Trait = trt0 match {
    case TypedAst.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, signatures0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0)
      val superTraits = superTraits0.map(visitTraitConstraint)
      val assocs = assocs0.map {
        case TypedAst.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc) => LoweredAst.AssocTypeSig(doc, mod, sym, tparam, kind, loc)
      }
      val signatures = signatures0.map(sig => sigs(sig.sym))
      val laws = laws0.map(visitDef)
      LoweredAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, signatures, laws, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def visitSpec(spec0: TypedAst.Spec)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Spec = spec0 match {
    case TypedAst.Spec(doc, ann, mod, tparams0, fparams, declaredScheme, retTpe, eff, tconstrs, econstrs, loc) => // TODO ASSOC-TYPES econstrs needed in lowering?
      val tparam = tparams0.map(visitTypeParam)
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      LoweredAst.Spec(doc, ann, mod, tparam, fs, ds, retTpe, eff, tconstrs, loc)
  }

  /**
    * Lowers the given `tparam`.
    */
  private def visitTypeParam(tparam: TypedAst.TypeParam)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.TypeParam = tparam match {
    case TypedAst.TypeParam(name, sym, loc) => LoweredAst.TypeParam(name, sym, loc)
  }

  /**
    * Lowers the given expression `exp0`.
    */
  private def visitExp(exp0: TypedAst.Expr)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = exp0 match {
    case TypedAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Cst(cst, t, loc)

    case TypedAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Var(sym, t, loc)

    case TypedAst.Expr.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Def(sym, t, loc)

    case TypedAst.Expr.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.Sig(sym, t, loc)

    case TypedAst.Expr.Hole(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.HoleWithExp(_, tpe, _, loc) =>
      val sym = Symbol.freshHoleSym(loc)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.HoleError(sym), List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.OpenAs(sym, exp, tpe, loc) =>
      visitExp(exp) // TODO RESTR-VARS maybe add to loweredAST

    case TypedAst.Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case TypedAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Lambda(p, e, t, loc)

    case TypedAst.Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expr.Apply(e, es, t, eff, loc)

    case TypedAst.Expr.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Unary(sop), List(e), t, eff, loc)

    case TypedAst.Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.Let(sym, mod, e1, e2, t, eff, loc)

    case TypedAst.Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.LetRec(sym, mod, e1, e2, t, eff, loc)

    case TypedAst.Expr.Region(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Region, List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Scope(sym, regionVar, e, t, eff, loc)

    case TypedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expr.IfThenElse(e1, e2, e3, t, eff, loc)

    case TypedAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.Stm(e1, e2, t, eff, loc)

    case TypedAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.Discard(e, eff, loc)

    case TypedAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitTypeMatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.TypeMatch(e, rs, t, eff, loc)

    case TypedAst.Expr.RestrictableChoose(_, exp, rules, tpe, eff, loc) =>
      // lower into an ordinary match
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      val t = visitType(tpe)
      LoweredAst.Expr.Match(e, rs, t, eff, loc)

    case TypedAst.Expr.Tag(sym, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(sym.sym), List(e), t, eff, loc)

    case TypedAst.Expr.RestrictableTag(sym0, exp, tpe, eff, loc) =>
      // Lower a restrictable tag into a normal tag.
      val caseSym = visitRestrictableCaseSym(sym0.sym)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), List(e), t, eff, loc)

    case TypedAst.Expr.Tuple(elms, tpe, eff, loc) =>
      val es = visitExps(elms)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, t, eff, loc)

    case TypedAst.Expr.RecordEmpty(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordEmpty, List.empty, t, Type.Pure, loc)

    case TypedAst.Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(label), List(e), t, eff, loc)

    case TypedAst.Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordExtend(label), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.RecordRestrict(label), List(e), t, eff, loc)

    case TypedAst.Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = visitExps(exps)
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, e :: es, t, eff, loc)

    case TypedAst.Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayNew, List(e1, e2, e3), t, eff, loc)

    case TypedAst.Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), Type.Int32, eff, loc)

    case TypedAst.Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.ArrayStore, List(e1, e2, e3), Type.Unit, eff, loc)

    case TypedAst.Expr.StructNew(sym, fields0, region0, tpe, eff, loc) =>
      val fields = fields0.map { case (k, v) => (k, visitExp(v)) }
      val region = visitExp(region0)
      val (names0, es) = fields.unzip
      val names = names0.map(_.sym)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructNew(sym, names), region :: es, tpe, eff, loc)

    case TypedAst.Expr.StructGet(exp0, field, tpe, eff, loc) =>
      val exp = visitExp(exp0)
      val idx = field.idx
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructGet(field), List(exp), tpe, eff, loc)

    case TypedAst.Expr.StructPut(exp0, field, exp1, tpe, eff, loc) =>
      val struct = visitExp(exp0)
      val rhs = visitExp(exp1)
      val idx = field.idx
      LoweredAst.Expr.ApplyAtomic(AtomicOp.StructPut(field), List(struct, rhs), tpe, eff, loc)

    case TypedAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expr.VectorLit(es, t, eff, loc)

    case TypedAst.Expr.VectorLoad(base, index, tpe, eff, loc) =>
      val b = visitExp(base)
      val i = visitExp(index)
      val t = visitType(tpe)
      LoweredAst.Expr.VectorLoad(b, i, t, eff, loc)

    case TypedAst.Expr.VectorLength(base, loc) =>
      val b = visitExp(base)
      LoweredAst.Expr.VectorLength(b, loc)

    case TypedAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Ascribe(e, t, eff, loc)

    case TypedAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InstanceOf(clazz), List(e), Type.Bool, e.eff, loc)

    case TypedAst.Expr.CheckedCast(_, exp, tpe, eff, loc) =>
      // Note: We do *NOT* erase checked (i.e. safe) casts.
      // In Java, `String` is a subtype of `Object`, but the Flix IR makes this upcast _explicit_.
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.Cast(e, Some(t), None, t, eff, loc)

    case TypedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val t = visitType(tpe)
      LoweredAst.Expr.Cast(e, dt, declaredEff, t, eff, loc)

    case TypedAst.Expr.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp)

    case TypedAst.Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case TypedAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe)
      LoweredAst.Expr.TryCatch(e, rs, t, eff, loc)

    case TypedAst.Expr.Throw(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Throw, List(e), t, eff, loc)

    case TypedAst.Expr.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitHandlerRule)
      val t = visitType(tpe)
      LoweredAst.Expr.TryWith(e, sym, rs, t, eff, loc)

    case TypedAst.Expr.Do(sym, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      LoweredAst.Expr.Do(sym, es, tpe, eff, loc)

    case TypedAst.Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeConstructor(constructor), es, t, eff, loc)

    case TypedAst.Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), e :: es, t, eff, loc)

    case TypedAst.Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val es = visitExps(exps)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.InvokeStaticMethod(method), es, t, eff, loc)

    case TypedAst.Expr.GetField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.GetField(field), List(e), t, eff, loc)

    case TypedAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.PutField(field), List(e1, e2), t, eff, loc)

    case TypedAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.GetStaticField(field), List.empty, t, eff, loc)

    case TypedAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.PutStaticField(field), List(e), t, eff, loc)

    case TypedAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod)
      LoweredAst.Expr.NewObject(name, clazz, t, eff, ms, loc)

    // New channel expressions are rewritten as follows:
    //     chan Int32 10
    // becomes a call to the standard library function:
    //     Concurrent/Channel.newChannel(10)
    //
    case TypedAst.Expr.NewChannel(_, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkNewChannelTuple(e, t, eff, loc)

    // Channel get expressions are rewritten as follows:
    //     <- c
    // becomes a call to the standard library function:
    //     Concurrent/Channel.get(c)
    //
    case TypedAst.Expr.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkGetChannel(e, t, eff, loc)

    // Channel put expressions are rewritten as follows:
    //     c <- 42
    // becomes a call to the standard library function:
    //     Concurrent/Channel.put(42, c)
    //
    case TypedAst.Expr.PutChannel(exp1, exp2, _, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      mkPutChannel(e1, e2, eff, loc)

    // Channel select expressions are rewritten as follows:
    //     select {
    //         case x <- ?ch1 => ?handlech1
    //         case y <- ?ch2 => ?handlech2
    //         case _ => ?default
    //     }
    // becomes:
    //     let ch1 = ?ch1;
    //     let ch2 = ?ch2;
    //     match selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: Nil, false) {  // true if no default
    //         case (0, locks) =>
    //             let x = unsafeGetAndUnlock(ch1, locks);
    //             ?handlech1
    //         case (1, locks) =>
    //             let y = unsafeGetAndUnlock(ch2, locks);
    //             ?handlech2
    //         case (-1, _) =>                                                  // Omitted if no default
    //             ?default                                                     // Unlock is handled by selectFrom
    //     }
    // Note: match is not exhaustive: we're relying on the simplifier to handle this for us
    //
    case TypedAst.Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      val t = visitType(tpe)

      val channels = rs map { case LoweredAst.SelectChannelRule(_, c, _) => (mkLetSym("chan", loc), c) }
      val admins = mkChannelAdminList(rs, channels, loc)
      val selectExp = mkChannelSelect(admins, d, loc)
      val cases = mkChannelCases(rs, channels, eff, loc)
      val defaultCase = mkSelectDefaultCase(d, t, loc)
      val matchExp = LoweredAst.Expr.Match(selectExp, cases ++ defaultCase, t, eff, loc)

      channels.foldRight[LoweredAst.Expr](matchExp) {
        case ((sym, c), e) => LoweredAst.Expr.Let(sym, Modifiers.Empty, c, e, t, eff, loc)
      }

    case TypedAst.Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e1, e2), t, eff, loc)

    case TypedAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map {
        case TypedAst.ParYieldFragment(pat, e, loc) => LoweredAst.ParYieldFragment(visitPat(pat), visitExp(e), loc)
      }
      val e = visitExp(exp)
      val t = visitType(tpe)
      mkParYield(fs, e, t, eff, loc)

    case TypedAst.Expr.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(e), t, Type.Pure, loc)

    case TypedAst.Expr.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      LoweredAst.Expr.ApplyAtomic(AtomicOp.Force, List(e), t, eff, loc)

    case TypedAst.Expr.FixpointConstraintSet(cs, _, loc) =>
      mkDatalog(cs, loc)

    case TypedAst.Expr.FixpointLambda(pparams, exp, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Rename)
      val defExp = LoweredAst.Expr.Def(defn.sym, Types.RenameType, loc)
      val predExps = mkList(pparams.map(pparam => mkPredSym(pparam.pred)), Types.mkList(Types.PredSym, loc), loc)
      val argExps = predExps :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expr.FixpointMerge(exp1, exp2, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Merge)
      val defExp = LoweredAst.Expr.Def(defn.sym, Types.MergeType, loc)
      val argExps = visitExp(exp1) :: visitExp(exp2) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expr.FixpointSolve(exp, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Solve)
      val defExp = LoweredAst.Expr.Def(defn.sym, Types.SolveType, loc)
      val argExps = visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expr.FixpointFilter(pred, exp, _, eff, loc) =>
      val defn = Defs.lookup(Defs.Filter)
      val defExp = LoweredAst.Expr.Def(defn.sym, Types.FilterType, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      val resultType = Types.Datalog
      LoweredAst.Expr.Apply(defExp, argExps, resultType, eff, loc)

    case TypedAst.Expr.FixpointInject(exp, pred, _, eff, loc) =>
      // Compute the arity of the functor F[(a, b, c)] or F[a].
      val arity = Type.eraseAliases(exp.tpe) match {
        case Type.Apply(_, innerType, _) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(l)) => l
          case Some(TypeConstructor.Unit) => 0
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-foldable type: '${exp.tpe}'.", loc)
      }

      // Compute the symbol of the function.
      val sym = Defs.ProjectInto(arity)

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, exp.tpe), Types.Datalog, loc)

      // Put everything together.
      val defExp = LoweredAst.Expr.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      LoweredAst.Expr.Apply(defExp, argExps, Types.Datalog, eff, loc)

    case TypedAst.Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
      // Compute the arity of the predicate symbol.
      // The type is either of the form `Array[(a, b, c)]` or `Array[a]`.
      val arity = Type.eraseAliases(tpe) match {
        case Type.Apply(Type.Cst(_, _), innerType, _) => innerType.typeConstructor match {
          case Some(TypeConstructor.Tuple(_)) => innerType.typeArguments.length
          case Some(TypeConstructor.Unit) => 0
          case _ => 1
        }
        case _ => throw InternalCompilerException(s"Unexpected non-list type: '$tpe'.", loc)
      }

      // Compute the symbol of the function.
      val sym = Defs.Facts(arity)

      // The type of the function.
      val defTpe = Type.mkPureUncurriedArrow(List(Types.PredSym, Types.Datalog), tpe, loc)

      // Put everything together.
      val defExp = LoweredAst.Expr.Def(sym, defTpe, loc)
      val argExps = mkPredSym(pred) :: visitExp(exp) :: Nil
      LoweredAst.Expr.Apply(defExp, argExps, tpe, eff, loc)

    case TypedAst.Expr.Error(m, _, _) =>
      throw InternalCompilerException(s"Unexpected error expression near", m.loc)

  }

  /**
    * Lowers the given list of expressions `exps0`.
    */
  private def visitExps(exps0: List[TypedAst.Expr])(implicit scope: Scope, root: TypedAst.Root, flix: Flix): List[LoweredAst.Expr] = exps0.map(visitExp)

  /**
    * Lowers the given pattern `pat0`.
    */
  private def visitPat(pat0: TypedAst.Pattern)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Pattern = pat0 match {
    case TypedAst.Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Wild(t, loc)

    case TypedAst.Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      LoweredAst.Pattern.Var(sym, t, loc)

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      LoweredAst.Pattern.Cst(cst, tpe, loc)

    case TypedAst.Pattern.Tag(sym, pat, tpe, loc) =>
      val p = visitPat(pat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tag(sym, p, t, loc)

    case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Tuple(es, t, loc)

    case TypedAst.Pattern.Record(pats, pat, tpe, loc) =>
      val patsVal = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, tpe1, pat1, loc1) =>
          val p1 = visitPat(pat1)
          val t1 = visitType(tpe1)
          LoweredAst.Pattern.Record.RecordLabelPattern(label, t1, p1, loc1)
      }
      val patVal = visitPat(pat)
      val t = visitType(tpe)
      LoweredAst.Pattern.Record(patsVal, patVal, t, loc)

    case TypedAst.Pattern.RecordEmpty(tpe, loc) =>
      LoweredAst.Pattern.RecordEmpty(visitType(tpe), loc)

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)
  }

  /**
    * Lowers the given scheme `sc0`.
    */
  private def visitScheme(sc0: Scheme)(implicit root: TypedAst.Root, flix: Flix): Scheme = sc0 match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      // TODO: What about constraints?
      val b = visitType(base)
      Scheme(quantifiers, tconstrs, econstrs, b)
  }

  /**
    * Lowers the given type `tpe0`.
    */
  private def visitType(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0.typeConstructor match {
    case Some(TypeConstructor.Schema) =>
      // We replace any Schema type, no matter the number of polymorphic type applications, with the erased Datalog type.
      Types.Datalog
    case _ => visitTypeNonSchema(tpe0)
  }

  /**
    * Lowers the given type `tpe0` which must not be a schema type.
    *
    * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
    */
  private def visitTypeNonSchema(tpe0: Type)(implicit root: TypedAst.Root, flix: Flix): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

    case Type.Var(sym, loc) => sym.kind match {
      case Kind.SchemaRow =>
        // Rewrite the kind of a Schema type variable to Star (because that is the kind of Types.Datalog)
        Type.Var(sym.withKind(Kind.Star), loc)
      case _ => tpe0 // Performance: Reuse tpe0.
    }

    // Rewrite Sender[t, r] to Concurrent.Channel.Mpmc[t, r]
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe1, _), tpe2, _) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      mkChannelTpe(t1, t2, loc)

    // Rewrite Receiver[t, r] to Concurrent.Channel.Mpmc[t, r]
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe1, _), tpe2, _) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      mkChannelTpe(t1, t2, loc)

    case Type.Apply(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      // Performance: Reuse tpe0, if possible.
      if ((t1 eq tpe1) && (t2 eq tpe2)) {
        tpe0
      } else {
        Type.Apply(t1, t2, loc)
      }

    case Type.Alias(sym, args, t, loc) =>
      Type.Alias(sym, args.map(visitType), visitType(t), loc)

    case Type.AssocType(cst, args, kind, loc) =>
      Type.AssocType(cst, args.map(visitType), kind, loc) // TODO ASSOC-TYPES can't put lowered stuff on right side of assoc type def...

    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
  }


  /**
    * Lowers the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: TypedAst.FormalParam)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.FormalParam = fparam0 match {
    case TypedAst.FormalParam(sym, mod, tpe, src, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Lowers the given restrictable choice rule `rule0` to a match rule.
    */
  private def visitRestrictableChooseRule(rule0: TypedAst.RestrictableChooseRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.RestrictableChooseRule(pat, exp) =>
      val e = visitExp(exp)
      pat match {
        case TypedAst.RestrictableChoosePattern.Tag(sym, pat0, tpe, loc) =>
          val termPatterns = pat0.map {
            case TypedAst.RestrictableChoosePattern.Var(sym, tpe, loc) => LoweredAst.Pattern.Var(sym, tpe, loc)
            case TypedAst.RestrictableChoosePattern.Wild(tpe, loc) => LoweredAst.Pattern.Wild(tpe, loc)
          }
          val pat1 = termPatterns match {
            case Nil => LoweredAst.Pattern.Cst(Constant.Unit, Type.mkUnit(loc), loc)
            case singular :: Nil => singular
            case _ => LoweredAst.Pattern.Tuple(termPatterns, Type.mkTuple(termPatterns.map(_.tpe), loc.asSynthetic), loc.asSynthetic)
          }
          val tagSym = visitRestrictableCaseSymUse(sym)
          val p = LoweredAst.Pattern.Tag(tagSym, pat1, tpe, loc)
          LoweredAst.MatchRule(p, None, e)
      }
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def visitCatchRule(rule0: TypedAst.CatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.CatchRule = rule0 match {
    case TypedAst.CatchRule(sym, clazz, exp) =>
      val e = visitExp(exp)
      LoweredAst.CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def visitHandlerRule(rule0: TypedAst.HandlerRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.HandlerRule = rule0 match {
    case TypedAst.HandlerRule(sym, fparams0, exp) =>
      val fparams = fparams0.map(visitFormalParam)
      val e = visitExp(exp)
      LoweredAst.HandlerRule(sym, fparams, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: TypedAst.MatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.MatchRule = rule0 match {
    case TypedAst.MatchRule(pat, guard, exp) =>
      val p = visitPat(pat)
      val g = guard.map(visitExp)
      val e = visitExp(exp)
      LoweredAst.MatchRule(p, g, e)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitTypeMatchRule(rule0: TypedAst.TypeMatchRule)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.TypeMatchRule = rule0 match {
    case TypedAst.TypeMatchRule(sym, tpe, exp) =>
      val e = visitExp(exp)
      LoweredAst.TypeMatchRule(sym, tpe, e)
  }

  /**
    * Lowers the given select channel rule `rule0`.
    */
  private def visitSelectChannelRule(rule0: TypedAst.SelectChannelRule)(implicit scope: Scope, root:TypedAst.Root, flix: Flix): LoweredAst.SelectChannelRule = rule0 match {
    case TypedAst.SelectChannelRule(sym, chan, exp) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      LoweredAst.SelectChannelRule(sym, c, e)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Datalog` value from the given list of Datalog constraints `cs`.
    */
  private def mkDatalog(cs: List[TypedAst.Constraint], loc: SourceLocation)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    val factExps = cs.filter(c => c.body.isEmpty).map(visitConstraint)
    val ruleExps = cs.filter(c => c.body.nonEmpty).map(visitConstraint)

    val factListExp = mkVector(factExps, Types.Constraint, loc)
    val ruleListExp = mkVector(ruleExps, Types.Constraint, loc)

    val innerExp = mkTuple(List(factListExp, ruleListExp), loc)
    mkTag(Enums.Datalog, "Datalog", innerExp, Types.Datalog, loc)
  }

  /**
    * Lowers the given constraint `c0`.
    */
  private def visitConstraint(c0: TypedAst.Constraint)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = c0 match {
    case TypedAst.Constraint(cparams, head, body, loc) =>
      val headExp = visitHeadPred(cparams, head)
      val bodyExp = mkVector(body.map(visitBodyPred(cparams, _)), Types.BodyPredicate, loc)
      val innerExp = mkTuple(headExp :: bodyExp :: Nil, loc)
      mkTag(Enums.Constraint, "Constraint", innerExp, Types.Constraint, loc)
  }

  /**
    * Lowers the given head predicate `p0`.
    */
  private def visitHeadPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Head)(implicit scope: Scope, root:TypedAst.Root, flix: Flix): LoweredAst.Expr = p0 match {
    case TypedAst.Predicate.Head.Atom(pred, den, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val termsExp = mkVector(terms.map(visitHeadTerm(cparams0, _)), Types.HeadTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: termsExp :: Nil, loc)
      mkTag(Enums.HeadPredicate, "HeadAtom", innerExp, Types.HeadPredicate, loc)
  }

  /**
    * Lowers the given body predicate `p0`.
    */
  private def visitBodyPred(cparams0: List[TypedAst.ConstraintParam], p0: TypedAst.Predicate.Body)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = p0 match {
    case TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, _, loc) =>
      val predSymExp = mkPredSym(pred)
      val denotationExp = mkDenotation(den, terms.lastOption.map(_.tpe), loc)
      val polarityExp = mkPolarity(polarity, loc)
      val fixityExp = mkFixity(fixity, loc)
      val termsExp = mkVector(terms.map(visitBodyTerm(cparams0, _)), Types.BodyTerm, loc)
      val innerExp = mkTuple(predSymExp :: denotationExp :: polarityExp :: fixityExp :: termsExp :: Nil, loc)
      mkTag(Enums.BodyPredicate, "BodyAtom", innerExp, Types.BodyPredicate, loc)

    case TypedAst.Predicate.Body.Functional(outVars, exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val inVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkFunctional(outVars, inVars, exp, loc)

    case TypedAst.Predicate.Body.Guard(exp0, loc) =>
      // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
      val quantifiedFreeVars = quantifiedVars(cparams0, exp0)
      val exp = visitExp(exp0)
      mkGuard(quantifiedFreeVars, exp, loc)

  }

  /**
    * Lowers the given head term `exp0`.
    */
  private def visitHeadTerm(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr)(implicit scope: Scope, root:TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    //
    // We need to consider four cases:
    //
    // Case 1.1: The expression is quantified variable. We translate it to a Var.
    // Case 1.2: The expression is a lexically bound variable. We translate it to a Lit that captures its value.
    // Case 2: The expression does not contain a quantified variable. We evaluate it to a (boxed) value.
    // Case 3: The expression contains quantified variables. We translate it to an application term.
    //
    exp0 match {
      case TypedAst.Expr.Var(sym, _, _) =>
        // Case 1: Variable term.
        if (isQuantifiedVar(sym, cparams0)) {
          // Case 1.1: Quantified variable.
          mkHeadTermVar(sym)
        } else {
          // Case 1.2: Lexically bound variable.
          mkHeadTermLit(box(visitExp(exp0)))
        }

      case _ =>
        // Compute the universally quantified variables (i.e. the variables not bound by the local scope).
        val quantifiedFreeVars = quantifiedVars(cparams0, exp0)

        if (quantifiedFreeVars.isEmpty) {
          // Case 2: No quantified variables. The expression can be reduced to a value.
          mkHeadTermLit(box(visitExp(exp0)))
        } else {
          // Case 3: Quantified variables. The expression is translated to an application term.
          mkAppTerm(quantifiedFreeVars, visitExp(exp0), exp0.loc)
        }
    }
  }

  /**
    * Lowers the given body term `pat0`.
    */
  private def visitBodyTerm(cparams0: List[TypedAst.ConstraintParam], pat0: TypedAst.Pattern)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = pat0 match {
    case TypedAst.Pattern.Wild(_, loc) =>
      mkBodyTermWild(loc)

    case TypedAst.Pattern.Var(sym, tpe, loc) =>
      if (isQuantifiedVar(sym, cparams0)) {
        // Case 1: Quantified variable.
        mkBodyTermVar(sym)
      } else {
        // Case 2: Lexically bound variable *expression*.
        mkBodyTermLit(box(LoweredAst.Expr.Var(sym, tpe, loc)))
      }

    case TypedAst.Pattern.Cst(cst, tpe, loc) =>
      mkBodyTermLit(box(LoweredAst.Expr.Cst(cst, tpe, loc)))

    case TypedAst.Pattern.Tag(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Tuple(_, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Record(_, _, _, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.RecordEmpty(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

    case TypedAst.Pattern.Error(_, loc) => throw InternalCompilerException(s"Unexpected pattern: '$pat0'.", loc)

  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit scope: Scope, root:TypedAst.Root, flix: Flix): LoweredAst.JvmMethod = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      val t = visitType(retTyp)
      LoweredAst.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Var` from the given variable symbol `sym`.
    */
  private def mkHeadTermVar(sym: Symbol.VarSym)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    val innerExp = mkVarSym(sym)
    mkTag(Enums.HeadTerm, "Var", innerExp, Types.HeadTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.HeadTerm.Lit` value which wraps the given expression `exp`.
    */
  private def mkHeadTermLit(exp: LoweredAst.Expr)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    mkTag(Enums.HeadTerm, "Lit", exp, Types.HeadTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Wild` from the given source location `loc`.
    */
  private def mkBodyTermWild(loc: SourceLocation): LoweredAst.Expr = {
    val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
    mkTag(Enums.BodyTerm, "Wild", innerExp, Types.BodyTerm, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Var` from the given variable symbol `sym`.
    */
  private def mkBodyTermVar(sym: Symbol.VarSym): LoweredAst.Expr = {
    val innerExp = mkVarSym(sym)
    mkTag(Enums.BodyTerm, "Var", innerExp, Types.BodyTerm, sym.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.BodyTerm.Lit` from the given expression `exp0`.
    */
  private def mkBodyTermLit(exp: LoweredAst.Expr)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    mkTag(Enums.BodyTerm, "Lit", exp, Types.BodyTerm, exp.loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.Denotation` from the given denotation `d` and type `tpeOpt`
    * (which must be the optional type of the last term).
    */
  private def mkDenotation(d: Denotation, tpeOpt: Option[Type], loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = d match {
    case Denotation.Relational =>
      val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Denotation, "Relational", innerExp, Types.Denotation, loc)

    case Denotation.Latticenal =>
      tpeOpt match {
        case None => throw InternalCompilerException("Unexpected nullary lattice predicate.", loc)
        case Some(tpe) =>
          // The type `Denotation[tpe]`.
          val unboxedDenotationType = Type.mkEnum(Enums.Denotation, tpe :: Nil, loc)

          // The type `Denotation[Boxed]`.
          val boxedDenotationType = Types.Denotation

          val Lattice: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Ast.Shared.lattice")
          val LatticeType: Type = Type.mkPureArrow(Type.Unit, unboxedDenotationType, loc)

          val Box: Symbol.DefnSym = Symbol.mkDefnSym("Fixpoint.Ast.Shared.box")
          val BoxType: Type = Type.mkPureArrow(unboxedDenotationType, boxedDenotationType, loc)

          val innerApply = LoweredAst.Expr.Apply(LoweredAst.Expr.Def(Lattice, LatticeType, loc), List(LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)), unboxedDenotationType, Type.Pure, loc)
          LoweredAst.Expr.Apply(LoweredAst.Expr.Def(Box, BoxType, loc), List(innerApply), boxedDenotationType, Type.Pure, loc)
      }
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Polarity` from the given polarity `p`.
    */
  private def mkPolarity(p: Polarity, loc: SourceLocation): LoweredAst.Expr = p match {
    case Polarity.Positive =>
      val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Polarity, "Positive", innerExp, Types.Polarity, loc)

    case Polarity.Negative =>
      val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Polarity, "Negative", innerExp, Types.Polarity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.Fixity` from the given fixity `f`.
    */
  private def mkFixity(f: Fixity, loc: SourceLocation): LoweredAst.Expr = f match {
    case Fixity.Loose =>
      val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Fixity, "Loose", innerExp, Types.Fixity, loc)

    case Fixity.Fixed =>
      val innerExp = LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)
      mkTag(Enums.Fixity, "Fixed", innerExp, Types.Fixity, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Shared.PredSym` from the given predicate `pred`.
    */
  private def mkPredSym(pred: Name.Pred): LoweredAst.Expr = pred match {
    case Name.Pred(sym, loc) =>
      val nameExp = LoweredAst.Expr.Cst(Ast.Constant.Str(sym), Type.Str, loc)
      val idExp = LoweredAst.Expr.Cst(Ast.Constant.Int64(0), Type.Int64, loc)
      val inner = mkTuple(List(nameExp, idExp), loc)
      mkTag(Enums.PredSym, "PredSym", inner, Types.PredSym, loc)
  }

  /**
    * Constructs a `Fixpoint/Ast/Datalog.VarSym` from the given variable symbol `sym`.
    */
  private def mkVarSym(sym: Symbol.VarSym): LoweredAst.Expr = {
    val nameExp = LoweredAst.Expr.Cst(Ast.Constant.Str(sym.text), Type.Str, sym.loc)
    mkTag(Enums.VarSym, "VarSym", nameExp, Types.VarSym, sym.loc)
  }

  /**
    * Returns the given expression `exp` in a box.
    */
  private def box(exp: LoweredAst.Expr)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    val loc = exp.loc
    val tpe = Type.mkPureArrow(exp.tpe, Types.Boxed, loc)
    val innerExp = LoweredAst.Expr.Def(Defs.Box, tpe, loc)
    LoweredAst.Expr.Apply(innerExp, List(exp), Types.Boxed, Type.Pure, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.GuardX`.
    */
  private def mkGuard(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit scope: Scope, root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)
      // Construct a lambda that takes the unit argument.
      val fparam = LoweredAst.FormalParam(sym, Ast.Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst.Expr.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.BodyPredicate, s"Guard0", lambdaExp, Types.BodyPredicate, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXb(lambdaExp, fvs.map(_._2))

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.BodyPredicate, s"Guard$arity", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.BodyPredicate.Functional`.
    */
  private def mkFunctional(outVars: List[Symbol.VarSym], inVars: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit root: TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    // Compute the number of in and out variables.
    val numberOfInVars = inVars.length
    val numberOfOutVars = outVars.length

    if (numberOfInVars == 0) {
      throw InternalCompilerException("Requires at least one in variable.", loc)
    }
    if (numberOfInVars > 5) {
      throw InternalCompilerException("Does not support more than 5 in variables.", loc)
    }
    if (numberOfOutVars == 0) {
      throw InternalCompilerException("Requires at least one out variable.", loc)
    }
    if (numberOfOutVars > 5) {
      throw InternalCompilerException("Does not support more than 5 out variables.", loc)
    }

    // Introduce a fresh variable for each in variable.
    val freshVars = inVars.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = inVars.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftXY(outVars, lambdaExp, inVars.map(_._2), exp.tpe, exp.loc)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val boundVarVector = mkVector(outVars.map(mkVarSym), Types.VarSym, loc)
    val freeVarVector = mkVector(inVars.map(kv => mkVarSym(kv._1)), Types.VarSym, loc)
    val innerExp = mkTuple(boundVarVector :: liftedExp :: freeVarVector :: Nil, loc)
    mkTag(Enums.BodyPredicate, s"Functional", innerExp, Types.BodyPredicate, loc)
  }

  /**
    * Returns a `Fixpoint/Ast/Datalog.HeadTerm.AppX`.
    */
  private def mkAppTerm(fvs: List[(Symbol.VarSym, Type)], exp: LoweredAst.Expr, loc: SourceLocation)(implicit scope: Scope, root:TypedAst.Root, flix: Flix): LoweredAst.Expr = {
    // Compute the number of free variables.
    val arity = fvs.length

    // Check that we have <= 5 free variables.
    if (arity > 5) {
      throw InternalCompilerException("Cannot lift functions with more than 5 free variables.", loc)
    }

    // Special case: No free variables.
    if (fvs.isEmpty) {
      val sym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, loc)
      // Construct a lambda that takes the unit argument.
      val fparam = LoweredAst.FormalParam(sym, Ast.Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, loc)
      val tpe = Type.mkPureArrow(Type.Unit, exp.tpe, loc)
      val lambdaExp = LoweredAst.Expr.Lambda(fparam, exp, tpe, loc)
      return mkTag(Enums.HeadTerm, s"App0", lambdaExp, Types.HeadTerm, loc)
    }

    // Introduce a fresh variable for each free variable.
    val freshVars = fvs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym]) {
      case (acc, (oldSym, _)) => acc + (oldSym -> Symbol.freshVarSym(oldSym))
    }

    // Substitute every symbol in `exp` for its fresh equivalent.
    val freshExp = substExp(exp, freshVars)

    // Curry `freshExp` in a lambda expression for each free variable.
    val lambdaExp = fvs.foldRight(freshExp) {
      case ((oldSym, tpe), acc) =>
        val freshSym = freshVars(oldSym)
        val fparam = LoweredAst.FormalParam(freshSym, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        val lambdaType = Type.mkPureArrow(tpe, acc.tpe, loc)
        LoweredAst.Expr.Lambda(fparam, acc, lambdaType, loc)
    }

    // Lift the lambda expression to operate on boxed values.
    val liftedExp = liftX(lambdaExp, fvs.map(_._2), exp.tpe)

    // Construct the `Fixpoint/Ast/Datalog.BodyPredicate` value.
    val varExps = fvs.map(kv => mkVarSym(kv._1))
    val innerExp = mkTuple(liftedExp :: varExps, loc)
    mkTag(Enums.HeadTerm, s"App$arity", innerExp, Types.HeadTerm, loc)
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val newChannel = LoweredAst.Expr.Def(Defs.ChannelNew, Type.mkIoArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expr.Apply(newChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a new channel tuple (sender, receiver) expression
    */
  private def mkNewChannelTuple(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val newChannel = LoweredAst.Expr.Def(Defs.ChannelNewTuple, Type.mkIoArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expr.Apply(newChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a channel get expression
    */
  private def mkGetChannel(exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val getChannel = LoweredAst.Expr.Def(Defs.ChannelGet, Type.mkIoArrow(exp.tpe, tpe, loc), loc)
    LoweredAst.Expr.Apply(getChannel, exp :: Nil, tpe, eff, loc)
  }

  /**
    * Make a channel put expression
    */
  private def mkPutChannel(exp1: LoweredAst.Expr, exp2: LoweredAst.Expr, eff: Type, loc: SourceLocation): LoweredAst.Expr = {
    val putChannel = LoweredAst.Expr.Def(Defs.ChannelPut, Type.mkIoUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc), loc)
    LoweredAst.Expr.Apply(putChannel, List(exp2, exp1), Type.Unit, eff, loc)
  }

  /**
    * Make the list of MpmcAdmin objects which will be passed to `selectFrom`
    */
  private def mkChannelAdminList(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expr)], loc: SourceLocation): LoweredAst.Expr = {
    val admins = rs.zip(channels) map {
      case (LoweredAst.SelectChannelRule(_, c, _), (chanSym, _)) =>
        val admin = LoweredAst.Expr.Def(Defs.ChannelMpmcAdmin, Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc), loc)
        LoweredAst.Expr.Apply(admin, List(LoweredAst.Expr.Var(chanSym, c.tpe, loc)), Types.ChannelMpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.ChannelMpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default
    */
  private def mkChannelSelect(admins: LoweredAst.Expr, default: Option[LoweredAst.Expr], loc: SourceLocation): LoweredAst.Expr = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val selectTpe = Type.mkIoUncurriedArrow(List(admins.tpe, Type.Bool), selectRetTpe, loc)
    val select = LoweredAst.Expr.Def(Defs.ChannelSelectFrom, selectTpe, loc)
    val blocking = default match {
      case Some(_) => LoweredAst.Expr.Cst(Ast.Constant.Bool(false), Type.Bool, loc)
      case None => LoweredAst.Expr.Cst(Ast.Constant.Bool(true), Type.Bool, loc)
    }
    LoweredAst.Expr.Apply(select, List(admins, blocking), selectRetTpe, Type.IO, loc)
  }

  /**
    * Construct a sequence of MatchRules corresponding to the given SelectChannelRules
    */
  private def mkChannelCases(rs: List[LoweredAst.SelectChannelRule], channels: List[(Symbol.VarSym, LoweredAst.Expr)], eff: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): List[LoweredAst.MatchRule] = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    rs.zip(channels).zipWithIndex map {
      case ((LoweredAst.SelectChannelRule(sym, chan, exp), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(List(LoweredAst.Pattern.Cst(Ast.Constant.Int32(i), Type.Int32, loc), LoweredAst.Pattern.Var(locksSym, locksType, loc)), loc)
        val (getTpe, _) = extractChannelTpe(chan.tpe)
        val get = LoweredAst.Expr.Def(Defs.ChannelUnsafeGetAndUnlock, Type.mkIoUncurriedArrow(List(chan.tpe, locksType), getTpe, loc), loc)
        val getExp = LoweredAst.Expr.Apply(get, List(LoweredAst.Expr.Var(chSym, chan.tpe, loc), LoweredAst.Expr.Var(locksSym, locksType, loc)), getTpe, eff, loc)
        val e = LoweredAst.Expr.Let(sym, Ast.Modifiers.Empty, getExp, exp, exp.tpe, eff, loc)
        LoweredAst.MatchRule(pat, None, e)
    }
  }

  /**
    * Construct additional MatchRule to handle the (optional) default case
    * NB: Does not need to unlock because that is handled inside Concurrent/Channel.selectFrom.
    */
  private def mkSelectDefaultCase(default: Option[LoweredAst.Expr], t: Type, loc: SourceLocation)(implicit flix: Flix): List[LoweredAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)
        val pat = mkTuplePattern(List(LoweredAst.Pattern.Cst(Ast.Constant.Int32(-1), Type.Int32, loc), LoweredAst.Pattern.Wild(locksType, loc)), loc)
        val defaultMatch = LoweredAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case _ =>
        List()
    }
  }

  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes`.
    *
    * Note: liftX and liftXb are similar and should probably be maintained together.
    */
  private def liftX(exp0: LoweredAst.Expr, argTypes: List[Type], resultType: Type): LoweredAst.Expr = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Fixpoint.Boxable.lift${argTypes.length}")

    //
    // The liftX family of functions are of the form: a -> b -> c -> `resultType` and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Boxed`.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> `resultType`.
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Boxed.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Types.Boxed, exp0.loc)

    // The type of the overall liftX function, i.e. (a -> b -> c -> `resultType`) -> (Boxed -> Boxed -> Boxed -> Boxed).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Construct a call to the liftX function.
    val defn = LoweredAst.Expr.Def(sym, liftType, exp0.loc)
    LoweredAst.Expr.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
  }

  /**
    * Lifts the given Boolean-valued lambda expression `exp0` with the given argument types `argTypes`.
    */
  private def liftXb(exp0: LoweredAst.Expr, argTypes: List[Type]): LoweredAst.Expr = {
    // Compute the liftXb symbol.
    val sym = Symbol.mkDefnSym(s"Fixpoint.Boxable.lift${argTypes.length}b")

    //
    // The liftX family of functions are of the form: a -> b -> c -> Bool and
    // returns a function of the form Boxed -> Boxed -> Boxed -> Boxed -> Bool.
    // That is, the function accepts a *curried* function and returns a *curried* function.
    //

    // The type of the function argument, i.e. a -> b -> c -> Bool.
    val argType = Type.mkPureCurriedArrow(argTypes, Type.Bool, exp0.loc)

    // The type of the returned function, i.e. Boxed -> Boxed -> Boxed -> Bool.
    val returnType = Type.mkPureCurriedArrow(argTypes.map(_ => Types.Boxed), Type.Bool, exp0.loc)

    // The type of the overall liftXb function, i.e. (a -> b -> c -> Bool) -> (Boxed -> Boxed -> Boxed -> Bool).
    val liftType = Type.mkPureArrow(argType, returnType, exp0.loc)

    // Construct a call to the liftXb function.
    val defn = LoweredAst.Expr.Def(sym, liftType, exp0.loc)
    LoweredAst.Expr.Apply(defn, List(exp0), returnType, Type.Pure, exp0.loc)
  }


  /**
    * Lifts the given lambda expression `exp0` with the given argument types `argTypes` and `resultType`.
    */
  private def liftXY(outVars: List[Symbol.VarSym], exp0: LoweredAst.Expr, argTypes: List[Type], resultType: Type, loc: SourceLocation): LoweredAst.Expr = {
    // Compute the number of bound ("output") and free ("input") variables.
    val numberOfInVars = argTypes.length
    val numberOfOutVars = outVars.length

    // Compute the liftXY symbol.
    // For example, lift3X2 is a function from three arguments to a Vector of pairs.
    val sym = Symbol.mkDefnSym(s"Fixpoint.Boxable.lift${numberOfInVars}X${numberOfOutVars}")

    //
    // The liftXY family of functions are of the form: i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)] and
    // returns a function of the form Vector[Boxed] -> Vector[Vector[Boxed]].
    // That is, the function accepts a *curried* function and an uncurried function that takes
    // its input as a boxed Vector and return its output as a vector of vectors.
    //

    // The type of the function argument, i.e. i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)].
    val argType = Type.mkPureCurriedArrow(argTypes, resultType, loc)

    // The type of the returned function, i.e. Vector[Boxed] -> Vector[Vector[Boxed]].
    val returnType = Type.mkPureArrow(Type.mkVector(Types.Boxed, loc), Type.mkVector(Type.mkVector(Types.Boxed, loc), loc), loc)

    // The type of the overall liftXY function, i.e. (i1 -> i2 -> i3 -> Vector[(o1, o2, o3, ...)]) -> (Vector[Boxed] -> Vector[Vector[Boxed]]).
    val liftType = Type.mkPureArrow(argType, returnType, loc)

    // Construct a call to the liftXY function.
    val defn = LoweredAst.Expr.Def(sym, liftType, loc)
    LoweredAst.Expr.Apply(defn, List(exp0), returnType, Type.Pure, loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkList(exps: List[LoweredAst.Expr], elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a vector expression constructed from the given `exps` with type list of `elmType`.
    */
  private def mkVector(exps: List[LoweredAst.Expr], elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    LoweredAst.Expr.VectorLit(exps, Type.mkVector(elmType, loc), Type.Pure, loc)
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): LoweredAst.Expr = {
    mkTag(Enums.FList, "Nil", LoweredAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc), Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: LoweredAst.Expr, tail: LoweredAst.Expr, loc: SourceLocation): LoweredAst.Expr = {
    val tuple = mkTuple(hd :: tail :: Nil, loc)
    mkTag(Enums.FList, "Cons", tuple, tail.tpe, loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exp: LoweredAst.Expr, tpe: Type, loc: SourceLocation): LoweredAst.Expr = {
    val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
    LoweredAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), List(exp), tpe, Type.Pure, loc)
  }

  /**
    * Returns a pure tuple expression constructed from the given list of expressions `exps`.
    */
  private def mkTuple(exps: List[LoweredAst.Expr], loc: SourceLocation): LoweredAst.Expr = {
    val tpe = Type.mkTuple(exps.map(_.tpe), loc)
    val eff = Type.Pure
    LoweredAst.Expr.ApplyAtomic(AtomicOp.Tuple, exps, tpe, eff, loc)
  }

  /**
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit scope: Scope, flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    mkChannelTpe(tpe, Type.IO, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe1` in region `tpe2`.
    */
  private def mkChannelTpe(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.ChannelMpmc, tpe1, loc), tpe2, loc)
  }

  /**
    * Returns `(t1, t2)` where `tpe = Concurrent.Channel.Mpmc[t1, t2]`.
    */
  private def extractChannelTpe(tpe: Type): (Type, Type) = eraseAliases(tpe) match {
    case Type.Apply(Type.Apply(Types.ChannelMpmc, elmType, _), regionType, _) => (elmType, regionType)
    case _ => throw InternalCompilerException(s"Cannot interpret '$tpe' as a channel type", tpe.loc)
  }

  /**
    * An expression for a channel variable called `sym`
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation): LoweredAst.Expr = {
    LoweredAst.Expr.Var(sym, mkChannelTpe(tpe, loc), loc)
  }

  /**
    * Returns a `GetChannel` expression based on `sym` and `exp`.
    */
  private def mkParWait(exp: LoweredAst.Expr, sym: Symbol.VarSym): LoweredAst.Expr = {
    val loc = exp.loc.asSynthetic
    val chExp = mkChannelExp(sym, exp.tpe, loc)
    mkGetChannel(chExp, exp.tpe, Type.IO, loc)
  }

  /**
    * Returns a full `par exp` expression.
    */
  private def mkParChannels(exp: LoweredAst.Expr, chanSymsWithExps: List[(Symbol.VarSym, LoweredAst.Expr)]): LoweredAst.Expr = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: LoweredAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, e.tpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, Type.IO, loc) // The put exp: `ch <- exp0`.
        val e3 = LoweredAst.Expr.ApplyAtomic(AtomicOp.Region, List.empty, Type.mkRegion(Type.IO, loc), Type.Pure, loc)
        val e4 = LoweredAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e2, e3), Type.Unit, Type.IO, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        LoweredAst.Expr.Stm(e4, acc, acc.tpe, Type.mkUnion(e4.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: LoweredAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(LoweredAst.Expr.Cst(Ast.Constant.Int32(1), Type.Int32, loc), mkChannelTpe(e.tpe, loc), Type.IO, loc) // The channel exp `chan 1`
        LoweredAst.Expr.Let(sym, Modifiers(List(Ast.Modifier.Synthetic)), chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), loc) // The let-binding `let ch = chan 1`
    }
  }

  /**
    * Returns a desugared let-match expression, i.e.
    * {{{
    *   let pattern = exp;
    *   body
    * }}}
    * is desugared to
    * {{{
    *   match exp {
    *     case pattern => body
    *   }
    * }}}
    */
  def mkLetMatch(pat: LoweredAst.Pattern, exp: LoweredAst.Expr, body: LoweredAst.Expr): LoweredAst.Expr = {
    val loc = exp.loc.asSynthetic
    val rule = List(LoweredAst.MatchRule(pat, None, body))
    val eff = Type.mkUnion(exp.eff, body.eff, loc)
    LoweredAst.Expr.Match(exp, rule, body.tpe, eff, loc)
  }

  /**
    * Returns an expression where the pattern variables used in `exp` are
    * bound to [[TypedAst.Expr.GetChannel]] expressions,
    * i.e.
    * {{{
    *   let pat1 = <- ch1;
    *   let pat2 = <- ch2;
    *   let pat3 = <- ch3;
    *   ...
    *   let patn = <- chn;
    *   exp
    * }}}
    */
  def mkBoundParWaits(patSymExps: List[(LoweredAst.Pattern, Symbol.VarSym, LoweredAst.Expr)], exp: LoweredAst.Expr): LoweredAst.Expr =
    patSymExps.map {
      case (p, sym, e) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, e.tpe, loc)
        (p, mkGetChannel(chExp, e.tpe, Type.IO, loc))
    }.foldRight(exp) {
      case ((pat, chan), e) => mkLetMatch(pat, chan, e)
    }

  /**
    * Returns a desugared [[TypedAst.Expr.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[LoweredAst.ParYieldFragment], exp: LoweredAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit scope: Scope, flix: Flix): LoweredAst.Expr = {
    // Partition fragments into complex and simple (vars or csts) exps.
    val (complex, varOrCsts) = frags.partition(f => isSpawnable(f.exp))

    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val (fs, lastComplex) = complex.splitAt(complex.length - 1)

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case LoweredAst.ParYieldFragment(p, e, l) => (p, mkLetSym("channel", l.asSynthetic), e) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Make expression that evaluates simple exps and the last fragment before proceeding to wait for channels.
    val desugaredYieldExp = mkParYieldCurrentThread(varOrCsts ::: lastComplex, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e) => (s, e) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast,
    LoweredAst.Expr.Cast(blockExp, None, Some(Type.Pure), tpe, eff, loc.asSynthetic)
  }

  /**
    * Returns the expression of a `ParYield` expression that should be evaluated in the current thread.
    */
  private def mkParYieldCurrentThread(exps: List[LoweredAst.ParYieldFragment], waitExps: LoweredAst.Expr): LoweredAst.Expr = {
    exps.foldRight(waitExps) {
      case (exp, acc) => mkLetMatch(exp.pat, exp.exp, acc)
    }
  }

  /**
    * Returns `true` if the ParYield fragment should be spawned in a thread. Wrapper for `isVarOrCst`.
    */
  private def isSpawnable(exp: LoweredAst.Expr): Boolean = !isVarOrCst(exp)

  /**
    * Returns `true` if `exp0` is either a literal or a variable.
    */
  private def isVarOrCst(exp0: LoweredAst.Expr): Boolean = exp0 match {
    case LoweredAst.Expr.Var(_, _, _) => true
    case LoweredAst.Expr.Cst(_: Ast.Constant, _, _) => true
    case _ => false
  }

  /**
    * Returns a TypedAst.Pattern representing a tuple of patterns.
    */
  def mkTuplePattern(patterns: List[LoweredAst.Pattern], loc: SourceLocation): LoweredAst.Pattern = {
    LoweredAst.Pattern.Tuple(patterns, Type.mkTuple(patterns.map(_.tpe), loc), loc)
  }

  /**
    * Return a list of quantified variables in the given expression `exp0`.
    *
    * A variable is quantified (i.e. *NOT* lexically bound) if it occurs in the expression `exp0`
    * but not in the constraint params `cparams0` of the constraint.
    */
  private def quantifiedVars(cparams0: List[TypedAst.ConstraintParam], exp0: TypedAst.Expr): List[(Symbol.VarSym, Type)] = {
    TypedAstOps.freeVars(exp0).toList.filter {
      case (sym, _) => isQuantifiedVar(sym, cparams0)
    }
  }

  /**
    * Returns `true` if the given variable symbol `sym` is a quantified variable according to the given constraint params `cparams0`.
    *
    * That is, the variable symbol is *NOT* lexically bound.
    */
  private def isQuantifiedVar(sym: Symbol.VarSym, cparams0: List[TypedAst.ConstraintParam]): Boolean =
    cparams0.exists(p => p.sym == sym)

  /**
    * Applies the given substitution `subst` to the given expression `exp0`.
    */
  private def substExp(exp0: LoweredAst.Expr, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.Expr = exp0 match {
    case LoweredAst.Expr.Cst(_, _, _) => exp0

    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.Expr.Var(s, tpe, loc)

    case LoweredAst.Expr.Def(_, _, _) => exp0

    case LoweredAst.Expr.Sig(_, _, _) => exp0

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = substFormalParam(fparam, subst)
      val e = substExp(exp, subst)
      LoweredAst.Expr.Lambda(p, e, tpe, loc)

    case LoweredAst.Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.Apply(e, es, tpe, eff, loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case LoweredAst.Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.Let(s, mod, e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.LetRec(s, mod, e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val s = subst.getOrElse(sym, sym)
      val e = substExp(exp, subst)
      LoweredAst.Expr.Scope(s, regionVar, e, tpe, eff, loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      val e3 = substExp(exp3, subst)
      LoweredAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Discard(e, eff, loc)

    case LoweredAst.Expr.Match(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expr.TypeMatch(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.VectorLit(es, tpe, eff, loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = substExp(exp1, subst)
      val e2 = substExp(exp2, subst)
      LoweredAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Ascribe(e, tpe, eff, loc)

    case LoweredAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      LoweredAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case LoweredAst.Expr.TryCatch(_, _, _, _, _) => ??? // TODO

    case LoweredAst.Expr.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val e = substExp(exp, subst)
      val rs = rules.map {
        case LoweredAst.HandlerRule(op, fparams, hexp) =>
          val fps = fparams.map(substFormalParam(_, subst))
          val he = substExp(hexp, subst)
          LoweredAst.HandlerRule(op, fps, he)
      }
      LoweredAst.Expr.TryWith(e, sym, rs, tpe, eff, loc)

    case LoweredAst.Expr.Do(sym, exps, tpe, eff, loc) =>
      val es = exps.map(substExp(_, subst))
      LoweredAst.Expr.Do(sym, es, tpe, eff, loc)

    case LoweredAst.Expr.NewObject(_, _, _, _, _, _) => exp0

  }

  /**
    * Applies the given substitution `subst` to the given formal param `fparam0`.
    */
  private def substFormalParam(fparam0: LoweredAst.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): LoweredAst.FormalParam = fparam0 match {
    case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
      val s = subst.getOrElse(sym, sym)
      LoweredAst.FormalParam(s, mod, tpe, src, loc)
  }

}
