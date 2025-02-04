/*
 * Copyright 2025 Chenhao Gao
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
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.{ChangeSet, Scheme, SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Pattern, RestrictableChoosePattern, Root}
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, EqualityConstraint, Input, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.MultiMap

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

object Dependencies {
  /**
    * Computes the dependency graph of the given `root`.
    *
    * Most of the time we just recursively visit the sub-nodes. We only add dependencies for three base cases:
    *   - Type
    *   - SymUse
    *   - Instance
    */
  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, Unit) = flix.phaseNew("Dependencies") {
    implicit val sctx: SharedContext = SharedContext(new ConcurrentHashMap[(Input, Input), Unit]())
    val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
    val effects = changeSet.updateStaleValues(root.effects, oldRoot.effects)(ParOps.parMapValues(_)(visitEff))
    val enums = changeSet.updateStaleValues(root.enums, oldRoot.enums)(ParOps.parMapValues(_)(visitEnum))
    val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances)(ParOps.parMapValueList(_)(visitInstance))
    val structs = changeSet.updateStaleValues(root.structs, oldRoot.structs)(ParOps.parMapValues(_)(visitStruct))
    val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait))
    val typeAliases = changeSet.updateStaleValues(root.typeAliases, oldRoot.typeAliases)(ParOps.parMapValues(_)(visitTypeAlias))

    var deps = MultiMap.empty[Input, Input]
    sctx.deps.forEach((k, _) => deps = deps + k)
    val dg = DependencyGraph(deps)
    (root.copy(
      dependencyGraph = dg,
      defs = defs,
      effects = effects,
      enums = enums,
      instances = instances,
      structs = structs,
      traits = traits,
      typeAliases = typeAliases
    ), ())
  }

  /**
    * Adds a dependency between the source and destination locations.
    * The value is fixed to () since it doesn't matter.
    */
  private def addDependency(src: SourceLocation, dst: SourceLocation)(implicit sctx: SharedContext): Unit = {
    sctx.deps.put((src.sp1.source.input, dst.sp1.source.input), ())
  }

  private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext): TypedAst.Def =  {
    visitExp(defn.exp)
    visitSpec(defn.spec)
    defn
  }

  private def visitEff(eff: TypedAst.Effect)(implicit sctx: SharedContext): TypedAst.Effect = {
    eff.ops.foreach(visitOp)
    eff
  }

  private def visitEnum(enm: TypedAst.Enum)(implicit sctx: SharedContext): TypedAst.Enum = {
    enm.cases.values.foreach(visitCase)
    enm
  }

  private def visitInstance(instance: TypedAst.Instance)(implicit sctx: SharedContext): TypedAst.Instance = {
    addDependency(instance.trt.sym.loc, instance.loc)
    instance
  }

  private def visitStruct(struct: TypedAst.Struct)(implicit sctx: SharedContext): TypedAst.Struct = {
    visitScheme(struct.sc)
    struct.fields.values.foreach(visitStructField)
    struct
  }

  private def visitTrait(trt: TypedAst.Trait)(implicit sctx: SharedContext): TypedAst.Trait = {
    trt.superTraits.foreach(visitTraitConstraint)
    trt.assocs.foreach(visitAssocTypeSig)
    trt.sigs.foreach(visitSig)
    trt.laws.foreach(visitDef)
    trt
  }

  private def visitTypeAlias(typeAlias: TypedAst.TypeAlias)(implicit sctx: SharedContext): TypedAst.TypeAlias = {
    visitType(typeAlias.tpe)
    typeAlias
  }

  private def visitExp(exp0: Expr)(implicit sctx: SharedContext): Unit = exp0 match {
    case Expr.Cst(_, tpe, _) =>
      visitType(tpe)

    case Expr.Var(_, tpe, _) =>
      visitType(tpe)

    case Expr.Hole(_, tpe, eff, _) =>
      visitType(tpe)
      visitType(eff)

    case Expr.HoleWithExp(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.OpenAs(symUse, exp, tpe, _) =>
      visitSymUse(symUse)
      visitExp(exp)
      visitType(tpe)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(fparam, exp, tpe, _) =>
      visitFormalParam(fparam)
      visitExp(exp)
      visitType(tpe)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplyDef(symUse, exps, itpe, tpe, eff, _) =>
      visitSymUse(symUse)
      exps.foreach(visitExp)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, _) =>
      visitSymUse(symUse)
      exps.foreach(visitExp)
      visitType(arrowTpe)
      visitType(tpe)
      visitType(eff)

    case Expr.ApplySig(symUse, exps, itpe, tpe, eff, _) =>
      visitSymUse(symUse)
      exps.foreach(visitExp)
      visitType(itpe)
      visitType(tpe)
      visitType(eff)

    case Expr.Unary(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Binary(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Let(bnd, exp1, exp2, tpe, eff, _) =>
      visitBinder(bnd)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, _) =>
      visitBinder(bnd)
      fparams.foreach(visitFormalParam)
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Region(tpe, _) =>
      visitType(tpe)

    case Expr.Scope(bnd, _, exp, tpe, eff, _) =>
      visitBinder(bnd)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.Stm(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Discard(exp, eff, _) =>
      visitExp(exp)
      visitType(eff)

    case Expr.Match(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitMatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.TypeMatch(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitTypeMatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.RestrictableChoose(_, exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitRestrictableChooseRule)
      visitType(tpe)
      visitType(eff)

    case Expr.Tag(sym, exps, tpe, eff, _) =>
      visitSymUse(sym)
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.RestrictableTag(sym, exps, tpe, eff, _) =>
      visitSymUse(sym)
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.Tuple(exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordSelect(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordExtend(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.RecordRestrict(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLit(exps, exp, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLoad(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ArrayLength(exp, eff, _) =>
      visitExp(exp)
      visitType(eff)

    case Expr.ArrayStore(exp1, exp2, exp3, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)
      visitType(eff)

    case Expr.StructNew(_, fields, region, tpe, eff, _) =>
      fields.foreach{ field =>
        visitSymUse(field._1)
        visitExp(field._2)
      }
      visitExp(region)
      visitType(tpe)
      visitType(eff)

    case Expr.StructGet(exp, sym, tpe, eff, _) =>
      visitExp(exp)
      visitSymUse(sym)
      visitType(tpe)
      visitType(eff)

    case Expr.StructPut(exp1, sym, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitSymUse(sym)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLit(exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, _) =>
      visitExp(exp)
      declaredType.toList.foreach(visitType)
      declaredEff.toList.foreach(visitType)
      visitType(tpe)
      visitType(eff)

    case Expr.Unsafe(exp, runEff, tpe, eff, _) =>
      visitExp(exp)
      visitType(runEff)
      visitType(tpe)
      visitType(eff)

    case Expr.Without(exp, sym, tpe, eff, _) =>
      visitExp(exp)
      visitSymUse(sym)
      visitType(tpe)
      visitType(eff)

    case Expr.TryCatch(exp, rules, tpe, eff, _) =>
      visitExp(exp)
      rules.foreach(visitCatchRule)
      visitType(tpe)
      visitType(eff)

    case Expr.Throw(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Handler(sym, rules, bodyType, bodyEff, handledEff, tpe, _) =>
      visitSymUse(sym)
      rules.foreach(visitHandlerRule)
      visitType(bodyType)
      visitType(bodyEff)
      visitType(handledEff)
      visitType(tpe)

    case Expr.RunWith(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.Do(op, exps, tpe, eff, _) =>
      visitSymUse(op)
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeConstructor(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeMethod(_, exp, exps, tpe, eff, _) =>
      visitExp(exp)
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.InvokeStaticMethod(_, exps, tpe, eff, _) =>
      exps.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.GetField(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutField(_, exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.GetStaticField(_, tpe, eff, _) =>
      visitType(tpe)
      visitType(eff)

    case Expr.PutStaticField(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.NewObject(_, _, tpe, eff, methods, _) =>
      visitType(tpe)
      visitType(eff)
      methods.foreach(visitJvmMethod)

    case Expr.NewChannel(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.PutChannel(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.SelectChannel(rules, default, tpe, eff, _) =>
      rules.foreach(visitSelectChannelRule)
      default.toList.foreach(visitExp)
      visitType(tpe)
      visitType(eff)

    case Expr.Spawn(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.ParYield(frags, exp, tpe, eff, _) =>
      frags.foreach(visitParYieldFragment)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Lazy(exp, tpe, _) =>
      visitExp(exp)
      visitType(tpe)

    case Expr.Force(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointConstraintSet(cs, tpe, _) =>
      cs.foreach(visitConstrait)
      visitType(tpe)

    case Expr.FixpointLambda(pparams, exp, tpe, eff, _) =>
      pparams.foreach(visitPParam)
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointSolve(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointFilter(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointInject(exp, _, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.FixpointProject(_, exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)

    case Expr.Error(_, tpe, eff) =>
      visitType(tpe)
      visitType(eff)

    case Expr.GetChannel(exp, tpe, eff, _) =>
      visitExp(exp)
      visitType(tpe)
      visitType(eff)
  }

  private def visitType(tpe: Type)(implicit sctx: SharedContext): Unit = tpe match {
    case Type.Apply(tpe1, tpe2, _) =>
      visitType(tpe1)
      visitType(tpe2)
    case Type.Alias(cst, args, _, loc) =>
      addDependency(cst.loc, loc)
      args.foreach(visitType)
    case Type.AssocType(_, arg, _, _) => visitType(arg)
    case Type.JvmToType(tpe, _) => visitType(tpe)
    case Type.JvmToEff(tpe, _) => visitType(tpe)
    case Type.UnresolvedJvmType(_, _) => ()
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => addDependency(sym.loc, loc)
    case Type.Cst(TypeConstructor.Struct(sym, _), loc) => addDependency(sym.loc, loc)
    case Type.Cst(_, _) => ()
    case Type.Var(_, _) => ()
    case Type.GetEff(_, tpe, _) => visitType(tpe)
  }

  private def visitSymUse(use: SymUse)(implicit sctx: SharedContext): Unit = use match {
    case SymUse.AssocTypeSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.CaseSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.DefSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.EffectSymUse(sym, qname) => addDependency(sym.loc, qname.loc)
    case SymUse.OpSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.SigSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.StructFieldSymUse(sym, loc) => addDependency(sym.loc, loc)
    case SymUse.TraitSymUse(sym, loc) => addDependency(sym.loc, loc)
    case _ => ()
  }


  private def visitSpec(spec: TypedAst.Spec)(implicit sctx: SharedContext): Unit = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, declaredScheme, retTpe, eff, tconstrs, econstrs) =>
      fparams.foreach(visitFormalParam)
      visitScheme(declaredScheme)
      visitType(retTpe)
      visitType(eff)
      tconstrs.foreach(visitTraitConstraint)
      econstrs.foreach(visitEqualityConstraint)
  }

  private def visitScheme(scheme: Scheme)(implicit sctx: SharedContext): Unit = scheme match {
    case Scheme(_, tconstrs, econstrs, base) =>
      visitType(base)
      tconstrs.foreach(visitTraitConstraint)
      econstrs.foreach(visitEqualityConstraint)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit sctx: SharedContext): Unit =
    visitType(tc.arg)

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit sctx: SharedContext): Unit = {
    visitType(ec.tpe1)
    visitType(ec.tpe2)
  }

  private def visitBinder(bnd: TypedAst.Binder)(implicit sctx: SharedContext): Unit =
    visitType(bnd.tpe)

  private def visitFormalParam(fparam: TypedAst.FormalParam)(implicit sctx: SharedContext): Unit = {
    visitBinder(fparam.bnd)
    visitType(fparam.tpe)
  }

  private def visitMatchRule(r: TypedAst.MatchRule)(implicit sctx: SharedContext): Unit = {
    visitPattern(r.pat)
    visitExp(r.exp)
    r.guard.toList.foreach(visitExp)
  }

  private def visitPattern(p: TypedAst.Pattern)(implicit sctx: SharedContext): Unit = p match {
    case Pattern.Var(bnd, tpe, _) =>
      visitBinder(bnd)
      visitType(tpe)
    case Pattern.Tag(sym, pats, tpe, _) =>
      visitSymUse(sym)
      pats.foreach(visitPattern)
      visitType(tpe)
    case Pattern.Tuple(pats, tpe, _) =>
      pats.foreach(visitPattern)
      visitType(tpe)
    case Pattern.Record(pats, pat, tpe, _) =>
      pats.foreach(visitRecordLabelPattern)
      visitPattern(pat)
      visitType(tpe)
    case pat =>
      visitType(pat.tpe)
  }

  private def visitRecordLabelPattern(pattern: Record.RecordLabelPattern)(implicit sctx: SharedContext): Unit = {
    visitPattern(pattern.pat)
    visitType(pattern.tpe)
  }

  private def visitTypeMatchRule(matchRule: TypedAst.TypeMatchRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(matchRule.bnd)
    visitType(matchRule.tpe)
    visitExp(matchRule.exp)
  }

  private def visitRestrictableChooseRule(rule: TypedAst.RestrictableChooseRule)(implicit sctx: SharedContext): Unit = {
    visitExp(rule.exp)
    visitRestrictableChoosePattern(rule.pat)
  }

  private def visitRestrictableChoosePattern(pattern: TypedAst.RestrictableChoosePattern)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Tag(sym, pats, tpe, _) =>
      visitSymUse(sym)
      pats.foreach(visitVarOrWild)
      visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) =>
      visitType(tpe)
  }

  private def visitVarOrWild(pattern: TypedAst.RestrictableChoosePattern.VarOrWild)(implicit sctx: SharedContext): Unit = pattern match {
    case RestrictableChoosePattern.Var(bnd, tpe, _) =>
      visitBinder(bnd)
      visitType(tpe)
    case RestrictableChoosePattern.Wild(tpe, _) =>
      visitType(tpe)
    case RestrictableChoosePattern.Error(tpe, _) =>
      visitType(tpe)
  }

  private def visitCatchRule(catchRule: TypedAst.CatchRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(catchRule.bnd)
    visitExp(catchRule.exp)
  }

  private def visitHandlerRule(handlerRule: TypedAst.HandlerRule)(implicit sctx: SharedContext): Unit = {
    visitSymUse(handlerRule.op)
    visitExp(handlerRule.exp)
    handlerRule.fparams.foreach(visitFormalParam)
  }

  private def visitJvmMethod(method: TypedAst.JvmMethod)(implicit sctx: SharedContext): Unit = {
    visitType(method.retTpe)
    visitType(method.eff)
    visitExp(method.exp)
    method.fparams.foreach(visitFormalParam)
  }

  private def visitSelectChannelRule(rule: TypedAst.SelectChannelRule)(implicit sctx: SharedContext): Unit = {
    visitBinder(rule.bnd)
    visitExp(rule.chan)
    visitExp(rule.exp)
  }

  private def visitParYieldFragment(fragment: TypedAst.ParYieldFragment)(implicit sctx: SharedContext): Unit = {
    visitExp(fragment.exp)
    visitPattern(fragment.pat)
  }

  private def visitConstrait(constraint: TypedAst.Constraint)(implicit sctx: SharedContext): Unit = {
    visitHead(constraint.head)
    constraint.cparams.foreach(visitContraintParam)
    constraint.body.foreach(visitConstraintBody)
  }

  private def visitHead(head: TypedAst.Predicate.Head)(implicit sctx: SharedContext): Unit = head match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, tpe, _) => visitType(tpe)
      terms.foreach(visitExp)
  }

  private def visitContraintParam(cp: TypedAst.ConstraintParam)(implicit sctx: SharedContext): Unit = {
    visitType(cp.tpe)
    visitBinder(cp.bnd)
  }

  private def visitConstraintBody(cb: Body)(implicit sctx: SharedContext): Unit = cb match {
    case Body.Atom(_, _, _, _, terms, tpe, _) =>
      terms.foreach(visitPattern)
      visitType(tpe)
    case Body.Functional(outBnds, exp, _) =>
      outBnds.foreach(visitBinder)
      visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  private def visitPParam(pparam: TypedAst.PredicateParam)(implicit sctx: SharedContext): Unit =
    visitType(pparam.tpe)

  private def visitOp(Op: TypedAst.Op)(implicit sctx: SharedContext): Unit =
    visitSpec(Op.spec)

  private def visitCase(cas: TypedAst.Case)(implicit sctx: SharedContext): Unit = {
    visitScheme(cas.sc)
    cas.tpes.foreach(visitType)
  }

  private def visitStructField(structField: TypedAst.StructField)(implicit sctx: SharedContext): Unit =
    visitType(structField.tpe)

  private def visitAssocTypeSig(assoc: TypedAst.AssocTypeSig)(implicit sctx: SharedContext): Unit =
    assoc.tpe.toList.foreach(visitType)

  private def visitSig(sig: TypedAst.Sig)(implicit sctx: SharedContext): Unit = {
    visitSpec(sig.spec)
    sig.exp.toList.foreach(visitExp)
  }

  /**
    * We want to compute a set of dependency edges from an input to its dependencies.
    * In other words, we want to compute `Map[Input, Set[Input]]`.
    * However, since we are in a concurrent setting, we prefer to simply compute the set of edges `Set[(Input, Input)]`.
    * However, since Java has no `ConcurrentSet[t]` we instead use  `ConcurrentMap[(Input, Input), Unit]` to record the edges.
    */
  private case class SharedContext(deps: ConcurrentMap[(Input, Input), Unit])
}
