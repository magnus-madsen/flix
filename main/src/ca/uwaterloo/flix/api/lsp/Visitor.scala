/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.*
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.TypedAst.{AssocTypeDef, Instance, *}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}

object Visitor {
  /**
    * Visits the root AST node and recursively visits
    * all children that are accepted by the acceptor,
    * "consuming" each node visited (including the root).
    *
    * By "consuming" what is meant is that the node is used
    * as the input to a function of the consumer associated with
    * the AST node type. Such consumer functions have no output,
    * but can have effects. For instance, the `consumer` can be defined such that each
    * expression containing a specific variable is collected in a list, via
    * mutation a variable.
    *
    * Note that if a node is not accepted, none of the nodes in its
    * subtree will be visited either. For instance, if a [[Def]] is not
    * accepted, none of the expressions in its `exp` will be visited eiter,
    * even if they would otherwise be accepted by the `acceptor`.
    *
    * @param root      The AST root node.
    * @param consumer  A [[Consumer]] that defines what to do when visiting different types of AST nodes.
    * @param acceptor  An [[Acceptor]] that defines the criteria for whether an AST node should be visited.
    */
  def visitRoot(root: Root, consumer: Consumer, acceptor: Acceptor): Unit = {

    implicit val c: Consumer = consumer
    implicit val a: Acceptor = acceptor

    // NB: the signatures in `root.sigs` are not visited here, since they will be visited after
    // their corresponding trait

    root.defs.values.foreach(visitDef)

    root.effects.values.foreach(visitEffect)

    root.enums.values.foreach(visitEnum)

    root.instances.values.flatten.foreach(visitInstance)

    root.structs.values.foreach(visitStruct)

    root.traits.values.foreach(visitTrait)

    root.typeAliases.values.foreach(visitTypeAlias)
  }

  private def visitEnum(enm: Enum)(implicit a: Acceptor, c: Consumer): Unit = {
    val Enum(_, ann, _, _, tparams, derives, cases, loc) = enm
    if (!a.accept(loc)) { return }

    c.consumeEnum(enm)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    visitDeriveList(derives)
    cases.values.foreach(visitCase)
  }

  private def visitDeriveList(derivations: Derivations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivations(traits, loc) = derivations
    if (!a.accept(loc)) { return }

    c.consumeDerivations(derivations)

    traits.foreach(visitDerive)
  }

  private def visitDerive(derive: Derivation)(implicit a: Acceptor, c: Consumer): Unit = {
    val Derivation(_, loc) = derive
    if (!a.accept(loc)) { return }

    c.consumeDerivation(derive)
  }

  private def visitCase(cse: Case)(implicit a: Acceptor, c: Consumer): Unit = {
    val Case(_, tpes, _, loc) = cse
    if (!a.accept(loc)) { return }

    c.consumeCase(cse)
    tpes.foreach(visitType)
  }

  private def visitInstance(ins: Instance)(implicit a: Acceptor, c: Consumer): Unit = {
    val Instance(_, ann, _, trt, _, tconstrs, assocs, defs, _, loc) = ins
    if (!a.accept(loc)) { return }

    c.consumeInstance(ins)

    visitAnnotations(ann)
    visitTraitSymUse(trt)
    tconstrs.foreach(visitTraitConstraint)
    assocs.foreach(visitAssocTypeDef)
    defs.foreach(visitDef)
  }

  private def visitTraitSymUse(symUse: TraitSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeTraitSymUse(symUse)
  }

  private def visitTraitConstraint(tc: TraitConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitConstraint(head, arg, loc) = tc
    if (!a.accept(loc)) { return }

    c.consumeTraitConstraint(tc)

    visitTraitConstraintHead(head)
    visitType(arg)
  }

  private def visitTraitConstraintHead(tcHead: TraitConstraint.Head)(implicit a: Acceptor, c: Consumer): Unit = {
    val TraitConstraint.Head(_, loc) = tcHead
    if (!a.accept(loc)) { return }

    c.consumeTraitConstraintHead(tcHead)
  }

  private def visitAssocTypeDef(tdefn: AssocTypeDef)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeDef(_, _, symUse, arg, _, loc) = tdefn
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeDef(tdefn)

    visitAssocTypeSymUse(symUse)
    visitType(arg)
  }

  private def visitAssocTypeSymUse(symUse: AssocTypeSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeSymUse(symUse)
  }

  private def visitSig(sig: Sig)(implicit a: Acceptor, c: Consumer): Unit = {
    val Sig(_, spec, exp, loc) = sig
    if (!a.accept(loc)) { return }

    c.consumeSig(sig)

    visitSpec(spec)
    exp.foreach(visitExpr)
  }

  private def visitStruct(struct: Struct)(implicit a: Acceptor, c: Consumer): Unit = {
    val Struct(_, ann, _, _, tparams, _, fields, loc) = struct
    if (!a.accept(loc)) { return }

    c.consumeStruct(struct)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fields.values.foreach(visitStructField)
  }

  private def visitStructField(field: StructField)(implicit a: Acceptor, c: Consumer): Unit = {
    val StructField(_, tpe, loc) = field
    if (!a.accept(loc)) { return }

    c.consumeStructField(field)

    visitType(tpe)
  }

  private def visitTrait(t: Trait)(implicit a: Acceptor, c: Consumer): Unit = {
    val Trait(_, ann, _, _, tparam, superTraits, assocs, sigs, laws, loc) = t
    if (!a.accept(loc)) { return }

    c.consumeTrait(t)

    visitAnnotations(ann)
    visitTypeParam(tparam)
    superTraits.foreach(visitTraitConstraint)
    assocs.foreach(visitAssocTypeSig)
    sigs.foreach(visitSig)
    laws.foreach(visitDef)
  }

  private def visitAssocTypeSig(assoc: AssocTypeSig)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeSig(_, _, _, tparam, _, tpe, loc) = assoc
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeSig(assoc)

    visitTypeParam(tparam)
    tpe.foreach(visitType)
  }

  private def visitEffect(eff: Effect)(implicit a: Acceptor, c: Consumer): Unit = {
    val Effect(_, ann, _, _, ops, loc) = eff
    if (!a.accept(loc)) { return }

    c.consumeEff(eff)

    visitAnnotations(ann)
    ops.foreach(visitOp)
  }

  private def visitOp(op: Op)(implicit a: Acceptor, c: Consumer): Unit = {
    val Op(_, spec, loc) = op
    if (!a.accept(loc)) { return }

    c.consumeOp(op)

    visitSpec(spec)
  }

  private def visitDef(defn: Def)(implicit a: Acceptor, c: Consumer): Unit = {
    val Def(_, spec, exp, loc) = defn
    if (!a.accept(loc)) { return }

    c.consumeDef(defn)

    visitSpec(spec)
    visitExpr(exp)
  }

  private def visitSpec(spec: Spec)(implicit a: Acceptor, c: Consumer): Unit = {
    val Spec(_, ann, _, tparams, fparams, _, retTpe, eff, tconstrs, econstrs) = spec

    // NB: Specs should not be consumed and when they occur, their corresponding
    // Def, Sig or Op is visited, they should always be visited as well.

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    fparams.foreach(visitFormalParam)
    visitType(retTpe)
    visitType(eff)
    tconstrs.foreach(visitTraitConstraint)
    econstrs.foreach(visitEqualityConstraint)
  }

  private def visitEqualityConstraint(ec: EqualityConstraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val EqualityConstraint(cst, tpe1, tpe2, loc) = ec
    if (!a.accept(loc)) { return }

    c.consumeEqualityConstraint(ec)

    visitAssocTypeConstructor(cst)
    visitType(tpe1)
    visitType(tpe2)
  }

  private def visitAssocTypeConstructor(tcst: AssocTypeConstructor)(implicit a: Acceptor, c: Consumer): Unit = {
    val AssocTypeConstructor(_, loc) = tcst
    if (!a.accept(loc)) { return }

    c.consumeAssocTypeConstructor(tcst)
  }

  private def visitTypeAlias(alias: TypeAlias)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeAlias(_, ann, _, _, tparams, tpe, loc) = alias
    if (!a.accept(loc)) { return }

    c.consumeTypeAlias(alias)

    visitAnnotations(ann)
    tparams.foreach(visitTypeParam)
    visitType(tpe)
  }

  private def visitTypeParam(tparam: TypeParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeParam(_, _, loc) = tparam
    if (!a.accept(loc)) { return }

    c.consumeTypeParam(tparam)
  }

  private def visitExpr(expr: Expr)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(expr.loc)) { return }

    c.consumeExpr(expr)

    expr match {
      case Expr.Cst(_, _, _) => ()
      case Expr.Var(_, _, _) => ()
      case Expr.Hole(_, _, _, _) => ()

      case Expr.HoleWithExp(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.OpenAs(_, _, _, _) => () // Not visited, unsupported feature.

      case Expr.Use(_, _, exp, _) =>
        visitExpr(exp)

      case Expr.Lambda(fparam, exp, _, _) =>
        visitFormalParam(fparam)
        visitExpr(exp)

      case Expr.ApplyClo(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.ApplyDef(symUse, exps, _, _, _, _) =>
        visitDefSymUse(symUse)
        exps.foreach(visitExpr)

      case Expr.ApplyLocalDef(symUse, exps, _, _, _, _) =>
        visitLocalDefSymUse(symUse)
        exps.foreach(visitExpr)

      case Expr.ApplySig(symUse, exps, _, _, _, _) =>
        visitSigSymUse(symUse)
        exps.foreach(visitExpr)

      case Expr.Unary(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.Binary(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Let(bnd, exp1, exp2, _, _, _) =>
        visitBinder(bnd)
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.LocalDef(_, fparams, exp1, exp2, _, _, _) =>
        fparams.foreach(visitFormalParam)
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Region(_, _) => ()

      case Expr.Scope(bnd, regionVar, exp, _, _, _) =>
        visitBinder(bnd)
        visitType(regionVar)
        visitExpr(exp)

      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.Stm(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.Discard(exp, _, _) =>
        visitExpr(exp)

      case Expr.Match(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitMatchRule)

      case Expr.TypeMatch(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitTypeMatchRule)

      case Expr.RestrictableChoose(_, _, _, _, _, _) => () // Not visited, unsupported feature.

      case Expr.Tag(symUse, exps, _, _, _) =>
        visitCaseSymUse(symUse)
        exps.foreach(visitExpr)

      case Expr.RestrictableTag(_, _, _, _, _) => () // Not visited, unsupported feature.

      case Expr.Tuple(exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Expr.RecordEmpty(_, _) => ()

      case Expr.RecordSelect(exp, _, _, _, _) =>
        visitExpr(exp)

      case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.RecordRestrict(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.ArrayLit(exps, exp, _, _, _) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.ArrayLength(exp, _, _) =>
        visitExpr(exp)

      case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)
        visitExpr(exp3)

      case Expr.StructNew(_, fields, region, _, _, _) =>
        fields.foreach{
          case (symUse, exp) =>
            visitStructFieldSymUse(symUse)
            visitExpr(exp)
        }
        visitExpr(region)

      case Expr.StructGet(exp, symUse, _, _, _) =>
        visitExpr(exp)
        visitStructFieldSymUse(symUse)

      case Expr.StructPut(exp1, symUse, exp2, _, _, _) =>
        visitExpr(exp1)
        visitStructFieldSymUse(symUse)
        visitExpr(exp2)

      case Expr.VectorLit(exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Expr.VectorLoad(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.VectorLength(exp, _) =>
        visitExpr(exp)

      case Expr.Ascribe(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.InstanceOf(exp, _, _) =>
        visitExpr(exp)

      case Expr.CheckedCast(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.UncheckedCast(exp, declaredType, declaredEff, _, _, _) =>
        visitExpr(exp)
        declaredType.foreach(visitType)
        declaredEff.foreach(visitType)

      case Expr.Unsafe(exp, runEff, _, _, _) =>
        // runEff is first syntactically
        visitType(runEff)
        visitExpr(exp)

      case Expr.Without(exp, effUse, _, _, _) =>
        visitExpr(exp)
        visitEffectSymUse(effUse)

      case Expr.TryCatch(exp, rules, _, _, _) =>
        visitExpr(exp)
        rules.foreach(visitCatchRule)

      case Expr.Throw(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.TryWith(exp, effUse, rules, _, _, _) =>
        visitExpr(exp)
        visitEffectSymUse(effUse)
        rules.foreach(visitHandlerRule)

      case Expr.Do(op, exps, _, _, _) =>
        visitOpSymUse(op)
        exps.foreach(visitExpr)

      case Expr.InvokeConstructor(_, exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
        visitExpr(exp)
        exps.foreach(visitExpr)

      case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
        exps.foreach(visitExpr)

      case Expr.GetField(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.PutField(_, exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.GetStaticField(_, _, _, _) => ()

      case Expr.PutStaticField(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.NewObject(_, _, _, _, methods, _) =>
        methods.foreach(visitJvmMethod)

      case Expr.NewChannel(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.GetChannel(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.PutChannel(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.SelectChannel(rules, default, _, _, _) =>
        rules.foreach(visitSelectChannelRule)
        default.foreach(visitExpr)

      case Expr.Spawn(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.ParYield(frags, exp, _, _, _) =>
        visitExpr(exp)
        frags.foreach(visitParYieldFrag)

      case Expr.Lazy(exp, _, _) =>
        visitExpr(exp)

      case Expr.Force(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.FixpointConstraintSet(cs, _, _) =>
        cs.foreach(visitConstraint)

      case Expr.FixpointLambda(pparams, exp, _, _, _) =>
        pparams.foreach(visitPredicateParam)
        visitExpr(exp)

      case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
        visitExpr(exp1)
        visitExpr(exp2)

      case Expr.FixpointSolve(exp, _, _, _) =>
        visitExpr(exp)

      case Expr.FixpointFilter(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.FixpointInject(exp, _, _, _, _) =>
        visitExpr(exp)

      case Expr.FixpointProject(_, exp, _, _, _) =>
        visitExpr(exp)

      case Expr.Error(_, _, _) => ()
    }
  }

  private def visitBinder(bnd: Binder)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(bnd.sym.loc)) { return }

    c.consumeBinder(bnd)
  }

  private def visitSigSymUse(symUse: SigSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val SigSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeSigSymUse(symUse)
  }

  private def visitLocalDefSymUse(symUse: LocalDefSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val LocalDefSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeLocalDefSym(symUse)
  }

  private def visitPredicateParam(pparam: PredicateParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val PredicateParam(_, _, loc) = pparam
    if (!a.accept(loc)) { return }

    c.consumePredicateParam(pparam)
  }

  private def visitStructFieldSymUse(symUse: StructFieldSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val StructFieldSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }

    c.consumeStructFieldSymUse(symUse)
  }

  private def visitEffectSymUse(effUse: EffectSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val EffectSymUse(_, qname) = effUse
    if (!a.accept(qname.loc)) { return }

    c.consumeEffectSymUse(effUse)
  }

  private def visitJvmMethod(method: JvmMethod)(implicit a: Acceptor, c: Consumer): Unit = {
    val JvmMethod(_, fparams, exp, retTpe, _, loc) = method
    if (!a.accept(loc)) { return }

    c.consumeJvmMethod(method)

    fparams.foreach(visitFormalParam)
    visitExpr(exp)
    visitType(retTpe)
  }

  private def visitDefSymUse(symUse: DefSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val DefSymUse(_, loc) = symUse
    if(!a.accept(loc)) { return }

    c.consumeDefSymUse(symUse)
  }

  private def visitSelectChannelRule(rule: SelectChannelRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val SelectChannelRule(bnd, chan, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since SelectChannelRule don't have locations
    val insideRule = a.accept(chan.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeSelectChannelRule(rule)

    visitBinder(bnd)
    visitExpr(chan)
    visitExpr(exp)
  }

  private def visitFormalParam(fparam: FormalParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val FormalParam(bnd, _, tpe, _, loc) = fparam
    if (!a.accept(loc)) { return }

    c.consumeFormalParam(fparam)

    visitBinder(bnd)
    visitType(tpe)
  }

  private def visitHandlerRule(rule: HandlerRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val HandlerRule(op, fparams, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since HandlerRules don't have locations
    val insideRule = a.accept(op.qname.loc) || fparams.map(_.loc).exists(a.accept) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeHandlerRule(rule)

    visitOpSymUse(op)
    fparams.foreach(visitFormalParam)
    visitExpr(exp)
  }

  private def visitOpSymUse(symUse: OpSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val OpSymUse(_, qname) = symUse
    if (!a.accept(qname.loc)) { return }

    c.consumeOpSymUse(symUse)
  }

  private def visitParYieldFrag(frag: ParYieldFragment)(implicit a: Acceptor, c: Consumer): Unit = {
    val ParYieldFragment(pat, exp, loc) = frag
    if (!a.accept(loc)) { return }

    c.consumeParYieldFragment(frag)

    visitPattern(pat)
    visitExpr(exp)
  }

  private def visitMatchRule(rule: MatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val MatchRule(pat, guard, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since MatchRules don't have locations
    val insideRule = a.accept(pat.loc) || guard.map(_.loc).exists(a.accept) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeMatchRule(rule)

    visitPattern(pat)
    guard.foreach(visitExpr)
    visitExpr(exp)
  }

  private def visitTypeMatchRule(rule: TypeMatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val TypeMatchRule(bnd, tpe, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since TypeMatchRules don't have locations
    val insideRule = a.accept(bnd.sym.loc) || a.accept(tpe.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeTypeMatchRule(rule)

    visitBinder(bnd)
    visitType(tpe)
    visitExpr(exp)
  }

  private def visitType(tpe: Type)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(tpe.loc)) { return }

    c.consumeType(tpe)

    tpe match {
      case Type.Var(_, _) => ()
      case Type.Cst(_, _) => ()
      case Type.Apply(t1, t2, _) =>
        visitType(t1)
        visitType(t2)
      case Type.Alias(_, args, _, _) => args.foreach(visitType)
      case Type.AssocType(_, t, _, _) => visitType(t)
      case Type.JvmToType(t, _) => visitType(t)
      case Type.JvmToEff(t, _) => visitType(t)
      case Type.UnresolvedJvmType(_, _) => ()
    }
  }

  private def visitAnnotations(anns: Annotations)(implicit a: Acceptor, c: Consumer): Unit = {
    val Annotations(ls) = anns

    ls.foreach(visitAnnotation)
  }

  private def visitAnnotation(ann: Annotation)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(ann.loc)) { return }

    c.consumeAnnotation(ann)
  }

  private def visitCatchRule(rule: CatchRule)(implicit a: Acceptor, c: Consumer): Unit = {
    val CatchRule(bnd, _, exp) = rule
    // TODO `insideRule` is a hack, should be removed eventually. Necessary for now since CatchRules don't have locations
    val insideRule = a.accept(bnd.sym.loc) || a.accept(exp.loc)
    if (!insideRule) { return }

    c.consumeCatchRule(rule)

    visitBinder(bnd)
    visitExpr(exp)
  }

  private def visitConstraint(cst: Constraint)(implicit a: Acceptor, c: Consumer): Unit = {
    val Constraint(cparams, head, body, loc) = cst
    if (!a.accept(loc)) { return }

    c.consumeConstraint(cst)

    cparams.foreach(visitConstraintParam)
    visitPredicate(head)
    body.foreach(visitPredicate)
  }

  private def visitConstraintParam(cparam: ConstraintParam)(implicit a: Acceptor, c: Consumer): Unit = {
    val ConstraintParam(bnd, _, loc) = cparam
    if (!a.accept(loc)) { return }

    c.consumeConstraintParam(cparam)

    visitBinder(bnd)
  }

  private def visitPredicate(p: Predicate)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(p.loc)) { return }

    c.consumePredicate(p)

    p match {
    	case Predicate.Head.Atom(_, _, terms, _, _) => terms.foreach(visitExpr)
    	case Predicate.Body.Atom(_, _, _, _, terms, _, _) => terms.foreach(visitPattern)
    	case Predicate.Body.Functional(_, exp, _) => visitExpr(exp)
    	case Predicate.Body.Guard(exp, _) => visitExpr(exp)
    }
  }

  private def visitPattern(pat: Pattern)(implicit a: Acceptor, c: Consumer): Unit = {
    if (!a.accept(pat.loc)) { return }

    c.consumePattern(pat)

    pat match {
    	case Wild(_, _) => ()
    	case Var(varSym, _, _) => visitBinder(varSym)
    	case Cst(_, _, _) => ()
    	case Tag(sym, pats, _, _) =>
    	  visitCaseSymUse(sym)
        pats.foreach(visitPattern)
    	case Tuple(pats, _, _) =>
    	  pats.foreach(visitPattern)
    	case Record(pats, pat, _, _) =>
    	  pats.foreach(visitRecordLabelPattern)
    	  visitPattern(pat)
    	case RecordEmpty(_, _) =>
    	case Pattern.Error(_, _) =>
    }
  }

  private def visitRecordLabelPattern(pat: RecordLabelPattern)(implicit a: Acceptor, c: Consumer): Unit = {
    val RecordLabelPattern(_, p, _, loc) = pat

    if (!a.accept(loc)) { return }

    c.consumeRecordLabelPattern(pat)

    visitPattern(p)
  }

  private def visitCaseSymUse(symUse: CaseSymUse)(implicit a: Acceptor, c: Consumer): Unit = {
    val CaseSymUse(_, loc) = symUse
    if (!a.accept(loc)) { return }
    c.consumeCaseSymUse(symUse)
  }

  /**
    * Returns `true` if the position `pos` within the file given by path `uri` is contained within `loc`.
    * Returns `false` otherwise.
    *
    * @param uri  the path of the file that `pos` is within.
    * @param pos  the position that we want to know whether is within `loc`.
    * @param loc  the `SourceLocation` that want to know if `pos` is within.
    * @return `true` if `pos` in file at path `uri` is within `loc`. `false` otherwise.
    */
  def inside(uri: String, pos: Position)(loc: SourceLocation): Boolean = {
    val sameSource = uri == loc.source.name
    if (!sameSource) { return false }

    val afterStart = loc.beginLine < pos.line ||
      (loc.beginLine == pos.line && loc.beginCol <= pos.character)
    if (!afterStart) { return false }


    val beforeEnd = pos.line < loc.endLine ||
      (pos.line == loc.endLine && pos.character < loc.endCol)
    if (!beforeEnd) { return false }

    true
  }
}
