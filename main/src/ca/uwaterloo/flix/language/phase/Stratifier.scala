/*
 *  Copyright 2017 Magnus Madsen and Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast._
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.language.phase.PredDeps.termTypesAndDenotation
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{ParOps, Result, Validation}

import scala.annotation.tailrec

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * A negated subgoal is generalized here to a subgoal that is negated
  * or fixed, collectively called a strong dependency.
  *
  * Reports a [[StratificationError]] if the constraints cannot be stratified.
  */
object Stratifier {
  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, StratificationError] = flix.phase("Stratifier") {
    implicit val g: LabelledPrecedenceGraph = root.precedenceGraph
    implicit val r: Root = root

    // Compute the stratification at every datalog expression in the ast.
    val newDefs = ParOps.parTraverseValues(root.defs)(visitDef(_))
    val newInstances = ParOps.parTraverseValues(root.instances)(traverse(_)(visitInstance(_)))
    val newTraits = ParOps.parTraverseValues(root.traits)(visitTrait(_))

    mapN(newDefs, newInstances, newTraits) {
      case (ds, is, ts) => root.copy(defs = ds, instances = is, traits = ts)
    }
  }(DebugValidation())

  /**
    * Performs Stratification of the given trait `t0`.
    */
  private def visitTrait(t0: TypedAst.Trait)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[TypedAst.Trait, StratificationError] = {
    val newLaws = traverse(t0.laws)(visitDef(_))
    val newSigs = traverse(t0.sigs)(visitSig(_))
    mapN(newLaws, newSigs) {
      case (nl, ns) => t0.copy(laws = nl, sigs = ns)
    }
  }

  /**
    * Performs Stratification of the given sig `s0`.
    */
  private def visitSig(s0: TypedAst.Sig)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[TypedAst.Sig, StratificationError] = {
    val newExp = traverseOpt(s0.exp)(visitExp(_))
    mapN(newExp) {
      case ne => s0.copy(exp = ne)
    }
  }


  /**
    * Performs Stratification of the given instance `i0`.
    */
  private def visitInstance(i0: TypedAst.Instance)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[TypedAst.Instance, StratificationError] =
    mapN(traverse(i0.defs)(d => visitDef(d)))(ds => i0.copy(defs = ds))

  /**
    * Performs stratification of the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[Def, StratificationError] =
    mapN(visitExp(def0.exp)) {
      case e => def0.copy(exp = e)
    }

  /**
    * Performs stratification of the given expression `exp0`.
    *
    * Returns [[Success]] if the expression is stratified. Otherwise returns [[HardFailure]] with a [[StratificationError]].
    */
  private def visitExp(exp0: Expr)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[Expr, StratificationError] = exp0 match {
    case Expr.Mutated(mutExp,_,_,_,_) => visitExp(mutExp)

    case Expr.Cst(_, _, _) => Validation.success(exp0)

    case Expr.Var(_, _, _) => Validation.success(exp0)

    case Expr.Def(_, _, _) => Validation.success(exp0)

    case Expr.Sig(_, _, _) => Validation.success(exp0)

    case Expr.Hole(_, _, _) => Validation.success(exp0)

    case Expr.HoleWithExp(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.HoleWithExp(e, tpe, eff, loc)
      }

    case Expr.OpenAs(sym, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.OpenAs(sym, e, tpe, loc)
      }

    case Expr.Use(sym, alias, exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Use(sym, alias, e, loc)
      }

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Lambda(fparam, e, tpe, loc)
      }

    case Expr.Apply(exp, exps, tpe, eff, loc) =>
      mapN(visitExp(exp), traverse(exps)(visitExp)) {
        case (e, es) => Expr.Apply(e, es, tpe, eff, loc)
      }

    case Expr.Unary(sop, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Unary(sop, e, tpe, eff, loc)
      }

    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.Binary(sop, e1, e2, tpe, eff, loc)
      }

    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.Let(sym, mod, e1, e2, tpe, eff, loc)
      }

    case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.LetRec(sym, ann, mod, e1, e2, tpe, eff, loc)
      }

    case Expr.Region(_, _) =>
      Validation.success(exp0)

    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Scope(sym, regionVar, e, tpe, eff, loc)
      }

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)
      }

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.Stm(e1, e2, tpe, eff, loc)
      }

    case Expr.Discard(exp, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Discard(e, eff, loc)
      }

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case MatchRule(pat, guard, body) => mapN(traverseOpt(guard)(visitExp), visitExp(body)) {
          case (g, b) => MatchRule(pat, g, b)
        }
      }
      mapN(matchVal, rulesVal) {
        case (m, rs) => Expr.Match(m, rs, tpe, eff, loc)
      }

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val matchVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case TypeMatchRule(sym, t, body) => mapN(visitExp(body)) {
          case b => TypeMatchRule(sym, t, b)
        }
      }
      mapN(matchVal, rulesVal) {
        case (m, rs) => Expr.TypeMatch(m, rs, tpe, eff, loc)
      }

    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      val expVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case RestrictableChooseRule(pat, body) => mapN(visitExp(body))(RestrictableChooseRule(pat, _))
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => Expr.RestrictableChoose(star, e, rs, tpe, eff, loc)
      }

    case Expr.Tag(sym, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Tag(sym, e, tpe, eff, loc)
      }

    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.RestrictableTag(sym, e, tpe, eff, loc)
      }

    case Expr.Tuple(elms, tpe, eff, loc) =>
      mapN(traverse(elms)(visitExp)) {
        case es => Expr.Tuple(es, tpe, eff, loc)
      }

    case Expr.RecordEmpty(tpe, loc) =>
      Validation.success(Expr.RecordEmpty(tpe, loc))

    case Expr.RecordSelect(base, label, tpe, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expr.RecordSelect(b, label, tpe, eff, loc)
      }

    case Expr.RecordExtend(label, value, rest, tpe, eff, loc) =>
      mapN(visitExp(value), visitExp(rest)) {
        case (v, r) => Expr.RecordExtend(label, v, r, tpe, eff, loc)
      }

    case Expr.RecordRestrict(label, rest, tpe, eff, loc) =>
      mapN(visitExp(rest)) {
        case r => Expr.RecordRestrict(label, r, tpe, eff, loc)
      }

    case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      mapN(traverse(exps)(visitExp), visitExp(exp)) {
        case (es, e) => Expr.ArrayLit(es, e, tpe, eff, loc)
      }

    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)
      }

    case Expr.ArrayLoad(base, index, tpe, eff, loc) =>
      mapN(visitExp(base), visitExp(index)) {
        case (b, i) => Expr.ArrayLoad(b, i, tpe, eff, loc)
      }

    case Expr.ArrayLength(base, eff, loc) =>
      mapN(visitExp(base)) {
        case b => Expr.ArrayLength(b, eff, loc)
      }

    case Expr.ArrayStore(base, index, elm, eff, loc) =>
      mapN(visitExp(base), visitExp(index), visitExp(elm)) {
        case (b, i, e) => Expr.ArrayStore(b, i, e, eff, loc)
      }

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      mapN(traverse(exps)(visitExp)) {
        case es => Expr.VectorLit(es, tpe, eff, loc)
      }

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.VectorLoad(e1, e2, tpe, eff, loc)
      }

    case Expr.VectorLength(exp, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.VectorLength(e, loc)
      }

    case Expr.Ref(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.Ref(e1, e2, tpe, eff, loc)
      }

    case Expr.Deref(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Deref(e, tpe, eff, loc)
      }

    case Expr.Assign(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.Assign(e1, e2, tpe, eff, loc)
      }

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Ascribe(e, tpe, eff, loc)
      }

    case Expr.InstanceOf(exp, clazz, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.InstanceOf(e, clazz, loc)
      }

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      mapN(visitExp(exp))(Expr.CheckedCast(cast, _, tpe, eff, loc))

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc)
      }

    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.UncheckedMaskingCast(e, tpe, eff, loc)
      }

    case Expr.Without(exp, sym, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Without(e, sym, tpe, eff, loc)
      }

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case CatchRule(sym, clazz, e) =>
          mapN(visitExp(e))(CatchRule(sym, clazz, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expr.TryCatch(e, rs, tpe, eff, loc)
      }

    case Expr.TryWith(exp, sym, rules, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case HandlerRule(op, fparams, e) =>
          mapN(visitExp(e))(HandlerRule(op, fparams, _))
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => Expr.TryWith(e, sym, rs, tpe, eff, loc)
      }

    case Expr.Do(sym, exps, tpe, eff, loc) =>
      mapN(traverse(exps)(visitExp)) {
        case es => Expr.Do(sym, es, tpe, eff, loc)
      }

    case Expr.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expr.InvokeConstructor(constructor, as, tpe, eff, loc)
      }

    case Expr.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      mapN(visitExp(exp), traverse(args)(visitExp)) {
        case (e, as) => Expr.InvokeMethod(method, e, as, tpe, eff, loc)
      }

    case Expr.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      mapN(traverse(args)(visitExp)) {
        case as => Expr.InvokeStaticMethod(method, as, tpe, eff, loc)
      }

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.GetField(field, e, tpe, eff, loc)
      }

    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.PutField(field, e1, e2, tpe, eff, loc)
      }

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      Validation.success(Expr.GetStaticField(field, tpe, eff, loc))

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.PutStaticField(field, e, tpe, eff, loc)
      }

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      mapN(traverse(methods)(visitJvmMethod)) {
        case ms => Expr.NewObject(name, clazz, tpe, eff, ms, loc)
      }

    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (r, e) => Expr.NewChannel(r, e, tpe, eff, loc)
      }

    case Expr.GetChannel(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.GetChannel(e, tpe, eff, loc)
      }

    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => Expr.PutChannel(e1, e2, tpe, eff, loc)
      }

    case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      val rulesVal = traverse(rules) {
        case SelectChannelRule(sym, chan, exp) => mapN(visitExp(chan), visitExp(exp)) {
          case (c, e) => SelectChannelRule(sym, c, e)
        }
      }

      val defaultVal = default match {
        case None => Validation.success(None)
        case Some(exp) =>
          mapN(visitExp(exp)) {
            case e => Some(e)
          }
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => Expr.SelectChannel(rs, d, tpe, eff, loc)
      }

    case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (r, e) => Expr.Spawn(r, e, tpe, eff, loc)
      }

    case Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fragsVal = traverse(frags) {
        case ParYieldFragment(p, e, l) => mapN(visitExp(e)) {
          case e1 => ParYieldFragment(p, e1, l)
        }
      }
      mapN(fragsVal, visitExp(exp)) {
        case (fs, e) => Expr.ParYield(fs, e, tpe, eff, loc)
      }

    case Expr.Lazy(exp, tpe, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Lazy(e, tpe, loc)
      }

    case Expr.Force(exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.Force(e, tpe, eff, loc)
      }

    case Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(stf) {
        case _ =>
          val cs = cs0.map(reorder)
          Expr.FixpointConstraintSet(cs, tpe, loc)
      }

    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)
      mapN(stf) {
        case _ => Expr.FixpointLambda(pparams, exp, tpe, eff, loc)
      }

    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(visitExp(exp1), visitExp(exp2), stf) {
        case (e1, e2, _) => Expr.FixpointMerge(e1, e2, tpe, eff, loc)
      }

    case Expr.FixpointSolve(exp, tpe, eff, loc) =>
      // Compute the stratification.
      val stf = stratify(g, tpe, loc)

      mapN(visitExp(exp), stf) {
        case (e, _) => Expr.FixpointSolve(e, tpe, eff, loc)
      }

    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.FixpointFilter(pred, e, tpe, eff, loc)
      }

    case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.FixpointInject(e, pred, tpe, eff, loc)
      }

    case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => Expr.FixpointProject(pred, e, tpe, eff, loc)
      }

    case Expr.Error(m, tpe, eff) =>
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.success(Expr.Error(m, tpe, eff))

  }

  private def visitJvmMethod(method: JvmMethod)(implicit root: Root, g: LabelledPrecedenceGraph, flix: Flix): Validation[JvmMethod, StratificationError] = method match {
    case JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      mapN(visitExp(exp)) {
        case e => JvmMethod(ident, fparams, e, tpe, eff, loc)
      }
  }

  /**
    * Reorders a constraint such that its negated atoms and loop predicates occur last.
    */
  private def reorder(c0: Constraint): Constraint = {
    /**
      * Returns `true` if the body predicate is negated.
      */
    def isNegativeOrLoop(p: Predicate.Body): Boolean = p match {
      case Predicate.Body.Atom(_, _, Polarity.Negative, _, _, _, _) => true
      case Predicate.Body.Functional(_, _, _) => true
      case _ => false
    }

    // Order the predicates from first to last.
    val last = c0.body filter isNegativeOrLoop
    val first = c0.body filterNot isNegativeOrLoop

    // Reassemble the constraint.
    c0.copy(body = first ::: last)
  }

  /**
    * Computes the stratification of the given labelled graph `g` for the given row type `tpe` at the given source location `loc`.
    */
  private def stratify(g: LabelledPrecedenceGraph, tpe: Type, loc: SourceLocation)(implicit flix: Flix): Validation[Unit, StratificationError] = {
    // The key is the set of predicates that occur in the row type.
    val key = predicateSymbolsOf(tpe)

    // Compute the restricted labelled graph.
    val rg = g.restrict(key, labelEq(_, _))

    // Compute the stratification.
    UllmansAlgorithm.stratify(labelledGraphToDependencyGraph(rg), tpe, loc) match {
      case Result.Ok(_) => Validation.success(())
      case Result.Err(e) => Validation.toSoftFailure((), e)
    }
  }

  /**
    * Returns the map of predicates that appears in the given Schema `tpe`.
    */
  private def predicateSymbolsOf(tpe: Type): Map[Name.Pred, Label] = {
    @tailrec
    def visitType(tpe: Type, acc: Map[Name.Pred, Label]): Map[Name.Pred, Label] = tpe match {
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _), rest, _) =>
        val (terms, labelDen) = termTypesAndDenotation(predType)
        val label = Label(pred, labelDen, terms.length, terms)
        visitType(rest, acc + (pred -> label))
      case _ => acc
    }

    Type.eraseAliases(tpe) match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, _), schemaRow, _) => visitType(schemaRow, Map.empty)
      case _ =>
        // We would like to assume that `tpe` must be a schema type. However, because type inference is resilient it is
        // possible that the stratifier is run on an expression where type inference was only partially successful.
        // Hence we may arrive here. If that happens there is nothing to be done.
        Map.empty
    }
  }

  /**
    * Returns `true` if the two given labels `l1` and `l2` are considered equal.
    */
  private def labelEq(l1: Label, l2: Label)(implicit flix: Flix): Boolean = {
    val isEqPredicate = l1.pred == l2.pred
    val isEqDenotation = l1.den == l2.den
    val isEqArity = l1.arity == l2.arity
    val isEqTermTypes = l1.terms.zip(l2.terms).forall {
      case (t1, t2) => Unification.unifiesWith(t1, t2, RigidityEnv.empty, ListMap.empty) // TODO ASSOC-TYPES empty right?
    }

    isEqPredicate && isEqDenotation && isEqArity && isEqTermTypes
  }

  /**
    * Computes the dependency graph from the labelled graph, throwing the labels away.
    * If a labelled edge is either negative or fixed it is transformed to a strong edge.
    */
  private def labelledGraphToDependencyGraph(g: LabelledPrecedenceGraph): UllmansAlgorithm.DependencyGraph =
    g.edges.map {
      case LabelledEdge(head, Polarity.Positive, Fixity.Loose, _, body, loc) =>
        // Positive, loose edges require that the strata of the head is equal to,
        // or below, the strata of the body hence a weak edge.
        UllmansAlgorithm.DependencyEdge.Weak(head, body, loc)
      case LabelledEdge(head, _, _, _, body, loc) =>
        // Edges that are either negatively bound or fixed are strong since they require
        // that the strata of the head is strictly higher than the strata of the body.
        UllmansAlgorithm.DependencyEdge.Strong(head, body, loc)
    }.toSet

}
