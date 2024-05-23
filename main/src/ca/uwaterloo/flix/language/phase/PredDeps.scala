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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Label, LabelledEdge, LabelledPrecedenceGraph}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * The [[PredDeps]] class computes the [[LabelledPrecedenceGraph]] of the whole program,
  * which represents all static dependencies between datalog predicates.
  *
  * The computed [[LabelledPrecedenceGraph]] is used in [[Stratifier]].
  */
object PredDeps {

  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("PredDeps") {
    // Compute an over-approximation of the dependency graph for all constraints in the program.
    val defExps = root.defs.values.map(_.exp)
    val instanceExps = root.instances.values.flatten.flatMap(_.defs).map(_.exp)
    val traitExps = root.traits.values.flatMap(t => t.laws.map(_.exp) ++ t.sigs.flatMap(_.exp))
    val allExps = defExps ++ instanceExps ++ traitExps

    val g = ParOps.parAgg(allExps, LabelledPrecedenceGraph.empty)({
      case (acc, d) => acc + visitExp(d)
    }, _ + _)

    Validation.success(root.copy(precedenceGraph = g))
  }(DebugValidation())

  /**
    * Returns the term types of the given relational or latticenal type.
    */
  def termTypesAndDenotation(tpe: Type): (List[Type], Denotation) = eraseAliases(tpe) match {
    case Type.Apply(Type.Cst(tc, _), t, _) =>
      val den = tc match {
        case TypeConstructor.Relation => Denotation.Relational
        case TypeConstructor.Lattice => Denotation.Latticenal
        case _ => throw InternalCompilerException(s"Unexpected non-denotation type constructor: '$tc'", tpe.loc)
      }
      t.baseType match {
        case Type.Cst(TypeConstructor.Tuple(_), _) => (t.typeArguments, den) // Multi-ary
        case Type.Cst(TypeConstructor.Unit, _) => (Nil, den)
        case _ => (List(t), den) // Unary
      }
    case _ =>
      // Resilience: We would want a relation or lattice, but type inference may have failed.
      // If so, we simply return the empty list of term types with a relational denotation.
      (Nil, Denotation.Relational)
  }

  /**
    * Returns the labelled graph of the given expression `exp0`.
    */
  private def visitExp(exp0: Expr): LabelledPrecedenceGraph = exp0 match {
    case Expr.Cst(_, _, _) => LabelledPrecedenceGraph.empty

    case Expr.Var(_, _, _) => LabelledPrecedenceGraph.empty

    case Expr.Def(_, _, _) => LabelledPrecedenceGraph.empty

    case Expr.Sig(_, _, _) => LabelledPrecedenceGraph.empty

    case Expr.Hole(_, _, _) => LabelledPrecedenceGraph.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Apply(exp, exps, _, _, _) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc + visitExp(exp)
      }

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.Let(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.LetRec(_, _, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.Region(_, _) =>
      LabelledPrecedenceGraph.empty

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) + visitExp(exp2) + visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      val dg = visitExp(exp)
      rules.foldLeft(dg) {
        case (acc, MatchRule(_, g, b)) => acc + g.map(visitExp).getOrElse(LabelledPrecedenceGraph.empty) + visitExp(b)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      val dg = visitExp(exp)
      rules.foldLeft(dg) {
        case (acc, TypeMatchRule(_, _, b)) => acc + visitExp(b)
      }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      val dg1 = visitExp(exp)
      val dg2 = rules.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, RestrictableChooseRule(_, body)) => acc + visitExp(body)
      }
      dg1 + dg2

    case Expr.Tag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.RestrictableTag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Tuple(elms, _, _, _) =>
      elms.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.RecordEmpty(_, _) =>
      LabelledPrecedenceGraph.empty

    case Expr.RecordSelect(base, _, _, _, _) =>
      visitExp(base)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value) + visitExp(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.foldLeft(visitExp(exp)) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) + visitExp(exp2) + visitExp(exp3)

    case Expr.ArrayLoad(base, index, _, _, _) =>
      visitExp(base) + visitExp(index)

    case Expr.ArrayLength(base, _, _) =>
      visitExp(base)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      visitExp(base) + visitExp(index) + visitExp(elm)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ref(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedMaskingCast(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc + visitExp(e)
      }

    case Expr.TryWith(exp, _, rules, _, _, _) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, HandlerRule(_, _, e)) => acc + visitExp(e)
      }

    case Expr.Do(_, exps, _, _, _) =>
      exps.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, exp) => acc + visitExp(exp)
      }

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      args.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      args.foldLeft(visitExp(exp)) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      args.foldLeft(LabelledPrecedenceGraph.empty) {
        case (acc, e) => acc + visitExp(e)
      }

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      LabelledPrecedenceGraph.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, _, _) =>
      LabelledPrecedenceGraph.empty

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val dg = default match {
        case None => LabelledPrecedenceGraph.empty
        case Some(d) => visitExp(d)
      }

      rules.foldLeft(dg) {
        case (acc, SelectChannelRule(_, exp1, exp2)) => acc + visitExp(exp1) + visitExp(exp2)
      }

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.foldLeft(visitExp(exp)) {
        case (acc, ParYieldFragment(_, e, _)) => acc + visitExp(e)
      }

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(LabelledPrecedenceGraph.empty) {
        case (dg, c) => dg + visitConstraint(c)
      }

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) + visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      LabelledPrecedenceGraph.empty

    case Expr.Mutated(mutExp, _, _, _, _) => visitExp(mutExp)
  }

  /**
    * Returns the labelled graph of the given constraint `c0`.
    */
  private def visitConstraint(c: Constraint): LabelledPrecedenceGraph = c match {
    case Constraint(_, Predicate.Head.Atom(headPred, den, _, headTpe, _), body0, _) =>
      val (headTerms, _) = termTypesAndDenotation(headTpe)

      // We add all body predicates and the head to the labels of each edge
      val bodyLabels: Vector[Label] = body0.collect {
        case Body.Atom(bodyPred, den, _, _, _, bodyTpe, _) =>
          val (terms, _) = termTypesAndDenotation(bodyTpe)
          Label(bodyPred, den, terms.length, terms)
      }.toVector

      val labels = bodyLabels :+ Label(headPred, den, headTerms.length, headTerms)

      val edges = body0.foldLeft(Vector.empty[LabelledEdge]) {
        case (edges, body) => body match {
          case Body.Atom(bodyPred, _, p, f, _, _, bodyLoc) =>
            edges :+ LabelledEdge(headPred, p, f, labels, bodyPred, bodyLoc)
          case Body.Functional(_, _, _) => edges
          case Body.Guard(_, _) => edges
        }
      }

      LabelledPrecedenceGraph(edges)
  }

}
