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
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.shared.LabelledPrecedenceGraph.{Label, LabelledEdge}
import ca.uwaterloo.flix.language.ast.shared.{Denotation, LabelledPrecedenceGraph}
import ca.uwaterloo.flix.language.ast.{ChangeSet, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The [[PredDeps]] class computes the [[LabelledPrecedenceGraph]] of the whole program,
  * which represents all static dependencies between datalog predicates.
  *
  * The computed [[LabelledPrecedenceGraph]] is used in [[Stratifier]].
  */
object PredDeps {

  def run(root: Root, oldRoot: Root, changeSet: ChangeSet)(implicit flix: Flix): (Root, List[CompilationMessage]) = flix.phaseNew("PredDeps") {
    // Compute an over-approximation of the dependency graph for all constraints in the program.
    implicit val sctx: SharedContext = SharedContext.mk()

    val defs = changeSet.updateStaleValues(root.defs, oldRoot.defs)(ParOps.parMapValues(_)(visitDef))
    val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait))
    val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances)(ParOps.parMapValueList(_)(visitInstance))

    val g = LabelledPrecedenceGraph(sctx.edges.asScala.toVector)
    (root.copy(defs = defs, traits = traits, instances = instances, precedenceGraph = g), List.empty)
  }

  private def visitDef(defn: Def)(implicit sctx: SharedContext): Def = {
    visitExp(defn.exp)
    defn
  }

  private def visitTrait(trt: Trait)(implicit sctx: SharedContext): Trait = {
    trt.laws.foreach(visitDef)
    trt.sigs.foreach(visitSig)
    trt
  }

  private def visitInstance(inst: Instance)(implicit sctx: SharedContext): Instance = {
    inst.defs.foreach(visitDef)
    inst
  }

  private def visitSig(sig: Sig)(implicit sctx: SharedContext): Unit =
    sig.exp.foreach(visitExp)

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
  private def visitExp(exp0: Expr)(implicit sctx: SharedContext): Unit = exp0 match {
    case Expr.Cst(_, _, _) => ()

    case Expr.Var(_, _, _) => ()

    case Expr.Hole(_, _, _, _) => ()

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Region(_, _) => ()

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case MatchRule(_, g, b) =>
          g.foreach(visitExp)
          visitExp(b)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { case TypeMatchRule(_, _, b) => visitExp(b) }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach{ case RestrictableChooseRule(_, body) => visitExp(body) }

    case Expr.Tag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Tuple(elms, _, _, _) =>
      elms.foreach(visitExp)

    case Expr.RecordSelect(base, _, _, _, _) =>
      visitExp(base)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value)
      visitExp(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.foreach(visitExp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.ArrayLoad(base, index, _, _, _) =>
      visitExp(base)
      visitExp(index)

    case Expr.ArrayLength(base, _, _) =>
      visitExp(base)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      visitExp(base)
      visitExp(index)
      visitExp(elm)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      visitExp(region)
      fields.foreach{ case (_, e) => visitExp(e) }

    case Expr.StructGet(e, _, _, _, _) =>
      visitExp(e)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foreach{ case CatchRule(_, _, e) => visitExp(e) }

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      rules.foreach{ case HandlerRule(_, _, e) => visitExp(e) }

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Do(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.InvokeConstructor(_, args, _, _, _) =>
      args.foreach(visitExp)

    case Expr.InvokeMethod(_, exp, args, _, _, _) =>
      args.foreach(visitExp)

    case Expr.InvokeStaticMethod(_, args, _, _, _) =>
      args.foreach(visitExp)

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) => ()

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, _, _) => ()

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      default.foreach(visitExp)
      rules.foreach{
        case SelectChannelRule(_, exp1, exp2) =>
          visitExp(exp1)
          visitExp(exp2)
      }

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.foreach{
        case ParYieldFragment(_, e, _) => visitExp(e)
      }

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foreach(visitConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) => ()
  }

  /**
    * Returns the labelled graph of the given constraint `c0`.
    */
  private def visitConstraint(c: Constraint)(implicit sctx: SharedContext): Unit = c match {
    case Constraint(_, Predicate.Head.Atom(headPred, den, _, headTpe, _), body0, _) =>
      val (headTerms, _) = termTypesAndDenotation(headTpe)

      // We add all body predicates and the head to the labels of each edge
      val bodyLabels: Vector[Label] = body0.collect {
        case Body.Atom(bodyPred, den, _, _, _, bodyTpe, _) =>
          val (terms, _) = termTypesAndDenotation(bodyTpe)
          Label(bodyPred, den, terms.length, terms)
      }.toVector

      val labels = bodyLabels :+ Label(headPred, den, headTerms.length, headTerms)

      body0.foreach {
        case body => body match {
          case Body.Atom(bodyPred, _, p, f, _, _, bodyLoc) =>
            sctx.edges.add(LabelledEdge(headPred, p, f, labels, bodyPred, bodyLoc))
          case Body.Functional(_, _, _) => ()
          case Body.Guard(_, _) => ()
        }
      }
  }

  /**
   * Companion object for [[SharedContext]]
   */
  private object SharedContext {

    /**
     * Returns a fresh shared context.
     */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
   * A global shared context. Must be thread-safe.
   *
   * @param edges the [[LabelledEdge]]s to build the graph.
   */
  private case class SharedContext(edges: ConcurrentLinkedQueue[LabelledEdge])
}
