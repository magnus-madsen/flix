/*
 * Copyright 2020 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Constraint, Def, Expression, MatchRule, Predicate, Root, SelectChannelRule}

object Indexer {

  /**
    * Returns a reverse index for the given AST `root`.
    */
  def visitRoot(root: Root): Index = root.defs.foldLeft(Index.empty) {
    case (index, (_, def0)) => index ++ visitDef(def0)
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  private def visitDef(def0: Def): Index = visitExp(def0.exp)

  /**
    * Returns a reverse index for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Index = exp0 match {
    case Expression.Unit(_) =>
      Index.of(exp0)

    case Expression.True(_) =>
      Index.of(exp0)

    case Expression.False(_) =>
      Index.of(exp0)

    case Expression.Char(_, _) =>
      Index.of(exp0)

    case Expression.Float32(_, _) =>
      Index.of(exp0)

    case Expression.Float64(_, _) =>
      Index.of(exp0)

    case Expression.Int8(_, _) =>
      Index.of(exp0)

    case Expression.Int16(_, _) =>
      Index.of(exp0)

    case Expression.Int32(_, _) =>
      Index.of(exp0)

    case Expression.Int64(_, _) =>
      Index.of(exp0)

    case Expression.BigInt(_, _) =>
      Index.of(exp0)

    case Expression.Str(_, _) =>
      Index.of(exp0)

    case Expression.Wild(_, _) =>
      Index.of(exp0)

    case Expression.Var(_, _, _) =>
      Index.of(exp0)

    case Expression.Def(_, _, _) =>
      Index.of(exp0)

    case Expression.Hole(_, _, _, _) =>
      Index.of(exp0)

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp) + exp0

    case Expression.Apply(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.LetRec(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) + exp0
      rules.foldLeft(i0) {
        case (index, MatchRule(_, guard, exp)) => index ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Tag(_, _, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Tuple(exps, tpe, eff, loc) =>
      visitExps(exps) + exp0

    case Expression.RecordEmpty(tpe, loc) =>
      Index.of(exp0)

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ArrayLit(exps, _, _, _) =>
      visitExps(exps) + exp0

    case Expression.ArrayNew(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.ArrayLength(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ArrayStore(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.ArraySlice(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0

    case Expression.VectorLit(exps, _, _, _) =>
      visitExps(exps) + exp0

    case Expression.VectorNew(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorLoad(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorStore(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.VectorLength(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.VectorSlice(exp, _, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Deref(exp1, _, _, _) =>
      visitExp(exp1) + exp0

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.Existential(_, exp, _) =>
      visitExp(exp) + exp0

    case Expression.Universal(_, exp, _) =>
      visitExp(exp) + exp0

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) + exp0
      rules.foldLeft(i0) {
        case (index, CatchRule(_, _, exp)) => index ++ visitExp(exp)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args) + exp0

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args) + exp0

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args) + exp0

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.GetStaticField(_, _, _, _) =>
      Index.of(exp0)

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val i0 = default.map(visitExp).getOrElse(Index.empty) + exp0
      rules.foldLeft(i0) {
        case (index, SelectChannelRule(_, _, exp)) => index ++ visitExp(exp)
      }

    case Expression.ProcessSpawn(exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.ProcessPanic(_, _, _, _) =>
      Index.empty

    case Expression.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Index.empty) {
        case (index, c) => index ++ visitConstraint(c)
      }

    case Expression.FixpointCompose(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp) + exp0

    case Expression.FixpointEntails(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) + exp0

    case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + exp0
  }

  /**
    * Returns a reverse index for the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression]): Index =
    exps0.foldLeft(Index.empty) {
      case (index, exp0) => index ++ visitExp(exp0)
    }

  /**
    * Returns a reverse index for the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint): Index = {
    val i = visitHead(c0.head)
    c0.body.foldLeft(i) {
      case (index, b0) => index ++ visitBody(b0)
    }
  }

  /**
    * Returns a reverse index for the given head predicate `h0`.
    */
  private def visitHead(h0: Predicate.Head): Index = h0 match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
    case Head.Union(exp, _, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given body predicate `b0`.
    */
  private def visitBody(b0: Predicate.Body): Index = b0 match {
    case Body.Atom(_, _, _, _, _, _) => Index.empty
    case Body.Guard(exp, _) => visitExp(exp)
  }

}
