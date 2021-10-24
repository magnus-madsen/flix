/*
 * Copyright 2021 Jacob Harris Cryer Kragh
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Constraint, Def, Expression, MatchRule, Pattern, Root, SelectChannelRule}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {

  /**
    * Processes a request for (full) semantic tokens.
    */
  def provideSemanticTokens(uri: String)(implicit index: Index, root: Root): JObject = {
    val entities = index.query(uri)
    val semanticTokens = root.defs.filter(_._1.loc.source.name == uri).flatMap {
      case (_, defn) => visitDef(defn)
    }.toList
    val encoding = encodeSemanticTokens(semanticTokens)
    val result = ("data" -> encoding)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def visitDef(defn0: Def): Iterator[SemanticToken] = {
    visitExp(defn0.impl.exp)
  }

  /**
    * Returns all semantic tokens in the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Iterator[SemanticToken] = exp0 match {
    case Expression.Wild(_, _) => Iterator.empty

    case Expression.Var(_, _, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
      Iterator(t)

    case Expression.Def(_, _, loc) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, loc)
      Iterator(t)

    case Expression.Sig(_, _, loc) =>
      val t = SemanticToken(SemanticTokenType.Method, Nil, loc)
      Iterator(t)

    case Expression.Hole(sym, tpe, loc) => Iterator.empty

    case Expression.Unit(loc) => Iterator.empty

    case Expression.Null(tpe, loc) => Iterator.empty

    case Expression.True(loc) => Iterator.empty

    case Expression.False(loc) => Iterator.empty

    case Expression.Char(lit, loc) => Iterator.empty

    case Expression.Float32(lit, loc) => Iterator.empty

    case Expression.Float64(lit, loc) => Iterator.empty

    case Expression.Int8(lit, loc) => Iterator.empty

    case Expression.Int16(lit, loc) => Iterator.empty

    case Expression.Int32(lit, loc) => Iterator.empty

    case Expression.Int64(lit, loc) => Iterator.empty

    case Expression.BigInt(lit, loc) => Iterator.empty

    case Expression.Str(lit, loc) => Iterator.empty

    case Expression.Default(tpe, loc) => Iterator.empty

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val env1 = Map(fparam.sym -> fparam.tpe)
      visitExp(exp)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(matchExp, rules, tpe, eff, loc) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (macc, MatchRule(pat, guard, exp)) =>
          macc ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      Iterator.empty // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      elms.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, elm) => macc ++ visitExp(elm)
      }

    case Expression.RecordEmpty(tpe, loc) => Iterator.empty

    case Expression.RecordSelect(base, field, tpe, eff, loc) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, loc)
      Iterator(t) ++ visitExp(base)

    case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
      visitExp(rest) ++ visitExp(value)

    case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, elm) => macc ++ visitExp(elm)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      visitExp(elm)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayStore(base, index, elm, loc) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArrayLength(base, eff, loc) =>
      visitExp(base)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)

    case Expression.Ref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Deref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Existential(fparam, exp, loc) =>
      visitExp(exp)

    case Expression.Universal(fparam, exp, loc) =>
      visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp)) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Iterator.empty

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan) ++ visitExp(exp)
      }

      val d = default.map(visitExp).getOrElse(Iterator.empty)

      rs ++ d

    case Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Lazy(exp, tpe, loc) => visitExp(exp)

    case Expression.Force(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      cs.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, c) => macc ++ visitConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) => Iterator.empty

    case Expression.ReifyType(_, _, _, _, _) => Iterator.empty

  }

  /**
    * Returns all semantic tokens in the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern): Iterator[SemanticToken] = pat0 match {
    case Pattern.Wild(_, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
      Iterator(t)

    case Pattern.Var(_, _, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
      Iterator(t)

    case Pattern.Unit(loc) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      Iterator(t)

    case Pattern.True(_) => Iterator.empty

    case Pattern.False(_) => Iterator.empty

    case Pattern.Char(_, _) => Iterator.empty

    case Pattern.Float32(_, _) => Iterator.empty

    case Pattern.Float64(_, _) => Iterator.empty

    case Pattern.Int8(_, _) => Iterator.empty

    case Pattern.Int16(_, _) => Iterator.empty

    case Pattern.Int32(_, _) => Iterator.empty

    case Pattern.Int64(_, _) => Iterator.empty

    case Pattern.BigInt(_, _) => Iterator.empty

    case Pattern.Str(_, _) => Iterator.empty

    case Pattern.Tag(sym, tag, pat, tpe, loc) => ??? // TODO

    case Pattern.Tuple(elms, _, _) => elms.flatMap(visitPat).iterator

    case Pattern.Array(elms, _, _) => elms.flatMap(visitPat).iterator

    case Pattern.ArrayTailSpread(elms, sym, tpe, loc) => ???

    case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) => ???
  }

  private def visitConstraint(c: Constraint): Iterator[SemanticToken] = Iterator.empty // TODO

  // Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
  private def encodeSemanticTokens(tokens: Iterable[SemanticToken]): List[Int] = {
    val encoding = new ArrayBuffer[Int](initialSize = 5 * tokens.size)

    var prevLine = 0
    var prevCol = 0

    implicit val tokenOrdering: Ordering[SemanticToken] = Ordering.by(_.loc)
    for (token <- SortedSet.empty.concat(tokens)) {
      var relLine = token.loc.beginLine - 1
      var relCol = token.loc.beginCol - 1

      if (encoding.nonEmpty) {
        relLine -= prevLine
        if (relLine == 0) {
          relCol -= prevCol
        }
      }

      encoding += relLine
      encoding += relCol
      encoding += token.loc.endCol - token.loc.beginCol
      encoding += token.tpe.toInt
      encoding += encodeModifiers(token.mod)

      prevLine = token.loc.beginLine - 1
      prevCol = token.loc.beginCol - 1
    }

    encoding.toList
  }

  /**
    * Encodes a list of modifiers as a bitset (as per the LSP spec).
    */
  def encodeModifiers(modifiers: List[SemanticTokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.toInt))
}
