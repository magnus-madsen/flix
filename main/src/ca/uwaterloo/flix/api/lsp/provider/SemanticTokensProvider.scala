/*
 * Copyright 2021 Jacob Harris Cryer Kragh, Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.collection.IteratorOps
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {

  /**
    * Processes a request for (full) semantic tokens.
    */
  def provideSemanticTokens(uri: String)(implicit root: Root): JObject = {
    //
    // This class uses iterators over lists to ensure fast append (!)
    //

    //
    // Construct an iterator of the semantic tokens from traits.
    //
    val traitTokens = root.traits.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitTrait(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from instances.
    //
    val instanceTokens = root.instances.values.flatMap {
      case instances => instances.flatMap {
        case instance if include(uri, instance.trt.loc) => visitInstance(instance)
        case _ => Nil
      }
    }

    //
    // Construct an iterator of the semantic tokens from defs.
    //
    val defnTokens = root.defs.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitDef(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from enums.
    //
    val enumTokens = root.enums.values.flatMap {
      case decl if include(uri, decl.loc) => visitEnum(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from structs.
    //
    val structTokens = root.structs.values.flatMap {
      case decl if include(uri, decl.loc) => visitStruct(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from type aliases.
    //
    val typeAliasTokens = root.typeAliases.flatMap {
      case (_, decl) if include(uri, decl.loc) => visitTypeAlias(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from effects.
    //
    val effectTokens = root.effects.flatMap {
      case (_, decl) if include(uri, decl.loc) => visitEffect(decl)
      case _ => Nil
    }

    //
    // Collect all tokens into one list.
    //
    val allTokens = (traitTokens ++ instanceTokens ++ defnTokens ++ enumTokens ++ structTokens ++ typeAliasTokens ++ effectTokens).toList

    //
    // We keep all tokens that are: (i) single-line tokens, (ii) have the same source as `uri`, and (iii) come from real source locations.
    //
    // Note that the last criteria (automatically) excludes:
    //   (a) tokens with unknown source locations,
    //   (b) tokens that come from entities inside `uri` but that originate from different uris, and
    //   (c) tokens that come from synthetic (generated) source code.
    //
    val filteredTokens = allTokens.filter(t => t.loc.isSingleLine && include(uri, t.loc) && !t.loc.isSynthetic)

    //
    // Encode the semantic tokens as a list of integers.
    //
    val encodedTokens = encodeSemanticTokens(filteredTokens)

    //
    // Construct the JSON result.
    //
    ("result" -> ("data" -> encodedTokens))
  }

  /**
    * Returns `true` if the given source location `loc` is associated with the given `uri`.
    */
  private def include(uri: String, loc: SourceLocation): Boolean = loc.source.name == uri

  /**
    * Returns all semantic tokens in the given trait `traitDecl`.
    */
  private def visitTrait(traitDecl: TypedAst.Trait): Iterator[SemanticToken] = traitDecl match {
    case TypedAst.Trait(_, _, _, sym, tparam, superTraits, assocs, signatures, laws, _) =>
      val t = SemanticToken(SemanticTokenType.Interface, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        superTraits.flatMap(visitTraitConstraint),
        assocs.flatMap(visitAssocTypeSig),
        visitTypeParam(tparam),
        signatures.flatMap(visitSig),
        laws.flatMap(visitDef),
      )
  }

  /**
    * Returns all semantic tokens in the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance): Iterator[SemanticToken] = inst0 match {
    case TypedAst.Instance(_, _, _, sym, tpe, tconstrs, assocs, defs, _, _) =>
      // NB: we use SemanticTokenType.Class because the OOP "Class" most directly corresponds to the FP "Instance"
      val t = SemanticToken(SemanticTokenType.Class, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitType(tpe),
        assocs.flatMap(visitAssocTypeDef),
        tconstrs.flatMap(visitTraitConstraint),
        defs.flatMap(visitDef),
      )
  }

  /**
    * Returns all semantic tokens in the given enum `enum0`.
    *
    * Returns tokens for the symbol, the type parameters, the derivations, and the cases.
    */
  private def visitEnum(enum0: TypedAst.Enum): Iterator[SemanticToken] = enum0 match {
    case TypedAst.Enum(_, _, _, sym, tparams, derives, cases, _) =>
      val t = SemanticToken(SemanticTokenType.Enum, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitTypeParams(tparams),
        Iterator(derives.traits *).map {
          case Derivation(_, loc) => SemanticToken(SemanticTokenType.Class, Nil, loc)
        },
        cases.foldLeft(Iterator.empty[SemanticToken]) {
          case (acc, (_, caze)) => acc ++ visitCase(caze)
        },
      )
  }

  /**
    * Returns all semantic tokens in the given case `case0`.
    */
  private def visitCase(case0: TypedAst.Case): Iterator[SemanticToken] = case0 match {
    case TypedAst.Case(sym, tpes, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, sym.loc)
      tpes.foldLeft(Iterator(t)) {
        case (acc, tpe) => acc ++ visitType(tpe)
      }
  }

  /**
    * Returns all semantic tokens in the given struct `struct0`.
    *
    * Returns tokens for the symbol, the type parameters, and the fields.
    */
  private def visitStruct(struct0: TypedAst.Struct): Iterator[SemanticToken] = struct0 match {
    case TypedAst.Struct(_, _, _, sym, tparams, _, fields, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitTypeParams(tparams),
        fields.foldLeft(Iterator.empty[SemanticToken]) {
          case (acc, (_, field)) => acc ++ visitField(field)
        }
      )
  }

  /**
    * Returns all semantic tokens in the given field `field0`
    */
  private def visitField(field0: StructField): Iterator[SemanticToken] = field0 match {
    case StructField(sym, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, sym.loc)
      Iterator(t) ++ visitType(tpe)
  }

  /**
    * Returns all semantic tokens in the given definition `defn0`.
    */
  private def visitDef(defn0: TypedAst.Def): Iterator[SemanticToken] = defn0 match {
    case Def(sym, spec, exp, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitSpec(spec),
        visitExp(exp),
      )
  }

  /**
    * Returns all semantic tokens in the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig): Iterator[SemanticToken] = sig0 match {
    case TypedAst.Sig(sym, spec, exp, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitSpec(spec),
        exp.iterator.flatMap(visitExp),
      )
  }

  /**
    * Returns all semantic tokens in the given `spec`.
    */
  private def visitSpec(spec: Spec): Iterator[SemanticToken] = spec match {
    case Spec(_, _, _, tparams, fparams, _, retTpe, eff, tconstrs, econstrs) =>
      IteratorOps.all(
        visitTypeParams(tparams),
        visitFormalParams(fparams),
        tconstrs.iterator.flatMap(visitTraitConstraint),
        econstrs.iterator.flatMap(visitEqualityConstraint),
        visitType(retTpe),
        visitType(eff),
      )
  }

  /**
    * Returns all semantic tokens in the given type alias `typeAlias0`.
    */
  private def visitTypeAlias(typeAlias0: TypedAst.TypeAlias): Iterator[SemanticToken] = typeAlias0 match {
    case TypedAst.TypeAlias(_, _, _, sym, tparams, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitTypeParams(tparams),
        visitType(tpe),
      )
  }

  /**
    * Returns all semantic tokens in the given associated type signature `assoc`.
    */
  private def visitAssocTypeSig(assoc: TypedAst.AssocTypeSig): Iterator[SemanticToken] = assoc match {
    case TypedAst.AssocTypeSig(_, _, sym, tparam, _, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitTypeParam(tparam),
        tpe.iterator.flatMap(visitType)
      )
  }

  /**
    * Returns all semantic tokens in the given associated type definition `assoc`.
    */
  private def visitAssocTypeDef(assoc: TypedAst.AssocTypeDef): Iterator[SemanticToken] = assoc match {
    case TypedAst.AssocTypeDef(_, _, sym, arg, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitType(arg),
        visitType(tpe),
      )
  }

  /**
    * Returns all semantic tokens in the given effect.
    */
  private def visitEffect(effect: TypedAst.Effect): Iterator[SemanticToken] = effect match {
    case TypedAst.Effect(_, _, _, sym, ops, _) =>
      val t = SemanticToken(SemanticTokenType.Interface, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        ops.flatMap(visitOp),
      )
  }

  /**
    * Returns all semantic tokens in the given effect operation.
    */
  private def visitOp(op: TypedAst.Op): Iterator[SemanticToken] = op match {
    case TypedAst.Op(sym, spec, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitSpec(spec),
      )
  }

  /**
    * Returns all semantic tokens in the given expression `exp0`.
    */
  private def visitExp(exp0: Expr): Iterator[SemanticToken] = exp0 match {
    case Expr.Var(sym, tpe, loc) =>
      val o = getSemanticTokenType(sym, tpe)
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expr.Hole(_, _, _, _) => Iterator.empty

    case Expr.HoleWithExp(exp, _, _, _) => visitExp(exp)

    case Expr.OpenAs(RestrictableEnumSymUse(_, loc), exp, _, _) =>
      val t = SemanticToken(SemanticTokenType.Enum, Nil, loc)
      Iterator(t) ++ visitExp(exp)

    case Expr.Use(_, _, exp, _) => visitExp(exp) // TODO NS-REFACTOR add token for sym

    case Expr.Cst(_, _, _) => Iterator.empty

    case Expr.Lambda(fparam, exp, _, _) =>
      visitFormalParam(fparam) ++ visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ApplyDef(DefSymUse(sym, loc), exps, _, _, _, _) =>
      val o = if (isOperatorName(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Function
      val t = SemanticToken(o, Nil, loc)
      exps.foldLeft(Iterator(t)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }
    case Expr.ApplyLocalDef(LocalDefSymUse(_, loc), exps, _, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, loc)
      exps.foldLeft(Iterator(t)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expr.ApplySig(SigSymUse(sym, loc), exps, _, _, _, _) =>
      val o = if (isOperatorName(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Method
      val t = SemanticToken(o, Nil, loc)
      exps.foldLeft(Iterator(t)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(bnd, exp1, exp2, _, _, _) =>
      val o = getSemanticTokenType(bnd.sym, exp1.tpe)
      val t = SemanticToken(o, Nil, bnd.sym.loc)
      Iterator(t) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expr.LocalDef(TypedAst.Binder(sym, _), fparams, exp1, exp2, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      IteratorOps.all(
        Iterator(t),
        visitFormalParams(fparams),
        visitExp(exp1),
        visitExp(exp2)
      )

    case Expr.Region(_, _) =>
      Iterator.empty

    case Expr.Scope(Binder(sym, _), _, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) => visitExp(exp)

    case Expr.Match(matchExp, rules, _, _, _) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (acc, MatchRule(pat, guard, exp)) =>
          acc ++ visitPat(pat) ++ guard.toList.flatMap(visitExp) ++ visitExp(exp)
      }

    case Expr.TypeMatch(matchExp, rules, _, _, _) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (acc, TypeMatchRule(bnd, tpe, exp)) =>
          val o = getSemanticTokenType(bnd.sym, tpe)
          val t = SemanticToken(o, Nil, bnd.sym.loc)
          acc ++ Iterator(t) ++ visitType(tpe) ++ visitExp(exp)
      }

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      val c = visitExp(exp)
      rules.foldLeft(c) {
        case (acc, RestrictableChooseRule(pat, exp)) =>
          acc ++ visitRestrictableChoosePat(pat) ++ visitExp(exp)
      }

    case Expr.Tag(CaseSymUse(_, loc), exps, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      exps.foldLeft(Iterator(t)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expr.RestrictableTag(RestrictableCaseSymUse(_, loc), exps, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      exps.foldLeft(Iterator(t)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expr.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expr.RecordEmpty(_, _) => Iterator.empty

    case Expr.RecordSelect(exp, label, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, label.loc)
      Iterator(t) ++ visitExp(exp)

    case Expr.RecordExtend(label, exp1, exp2, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, label.loc)
      Iterator(t) ++ visitExp(exp2) ++ visitExp(exp1)

    case Expr.RecordRestrict(label, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, label.loc)
      Iterator(t) ++ visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.StructNew(sym, fields, region, _, _, _) =>
      val (names, exps) = fields.unzip
      val ts = names.map(name => SemanticToken(SemanticTokenType.Property, Nil, name.loc))
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      visitExps(exps) ++ visitExp(region) ++ ts ++ Iterator(t)

    case Expr.StructGet(exp, field, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, field.loc)
      visitExp(exp) ++ Iterator(t)

    case Expr.StructPut(exp1, field, exp2, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, field.loc)
      visitExp(exp1) ++ visitExp(exp2) ++ Iterator(t)

    case Expr.VectorLit(exps, _, _, _) =>
      visitExps(exps)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, tpe, _, _) =>
      visitExp(exp) ++ visitType(tpe)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, _, _, _) =>
      visitExp(exp) ++ declaredType.map(visitType).getOrElse(Iterator())  ++ declaredEff.map(visitType).getOrElse(Iterator())

    case Expr.Unsafe(exp, runEff, _, _, _) =>
      visitType(runEff) ++ visitExp(exp)

    case Expr.Without(exp, eff, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, eff.qname.loc)
      Iterator(t) ++ visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(bnd, _, exp)) =>
          val t = SemanticToken(SemanticTokenType.Variable, Nil, bnd.sym.loc)
          acc ++ Iterator(t) ++ visitExp(exp)
      }

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.TryWith(exp, eff, rules, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, eff.qname.loc)
      val st1 = Iterator(t)
      val st2 = rules.foldLeft(visitExp(exp)) {
        case (acc, HandlerRule(op, fparams, exp)) =>
          val st = SemanticToken(SemanticTokenType.Type, Nil, op.qname.loc)
          val t1 = Iterator(st)
          val t2 = visitFormalParams(fparams)
          acc ++ t1 ++ t2 ++ visitExp(exp)
      }
      st1 ++ st2

    case Expr.Do(op, exps, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, op.qname.loc)
      Iterator(t) ++ visitExps(exps)

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      exps.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      exps.foldLeft(visitExp(exp)) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      exps.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expr.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      Iterator.empty

    case Expr.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, m) => acc ++ visitJvmMethod(m)
      }

    case Expr.NewChannel(exp, _, _, _) => visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) => visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      val rs = rules.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, SelectChannelRule(Binder(sym, _), chan, exp)) =>
          val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
          acc ++ Iterator(t) ++ visitExp(chan) ++ visitExp(exp)
      }
      val d = default.map(visitExp).getOrElse(Iterator.empty)
      rs ++ d

    case Expr.Spawn(exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      val e0 = visitExp(exp)
      frags.foldLeft(e0) {
        case (acc, ParYieldFragment(p, e, _)) =>
          acc ++ visitPat(p) ++ visitExp(e)
      }

    case Expr.Lazy(exp, _, _) => visitExp(exp)

    case Expr.Force(exp, _, _, _) => visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, c) => acc ++ visitConstraint(c)
      }

    case Expr.FixpointLambda(pparams, exp, _, _, _) =>
      visitPredicateParams(pparams) ++ visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      Iterator.empty

  }

  /**
    * Returns all semantic tokens in the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expr]): Iterator[SemanticToken] =
    exps0.flatMap(visitExp).iterator

  /**
    * Returns all semantic tokens in the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern): Iterator[SemanticToken] = pat0 match {
    case Pattern.Wild(_, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
      Iterator(t)

    case Pattern.Var(Binder(sym, _), tpe, loc) =>
      val o = getSemanticTokenType(sym, tpe)
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Pattern.Cst(_, _, _) => Iterator.empty

    case Pattern.Tag(CaseSymUse(_, loc), pats, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      pats.foldLeft(Iterator(t)) {
        case (acc, pat) => acc ++ visitPat(pat)
      }

    case Pattern.Tuple(pats, _, _) => pats.flatMap(visitPat).iterator

    case Pattern.Record(pats, pat, tpe, _) =>
      val patsVal = pats.flatMap {
        case Pattern.Record.RecordLabelPattern(_, pat1, tpe1, loc1) =>
          val f = SemanticToken(SemanticTokenType.Property, Nil, loc1)
          Iterator(f) ++ visitType(tpe1) ++ visitPat(pat1)
      }.iterator
      val patVal = visitPat(pat)
      val tVal = visitType(tpe)
      patsVal ++ patVal ++ tVal

    case Pattern.RecordEmpty(_, _) => Iterator.empty

    case Pattern.Error(_, _) => Iterator.empty
  }

  /**
    * Returns all semantic tokens in the given pattern `pat0`.
    */
  private def visitRestrictableChoosePat(pat0: RestrictableChoosePattern): Iterator[SemanticToken] = pat0 match {
    case RestrictableChoosePattern.Tag(RestrictableCaseSymUse(_, tagLoc), pat1, _, _) =>
      val t1 = SemanticToken(SemanticTokenType.EnumMember, Nil, tagLoc)
      val ts = pat1.iterator.flatMap {
        case RestrictableChoosePattern.Wild(_, loc) => Iterator(SemanticToken(SemanticTokenType.Variable, Nil, loc))
        case RestrictableChoosePattern.Var(_, _, loc) => Iterator(SemanticToken(SemanticTokenType.Variable, Nil, loc))
        case RestrictableChoosePattern.Error(_, _) => Iterator.empty
      }
      Iterator(t1) ++ ts
    case RestrictableChoosePattern.Error(_, _) => Iterator.empty
  }

  /**
    * Returns all semantic tokens in the given type `tpe0`.
    */
  private def visitType(tpe0: Type): Iterator[SemanticToken] = tpe0 match {
    case Type.Var(_, loc) =>
      val t = SemanticToken(SemanticTokenType.TypeParameter, Nil, loc)
      Iterator(t)

    case Type.Cst(cst, loc) =>
      if (isVisibleTypeConstructor(cst)) {
        val t = SemanticToken(SemanticTokenType.Type, Nil, loc)
        Iterator(t)
      } else {
        Iterator.empty
      }

    case Type.Apply(tpe1, tpe2, _) =>
      visitType(tpe1) ++ visitType(tpe2)

    case Type.Alias(cst, args, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, cst.loc)
      Iterator(t) ++ args.flatMap(visitType).iterator

    case Type.AssocType(cst, arg, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, cst.loc)
      Iterator(t) ++ visitType(arg)

    // Jvm types should not be exposed to the user.
    case _: Type.JvmToType => Iterator.empty
    case _: Type.JvmToEff => Iterator.empty
    case _: Type.UnresolvedJvmType => Iterator.empty
  }

  /**
    * Returns true if the type constructor should be highlighted.
    * This is restricted to type constructors whose that use the standard shape (X[Y, Z]).
    */
  private def isVisibleTypeConstructor(tycon: TypeConstructor): Boolean = tycon match {
    // visible
    case TypeConstructor.Void => true
    case TypeConstructor.Unit => true
    case TypeConstructor.Null => true
    case TypeConstructor.Bool => true
    case TypeConstructor.Char => true
    case TypeConstructor.Float32 => true
    case TypeConstructor.Float64 => true
    case TypeConstructor.BigDecimal => true
    case TypeConstructor.Int8 => true
    case TypeConstructor.Int16 => true
    case TypeConstructor.Int32 => true
    case TypeConstructor.Int64 => true
    case TypeConstructor.BigInt => true
    case TypeConstructor.Str => true
    case TypeConstructor.Regex => true
    case TypeConstructor.Sender => true
    case TypeConstructor.Receiver => true
    case TypeConstructor.Lazy => true
    case TypeConstructor.Enum(_, _) => true
    case TypeConstructor.Struct(_, _) => true
    case TypeConstructor.RestrictableEnum(_, _) => true
    case TypeConstructor.Native(_) => true
    case TypeConstructor.Array => true
    case TypeConstructor.Vector => true
    case TypeConstructor.Pure => true
    case TypeConstructor.Univ => true
    case TypeConstructor.True => true
    case TypeConstructor.False => true
    case TypeConstructor.Effect(_) => true
    case TypeConstructor.RegionToStar => true

    // invisible
    case TypeConstructor.AnyType => false
    case TypeConstructor.Arrow(_) => false
    case TypeConstructor.RecordRowEmpty => false
    case TypeConstructor.RecordRowExtend(_) => false
    case TypeConstructor.Record => false
    case TypeConstructor.SchemaRowEmpty => false
    case TypeConstructor.SchemaRowExtend(_) => false
    case TypeConstructor.Schema => false
    case TypeConstructor.Tuple(_) => false
    case TypeConstructor.Relation => false
    case TypeConstructor.Lattice => false
    case TypeConstructor.Not => false
    case TypeConstructor.And => false
    case TypeConstructor.Or => false
    case TypeConstructor.Complement => false
    case TypeConstructor.Union => false
    case TypeConstructor.Intersection => false
    case TypeConstructor.Difference => false
    case TypeConstructor.SymmetricDiff => false
    case TypeConstructor.CaseComplement(_) => false
    case TypeConstructor.CaseUnion(_) => false
    case TypeConstructor.CaseIntersection(_) => false
    case TypeConstructor.CaseSet(_, _) => false
    case TypeConstructor.JvmField(_) => false
    case TypeConstructor.JvmConstructor(_) => false
    case TypeConstructor.JvmMethod(_) => false
    case TypeConstructor.Error(_, _) => false

    // backend constructors
    case TypeConstructor.ArrowWithoutEffect(_) => false
    case TypeConstructor.ArrayWithoutRegion => false
    case TypeConstructor.RegionWithoutRegion => false
  }

  /**
    * Returns all semantic tokens in the given type constraint `tc0`.
    */
  private def visitTraitConstraint(tc0: TraitConstraint): Iterator[SemanticToken] = tc0 match {
    case TraitConstraint(head, arg, _) =>
      visitTraitConstraintHead(head) ++ visitType(arg)
  }

  /**
    * Returns all semantic tokens in the given type constraint head `head0`.
    */
  private def visitTraitConstraintHead(head0: TraitConstraint.Head): Iterator[SemanticToken] = head0 match {
    case TraitConstraint.Head(_, loc) =>
      val o = SemanticTokenType.Class
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)
  }

  /**
    * Returns all semantic tokens in the given equality constraint `tc0`.
    */
  private def visitEqualityConstraint(ec0: EqualityConstraint): Iterator[SemanticToken] = ec0 match {
    case EqualityConstraint(cst, tpe1, tpe2, _) =>
      visitAssocTypeConstructor(cst) ++ visitType(tpe1) ++ visitType(tpe2)
  }

  /**
    * Returns all semantic tokens in the given associated type constructor `cst`.
    */
  private def visitAssocTypeConstructor(cst: AssocTypeConstructor): Iterator[SemanticToken] = cst match {
    case AssocTypeConstructor(_, loc) =>
      val o = SemanticTokenType.Type
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)
  }

  /**
    * Returns all semantic tokens in the given formal parameters `fparams0`.
    */
  private def visitFormalParams(fparams0: List[FormalParam]): Iterator[SemanticToken] =
    fparams0.foldLeft(Iterator.empty[SemanticToken]) {
      case (acc, fparam0) => acc ++ visitFormalParam(fparam0)
    }

  /**
    * Returns all semantic tokens in the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam): Iterator[SemanticToken] = fparam0 match {
    case FormalParam(bnd, _, tpe, _, _) =>
      val o = getSemanticTokenType(bnd.sym, tpe)
      val t = SemanticToken(o, Nil, bnd.sym.loc)
      Iterator(t) ++ visitType(tpe)
  }

  /**
    * Returns all semantic tokens in the given predicate parameters `pparams0`.
    */
  private def visitPredicateParams(pparams0: List[PredicateParam]): Iterator[SemanticToken] =
    pparams0.foldLeft(Iterator.empty[SemanticToken]) {
      case (acc, fparam0) => acc ++ visitPredicateParam(fparam0)
    }

  /**
    * Returns all semantic tokens in the given predicate parameter `pparam0`.
    */
  private def visitPredicateParam(pparam0: PredicateParam): Iterator[SemanticToken] = pparam0 match {
    case PredicateParam(pred, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, pred.loc)
      Iterator(t) ++ visitType(tpe)
  }

  /**
    * Returns all semantic tokens in the given type parameter `tparam0`.
    */
  private def visitTypeParams(tparams0: List[TypedAst.TypeParam]): Iterator[SemanticToken] =
    tparams0.foldLeft(Iterator.empty[SemanticToken]) {
      case (acc, tparam0) => acc ++ visitTypeParam(tparam0)
    }

  /**
    * Returns all semantic tokens in the given type parameter `tparam0`.
    */
  private def visitTypeParam(tparam0: TypedAst.TypeParam): Iterator[SemanticToken] = tparam0 match {
    case TypeParam(_, sym, _) =>
      val t = SemanticToken(SemanticTokenType.TypeParameter, Nil, sym.loc)
      Iterator(t)
  }

  /**
    * Returns all semantic tokens in the given constraint `constraint0`.
    */
  private def visitConstraint(constraint0: Constraint): Iterator[SemanticToken] = constraint0 match {
    case Constraint(_, head, body, _) =>
      visitHeadPredicate(head) ++ body.flatMap(visitBodyPredicate)
  }

  /**
    * Returns all semantic tokens in the given head predicate `h0`.
    */
  private def visitHeadPredicate(h0: TypedAst.Predicate.Head): Iterator[SemanticToken] = h0 match {
    case Head.Atom(pred, _, terms, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, pred.loc)
      Iterator(t) ++ terms.flatMap(visitExp).iterator
  }

  /**
    * Returns all semantic tokens in the given body predicate `b0`.
    */
  private def visitBodyPredicate(b0: TypedAst.Predicate.Body): Iterator[SemanticToken] = b0 match {
    case Body.Atom(pred, _, _, _, terms, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, pred.loc)
      Iterator(t) ++ terms.flatMap(visitPat).iterator

    case Body.Functional(outBnds, exp, _) =>
      val ts = outBnds.map(bnd => SemanticToken(SemanticTokenType.Variable, Nil, bnd.sym.loc))
      visitExp(exp) ++ ts

    case Body.Guard(exp, _) =>
      visitExp(exp)
  }

  /**
    * Returns all semantic tokens in the given JvmMethod `method`
    */
  private def visitJvmMethod(method: TypedAst.JvmMethod): Iterator[SemanticToken] = method match {
    case TypedAst.JvmMethod(_, fparams, exp, tpe, eff, _) =>
      visitFormalParams(fparams) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff)
  }

  /**
    * Returns the semantic token type associated with the given variable `sym` of the given type `tpe`.
    */
  private def getSemanticTokenType(sym: Symbol.VarSym, tpe: Type): SemanticTokenType = {
    if (boundByFormalParam(sym))
      SemanticTokenType.Parameter
    else if (isOperatorName(sym.text))
      SemanticTokenType.Operator
    else if (isFunctionType(tpe))
      SemanticTokenType.Function
    else
      SemanticTokenType.Variable
  }

  /**
    * Returns `true` if the given string `s` contains non-letter symbols.
    */
  private def isOperatorName(s: String): Boolean = s.forall(c => !Character.isLetter(c))

  /**
    * Returns `true` if the given type `tpe` is a function type.
    */
  private def isFunctionType(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => true
    case _ => false
  }

  /**
    * Returns `true` if the given symbol `sym` is bound as a formal parameter.
    */
  private def boundByFormalParam(sym: Symbol.VarSym): Boolean = sym.boundBy match {
    case BoundBy.FormalParam => true
    case _ => false
  }

  /**
    * Returns the given `tokens` as an encoded list of integers.
    *
    * Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
    */
  private def encodeSemanticTokens(tokens: List[SemanticToken]): List[Int] = {
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
  private def encodeModifiers(modifiers: List[SemanticTokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.toInt))
}
