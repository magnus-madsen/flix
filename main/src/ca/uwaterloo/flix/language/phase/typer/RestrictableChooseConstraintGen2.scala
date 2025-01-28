/*
 * Copyright 2023 Matthew Lutze, Jonathan Lindegaard Starup
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.RestrictableChoosePattern
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.shared.SymUse.RestrictableEnumSymUse
import ca.uwaterloo.flix.language.ast.{Kind, KindedAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.ConstraintGen2.visitExp
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable.SortedSet

object RestrictableChooseConstraintGen2 {

  /**
    * Returns the domain of the list of rules,
    * i.e., the set of tags in the patterns.
    *
    * For example, if the rules are
    *
    * {{{
    *     match x {
    *         case Expr.Var(y) => Expr.Cst(22)
    *         case Expr.Xor(y, z) => Expr.Xor(y, z)
    *         case Expr.Cst(y)    => Expr.Cst(y)
    *         case Expr.And(y, z) => Expr.Or(x, y)
    *     }
    * }}}
    *
    * then it returns { Expr.Var, Expr.Xor, Expr.Cst, Expr.And }.
    */
  private def dom(rules: List[KindedAst.RestrictableChooseRule]): Set[Symbol.RestrictableCaseSym] = {
    rules.flatMap {
      case KindedAst.RestrictableChooseRule(KindedAst.RestrictableChoosePattern.Tag(symUse, _, _, _), _) => List(symUse.sym)
      case KindedAst.RestrictableChooseRule(KindedAst.RestrictableChoosePattern.Error(_, _), _) => List.empty
    }.toSet
  }

  /**
    * Converts the list of restrictable case symbols to a closed set type.
    */
  private def toType(syms: Set[Symbol.RestrictableCaseSym], enumSym: Symbol.RestrictableEnumSym, loc: SourceLocation): Type = {
    Type.Cst(TypeConstructor.CaseSet(syms.to(SortedSet), enumSym), loc)
  }

  /**
    * Unifies t1 and t2 where t1 is a subset of t2.
    */
  private def unifySubset(t1: Type, t2: Type, sym: Symbol.RestrictableEnumSym, loc: SourceLocation)(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): Unit = {
    val diff = Type.mkCaseDifference(t1, t2, sym, loc)
    // t1 <: t2 <=> t1 - t2 ≡ ∅
    c.unifyType(diff, Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), loc), loc)
  }

  /**
    * Performs type inference on the given restrictable choose expression.
    */
  def visitRestrictableChoose(exp: KindedAst.Expr.RestrictableChoose)(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope

    exp match {
      case KindedAst.Expr.RestrictableChoose(false, exp0, rules0, tpe0, loc) =>

        // Get the enum symbols for the matched type
        rules0.headOption match {
          case None => throw InternalCompilerException("unexpected empty choose", loc)
          case Some(rule) => rule.pat match {
            case RestrictableChoosePattern.Error(_, loc) => // If the pattern is an error we just return fresh error variables
              (Type.freshError(Kind.Star, loc), Type.freshError(Kind.Eff, loc))

            case RestrictableChoosePattern.Tag(symUse, _, _, _) => // If the pattern is Tag then we emit constraint rules
              val enumSym = symUse.sym.enumSym
              val enum0 = root.restrictableEnums(enumSym)
              val universe = enum0.cases.keys.toSet
              val (enumType, indexVar, _) = instantiatedEnumType(enumSym, enum0, loc.asSynthetic)
              val domSet = dom(rules0)
              val domM = toType(domSet, enumSym, loc.asSynthetic)

              // Γ ⊢ e: τ_in
              val (tpe, eff) = visitExp(exp0)
              val patTpes = visitRestrictableChoosePatterns(rules0.map(_.pat))
              c.unifyAllTypes(tpe :: patTpes, loc)

              // τ_in = (... + l_i(τ_i) + ...)[φ_in]
              c.unifyType(enumType, tpe, loc)

              // φ_in <: dom(M)
              if (domSet != universe) {
                unifySubset(indexVar, domM, enumSym, loc.asSynthetic)
              }

              // Γ, x_i: τ_i ⊢ e_i: τ_out
              val (tpes, effs) = rules0.map(rule => visitExp(rule.exp)).unzip

              // τ_out
              c.unifyAllTypes(tpe0 :: tpes, loc)
              val resTpe = tpe0
              val resEff = Type.mkUnion(eff :: effs, loc)
              (resTpe, resEff)

          }
        }

      case KindedAst.Expr.RestrictableChoose(true, exp0, rules0, tpe0, loc) =>

        // Get the enum symbols for the matched type
        rules0.headOption match {
          case None => throw InternalCompilerException("unexpected empty choose", loc)
          case Some(rule) => rule.pat match {
            case RestrictableChoosePattern.Error(_, loc) => // If the pattern is an error we just return fresh error variables
              (Type.freshError(Kind.Star, loc), Type.freshError(Kind.Eff, loc))

            case RestrictableChoosePattern.Tag(symUse, _, _, _) => // If the pattern is Tag then we emit constraint rules
              val enumSym = symUse.sym.enumSym
              val enum0 = root.restrictableEnums(enumSym)

              // The expected enum types and the index variables.
              val (enumTypeIn, indexInVar, _) = instantiatedEnumType(enumSym, enum0, loc.asSynthetic)
              val (enumTypeOut, indexOutVar, targsOut) = instantiatedEnumType(enumSym, enum0, loc.asSynthetic)
              val (bodyTypes, bodyIndexVars, bodyTargs) = rules0.map(_ => instantiatedEnumType(enumSym, enum0, loc.asSynthetic)).unzip3
              val patternTagTypes = rules0.map(_.pat match {
                case RestrictableChoosePattern.Tag(symUse, _, _, loc) => Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), loc.asSynthetic)
                case RestrictableChoosePattern.Error(tvar, _) => tvar
              })

              val domSet = dom(rules0)
              val domM = toType(domSet, enumSym, loc.asSynthetic)

              def mkUnion(l: List[Type]): Type =
                l.reduceOption(Type.mkCaseUnion(_, _, enumSym, loc.asSynthetic)).
                  getOrElse(throw InternalCompilerException("unexpected empty choose", loc))

              // Γ ⊢ e: τ_in
              val (tpe, eff) = visitExp(exp0)
              val patTpes = visitRestrictableChoosePatterns(rules0.map(_.pat))
              c.unifyAllTypes(tpe :: patTpes, loc)

              // τ_in = (... + l^in_i(τ^in_i) + ...)[φ_in]
              c.unifyType(enumTypeIn, tpe, loc)

              // φ_in <: dom(M)
              unifySubset(indexInVar, domM, enumSym, loc.asSynthetic)

              // Γ, x_i: τ^in_i ⊢ e_i: τ^out_i
              val (tpes, effs) = rules0.map(rule => visitExp(rule.exp)).unzip
              tpes.zip(bodyTypes).foreach { case (t1, t2) => c.unifyType(t1, t2, loc) }

              // τ_out = (... + l^out_i(τ^out_i) + ...)[_]
              ((targsOut :: bodyTargs).transpose).foreach(c.unifyAllTypes(_, loc))

              val indicesAndTags = bodyIndexVars.zip(patternTagTypes)
              val intros = mkUnion(indicesAndTags.map { case (i, tag) => Type.mkCaseDifference(i, tag, enumSym, loc.asSynthetic) })
              val potentiallyStable = mkUnion(indicesAndTags.map { case (i, tag) => Type.mkCaseIntersection(i, tag, enumSym, loc.asSynthetic) })

              // φ_out :> (φ_in ∩ potentiallyStable) ∪ intros
              val set = Type.mkCaseUnion(
                Type.mkCaseIntersection(indexInVar, potentiallyStable, enumSym, loc),
                intros,
                enumSym,
                loc
              )
              unifySubset(set, indexOutVar, enumSym, loc)

              // τ_out
              c.unifyType(enumTypeOut, tpe0, loc)
              val resTpe = enumTypeOut
              val resEff = Type.mkUnion(eff :: effs, loc)
              (resTpe, resEff)

          }
        }
    }
  }

  /**
    * Performs type inference on the given restrictable tag expression.
    */
  def visitApplyRestrictableTag(exp: KindedAst.Expr.RestrictableTag)(implicit scope: Scope, c: TypeContext2, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    exp match {
      case KindedAst.Expr.RestrictableTag(symUse, exps, isOpen, tvar, evar, loc) =>

        // Lookup the enum declaration.
        val enumSym = symUse.sym.enumSym
        val decl = root.restrictableEnums(enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // create the schema output type
        val (enumType, indexVar, targs) = instantiatedEnumType(enumSym, decl, loc.asSynthetic)
        // create our output type
        val (enumTypeOut, indexVarOut, targsOut) = instantiatedEnumType(enumSym, decl, loc.asSynthetic)

        // for open tags we want to add the label to the index
        // indexVarOut == indexVar U {label}
        // targs == targsOut

        // for non-open tags we want to constrict the index to the singelton label
        // indexVar == {label} == indexVarOut
        // targs == targsOut

        // We do this because indexVar is unconstrained for non-recursive types
        // and we want to control the open variables in the source program for
        // unification performance

        // TODO ASSOC-TYPES is there a reason we do this later?
        if (isOpen) {
          // φ_in ∪ {l_i}
          val index =
            Type.mkCaseUnion(
              indexVar,
              Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), loc.asSynthetic),
              enumSym,
              loc.asSynthetic
            )
          c.unifyType(index, indexVarOut, loc)
        } else {
          // {l_i}
          val index = Type.Cst(TypeConstructor.CaseSet(SortedSet(symUse.sym), enumSym), loc.asSynthetic)
          c.unifyType(index, indexVar, loc)
          c.unifyType(indexVar, indexVarOut, loc)
        }


        // Instantiate the type scheme of the case.
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of terms to the type of the enum.
        //
        // Γ ⊢ e: τ
        val (tpes, effs) = exps.map(visitExp).unzip
        val constructorBase = Type.mkPureUncurriedArrow(tpes, enumType, loc)
        c.unifyType(tagType, constructorBase, loc)
        targs.zip(targsOut).foreach { case (targ, targOut) => c.unifyType(targ, targOut, loc) }
        //        _ <- indexUnification // TODO ASSOC-TYPES here is where we did the index unification before
        c.unifyType(enumTypeOut, tvar, loc)
        c.unifyType(Type.mkUnion(effs, loc), evar, loc)
        val resTpe = tvar
        val resEff = evar
        (resTpe, resEff)
    }
  }

  /**
    * Performs type inference on the given OpenAs expression.
    *
    * `OpenAs X e` requires that `e` have the type X[s] for some s
    * The result type of the expression is X[s + φ] for some free φ
    *
    * Γ ⊢ e : X[s][α1 ... αn]
    * -------------------------------------
    * Γ ⊢ open_as X e : X[s + φ][α1 ... αn]
    */
  def visitOpenAs(exp0: KindedAst.Expr.OpenAs)(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): (Type, Type) = {
    implicit val scope: Scope = c.getScope
    exp0 match {
      case KindedAst.Expr.OpenAs(RestrictableEnumSymUse(sym, _), exp, tvar, loc) =>
        val `enum` = root.restrictableEnums(sym)

        val (enumType, indexVar, targs) = instantiatedEnumType(sym, `enum`, loc.asSynthetic)
        val kargs = `enum`.index.sym.kind :: `enum`.tparams.map(_.sym.kind)
        val kind = Kind.mkArrow(kargs)

        // infer the inner expression type τ
        val (tpe, eff) = visitExp(exp)

        // make sure the expression has type EnumType[s][α1 ... αn]
        c.expectType(expected = enumType, actual = tpe, loc)

        // the new index is s ∪ φ for some free φ
        val openIndex = Type.mkCaseUnion(indexVar, Type.freshVar(Kind.CaseSet(`enum`.sym), loc.asSynthetic), sym, loc)

        // the result type is EnumType[s ∪ φ][α1 ... αn]
        val resultType = Type.mkApply(
          Type.Cst(TypeConstructor.RestrictableEnum(sym, kind), loc.asSynthetic),
          openIndex :: targs,
          loc
        )

        // unify the tvar
        c.unifyType(tvar, resultType, loc)

        val resTpe = resultType
        val resEff = eff
        (resTpe, resEff)
    }
  }

  /**
    * Returns the instantiated conceptual schema of the enum along with the type
    * variable that is the index type argument.
    *
    * The first and the second instantiation share all variables except the index.
    */
  private def instantiatedEnumType(enumSym: Symbol.RestrictableEnumSym, decl: KindedAst.RestrictableEnum, loc: SourceLocation)(implicit scope: Scope, flix: Flix): (Type, Type.Var, List[Type]) = {
    // TODO RESTR-VARS can get rid of enumSym since it's in the decl
    // Make fresh vars for all the type parameters
    // This will unify with the enum type to extract the index

    def instantiate(tp: KindedAst.TypeParam): Type.Var = tp match {
      case KindedAst.TypeParam(_, sym, loc) => Type.freshVar(sym.kind, loc, sym.isRegion)
    }

    val indexVar = instantiate(decl.index)
    val tparamArgs = decl.tparams.map(instantiate)
    val targs = indexVar :: tparamArgs

    val enumConstructorKind = Kind.mkArrow(targs.map(_.kind))
    val enumConstructor = Type.Cst(TypeConstructor.RestrictableEnum(enumSym, enumConstructorKind), loc)

    // The expected enum type.
    val enumType = Type.mkApply(enumConstructor, targs, loc)

    (enumType, indexVar, tparamArgs)
  }

  /**
    * Infers the type of the given restrictable choice pattern `pat0`.
    */
  private def visitRestrictableChoosePattern(pat0: KindedAst.RestrictableChoosePattern)(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): Type = {
    implicit val scope: Scope = c.getScope

    /**
      * Local pattern visitor.
      */
    def visit(p: KindedAst.RestrictableChoosePattern): Type = p match {
      case KindedAst.RestrictableChoosePattern.Tag(symUse, pat, tvar, loc) =>
        // Lookup the enum declaration.
        val decl = root.restrictableEnums(symUse.sym.enumSym)

        // Lookup the case declaration.
        val caze = decl.cases(symUse.sym)

        // Instantiate the type scheme of the case.
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        //
        // The tag type is a function from the type of variant to the type of the enum.
        //
        val tpes = pat.map(visitVarOrWild)
        val constructorBase = Type.mkPureUncurriedArrow(tpes, tvar, loc)
        c.unifyType(tagType, constructorBase, loc)
        val resTpe = tvar
        resTpe

      case KindedAst.RestrictableChoosePattern.Error(_, loc) => Type.freshError(Kind.Star, loc)
    }

    visit(pat0)
  }

  /**
    * Infers the type of the given restrictable choice pattern `pat0`.
    */
  private def visitVarOrWild(pat: KindedAst.RestrictableChoosePattern.VarOrWild)(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): Type = pat match {
    case KindedAst.RestrictableChoosePattern.Wild(tvar, _) => tvar
    case KindedAst.RestrictableChoosePattern.Var(sym, tvar, loc) =>
      c.unifyType(sym.tvar, tvar, loc)
      tvar
    case KindedAst.RestrictableChoosePattern.Error(tvar, _) => tvar
  }

  /**
    * Infers the type of the given patterns `pats0`.
    */
  private def visitRestrictableChoosePatterns(pats0: List[KindedAst.RestrictableChoosePattern])(implicit c: TypeContext2, root: KindedAst.Root, flix: Flix): List[Type] = {
    pats0.map(visitRestrictableChoosePattern)
  }
}
