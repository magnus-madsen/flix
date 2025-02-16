/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol.KindedTypeVarSym
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{EffUnification3, EqualityEnv, Unification}
import ca.uwaterloo.flix.util.Options

import scala.collection.immutable.SortedSet

object EffectLock {
  def println(s: Object): Unit = {
    if (false) {
      Predef.println(s)
    }
  }

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val sc1rename = naiveAlphaRename(sc1)
    val sc2rename = naiveAlphaRename(sc2)
    println(s"g: $sc1")
    println(s"f: $sc2")
    println(s"renamed sc1: $sc1rename")
    println(s"renamed sc2: $sc2rename")
    isGeneralizable(sc1, sc2) || isSubset(sc1rename, sc2rename)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def isGeneralizable(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, renv, EqualityEnv.empty)(Scope.Top, flix)
    unification match {
      case Some(subst) =>
        println(subst)
        println(s"subst(sc1.base): ${subst(sc1.base)}")
        println(s"sc2.base: ${sc2.base}")
        subst(sc1.base) == sc2.base
      case None => false
    }
  }

  /**
    * Subset-rule
    *
    * 𝜑 ∪ 𝜑′ ≡ 𝜑′
    * ----------
    * 𝜏1 −→ 𝜏2 \ 𝜑 ⪯ 𝜏1 -→ 𝜏2 \ 𝜑′
    *
    */
  private def isSubset(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    val tpe1 = sc1.base
    val tpe2 = sc2.base

    // 1. Types match t1 -> t2
    (tpe1.typeConstructor, tpe2.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) =>

        val isMatchingArgs = tpe1.arrowArgTypes == tpe2.arrowArgTypes
        println(s"tpe1.arrowArgTypes: ${tpe1.arrowArgTypes}")
        println(s"tpe2.arrowArgTypes: ${tpe2.arrowArgTypes}")
        println(s"isMatchingArgs: $isMatchingArgs")
        val tpe1Res = tpe1.arrowResultType
        val tpe2Res = tpe2.arrowResultType
        val isMatchingResultTypes = (tpe1.arrowResultType.typeConstructor, tpe2.arrowResultType.typeConstructor) match {
          case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) =>
            val sc11 = Scheme(sc1.quantifiers, List.empty, List.empty, tpe1Res)
            val sc21 = Scheme(sc2.quantifiers, List.empty, List.empty, tpe2Res)
            isSubset(sc11, sc21)

          case (t1, t2) =>
            println(s"t1: $t1")
            println(s"t2: $t2")
            t1 == t2
        }
        println(s"tpe1.arrowResultType: ${tpe1.arrowResultType}")
        println(s"tpe2.arrowResultType: ${tpe2.arrowResultType}")
        println(s"isMatchingResultTypes: $isMatchingResultTypes")


        // 2. Boolean unification of effects phi + phi' = phi'
        val sc1Effs = tpe1.arrowEffectType
        val sc2Effs = tpe2.arrowEffectType
        val left = Type.mkUnion(sc1Effs, sc2Effs, sc1Effs.loc)
        val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
        val (unsolvedConstraints, _) = EffUnification3.unifyAll(List((left, sc2Effs, sc2Effs.loc)), Scope.Top, renv)
        println(s"sc1Effs: $sc1Effs")
        println(s"sc2Effs: $sc2Effs")
        println(unsolvedConstraints)
        isMatchingArgs && isMatchingResultTypes & unsolvedConstraints.isEmpty

      case _ => false
    }
  }

  private def naiveAlphaRename(sc: Scheme): Scheme = {
    val seen = scala.collection.mutable.Map.empty[KindedTypeVarSym, KindedTypeVarSym]

    def visit(tpe00: Type): Type = tpe00 match {
      case Type.Var(sym, loc) => seen.get(sym) match {
        case Some(subst) => Type.Var(subst, loc)
        case None =>
          val subst = new KindedTypeVarSym(seen.size, sym.text, sym.kind, sym.isSlack, sym.scope, sym.loc)
          seen += sym -> subst
          Type.Var(subst, loc)
      }

      case Type.Cst(tc, loc) =>
        Type.Cst(tc, loc)

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = visit(tpe1)
        val t2 = visit(tpe2)
        Type.Apply(t1, t2, loc)

      case Type.Alias(symUse, args, tpe, loc) =>
        val as = args.map(visit)
        val t = visit(tpe)
        Type.Alias(symUse, as, t, loc)

      case Type.AssocType(symUse, arg, kind, loc) =>
        val a = visit(arg)
        Type.AssocType(symUse, a, kind, loc)

      case Type.JvmToType(tpe, loc) =>
        val t = visit(tpe)
        Type.JvmToType(t, loc)

      case Type.JvmToEff(tpe, loc) =>
        val t = visit(tpe)
        Type.JvmToEff(t, loc)

      case Type.UnresolvedJvmType(member, loc) =>
        Type.UnresolvedJvmType(member, loc)
    }

    val base = visit(sc.base)
    val qs = sc.quantifiers.map(q => seen.getOrElse(q, q))
    Scheme(qs, sc.tconstrs, sc.econstrs, base)
  }
}

