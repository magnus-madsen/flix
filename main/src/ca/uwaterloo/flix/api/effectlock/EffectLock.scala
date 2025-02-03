package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Unification}

object EffectLock {

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = { // TODO: Check if new flix instance can be used locally
    unifiableSchemes(sc1, sc2) || monomorphicDowngrade(sc1, sc2)
  }

  private def unifiableSchemes(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, RigidityEnv.empty, EqualityEnv.empty)(Scope.Top, flix)
    unification match {
      case Some(subst) => subst(sc2) == sc2
      case None => false
    }
  }

  private def monomorphicDowngrade(sc1: Scheme, sc2: Scheme): Boolean = {
    val noQuantifiers = sc1.quantifiers.isEmpty && sc2.quantifiers.isEmpty
    val sameMonomorphicType = checkSameMonomorphicType(sc1.base, sc2.base)
    val subsetOfEffects = sc1.base.arrowEffectType.effects.subsetOf(sc2.base.arrowEffectType.effects)
    noQuantifiers && sameMonomorphicType && subsetOfEffects
  }

  private def checkSameMonomorphicType(tpe1: Type, tpe2: Type): Boolean = (tpe1, tpe2) match {
    // Ignore effect cases
    case (Type.Cst(TypeConstructor.Effect(_), _), _) => true
    case (Type.Cst(TypeConstructor.Pure, _), _) => true
    case (_, Type.Cst(TypeConstructor.Effect(_), _)) => true
    case (_, Type.Cst(TypeConstructor.Pure, _)) => true

    case (Type.Cst(tc1, _), Type.Cst(tc2, _)) =>
      tc1 == tc2

    case (Type.Apply(tpe11, tpe12, _), Type.Apply(tpe21, tpe22, _)) =>
      checkSameMonomorphicType(tpe11, tpe21) && checkSameMonomorphicType(tpe12, tpe22)

    case (Type.Alias(symUse1, args1, tpe1, _), Type.Alias(symUse2, args2, tpe2, _)) => ???
    case (Type.AssocType(symUse1, arg1, kind1, _), Type.AssocType(symUse2, arg2, kind2, _)) => ???
    case _ => false
  }
}
