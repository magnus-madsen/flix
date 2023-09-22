package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.KindedAst.Expr
import ca.uwaterloo.flix.language.ast.{KindedAst, LevelEnv, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.Chain

object ConstraintGeneration {

  sealed class Constraint

  object Constraint {
    case class Equality(tpe1: Type, tpe2: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint

    case class Class(sym: Symbol.ClassSym, tpe: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation) extends Constraint
  }
  def unifyTypeM(tpe1: Type, tpe2: Type, renv: RigidityEnv, lenv: LevelEnv, loc: SourceLocation): Monad[Unit] = {
    Monad((), Chain(Constraint.Equality(tpe1, tpe2, renv, lenv, loc)))
  }

  case object Monad {
    def point[A](value: A): Monad[A] = Monad(value, Chain.empty)
  }
  case class Monad[+A](value: A, constrs: Chain[Constraint]) {
    def map[B](f: A => B): Monad[B] = Monad(f(value), constrs)

    def flatMap[B](f: A => Monad[B]): Monad[B] = {
      val Monad(newValue, newConstrs) = f(value)
      Monad(newValue, constrs ++ newConstrs)
    }

    def withFilter(f: A => Boolean): Monad[A] = if (f(value)) this else Monad(value, Chain.empty)
  }
  def visitExp(exp0: KindedAst.Expr, renv: RigidityEnv, lenv: LevelEnv): Monad[(Type, Type)] = exp0 match {
    case Expr.Var(sym, loc) => Monad.point((sym.tvar, Type.Pure))
    case Expr.Def(sym, tpe, loc) => ???
    case Expr.Sig(sym, tpe, loc) => ???
    case Expr.Hole(sym, tpe, loc) => ???
    case Expr.HoleWithExp(exp, tpe, eff, loc) => ???
    case Expr.OpenAs(symUse, exp, tvar, loc) => ???
    case Expr.Use(sym, alias, exp, loc) => ???
    case Expr.Cst(cst, loc) => ???
    case Expr.Apply(exp, exps, tpe, eff, loc) => ???
    case Expr.Lambda(fparam, exp, loc) => {
      for {
        _ <- unifyTypeM(fparam.sym.tvar, fparam.tpe, renv, lenv, loc)
        (tpe, eff) <- visitExp(exp, renv, lenv)
        resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, tpe, loc)
        resEff = Type.Pure
      } yield (resTpe, resEff)
    }
    case Expr.Unary(sop, exp, tpe, loc) => ???
    case Expr.Binary(sop, exp1, exp2, tpe, loc) => ???
    case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      for {
        (tpe1, eff1) <- visitExp(exp1, renv, lenv)
        (tpe2, eff2) <- visitExp(exp2, renv, lenv)
        (tpe3, eff3) <- visitExp(exp3, renv, lenv)
        _ <- unifyTypeM(tpe1, Type.Bool, renv, lenv, exp1.loc) // TODO ASSOC-TYPES add `expectType`
        _ <- unifyTypeM(tpe2, tpe3, renv, lenv, loc)
        resTpe = tpe3
        resEff = Type.mkUnion(eff1, eff2, eff3, loc)
      } yield (resTpe, resEff)
    case Expr.Stm(exp1, exp2, loc) =>
      for {
        (tpe1, eff1) <- visitExp(exp1, renv, lenv)
        (tpe2, eff2) <- visitExp(exp2, renv, lenv)
        resTpe = tpe2
        resEff = Type.mkUnion(eff1, eff2, loc)
      } yield (resTpe, resEff)
    case Expr.Discard(exp, loc) => ???
    case Expr.Let(sym, mod, exp1, exp2, loc) => ???
    case Expr.LetRec(sym, mod, exp1, exp2, loc) => ???
    case Expr.Region(tpe, loc) => ???
    case Expr.Scope(sym, regionVar, exp1, pvar, loc) => ???
    case Expr.ScopeExit(exp1, exp2, loc) => ???
    case Expr.Match(exp, rules, loc) => ???
    case Expr.TypeMatch(exp, rules, loc) => ???
    case Expr.RelationalChoose(star, exps, rules, tpe, loc) => ???
    case Expr.RestrictableChoose(star, exp, rules, tpe, loc) => ???
    case Expr.Tag(sym, exp, tpe, loc) => ???
    case Expr.RestrictableTag(sym, exp, isOpen, tpe, loc) => ???
    case Expr.Tuple(elms, loc) => ???
    case Expr.RecordEmpty(loc) => ???
    case Expr.RecordSelect(exp, label, tpe, loc) => ???
    case Expr.RecordExtend(label, value, rest, tpe, loc) => ???
    case Expr.RecordRestrict(label, rest, tpe, loc) => ???
    case Expr.ArrayLit(exps, exp, tvar, pvar, loc) => ???
    case Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) => ???
    case Expr.ArrayLoad(base, index, tpe, pvar, loc) => ???
    case Expr.ArrayStore(base, index, elm, pvar, loc) => ???
    case Expr.ArrayLength(base, loc) => ???
    case Expr.VectorLit(exps, tvar, pvar, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tpe, pvar, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ref(exp1, exp2, tvar, pvar, loc) => ???
    case Expr.Deref(exp, tvar, pvar, loc) => ???
    case Expr.Assign(exp1, exp2, pvar, loc) => ???
    case Expr.Ascribe(exp, expectedType, expectedPur, tpe, loc) => ???
    case Expr.InstanceOf(exp, clazz, loc) => ???
    case Expr.CheckedCast(cast, exp, tvar, pvar, loc) => ???
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, loc) => ???
    case Expr.UncheckedMaskingCast(exp, loc) => ???
    case Expr.Without(exp, eff, loc) => ???
    case Expr.TryCatch(exp, rules, loc) => ???
    case Expr.TryWith(exp, eff, rules, tvar, loc) => ???
    case Expr.Do(op, args, tvar, loc) => ???
    case Expr.Resume(exp, argTvar, retTvar, loc) => ???
    case Expr.InvokeConstructor(constructor, args, loc) => ???
    case Expr.InvokeMethod(method, clazz, exp, args, loc) => ???
    case Expr.InvokeStaticMethod(method, args, loc) => ???
    case Expr.GetField(field, clazz, exp, loc) => ???
    case Expr.PutField(field, clazz, exp1, exp2, loc) => ???
    case Expr.GetStaticField(field, loc) => ???
    case Expr.PutStaticField(field, exp, loc) => ???
    case Expr.NewObject(name, clazz, methods, loc) => ???
    case Expr.NewChannel(exp1, exp2, tvar, loc) => ???
    case Expr.GetChannel(exp, tpe, loc) => ???
    case Expr.PutChannel(exp1, exp2, loc) => ???
    case Expr.SelectChannel(rules, default, tpe, loc) => ???
    case Expr.Spawn(exp1, exp2, loc) => ???
    case Expr.ParYield(frags, exp, loc) => ???
    case Expr.Lazy(exp, loc) => ???
    case Expr.Force(exp, tpe, loc) => ???
    case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
    case Expr.FixpointLambda(pparams, exp, tpe, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, loc) => ???
    case Expr.FixpointSolve(exp, loc) => ???
    case Expr.FixpointFilter(pred, exp, tpe, loc) => ???
    case Expr.FixpointInject(exp, pred, tpe, loc) => ???
    case Expr.FixpointProject(pred, exp1, exp2, tpe, loc) => ???
    case Expr.Error(m, tpe, eff) => ???
  }
}
