package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.{BootstrapError, Flix}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.shared.Input
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.phase.typer.PrimitiveEffects
import ca.uwaterloo.flix.tools.pkg.{Dependency, Permissions}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.lang.reflect.{Constructor, Method}
import java.nio.file.Path

object TrustValidation {

  def run(root: TypedAst.Root, dependencies: Set[Dependency.FlixDependency])(implicit flix: Flix): List[BootstrapError.TrustError] = {
    val suspiciousExprs = findSuspiciousExprs(root)
    val suspiciousLibExprs = pairWithLib(suspiciousExprs)
    dependencies.toList.flatMap(l => validateTrustLevels(l, suspiciousLibExprs.get(l.projectName))) // can use parmap
  }

  private def findSuspiciousExprs(root: TypedAst.Root)(implicit flix: Flix): List[SuspiciousExpr] = {
    val traitErrors = ParOps.parMap(root.traits.values)(visitTrait).flatten.toList
    val instanceErrors = ParOps.parMap(root.instances.values)(visitInstance).flatten.toList
    val signatureErrors = ParOps.parMap(root.sigs.values)(visitSig).flatten.toList
    val defErrors = ParOps.parMap(root.defs.values)(visitDef).flatten.toList
    traitErrors ::: instanceErrors ::: signatureErrors ::: defErrors
  }

  private def visitTrait(trait0: TypedAst.Trait): List[SuspiciousExpr] = trait0 match {
    case TypedAst.Trait(_, _, _, _, _, _, _, sigs, laws, loc) if isLibrary(loc) => sigs.flatMap(visitSig) ::: laws.flatMap(visitDef)
    case TypedAst.Trait(_, _, _, _, _, _, _, _, _, _) => List.empty
  }

  private def visitInstance(instance0: TypedAst.Instance): List[SuspiciousExpr] = instance0 match {
    case TypedAst.Instance(_, _, _, _, _, _, _, defs, _, loc) if isLibrary(loc) => defs.flatMap(visitDef)
    case TypedAst.Instance(_, _, _, _, _, _, _, _, _, _) => List.empty
  }

  private def visitSig(sig0: TypedAst.Sig): List[SuspiciousExpr] = sig0 match {
    case TypedAst.Sig(_, _, Some(exp), loc) if isLibrary(loc) => visitExp(exp)
    case TypedAst.Sig(_, _, _, _) => List.empty
  }

  private def visitDef(defn0: TypedAst.Def): List[SuspiciousExpr] = defn0 match {
    case TypedAst.Def(_, _, exp, loc) if isLibrary(loc) => visitExp(exp)
    case TypedAst.Def(_, _, _, _) => List.empty
  }

  private def visitExp(expr0: TypedAst.Expr): List[SuspiciousExpr] = expr0 match {
    case Expr.Cst(_, _, _) =>
      List.empty

    case Expr.Var(_, _, _) =>
      List.empty

    case Expr.Hole(_, _, _, _) =>
      List.empty

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Region(_, _) =>
      List.empty

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp) ::: r.guard.map(visitExp).getOrElse(List.empty))

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.Tag(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.Tuple(exps, _, _, _) =>
      exps.flatMap(visitExp)
    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      exps.flatMap(visitExp) ::: visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      fields.flatMap(se => visitExp(se._2)) ::: visitExp(region)

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, clazz, loc) =>
      val err = SuspiciousExpr.InstanceOfUse(Expr.InstanceOf(exp, clazz, loc))
      err :: visitExp(exp)

    case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.CheckedCastUse(Expr.CheckedCast(cast, exp, tpe, eff, loc))
      err :: visitExp(exp)

    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val err = SuspiciousExpr.UncheckedCastUse(Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc))
      err :: visitExp(exp)

    case Expr.Unsafe(exp, runEff, tpe, eff, loc) =>
      val err = SuspiciousExpr.UnsafeUse(Expr.Unsafe(exp, runEff, tpe, eff, loc))
      err :: visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val err = SuspiciousExpr.TryCatchUse(Expr.TryCatch(exp, rules, tpe, eff, loc))
      err :: visitExp(exp) ::: rules.flatMap(r => visitExp(r.exp))

    case Expr.Throw(exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.ThrowUse(Expr.Throw(exp, tpe, eff, loc))
      err :: visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      rules.flatMap(r => visitExp(r.exp))

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeConstructorUse(Expr.InvokeConstructor(constructor, exps, tpe, eff, loc))
      err :: exps.flatMap(visitExp)

    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeMethodUse(Expr.InvokeMethod(method, exp, exps, tpe, eff, loc))
      err :: visitExp(exp) ::: exps.flatMap(visitExp)

    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
      val err = SuspiciousExpr.InvokeStaticMethodUse(Expr.InvokeStaticMethod(method, exps, tpe, eff, loc))
      err :: exps.flatMap(visitExp)

    case Expr.GetField(field, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.GetFieldUse(Expr.GetField(field, exp, tpe, eff, loc))
      err :: visitExp(exp)


    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val err = SuspiciousExpr.PutFieldUse(Expr.PutField(field, exp1, exp2, tpe, eff, loc))
      err :: visitExp(exp1) ::: visitExp(exp2)

    case Expr.GetStaticField(field, tpe, eff, loc) =>
      val err = SuspiciousExpr.GetStaticFieldUse(Expr.GetStaticField(field, tpe, eff, loc))
      List(err)

    case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val err = SuspiciousExpr.PutStaticFieldUse(Expr.PutStaticField(field, exp, tpe, eff, loc))
      err :: visitExp(exp)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val err = SuspiciousExpr.NewObjectUse(Expr.NewObject(name, clazz, tpe, eff, methods, loc))
      err :: methods.flatMap(m => visitExp(m.exp))

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap(r => visitExp(r.exp) ::: visitExp(r.chan)) ::: default.map(visitExp).getOrElse(List.empty)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.flatMap(f => visitExp(f.exp)) ::: visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.flatMap(visitConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) => List.empty
  }

  private def visitConstraint(constr0: TypedAst.Constraint): List[SuspiciousExpr] = constr0 match {
    case TypedAst.Constraint(_, head, body, _) =>
      val headErrors = head match {
        case Head.Atom(_, _, terms, _, _) => terms.flatMap(visitExp)
      }
      val bodyErrors = body.flatMap {
        case Body.Atom(_, _, _, _, _, _, _) => List.empty
        case Body.Functional(_, exp, _) => visitExp(exp)
        case Body.Guard(exp, _) => visitExp(exp)
      }
      headErrors ::: bodyErrors
  }

  private def isLibrary(loc0: SourceLocation): Boolean = loc0.sp1.source.input match {
    case Input.Text(_, _, _) => false
    case Input.TxtFile(_, _) => false
    case Input.PkgFile(_, _) => false // TODO maybe consider flipping this to false?
    case Input.FileInPackage(_, _, _, _) => true
    case Input.Unknown => false
  }

  private def libFromLoc(loc: SourceLocation): String = loc.sp1.source.input match {
    case Input.Text(_, _, _) => throw InternalCompilerException("expected library input", loc)
    case Input.TxtFile(_, _) => throw InternalCompilerException("expected library input", loc)
    case Input.PkgFile(packagePath, _) => resolveLibName(packagePath) // TODO: May break
    case Input.FileInPackage(packagePath, _, _, _) => resolveLibName(packagePath)
    case Input.Unknown => throw InternalCompilerException("expected library input", loc)
  }

  private def pairWithLib(suspiciousExprs: List[SuspiciousExpr]): ListMap[String, SuspiciousExpr] = {
    ListMap.from(suspiciousExprs.map {
      case expr => libFromLoc(expr.expr.loc) -> expr
    })
  }

  private def validateTrustLevels(dependency: Dependency.FlixDependency, suspiciousLibExprs: List[SuspiciousExpr]): List[BootstrapError.TrustError] = dependency.permissions match {
    // TODO: Use lib name for error reporting
    case Permissions.FlixOnly => suspiciousLibExprs.map(e => BootstrapError.TrustError(e.expr.loc)) // if it is empty then no alarms were raised
    case Permissions.Restricted => suspiciousLibExprs.flatMap(validationSuspiciousExpr(TrustedJvmBase.get)) // TODO: refactor trustedjvmbase.get
    case Permissions.All => List.empty
  }

  /**
    * Validates suspicious expressions according to the [[Permissions.Restricted]] level.
    */
  private def validationSuspiciousExpr(trustedBase: TrustedJvmBase)(expr0: SuspiciousExpr): Option[BootstrapError.TrustError] = expr0 match {
    // TODO: Move to TrustValidation
    case SuspiciousExpr.InstanceOfUse(expr) =>
      val safe = trustedBase.contains(expr.clazz)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.CheckedCastUse(expr) =>
      Some(BootstrapError.TrustError(expr.loc))

    case SuspiciousExpr.UncheckedCastUse(expr) =>
      Some(BootstrapError.TrustError(expr.loc))

    case SuspiciousExpr.UnsafeUse(expr) =>
      None

    case SuspiciousExpr.TryCatchUse(expr) =>
      val safe = expr.rules.forall(r => trustedBase.contains(r.clazz)) // Maybe sequence options?
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc)) // TODO: Report error for each occurrence
      }

    case SuspiciousExpr.ThrowUse(expr) =>
      None

    case SuspiciousExpr.InvokeConstructorUse(expr) =>
      val safe = trustedBase.contains(expr.constructor)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.InvokeMethodUse(expr) =>
      val safe = trustedBase.contains(expr.method)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.InvokeStaticMethodUse(expr) =>
      val safe = trustedBase.contains(expr.method)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.GetFieldUse(expr) =>
      val safe = trustedBase.contains(expr.field.getDeclaringClass)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.PutFieldUse(expr) =>
      val safe = trustedBase.contains(expr.field.getDeclaringClass)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.GetStaticFieldUse(expr) =>
      val safe = trustedBase.contains(expr.field.getDeclaringClass)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.PutStaticFieldUse(expr) =>
      val safe = trustedBase.contains(expr.field.getDeclaringClass)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }

    case SuspiciousExpr.NewObjectUse(expr) =>
      val safe = trustedBase.contains(expr.clazz)
      if (safe) {
        None
      } else {
        Some(BootstrapError.TrustError(expr.loc))
      }
  }

  private case class TrustedJvmBase(methods: Set[Method], constructors: Set[Constructor[?]], classes: Set[Class[?]], packages: Set[Package]) {
    def contains(method: Method): Boolean = {
      methods.contains(method) || contains(method.getDeclaringClass)
    }

    def contains(constructor: Constructor[?]): Boolean = {
      constructors.contains(constructor) || contains(constructor.getDeclaringClass)
    }

    def contains(clazz: Class[?]): Boolean = {
      classes.contains(clazz) || contains(clazz.getPackage)
    }

    def contains(pkg: Package): Boolean = {
      packages.contains(pkg)
    }

  }

  private object TrustedJvmBase {
    def get: TrustedJvmBase = {
      val methods = PrimitiveEffects.getAnnotatedMethods
      val constructors = PrimitiveEffects.getAnnotatedConstructors
      val classes = PrimitiveEffects.getAnnotatedClasses
      val packages = PrimitiveEffects.getAnnotatedPackages
      TrustedJvmBase(methods, constructors, classes, packages)
    }
  }

  private def resolveLibName(path: Path): String = {
    path.getParent.getParent.normalize().getFileName.toString
  }
}
