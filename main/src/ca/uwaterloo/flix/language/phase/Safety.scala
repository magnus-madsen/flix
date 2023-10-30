package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Denotation, Fixity, Polarity}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError._
import ca.uwaterloo.flix.util.Validation

import java.math.BigInteger
import scala.annotation.tailrec

/**
  * Performs safety and well-formedness checks on:
  *
  *  - Datalog constraints.
  *  - New object expressions.
  *  - CheckedCast expressions.
  *  - UncheckedCast expressions.
  *  - TypeMatch expressions.
  */
object Safety {

  /**
    * Performs safety and well-formedness checks on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Safety") {
    //
    // Collect all errors.
    //
    val errors = visitDefs(root) ++ visitSendable(root)

    //
    // Check if any errors were found.
    //
    if (errors.isEmpty)
      Validation.Success(root)
    else
      Validation.SoftFailure(root, errors.to(LazyList))
  }

  /**
    * Checks that no type parameters for types that implement `Sendable` of kind `Region`
    */
  private def visitSendable(root: Root)(implicit flix: Flix): List[CompilationMessage] = {

    val sendableClass = new Symbol.ClassSym(Nil, "Sendable", SourceLocation.Unknown)

    root.instances.getOrElse(sendableClass, Nil) flatMap {
      case Instance(_, _, _, _, tpe, _, _, _, _, loc) =>
        if (tpe.typeArguments.exists(_.kind == Kind.Eff))
          List(SafetyError.SendableError(tpe, loc))
        else
          Nil
    }
  }

  /**
    * Performs safety and well-formedness checks all defs in the given AST `root`.
    */
  private def visitDefs(root: Root)(implicit flix: Flix): List[CompilationMessage] = {
    root.defs.flatMap {
      case (_, defn) => visitDef(defn, root)
    }.toList
  }

  /**
    * Performs safety and well-formedness checks on the given definition `def0`.
    */
  private def visitDef(def0: Def, root: Root)(implicit flix: Flix): List[CompilationMessage] = {
    val renv = def0.spec.tparams.map(_.sym).foldLeft(RigidityEnv.empty) {
      case (acc, e) => acc.markRigid(e)
    }
    val tailpos = if (def0.spec.ann.isTailRecursive) TailPosition(def0.sym) else NonTailPosition
    visitTestEntryPoint(def0) ::: visitExp(def0.exp, renv, tailpos, root)
  }

  /**
    * Checks that if `def0` is a test entry point that it is well-behaved.
    */
  private def visitTestEntryPoint(def0: Def): List[CompilationMessage] = {
    if (def0.spec.ann.isTest) {
      def isUnitType(fparam: FormalParam): Boolean = fparam.tpe.typeConstructor.contains(TypeConstructor.Unit)

      val hasUnitParameter = def0.spec.fparams match {
        case fparam :: Nil => isUnitType(fparam)
        case _ => false
      }

      if (!hasUnitParameter) {
        val err = SafetyError.IllegalTestParameters(def0.sym.loc)
        List(err)
      } else {
        Nil
      }
    }

    else Nil

  }

  /**
    * Represents the type of position a function call can appear in.
    */
  private sealed trait CallPosition

  private case class TailPosition(sym: Symbol.DefnSym) extends CallPosition

  private case object NonTailPosition extends CallPosition

  private def checkTailCallAnnotation(expectedPosition: CallPosition, actualPosition: CallPosition, actualSym: Option[Symbol.DefnSym], loc: SourceLocation): List[CompilationMessage] =
    (expectedPosition, actualPosition, actualSym) match {
      case (TailPosition(sym), NonTailPosition, Some(asym)) if sym == asym => SafetyError.NonTailRecursiveFunction(sym, loc) :: Nil
      case _ => Nil
    }

  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(e0: Expr, renv: RigidityEnv, expectedCallPosition: CallPosition, root: Root)(implicit flix: Flix): List[CompilationMessage] = {

    var currentCallSym: Option[Symbol.DefnSym] = None
    var containsRecursiveCall = false

    /**
      * Local visitor.
      */
    def visit(exp0: Expr, currentCallPosition: CallPosition): List[CompilationMessage] = exp0 match {
      case Expr.Cst(_, _, _) => Nil

      case Expr.Var(_, _, _) => Nil

      case Expr.Def(sym, _, _) =>
        currentCallSym = Some(sym)
        expectedCallPosition match {
          case TailPosition(esym) if sym == esym => containsRecursiveCall = true
          case _ => ()
        }
        Nil

      case Expr.Sig(_, _, _) => Nil

      case Expr.Hole(_, _, _) => Nil

      case Expr.HoleWithExp(exp, _, _, _) =>
        visit(exp, currentCallPosition)

      case Expr.OpenAs(_, exp, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Use(_, _, exp, _) =>
        visit(exp, NonTailPosition)

      case Expr.Lambda(_, exp, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Apply(exp, exps, _, _, loc) =>
        currentCallSym = None
        visit(exp, NonTailPosition) ++
          checkTailCallAnnotation(expectedCallPosition, currentCallPosition, currentCallSym, loc) ++
          exps.flatMap(visit(_, NonTailPosition))

      case Expr.Unary(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Binary(_, exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.Let(_, _, exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, currentCallPosition)

      case Expr.LetRec(sym, ann, _, exp1, exp2, _, _, _) =>
        val e1 =     if (ann.isTailRecursive) {
          visitExp(exp1, renv, TailPosition(sym), root)
        } else {
          visit(exp1, NonTailPosition)
        }
        val e2 = visit(exp2, currentCallPosition)
        e1 ++ e2

      case Expr.Region(_, _) => Nil

      case Expr.Scope(_, _, exp, _, _, _) =>
        visit(exp, currentCallPosition)

      case Expr.ScopeExit(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, currentCallPosition) ++ visit(exp3, currentCallPosition)

      case Expr.Stm(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, currentCallPosition)

      case Expr.Discard(exp, _, _) =>
        visit(exp, currentCallPosition)

      case Expr.Match(exp, rules, _, _, _) =>
        visit(exp, NonTailPosition) ++
          rules.flatMap { case MatchRule(_, g, e) => g.toList.flatMap(visit(_, NonTailPosition)) ++ visit(e, currentCallPosition) }

      case Expr.TypeMatch(exp, rules, _, _, _) =>
        // check whether the last case in the type match looks like `...: _`
        val missingDefault = rules.last match {
          case TypeMatchRule(_, tpe, _) => tpe match {
            case Type.Var(sym, _) if renv.isFlexible(sym) => Nil
            case _ => List(SafetyError.MissingDefaultTypeMatchCase(exp.loc))
          }
        }
        visit(exp, NonTailPosition) ++ missingDefault ++
          rules.flatMap { case TypeMatchRule(_, _, e) => visit(e, currentCallPosition) }

      case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
        visit(exp, NonTailPosition) ++
          rules.flatMap { case RestrictableChooseRule(_, exp) => visit(exp, currentCallPosition) }

      case Expr.Tag(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.RestrictableTag(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Tuple(elms, _, _, _) =>
        elms.flatMap(visit(_, NonTailPosition))

      case Expr.RecordEmpty(_, _) => Nil

      case Expr.RecordSelect(exp, _, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.RecordExtend(_, value, rest, _, _, _) =>
        visit(value, NonTailPosition) ++ visit(rest, NonTailPosition)

      case Expr.RecordRestrict(_, rest, _, _, _) =>
        visit(rest, NonTailPosition)

      case Expr.ArrayLit(elms, exp, _, _, _) =>
        elms.flatMap(visit(_, NonTailPosition)) ++ visit(exp, NonTailPosition)

      case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition) ++ visit(exp3, NonTailPosition)

      case Expr.ArrayLoad(base, index, _, _, _) =>
        visit(base, NonTailPosition) ++ visit(index, NonTailPosition)

      case Expr.ArrayLength(base, _, _) =>
        visit(base, NonTailPosition)

      case Expr.ArrayStore(base, index, elm, _, _) =>
        visit(base, NonTailPosition) ++ visit(index, NonTailPosition) ++ visit(elm, NonTailPosition)

      case Expr.VectorLit(elms, _, _, _) =>
        elms.flatMap(visit(_, NonTailPosition))

      case Expr.VectorLoad(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.VectorLength(exp, _) =>
        visit(exp, NonTailPosition)

      case Expr.Ref(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.Deref(exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Assign(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.Ascribe(exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.InstanceOf(exp, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
        cast match {
          case CheckedCastType.TypeCast =>
            val from = Type.eraseAliases(exp.tpe)
            val to = Type.eraseAliases(tpe)
            val errors = verifyCheckedTypeCast(from, to, loc)
            visit(exp, currentCallPosition) ++ errors

          case CheckedCastType.EffectCast =>
            val from = Type.eraseAliases(exp.eff)
            val to = Type.eraseAliases(eff)
            val errors = verifyCheckedEffectCast(from, to, renv, loc)
            visit(exp, currentCallPosition) ++ errors
        }

      case e@Expr.UncheckedCast(exp, _, _, _, _, _) =>
        val errors = verifyUncheckedCast(e)
        visit(exp, currentCallPosition) ++ errors

      case Expr.UncheckedMaskingCast(exp, _, _, _) =>
        visit(exp, currentCallPosition)

      case Expr.Without(exp, _, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.TryCatch(exp, rules, _, _, _) =>
        visit(exp, NonTailPosition) ++
          rules.flatMap { case CatchRule(_, _, e) => visit(e, currentCallPosition) }

      case Expr.TryWith(exp, _, rules, _, _, _) =>
        visit(exp, NonTailPosition) ++
          rules.flatMap { case HandlerRule(_, _, e) => visit(e, currentCallPosition) }

      case Expr.Do(_, exps, _, _, _) =>
        val first = exps.take(exps.length - 1).flatMap(visit(_, NonTailPosition))
        first ++ visit(exps.last, currentCallPosition)

      case Expr.Resume(exp, _, _) =>
        visit(exp, currentCallPosition)

      case Expr.InvokeConstructor(_, args, _, _, loc) =>
        checkTailCallAnnotation(expectedCallPosition, currentCallPosition, None, loc) ++
          args.flatMap(visit(_, NonTailPosition))

      case Expr.InvokeMethod(_, exp, args, _, _, loc) =>
        checkTailCallAnnotation(expectedCallPosition, currentCallPosition, None, loc) ++
          visit(exp, NonTailPosition) ++ args.flatMap(visit(_, NonTailPosition))

      case Expr.InvokeStaticMethod(_, args, _, _, loc) =>
        checkTailCallAnnotation(expectedCallPosition, currentCallPosition, None, loc) ++
          args.flatMap(visit(_, NonTailPosition))

      case Expr.GetField(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.PutField(_, exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.GetStaticField(_, _, _, _) =>
        Nil

      case Expr.PutStaticField(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.NewObject(_, clazz, tpe, _, methods, loc) =>
        val erasedType = Type.eraseAliases(tpe)
        checkObjectImplementation(clazz, erasedType, methods, loc) ++
          methods.flatMap {
            case JvmMethod(_, _, exp, _, _, _) => visit(exp, NonTailPosition)
          }

      case Expr.NewChannel(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.GetChannel(exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.PutChannel(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.SelectChannel(rules, default, _, _, _) =>
        rules.flatMap { case SelectChannelRule(_, chan, body) => visit(chan, NonTailPosition) ++
          visit(body, currentCallPosition)
        } ++
          default.map(visit(_, currentCallPosition)).getOrElse(Nil)

      case Expr.Spawn(exp1, exp2, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.ParYield(frags, exp, _, _, _) =>
        frags.flatMap { case ParYieldFragment(_, e, _) => visit(e, NonTailPosition) } ++ visit(exp, currentCallPosition)

      case Expr.Lazy(exp, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Force(exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.FixpointConstraintSet(cs, _, _, _) =>
        cs.flatMap(checkConstraint(_, renv, root))

      case Expr.FixpointLambda(_, exp, _, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.FixpointMerge(exp1, exp2, _, _, _, _) =>
        visit(exp1, NonTailPosition) ++ visit(exp2, NonTailPosition)

      case Expr.FixpointSolve(exp, _, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.FixpointFilter(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.FixpointInject(exp, _, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.FixpointProject(_, exp, _, _, _) =>
        visit(exp, NonTailPosition)

      case Expr.Error(_, _, _) =>
        Nil

    }

    val visitorErrors = visit(e0, expectedCallPosition)

    val recursiveCallError = expectedCallPosition match {
      case TailPosition(sym) if !containsRecursiveCall =>
        val loc = root.defs(sym).spec.loc
        SafetyError.TailRecursiveFunctionWithoutRecursiveCall(sym, loc) :: Nil
      case _ => Nil
    }

    visitorErrors ++ recursiveCallError

  }

  /**
    * Checks if the given type cast is legal.
    */
  private def verifyCheckedTypeCast(from: Type, to: Type, loc: SourceLocation)(implicit flix: Flix): List[SafetyError] = {
    (from.baseType, to.baseType) match {

      // Allow casting Null to a Java type.
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Native(_), _)) => Nil
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigInt, _)) => Nil
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigDecimal, _)) => Nil
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Str, _)) => Nil
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Regex, _)) => Nil
      case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Array, _)) => Nil

      // Allow casting one Java type to another if there is a sub-type relationship.
      case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(left)) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Similar, but for String.
      case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[String])) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Similar, but for Regex.
      case (Type.Cst(TypeConstructor.Regex, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[java.util.regex.Pattern])) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Similar, but for BigInt.
      case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[BigInteger])) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Similar, but for BigDecimal.
      case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[java.math.BigDecimal])) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Similar, but for Arrays.
      case (Type.Cst(TypeConstructor.Array, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[Array[Object]])) Nil else IllegalCheckedTypeCast(from, to, loc) :: Nil

      // Disallow casting a type variable.
      case (src@Type.Var(_, _), _) =>
        IllegalCastFromVar(src, to, loc) :: Nil

      // Disallow casting a type variable (symmetric case)
      case (_, dst@Type.Var(_, _)) =>
        IllegalCastToVar(from, dst, loc) :: Nil

      // Disallow casting a Java type to any other type.
      case (Type.Cst(TypeConstructor.Native(clazz), _), _) =>
        IllegalCastToNonJava(clazz, to, loc) :: Nil

      // Disallow casting a Java type to any other type (symmetric case).
      case (_, Type.Cst(TypeConstructor.Native(clazz), _)) =>
        IllegalCastFromNonJava(from, clazz, loc) :: Nil

      // Disallow all other casts.
      case _ => IllegalCheckedTypeCast(from, to, loc) :: Nil
    }
  }

  /**
    * Checks if the given effect cast is legal.
    */
  private def verifyCheckedEffectCast(from: Type, to: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): List[SafetyError] = {
    // Effect casts are -- by construction in the Typer -- safe.
    Nil
  }

  /**
    * Checks if there are any impossible casts, i.e. casts that always fail.
    *
    * - No primitive type can be cast to a reference type and vice-versa.
    * - No Bool type can be cast to a non-Bool type  and vice-versa.
    */
  private def verifyUncheckedCast(cast: Expr.UncheckedCast)(implicit flix: Flix): List[SafetyError.ImpossibleCast] = {
    val tpe1 = Type.eraseAliases(cast.exp.tpe).baseType
    val tpe2 = cast.declaredType.map(Type.eraseAliases).map(_.baseType)

    val primitives = {
      Type.Unit :: Type.Bool :: Type.Char ::
        Type.Float32 :: Type.Float64 :: Type.Int8 ::
        Type.Int16 :: Type.Int32 :: Type.Int64 ::
        Type.Str :: Type.Regex :: Type.BigInt :: Type.BigDecimal :: Nil
    }

    (tpe1, tpe2) match {
      // Allow casts where one side is a type variable.
      case (Type.Var(_, _), _) => Nil
      case (_, Some(Type.Var(_, _))) => Nil

      // Allow casts between Java types.
      case (Type.Cst(TypeConstructor.Native(_), _), _) => Nil
      case (_, Some(Type.Cst(TypeConstructor.Native(_), _))) => Nil

      // Disallow casting a Boolean to another primitive type.
      case (Type.Bool, Some(t2)) if primitives.filter(_ != Type.Bool).contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // Disallow casting a Boolean to another primitive type (symmetric case).
      case (t1, Some(Type.Bool)) if primitives.filter(_ != Type.Bool).contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // Disallowing casting a non-primitive type to a primitive type.
      case (t1, Some(t2)) if primitives.contains(t1) && !primitives.contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // Disallowing casting a non-primitive type to a primitive type (symmetric case).
      case (t1, Some(t2)) if primitives.contains(t2) && !primitives.contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      case _ => Nil
    }
  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint, renv: RigidityEnv, root: Root)(implicit flix: Flix): List[CompilationMessage] = {
    //
    // Compute the set of positively defined variable symbols in the constraint.
    //
    val posVars = positivelyDefinedVariables(c0)

    // The variables that are used in a non-fixed lattice position
    val latVars0 = nonFixedLatticeVariablesOf(c0)

    // the variables that are used in a fixed position
    val fixedLatVars0 = fixedLatticeVariablesOf(c0)

    // The variables that are used in lattice position, either fixed or non-fixed.
    val latVars = latVars0 union fixedLatVars0

    // The lattice variables that are always fixed can be used in the head.
    val safeLatVars = fixedLatVars0 -- latVars0

    // The lattice variables that cannot be used relationally in the head.
    val unsafeLatVars = latVars -- safeLatVars

    //
    // Compute the quantified variables in the constraint.
    //
    // A lexically bound variable does not appear in this set and is never free.
    //
    val quantVars = c0.cparams.map(_.sym).toSet

    //
    // Check that all negative atoms only use positively defined variable symbols
    // and that lattice variables are not used in relational position.
    //
    val err1 = c0.body.flatMap(checkBodyPredicate(_, posVars, quantVars, latVars, renv, root))

    //
    // Check that the free relational variables in the head atom are not lattice variables.
    //
    val err2 = checkHeadPredicate(c0.head, unsafeLatVars)

    //
    // Check that patterns in atom body are legal
    //
    val err3 = c0.body.flatMap(s => checkBodyPattern(s))

    err1 ++ err2 ++ err3
  }

  /**
    * Performs safety check on the pattern of an atom body.
    */
  private def checkBodyPattern(p0: Predicate.Body): List[CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, _, _, _, terms, _, loc) =>
      terms.foldLeft[List[SafetyError]](Nil)((acc, term) => term match {
        case Pattern.Var(_, _, _) => acc
        case Pattern.Wild(_, _) => acc
        case Pattern.Cst(_, _, _) => acc
        case _ => UnexpectedPatternInBodyAtom(loc) :: acc
      })
    case _ => Nil
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym], renv: RigidityEnv, root: Root)(implicit flix: Flix): List[CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, den, polarity, _, terms, _, loc) =>
      // check for non-positively bound negative variables.
      val err1 = polarity match {
        case Polarity.Positive => Nil
        case Polarity.Negative =>
          // Compute the free variables in the terms which are *not* bound by the lexical scope.
          val freeVars = terms.flatMap(freeVarsOf).toSet intersect quantVars
          val wildcardNegErrors = visitPats(terms, loc)

          // Check if any free variables are not positively bound.
          val variableNegErrors = ((freeVars -- posVars) map (makeIllegalNonPositivelyBoundVariableError(_, loc))).toList
          wildcardNegErrors ++ variableNegErrors
      }
      // check for relational use of lattice variables. We still look at fixed
      // atoms since latVars (which means that they occur non-fixed) cannot be
      // in another fixed atom.
      val relTerms = den match {
        case Denotation.Relational => terms
        case Denotation.Latticenal => terms.dropRight(1)
      }
      val err2 = relTerms.flatMap(freeVarsOf).filter(latVars.contains).map(
        s => IllegalRelationalUseOfLatticeVariable(s, loc)
      )

      // Combine the messages
      err1 ++ err2

    case Predicate.Body.Functional(outVars, exp, loc) =>
      // check for non-positively in variables (free variables in exp).
      val inVars = freeVars(exp).keySet intersect quantVars
      val err1 = ((inVars -- posVars) map (makeIllegalNonPositivelyBoundVariableError(_, loc))).toList

      err1 ::: visitExp(exp, renv, NonTailPosition, root)

    case Predicate.Body.Guard(exp, _) => visitExp(exp, renv, NonTailPosition, root)

  }

  /**
    * Creates an error for a non-positively bound variable, dependent on `sym.isWild`.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def makeIllegalNonPositivelyBoundVariableError(sym: Symbol.VarSym, loc: SourceLocation): SafetyError =
    if (sym.isWild) IllegalNegativelyBoundWildVariable(sym, loc) else IllegalNonPositivelyBoundVariable(sym, loc)

  /**
    * Returns all the positively defined variable symbols in the given constraint `c0`.
    */
  private def positivelyDefinedVariables(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(positivelyDefinedVariables).toSet

  /**
    * Returns all positively defined variable symbols in the given body predicate `p0`.
    */
  private def positivelyDefinedVariables(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(_, _, polarity, _, terms, _, _) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }

    case Predicate.Body.Functional(_, _, _) =>
      // A functional does not positively bind any variables. Not even its outVars.
      Set.empty

    case Predicate.Body.Guard(_, _) => Set.empty

  }

  /**
    * Computes the free variables that occur in lattice position in
    * atoms that are marked with fix.
    */
  private def fixedLatticeVariablesOf(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(fixedLatticenalVariablesOf).toSet

  /**
    * Computes the lattice variables of `p0` if it is a fixed atom.
    */
  private def fixedLatticenalVariablesOf(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Body.Atom(_, Denotation.Latticenal, _, Fixity.Fixed, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)

    case _ => Set.empty
  }

  /**
    * Computes the free variables that occur in a lattice position in
    * atoms that are not marked with fix.
    */
  private def nonFixedLatticeVariablesOf(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(latticenalVariablesOf).toSet

  /**
    * Computes the lattice variables of `p0` if it is not a fixed atom.
    */
  private def latticenalVariablesOf(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(_, Denotation.Latticenal, _, Fixity.Loose, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)

    case _ => Set.empty
  }

  /**
    * Checks for `IllegalRelationalUseOfLatticeVariable` in the given `head` predicate.
    */
  private def checkHeadPredicate(head: Predicate.Head, latVars: Set[Symbol.VarSym]): List[CompilationMessage] = head match {
    case Predicate.Head.Atom(_, Denotation.Latticenal, terms, _, loc) =>
      // Check the relational terms ("the keys").
      checkTerms(terms.dropRight(1), latVars, loc)
    case Predicate.Head.Atom(_, Denotation.Relational, terms, _, loc) =>
      // Check every term.
      checkTerms(terms, latVars, loc)
  }

  /**
    * Checks that the free variables of the terms does not contain any of the variables in `latVars`.
    * If they do contain a lattice variable then a `IllegalRelationalUseOfLatticeVariable` is created.
    */
  private def checkTerms(terms: List[Expr], latVars: Set[Symbol.VarSym], loc: SourceLocation): List[CompilationMessage] = {
    // Compute the free variables in all terms.
    val allVars = terms.foldLeft(Set.empty[Symbol.VarSym])({
      case (acc, term) => acc ++ freeVars(term).keys
    })

    // Compute the lattice variables that are illegally used in the terms.
    allVars.intersect(latVars).toList.map(sym => IllegalRelationalUseOfLatticeVariable(sym, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards in each term.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def visitPats(terms: List[Pattern], loc: SourceLocation): List[CompilationMessage] = {
    terms.flatMap(visitPat(_, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards.
    *
    * @param loc the location of the atom containing the term.
    */
  @tailrec
  private def visitPat(term: Pattern, loc: SourceLocation): List[CompilationMessage] = term match {
    case Pattern.Wild(_, _) => List(IllegalNegativelyBoundWildcard(loc))
    case Pattern.Var(_, _, _) => Nil
    case Pattern.Cst(_, _, _) => Nil
    case Pattern.Tag(_, pat, _, _) => visitPat(pat, loc)
    case Pattern.Tuple(elms, _, _) => visitPats(elms, loc)
    case Pattern.Record(pats, pat, _, _) => visitRecordPattern(pats, pat, loc)
    case Pattern.RecordEmpty(_, _) => Nil
  }

  /**
    * Helper function for [[visitPat]].
    */
  private def visitRecordPattern(pats: List[Pattern.Record.RecordLabelPattern], pat: Pattern, loc: SourceLocation): List[CompilationMessage] = {
    visitPats(pats.map(_.pat), loc) ++ visitPat(pat, loc)
  }

  /**
    * Ensures that `methods` fully implement `clazz`
    */
  private def checkObjectImplementation(clazz: java.lang.Class[_], tpe: Type, methods: List[JvmMethod], loc: SourceLocation): List[CompilationMessage] = {
    //
    // Check that `clazz` doesn't have a non-default constructor
    //
    val constructorErrors = if (clazz.isInterface) {
      // Case 1: Interface. No need for a constructor.
      List.empty
    } else {
      // Case 2: Class. Must have a non-private zero argument constructor.
      if (hasNonPrivateZeroArgConstructor(clazz))
        List.empty
      else
        List(MissingPublicZeroArgConstructor(clazz, loc))
    }

    //
    // Check that `clazz` is public
    //
    val visibilityErrors = if (!isPublicClass(clazz))
      List(NonPublicClass(clazz, loc))
    else
      List.empty

    //
    // Check that the first argument looks like "this"
    //
    val thisErrors = methods.flatMap {
      case JvmMethod(ident, fparams, _, _, _, methodLoc) =>
        // Check that the declared type of `this` matches the type of the class or interface.
        val fparam = fparams.head
        val thisType = Type.eraseAliases(fparam.tpe)
        thisType match {
          case `tpe` => None
          case Type.Unit => Some(MissingThis(clazz, ident.name, methodLoc))
          case _ => Some(IllegalThisType(clazz, fparam.tpe, ident.name, methodLoc))
        }
    }

    val flixMethods = getFlixMethodSignatures(methods)
    val implemented = flixMethods.keySet

    val javaMethods = getJavaMethodSignatures(clazz)
    val objectMethods = getJavaMethodSignatures(classOf[Object]).keySet
    val canImplement = javaMethods.keySet
    val mustImplement = canImplement.filter(m => isAbstractMethod(javaMethods(m)) && !objectMethods.contains(m))

    //
    // Check that there are no unimplemented methods.
    //
    val unimplemented = mustImplement diff implemented
    val unimplementedErrors = unimplemented.map(m => UnimplementedMethod(clazz, javaMethods(m), loc))

    //
    // Check that there are no methods that aren't in the interface
    //
    val extra = implemented diff canImplement
    val extraErrors = extra.map(m => ExtraMethod(clazz, m.name, flixMethods(m).loc))

    constructorErrors ++ visibilityErrors ++ thisErrors ++ unimplementedErrors ++ extraErrors
  }

  /**
    * Represents the signature of a method, used to compare Java signatures against Flix signatures.
    */
  private case class MethodSignature(name: String, paramTypes: List[Type], retTpe: Type)

  /**
    * Convert a list of Flix methods to a set of MethodSignatures. Returns a map to allow subsequent reverse lookup.
    */
  private def getFlixMethodSignatures(methods: List[JvmMethod]): Map[MethodSignature, JvmMethod] = {
    methods.foldLeft(Map.empty[MethodSignature, JvmMethod]) {
      case (acc, m@JvmMethod(ident, fparams, _, retTpe, _, _)) =>
        // Drop the first formal parameter (which always represents `this`)
        val paramTypes = fparams.tail.map(_.tpe)
        val signature = MethodSignature(ident.name, paramTypes.map(t => Type.eraseAliases(t)), Type.eraseAliases(retTpe))
        acc + (signature -> m)
    }
  }

  /**
    * Get a Set of MethodSignatures representing the methods of `clazz`. Returns a map to allow subsequent reverse lookup.
    */
  private def getJavaMethodSignatures(clazz: java.lang.Class[_]): Map[MethodSignature, java.lang.reflect.Method] = {
    val methods = clazz.getMethods.toList.filterNot(isStaticMethod)
    methods.foldLeft(Map.empty[MethodSignature, java.lang.reflect.Method]) {
      case (acc, m) =>
        val signature = MethodSignature(m.getName, m.getParameterTypes.toList.map(Type.getFlixType), Type.getFlixType(m.getReturnType))
        acc + (signature -> m)
    }
  }

  /**
    * Return true if the given `clazz` has a non-private zero argument constructor.
    */
  private def hasNonPrivateZeroArgConstructor(clazz: java.lang.Class[_]): Boolean = {
    try {
      val constructor = clazz.getDeclaredConstructor()
      !java.lang.reflect.Modifier.isPrivate(constructor.getModifiers)
    } catch {
      case _: NoSuchMethodException => false
    }
  }

  /**
    * Returns `true` if the given class `c` is public.
    */
  private def isPublicClass(c: java.lang.Class[_]): Boolean =
    java.lang.reflect.Modifier.isPublic(c.getModifiers)

  /**
    * Return `true` if the given method `m` is abstract.
    */
  private def isAbstractMethod(m: java.lang.reflect.Method): Boolean =
    java.lang.reflect.Modifier.isAbstract(m.getModifiers)

  /**
    * Returns `true` if the given method `m` is static.
    */
  private def isStaticMethod(m: java.lang.reflect.Method): Boolean =
    java.lang.reflect.Modifier.isStatic(m.getModifiers)

}
