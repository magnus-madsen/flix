/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.{DefSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.TraitEnvironment
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{CofiniteEffSet, InternalCompilerException, ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Processes all entry points of the program.
  *
  * A function is an entry point if:
  *   - It is the main function (called `main` by default, but can configured to an arbitrary name).
  *   - It is a test (annotated with `@Test`).
  *   - It is an exported function (annotated with `@Export`).
  *
  * This phase has these sub-phases:
  *   - Resolve the entrypoint option so that there is no implicit default entry point.
  *   - Check that all entry points have valid signatures, where rules differ from main, tests, and
  *     exports. If an entrypoint does not have a valid signature, its related annotation is
  *     removed to allow further compilation to continue with valid assumptions.
  *   - Replace the existing main function by a new main function that prints the returned value if
  *     its return type is not Unit.
  *   - Compute the set of all entry points and store it in Root.
  */
object EntryPoints {

  private case object ErrorOrMalformed

  // We don't use regions, so we are safe to use the global scope everywhere in this phase.
  private implicit val S: Scope = Scope.Top

  def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = flix.phaseNew("EntryPoints") {
    val (root1, errs1) = resolveMain(root)
    val (root2, errs2) = CheckEntryPoints.run(root1)
    // WrapMain assumes a sensible main, so CheckEntryPoints must run first.
    val (root3, errs3) = WrapMain.run(root2)
    // WrapMain might change main, so FindEntryPoints must be after.
    val root4 = findEntryPoints(root3)
    (root4, errs1 ++ errs2 ++ errs3)
  }

  /**
    * Converts [[TypedAst.Root.mainEntryPoint]] to be explicit instead of implicit and checks that a
    * given entry point exists.
    *
    * In the input, a None entrypoint means to use `main` if it exists.
    * In the output, None means no entrypoint and Some is an entrypoint, guaranteed to be in defs.
    */
  private def resolveMain(root: TypedAst.Root): (TypedAst.Root, List[EntryPointError]) = {
    val defaultMainName = Symbol.mkDefnSym("main")

    root.mainEntryPoint match {
      case None =>
        root.defs.get(defaultMainName) match {
          case None =>
            // No main is given and default does not exist - no main.
            (root, Nil)
          case Some(entryPoint) =>
            // No main is given but default exists - use default.
            (root.copy(mainEntryPoint = Some(entryPoint.sym)), Nil)
        }
      case Some(sym) => root.defs.get(sym) match {
        case Some(_) =>
          // A main is given and it exists - use it.
          (root, Nil)
        case None =>
          // A main is given and it does not exist - no main and give an error.
          (root.copy(mainEntryPoint = None), List(EntryPointError.MainEntryPointNotFound(sym)))
      }
    }
  }

  /**
    * A function is an entry point if:
    *   - It is the main function (called `main` by default, but can configured
    *     to an arbitrary name).
    *   - It is a test (annotated with `@Test`).
    *   - It is an exported function (annotated with `@Export`).
    */
  private def isEntryPoint(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    isMain(defn) || isTest(defn) || isExport(defn)

  /** Returns `true` if `defn` is a test. */
  private def isTest(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isTest

  /** Returns `true` if `defn` is an exported function. */
  private def isExport(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isExport

  /** Returns `true` if `defn` is the main function. */
  private def isMain(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    root.mainEntryPoint.contains(defn.sym)

  /** Returns `true` if `tpe` is equivalent to Unit (via type aliases). */
  @tailrec
  private def isUnitType(tpe: Type): Result[Boolean, ErrorOrMalformed.type] = tpe match {
    case Type.Cst(TypeConstructor.Unit, _) => Result.Ok(true)
    case Type.Cst(_, _) => Result.Ok(false)
    case Type.Apply(_, _, _) => Result.Ok(false)
    case Type.Alias(_, _, tpe, _) => isUnitType(tpe)
    case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
    case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
    case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
    case Type.GetEff(action, tpe, loc) => Result.Err(ErrorOrMalformed)
  }

  private object CheckEntryPoints {

    /**
      * CheckEntryPoints checks that all entry points (main/test/export) have valid signatures.
      *
      * Because of resilience, invalid entry points are not discarded. Its entry point marker is
      * removed (removed as the main function in root or have its annotation removed).
      */
    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      implicit val sctx: SharedContext = SharedContext.mk()
      implicit val r: TypedAst.Root = root

      ParOps.parMapValues(root.defs)(visitDef(_))

      // Remove the entrypoint if it is not valid.
      val root1 = if (sctx.invalidMain.get()) root.copy(mainEntryPoint = None) else root
      val errs = sctx.errors.asScala.toList
      (root1, errs)
    }

    /**
      * Checks `defn` with relevant checks for its entry point kind (main/test/export).
      *
      * Because of resilience, invalid entry points are not discarded. Its entry point marker is
      * removed (removed as the main function in root or have its annotation removed).
      *
      * A function can be main, a test, and exported at the same time.
      */
    private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): TypedAst.Def = {
      // checkMain is different than the other two because the entry point designation exists on
      // root and invalid main functions are communicated via SharedContext.
      if (isMain(defn)) checkMain(defn)
      val defn1 = if (isTest(defn)) visitTest(defn) else defn
      val defn2 = if (isExport(defn)) visitExport(defn1) else defn1
      defn2
    }

    /**
      * Rules for main - it has:
      *   - No type variables.
      *   - One parameter of type Unit.
      *   - An effect that is a subset of the primitive effects.
      *   - Return type Unit or return type `t` where `ToString[t]` exists
      *     (This is split into two cases to allow compilation without the standard library).
      */
    private def checkMain(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
      val errs = checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkUnitArg(defn) ++ checkToStringOrUnitResult(defn) ++ checkPrimitiveEffect(defn)
      }
      if (errs.nonEmpty) {
        // Invalidate main and add errors.
        sctx.invalidMain.set(true)
        errs.foreach(sctx.errors.add)
      }
    }

    /**
      * Rules for tests - a test has:
      *   - No type variables.
      *   - One parameter of type Unit.
      *   - An effect that is a subset of the primitive effects.
      */
    private def visitTest(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkUnitArg(defn) ++ checkPrimitiveEffect(defn)
      }
      if (errs.isEmpty) {
        defn
      } else {
        errs.foreach(sctx.errors.add)
        removeTestAnnotation(defn)
      }
    }

    /** Returns `defn` without a test annotation. */
    private def removeTestAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Test])
          )
        )
      )

    /**
      * Rules for exported functions - an exported function has:
      *   - No type variables.
      *   - An effect that is a subset of the primitive effects.
      *   - Is not in the root namespace.
      *   - Is `pub`.
      *   - Has a name that is valid in Java.
      *   - Has types that are valid in Java (not Flix types like `List[Int32]`).
      */
    private def visitExport(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = (checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkPrimitiveEffect(defn).toList ++ checkJavaTypes(defn)
      }) ++
        checkNonRootNamespace(defn) ++
        checkPub(defn) ++
        checkValidJavaName(defn)
      if (errs.isEmpty) {
        defn
      } else {
        errs.foreach(sctx.errors.add)
        removeExportAnnotation(defn)
      }
    }

    /** Returns `defn` without a test annotation. */
    private def removeExportAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Export])
          )
        )
      )

    /**
      * Returns an error if `defn` has type variables.
      *
      * If a function has no type variables in surface syntax we know that:
      *   - Traits and trait constraints do not occur since their syntax is limited to `Trait[var]`.
      *   - Associated types do not occur since their syntax is limited to `Trait.Assoc[var]`.
      *   - Equality constraints do not occur since their syntax is limited to
      *     `Trait.Assoc[var] ~ type`
      */
    private def checkNoTypeVariables(defn: TypedAst.Def): Option[EntryPointError] = {
      // `tparams` lies sometimes when explicit tparams are given and _ are used.
      val monomorphic = defn.spec.tparams.isEmpty && typesOf(defn).forall(_.typeVars.isEmpty)
      if (monomorphic) None
      else Some(EntryPointError.IllegalEntryPointTypeVariables(defn.sym.loc))
    }

    /** Returns all the types in the signature of `defn`. */
    private def typesOf(defn: TypedAst.Def): List[Type] = {
      defn.spec.fparams.map(_.tpe) ++
        List(defn.spec.retTpe) ++
        List(defn.spec.eff) ++
        defn.spec.tconstrs.map(_.arg) ++
        defn.spec.econstrs.flatMap(ec => List(ec.tpe1, ec.tpe2))
    }

    /** Returns `None` if `defn` has a single parameter of type Unit. Returns an error otherwise. */
    private def checkUnitArg(defn: TypedAst.Def): Option[EntryPointError] = {
      defn.spec.fparams match {
        // One parameter of type Unit - valid.
        case List(arg) =>
          isUnitType(arg.tpe) match {
            case Result.Ok(true) => None
            case Result.Ok(false) =>
              Some(EntryPointError.IllegalRunnableEntryPointArgs(defn.sym.loc))
            case Result.Err(ErrorOrMalformed) =>
              // Do not report an error, since previous phases should have done already.
              None
          }
        // One parameter of a non-Unit type or more than two parameters - invalid.
        case _ :: _ =>
          Some(EntryPointError.IllegalRunnableEntryPointArgs(defn.sym.loc))
        // Zero parameters.
        case Nil => throw InternalCompilerException(s"Unexpected main with zero parameters ('${defn.sym}'", defn.sym.loc)
      }
    }

    /**
      * Returns `None` if `defn` has return type Unit or has a return type with `ToString` defined.
      * Returns an error otherwise.
      *
      * In order to support compilation without the standard library, Unit is checked first even
      * though it has `ToString` defined.
      */
    private def checkToStringOrUnitResult(defn: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): Option[EntryPointError] = {
      val resultType = defn.spec.retTpe
      isUnitType(resultType) match {
        case Result.Ok(true) =>
          None
        case Result.Ok(false) =>
          val unknownTraitSym = new Symbol.TraitSym(Nil, "ToString", SourceLocation.Unknown)
          val traitSym = root.traits.getOrElse(unknownTraitSym, throw InternalCompilerException(s"'$unknownTraitSym' trait not found", defn.sym.loc)).sym
          val constraint = TraitConstraint(TraitSymUse(traitSym, SourceLocation.Unknown), resultType, SourceLocation.Unknown)
          val hasToString = TraitEnvironment.holds(constraint, root.traitEnv, root.eqEnv)
          if (hasToString) None
          else Some(EntryPointError.IllegalMainEntryPointResult(resultType, resultType.loc))
        case Result.Err(ErrorOrMalformed) =>
          // Do not report an error, since previous phases should have done already.
          None
      }
    }

    /**
      * Returns `None` if `defn` has an effect that is a subset of the set of primitive effects
      * (e.g. `IO + Console`). Returns an error otherwise.
      */
    private def checkPrimitiveEffect(defn: TypedAst.Def)(implicit flix: Flix): Option[EntryPointError] = {
      val eff = defn.spec.eff
      isPrimitiveEffect(eff) match {
        case Result.Ok(true) =>
          None
        case Result.Ok(false) =>
          Some(EntryPointError.IllegalEntryPointEffect(eff, eff.loc))
        case Result.Err(ErrorOrMalformed) =>
          // Do not report an error, since previous phases should have done already.
          None
      }
    }

    /**
      * Returns `true` if `eff` is a subset of the primitive effects.
      *
      * Returns `false` for all effects with either type variables or Error.
      */
    private def isPrimitiveEffect(eff: Type): Result[Boolean, ErrorOrMalformed.type] = {
      eval(eff).map {
        case s =>
          // Check that it is a set is a subset of the primitive effects.
          // s ⊆ prim <=> s - prim = {}
          val primEffs = CofiniteEffSet.mkSet(Symbol.PrimitiveEffs)
          CofiniteEffSet.difference(s, primEffs).isEmpty
      }
    }

    /**
      * Evaluates `eff` if it is well-formed and has no type variables,
      * associated types, or error types.
      */
    private def eval(eff: Type): Result[CofiniteEffSet, ErrorOrMalformed.type] = eff match {
      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Pure => Result.Ok(CofiniteEffSet.empty)
        case TypeConstructor.Univ => Result.Ok(CofiniteEffSet.universe)
        case TypeConstructor.Effect(sym) => Result.Ok(CofiniteEffSet.mkSet(sym))
        case _ => Result.Err(ErrorOrMalformed)
      }
      case Type.Apply(Type.Cst(TypeConstructor.Complement, _), x0, _) =>
        Result.mapN(eval(x0)) {
          case x => CofiniteEffSet.complement(x)
        }
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x0, _), y0, _) =>
        Result.mapN(eval(x0), eval(y0)) {
          case (x, y) => CofiniteEffSet.union(x, y)
        }
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x0, _), y0, _) =>
        Result.mapN(eval(x0), eval(y0)) {
          case (x, y) => CofiniteEffSet.intersection(x, y)
        }
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x0, _), y0, _) =>
        Result.mapN(eval(x0), eval(y0)) {
          case (x, y) => CofiniteEffSet.difference(x, y)
        }
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x0, _), y0, _) =>
        Result.mapN(eval(x0), eval(y0)) {
          case (x, y) => CofiniteEffSet.xor(x, y)
        }
      case Type.Alias(_, _, tpe, _) => eval(tpe)
      case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
      case Type.Apply(_, _, _) => Result.Err(ErrorOrMalformed)
      case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
      case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
      case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
      case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
      case Type.GetEff(_, tpe, _) => Result.Err(ErrorOrMalformed)
    }

    /** Returns an error if `defn` is in the root namespace. */
    private def checkNonRootNamespace(defn: TypedAst.Def): Option[EntryPointError] = {
      val inRoot = defn.sym.namespace.isEmpty
      if (inRoot) Some(EntryPointError.IllegalExportNamespace(defn.sym.loc))
      else None
    }

    /** Returns an error if `defn` is not a public function. */
    private def checkPub(defn: TypedAst.Def): Option[EntryPointError] = {
      val isPub = defn.spec.mod.isPublic
      if (isPub) None
      else Some(EntryPointError.NonPublicExport(defn.sym.loc))
    }

    /** Returns `None` if `defn` has a name that is valid in Java. Returns an error otherwise. */
    private def checkValidJavaName(defn: TypedAst.Def): Option[EntryPointError] = {
      val validName = defn.sym.name.matches("[a-z][a-zA-Z0-9]*")
      if (validName) None
      else Some(EntryPointError.IllegalExportName(defn.sym.loc))
    }

    /** Returns an error for each type in `defn` that is not valid in Java. */
    private def checkJavaTypes(defn: TypedAst.Def): List[EntryPointError] = {
      val types = defn.spec.retTpe :: defn.spec.fparams.map(_.tpe)
      types.flatMap(tpe => {
        isExportableType(tpe) match {
          case Result.Ok(true) =>
            None
          case Result.Ok(false) =>
            Some(EntryPointError.IllegalExportType(tpe, tpe.loc))
          case Result.Err(ErrorOrMalformed) =>
            // Do not report an error, since previous phases should have done already.
            None
        }
      })
    }

    /**
      * Returns `true` if `tpe` is a valid Java type that can be exported.
      *
      *   - `isExportableType(Int32) = true`
      *   - `isExportableType(Bool) = true`
      *   - `isExportableType(String) = true`
      *   - `isExportableType(List[String]) = false`
      *   - `isExportableType(java.lang.Object) = true`
      */
    @tailrec
    private def isExportableType(tpe: Type): Result[Boolean, ErrorOrMalformed.type] = {
      // TODO: Currently, because of eager erasure, we only allow primitive types and Object.
      tpe match {
        case Type.Cst(TypeConstructor.Bool, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Char, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Float32, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Float64, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int8, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int16, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int32, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int64, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Native(clazz), _) if clazz == classOf[java.lang.Object] => Result.Ok(true)
        case Type.Cst(_, _) => Result.Ok(false)
        case Type.Apply(_, _, _) => Result.Ok(false)
        case Type.Alias(_, _, tpe, _) => isExportableType(tpe)
        case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
        case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
        case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
        case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
        case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
        case Type.GetEff(_, _, _) => Result.Err(ErrorOrMalformed)
      }
    }

    private object SharedContext {
      /** Returns a fresh shared context. */
      def mk(): SharedContext = new SharedContext(
        new ConcurrentLinkedQueue(),
        new AtomicBoolean(false)
      )
    }

    /**
      * A global shared context. Must be thread-safe.
      *
      * @param errors      the [[EntryPointError]]s in the AST, if any.
      * @param invalidMain marks the main entrypoint as invalid.
      */
    private case class SharedContext(errors: ConcurrentLinkedQueue[EntryPointError], invalidMain: AtomicBoolean)

  }

  private object WrapMain {

    /**
      * WrapMain takes the main function (if it exists) and creates a new main function that prints
      * the value of the existing main function (unless it returns unit).
      *
      * From previous phases we know that the main function:
      *   - Exists if root.mainEntryPoint is Some.
      *   - Returns Unit or has a return type with ToString defined
      *   - Has a valid signature (without type variables, etc.)
      */
    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      root.mainEntryPoint.map(root.defs(_)) match {
        case Some(main0: TypedAst.Def) =>
          isUnitType(main0.spec.retTpe) match {
            case Result.Ok(true) =>
              // main doesn't need a wrapper.
              (root, Nil)
            case Result.Ok(false) =>
              // main needs a wrapper.
              val main = mkEntryPoint(main0, root)
              val root1 = root.copy(
                defs = root.defs + (main.sym -> main),
                mainEntryPoint = Some(main.sym)
              )
              (root1, Nil)
            case Result.Err(ErrorOrMalformed) =>
              // Do not report an error, since previous phases should have done already.
              (root, Nil)
          }

        case Some(_) | None =>
          // Main doesn't need a wrapper or no main exists.
          (root, Nil)
      }
    }

    /**
      * Returns a new function that calls the main function and prints its value
      * (`mainFunc` in the example below).
      *
      * {{{
      *   def mainFunc(): tpe \ ef = exp
      *
      *   //
      *   def main$(): Unit \ ef + IO = println(main())
      * }}}
      */
    private def mkEntryPoint(oldEntryPoint: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {
      val argSym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, SourceLocation.Unknown)

      // The new type is `Unit -> Unit \ IO + eff` where `eff` is the effect of the old main.
      val argType = Type.Unit
      val retType = Type.Unit
      val eff = Type.mkUnion(Type.IO, oldEntryPoint.spec.eff, SourceLocation.Unknown)

      val tpe = Type.mkArrowWithEffect(argType, eff, retType, SourceLocation.Unknown)
      val spec = TypedAst.Spec(
        doc = Doc(Nil, SourceLocation.Unknown),
        ann = Annotations.Empty,
        mod = Modifiers.Empty,
        tparams = Nil,
        fparams = List(TypedAst.FormalParam(
          TypedAst.Binder(argSym, argType),
          Modifiers.Empty,
          argType,
          TypeSource.Ascribed,
          SourceLocation.Unknown)
        ),
        declaredScheme = Scheme(Nil, Nil, Nil, tpe),
        retTpe = retType,
        eff = eff,
        tconstrs = Nil,
        econstrs = Nil,
      )

      val mainArrowType = oldEntryPoint.spec.declaredScheme.base
      val mainSym = DefSymUse(oldEntryPoint.sym, SourceLocation.Unknown)
      val mainArgType = Type.Unit
      val mainArg = TypedAst.Expr.Cst(Constant.Unit, mainArgType, SourceLocation.Unknown)
      val mainReturnType = mainArrowType.arrowResultType
      val mainEffect = mainArrowType.arrowEffectType
      // `mainFunc()`
      val mainCall = TypedAst.Expr.ApplyDef(mainSym, List(mainArg), mainArrowType, mainReturnType, mainEffect, SourceLocation.Unknown)

      // `println(mainFunc())`
      val printSym = DefSymUse(root.defs(new Symbol.DefnSym(None, Nil, "println", SourceLocation.Unknown)).sym, SourceLocation.Unknown)
      val printArg = mainCall
      val printArgType = mainCall.tpe
      val printReturnType = Type.Unit
      val printEffect = Type.IO
      val printArrowType = Type.mkArrowWithEffect(printArgType, printEffect, printReturnType, SourceLocation.Unknown)
      val printCallEffect = Type.mkUnion(printEffect, printArg.eff, SourceLocation.Unknown)
      val printCall = TypedAst.Expr.ApplyDef(printSym, List(printArg), printArrowType, printReturnType, printCallEffect, SourceLocation.Unknown)

      val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

      TypedAst.Def(sym, spec, printCall, SourceLocation.Unknown)
    }

  }

  /** Returns a new root where [[TypedAst.Root.entryPoints]] contains all entry points (main/test/export). */
  def findEntryPoints(root: TypedAst.Root): TypedAst.Root = {
    val s = mutable.Set.empty[Symbol.DefnSym]
    for ((sym, defn) <- root.defs if isEntryPoint(defn)(root)) {
      s += sym
    }
    val entryPoints = s.toSet
    root.copy(entryPoints = entryPoints)
  }

}
