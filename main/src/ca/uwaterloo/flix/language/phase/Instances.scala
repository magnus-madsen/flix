/*
 * Copyright 2020 Matthew Lutze
 * Copyright 2025 Chenhao Gao
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
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.SymUse.TraitSymUse
import ca.uwaterloo.flix.language.ast.shared.{Instance, Scope, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{ChangeSet, RigidityEnv, Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugTypedAst
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, TypeConstraint}
import ca.uwaterloo.flix.language.phase.unification.*
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

object Instances {

  // We use top scope everywhere here since we are only looking at declarations.
  private implicit val S: Scope = Scope.Top

  /**
    * Validates instances and traits in the given AST root.
    */
  def run(root: TypedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (TypedAst.Root, List[InstanceError]) = {
    implicit val sctx: SharedContext = SharedContext.mk()
    flix.phaseNew("Instances") {
      val instances = changeSet.updateStaleValueLists(root.instances, oldRoot.instances)(ParOps.parMapValueList2(_)(checkInstancesOfTrait(_, root)))
      val traits = changeSet.updateStaleValues(root.traits, oldRoot.traits)(ParOps.parMapValues(_)(visitTrait))
      (root.copy(instances = instances, traits = traits), sctx.errors.asScala.toList)
    }
  }



  /**
    * Checks that all signatures in `trait0` are used in laws if `trait0` is marked `lawful`.
    */
  private def checkLawApplication(trait0: TypedAst.Trait)(implicit sctx: SharedContext): Unit = trait0 match {
    // Case 1: lawful trait
    case TypedAst.Trait(_, _, mod, _, _, _, _, sigs, laws, _) if mod.isLawful =>
      val usedSigs = laws.foldLeft(Set.empty[Symbol.SigSym]) {
        case (acc, TypedAst.Def(_, _, exp, _)) => acc ++ TypedAstOps.sigSymsOf(exp)
      }
      val unusedSigs = sigs.map(_.sym).toSet -- usedSigs
      unusedSigs.toList.foreach {
        sig => sctx.errors.add(InstanceError.UnlawfulSignature(sig, sig.loc))
      }
    // Case 2: non-lawful trait
    case _ => ()
  }

  /**
    * Performs validations on a single trait.
    */
  private def visitTrait(trait0: TypedAst.Trait)(implicit sctx: SharedContext): TypedAst.Trait = {
    checkLawApplication(trait0)
    trait0
  }

  /**
    * Checks that an instance is not an orphan.
    * It is declared in either:
    * * The trait's companion namespace.
    * * The same namespace as its type.
    */
  private def checkOrphan(inst: TypedAst.Instance)(implicit sctx: SharedContext, flix: Flix): Unit = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, _, _, _, ns, _) => tpe.typeConstructor match {
      // Case 1: Enum type in the same namespace as the instance: not an orphan
      case Some(TypeConstructor.Enum(enumSym, _)) if enumSym.namespace == ns.idents.map(_.name) => ()
      // Case 2: Any type in the trait namespace: not an orphan
      case _ if trt.sym.namespace == ns.idents.map(_.name) => ()
      // Case 3: Any type outside the trait companion namespace and enum declaration namespace: orphan
      case _ => sctx.errors.add(InstanceError.OrphanInstance(trt.sym, tpe, trt.loc))
    }
  }

  /**
    * Returns a Boolean to indicate if no error is found.
    *
    * Checks that the instance type is simple:
    * * all type variables are unique
    * * all type arguments are variables
    */
  private def checkSimple(inst: TypedAst.Instance)(implicit sctx: SharedContext, flix: Flix): Boolean = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, _, _, _, _, _) => tpe match {
      case _: Type.Cst => true
      case _: Type.Var =>
        sctx.errors.add(InstanceError.ComplexInstance(tpe, trt.sym, trt.loc))
        false
      case _: Type.Apply =>
        var notFound = true
        // ensure that the head is a concrete type
        tpe.typeConstructor match {
          case None =>
            sctx.errors.add(InstanceError.ComplexInstance(tpe, trt.sym, trt.loc))
            notFound = false
          case Some(_) => ()
        }
        tpe.typeArguments.foldLeft(List.empty[Type.Var]) {
          // Case 1: Type variable
          case (seen, tvar: Type.Var) =>
            // Case 1.1 We've seen it already. Error.
            if (seen.contains(tvar)) {
              sctx.errors.add(InstanceError.DuplicateTypeVar(tvar, trt.sym, trt.loc))
              notFound = false
              seen
            }
            // Case 1.2 We haven't seen it before. Add it to the list.
            else
              tvar :: seen
          // Case 2: Non-variable. Error.
          case (seen, _) =>
            sctx.errors.add(InstanceError.ComplexInstance(tpe, trt.sym, trt.loc))
            notFound = false
            seen
        }
        notFound

      case Type.Alias(alias, _, _, _) =>
        sctx.errors.add(InstanceError.IllegalTypeAliasInstance(alias.sym, trt.sym, trt.loc))
        false
      case Type.AssocType(assoc, _, _, loc) =>
        sctx.errors.add(InstanceError.IllegalAssocTypeInstance(assoc.sym, trt.sym, loc))
        false

      case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type in instance declaration", loc)
      case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff in instance declaration", loc)
      case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type in instance declaration", loc)
    }
  }

  /**
    * Checks for overlap of instance types, assuming the instances are of the same trait.
    */
  private def checkOverlap(inst1: TypedAst.Instance, heads: Map[TypeConstructor, TypedAst.Instance])(implicit sctx: SharedContext, flix: Flix): Unit = {
    // Note: We have that Type.Error unifies with any other type, hence we filter such instances here.
    unsafeGetHead(inst1) match {
      case TypeConstructor.Error(_, _) =>
        // Suppress error for Type.Error.
        ()
      case tc => heads.get(tc) match {
        // Case 1: No match. No Error.
        case None =>
          ()
        // Case 2: An instance matching this type exists. Error.
        case Some(inst2) =>
          sctx.errors.add(InstanceError.OverlappingInstances(inst1.trt.sym, inst1.trt.loc, inst2.trt.loc))
          sctx.errors.add(InstanceError.OverlappingInstances(inst1.trt.sym, inst2.trt.loc, inst1.trt.loc))
      }
    }
  }

  /**
    * Checks that every signature in `trt` is implemented in `inst`, and that `inst` does not have any extraneous definitions.
    */
  private def checkSigMatch(inst: TypedAst.Instance, root: TypedAst.Root)(implicit sctx: SharedContext, flix: Flix): Unit = {
    val trt = root.traits(inst.trt.sym)

    // Step 1: check that each signature has an implementation.
    trt.sigs.foreach {
      sig =>
        (inst.defs.find(_.sym.text == sig.sym.name), sig.exp) match {
          // Case 1: there is no definition with the same name, and no default implementation
          case (None, None) => sctx.errors.add(InstanceError.MissingImplementation(sig.sym, inst.trt.loc))
          // Case 2: there is no definition with the same name, but there is a default implementation
          case (None, Some(_)) => ()
          // Case 3: there is an implementation marked override, but no default implementation
          case (Some(defn), None) if defn.spec.mod.isOverride => sctx.errors.add(InstanceError.IllegalOverride(defn.sym, defn.sym.loc))
          // Case 4: there is an overriding implementation, but no override modifier
          case (Some(defn), Some(_)) if !defn.spec.mod.isOverride => sctx.errors.add(InstanceError.UnmarkedOverride(defn.sym, defn.sym.loc))
          // Case 5: there is an implementation with the right modifier
          case (Some(defn), _) =>
            val expectedScheme = Scheme.partiallyInstantiate(sig.spec.declaredScheme, trt.tparam.sym, inst.tpe, defn.sym.loc)(Scope.Top, flix)
            if (Scheme.equal(expectedScheme, defn.spec.declaredScheme, root.traitEnv, root.eqEnv)) {
              // Case 5.1: the schemes match. Success!
              ()
            } else {
              // Case 5.2: the schemes do not match
              sctx.errors.add(InstanceError.MismatchedSignatures(sig.sym, defn.sym.loc, expectedScheme, defn.spec.declaredScheme))
            }
        }
    }
    // Step 2: check that there are no extra definitions
    inst.defs.foreach {
      defn =>
        trt.sigs.find(_.sym.name == defn.sym.text) match {
          case None => sctx.errors.add(InstanceError.ExtraneousDef(defn.sym, inst.trt.sym, defn.sym.loc))
          case _ => ()
        }
    }
  }

  /**
    * Finds an instance of the trait for a given type.
    */
  def findInstanceForType(tpe: Type, trt: Symbol.TraitSym, root: TypedAst.Root)(implicit flix: Flix): Option[(Instance, Substitution)] = {
    val instOpt = root.traitEnv.getInstance(trt, tpe)
    // lazily find the instance whose type unifies and save the substitution
    instOpt.flatMap {
      superInst =>
        ConstraintSolver2.fullyUnify(tpe, superInst.tpe, Scope.Top, RigidityEnv.empty)(root.eqEnv, flix).map {
          case subst => (superInst, subst)
        }
    }
  }

  /**
    * Checks that there is an instance for each super trait of the trait of `inst`,
    * and that the constraints on `inst` entail the constraints on the super instance.
    */
  private def checkSuperInstances(inst: TypedAst.Instance, root: TypedAst.Root)(implicit sctx: SharedContext, flix: Flix): Unit = inst match {
    case TypedAst.Instance(_, _, _, trt, tpe, tconstrs, _, _, _, _) =>
      val superTraits = root.traitEnv.getSuperTraits(trt.sym)
      superTraits.foreach {
        superTrait =>
          // Find the instance of the super trait matching the type of this instance.
          findInstanceForType(tpe, superTrait, root) match {
            case Some((superInst, subst)) =>
              // Case 1: An instance matches. Check that its constraints are entailed by this instance.
              superInst.tconstrs.foreach {
                tconstr =>
                  TraitEnvironment.entail(tconstrs.map(subst.apply), subst(tconstr), root.traitEnv, root.eqEnv) match {
                    case Result.Ok(_) => Nil
                    case Result.Err(errors) => errors.foreach {
                      case TypeConstraint.Trait(sym, tpe, loc) =>
                        sctx.errors.add(InstanceError.MissingTraitConstraint(TraitConstraint(TraitSymUse(sym, loc), tpe, loc), superTrait, trt.loc))
                      case _ =>
                        throw InternalCompilerException("Unexpected type constraint", inst.loc)
                    }
                  }
              }
            case None =>
              // Case 2: No instance matches. Error
              sctx.errors.add(InstanceError.MissingSuperTraitInstance(tpe, trt.sym, superTrait, trt.loc))
          }
      }
  }

  /**
    * Reassembles an instance
    */
  private def checkInstance(inst: TypedAst.Instance, root: TypedAst.Root)(implicit sctx: SharedContext, flix: Flix): Unit = {
    checkSigMatch(inst, root)
    checkOrphan(inst)
    checkSuperInstances(inst, root)
  }

  /**
    * Reassembles a set of instances of the same trait.
    */
  private def checkInstancesOfTrait(insts0: List[TypedAst.Instance], root: TypedAst.Root)(implicit sctx: SharedContext, flix: Flix): List[TypedAst.Instance] = {

    // Instances can be uniquely identified by their heads,
    // due to the non-complexity rule and non-overlap rule.
    // This maps each instance head to its corresponding instance.
    var heads = Map.empty[TypeConstructor, TypedAst.Instance]

    insts0.foreach {
      // check that the instance is on a valid type, suppressing other errors if not
      case inst =>
        if (checkSimple(inst)) {
          checkOverlap(inst, heads)
          checkInstance(inst, root)
          heads += (unsafeGetHead(inst) -> inst)
        }
    }

    insts0
  }

  /**
    * Retrieves the head of a simple instance.
    *
    * The head of an instance `Trait[T[a, b, c]]` is T
    */
  private def unsafeGetHead(inst: TypedAst.Instance): TypeConstructor = {
    inst.tpe.typeConstructor match {
      case Some(tc) => tc
      case None => throw InternalCompilerException("unexpected non-simple type", inst.trt.loc)
    }
  }

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[InstanceError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[InstanceError])

}
