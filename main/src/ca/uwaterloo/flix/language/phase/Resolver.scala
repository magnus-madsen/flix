/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, VarText}
import ca.uwaterloo.flix.language.ast.NamedAst.{Declaration, RestrictableChoosePattern}
import ca.uwaterloo.flix.language.ast.ResolvedAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.UnkindedType.*
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{NamedAst, Symbol, *}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.ResolutionError.*
import ca.uwaterloo.flix.language.errors.{Recoverable, ResolutionError, Unrecoverable}
import ca.uwaterloo.flix.language.phase.jvm.JvmOps
import ca.uwaterloo.flix.language.phase.typer.TypeReduction
import ca.uwaterloo.flix.util.Validation.*
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.{Chain, ListMap, MapOps}

import java.lang.reflect.{Constructor, Field, Method, Modifier}
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
  * The Resolver phase performs name resolution on the program.
  */
object Resolver {

  /**
    * Java classes for primitives and Object
    */
  private val Int = classOf[Int]
  private val Long = classOf[Long]
  private val Double = classOf[Double]
  private val Boolean = classOf[Boolean]
  private val Object = classOf[AnyRef]

  /**
    * The set of cases that are used by default in the namespace.
    */
  private val DefaultCases = Map(
    "Nil" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "List", SourceLocation.Unknown), "Nil", SourceLocation.Unknown),
    "Cons" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "List", SourceLocation.Unknown), "Cons", SourceLocation.Unknown),

    "None" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Option", SourceLocation.Unknown), "None", SourceLocation.Unknown),
    "Some" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Option", SourceLocation.Unknown), "Some", SourceLocation.Unknown),

    "Err" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Result", SourceLocation.Unknown), "Err", SourceLocation.Unknown),
    "Ok" -> new Symbol.CaseSym(new Symbol.EnumSym(Nil, "Result", SourceLocation.Unknown), "Ok", SourceLocation.Unknown)
  )

  /**
    * Built-in Kinds.
    */
  private val Kinds = Map(
    "Bool" -> Kind.Bool,
    "Eff" -> Kind.Eff,
    "Type" -> Kind.Star,
    "Region" -> Kind.Eff,
    "RecordRow" -> Kind.RecordRow,
    "SchemaRow" -> Kind.SchemaRow,
    "Predicate" -> Kind.Predicate
  )

  /**
    * Performs name resolution on the given program `root`.
    */
def run(root: NamedAst.Root, oldRoot: ResolvedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[ResolvedAst.Root, ResolutionError] = flix.phase("Resolver") {


    // Get the default uses.
    // Skip over anything we can't find
    // in order to support LibMin/LibNix
    val defaultUses: ListMap[String, Resolution] = ListMap(MapOps.mapValues(DefaultCases) {
      case sym => root.symbols.getOrElse(Name.mkUnlocatedNName(sym.namespace), Map.empty).getOrElse(sym.name, Nil).map(Resolution.Declaration.apply)
    })

    val usesVal = root.uses.map {
      case (ns, uses0) =>
        mapN(traverse(uses0)(visitUseOrImport(_, ns, root))) {
          u => (new Symbol.ModuleSym(ns.parts) -> u)
        }
    }

    // Type aliases must be processed first in order to provide a `taenv` for looking up type alias symbols.
    flatMapN(sequence(usesVal), resolveTypeAliases(defaultUses, root)) {
      case (uses, (taenv, taOrder)) =>

        val unitsVal = ParOps.parTraverse(root.units.values)(visitUnit(_, defaultUses)(taenv, root, flix))
        flatMapN(unitsVal) {
          case units =>
            val table = SymbolTable.traverse(units)(tableUnit)
            val structFieldTraitsVal = traverse(root.structFieldTraits)(resolveTrait(_, ListMap.empty, Name.RootNS)(taenv, root, flix))
            mapN(checkSuperTraitDag(table.traits), structFieldTraitsVal) {
              case ((), structFieldTraitsList) =>
                val structFieldTraits = structFieldTraitsList.map(t => t.sym -> t)
                ResolvedAst.Root(
                  table.traits ++ structFieldTraits,
                  table.instances.m, // TODO NS-REFACTOR use ListMap elsewhere for this too
                  table.defs,
                  table.enums,
                  table.structs,
                  table.restrictableEnums,
                  table.effects,
                  table.typeAliases,
                  uses.toMap,
                  taOrder,
                  root.entryPoint,
                  root.sources,
                  root.names
                )
            }
        }
    }
  }(DebugValidation())

  /**
    * Builds a symbol table from the compilation unit.
    */
  private def tableUnit(unit: ResolvedAst.CompilationUnit): SymbolTable = unit match {
    case ResolvedAst.CompilationUnit(_, decls, _) => SymbolTable.traverse(decls)(tableDecl)
  }

  /**
    * Builds a symbol table from the declaration.
    */
  private def tableDecl(decl: ResolvedAst.Declaration): SymbolTable = decl match {
    case ResolvedAst.Declaration.Namespace(_, _, decls, _) => SymbolTable.traverse(decls)(tableDecl)
    case trt: ResolvedAst.Declaration.Trait => SymbolTable.empty.addTrait(trt)
    case inst: ResolvedAst.Declaration.Instance => SymbolTable.empty.addInstance(inst)
    case defn: ResolvedAst.Declaration.Def => SymbolTable.empty.addDef(defn)
    case enum0: ResolvedAst.Declaration.Enum => SymbolTable.empty.addEnum(enum0)
    case struct: ResolvedAst.Declaration.Struct => SymbolTable.empty.addStruct(struct)
    case enum0: ResolvedAst.Declaration.RestrictableEnum => SymbolTable.empty.addRestrictableEnum(enum0)
    case alias: ResolvedAst.Declaration.TypeAlias => SymbolTable.empty.addTypeAlias(alias)
    case effect: ResolvedAst.Declaration.Effect => SymbolTable.empty.addEffect(effect)
    case ResolvedAst.Declaration.Case(sym, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.RestrictableCase(sym, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.Op(sym, spec) => throw InternalCompilerException(s"Unexpected declaration: $sym", spec.loc)
    case ResolvedAst.Declaration.Sig(sym, spec, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", spec.loc)
    case ResolvedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) => throw InternalCompilerException(s"Unexpected declaration: $sym", sym.loc)
    case ResolvedAst.Declaration.AssocTypeDef(_, _, ident, _, _, _) => throw InternalCompilerException(s"Unexpected declaration: $ident", ident.loc)
  }

  /**
    * Semi-resolves the type aliases in the root.
    */
  private def semiResolveTypeAliases(defaultUses: ListMap[String, Resolution], root: NamedAst.Root)(implicit flix: Flix): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    fold(root.units.values, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (acc, unit) => mapN(semiResolveTypeAliasesInUnit(unit, defaultUses, root)) {
        case aliases => aliases.foldLeft(acc) {
          case (innerAcc, alias) => innerAcc + (alias.sym -> alias)
        }
      }
    }
  }

  /**
    * Semi-resolves the type aliases in the unit.
    */
  private def semiResolveTypeAliasesInUnit(unit: NamedAst.CompilationUnit, defaultUses: ListMap[String, Resolution], root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls, loc) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = appendAllUseEnv(defaultUses, usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, env, Name.RootNS, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, defaultUses, root))
          mapN(aliasesVal, nsVal) {
            case (aliases, ns) => aliases ::: ns.flatten
          }
      }
  }

  /**
    * Semi-resolves the type aliases in the namespace.
    */
  private def semiResolveTypeAliasesInNamespace(ns0: NamedAst.Declaration.Namespace, defaultUses: ListMap[String, Resolution], root: NamedAst.Root)(implicit flix: Flix): Validation[List[ResolvedAst.Declaration.TypeAlias], ResolutionError] = ns0 match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls, loc) =>
      val ns = Name.mkUnlocatedNName(sym.ns)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = appendAllUseEnv(defaultUses, usesAndImports, root)
          val namespaces = decls.collect {
            case ns: NamedAst.Declaration.Namespace => ns
          }
          val aliases0 = decls.collect {
            case alias: NamedAst.Declaration.TypeAlias => alias
          }
          val aliasesVal = traverse(aliases0)(semiResolveTypeAlias(_, env, ns, root))
          val nsVal = traverse(namespaces)(semiResolveTypeAliasesInNamespace(_, defaultUses, root))
          mapN(aliasesVal, nsVal) {
            case (aliases, ns) => aliases ::: ns.flatten
          }
      }
  }

  /**
    * Partially resolves the type alias.
    *
    * Type aliases within the type are given temporary placeholders.
    */
  private def semiResolveTypeAlias(alias: NamedAst.Declaration.TypeAlias, env0: ListMap[String, Resolution], ns: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.TypeAlias, ResolutionError] = alias match {
    case NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams0, tpe0, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, env0, ns, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val env = env0 ++ mkTypeParamEnv(tparams)
          mapN(semiResolveType(tpe0, Wildness.ForbidWild, env, ns, root)) {
            tpe => ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
          }
      }
  }

  /**
    * Resolves the type aliases in the given root.
    *
    * Returns a pair:
    *   - a map of type alias symbols to their AST nodes
    *   - a list of the aliases in a processing order,
    *     such that any alias only depends on those earlier in the list
    */
private def resolveTypeAliases(defaultUses: ListMap[String, Resolution], root: NamedAst.Root)(implicit flix: Flix): Validation[(Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], List[Symbol.TypeAliasSym]), ResolutionError] = {
    flatMapN(semiResolveTypeAliases(defaultUses, root)) {
      case semiResolved =>
        flatMapN(findResolutionOrder(semiResolved.values)) {
          case orderedSyms =>
            val orderedSemiResolved = orderedSyms.map(semiResolved)
            mapN(finishResolveTypeAliases(orderedSemiResolved)) {
              case aliases => (aliases, orderedSyms)
            }
        }
    }
  }

  /**
    * Gets a list of all type aliases used in the partially resolved type tpe0.
    */
private def getAliasUses(tpe0: UnkindedType): List[Symbol.TypeAliasSym] = tpe0 match {
    case _: UnkindedType.Var => Nil
    case UnkindedType.Ascribe(tpe, _, _) => getAliasUses(tpe)
    case UnkindedType.UnappliedAlias(sym, _) => sym :: Nil
    case _: UnkindedType.UnappliedAssocType => Nil
    case _: UnkindedType.Cst => Nil
    case UnkindedType.Apply(tpe1, tpe2, _) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case _: UnkindedType.Arrow => Nil
    case _: UnkindedType.CaseSet => Nil
    case UnkindedType.CaseComplement(tpe, loc) => getAliasUses(tpe)
    case UnkindedType.CaseUnion(tpe1, tpe2, loc) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case UnkindedType.CaseIntersection(tpe1, tpe2, loc) => getAliasUses(tpe1) ::: getAliasUses(tpe2)
    case _: UnkindedType.Enum => Nil
    case _: UnkindedType.Struct => Nil
    case _: UnkindedType.RestrictableEnum => Nil
    case _: UnkindedType.Error => Nil
    case alias: UnkindedType.Alias => throw InternalCompilerException("unexpected applied alias", alias.loc)
    case assoc: UnkindedType.AssocType => throw InternalCompilerException("unexpected applied associated type", assoc.loc)
  }

  /**
    * Create a list of CyclicTypeAliases errors, one for each type alias.
    */
private def mkCycleErrors[T](cycle: List[Symbol.TypeAliasSym]): Validation.HardFailure[T, ResolutionError] = {
    val errors = cycle.map {
      sym => ResolutionError.CyclicTypeAliases(cycle, sym.loc)
    }
    Validation.HardFailure(Chain.from(errors))
  }

  /**
    * Gets the resolution order for the aliases.
    *
    * Any alias only depends on those earlier in the list
    */
private def findResolutionOrder(aliases: Iterable[ResolvedAst.Declaration.TypeAlias]): Validation[List[Symbol.TypeAliasSym], ResolutionError] = {
    val aliasSyms = aliases.map(_.sym)
    val aliasLookup = aliases.map(alias => alias.sym -> alias).toMap
    val getUses = (sym: Symbol.TypeAliasSym) => getAliasUses(aliasLookup(sym).tpe)

    Graph.topologicalSort(aliasSyms, getUses) match {
      case Graph.TopologicalSort.Sorted(sorted) => Validation.success(sorted)
      case Graph.TopologicalSort.Cycle(path) => mkCycleErrors(path)
    }
  }

  /**
    * Finishes the resolution of the given type aliases.
    *
    * Replaces placeholder type alias constructors with the real type aliases.
    *
    * The given aliases must be in resolution order.
    */
  private def finishResolveTypeAliases(aliases0: List[ResolvedAst.Declaration.TypeAlias]): Validation[Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ResolutionError] = {
    Validation.fold(aliases0, Map.empty[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
      case (taenv, ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe0, loc)) =>
        mapN(finishResolveType(tpe0, taenv)) {
          tpe =>
            val alias = ResolvedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
            taenv + (sym -> alias)
        }
    }
  }

  /**
    * Performs name resolution on the compilation unit.
    */
  private def visitUnit(unit: NamedAst.CompilationUnit, defaultUses: ListMap[String, Resolution])(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.CompilationUnit, ResolutionError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, Name.RootNS, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          val env = appendAllUseEnv(defaultUses, usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, env, Name.RootNS, defaultUses))
          mapN(declsVal) {
            case decls => ResolvedAst.CompilationUnit(usesAndImports, decls, loc)
          }
      }
  }

  /**
    * Performs name resolution on the declaration.
    */
  private def visitDecl(decl: NamedAst.Declaration, env0: ListMap[String, Resolution], ns0: Name.NName, defaultUses: ListMap[String, Resolution])(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration, ResolutionError] = decl match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports0, decls0, loc) =>
      // TODO NS-REFACTOR move to helper for consistency
      // use the new namespace
      val ns = Name.mkUnlocatedNName(sym.ns)
      val usesAndImportsVal = traverse(usesAndImports0)(visitUseOrImport(_, ns, root))
      flatMapN(usesAndImportsVal) {
        case usesAndImports =>
          // reset the env
          val env = appendAllUseEnv(defaultUses, usesAndImports, root)
          val declsVal = traverse(decls0)(visitDecl(_, env, ns, defaultUses))
          mapN(declsVal) {
            case decls => ResolvedAst.Declaration.Namespace(sym, usesAndImports, decls, loc)
          }
      }
    case trt@NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc) =>
      resolveTrait(trt, env0, ns0)
    case inst@NamedAst.Declaration.Instance(doc, ann, mod, trt, tparams, tpe, tconstrs, assocs, defs, ns, loc) =>
      resolveInstance(inst, env0, ns0)
    case defn@NamedAst.Declaration.Def(sym, spec, exp) =>
      resolveDef(defn, None, env0)(ns0, taenv, root, flix)
    case enum0@NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) =>
      resolveEnum(enum0, env0, taenv, ns0, root)
    case struct@NamedAst.Declaration.Struct(_, _, _, _, _, _, _, _) =>
      resolveStruct(struct, env0, taenv, ns0, root)
    case enum0@NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc) =>
      resolveRestrictableEnum(enum0, env0, taenv, ns0, root)
    case NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc) =>
      Validation.success(taenv(sym))
    case eff@NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) =>
      resolveEffect(eff, env0, taenv, ns0, root)
    case op@NamedAst.Declaration.Op(sym, spec) => throw InternalCompilerException("unexpected op", sym.loc)
    case NamedAst.Declaration.Sig(sym, spec, exp) => throw InternalCompilerException("unexpected sig", sym.loc)
    case NamedAst.Declaration.Case(sym, tpe, _) => throw InternalCompilerException("unexpected case", sym.loc)
    case NamedAst.Declaration.StructField(_, sym, tpe, _) => throw InternalCompilerException("unexpected struct field", sym.loc)
    case NamedAst.Declaration.RestrictableCase(sym, tpe, _) => throw InternalCompilerException("unexpected case", sym.loc)
    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, args, tpe, loc) => throw InternalCompilerException("unexpected associated type definition", ident.loc)
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, tpe, loc) => throw InternalCompilerException("unexpected associated type signature", sym.loc)
  }

  /**
    * Creates a map from a list of key-(value list) pairs, appending in the case of duplicates.
    */
  private def combine[K, V](list: List[(K, List[V])]): Map[K, List[V]] = {
    list.foldLeft(Map.empty[K, List[V]]) {
      case (acc, (key, value)) => acc + (key -> (value ++ acc.getOrElse(key, Nil)))
    }
  }

  /**
    * Checks that the super traits form a DAG (no cycles).
    */
  private def checkSuperTraitDag(traits: Map[Symbol.TraitSym, ResolvedAst.Declaration.Trait]): Validation[Unit, ResolutionError] = {

    /**
      * Create a list of CyclicTraitHierarchy errors, one for each trait.
      */
    def mkCycleErrors[T](cycle: List[Symbol.TraitSym]): Validation.HardFailure[T, ResolutionError] = {
      val errors = cycle.map {
        sym => ResolutionError.CyclicTraitHierarchy(cycle, sym.loc)
      }
      Validation.HardFailure(Chain.from(errors))
    }

    val traitSyms = traits.values.map(_.sym)
    val getSuperTraits = (trt: Symbol.TraitSym) => traits(trt).superTraits.map(_.head.sym)
    Graph.topologicalSort(traitSyms, getSuperTraits) match {
      case Graph.TopologicalSort.Cycle(path) => mkCycleErrors(path)
      case Graph.TopologicalSort.Sorted(_) => Validation.success(())
    }
  }

  /**
    * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
    */
  private def resolveConstraint(c0: NamedAst.Constraint, env0: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Constraint, ResolutionError] = c0 match {
    case NamedAst.Constraint(cparams0, head0, body0, loc) =>
      val cparams = resolveConstraintParams(cparams0, env0)
      val env = env0 ++ mkConstraintParamEnv(cparams)
      val headVal = resolvePredicateHead(head0, env)
      val bodyVal = traverse(body0)(resolvePredicateBody(_, env))
      mapN(headVal, bodyVal) {
        case (head, body) => ResolvedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Resolves all the traits in the given root.
    */
private def resolveTrait(c0: NamedAst.Declaration.Trait, env0: ListMap[String, Resolution], ns0: Name.NName)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Trait, ResolutionError] = c0 match {
    case NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam0, superTraits0, assocs0, signatures, laws0, loc) =>
      val tparamVal = resolveTypeParam(tparam0, env0, ns0, root)
      flatMapN(tparamVal) {
        case tparam =>
          val env = env0 ++ mkTypeParamEnv(List(tparam))
          // ignore the parameter of the super traits; we don't use it
          val superTraitsVal = traverse(superTraits0)(tconstr => resolveSuperTrait(tconstr, env, taenv, ns0, root))
          val tconstr = ResolvedAst.TraitConstraint(Ast.TraitConstraint.Head(sym, sym.loc), UnkindedType.Var(tparam.sym, tparam.sym.loc), sym.loc)
          val assocsVal = traverse(assocs0)(resolveAssocTypeSig(_, env, taenv, ns0, root))
          val sigsListVal = traverse(signatures)(resolveSig(_, sym, tparam.sym, env)(ns0, taenv, root, flix))
          val lawsVal = traverse(laws0)(resolveDef(_, Some(tconstr), env)(ns0, taenv, root, flix))
          mapN(superTraitsVal, assocsVal, sigsListVal, lawsVal) {
            case (superTraits, assocs, sigsList, laws) =>
              val sigs = sigsList.map(sig => (sig.sym, sig)).toMap
              ResolvedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given instance `i0` in the given namespace `ns0`.
    */
private def resolveInstance(i0: NamedAst.Declaration.Instance, env0: ListMap[String, Resolution], ns0: Name.NName)(implicit taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Instance, ResolutionError] = i0 match {
    case NamedAst.Declaration.Instance(doc, ann, mod, trt0, tparams0, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      // TODO NS-REFACTOR pull tparams all the way through phases
      val tparamsVal = resolveTypeParams(tparams0, env0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val env = env0 ++ mkTypeParamEnv(tparams)
          val traitVal = lookupTraitForImplementation(trt0, env, ns0, root)
          val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
          val tconstrsVal = traverse(tconstrs0)(resolveTraitConstraint(_, env, taenv, ns0, root))
          flatMapN(traitVal, tpeVal, tconstrsVal) {
            case (trt, tpe, tconstrs) =>
              val assocsVal = resolveAssocTypeDefs(assocs0, trt, tpe, env, taenv, ns0, root, loc)
              val tconstr = ResolvedAst.TraitConstraint(Ast.TraitConstraint.Head(trt.sym, trt0.loc), tpe, trt0.loc)
              val defsVal = traverse(defs0)(resolveDef(_, Some(tconstr), env)(ns0, taenv, root, flix))
              mapN(defsVal, assocsVal) {
                case (defs, assocs) =>
                  val traitUse = Ast.TraitSymUse(trt.sym, trt0.loc)
                  ResolvedAst.Declaration.Instance(doc, ann, mod, traitUse, tpe, tconstrs, assocs, defs, Name.mkUnlocatedNName(ns), loc)
              }
          }
      }
  }

  /**
    * Performs name resolution on the given signature `s0` in the given namespace `ns0`.
    */
private def resolveSig(s0: NamedAst.Declaration.Sig, trt: Symbol.TraitSym, traitTvar: Symbol.UnkindedTypeVarSym, env0: ListMap[String, Resolution])(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Sig, ResolutionError] = s0 match {
    case NamedAst.Declaration.Sig(sym, spec0, exp0) =>
      val tconstr = ResolvedAst.TraitConstraint(Ast.TraitConstraint.Head(trt, trt.loc), UnkindedType.Var(traitTvar, traitTvar.loc), trt.loc)
      val specVal = resolveSpec(spec0, Some(tconstr), env0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val env = env0 ++ mkSpecEnv(spec)
          val specCheckVal = checkSigSpec(sym, spec, traitTvar)
          val expVal = traverseOpt(exp0)(resolveExp(_, env)(Scope.Top, ns0, taenv, root, flix))
          mapN(specCheckVal, expVal) {
            case (_, exp) => ResolvedAst.Declaration.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs name resolution on the given definition `d0` in the given namespace `ns0`.
    */
private def resolveDef(d0: NamedAst.Declaration.Def, tconstr: Option[ResolvedAst.TraitConstraint], env0: ListMap[String, Resolution])(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Declaration.Def, ResolutionError] = d0 match {
    case NamedAst.Declaration.Def(sym, spec0, exp0) =>
      flix.subtask(sym.toString, sample = true)

      val specVal = resolveSpec(spec0, tconstr, env0, taenv, ns0, root)
      flatMapN(specVal) {
        case spec =>
          val env = env0 ++ mkSpecEnv(spec)
          val expVal = resolveExp(exp0, env)(Scope.Top, ns0, taenv, root, flix)
          mapN(expVal) {
            case exp => ResolvedAst.Declaration.Def(sym, spec, exp)
          }
      }
  }

  /**
    * Performs name resolution on the given spec `s0` in the given namespace `ns0`.
    */
private def resolveSpec(s0: NamedAst.Spec, tconstr: Option[ResolvedAst.TraitConstraint], env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Spec, ResolutionError] = s0 match {
    case NamedAst.Spec(doc, ann, mod, tparams0, fparams0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, env0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val env1 = env0 ++ mkTypeParamEnv(tparams)
          val fparamsVal = traverse(fparams0)(resolveFormalParam(_, env1, taenv, ns0, root))
          flatMapN(fparamsVal) {
            case fparams =>
              val env = env1 ++ mkFormalParamEnv(fparams)
              val tpeVal = resolveType(tpe0, Wildness.AllowWild, env, taenv, ns0, root)
              val effVal = traverseOpt(eff0)(resolveType(_, Wildness.AllowWild, env, taenv, ns0, root))
              val tconstrsVal = traverse(tconstrs0)(resolveTraitConstraint(_, env, taenv, ns0, root))
              val econstrsVal = traverse(econstrs0)(resolveEqualityConstraint(_, env, taenv, ns0, root))

              mapN(tpeVal, effVal, tconstrsVal, econstrsVal) {
                case (tpe, eff, tconstrs, econstrs) =>
                  // add the inherited type constraint to the the list
                  ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstr.toList ::: tconstrs, econstrs, loc)
              }
          }
      }
  }

  /**
    * Performs name resolution on the given enum `e0` in the given namespace `ns0`.
    */
private def resolveEnum(e0: NamedAst.Declaration.Enum, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Enum, ResolutionError] = e0 match {
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams0, derives0, cases0, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, env0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val env = env0 ++ mkTypeParamEnv(tparams)
          val derivesVal = resolveDerivations(derives0, env, ns0, root)
          val casesVal = traverse(cases0)(resolveCase(_, env, taenv, ns0, root))
          mapN(derivesVal, casesVal) {
            case (derives, cases) =>
              ResolvedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given struct `s0` in the given namespace `ns0`.
    */
private def resolveStruct(s0: NamedAst.Declaration.Struct, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Struct, ResolutionError] = s0 match {
    case NamedAst.Declaration.Struct(doc, ann, mod, sym, tparams0, fields0, _, loc) =>
      val tparamsVal = resolveTypeParams(tparams0, env0, ns0, root)
      flatMapN(tparamsVal) {
        case tparams =>
          val env = env0 ++ mkTypeParamEnv(tparams)
          val fieldsVal = traverse(fields0.zipWithIndex) { case (field, idx) => resolveStructField(s0.sym, idx, field, env, taenv, ns0, root) }
          mapN(fieldsVal) {
            case (fields) =>
              ResolvedAst.Declaration.Struct(doc, ann, mod, sym, tparams, fields, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given restrictable enum `e0` in the given namespace `ns0`.
    */
private def resolveRestrictableEnum(e0: NamedAst.Declaration.RestrictableEnum, env0: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.RestrictableEnum, ResolutionError] = e0 match {
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index0, tparams0, derives0, cases0, loc) =>
      val indexVal = resolveTypeParam(index0, env0, ns0, root)
      val tparamsVal = resolveTypeParams(tparams0, env0, ns0, root)
      flatMapN(indexVal, tparamsVal) {
        case (index, tparams) =>
          val env = env0 ++ mkTypeParamEnv(index :: tparams)
          val derivesVal = resolveDerivations(derives0, env, ns0, root)
          val casesVal = traverse(cases0)(resolveRestrictableCase(_, env, taenv, ns0, root))
          mapN(derivesVal, casesVal) {
            case (derives, cases) =>
              ResolvedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc)
          }
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveCase(caze0: NamedAst.Declaration.Case, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Case, ResolutionError] = caze0 match {
    case NamedAst.Declaration.Case(sym, tpe0, loc) =>
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.Case(sym, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given struct field `field0` in the given namespace `ns0`.
    */
  private def resolveStructField(structSym: Symbol.StructSym, idx: Int, field0: NamedAst.Declaration.StructField, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.StructField, ResolutionError] = field0 match {
    case NamedAst.Declaration.StructField(mod, sym, tpe0, loc) =>
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.StructField(mod, sym, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given case `caze0` in the given namespace `ns0`.
    */
  private def resolveRestrictableCase(caze0: NamedAst.Declaration.RestrictableCase, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.RestrictableCase, ResolutionError] = caze0 match {
    case NamedAst.Declaration.RestrictableCase(sym, tpe0, loc) =>
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      mapN(tpeVal) {
        tpe => ResolvedAst.Declaration.RestrictableCase(sym, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given effect `eff0` in the given namespace `ns0`.
    */
  private def resolveEffect(eff0: NamedAst.Declaration.Effect, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Effect, ResolutionError] = eff0 match {
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops0, loc) =>
      // TODO NS-REFACTOR maybe start a new env
      val opsVal = traverse(ops0)(resolveOp(_, env, taenv, ns0, root))
      mapN(opsVal) {
        case ops => ResolvedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs name resolution on the given effect operation `op0` in the given namespace `ns0`.
    */
  private def resolveOp(op0: NamedAst.Declaration.Op, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.Op, ResolutionError] = op0 match {
    case NamedAst.Declaration.Op(sym, spec0) =>
      val specVal = resolveSpec(spec0, None, env, taenv, ns0, root)
      mapN(specVal) {
        spec => ResolvedAst.Declaration.Op(sym, spec)
      }
  }

  /**
    * Performs name resolution on the given associated type signature `s0` in the given namespace `ns0`.
    */
  private def resolveAssocTypeSig(s0: NamedAst.Declaration.AssocTypeSig, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.AssocTypeSig, ResolutionError] = s0 match {
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam0, kind0, tpe0, loc) =>
      val tparamVal = resolveTypeParam(tparam0, env, ns0, root)
      val kindVal = resolveKind(kind0, env, ns0, root)
      val tpeVal = traverseOpt(tpe0)(resolveType(_, Wildness.ForbidWild, env, taenv, ns0, root))
      mapN(tparamVal, kindVal, tpeVal) {
        case (tparam, kind, tpe) => ResolvedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given associated type definitions `d0` in the given namespace `ns0`.
    * `loc` is the location of the instance symbol for reporting errors.
    */
  private def resolveAssocTypeDefs(d0: List[NamedAst.Declaration.AssocTypeDef], trt: NamedAst.Declaration.Trait, targ: UnkindedType, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation)(implicit flix: Flix): Validation[List[ResolvedAst.Declaration.AssocTypeDef], ResolutionError] = {
    flatMapN(Validation.traverse(d0)(resolveAssocTypeDef(_, trt, env, taenv, ns0, root))) {
      case xs =>
        // Computes a map from associated type symbols to their definitions.
        val m = mutable.Map.empty[Symbol.AssocTypeSym, ResolvedAst.Declaration.AssocTypeDef]

        // We collect [[DuplicateAssocTypeDef]] and [[DuplicateAssocTypeDef]] errors.
        val errors = mutable.ListBuffer.empty[ResolutionError with Unrecoverable]

        // Build the map `m` and check for [[DuplicateAssocTypeDef]].
        for (d@ResolvedAst.Declaration.AssocTypeDef(_, _, use, _, _, loc1) <- xs) {
          val sym = use.sym
          m.get(sym) match {
            case None =>
              m.put(sym, d)
            case Some(otherDecl) =>
              val loc2 = otherDecl.loc
              errors += ResolutionError.DuplicateAssocTypeDef(sym, loc1, loc2)
              errors += ResolutionError.DuplicateAssocTypeDef(sym, loc2, loc1)
          }
        }

        // Check for [[MissingAssocTypeDef]] and recover.
        for (NamedAst.Declaration.AssocTypeSig(_, _, ascSym, _, _, tpe, _) <- trt.assocs) {
          if (!m.contains(ascSym) && tpe.isEmpty) {
            // Missing associated type.
            errors += ResolutionError.MissingAssocTypeDef(ascSym.name, loc)

            // We recover by introducing a dummy associated type definition.
            val doc = Ast.Doc(Nil, loc)
            val mod = Ast.Modifiers.Empty
            val use = Ast.AssocTypeSymUse(ascSym, loc)
            val arg = targ
            val tpe = UnkindedType.Error(loc)
            val ascDef = ResolvedAst.Declaration.AssocTypeDef(doc, mod, use, arg, tpe, loc)
            m.put(ascSym, ascDef)
          }
        }

        // TODO ASSOC-TYPES this should be a soft failure once we know how to handle error types in unification
        // We use `m.values` here because we have eliminated duplicates and introduced missing associated type defs.
        if (errors.isEmpty) {
          Validation.success(m.values.toList)
        } else {
          Validation.HardFailure(Chain.from(errors))
        }
    }
  }

  /**
    * Performs name resolution on the given associated type definition `d0` in the given namespace `ns0`.
    */
  private def resolveAssocTypeDef(d0: NamedAst.Declaration.AssocTypeDef, trt: NamedAst.Declaration.Trait, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.Declaration.AssocTypeDef, ResolutionError] = d0 match {
    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>

      // For now we don't add any tvars from the args. We should have gotten those directly from the instance
      val argVal = resolveType(arg0, Wildness.ForbidWild, env, taenv, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)
      val symVal = trt.assocs.collectFirst {
        case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) if sym.name == ident.name => sym
      } match {
        case None => Validation.toHardFailure(ResolutionError.UndefinedAssocType(Name.QName(Name.RootNS, ident, ident.loc), ident.loc))
        case Some(sym) => Validation.success(sym)
      }
      mapN(symVal, argVal, tpeVal) {
        case (sym, arg, tpe) =>
          val symUse = Ast.AssocTypeSymUse(sym, ident.loc)
          ResolvedAst.Declaration.AssocTypeDef(doc, mod, symUse, arg, tpe, loc)
      }
  }

  /**
    * Checks that the signature spec is legal.
    *
    * A signature spec is legal if it contains the trait's type variable in its formal parameters or return type.
    */
  private def checkSigSpec(sym: Symbol.SigSym, spec0: ResolvedAst.Spec, tvar: Symbol.UnkindedTypeVarSym): Validation[Unit, ResolutionError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, tpe, _, _, _, _) =>
      val tpes = tpe :: fparams.flatMap(_.tpe)
      val tvars = tpes.flatMap(_.definiteTypeVars).to(SortedSet)
      if (tvars.contains(tvar)) {
        Validation.success(())
      } else {
        Validation.toSoftFailure((), ResolutionError.IllegalSignature(sym, sym.loc))
      }
  }

  private def resolveKind(kind0: NamedAst.Kind, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[Kind, ResolutionError] = kind0 match {
    case NamedAst.Kind.Ambiguous(qname, loc) =>
      if (qname.isUnqualified) {
        val name = qname.ident.name
        Kinds.get(name) match {
          case None =>
            lookupRestrictableEnum(qname, env, ns0, root).toHardResult match {
              case Result.Ok(enum0) =>
                Validation.success(Kind.CaseSet(enum0.sym))
              case Result.Err(_) =>
                // We don't know the kind, but we can find the best match.
                val closestMatch = Similarity.closestMatch(name, Kinds)
                Validation.toSoftFailure(closestMatch, ResolutionError.UndefinedKind(qname, ns0, loc))
            }
          case Some(kind) => Validation.success(kind)
        }
      } else {
        lookupRestrictableEnum(qname, env, ns0, root).toHardResult match {
          case Result.Ok(enum0) => Validation.success(Kind.CaseSet(enum0.sym))
          case Result.Err(_) =>
            // We don't know the kind, so default to Star.
            Validation.toSoftFailure(Kind.Star, ResolutionError.UndefinedKind(qname, ns0, loc))
        }
      }
    case NamedAst.Kind.Arrow(k10, k20, loc) =>
      val k1Val = resolveKind(k10, env, ns0, root)
      val k2Val = resolveKind(k20, env, ns0, root)

      mapN(k1Val, k2Val) {
        case (k1, k2) => Kind.Arrow(k1, k2)
      }
  }

  /**
    * Performs name resolution on the given expression `exp0` in the namespace `ns0`.
    */
private def resolveExp(exp0: NamedAst.Expr, env0: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = exp0 match {
    case NamedAst.Expr.Ambiguous(qname, loc) =>
      // Special Case: We must check if we have a static field access, e.g. Math.PI
      if (qname.namespace.idents.length == 1) {
        // We may have an imported class name.
        val className = qname.namespace.idents.head
        env0.get(className.name) match {
          case Some(List(Resolution.JavaClass(clazz))) =>
            // We have a static field access.
            val fieldName = qname.ident
            try {
              val field = clazz.getField(fieldName.name)
              // Returns out of resolveExp
              return Validation.success(ResolvedAst.Expr.GetStaticField(field, loc))
            } catch {
              case _: NoSuchFieldException =>
                val m = ResolutionError.UndefinedJvmStaticField(clazz, fieldName, loc)
                // Returns out of resolveExp
                return Validation.toSoftFailure(ResolvedAst.Expr.Error(m), m)
            }
          case _ =>
          // Fallthrough to below.
        }
      }

      mapN(lookupQName(qname, env0, ns0, root)) {
        case ResolvedQName.Def(defn) => visitDef(defn, loc)
        case ResolvedQName.Sig(sig) => visitSig(sig, loc)
        case ResolvedQName.Var(sym) => ResolvedAst.Expr.Var(sym, loc)
        case ResolvedQName.Tag(caze) => visitTag(caze, loc)
        case ResolvedQName.RestrictableTag(caze) => visitRestrictableTag(caze, isOpen = false, loc)
        case ResolvedQName.Error(e) => ResolvedAst.Expr.Error(e)
      }

    case NamedAst.Expr.Open(name, loc) =>
      mapN(lookupQName(name, env0, ns0, root)) {
        case ResolvedQName.Def(defn) => visitDef(defn, loc)
        case ResolvedQName.Sig(sig) => visitSig(sig, loc)
        case ResolvedQName.Var(sym) => ResolvedAst.Expr.Var(sym, loc)
        case ResolvedQName.Tag(caze) => visitTag(caze, loc)
        case ResolvedQName.RestrictableTag(caze) => visitRestrictableTag(caze, isOpen = true, loc)
        case ResolvedQName.Error(e) => ResolvedAst.Expr.Error(e)
      }

    case NamedAst.Expr.OpenAs(name, exp, loc) =>
      val enumVal = lookupRestrictableEnum(name, env0, ns0, root)
      val eVal = resolveExp(exp, env0)
      mapN(enumVal, eVal) {
        case (enum0, e) => ResolvedAst.Expr.OpenAs(Ast.RestrictableEnumSymUse(enum0.sym, name.loc), e, loc)
      }

    case NamedAst.Expr.Hole(nameOpt, loc) =>
      val sym = nameOpt match {
        case None => Symbol.freshHoleSym(loc)
        case Some(name) => Symbol.mkHoleSym(ns0, name)
      }
      Validation.success(ResolvedAst.Expr.Hole(sym, loc))

    case NamedAst.Expr.HoleWithExp(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.HoleWithExp(e, loc)
      }

    case NamedAst.Expr.Use(use, exp, loc) =>
      // Lookup the used name and add it to the env
      use match {
        case NamedAst.UseOrImport.Use(qname, alias, _) =>
          // TODO NS-REFACTOR allowing relative uses here...
          flatMapN(lookupQualifiedName(qname, env0, ns0, root)) {
            case decls =>
              val env = decls.foldLeft(env0) {
                case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
              }
              mapN(resolveExp(exp, env)) {
                // TODO NS-REFACTOR: multiple uses here
                case e => ResolvedAst.Expr.Use(getSym(decls.head), alias, e, loc)
              }
          }

        case NamedAst.UseOrImport.Import(_, _, loc) => throw InternalCompilerException("unexpected import", loc)
      }

    case NamedAst.Expr.Cst(cst, loc) =>
      Validation.success(ResolvedAst.Expr.Cst(cst, loc))

    case app@NamedAst.Expr.Apply(NamedAst.Expr.Ambiguous(qname, innerLoc), exps, outerLoc) =>
      // Special Case: We must check if we have a static method call, i.e. Math.abs(123)
      if (qname.namespace.idents.length == 1) {
        // We may have an imported class name.
        val className = qname.namespace.idents.head
        env0.get(className.name) match {
          case Some(List(Resolution.JavaClass(clazz))) =>
            // We have a static method call.
            val methodName = qname.ident
            val expsVal = traverse(exps)(resolveExp(_, env0))
            // Check for a single Unit argument
            // Returns out of resolveExp
            return flatMapN(expsVal) {
              case ResolvedAst.Expr.Cst(Ast.Constant.Unit, _) :: Nil =>
                Validation.success(ResolvedAst.Expr.InvokeStaticMethod2(clazz, methodName, Nil, outerLoc))
              case es =>
                Validation.success(ResolvedAst.Expr.InvokeStaticMethod2(clazz, methodName, es, outerLoc))
            }
          case _ =>
          // Fallthrough to below.
        }
      }

      flatMapN(lookupQName(qname, env0, ns0, root)) {
        case ResolvedQName.Def(defn) => visitApplyDef(app, defn, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.Sig(sig) => visitApplySig(app, sig, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.Var(_) => visitApply(app, env0)
        case ResolvedQName.Tag(caze) => visitApplyTag(caze, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.RestrictableTag(caze) => visitApplyRestrictableTag(caze, exps, isOpen = false, env0, innerLoc, outerLoc)
        case ResolvedQName.Error(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)

      }

    case app@NamedAst.Expr.Apply(NamedAst.Expr.Open(qname, innerLoc), exps, outerLoc) =>
      flatMapN(lookupQName(qname, env0, ns0, root)) {
        case ResolvedQName.Def(defn) => visitApplyDef(app, defn, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.Sig(sig) => visitApplySig(app, sig, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.Var(_) => visitApply(app, env0)
        case ResolvedQName.Tag(caze) => visitApplyTag(caze, exps, env0, innerLoc, outerLoc)
        case ResolvedQName.RestrictableTag(caze) => visitApplyRestrictableTag(caze, exps, isOpen = true, env0, innerLoc, outerLoc)
        case ResolvedQName.Error(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case app@NamedAst.Expr.Apply(_, _, _) =>
      visitApply(app, env0)

    case NamedAst.Expr.Lambda(fparam, exp, loc) =>
      val pVal = resolveFormalParam(fparam, env0, taenv, ns0, root)
      flatMapN(pVal) {
        case p =>
          val env = env0 ++ mkFormalParamEnv(List(p))
          val eVal = resolveExp(exp, env)
          mapN(eVal) {
            case e => ResolvedAst.Expr.Lambda(p, e, loc)
          }
      }

    case NamedAst.Expr.Unary(sop, exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Unary(sop, e, loc)
      }

    case NamedAst.Expr.Binary(sop, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Binary(sop, e1, e2, loc)
      }

    case NamedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      val e3Val = resolveExp(exp3, env0)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) => ResolvedAst.Expr.IfThenElse(e1, e2, e3, loc)
      }

    case NamedAst.Expr.Stm(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Stm(e1, e2, loc)
      }

    case NamedAst.Expr.Discard(exp, loc) =>
      mapN(resolveExp(exp, env0)) {
        case e => ResolvedAst.Expr.Discard(e, loc)
      }

    case NamedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val env = env0 ++ mkVarEnv(sym)
      val e2Val = resolveExp(exp2, env)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.Let(sym, mod, e1, e2, loc)
      }

    case NamedAst.Expr.LetRec(sym, ann, mod, exp1, exp2, loc) =>
      val env = env0 ++ mkVarEnv(sym)
      val e1Val = resolveExp(exp1, env)
      val e2Val = resolveExp(exp2, env)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.LetRec(sym, ann, mod, e1, e2, loc)
      }

    case NamedAst.Expr.Region(tpe, loc) =>
      Validation.success(ResolvedAst.Expr.Region(tpe, loc))

    case NamedAst.Expr.Scope(sym, regionVar, exp, loc) =>
      val env = env0 ++ mkVarEnv(sym) ++ mkTypeVarEnv(regionVar)
      // Visit the body in the new scope
      val eVal = resolveExp(exp, env)(scope.enter(regionVar.withKind(Kind.Eff)), ns0, taenv, root, flix)
      mapN(eVal) {
        e => ResolvedAst.Expr.Scope(sym, regionVar, e, loc)
      }

    case NamedAst.Expr.Match(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.MatchRule(pat, guard, body) =>
          val pVal = resolvePattern(pat, env0, ns0, root)
          flatMapN(pVal) {
            case p =>
              val env = env0 ++ mkPatternEnv(p)
              val gVal = traverseOpt(guard)(resolveExp(_, env))
              val bVal = resolveExp(body, env)
              mapN(gVal, bVal) {
                case (g, b) => ResolvedAst.MatchRule(p, g, b)
              }
          }
      }

      val eVal = resolveExp(exp, env0)
      val rsVal = rulesVal
      mapN(eVal, rsVal) {
        case (e, rs) => ResolvedAst.Expr.Match(e, rs, loc)
      }

    case NamedAst.Expr.TypeMatch(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.TypeMatchRule(sym, tpe, body) =>
          val tVal = resolveType(tpe, Wildness.AllowWild, env0, taenv, ns0, root)
          val env = env0 ++ mkVarEnv(sym)
          val bVal = resolveExp(body, env)
          mapN(tVal, bVal) {
            case (t, b) => ResolvedAst.TypeMatchRule(sym, t, b)
          }
      }

      val eVal = resolveExp(exp, env0)
      mapN(eVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.TypeMatch(e, rs, loc)
      }

    case NamedAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      val expVal = resolveExp(exp, env0)
      val rulesVal = traverse(rules) {
        case NamedAst.RestrictableChooseRule(pat0, exp0) =>
          val pVal = pat0 match {
            case NamedAst.RestrictableChoosePattern.Tag(qname, pat, loc) =>
              val tagVal = lookupRestrictableTag(qname, env0, ns0, root)
              val pats = pat.map {
                case NamedAst.RestrictableChoosePattern.Wild(loc) => ResolvedAst.RestrictableChoosePattern.Wild(loc)
                case NamedAst.RestrictableChoosePattern.Var(sym, loc) => ResolvedAst.RestrictableChoosePattern.Var(sym, loc)
              }
              mapN(tagVal) {
                case tag => ResolvedAst.RestrictableChoosePattern.Tag(Ast.RestrictableCaseSymUse(tag.sym, qname.loc), pats, loc)
              }
          }
          val env = pat0 match {
            case RestrictableChoosePattern.Tag(qname, pat, loc) =>
              pat.foldLeft(env0) {
                case (acc, NamedAst.RestrictableChoosePattern.Var(sym, loc)) => acc + (sym.text -> Resolution.Var(sym))
                case (acc, NamedAst.RestrictableChoosePattern.Wild(loc)) => acc
              }
          }

          val eVal = resolveExp(exp0, env)
          flatMapN(pVal, eVal) {
            case (p, e) =>
              Validation.success(ResolvedAst.RestrictableChooseRule(p, e))
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.RestrictableChoose(star, e, rs, loc)
      }

    case NamedAst.Expr.Tuple(elms, loc) =>
      val esVal = traverse(elms)(e => resolveExp(e, env0))
      mapN(esVal) {
        es => ResolvedAst.Expr.Tuple(es, loc)
      }

    case NamedAst.Expr.RecordEmpty(loc) =>
      Validation.success(ResolvedAst.Expr.RecordEmpty(loc))

    case NamedAst.Expr.RecordSelect(base, label, loc) =>
      val bVal = resolveExp(base, env0)
      mapN(bVal) {
        b => ResolvedAst.Expr.RecordSelect(b, label, loc)
      }

    case NamedAst.Expr.RecordExtend(label, value, rest, loc) =>
      val vVal = resolveExp(value, env0)
      val rVal = resolveExp(rest, env0)
      mapN(vVal, rVal) {
        case (v, r) => ResolvedAst.Expr.RecordExtend(label, v, r, loc)
      }

    case NamedAst.Expr.RecordRestrict(label, rest, loc) =>
      val rVal = resolveExp(rest, env0)
      mapN(rVal) {
        r => ResolvedAst.Expr.RecordRestrict(label, r, loc)
      }

    case NamedAst.Expr.ArrayLit(exps, exp, loc) =>
      val expsVal = traverse(exps)(resolveExp(_, env0))
      val expVal = resolveExp(exp, env0)
      mapN(expsVal, expVal) {
        case (es, e) =>
          ResolvedAst.Expr.ArrayLit(es, e, loc)
      }

    case NamedAst.Expr.ArrayNew(exp1, exp2, exp3, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      val e3Val = resolveExp(exp3, env0)
      mapN(e1Val, e2Val, e3Val) {
        case (e1, e2, e3) =>
          ResolvedAst.Expr.ArrayNew(e1, e2, e3, loc)
      }

    case NamedAst.Expr.ArrayLoad(base, index, loc) =>
      val bVal = resolveExp(base, env0)
      val iVal = resolveExp(index, env0)
      mapN(bVal, iVal) {
        case (b, i) => ResolvedAst.Expr.ArrayLoad(b, i, loc)
      }

    case NamedAst.Expr.ArrayStore(base, index, elm, loc) =>
      val bVal = resolveExp(base, env0)
      val iVal = resolveExp(index, env0)
      val eVal = resolveExp(elm, env0)
      mapN(bVal, iVal, eVal) {
        case (b, i, e) => ResolvedAst.Expr.ArrayStore(b, i, e, loc)
      }

    case NamedAst.Expr.ArrayLength(base, loc) =>
      val bVal = resolveExp(base, env0)
      mapN(bVal) {
        b => ResolvedAst.Expr.ArrayLength(b, loc)
      }

    case NamedAst.Expr.StructNew(name, fields, region, loc) =>
      lookupStruct(name, env0, ns0, root) match {

        case Result.Ok(st0) =>
          flatMapN(getStructIfAccessible(st0, ns0, loc)) {
            case st =>
              val fieldsVal = traverse(fields) {
                case (f, e) =>
                  val eVal = resolveExp(e, env0)
                  val (idx, defLoc) = st0.indicesAndLocs.getOrElse(f, (0, SourceLocation.Unknown))
                  val fieldSym = Symbol.mkStructFieldSym(st0.sym, idx, Name.Label(f.name, defLoc))
                  val fieldSymUse = Ast.StructFieldSymUse(fieldSym, f.loc)
                  mapN(eVal) {
                    case e => (fieldSymUse, e)
                  }
              }
              val regionVal = resolveExp(region, env0)
              val structNew = mapN(fieldsVal, regionVal) {
                case (fields, region) =>
                  ResolvedAst.Expr.StructNew(st.sym, fields, region, loc)
              }
              // Potential errors
              val providedFieldNames = fields.map { case (k, _) => Name.Label(k.name, k.loc) }
              val expectedFieldNames = st.fields.map(field => Name.Label(field.sym.name, field.sym.loc))
              val extraFields = providedFieldNames.diff(expectedFieldNames)
              val missingFields = expectedFieldNames.diff(providedFieldNames)

              val extraFieldErrors = extraFields.map(ResolutionError.ExtraStructFieldInNew(st0.sym, _, loc))
              val missingFieldErrors = missingFields.map(ResolutionError.MissingStructFieldInNew(st0.sym, _, loc))
              val errors0 = extraFieldErrors ++ missingFieldErrors
              val errors = if (!errors0.isEmpty) {
                errors0
              } else if (providedFieldNames != expectedFieldNames) {
                List(ResolutionError.IllegalFieldOrderInNew(st.sym, providedFieldNames, expectedFieldNames, loc))
              } else {
                Nil
              }

              structNew.withSoftFailures(errors)
          }
        case Result.Err(e) =>
          Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

      case NamedAst.Expr.StructGet(e, field0, loc) =>
        val eVal = resolveExp(e, env0)
        mapN(eVal) {
          case e => ResolvedAst.Expr.StructGet(e, field0, loc)
        }

      case NamedAst.Expr.StructPut(e1, field0, e2, loc) =>
        val e1Val = resolveExp(e1, env0)
        val e2Val = resolveExp(e2, env0)
        mapN(e1Val, e2Val) {
          case (e1, e2) => ResolvedAst.Expr.StructPut(e1, field0, e2, loc)
        }

    case NamedAst.Expr.VectorLit(exps, loc) =>
      val expsVal = traverse(exps)(resolveExp(_, env0))
      mapN(expsVal) {
        case es => ResolvedAst.Expr.VectorLit(es, loc)
      }

    case NamedAst.Expr.VectorLoad(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.VectorLoad(e1, e2, loc)
      }

    case NamedAst.Expr.VectorLength(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.VectorLength(e, loc)
      }

    case NamedAst.Expr.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expectedTypVal = traverseOpt(expectedType)(resolveType(_, Wildness.AllowWild, env0, taenv, ns0, root))
      val expectedEffVal = traverseOpt(expectedEff)(resolveType(_, Wildness.AllowWild, env0, taenv, ns0, root))

      val eVal = resolveExp(exp, env0)
      mapN(eVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => ResolvedAst.Expr.Ascribe(e, t, f, loc)
      }

    case NamedAst.Expr.InstanceOf(exp, className, loc) =>
      flatMapN(resolveExp(exp, env0)) {
        case e => env0.get(className.name) match {
          case Some(List(Resolution.JavaClass(clazz))) =>
            Validation.success(ResolvedAst.Expr.InstanceOf(e, clazz, loc))
          case _ =>
            val m = ResolutionError.UndefinedJvmClass(className.name, "", loc)
            Validation.toSoftFailure(ResolvedAst.Expr.Error(m), m)
        }
      }

    case NamedAst.Expr.CheckedCast(c, exp, loc) =>
      mapN(resolveExp(exp, env0)) {
        case e => ResolvedAst.Expr.CheckedCast(c, e, loc)
      }

    case NamedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      val declaredTypVal = traverseOpt(declaredType)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
      val declaredEffVal = traverseOpt(declaredEff)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))

      val eVal = resolveExp(exp, env0)
      mapN(eVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => ResolvedAst.Expr.UncheckedCast(e, t, f, loc)
      }

    case NamedAst.Expr.UncheckedMaskingCast(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.UncheckedMaskingCast(e, loc)
      }

    case NamedAst.Expr.TryCatch(exp, rules, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.CatchRule(sym, className, body) =>
          val env = env0 ++ mkVarEnv(sym)
          val clazzVal = lookupJvmClass2(className, env0, sym.loc).toValidation
          val bVal = resolveExp(body, env)
          mapN(clazzVal, bVal) {
            case (clazz, b) => ResolvedAst.CatchRule(sym, clazz, b)
          }
      }

      val eVal = resolveExp(exp, env0)
      mapN(eVal, rulesVal) {
        case (e, rs) => ResolvedAst.Expr.TryCatch(e, rs, loc)
      }

    case NamedAst.Expr.Throw(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        case e => ResolvedAst.Expr.Throw(e, loc)
      }

    case NamedAst.Expr.Without(exp, eff, loc) =>
      lookupEffect(eff, env0, ns0, root) match {
        case Result.Ok(decl) =>
          flatMapN(resolveExp(exp, env0)) {
            case e => mapN(getEffectIfAccessible(decl, ns0, eff.loc)) {
              case decl =>
                val effUse = Ast.EffectSymUse(decl.sym, eff.loc)
                ResolvedAst.Expr.Without(e, effUse, loc)
            }
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.TryWith(exp, eff, rules, loc) =>
      lookupEffect(eff, env0, ns0, root) match {
        case Result.Ok(decl) =>
          flatMapN(getEffectIfAccessible(decl, ns0, eff.loc), resolveExp(exp, env0)) {
            case (f, e) =>
              val effUse = Ast.EffectSymUse(f.sym, eff.loc)
              val rulesVal = traverse(rules) {
                case NamedAst.HandlerRule(ident, fparams, body) =>
                  val opVal = findOpInEffect(ident, f)
                  val fparamsVal = traverse(fparams)(resolveFormalParam(_, env0, taenv, ns0, root))
                  flatMapN(opVal, fparamsVal) {
                    case (o, fp) =>
                      val env = env0 ++ mkFormalParamEnv(fp)
                      // ignore the continuation for checking arity
                      val arityVal = checkOpArity(o, fp.length - 1, ident.loc)
                      val bodyVal = resolveExp(body, env)
                      mapN(arityVal, bodyVal) {
                        case (_, b) =>
                          val opUse = Ast.OpSymUse(o.sym, ident.loc)
                          ResolvedAst.HandlerRule(opUse, fp, b)
                      }
                  }
              }
              mapN(rulesVal) {
                rs => ResolvedAst.Expr.TryWith(e, effUse, rs, loc)
              }
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.Do(op, exps, loc) =>
      val opVal = lookupOp(op, env0, ns0, root)
      val expsVal = traverse(exps)(resolveExp(_, env0))
      flatMapN(opVal, expsVal) {
        case (o, es) =>
          mapN(checkOpArity(o, exps.length, loc)) {
            case _ =>
              val opUse = Ast.OpSymUse(o.sym, op.loc)
              ResolvedAst.Expr.Do(opUse, es, loc)
          }
      }

    case NamedAst.Expr.InvokeConstructor2(className, exps, loc) =>
      val esVal = traverse(exps)(resolveExp(_, env0))
      flatMapN(esVal) {
        es =>
          env0.get(className.name) match {
            case Some(List(Resolution.JavaClass(clazz))) =>
              Validation.success(ResolvedAst.Expr.InvokeConstructor2(clazz, es, loc))
            case _ =>
              val m = ResolutionError.UndefinedJvmClass(className.name, "", loc)
              Validation.toSoftFailure(ResolvedAst.Expr.Error(m), m)
          }
      }

    case NamedAst.Expr.InvokeMethod2(exp, name, exps, loc) =>
      val eVal = resolveExp(exp, env0)
      val esVal = traverse(exps)(resolveExp(_, env0))
      mapN(eVal, esVal) {
        case (e, es) =>
          ResolvedAst.Expr.InvokeMethod2(e, name, es, loc)
      }

    case NamedAst.Expr.GetField2(exp, name, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        case e =>
          ResolvedAst.Expr.GetField2(e, name, loc)
      }

    case NamedAst.Expr.InvokeConstructorOld(className, args, sig, loc) =>
      lookupJvmClass(className, loc) match {
        case Result.Ok(clazz) =>
          val argsVal = traverse(args)(resolveExp(_, env0))
          val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
          flatMapN(sigVal, argsVal) {
            case (sig, as) =>
              flatMapN(lookupSignature(sig, loc)) {
                case ts => lookupJvmConstructor(clazz, ts, loc) match {
                  case Result.Ok(constructor) =>
                    Validation.success(ResolvedAst.Expr.InvokeConstructorOld(constructor, as, loc))
                  case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
                }
              }
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.InvokeMethodOld(className, methodName, exp, args, sig, retTpe, loc) =>
      val expVal = resolveExp(exp, env0)
      val argsVal = traverse(args)(resolveExp(_, env0))
      val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
      val retVal = resolveType(retTpe, Wildness.ForbidWild, env0, taenv, ns0, root)
      val clazzVal = lookupJvmClass(className, loc).toValidation
      flatMapN(sigVal, expVal, argsVal, retVal, clazzVal) {
        case (signature, e, as, ret, clazz) =>
          flatMapN(lookupSignature(signature, loc)) {
            case sig => lookupJvmMethod(clazz, methodName, sig, ret, static = false, loc) match {
              case Result.Ok(method) =>
                Validation.success(ResolvedAst.Expr.InvokeMethodOld(method, clazz, e, as, loc))
              case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
            }
          }
      }

    case NamedAst.Expr.InvokeStaticMethodOld(className, methodName, args, sig, retTpe, loc) =>
      val argsVal = traverse(args)(resolveExp(_, env0))
      val sigVal = traverse(sig)(resolveType(_, Wildness.ForbidWild, env0, taenv, ns0, root))
      val retVal = resolveType(retTpe, Wildness.ForbidWild, env0, taenv, ns0, root)
      val clazzVal = lookupJvmClass(className, loc).toValidation
      flatMapN(sigVal, argsVal, retVal, clazzVal) {
        case (signature, as, ret, clazz) =>
          flatMapN(lookupSignature(signature, loc)) {
            case sig => lookupJvmMethod(clazz, methodName, sig, ret, static = true, loc) match {
              case Result.Ok(method) =>
                Validation.success(ResolvedAst.Expr.InvokeStaticMethodOld(method, as, loc))
              case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
            }
          }
      }

    case NamedAst.Expr.GetFieldOld(className, fieldName, exp, loc) =>
      lookupJvmField(className, fieldName, static = false, loc) match {
        case Result.Ok((clazz, field)) =>
          mapN(resolveExp(exp, env0)) {
            case e => ResolvedAst.Expr.GetFieldOld(field, clazz, e, loc)
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.PutField(className, fieldName, exp1, exp2, loc) =>
      lookupJvmField(className, fieldName, static = false, loc) match {
        case Result.Ok((clazz, field)) =>
          mapN(resolveExp(exp1, env0), resolveExp(exp2, env0)) {
            case (e1, e2) => ResolvedAst.Expr.PutField(field, clazz, e1, e2, loc)
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.GetStaticField(className, fieldName, loc) =>
      lookupJvmField(className, fieldName, static = true, loc) match {
        case Result.Ok((_, field)) =>
          Validation.success(ResolvedAst.Expr.GetStaticField(field, loc))
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.PutStaticField(className, fieldName, exp, loc) =>
      lookupJvmField(className, fieldName, static = true, loc) match {
        case Result.Ok((_, field)) =>
          mapN(resolveExp(exp, env0)) {
            case e => ResolvedAst.Expr.PutStaticField(field, e, loc)
          }
        case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Expr.Error(e), e)
      }

    case NamedAst.Expr.NewObject(name, tpe, methods, loc) =>
      flatMapN(resolveType(tpe, Wildness.ForbidWild, env0, taenv, ns0, root), traverse(methods)(visitJvmMethod(_, env0))) {
        case (t, ms) =>
          //
          // Check that the type is a JVM type (after type alias erasure).
          //
          UnkindedType.eraseAliases(t) match {
            case UnkindedType.Cst(TypeConstructor.Native(clazz), _) =>
              Validation.success(ResolvedAst.Expr.NewObject(name, clazz, ms, loc))
            case _ =>
              val err = ResolutionError.IllegalNonJavaType(t, t.loc)
              Validation.toSoftFailure(ResolvedAst.Expr.Error(err), err)
          }
      }

    case NamedAst.Expr.NewChannel(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.NewChannel(e1, e2, loc)
      }

    case NamedAst.Expr.GetChannel(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.GetChannel(e, loc)
      }

    case NamedAst.Expr.PutChannel(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.PutChannel(e1, e2, loc)
      }

    case NamedAst.Expr.SelectChannel(rules, default, loc) =>
      val rulesVal = traverse(rules) {
        case NamedAst.SelectChannelRule(sym, chan, body) =>
          val cVal = resolveExp(chan, env0)
          val env = env0 ++ mkVarEnv(sym)
          val bVal = resolveExp(body, env)
          mapN(cVal, bVal) {
            case (c, b) => ResolvedAst.SelectChannelRule(sym, c, b)
          }
      }

      val defaultVal = default match {
        case Some(exp) =>
          val eVal = resolveExp(exp, env0)
          mapN(eVal) {
            e => Some(e)
          }
        case None =>
          Validation.success(None)
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => ResolvedAst.Expr.SelectChannel(rs, d, loc)
      }

    case NamedAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) =>
          ResolvedAst.Expr.Spawn(e1, e2, loc)
      }

    case NamedAst.Expr.ParYield(frags, exp, loc) =>
      // mutable env to be updated during traversal
      var finalUenv = env0

      val fragsVal = traverse(frags) {
        case NamedAst.ParYieldFragment(pat, e0, l0) =>
          val pVal = resolvePattern(pat, env0, ns0, root)
          flatMapN(pVal) {
            case p =>
              val patEnv = mkPatternEnv(p)
              val env = env0 ++ patEnv
              finalUenv = finalUenv ++ patEnv
              val e0Val = resolveExp(e0, env)
              mapN(e0Val) {
                case e1 => ResolvedAst.ParYieldFragment(p, e1, l0)
              }
          }
      }

      mapN(fragsVal, resolveExp(exp, finalUenv)) {
        case (fs, e) => ResolvedAst.Expr.ParYield(fs, e, loc)
      }

    case NamedAst.Expr.Lazy(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Lazy(e, loc)
      }

    case NamedAst.Expr.Force(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.Force(e, loc)
      }

    case NamedAst.Expr.FixpointConstraintSet(cs0, loc) =>
      val csVal = traverse(cs0)(resolveConstraint(_, env0))
      mapN(csVal) {
        cs => ResolvedAst.Expr.FixpointConstraintSet(cs, loc)
      }

    case NamedAst.Expr.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(resolvePredicateParam(_, env0))
      val eVal = resolveExp(exp, env0)
      mapN(psVal, eVal) {
        case (ps, e) => ResolvedAst.Expr.FixpointLambda(ps, e, loc)
      }

    case NamedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.FixpointMerge(e1, e2, loc)
      }

    case NamedAst.Expr.FixpointSolve(exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointSolve(e, loc)
      }

    case NamedAst.Expr.FixpointFilter(pred, exp, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointFilter(pred, e, loc)
      }

    case NamedAst.Expr.FixpointInject(exp, pred, loc) =>
      val eVal = resolveExp(exp, env0)
      mapN(eVal) {
        e => ResolvedAst.Expr.FixpointInject(e, pred, loc)
      }

    case NamedAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      val e1Val = resolveExp(exp1, env0)
      val e2Val = resolveExp(exp2, env0)
      mapN(e1Val, e2Val) {
        case (e1, e2) => ResolvedAst.Expr.FixpointProject(pred, e1, e2, loc)
      }

    case NamedAst.Expr.Error(m) =>
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.success(ResolvedAst.Expr.Error(m))

  }

  /**
    * Creates `arity` fresh fparams for use in a curried def or sig application.
    */
private def mkFreshFparams(arity: Int, loc: SourceLocation)(implicit scope: Scope, flix: Flix): List[ResolvedAst.FormalParam] = {
    // Introduce a fresh variable symbol for each argument of the function definition.
    val varSyms = (0 until arity).map(i => Symbol.freshVarSym(Flix.Delimiter + i, BoundBy.FormalParam, loc)).toList

    // Introduce a formal parameter for each variable symbol.
    varSyms.map(sym => ResolvedAst.FormalParam(sym, Ast.Modifiers.Empty, None, loc))
  }

  /**
    * Creates a lambda for use in a curried dif or sig application.
    */
private def mkCurriedLambda(fparams: List[ResolvedAst.FormalParam], baseExp: ResolvedAst.Expr, loc: SourceLocation): ResolvedAst.Expr = {
    val l = loc.asSynthetic

    // The arguments passed to the definition (i.e. the fresh variable symbols).
    val argExps = fparams.map(fparam => ResolvedAst.Expr.Var(fparam.sym, l))

    // The apply expression inside the lambda.
    val applyExp = ResolvedAst.Expr.Apply(baseExp, argExps, l)

    // The curried lambda expressions.
    fparams.foldRight(applyExp: ResolvedAst.Expr) {
      case (fparam, acc) => ResolvedAst.Expr.Lambda(fparam, acc, l)
    }
  }

  /**
    * Curry the def, wrapping it in lambda expressions.
    */
private def visitDef(defn: NamedAst.Declaration.Def, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    // Find the arity of the function definition.
    val arity = defn.spec.fparams.length

    // Create the fresh fparams
    val fparams = mkFreshFparams(arity, loc.asSynthetic)

    // The definition expression.
    val defExp = ResolvedAst.Expr.Def(defn.sym, loc)

    // Create and apply the lambda expressions
    mkCurriedLambda(fparams, defExp, loc.asSynthetic)
  }

  /**
    * Curry the sig, wrapping it in lambda expressions.
    */
private def visitSig(sig: NamedAst.Declaration.Sig, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    // Find the arity of the function definition.
    val arity = sig.spec.fparams.length

    // Create the fresh fparams
    val fparams = mkFreshFparams(arity, loc.asSynthetic)

    // The signature expression.
    val sigExp = ResolvedAst.Expr.Sig(sig.sym, loc)

    // Create and apply the lambda expressions
    mkCurriedLambda(fparams, sigExp, loc.asSynthetic)
  }

  /**
    * Curry the tag, wrapping it in a lambda expression if it is not nullary.
    */
private def visitTag(caze: NamedAst.Declaration.Case, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    // Check if the tag value has Unit type.
    if (isUnitType(caze.tpe)) {
      // Case 1: The tag value has Unit type. Construct the Unit expression.
      val e = ResolvedAst.Expr.Cst(Ast.Constant.Unit, loc)
      ResolvedAst.Expr.Tag(Ast.CaseSymUse(caze.sym, loc), e, loc)
    } else {
      // Case 2: The tag has a non-Unit type. Hence the tag is used as a function.
      // If the tag is `Some` we construct the lambda: x -> Some(x).

      // Construct a fresh symbol for the formal parameter.
      val freshVar = Symbol.freshVarSym("x" + Flix.Delimiter, BoundBy.FormalParam, loc)

      // Construct the formal parameter for the fresh symbol.
      val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, None, loc)

      // Construct a variable expression for the fresh symbol.
      val varExp = ResolvedAst.Expr.Var(freshVar, loc)

      // Construct the tag expression on the fresh symbol expression.
      val tagExp = ResolvedAst.Expr.Tag(Ast.CaseSymUse(caze.sym, loc), varExp, loc)

      // Assemble the lambda expressions.
      ResolvedAst.Expr.Lambda(freshParam, tagExp, loc)
    }
  }

  /**
    * Curry the tag, wrapping it in a lambda expression if it is not nullary.
    */
private def visitRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, isOpen: Boolean, loc: SourceLocation)(implicit scope: Scope, flix: Flix): ResolvedAst.Expr = {
    // Check if the tag value has Unit type.
    if (isUnitType(caze.tpe)) {
      // Case 1: The tag value has Unit type. Construct the Unit expression.
      val e = ResolvedAst.Expr.Cst(Ast.Constant.Unit, loc)
      ResolvedAst.Expr.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, loc), e, isOpen, loc)
    } else {
      // Case 2: The tag has a non-Unit type. Hence the tag is used as a function.
      // If the tag is `Some` we construct the lambda: x -> Some(x).

      // Construct a fresh symbol for the formal parameter.
      val freshVar = Symbol.freshVarSym("x" + Flix.Delimiter, BoundBy.FormalParam, loc)

      // Construct the formal parameter for the fresh symbol.
      val freshParam = ResolvedAst.FormalParam(freshVar, Ast.Modifiers.Empty, None, loc)

      // Construct a variable expression for the fresh symbol.
      val varExp = ResolvedAst.Expr.Var(freshVar, loc)

      // Construct the tag expression on the fresh symbol expression.
      val tagExp = ResolvedAst.Expr.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, loc), varExp, isOpen, loc)

      // Assemble the lambda expressions.
      ResolvedAst.Expr.Lambda(freshParam, tagExp, loc)
    }
  }

  /**
    * Resolve the application expression, performing currying over the subexpressions.
    */
private def visitApply(exp: NamedAst.Expr.Apply, env0: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = exp match {
    case NamedAst.Expr.Apply(exp0, exps0, loc) =>
      val expVal = resolveExp(exp0, env0)
      val expsVal = traverse(exps0)(resolveExp(_, env0))
      mapN(expVal, expsVal) {
        case (e, es) =>
          es.foldLeft(e) {
            case (acc, a) => ResolvedAst.Expr.Apply(acc, List(a), loc.asSynthetic)
          }
      }
  }

  /**
    * Resolve the application expression, applying `defn` to `exps`.
    */
private def visitApplyDef(app: NamedAst.Expr.Apply, defn: NamedAst.Declaration.Def, exps: List[NamedAst.Expr], env0: ListMap[String, Resolution], innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    if (defn.spec.fparams.length == exps.length) {
      // Case 1: Hooray! We can call the function directly.
      val esVal = traverse(exps)(resolveExp(_, env0))
      mapN(esVal) {
        es =>
          val base = ResolvedAst.Expr.Def(defn.sym, innerLoc)
          ResolvedAst.Expr.Apply(base, es, outerLoc)
      }
    } else {
      // Case 2: We have to curry. (See below).
      visitApply(app, env0)
    }
  }

  /**
    * Resolve the application expression, applying `sig` to `exps`.
    */
private def visitApplySig(app: NamedAst.Expr.Apply, sig: NamedAst.Declaration.Sig, exps: List[NamedAst.Expr], env0: ListMap[String, Resolution], innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    if (sig.spec.fparams.length == exps.length) {
      // Case 1: Hooray! We can call the function directly.
      val esVal = traverse(exps)(resolveExp(_, env0))
      mapN(esVal) {
        case es =>
          val base = ResolvedAst.Expr.Sig(sig.sym, innerLoc)
          ResolvedAst.Expr.Apply(base, es, outerLoc)
      }
    } else {
      // Case 2: We have to curry. (See below).
      visitApply(app, env0)
    }
  }

  /**
    * Resolves the tag application.
    */
private def visitApplyTag(caze: NamedAst.Declaration.Case, exps: List[NamedAst.Expr], env0: ListMap[String, Resolution], innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    val esVal = traverse(exps)(resolveExp(_, env0))
    mapN(esVal) {
      // Case 1: one expression. No tuple.
      case e :: Nil =>
        ResolvedAst.Expr.Tag(Ast.CaseSymUse(caze.sym, innerLoc), e, outerLoc)
      // Case 2: multiple expressions. Make them a tuple
      case es =>
        val exp = ResolvedAst.Expr.Tuple(es, outerLoc)
        ResolvedAst.Expr.Tag(Ast.CaseSymUse(caze.sym, innerLoc), exp, outerLoc)
    }
  }

  /**
    * Resolves the tag application.
    */
private def visitApplyRestrictableTag(caze: NamedAst.Declaration.RestrictableCase, exps: List[NamedAst.Expr], isOpen: Boolean, env0: ListMap[String, Resolution], innerLoc: SourceLocation, outerLoc: SourceLocation)(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Expr, ResolutionError] = {
    val esVal = traverse(exps)(resolveExp(_, env0))
    mapN(esVal) {
      // Case 1: one expression. No tuple.
      case e :: Nil =>
        ResolvedAst.Expr.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, innerLoc), e, isOpen, outerLoc)
      // Case 2: multiple expressions. Make them a tuple
      case es =>
        val exp = ResolvedAst.Expr.Tuple(es, outerLoc)
        ResolvedAst.Expr.RestrictableTag(Ast.RestrictableCaseSymUse(caze.sym, innerLoc), exp, isOpen, outerLoc)
    }
  }

  /**
    * Performs name resolution on the given JvmMethod `method` in the namespace `ns0`.
    */
private def visitJvmMethod(method: NamedAst.JvmMethod, env0: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.JvmMethod, ResolutionError] = method match {
    case NamedAst.JvmMethod(ident, fparams, exp, tpe, eff, loc) =>
      val fparamsVal = traverse(fparams)(resolveFormalParam(_, env0, taenv, ns0, root))
      flatMapN(fparamsVal) {
        case fparams =>
          val env = env0 ++ mkFormalParamEnv(fparams)
          val expVal = resolveExp(exp, env)
          val tpeVal = resolveType(tpe, Wildness.ForbidWild, env, taenv, ns0, root)
          val effVal = traverseOpt(eff)(resolveType(_, Wildness.ForbidWild, env, taenv, ns0, root))
          mapN(expVal, tpeVal, effVal) {
            case (e, t, p) => ResolvedAst.JvmMethod(ident, fparams, e, t, p, loc)
          }
      }
  }


  /**
    * Performs name resolution on the given constraint pattern `pat0` in the namespace `ns0`.
    * Constraint patterns do not introduce new variables.
    */
private def resolvePatternInConstraint(pat0: NamedAst.Pattern, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

    def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
      case NamedAst.Pattern.Wild(loc) =>
        Validation.success(ResolvedAst.Pattern.Wild(loc))

      case NamedAst.Pattern.Var(sym0, loc) =>
        // TODO NS-REFACTOR wild patterns should not be counted as vars
        // if the sym is wild then just call the pattern wild
        if (sym0.isWild) {
          Validation.success(ResolvedAst.Pattern.Wild(loc))
        } else {
          env(sym0.text).collectFirst {
            case Resolution.Var(sym) => sym
          } match {
            case Some(sym) =>
              Validation.success(ResolvedAst.Pattern.Var(sym, loc))
            case None => throw InternalCompilerException("unexpected unrecognized sym in constraint pattern", loc)
          }
        }

      case NamedAst.Pattern.Cst(cst, loc) =>
        Validation.success(ResolvedAst.Pattern.Cst(cst, loc))

      case NamedAst.Pattern.Tag(qname, pat, loc) =>
        lookupTag(qname, env, ns0, root) match {
          case Result.Ok(c) => mapN(visit(pat)) {
            case p => ResolvedAst.Pattern.Tag(Ast.CaseSymUse(c.sym, qname.loc), p, loc)
          }
          case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Pattern.Error(loc), e)
        }

      case NamedAst.Pattern.Tuple(elms, loc) =>
        val esVal = traverse(elms)(visit)
        mapN(esVal) {
          es => ResolvedAst.Pattern.Tuple(es, loc)
        }

      case NamedAst.Pattern.Record(pats, pat, loc) =>
        val psVal = traverse(pats) {
          case NamedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
            mapN(visit(pat1)) {
              case p => ResolvedAst.Pattern.Record.RecordLabelPattern(label, p, loc1)
            }
        }
        val pVal = visit(pat)
        mapN(psVal, pVal) {
          case (ps, p) => ResolvedAst.Pattern.Record(ps, p, loc)
        }

      case NamedAst.Pattern.RecordEmpty(loc) =>
        Validation.success(ResolvedAst.Pattern.RecordEmpty(loc))

      case NamedAst.Pattern.Error(loc) =>
        Validation.success(ResolvedAst.Pattern.Error(loc))
    }

    visit(pat0)
  }

  /**
    * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
    */
private def resolvePattern(pat0: NamedAst.Pattern, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.Pattern, ResolutionError] = {

    def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
      case NamedAst.Pattern.Wild(loc) =>
        Validation.success(ResolvedAst.Pattern.Wild(loc))

      case NamedAst.Pattern.Var(sym, loc) =>
        Validation.success(ResolvedAst.Pattern.Var(sym, loc))

      case NamedAst.Pattern.Cst(cst, loc) =>
        Validation.success(ResolvedAst.Pattern.Cst(cst, loc))

      case NamedAst.Pattern.Tag(qname, pat, loc) =>
        lookupTag(qname, env, ns0, root) match {
          case Result.Ok(c) => mapN(visit(pat)) {
            case p => ResolvedAst.Pattern.Tag(Ast.CaseSymUse(c.sym, qname.loc), p, loc)
          }
          case Result.Err(e) => Validation.toSoftFailure(ResolvedAst.Pattern.Error(loc), e)
        }

      case NamedAst.Pattern.Tuple(elms, loc) =>
        val esVal = traverse(elms)(visit)
        mapN(esVal) {
          es => ResolvedAst.Pattern.Tuple(es, loc)
        }

      case NamedAst.Pattern.Record(pats, pat, loc) =>
        val psVal = traverse(pats) {
          case NamedAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
            mapN(visit(pat1)) {
              case p => ResolvedAst.Pattern.Record.RecordLabelPattern(label, p, loc1)
            }
        }
        val pVal = visit(pat)
        mapN(psVal, pVal) {
          case (ps, p) => ResolvedAst.Pattern.Record(ps, p, loc)
        }

      case NamedAst.Pattern.RecordEmpty(loc) =>
        Validation.success(ResolvedAst.Pattern.RecordEmpty(loc))

      case NamedAst.Pattern.Error(loc) =>
        Validation.success(ResolvedAst.Pattern.Error(loc))
    }

    visit(pat0)
  }

  /**
    * Performs name resolution on the given head predicate `h0` in the given namespace `ns0`.
    */
private def resolvePredicateHead(h0: NamedAst.Predicate.Head, env: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
    case NamedAst.Predicate.Head.Atom(pred, den, terms, loc) =>
      val tsVal = traverse(terms)(t => resolveExp(t, env))
      mapN(tsVal) {
        ts => ResolvedAst.Predicate.Head.Atom(pred, den, ts, loc)
      }
  }

  /**
    * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
    */
private def resolvePredicateBody(b0: NamedAst.Predicate.Body, env: ListMap[String, Resolution])(implicit scope: Scope, ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
    case NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val tsVal = traverse(terms)(resolvePatternInConstraint(_, env, ns0, root))
      mapN(tsVal) {
        ts => ResolvedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc)
      }

    case NamedAst.Predicate.Body.Functional(idents, exp, loc) =>
      val outVars = idents.map {
        case ident => env(ident.name).collectFirst {
          case Resolution.Var(sym) => sym
        }.getOrElse(throw InternalCompilerException(s"Unbound variable in functional predicate: '$ident'.", ident.loc))
      }
      val eVal = resolveExp(exp, env)
      mapN(eVal) {
        case e => ResolvedAst.Predicate.Body.Functional(outVars, e, loc)
      }

    case NamedAst.Predicate.Body.Guard(exp, loc) =>
      val eVal = resolveExp(exp, env)
      mapN(eVal) {
        e => ResolvedAst.Predicate.Body.Guard(e, loc)
      }
  }

  /**
    * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
    */
private def resolveFormalParam(fparam0: NamedAst.FormalParam, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.FormalParam, ResolutionError] = {
    val tVal = traverseOpt(fparam0.tpe)(resolveType(_, Wildness.AllowWild, env, taenv, ns0, root))
    mapN(tVal) {
      t => ResolvedAst.FormalParam(fparam0.sym, fparam0.mod, t, fparam0.loc)
    }
  }

  /**
    * Performs name resolution on the given predicate parameter `pparam0` in the given namespace `ns0`.
    */
private def resolvePredicateParam(pparam0: NamedAst.PredicateParam, env: ListMap[String, Resolution])(implicit ns0: Name.NName, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], root: NamedAst.Root, flix: Flix): Validation[ResolvedAst.PredicateParam, ResolutionError] = pparam0 match {
    case NamedAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      Validation.success(ResolvedAst.PredicateParam.PredicateParamUntyped(pred, loc))

    case NamedAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(resolveType(_, Wildness.ForbidWild, env, taenv, ns0, root))) {
        case ts => ResolvedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
      }

  }

  /**
    * Performs name resolution on the given type parameter `tparam0` in the given namespace `ns0`.
    */
private def resolveTypeParam(tparam0: NamedAst.TypeParam, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.TypeParam, ResolutionError] = tparam0 match {
    case tparam: NamedAst.TypeParam.Kinded => resolveKindedTypeParam(tparam, env, ns0, root)
    case tparam: NamedAst.TypeParam.Unkinded => Validation.success(resolveUnkindedTypeParam(tparam))
    case tparam: NamedAst.TypeParam.Implicit => throw InternalCompilerException("unexpected implicit tparam", tparam.loc)
  }

  /**
    * Performs name resolution on the given kinded type parameter `tparam0` in the given namespace `ns0`.
    */
private def resolveKindedTypeParam(tparam0: NamedAst.TypeParam.Kinded, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedAst.TypeParam.Kinded, ResolutionError] = tparam0 match {
    case NamedAst.TypeParam.Kinded(name, tpe, kind0, loc) =>
      val kindVal = resolveKind(kind0, env, ns0, root)
      mapN(kindVal) {
        case kind => ResolvedAst.TypeParam.Kinded(name, tpe, kind, loc)
      }
  }

  /**
    * Performs name resolution on the given unkinded type parameter `tparam0` in the given namespace `ns0`.
    */
private def resolveUnkindedTypeParam(tparam0: NamedAst.TypeParam.Unkinded): ResolvedAst.TypeParam.Unkinded = tparam0 match {
    case NamedAst.TypeParam.Unkinded(name, tpe, loc) => ResolvedAst.TypeParam.Unkinded(name, tpe, loc)
  }

  /**
    * Performs name resolution on the given implicit type parameter `tparam0` in the given namespace `ns0`.
    */
private def resolveImplicitTypeParam(tparam0: NamedAst.TypeParam, env0: ListMap[String, Resolution]): Option[ResolvedAst.TypeParam] = tparam0 match {
    case NamedAst.TypeParam.Implicit(name, tpe, loc) =>
      // Check if the tparam is in the environment
      env0(name.name) collectFirst {
        case Resolution.TypeVar(sym) => sym
      } match {
        // Case 1: Already in the environment, this is not a type parameter.
        case Some(_) => None
        // Case 2: Not in the environment. This is a real type parameter.
        case None => Some(ResolvedAst.TypeParam.Implicit(name, tpe, loc))
      }
    case NamedAst.TypeParam.Kinded(_, _, _, loc) => throw InternalCompilerException("unexpected explicit type parameter", loc)
    case NamedAst.TypeParam.Unkinded(_, _, loc) => throw InternalCompilerException("unexpected explicit type parameter", loc)
  }

  /**
    * Performs name resolution on the given constraint parameter.
    */
private def resolveConstraintParam(cparam0: NamedAst.ConstraintParam, env0: ListMap[String, Resolution]): Option[ResolvedAst.ConstraintParam] = cparam0 match {
    case NamedAst.ConstraintParam(sym, loc) =>
      // Check if the cparam is in the environment
      env0(sym.text) collectFirst {
        case Resolution.Var(sym) => sym
      } match {
        // Case 1: Already in the environment, this is not a constraint parameter.
        case Some(_) => None
        // Case 2: Not in the environment. This is a real constraint parameter.
        case None => Some(ResolvedAst.ConstraintParam(sym, loc))
      }
  }

  /**
    * Performs name resolution on the given type parameters `tparams0`.
    */
private def resolveTypeParams(tparams0: List[NamedAst.TypeParam], env0: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[List[ResolvedAst.TypeParam], ResolutionError] = {
    // Isolate the implicit type params: these are allowed to have redundancies.
    val (impTparams0, expTparams0) = tparams0.partition {
      case _: NamedAst.TypeParam.Implicit => true
      case _: NamedAst.TypeParam.Kinded => false
      case _: NamedAst.TypeParam.Unkinded => false
    }

    val impTparams = impTparams0.flatMap(resolveImplicitTypeParam(_, env0))
    val expTparamsVal = traverse(expTparams0)(resolveTypeParam(_, env0, ns0, root))

    mapN(expTparamsVal) {
      case expTparams =>
        impTparams ++ expTparams
    }
  }

  /**
    * Performs name resolution on the given constraint parameters `cparams0`.
    */
private def resolveConstraintParams(cparams0: List[NamedAst.ConstraintParam], env0: ListMap[String, Resolution]): List[ResolvedAst.ConstraintParam] = {
    cparams0.flatMap(resolveConstraintParam(_, env0))
  }

  /**
    * Performs name resolution on the given type constraint `tconstr0`.
    */
private def resolveTraitConstraint(tconstr0: NamedAst.TraitConstraint, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TraitConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TraitConstraint(trt0, tpe0, loc) =>
      val traitVal = lookupTrait(trt0, env, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)

      mapN(traitVal, tpeVal) {
        case (trt, tpe) =>
          val head = Ast.TraitConstraint.Head(trt.sym, trt0.loc)
          ResolvedAst.TraitConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given equality constraint `econstr0`.
    */
private def resolveEqualityConstraint(tconstr0: NamedAst.EqualityConstraint, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.EqualityConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.EqualityConstraint(qname, tpe1, tpe2, loc) =>
      val assocVal = lookupAssocType(qname, env, ns0, root)

      val t1Val = resolveType(tpe1, Wildness.ForbidWild, env, taenv, ns0, root)
      val t2Val = resolveType(tpe2, Wildness.ForbidWild, env, taenv, ns0, root)

      mapN(assocVal, t1Val, t2Val) {
        case (assoc, t1, t2) =>
          val head = Ast.AssocTypeConstructor(assoc.sym, qname.loc)
          ResolvedAst.EqualityConstraint(head, t1, t2, loc)
      }
  }

  /**
    * Performs name resolution on the given supertrait constraint `tconstr0`.
    */
private def resolveSuperTrait(tconstr0: NamedAst.TraitConstraint, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[ResolvedAst.TraitConstraint, ResolutionError] = tconstr0 match {
    case NamedAst.TraitConstraint(trt0, tpe0, loc) =>
      val traitVal = lookupTraitForImplementation(trt0, env, ns0, root)
      val tpeVal = resolveType(tpe0, Wildness.ForbidWild, env, taenv, ns0, root)

      mapN(traitVal, tpeVal) {
        case (trt, tpe) =>
          val head = Ast.TraitConstraint.Head(trt.sym, trt0.loc)
          ResolvedAst.TraitConstraint(head, tpe, loc)
      }
  }

  /**
    * Performs name resolution on the given derivations `derives0`.
    */
  private def resolveDerivations(derives0: NamedAst.Derivations, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[Ast.Derivations, ResolutionError] = {
    val qnames = derives0.traits
    val derivesVal = Validation.traverse(qnames)(resolveDerivation(_, env, ns0, root))
    flatMapN(derivesVal) {
      derives =>
        // Check for [[DuplicateDerivation]].
        val seen = mutable.Map.empty[Symbol.TraitSym, SourceLocation]
        val errors = mutable.ArrayBuffer.empty[DuplicateDerivation]
        for (Ast.Derivation(traitSym, loc1) <- derives) {
          seen.get(traitSym) match {
            case None =>
              seen.put(traitSym, loc1)
            case Some(loc2) =>
              errors += DuplicateDerivation(traitSym, loc1, loc2)
              errors += DuplicateDerivation(traitSym, loc2, loc1)
          }
        }

        Validation.toSuccessOrSoftFailure(Ast.Derivations(derives, derives0.loc), errors)
    }
  }

  /**
    * Performs name resolution on the given of derivation `derive0`.
    */
private def resolveDerivation(derive0: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[Ast.Derivation, ResolutionError] = {
    val trtVal = lookupTrait(derive0, env, ns0, root)
    mapN(trtVal) {
      trt => Ast.Derivation(trt.sym, derive0.loc)
    }
  }

  /**
    * Finds the trait with the qualified name `qname` in the namespace `ns0`, for the purposes of implementation.
    */
  private def lookupTraitForImplementation(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Trait, ResolutionError] = {
    val traitOpt = tryLookupName(qname, env, ns0, root)
    traitOpt.collectFirst {
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt
    } match {
      case Some(trt) =>
        getTraitAccessibility(trt, ns0) match {
          case TraitAccessibility.Accessible => Validation.success(trt)
          case TraitAccessibility.Sealed => Validation.toSoftFailure(trt, ResolutionError.SealedTrait(trt.sym, ns0, qname.loc))
          case TraitAccessibility.Inaccessible => Validation.toSoftFailure(trt, ResolutionError.InaccessibleTrait(trt.sym, ns0, qname.loc))
        }
      case None => Validation.toHardFailure(ResolutionError.UndefinedTrait(qname, ns0, qname.loc))
    }
  }

  /**
    * Finds the trait with the qualified name `qname` in the namespace `ns0`.
    */
private def lookupTrait(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Trait, ResolutionError] = {
    val traitOpt = tryLookupName(qname, env, ns0, root)
    traitOpt.collectFirst {
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt
    } match {
      case Some(trt) =>
        getTraitAccessibility(trt, ns0) match {
          case TraitAccessibility.Accessible | TraitAccessibility.Sealed => Validation.success(trt)
          case TraitAccessibility.Inaccessible => Validation.toSoftFailure(trt, ResolutionError.InaccessibleTrait(trt.sym, ns0, qname.loc))
        }
      case None => Validation.toHardFailure(ResolutionError.UndefinedTrait(qname, ns0, qname.loc))
    }
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupQName(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[ResolvedQName, ResolutionError] = {
    // first look in the local env
    val resolutions = tryLookupName(qname, env, ns0, root)

    resolutions.collect {
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Def) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Sig) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.Case) => decl
      case decl@Resolution.Declaration(_: NamedAst.Declaration.RestrictableCase) => decl
      case decl@Resolution.Var(_) => decl
    } match {
      case Resolution.Declaration(defn: NamedAst.Declaration.Def) :: _ =>
        if (isDefAccessible(defn, ns0)) {
          Validation.success(ResolvedQName.Def(defn))
        } else {
          Validation.toSoftFailure(ResolvedQName.Def(defn), ResolutionError.InaccessibleDef(defn.sym, ns0, qname.loc))
        }
      case Resolution.Declaration(sig: NamedAst.Declaration.Sig) :: _ =>
        if (isSigAccessible(sig, ns0)) {
          Validation.success(ResolvedQName.Sig(sig))
        } else {
          Validation.toSoftFailure(ResolvedQName.Sig(sig), ResolutionError.InaccessibleSig(sig.sym, ns0, qname.loc))
        }
      //      case Resolution.Declaration(caze1: NamedAst.Declaration.Case) :: Resolution.Declaration(caze2: NamedAst.Declaration.Case) :: _ =>
      //        // Multiple case matches. Error.
      //        ResolutionError.AmbiguousTag(qname.ident.name, ns0, List(caze1.sym.loc, caze2.sym.loc), qname.ident.loc).toFailure
      // TODO NS-REFACTOR overlapping tag check disabled. Revisit?
      case Resolution.Declaration(caze: NamedAst.Declaration.Case) :: _ =>
        Validation.success(ResolvedQName.Tag(caze))
      // TODO NS-REFACTOR check accessibility
      case Resolution.Declaration(caze: NamedAst.Declaration.RestrictableCase) :: Nil =>
        Validation.success(ResolvedQName.RestrictableTag(caze))
      // TODO NS-REFACTOR check accessibility
      case Resolution.Var(sym) :: _ => Validation.success(ResolvedQName.Var(sym))
      case _ =>
        val e = ResolutionError.UndefinedName(qname, ns0, filterToVarEnv(env), isUse = false, qname.loc)
        Validation.toSoftFailure(ResolvedQName.Error(e), e)
    }
  }

  /**
    * Looks up the effect operation with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupOp(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.Op, ResolutionError] = {
    val opOpt = tryLookupName(qname, env, ns0, root)

    opOpt match {
      case Resolution.Declaration(op: NamedAst.Declaration.Op) :: Nil =>
        if (isOpAccessible(op, ns0)) {
          Validation.success(op)
        } else {
          Validation.toSoftFailure(op, ResolutionError.InaccessibleOp(op.sym, ns0, qname.loc))
        }
      case _ => Validation.toHardFailure(ResolutionError.UndefinedOp(qname, qname.loc))
    }
  }

  /**
    * Looks up the effect operation as a member of the given effect.
    */
  private def findOpInEffect(ident: Name.Ident, eff: NamedAst.Declaration.Effect): Validation[NamedAst.Declaration.Op, ResolutionError] = {
    val opOpt = eff.ops.find(o => o.sym.name == ident.name)
    opOpt match {
      case None =>
        val nname = eff.sym.namespace :+ eff.sym.name
        val qname = Name.mkQName(nname, ident.name, SourceLocation.Unknown)
        Validation.toHardFailure(ResolutionError.UndefinedOp(qname, ident.loc))
      case Some(op) =>
        Validation.success(op)
    }
  }

  /**
    * Finds the enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupTag(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Case, ResolutionError.UndefinedTag] = {
    // look up the name
    val matches = tryLookupName(qname, env, ns0, root).collect {
      case Resolution.Declaration(c: NamedAst.Declaration.Case) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => Result.Err(ResolutionError.UndefinedTag(qname.ident.name, ns0, qname.loc))
      // Case 1: A match was found. Success. Note that multiple matches can be found but they are prioritized by tryLookupName so this is fine.
      case caze :: _ => Result.Ok(caze)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the struct that matches the given name `qname` in the namespace `ns0`.
    */
  private def lookupStruct(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Struct, ResolutionError.UndefinedStruct] = {
    val matches = tryLookupName(qname, env, ns0, root).collect {
      case Resolution.Declaration(s: NamedAst.Declaration.Struct) => s
    }
    matches match {
      // Case 0: No matches. Error.
      case Nil => Result.Err(ResolutionError.UndefinedStruct(qname, qname.loc))
      // Case 1: A match was found. Success. Note that multiple matches can be found but they are prioritized by tryLookupName so this is fine.
      case st :: _ => Result.Ok(st)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Finds the restrictable enum case that matches the given qualified name `qname` and `tag` in the namespace `ns0`.
    */
  private def lookupRestrictableTag(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.RestrictableCase, ResolutionError] = {
    // look up the name
    val matches = tryLookupName(qname, env, ns0, root) collect {
      case Resolution.Declaration(c: NamedAst.Declaration.RestrictableCase) => c
    }

    matches match {
      // Case 0: No matches. Error.
      case Nil => Validation.toHardFailure(ResolutionError.UndefinedRestrictableTag(qname.ident.name, ns0, qname.loc))
      // Case 1: Exactly one match. Success.
      case caze :: Nil =>
        Validation.success(caze)
      // Case 2: Multiple matches. Error
      case cazes => throw InternalCompilerException(s"unexpected duplicate tag: ${qname}", qname.loc)
    }
    // TODO NS-REFACTOR check accessibility
  }

  /**
    * Looks up the restrictable enum name.
    */
  private def lookupRestrictableEnum(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.RestrictableEnum, ResolutionError] = {
    val matches = tryLookupName(qname, env, ns0, root) collect {
      case Resolution.Declaration(c: NamedAst.Declaration.RestrictableEnum) => c
    }

    matches match {
      case Nil => Validation.toHardFailure(ResolutionError.UndefinedRestrictableType(qname, ns0, qname.loc))
      case enum0 :: _ => Validation.success(enum0)
    }
  }

  /**
    * Returns `true` iff the given type `tpe0` is the Unit type.
    */
private def isUnitType(tpe: NamedAst.Type): Boolean = tpe match {
    case NamedAst.Type.Unit(loc) => true
    case _ => false
  }

  /**
    * Partially resolves the given type `tpe0` in the given namespace `ns0`.
    *
    * Type aliases are given temporary placeholders.
    */
  private def semiResolveType(tpe0: NamedAst.Type, wildness: Wildness, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType, ResolutionError] = {
    def visit(tpe0: NamedAst.Type): Validation[UnkindedType, ResolutionError] = tpe0 match {
      case NamedAst.Type.Var(ident, loc) =>
        lookupTypeVar(ident, wildness, env) match {
          case Result.Ok(sym) => Validation.success(UnkindedType.Var(sym, loc))
          case Result.Err(e) =>
            // Note: We assume the default type variable has kind Star.
            Validation.toSoftFailure(UnkindedType.Error(loc), e)
        }

      case NamedAst.Type.Unit(loc) => Validation.success(UnkindedType.Cst(TypeConstructor.Unit, loc))

      case NamedAst.Type.Ambiguous(qname, loc) if qname.isUnqualified => qname.ident.name match {
        // Basic Types
        case "Void" => Validation.success(UnkindedType.Cst(TypeConstructor.Void, loc))
        case "Unit" => Validation.success(UnkindedType.Cst(TypeConstructor.Unit, loc))
        case "Null" => Validation.success(UnkindedType.Cst(TypeConstructor.Null, loc))
        case "Bool" => Validation.success(UnkindedType.Cst(TypeConstructor.Bool, loc))
        case "Char" => Validation.success(UnkindedType.Cst(TypeConstructor.Char, loc))
        case "Float32" => Validation.success(UnkindedType.Cst(TypeConstructor.Float32, loc))
        case "Float64" => Validation.success(UnkindedType.Cst(TypeConstructor.Float64, loc))
        case "BigDecimal" => Validation.success(UnkindedType.Cst(TypeConstructor.BigDecimal, loc))
        case "Int8" => Validation.success(UnkindedType.Cst(TypeConstructor.Int8, loc))
        case "Int16" => Validation.success(UnkindedType.Cst(TypeConstructor.Int16, loc))
        case "Int32" => Validation.success(UnkindedType.Cst(TypeConstructor.Int32, loc))
        case "Int64" => Validation.success(UnkindedType.Cst(TypeConstructor.Int64, loc))
        case "BigInt" => Validation.success(UnkindedType.Cst(TypeConstructor.BigInt, loc))
        case "String" => Validation.success(UnkindedType.Cst(TypeConstructor.Str, loc))
        case "Regex" => Validation.success(UnkindedType.Cst(TypeConstructor.Regex, loc))
        case "Sender" => Validation.success(UnkindedType.Cst(TypeConstructor.Sender, loc))
        case "Receiver" => Validation.success(UnkindedType.Cst(TypeConstructor.Receiver, loc))
        case "Lazy" => Validation.success(UnkindedType.Cst(TypeConstructor.Lazy, loc))
        case "Array" => Validation.success(UnkindedType.Cst(TypeConstructor.Array, loc))
        case "Vector" => Validation.success(UnkindedType.Cst(TypeConstructor.Vector, loc))
        case "Region" => Validation.success(UnkindedType.Cst(TypeConstructor.RegionToStar, loc))

        // Disambiguate type.
        case typeName =>
          lookupType(qname, env, ns0, root) match {
            case TypeLookupResult.Enum(enum0) => getEnumTypeIfAccessible(enum0, ns0, loc)
            case TypeLookupResult.Struct(struct) => getStructTypeIfAccessible(struct, ns0, loc)
            case TypeLookupResult.RestrictableEnum(enum0) => getRestrictableEnumTypeIfAccessible(enum0, ns0, loc)
            case TypeLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
            case TypeLookupResult.Effect(eff) => getEffectTypeIfAccessible(eff, ns0, root, loc)
            case TypeLookupResult.JavaClass(clazz) => Validation.success(flixifyType(clazz, loc))
            case TypeLookupResult.AssocType(assoc) => getAssocTypeTypeIfAccessible(assoc, ns0, root, loc)
            case TypeLookupResult.NotFound =>
              Validation.toSoftFailure(UnkindedType.Error(loc), ResolutionError.UndefinedType(qname, ns0, loc))
          }
      }

      case NamedAst.Type.Ambiguous(qname, loc) =>
        // Disambiguate type.
        lookupType(qname, env, ns0, root) match {
          case TypeLookupResult.Enum(enum0) => getEnumTypeIfAccessible(enum0, ns0, loc)
          case TypeLookupResult.Struct(struct) => getStructTypeIfAccessible(struct, ns0, loc)
          case TypeLookupResult.RestrictableEnum(enum0) => getRestrictableEnumTypeIfAccessible(enum0, ns0, loc)
          case TypeLookupResult.TypeAlias(typeAlias) => getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
          case TypeLookupResult.Effect(eff) => getEffectTypeIfAccessible(eff, ns0, root, loc)
          case TypeLookupResult.JavaClass(clazz) => Validation.success(flixifyType(clazz, loc))
          case TypeLookupResult.AssocType(assoc) => getAssocTypeTypeIfAccessible(assoc, ns0, root, loc)
          case TypeLookupResult.NotFound =>
            Validation.toSoftFailure(UnkindedType.Error(loc), ResolutionError.UndefinedType(qname, ns0, loc))
        }

      case NamedAst.Type.Tuple(elms0, loc) =>
        val elmsVal = traverse(elms0)(tpe => visit(tpe))
        mapN(elmsVal) {
          elms => UnkindedType.mkTuple(elms, loc)
        }

      case NamedAst.Type.RecordRowEmpty(loc) =>
        Validation.success(UnkindedType.Cst(TypeConstructor.RecordRowEmpty, loc))

      case NamedAst.Type.RecordRowExtend(label, value, rest, loc) =>
        val vVal = visit(value)
        val rVal = visit(rest)
        mapN(vVal, rVal) {
          case (v, r) => UnkindedType.mkRecordRowExtend(label, v, r, loc)
        }

      case NamedAst.Type.Record(row, loc) =>
        val rVal = visit(row)
        mapN(rVal) {
          r => UnkindedType.mkRecord(r, loc)
        }

      case NamedAst.Type.SchemaRowEmpty(loc) =>
        Validation.success(UnkindedType.Cst(TypeConstructor.SchemaRowEmpty, loc))

      case NamedAst.Type.SchemaRowExtendWithAlias(qname, targs, rest, loc) =>
        // Lookup the type alias.
        flatMapN(lookupTypeAlias(qname, env, ns0, root)) {
          typeAlias =>
            val tVal = getTypeAliasTypeIfAccessible(typeAlias, ns0, root, loc)
            val tsVal = traverse(targs)(visit)
            val rVal = visit(rest)
            mapN(tVal, tsVal, rVal) {
              case (t, ts, r) =>
                val app = UnkindedType.mkApply(t, ts, loc)
                UnkindedType.mkSchemaRowExtend(Name.mkPred(qname.ident), app, r, loc)
            }
        }

      case NamedAst.Type.SchemaRowExtendWithTypes(ident, den, tpes, rest, loc) =>
        val tsVal = traverse(tpes)(visit)
        val rVal = visit(rest)
        mapN(tsVal, rVal) {
          case (ts, r) =>
            val pred = mkPredicate(den, ts, loc)
            UnkindedType.mkSchemaRowExtend(Name.mkPred(ident), pred, r, loc)
        }

      case NamedAst.Type.Schema(row, loc) =>
        val rVal = visit(row)
        mapN(rVal) {
          r => UnkindedType.mkSchema(r, loc)
        }

      case NamedAst.Type.Native(fqn, loc) =>
        mapN(lookupJvmClass(fqn, loc).toValidation) {
          case clazz => flixifyType(clazz, loc)
        }

      case NamedAst.Type.Arrow(tparams0, eff0, tresult0, loc) =>
        val tparamsVal = traverse(tparams0)(visit)
        val tresultVal = visit(tresult0)
        val effVal = traverseOpt(eff0)(visit)
        mapN(tparamsVal, tresultVal, effVal) {
          case (tparams, tresult, eff) => mkUncurriedArrowWithEffect(tparams, eff, tresult, loc)
        }

      case NamedAst.Type.Apply(base0, targ0, loc) =>
        val tpe1Val = visit(base0)
        val tpe2Val = visit(targ0)
        mapN(tpe1Val, tpe2Val) {
          case (tpe1, tpe2) => UnkindedType.Apply(tpe1, tpe2, loc)
        }

      case NamedAst.Type.True(loc) =>
        Validation.success(UnkindedType.Cst(TypeConstructor.True, loc))

      case NamedAst.Type.False(loc) =>
        Validation.success(UnkindedType.Cst(TypeConstructor.False, loc))

      case NamedAst.Type.Not(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => mkNot(t, loc)
        }

      case NamedAst.Type.And(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkAnd(t1, t2, loc)
        }

      case NamedAst.Type.Or(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkOr(t1, t2, loc)
        }

      case NamedAst.Type.Complement(tpe, loc) =>
        mapN(visit(tpe)) {
          t => mkComplement(t, loc)
        }

      case NamedAst.Type.Union(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkUnion(t1, t2, loc)
        }

      case NamedAst.Type.Intersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => mkIntersection(t1, t2, loc)
        }

      case NamedAst.Type.Pure(loc) =>
        Validation.success(UnkindedType.Cst(TypeConstructor.Pure, loc))

      case NamedAst.Type.CaseSet(cases0, loc) =>
        val casesVal = traverse(cases0)(lookupRestrictableTag(_, env, ns0, root))
        mapN(casesVal) {
          case cases => UnkindedType.CaseSet(cases.map(_.sym), loc)
        }

      case NamedAst.Type.CaseComplement(tpe, loc) =>
        mapN(visit(tpe)) {
          t => UnkindedType.CaseComplement(t, loc)
        }

      case NamedAst.Type.CaseUnion(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => UnkindedType.CaseUnion(t1, t2, loc)
        }

      case NamedAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => UnkindedType.CaseIntersection(t1, t2, loc)
        }

      case NamedAst.Type.Ascribe(t0, kind0, loc) =>
        val tVal = visit(t0)
        val kindVal = resolveKind(kind0, env, ns0, root)
        mapN(tVal, kindVal) {
          (t, kind) => UnkindedType.Ascribe(t, kind, loc)
        }

      case NamedAst.Type.Error(loc) =>
        Validation.success(UnkindedType.Error(loc))

    }

    visit(tpe0)
  }

  /**
    * Finishes resolving the partially resolved type `tpe0`.
    *
    * Replaces type alias placeholders with the real type aliases.
    */
  private def finishResolveType(tpe0: UnkindedType, taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]): Validation[UnkindedType, ResolutionError] = {

    /**
      * Performs beta-reduction on the given type alias.
      * The list of arguments must be the same length as the alias's parameters.
      */
    def applyAlias(alias: ResolvedAst.Declaration.TypeAlias, args: List[UnkindedType], cstLoc: SourceLocation): UnkindedType = {
      val map = alias.tparams.map(_.sym).zip(args).toMap[Symbol.UnkindedTypeVarSym, UnkindedType]
      val tpe = alias.tpe.map(map)
      val cst = Ast.AliasConstructor(alias.sym, cstLoc)
      UnkindedType.Alias(cst, args, tpe, tpe0.loc)
    }

    val baseType = tpe0.baseType
    val targs = tpe0.typeArguments

    baseType match {
      case UnkindedType.UnappliedAlias(sym, loc) =>
        val alias = taenv(sym)
        val tparams = alias.tparams
        val numParams = tparams.length
        if (targs.length < numParams) {
          // Case 1: The type alias is under-applied.
          Validation.toSoftFailure(UnkindedType.Error(loc), ResolutionError.UnderAppliedTypeAlias(sym, loc))
        } else {
          // Case 2: The type alias is fully applied.
          // Apply the types within the alias, then apply any leftover types.
          mapN(traverse(targs)(finishResolveType(_, taenv))) {
            resolvedArgs =>
              val (usedArgs, extraArgs) = resolvedArgs.splitAt(numParams)
              UnkindedType.mkApply(applyAlias(alias, usedArgs, loc), extraArgs, tpe0.loc)
          }
        }

      case UnkindedType.UnappliedAssocType(sym, loc) =>
        targs match {
          // Case 1: The associated type is under-applied.
          case Nil => Validation.toSoftFailure(UnkindedType.Error(loc), ResolutionError.UnderAppliedAssocType(sym, loc))

          // Case 2: The associated type is fully applied.
          // Apply the types first type inside the assoc type, then apply any leftover types.
          case targHead :: targTail =>
            val targHeadVal = finishResolveType(targHead, taenv)
            val targTailVal = traverse(targTail)(finishResolveType(_, taenv))
            flatMapN(targHeadVal, targTailVal) {
              case (targHd: UnkindedType.Var, targTl) =>
                val cst = Ast.AssocTypeConstructor(sym, loc)
                val assoc = UnkindedType.AssocType(cst, targHd, tpe0.loc)
                Validation.success(UnkindedType.mkApply(assoc, targTl, tpe0.loc))
              case _ =>
                Validation.toSoftFailure(UnkindedType.Error(loc), ResolutionError.IllegalAssocTypeApplication(tpe0.loc))
            }
        }

      case _: UnkindedType.Var =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Cst =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Enum =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Struct =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.RestrictableEnum =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.CaseSet =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case UnkindedType.Arrow(eff, arity, loc) =>
        val effVal = traverseOpt(eff)(finishResolveType(_, taenv))
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(effVal, targsVal) {
          case (p, ts) => UnkindedType.mkApply(UnkindedType.Arrow(p, arity, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseComplement(tpe, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.CaseComplement(t, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseUnion(tpe1, tpe2, loc) =>
        val tpe1Val = finishResolveType(tpe1, taenv)
        val tpe2Val = finishResolveType(tpe2, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpe1Val, tpe2Val, targsVal) {
          case (t1, t2, ts) => UnkindedType.mkApply(UnkindedType.CaseUnion(t1, t2, loc), ts, tpe0.loc)
        }

      case UnkindedType.CaseIntersection(tpe1, tpe2, loc) =>
        val tpe1Val = finishResolveType(tpe1, taenv)
        val tpe2Val = finishResolveType(tpe2, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpe1Val, tpe2Val, targsVal) {
          case (t1, t2, ts) => UnkindedType.mkApply(UnkindedType.CaseIntersection(t1, t2, loc), ts, tpe0.loc)
        }

      case UnkindedType.Ascribe(tpe, kind, loc) =>
        val tpeVal = finishResolveType(tpe, taenv)
        val targsVal = traverse(targs)(finishResolveType(_, taenv))
        mapN(tpeVal, targsVal) {
          case (t, ts) => UnkindedType.mkApply(UnkindedType.Ascribe(t, kind, loc), ts, tpe0.loc)
        }

      case _: UnkindedType.Error =>
        mapN(traverse(targs)(finishResolveType(_, taenv))) {
          resolvedArgs => UnkindedType.mkApply(baseType, resolvedArgs, tpe0.loc)
        }

      case _: UnkindedType.Apply => throw InternalCompilerException("unexpected type application", baseType.loc)
      case _: UnkindedType.Alias => throw InternalCompilerException("unexpected resolved alias", baseType.loc)
      case _: UnkindedType.AssocType => throw InternalCompilerException("unexpected resolved associated type", baseType.loc)
    }
  }

  /**
    * Performs name resolution on the given type `tpe0` in the given namespace `ns0`.
    */
private def resolveType(tpe0: NamedAst.Type, wildness: Wildness, env: ListMap[String, Resolution], taenv: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias], ns0: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[UnkindedType, ResolutionError] = {
    val tVal = semiResolveType(tpe0, wildness, env, ns0, root)
    flatMapN(tVal) {
      t => finishResolveType(t, taenv)
    }
  }

  /**
    * The result of looking up an ambiguous type.
    */
  private sealed trait TypeLookupResult {
    /**
      * Returns `other` if this result is [[TypeLookupResult.NotFound]].
      *
      * Otherwise, returns this result.
      */
    def orElse(other: => TypeLookupResult): TypeLookupResult = this match {
      case res: TypeLookupResult.Enum => res
      case res: TypeLookupResult.Struct => res
      case res: TypeLookupResult.RestrictableEnum => res
      case res: TypeLookupResult.TypeAlias => res
      case res: TypeLookupResult.Effect => res
      case res: TypeLookupResult.JavaClass => res
      case res: TypeLookupResult.AssocType => res
      case TypeLookupResult.NotFound => other
    }
  }

  private object TypeLookupResult {
    /**
      * The result is an enum.
      */
    case class Enum(enum0: NamedAst.Declaration.Enum) extends TypeLookupResult

    /**
      * The result is an struct.
      */
    case class Struct(struct0: NamedAst.Declaration.Struct) extends TypeLookupResult

    /**
      * The result is a restrictable enum.
      */
    case class RestrictableEnum(enum0: NamedAst.Declaration.RestrictableEnum) extends TypeLookupResult

    /**
      * The result is a type alias.
      */
    case class TypeAlias(typeAlias: NamedAst.Declaration.TypeAlias) extends TypeLookupResult

    /**
      * The result is an effect.
      */
    case class Effect(eff: NamedAst.Declaration.Effect) extends TypeLookupResult

    /**
      * The result is a Java class.
      */
    case class JavaClass(clazz: Class[_]) extends TypeLookupResult

    /**
      * The result is an associated type constructor.
      */
    case class AssocType(assoc: NamedAst.Declaration.AssocTypeSig) extends TypeLookupResult

    /**
      * The type cannot be found.
      */
    case object NotFound extends TypeLookupResult
  }


  /**
    * Looks up the ambiguous type.
    */
  private def lookupType(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): TypeLookupResult = {
    tryLookupName(qname, env, ns0, root).collectFirst {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) =>
        // Case 1: found a type alias
        TypeLookupResult.TypeAlias(alias)
      case Resolution.Declaration(enum0: NamedAst.Declaration.Enum) =>
        // Case 2: found an enum
        TypeLookupResult.Enum(enum0)
      case Resolution.Declaration(struct: NamedAst.Declaration.Struct) =>
        // Case 3: found a struct
        TypeLookupResult.Struct(struct)
      case Resolution.Declaration(enum0: NamedAst.Declaration.RestrictableEnum) =>
        // Case 4: found a restrictable enum
        TypeLookupResult.RestrictableEnum(enum0)
      case Resolution.Declaration(effect: NamedAst.Declaration.Effect) =>
        // Case 5: found an effect
        TypeLookupResult.Effect(effect)
      case Resolution.Declaration(assoc: NamedAst.Declaration.AssocTypeSig) =>
        // Case 6: found an associated type
        TypeLookupResult.AssocType(assoc)
      case Resolution.JavaClass(clazz) =>
        // Case 7: found a Java class
        TypeLookupResult.JavaClass(clazz)
    }.getOrElse(TypeLookupResult.NotFound)
  }

  /**
    * Optionally returns the type alias with the given `name` in the given namespace `ns0`.
    */
  private def lookupTypeAlias(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.TypeAlias, ResolutionError] = {
    val symOpt = tryLookupName(qname, env, ns0, root)

    symOpt.collectFirst {
      case Resolution.Declaration(alias: NamedAst.Declaration.TypeAlias) => getTypeAliasIfAccessible(alias, ns0, qname.loc)
    }.getOrElse(Validation.toHardFailure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, Map.empty, isUse = false, qname.loc)))
  }

  /**
    * Optionally returns the associated type signature with the given `name` in the given namespace `ns0`.
    */
  private def lookupAssocType(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[NamedAst.Declaration.AssocTypeSig, ResolutionError] = {
    val symOpt = tryLookupName(qname, env, ns0, root)

    symOpt.collectFirst {
      case Resolution.Declaration(assoc: NamedAst.Declaration.AssocTypeSig) => getAssocTypeIfAccessible(assoc, ns0, qname.loc)
    }.getOrElse(Validation.toHardFailure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, Map.empty, isUse = false, qname.loc)))
  }

  /**
    * Looks up the definition or signature with qualified name `qname` in the namespace `ns0`.
    */
  private def lookupEffect(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Result[NamedAst.Declaration.Effect, UndefinedEffect] = {
    val effOpt = tryLookupName(qname, env, ns0, root).collectFirst {
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) => eff
    }

    effOpt match {
      case None => Result.Err(ResolutionError.UndefinedEffect(qname, ns0, qname.loc))
      case Some(decl) => Result.Ok(decl)
    }
  }

  /**
    * Looks up the type variable with the given name.
    */
  private def lookupTypeVar(ident: Name.Ident, wildness: Wildness, env: ListMap[String, Resolution])(implicit flix: Flix): Result[Symbol.UnkindedTypeVarSym, ResolutionError with Recoverable] = {
    if (ident.isWild) {
      wildness match {
        case Wildness.AllowWild =>
          // We use Top scope because these lookups only occur at top level
          Result.Ok(Symbol.freshUnkindedTypeVarSym(VarText.SourceText(ident.name), isRegion = false, ident.loc)(Scope.Top, flix))
        case Wildness.ForbidWild =>
          Result.Err(ResolutionError.IllegalWildType(ident, ident.loc))
      }
    } else {
      val typeVarOpt = env(ident.name).collectFirst {
        case Resolution.TypeVar(sym) => sym
      }
      typeVarOpt match {
        case Some(sym) => Result.Ok(sym)
        case None => Result.Err(ResolutionError.UndefinedTypeVar(ident.name, ident.loc))
      }
    }
  }

  /**
    * Returns the list of symbols this name points to, ordered from most closely declared to furthest.
    */
  private def tryLookupName(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): List[Resolution] = {
    if (qname.isUnqualified) {
      // Case 1: Unqualified name.

      // Gather names according to priority:
      // 1st priority: imported names
      val envNames = env(qname.ident.name)

      // 2nd priority: names in the current namespace
      val localNames = if (ns0.idents.nonEmpty) {
        root.symbols.getOrElse(ns0, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration.apply)
      } else {
        Nil
      }

      // 3rd priority: the name of the current namespace
      val currentNamespace = {
        // Make sure we don't duplicate results in `rootNames`
        if (ns0.idents.size > 1 && ns0.idents.lastOption.contains(qname.ident)) {
          // Case 1.1.1.1: We are referring to the current namespace. Use that.
          root.symbols.getOrElse(Name.mkUnlocatedNName(ns0.parts.init), Map.empty).getOrElse(ns0.parts.last, Nil).map(Resolution.Declaration.apply)
        } else {
          Nil
        }
      }

      // 4th priority: names in the root namespace
      val rootNames = root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(qname.ident.name, Nil).map(Resolution.Declaration.apply)

      envNames ::: localNames ::: currentNamespace ::: rootNames

    } else {
      // Case 2. Qualified name. Look it up directly.
      tryLookupQualifiedName(qname, env, ns0, root).getOrElse(Nil).map(Resolution.Declaration.apply)
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def tryLookupQualifiedName(qname0: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Option[List[NamedAst.Declaration]] = {
    // First resolve the root of the qualified name
    val head = qname0.namespace.parts.head
    tryLookupModule(head, env, ns0, root) match {
      case None => None
      case Some(prefix) =>
        val ns = prefix ::: qname0.namespace.parts.tail
        val qname = Name.mkQName(ns, qname0.ident.name, SourceLocation.Unknown)
        root.symbols.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
    }
  }

  /**
    * Looks up the given module in the root.
    */
  private def tryLookupModule(name: String, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Option[List[String]] = {
    // First see if there's a module with this name imported into our environment
    env(name).collectFirst {
      case Resolution.Declaration(ns: NamedAst.Declaration.Namespace) => ns.sym.ns
      case Resolution.Declaration(trt: NamedAst.Declaration.Trait) => trt.sym.namespace :+ trt.sym.name
      case Resolution.Declaration(enum0: NamedAst.Declaration.Enum) => enum0.sym.namespace :+ enum0.sym.name
      case Resolution.Declaration(struct: NamedAst.Declaration.Struct) => struct.sym.namespace :+ struct.sym.name
      case Resolution.Declaration(enum0: NamedAst.Declaration.RestrictableEnum) => enum0.sym.namespace :+ enum0.sym.name
      case Resolution.Declaration(eff: NamedAst.Declaration.Effect) => eff.sym.namespace :+ eff.sym.name
    }.orElse {
      // Then see if there's a module with this name declared in our namespace
      root.symbols.getOrElse(ns0, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, usesAndImports, decls, loc) => sym.ns
        case Declaration.Trait(doc, ann, mod, sym, tparam, superClasses, _, sigs, laws, loc) => sym.namespace :+ sym.name
        case Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Struct(doc, ann, mod, sym, tparams, fields, indices, loc) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.namespace :+ sym.name
      }
    }.orElse {
      // Then see if there's a module with this name declared in the root namespace
      root.symbols.getOrElse(Name.RootNS, Map.empty).getOrElse(name, Nil).collectFirst {
        case Declaration.Namespace(sym, usesAndImports, decls, loc) => sym.ns
        case Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, _, sigs, laws, loc) => sym.namespace :+ sym.name
        case Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Struct(doc, ann, mod, sym, tparams, fields, indices, loc) => sym.namespace :+ sym.name
        case Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.namespace :+ sym.name
        case Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.namespace :+ sym.name
      }
    }
  }

  /**
    * Looks up the qualified name in the given root.
    */
  private def lookupQualifiedName(qname: Name.QName, env: ListMap[String, Resolution], ns0: Name.NName, root: NamedAst.Root): Validation[List[NamedAst.Declaration], ResolutionError] = {
    tryLookupQualifiedName(qname, env, ns0, root) match {
      case None => Validation.toHardFailure(ResolutionError.UndefinedNameUnrecoverable(qname, ns0, Map.empty, isUse = false, qname.loc))
      case Some(decl) => Validation.success(decl)
    }
  }

  /**
    * Determines if the trait is accessible from the namespace.
    *
    * Accessibility depends on the modifiers on the trait
    * and the accessing namespace's relation to the trait namespace:
    *
    * |            | same | child | other |
    * |------------|------|-------|-------|
    * | (none)     | A    | A     | I     |
    * | sealed     | A    | S     | I     |
    * | pub        | A    | A     | A     |
    * | pub sealed | A    | S     | S     |
    *
    * (A: Accessible, S: Sealed, I: Inaccessible)
    */
  private def getTraitAccessibility(trait0: NamedAst.Declaration.Trait, ns0: Name.NName): TraitAccessibility = {

    val traitNs = trait0.sym.namespace
    val accessingNs = ns0.idents.map(_.name)

    if (traitNs == accessingNs) {
      // Case 1: We're in the same namespace: Accessible
      TraitAccessibility.Accessible
    } else if (!trait0.mod.isPublic && !accessingNs.startsWith(traitNs)) {
      // Case 2: The trait is private and we're in unrelated namespaces: Inaccessible
      TraitAccessibility.Inaccessible
    } else if (trait0.mod.isSealed) {
      // Case 3: The trait is accessible but sealed
      TraitAccessibility.Sealed
    } else {
      // Case 4: The trait is otherwise accessible
      TraitAccessibility.Accessible
    }
  }

  /**
    * Determines if the definition is accessible from the namespace.
    *
    * A definition `defn0` is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def isDefAccessible(defn0: NamedAst.Declaration.Def, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (defn0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = defn0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }

  /**
    * Determines if the signature is accessible from the namespace.
    *
    * A signature `sig0` is accessible from a namespace `ns0` if:
    *
    * (a) the signature is marked public, or
    * (b) the signature is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def isSigAccessible(sig0: NamedAst.Declaration.Sig, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (sig0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = sig0.sym.trt.namespace :+ sig0.sym.trt.name
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }


  /**
    * Determines if the operation is accessible from the namespace.
    *
    * An operation `op0` is accessible from a namespace `ns0` if:
    *
    * (a) the operation is marked public, or
    * (b) the operation is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
private def isOpAccessible(op0: NamedAst.Declaration.Op, ns0: Name.NName): Boolean = {
    //
    // Check if the definition is marked public.
    //
    if (op0.spec.mod.isPublic)
      return true

    //
    // Check if the definition is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = op0.sym.eff.namespace :+ op0.sym.eff.name
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return true

    //
    // The definition is not accessible.
    //
    false
  }

  /**
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getEnumIfAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.Enum, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (enum0.mod.isPublic)
      return Validation.success(enum0)

    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return Validation.success(enum0)

    //
    // The enum is not accessible.
    //
    Validation.toSoftFailure(enum0, ResolutionError.InaccessibleEnum(enum0.sym, ns0, loc))
  }

  /**
    * Successfully returns the given `struct0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * A struct is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getStructIfAccessible(struct0: NamedAst.Declaration.Struct, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.Struct, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (struct0.mod.isPublic)
      return Validation.success(struct0)

    //
    // Check if the struct is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = struct0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return Validation.success(struct0)

    //
    // The struct is not accessible.
    //
    Validation.toSoftFailure(struct0, ResolutionError.InaccessibleStruct(struct0.sym, ns0, loc))
  }


  /**
    * Successfully returns the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
private def getRestrictableEnumIfAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.RestrictableEnum, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (enum0.mod.isPublic)
      return Validation.success(enum0)


    //
    // Check if the enum is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = enum0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return Validation.success(enum0)

    //
    // The enum is not accessible.
    //
    Validation.toHardFailure(ResolutionError.InaccessibleRestrictableEnum(enum0.sym, ns0, loc))
  }


  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getEnumTypeIfAccessible(enum0: NamedAst.Declaration.Enum, ns0: Name.NName, loc: SourceLocation): Validation[UnkindedType, ResolutionError] =
    mapN(getEnumIfAccessible(enum0, ns0, loc)) {
      case enum0 => mkEnum(enum0.sym, loc)
    }

  /**
    * Successfully returns the type of the given `struct0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getStructTypeIfAccessible(struct0: NamedAst.Declaration.Struct, ns0: Name.NName, loc: SourceLocation): Validation[UnkindedType, ResolutionError] =
    mapN(getStructIfAccessible(struct0, ns0, loc)) {
      case struct => mkStruct(struct.sym, loc)
    }

  /**
    * Successfully returns the type of the given `enum0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getRestrictableEnumTypeIfAccessible(enum0: NamedAst.Declaration.RestrictableEnum, ns0: Name.NName, loc: SourceLocation): Validation[UnkindedType, ResolutionError] =
    mapN(getRestrictableEnumIfAccessible(enum0, ns0, loc)) {
      case enum0 => mkRestrictableEnum(enum0.sym, loc)
    }

  /**
    * Successfully returns the given type alias `alia0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An enum is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getTypeAliasIfAccessible(alia0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.TypeAlias, ResolutionError] = {
    //
    // Check if the definition is marked public.
    //
    if (alia0.mod.isPublic)
      return Validation.success(alia0)

    //
    // Check if the type alias is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = alia0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return Validation.success(alia0)

    //
    // The type alias is not accessible.
    //
    Validation.toSoftFailure(alia0, ResolutionError.InaccessibleTypeAlias(alia0.sym, ns0, loc))
  }

  /**
    * Successfully returns the type of the given type alias `alia0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getTypeAliasTypeIfAccessible(alia0: NamedAst.Declaration.TypeAlias, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[UnkindedType, ResolutionError] = {
    mapN(getTypeAliasIfAccessible(alia0, ns0, loc)) {
      alias => mkUnappliedTypeAlias(alias.sym, loc)
    }
  }

  /**
    * Successfully returns the given associated type `assoc0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An associated type is accessible from a namespace `ns0` if:
    *
    * (a) its trait is marked public, or
    * (b) the trait is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getAssocTypeIfAccessible(assoc0: NamedAst.Declaration.AssocTypeSig, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.AssocTypeSig, ResolutionError] = {
    Validation.success(assoc0) // TODO ASSOC-TYPES check class accessibility
  }

  /**
    * Successfully returns the type of the given associated type `assoc0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getAssocTypeTypeIfAccessible(assoc0: NamedAst.Declaration.AssocTypeSig, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[UnkindedType, ResolutionError] = {
    mapN(getAssocTypeIfAccessible(assoc0, ns0, loc)) {
      assoc => mkUnappliedAssocType(assoc0.sym, loc)
    }
  }

  /**
    * Successfully returns the given `eff0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    *
    * An effect is accessible from a namespace `ns0` if:
    *
    * (a) the definition is marked public, or
    * (b) the definition is defined in the namespace `ns0` itself or in a parent of `ns0`.
    */
  private def getEffectIfAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, loc: SourceLocation): Validation[NamedAst.Declaration.Effect, ResolutionError] = {
    //
    // Check if the effect is marked public.
    //
    if (eff0.mod.isPublic)
      return Validation.success(eff0)

    //
    // Check if the effect is defined in `ns0` or in a parent of `ns0`.
    //
    val prefixNs = eff0.sym.namespace
    val targetNs = ns0.idents.map(_.name)
    if (targetNs.startsWith(prefixNs))
      return Validation.success(eff0)

    //
    // The effect is not accessible.
    //
    Validation.toSoftFailure(eff0, ResolutionError.InaccessibleEffect(eff0.sym, ns0, loc))
  }

  /**
    * Successfully returns the type of the given effect `eff0` if it is accessible from the given namespace `ns0`.
    *
    * Otherwise fails with a resolution error.
    */
  private def getEffectTypeIfAccessible(eff0: NamedAst.Declaration.Effect, ns0: Name.NName, root: NamedAst.Root, loc: SourceLocation): Validation[UnkindedType, ResolutionError] = {
    mapN(getEffectIfAccessible(eff0, ns0, loc)) {
      alias => mkEffect(alias.sym, loc)
    }
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass(className: String, loc: SourceLocation)(implicit flix: Flix): Result[Class[_], ResolutionError with Recoverable] = try {
    // Don't initialize the class; we don't want to execute static initializers.
    val initialize = false
    Result.Ok(Class.forName(className, initialize, flix.jarLoader))
  } catch {
    case ex: ClassNotFoundException => Result.Err(ResolutionError.UndefinedJvmClass(className, ex.getMessage, loc))
    case ex: NoClassDefFoundError => Result.Err(ResolutionError.UndefinedJvmClass(className, ex.getMessage, loc))
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  private def lookupJvmClass2(className: String, env0: ListMap[String, Resolution], loc: SourceLocation)(implicit flix: Flix): Result[Class[_], ResolutionError with Recoverable] = {
    lookupJvmClass(className, loc) match {
      case Result.Ok(clazz) => Result.Ok(clazz)
      case Result.Err(e) => env0.get(className) match {
        case Some(List(Resolution.JavaClass(clazz))) => Result.Ok(clazz)
        case _ => Result.Err(e)
      }
    }
  }

  /**
    * Returns the constructor reflection object for the given `clazz` and `signature`.
    */
  private def lookupJvmConstructor(clazz: Class[_], signature: List[Class[_]], loc: SourceLocation)(implicit flix: Flix): Result[Constructor[_], ResolutionError with Recoverable] = {
    try {
      // Lookup the constructor with the appropriate signature.
      Result.Ok(clazz.getConstructor(signature: _*))
    } catch {
      case ex: NoSuchMethodException => Result.Err(ResolutionError.UndefinedJvmConstructor(clazz, signature, clazz.getConstructors.toList, loc))
      // ClassNotFoundException:  Cannot happen because we already have the `Class` object.
      // NoClassDefFoundError:    Cannot happen because we already have the `Class` object.
    }
  }

  /**
    * Returns the method reflection object for the given `clazz`, `methodName`, and `signature`.
    */
  private def lookupJvmMethod(clazz: Class[_], methodName: String, signature: List[Class[_]], retTpe: UnkindedType, static: Boolean, loc: SourceLocation)(implicit flix: Flix): Result[Method, ResolutionError with Recoverable] = {
    try {
      // Lookup the method with the appropriate signature.
      val method = clazz.getMethod(methodName, signature: _*)

      // Check if the method should be and is static.
      if (static != Modifier.isStatic(method.getModifiers)) {
        throw new NoSuchMethodException()
      } else {
        // Check that the return type of the method matches the declared type.
        // We currently don't know how to handle all possible return types,
        // so only check the straightforward cases for now and succeed all others.
        // TODO move to typer
        val erasedRetTpe = UnkindedType.eraseAliases(retTpe)
        erasedRetTpe.baseType match {
          case UnkindedType.Cst(TypeConstructor.Unit, _) | UnkindedType.Cst(TypeConstructor.Bool, _) |
               UnkindedType.Cst(TypeConstructor.Char, _) | UnkindedType.Cst(TypeConstructor.Float32, _) |
               UnkindedType.Cst(TypeConstructor.Float64, _) | UnkindedType.Cst(TypeConstructor.BigDecimal, _) |
               UnkindedType.Cst(TypeConstructor.Int8, _) | UnkindedType.Cst(TypeConstructor.Int16, _) |
               UnkindedType.Cst(TypeConstructor.Int32, _) | UnkindedType.Cst(TypeConstructor.Int64, _) |
               UnkindedType.Cst(TypeConstructor.BigInt, _) | UnkindedType.Cst(TypeConstructor.Str, _) |
               UnkindedType.Cst(TypeConstructor.Regex, _) | UnkindedType.Cst(TypeConstructor.Native(_), _) =>

            val expectedTpe = UnkindedType.getFlixType(method.getReturnType)
            if (expectedTpe == erasedRetTpe)
              Result.Ok(method)
            else
              Result.Err(ResolutionError.MismatchedReturnType(clazz.getName, methodName, retTpe, expectedTpe, loc))

          case _ => Result.Ok(method)
        }
      }
    } catch {
      case ex: NoSuchMethodException =>
        val candidateMethods = JvmOps.getMethods(clazz).filter(m => m.getName == methodName)
        Result.Err(ResolutionError.UndefinedJvmMethod(clazz.getName, methodName, static, signature, candidateMethods, loc))
      // ClassNotFoundException:  Cannot happen because we already have the `Class` object.
      // NoClassDefFoundError:    Cannot happen because we already have the `Class` object.
    }
  }

  /**
    * Returns the class and field reflection objects for the given `className` and `fieldName`.
    */
  private def lookupJvmField(className: String, fieldName: String, static: Boolean, loc: SourceLocation)(implicit flix: Flix): Result[(Class[_], Field), ResolutionError with Recoverable] = {
    lookupJvmClass(className, loc).flatMap {
      case clazz =>
        try {
          // Lookup the field.
          val field = clazz.getField(fieldName)

          // Check if the field should be and is static.
          if (static == Modifier.isStatic(field.getModifiers))
            Result.Ok((clazz, field))
          else
            throw new NoSuchFieldException()
        } catch {
          case ex: NoSuchFieldException =>
            val candidateFields = clazz.getFields.toList
            Result.Err(ResolutionError.UndefinedJvmField(clazz.getName, fieldName, static, candidateFields, loc))
          // ClassNotFoundException:  Cannot happen because we already have the `Class` object.
          // NoClassDefFoundError:    Cannot happen because we already have the `Class` object.
        }
    }
  }

  /**
    * Performs name resolution on the given `signature`.
    */
  private def lookupSignature(signature: List[UnkindedType], loc: SourceLocation)(implicit flix: Flix): Validation[List[Class[_]], ResolutionError] = {
    Result.traverse(signature)(getJVMType(_, loc)).toValidation
  }

  /**
    * Returns the JVM type corresponding to the given Flix type `tpe`.
    *
    * A non-primitive Flix type is mapped to java.lang.Object.
    *
    * An array type is mapped to the corresponding array type.
    */
  private def getJVMType(tpe: UnkindedType, loc: SourceLocation)(implicit flix: Flix): Result[Class[_], ResolutionError] = {
    val erased = UnkindedType.eraseAliases(tpe)
    val baseType = erased.baseType
    baseType match {
      // Case 1: Constant: Match on the type.
      case UnkindedType.Cst(tc, _) => tc match {
        case TypeConstructor.Void =>
          // Flix `Void` is _not_ Java's `void`.
          Result.Err(ResolutionError.IllegalType(tpe, loc))

        case TypeConstructor.Unit => Result.Ok(Class.forName("java.lang.Object"))

        case TypeConstructor.Bool => Result.Ok(classOf[Boolean])

        case TypeConstructor.Char => Result.Ok(classOf[Char])

        case TypeConstructor.Float32 => Result.Ok(classOf[Float])

        case TypeConstructor.Float64 => Result.Ok(classOf[Double])

        case TypeConstructor.BigDecimal => Result.Ok(Class.forName("java.math.BigDecimal"))

        case TypeConstructor.Int8 => Result.Ok(classOf[Byte])

        case TypeConstructor.Int16 => Result.Ok(classOf[Short])

        case TypeConstructor.Int32 => Result.Ok(classOf[Int])

        case TypeConstructor.Int64 => Result.Ok(classOf[Long])

        case TypeConstructor.BigInt => Result.Ok(Class.forName("java.math.BigInteger"))

        case TypeConstructor.Str => Result.Ok(Class.forName("java.lang.String"))

        case TypeConstructor.Regex => Result.Ok(Class.forName("java.util.regex.Pattern"))

        case TypeConstructor.Sender => Result.Ok(Class.forName("java.lang.Object"))

        case TypeConstructor.Receiver => Result.Ok(Class.forName("java.lang.Object"))

        case TypeConstructor.Tuple(_) => Result.Ok(Class.forName("java.lang.Object"))

        case TypeConstructor.Array =>
          erased.typeArguments match {
            case elmTyp :: region :: Nil =>
              getJVMType(elmTyp, loc) map {
                case elmClass => getJVMArrayType(elmClass)
              }
            case _ => Result.Err(ResolutionError.IllegalType(tpe, loc))
          }

        case TypeConstructor.Vector =>
          erased.typeArguments match {
            case elmTyp :: region :: Nil =>
              getJVMType(elmTyp, loc) map {
                case elmClass => getJVMArrayType(elmClass)
              }
            case _ => Result.Err(ResolutionError.IllegalType(tpe, loc))
          }

        case TypeConstructor.Native(clazz) => Result.Ok(clazz)

        case TypeConstructor.Record => Result.Ok(Class.forName("java.lang.Object"))

        case TypeConstructor.Schema => Result.Ok(Class.forName("java.lang.Object"))


        case TypeConstructor.True => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.False => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Not => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.And => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Or => Result.Err(ResolutionError.IllegalType(tpe, loc))

        case TypeConstructor.Union => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Effect(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Univ => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Lattice => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Lazy => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Complement => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Null => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Intersection => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.RecordRowEmpty => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.RecordRowExtend(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.RegionToStar => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Relation => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.SchemaRowEmpty => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.SchemaRowExtend(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Pure => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.CaseComplement(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.CaseSet(_, _) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.CaseIntersection(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.CaseUnion(_) => Result.Err(ResolutionError.IllegalType(tpe, loc))
        case TypeConstructor.Error(_, _) => Result.Err(ResolutionError.IllegalType(tpe, loc))

        case TypeConstructor.AnyType => throw InternalCompilerException(s"unexpected type: $tc", tpe.loc)
        case t: TypeConstructor.Arrow => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case t: TypeConstructor.Enum => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case t: TypeConstructor.Struct => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case t: TypeConstructor.RestrictableEnum => throw InternalCompilerException(s"unexpected type: $t", tpe.loc)
        case TypeConstructor.JvmConstructor(_) => throw InternalCompilerException(s"unexpected type: $tc", tpe.loc)
        case TypeConstructor.JvmMethod(_) => throw InternalCompilerException(s"unexpected type: $tc", tpe.loc)
        case TypeConstructor.JvmField(_) => throw InternalCompilerException(s"unexpected type: $tc", tpe.loc)

      }

      // Case 2: Arrow. Convert to Java function interface
      case UnkindedType.Arrow(_, _, _) =>
        val targsVal = Result.traverse(erased.typeArguments)(targ => getJVMType(targ, targ.loc))
        val returnsUnit = erased.typeArguments.lastOption match {
          case Some(ty) => isBaseTypeUnit(ty)
          case None => false
        }
        targsVal flatMap {
          case Object :: Object :: Nil =>
            if (returnsUnit)
              Result.Ok(Class.forName("java.util.function.Consumer"))
            else
              Result.Ok(Class.forName("java.util.function.Function"))
          case Object :: Boolean :: Nil => Result.Ok(Class.forName("java.util.function.Predicate"))
          case Int :: Object :: Nil =>
            if (returnsUnit)
              Result.Ok(Class.forName("java.util.function.IntConsumer"))
            else
              Result.Ok(Class.forName("java.util.function.IntFunction"))
          case Int :: Boolean :: Nil => Result.Ok(Class.forName("java.util.function.IntPredicate"))
          case Int :: Int :: Nil => Result.Ok(Class.forName("java.util.function.IntUnaryOperator"))
          case Long :: Object :: Nil =>
            if (returnsUnit)
              Result.Ok(Class.forName("java.util.function.LongConsumer"))
            else
              Result.Ok(Class.forName("java.util.function.LongFunction"))
          case Long :: Boolean :: Nil => Result.Ok(Class.forName("java.util.function.LongPredicate"))
          case Long :: Long :: Nil => Result.Ok(Class.forName("java.util.function.LongUnaryOperator"))
          case Double :: Object :: Nil =>
            if (returnsUnit)
              Result.Ok(Class.forName("java.util.function.DoubleConsumer"))
            else
              Result.Ok(Class.forName("java.util.function.DoubleFunction"))
          case Double :: Boolean :: Nil => Result.Ok(Class.forName("java.util.function.DoublePredicate"))
          case Double :: Double :: Nil => Result.Ok(Class.forName("java.util.function.DoubleUnaryOperator"))
          case _ => Result.Err(ResolutionError.IllegalType(tpe, loc))
        }

      // Case 3: Enum / Struct. Return an object type.
      case _: UnkindedType.Enum => Result.Ok(Class.forName("java.lang.Object"))
      // TODO STRUCTS Should this be a specific class instead?
      case _: UnkindedType.Struct => Result.Ok(Class.forName("java.lang.Object"))
      case _: UnkindedType.RestrictableEnum => Result.Ok(Class.forName("java.lang.Object"))

      // Case 4: Ascription. Ignore it and recurse.
      case UnkindedType.Ascribe(t, _, _) => getJVMType(UnkindedType.mkApply(t, erased.typeArguments, loc), loc)

      // Case 5: It's a broken type. Lets hope it is an object.
      case UnkindedType.Error(_) => Result.Ok(Class.forName("java.lang.Object"))

      // Case 5: Illegal type. Error.
      case _: UnkindedType.Var => Result.Err(ResolutionError.IllegalType(tpe, loc))
      case _: UnkindedType.CaseSet => Result.Err(ResolutionError.IllegalType(tpe, loc))
      case _: UnkindedType.CaseComplement => Result.Err(ResolutionError.IllegalType(tpe, loc))
      case _: UnkindedType.CaseUnion => Result.Err(ResolutionError.IllegalType(tpe, loc))
      case _: UnkindedType.CaseIntersection => Result.Err(ResolutionError.IllegalType(tpe, loc))
      case _: UnkindedType.AssocType => Result.Err(ResolutionError.IllegalType(tpe, loc))

      // Case 6: Unexpected type. Crash.
      case t: UnkindedType.Apply => throw InternalCompilerException(s"unexpected type: $t", loc)
      case t: UnkindedType.UnappliedAlias => throw InternalCompilerException(s"unexpected type: $t", loc)
      case t: UnkindedType.UnappliedAssocType => throw InternalCompilerException(s"unexpected type: $t", loc)
      case t: UnkindedType.Alias => throw InternalCompilerException(s"unexpected type: $t", loc)
    }
  }

  /**
    * Returns the class object for an array with elements of the given `elmClass` type.
    */
  private def getJVMArrayType(elmClass: Class[_]): Class[_] = {
    // See: https://stackoverflow.com/questions/1679421/how-to-get-the-array-class-for-a-given-class-in-java
    java.lang.reflect.Array.newInstance(elmClass, 0).getClass
  }

  private def isBaseTypeUnit(tpe: UnkindedType): Boolean = {
    val erased = UnkindedType.eraseAliases(tpe)
    val baseType = erased.baseType
    baseType match {
      // Case 1: Constant: Match on the type.
      case UnkindedType.Cst(tc, _) => tc match {
        case TypeConstructor.Unit => true
        case _ => false
      }
      case _ => false
    }
  }

  /**
    * Construct the type alias type constructor for the given symbol `sym` with the given kind `k`.
    */
private def mkUnappliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation): UnkindedType = UnkindedType.UnappliedAlias(sym, loc)

  /**
    * Construct the associated type constructor for the given symbol `sym` with the given kind `k`.
    */
private def mkUnappliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation): UnkindedType = UnkindedType.UnappliedAssocType(sym, loc)

  /**
    * Gets the proper symbol from the given named symbol.
    */
  private def getSym(symbol: NamedAst.Declaration): Symbol = symbol match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, loc) => sym
    case NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superClasses, _, sigs, laws, loc) => sym
    case NamedAst.Declaration.Sig(sym, spec, exp) => sym
    case NamedAst.Declaration.Def(sym, spec, exp) => sym
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym
    case NamedAst.Declaration.Struct(doc, ann, mod, sym, tparams, fields, indices, loc) => sym
    case NamedAst.Declaration.StructField(_, sym, _, _) => sym
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym
    case NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc) => sym
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, tpe, loc) => sym
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym
    case NamedAst.Declaration.Op(sym, spec) => sym
    case NamedAst.Declaration.Case(sym, tpe, _) => sym
    case NamedAst.Declaration.RestrictableCase(sym, tpe, _) => sym
    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, args, tpe, loc) => throw InternalCompilerException("unexpected associated type definition", loc)
    case NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, _, defs, ns, loc) => throw InternalCompilerException("unexpected instance", loc)
  }

  /**
    * Resolves the symbol where the symbol is known to point to a valid declaration.
    */
  private def infallableLookupSym(sym: Symbol, root: NamedAst.Root)(implicit flix: Flix): List[NamedAst.Declaration] = sym match {
    case sym: Symbol.DefnSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.StructSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableEnumSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.CaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.StructFieldSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.RestrictableCaseSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.TraitSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.SigSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.TypeAliasSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.AssocTypeSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.EffectSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.OpSym => root.symbols(Name.mkUnlocatedNName(sym.namespace))(sym.name)
    case sym: Symbol.ModuleSym => root.symbols(Name.mkUnlocatedNName(sym.ns.init))(sym.ns.last)
    case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
    case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol $sym", SourceLocation.Unknown)
    case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol $sym", sym.loc)
  }

  /**
    * Resolves the given Use.
    */
  private def visitUseOrImport(useOrImport: NamedAst.UseOrImport, ns: Name.NName, root: NamedAst.Root)(implicit flix: Flix): Validation[Ast.UseOrImport, ResolutionError] = useOrImport match {
    case NamedAst.UseOrImport.Use(qname, alias, loc) => tryLookupName(qname, ListMap.empty, ns, root) match {
      // Case 1: No matches. Error.
      case Nil => Validation.toHardFailure(ResolutionError.UndefinedNameUnrecoverable(qname, ns, Map.empty, isUse = true, loc))
      // Case 2: A match. Map it to a use.
      // TODO NS-REFACTOR: should map to multiple uses or ignore namespaces or something
      case Resolution.Declaration(d) :: _ =>
        Validation.success(Ast.UseOrImport.Use(getSym(d), alias, loc))
      // Case 3: Impossible. Hard error.
      case _ => throw InternalCompilerException("unexpected conflicted imports", loc)
    }

    case NamedAst.UseOrImport.Import(name, alias, loc) =>
      val clazzVal = lookupJvmClass(name.toString, loc).toValidation
      mapN(clazzVal) {
        case clazz => Ast.UseOrImport.Import(clazz, alias, loc)
      }
  }

  /**
    * Adds the given use or import to the use environment.
    */
  private def appendUseEnv(env: ListMap[String, Resolution], useOrImport: Ast.UseOrImport, root: NamedAst.Root)(implicit flix: Flix): ListMap[String, Resolution] = useOrImport match {
    case Ast.UseOrImport.Use(sym, alias, loc) =>
      val decls = infallableLookupSym(sym, root)
      decls.foldLeft(env) {
        case (acc, decl) => acc + (alias.name -> Resolution.Declaration(decl))
      }
    case Ast.UseOrImport.Import(clazz, alias, loc) => env + (alias.name -> Resolution.JavaClass(clazz))
  }

  /**
    * Adds the given uses and imports to the use environment.
    */
  private def appendAllUseEnv(env: ListMap[String, Resolution], usesAndImports: List[Ast.UseOrImport], root: NamedAst.Root)(implicit flix: Flix): ListMap[String, Resolution] = {
    usesAndImports.foldLeft(env)(appendUseEnv(_, _, root))
  }

  /**
    * Creates a use environment from the given type parameters.
    */
  private def mkTypeParamEnv(tparams: List[ResolvedAst.TypeParam]): ListMap[String, Resolution] = {
    tparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, tparam) => acc + (tparam.name.name -> Resolution.TypeVar(tparam.sym))
    }
  }

  /**
    * Creates a use environment from the given formal parameters.
    */
  private def mkFormalParamEnv(fparams: List[ResolvedAst.FormalParam]): ListMap[String, Resolution] = {
    fparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, fparam) => acc + (fparam.sym.text -> Resolution.Var(fparam.sym))
    }
  }

  /**
    * Creates an environment from the given constraint parameters.
    */
  private def mkConstraintParamEnv(cparams: List[ResolvedAst.ConstraintParam]): ListMap[String, Resolution] = {
    cparams.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, cparam) => acc + (cparam.sym.text -> Resolution.Var(cparam.sym))
    }
  }

  /**
    * Creates an environment from the given spec.
    */
  private def mkSpecEnv(spec: ResolvedAst.Spec): ListMap[String, Resolution] = spec match {
    case ResolvedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstrs, econstrs, loc) =>
      mkTypeParamEnv(tparams) ++ mkFormalParamEnv(fparams)
  }

  /**
    * Creates an environment from the given pattern.
    */
  @tailrec
  private def mkPatternEnv(pat0: ResolvedAst.Pattern): ListMap[String, Resolution] = pat0 match {
    case ResolvedAst.Pattern.Wild(loc) => ListMap.empty
    case ResolvedAst.Pattern.Var(sym, loc) => mkVarEnv(sym)
    case ResolvedAst.Pattern.Cst(cst, loc) => ListMap.empty
    case ResolvedAst.Pattern.Tag(sym, pat, loc) => mkPatternEnv(pat)
    case ResolvedAst.Pattern.Tuple(elms, loc) => mkPatternsEnv(elms)
    case ResolvedAst.Pattern.Record(pats, pat, _) => mkRecordPatternEnv(pats, pat)
    case ResolvedAst.Pattern.RecordEmpty(_) => ListMap.empty
    case ResolvedAst.Pattern.Error(_) => ListMap.empty
  }

  /**
    * Creates an environment from the given record pattern.
    */
  private def mkRecordPatternEnv(pats: List[Record.RecordLabelPattern], pat: ResolvedAst.Pattern): ListMap[String, Resolution] = {
    mkPatternsEnv(pats.map(_.pat)) ++ mkPatternEnv(pat)
  }

  /**
    * Creates an environment from the given patterns.
    */
  private def mkPatternsEnv(pats: List[ResolvedAst.Pattern]): ListMap[String, Resolution] = {
    pats.foldLeft(ListMap.empty[String, Resolution]) {
      case (acc, pat) => acc ++ mkPatternEnv(pat)
    }
  }

  /**
    * Creates an environment from the given variable symbol.
    */
  private def mkVarEnv(sym: Symbol.VarSym): ListMap[String, Resolution] = ListMap.singleton(sym.text, Resolution.Var(sym))

  /**
    * Creates an environment from the given type variable symbol.
    */
  private def mkTypeVarEnv(sym: Symbol.UnkindedTypeVarSym): ListMap[String, Resolution] = {
    sym.text match {
      case VarText.Absent => throw InternalCompilerException("unexpected unnamed type var sym", sym.loc)
      case VarText.SourceText(s) => ListMap.singleton(s, Resolution.TypeVar(sym))
    }
  }

  /**
    * Builds a variable environment from the given resolution environment.
    */
  private def filterToVarEnv(env: ListMap[String, Resolution]): Map[String, Symbol.VarSym] = {
    env.m.flatMap {
      case (name, res) => res.collectFirst {
        case Resolution.Var(sym) => (name, sym)
      }
    }
  }

  /**
    * Converts the class into a Flix type.
    */
  private def flixifyType(clazz: Class[_], loc: SourceLocation): UnkindedType = clazz.getName match {
    case "java.math.BigDecimal" => UnkindedType.Cst(TypeConstructor.BigDecimal, loc)
    case "java.math.BigInteger" => UnkindedType.Cst(TypeConstructor.BigInt, loc)
    case "java.lang.String" => UnkindedType.Cst(TypeConstructor.Str, loc)
    case "java.util.regex.Pattern" => UnkindedType.Cst(TypeConstructor.Regex, loc)
    case "java.util.function.Function" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.Consumer" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.Predicate" => UnkindedType.mkIoArrow(UnkindedType.mkObject(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntFunction" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.IntConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.IntPredicate" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.IntUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkInt32(loc), UnkindedType.mkInt32(loc), loc)
    case "java.util.function.LongFunction" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.LongConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.LongPredicate" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.LongUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkInt64(loc), UnkindedType.mkInt64(loc), loc)
    case "java.util.function.DoubleFunction" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkObject(loc), loc)
    case "java.util.function.DoubleConsumer" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkUnit(loc), loc)
    case "java.util.function.DoublePredicate" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkBool(loc), loc)
    case "java.util.function.DoubleUnaryOperator" => UnkindedType.mkIoArrow(UnkindedType.mkFloat64(loc), UnkindedType.mkFloat64(loc), loc)
    case _ => UnkindedType.Cst(TypeConstructor.Native(clazz), loc)
  }

  /**
    * Checks that the operator's arity matches the number of arguments given.
    */
  private def checkOpArity(op: Declaration.Op, numArgs: Int, loc: SourceLocation): Validation[Unit, ResolutionError] = {
    if (op.spec.fparams.length == numArgs) {
      Validation.success(())
    } else {
      Validation.toSoftFailure((), ResolutionError.MismatchedOpArity(op.sym, op.spec.fparams.length, numArgs, loc))
    }
  }

  /**
    * Enum describing the extent to which a class is accessible.
    */
  private sealed trait TraitAccessibility

  private object TraitAccessibility {
    case object Accessible extends TraitAccessibility

    case object Sealed extends TraitAccessibility

    case object Inaccessible extends TraitAccessibility
  }

  /**
    * Union of variables, definitions, and signatures.
    */
  private sealed trait ResolvedQName

  private object ResolvedQName {
    case class Var(sym: Symbol.VarSym) extends ResolvedQName

    case class Def(defn: NamedAst.Declaration.Def) extends ResolvedQName

    case class Sig(sig: NamedAst.Declaration.Sig) extends ResolvedQName

    case class Tag(caze: NamedAst.Declaration.Case) extends ResolvedQName

    case class RestrictableTag(caze: NamedAst.Declaration.RestrictableCase) extends ResolvedQName

    case class Error(e: ResolutionError.UndefinedName) extends ResolvedQName
  }

  /**
    * Result of a name resolution.
    */
  private sealed trait Resolution

  private object Resolution {
    case class Declaration(decl: NamedAst.Declaration) extends Resolution

    case class JavaClass(clazz: Class[_]) extends Resolution

    case class Var(sym: Symbol.VarSym) extends Resolution

    case class TypeVar(sym: Symbol.UnkindedTypeVarSym) extends Resolution
  }

  /**
    * Enum indicating whether a variable may be a wildcard.
    */
  private sealed trait Wildness

  private object Wildness {
    case object AllowWild extends Wildness

    case object ForbidWild extends Wildness
  }

  /**
    * A table of all the symbols in the program.
    */
  private case class SymbolTable(traits: Map[Symbol.TraitSym, ResolvedAst.Declaration.Trait],
                                 instances: ListMap[Symbol.TraitSym, ResolvedAst.Declaration.Instance],
                                 defs: Map[Symbol.DefnSym, ResolvedAst.Declaration.Def],
                                 enums: Map[Symbol.EnumSym, ResolvedAst.Declaration.Enum],
                                 structs: Map[Symbol.StructSym, ResolvedAst.Declaration.Struct],
                                 structFields: Map[Symbol.StructFieldSym, ResolvedAst.Declaration.StructField],
                                 restrictableEnums: Map[Symbol.RestrictableEnumSym, ResolvedAst.Declaration.RestrictableEnum],
                                 effects: Map[Symbol.EffectSym, ResolvedAst.Declaration.Effect],
                                 typeAliases: Map[Symbol.TypeAliasSym, ResolvedAst.Declaration.TypeAlias]) {
    def addTrait(trt: ResolvedAst.Declaration.Trait): SymbolTable = copy(traits = traits + (trt.sym -> trt))

    def addDef(defn: ResolvedAst.Declaration.Def): SymbolTable = copy(defs = defs + (defn.sym -> defn))

    def addEnum(enum0: ResolvedAst.Declaration.Enum): SymbolTable = copy(enums = enums + (enum0.sym -> enum0))

    def addStruct(struct: ResolvedAst.Declaration.Struct): SymbolTable = copy(structs = structs + (struct.sym -> struct))

    def addStructField(field: ResolvedAst.Declaration.StructField): SymbolTable = copy(structFields = structFields + (field.sym -> field))

    def addRestrictableEnum(enum0: ResolvedAst.Declaration.RestrictableEnum): SymbolTable = copy(restrictableEnums = restrictableEnums + (enum0.sym -> enum0))

    def addEffect(effect: ResolvedAst.Declaration.Effect): SymbolTable = copy(effects = effects + (effect.sym -> effect))

    def addTypeAlias(alias: ResolvedAst.Declaration.TypeAlias): SymbolTable = copy(typeAliases = typeAliases + (alias.sym -> alias))

    def addInstance(inst: ResolvedAst.Declaration.Instance): SymbolTable = copy(instances = instances + (inst.trt.sym -> inst))

    def ++(that: SymbolTable): SymbolTable = {
      SymbolTable(
        traits = this.traits ++ that.traits,
        instances = this.instances ++ that.instances,
        defs = this.defs ++ that.defs,
        enums = this.enums ++ that.enums,
        structs = this.structs ++ that.structs,
        structFields = this.structFields ++ that.structFields,
        restrictableEnums = this.restrictableEnums ++ that.restrictableEnums,
        effects = this.effects ++ that.effects,
        typeAliases = this.typeAliases ++ that.typeAliases
      )
    }
  }

  private object SymbolTable {
    val empty: SymbolTable = SymbolTable(Map.empty, ListMap.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

    /**
      * Traverses `xs`, gathering the symbols from each element by applying the function `f`.
      */
    def traverse[T](xs: Iterable[T])(f: T => SymbolTable): SymbolTable = {
      xs.foldLeft(SymbolTable.empty) {
        case (acc, x) => acc ++ f(x)
      }
    }
  }
}
