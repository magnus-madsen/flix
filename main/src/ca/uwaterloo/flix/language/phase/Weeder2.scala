/*
 * Copyright 2024 Herluf Baggesen
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.*
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.shared.{Annotation, Annotations, CheckedCastType, Constant, Denotation, Doc, Fixity, Modifier, Modifiers, Polarity}
import ca.uwaterloo.flix.language.ast.{ChangeSet, Name, ReadAst, SemanticOp, SourceLocation, Symbol, SyntaxTree, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.ParseError.*
import ca.uwaterloo.flix.language.errors.{Recoverable, WeederError}
import ca.uwaterloo.flix.language.errors.WeederError.*
import ca.uwaterloo.flix.util.Validation.*
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

import java.lang.{Byte as JByte, Integer as JInt, Long as JLong, Short as JShort}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.regex.{PatternSyntaxException, Pattern as JPattern}
import scala.annotation.tailrec
import scala.collection.immutable.{::, List, Nil}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Weeder2 walks a [[Tree]], validates it and thereby transforms it into a [[WeededAst]].
  *
  * Function names in Weeder2 follow this pattern:
  *   1. visit* : Visits a Tree of a known kind. These functions assert that the kind is indeed known.
  *   1. pick* : Picks first sub-Tree of a kind and visits it.
  *   1. tryPick* : Works like pick* but only runs the visitor if the child of kind is found. Returns an option containing the result.
  *   1. pickAll* : These will pick all subtrees of a specified kind and run a visitor on it.
  */
object Weeder2 {

  /**
    * The maximum nesting of expressions allowed.
    *
    * This is here to avoid stack overflow during compiler phases.
    */
  private val ExpressionNestingLimit = 250

  import WeededAst.*

  def run(readRoot: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], root: SyntaxTree.Root, oldRoot: WeededAst.Root, changeSet: ChangeSet)(implicit flix: Flix): (Validation[WeededAst.Root, CompilationMessage], List[CompilationMessage]) = {
    flix.phaseNew("Weeder2") {
      implicit val sctx: SharedContext = SharedContext.mk()
      val (stale, fresh) = changeSet.partition(root.units, oldRoot.units)
      // Parse each source file in parallel and join them into a WeededAst.Root
      val refreshed = ParOps.parMap(stale) {
        case (src, tree) => mapN(weed(tree))(tree => src -> tree)
      }

      val compilationUnits = mapN(sequence(refreshed))(_.toMap ++ fresh)
      (mapN(compilationUnits)(WeededAst.Root(_, entryPoint, readRoot.names)), sctx.errors.asScala.toList)
    }(DebugValidation())
  }

  private def weed(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[CompilationUnit, CompilationMessage] = {
    mapN(pickAllUsesAndImports(tree), Decls.pickAllDeclarations(tree)) {
      (usesAndImports, declarations) => CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def pickAllUsesAndImports(tree: Tree)(implicit sctx: SharedContext): Validation[List[UseOrImport], CompilationMessage] = {
    expectAny(tree, List(TreeKind.Root, TreeKind.Decl.Module))
    val maybeTree = tryPick(TreeKind.UsesOrImports.UseOrImportList, tree)
    val maybeUsesAndImports = traverseOpt(maybeTree) {
      tree =>
        val uses = pickAll(TreeKind.UsesOrImports.Use, tree)
        val imports = pickAll(TreeKind.UsesOrImports.Import, tree)
        mapN(traverse(uses)(visitUse), traverse(imports)(visitImport)) {
          (uses, imports) => uses.flatten ++ imports.flatten
        }
    }
    mapN(maybeUsesAndImports) {
      case Some(usesAndImports) => usesAndImports
      case None => List.empty
    }
  }

  private def visitUse(tree: Tree)(implicit sctx: SharedContext): Validation[List[UseOrImport], CompilationMessage] = {
    expect(tree, TreeKind.UsesOrImports.Use)
    val maybeUseMany = tryPick(TreeKind.UsesOrImports.UseMany, tree)
    mapN(pickQName(tree)) { qname =>
      val isTopLevelName = qname.namespace.idents.isEmpty
      val isNotImportedByUse = maybeUseMany.isEmpty
      val isUnqualifiedUse = isTopLevelName && isNotImportedByUse
      if (isUnqualifiedUse) {
        val error = UnqualifiedUse(qname.loc)
        sctx.errors.add(error)
        List.empty
      } else {
        val nname = Name.NName(qname.namespace.idents :+ qname.ident, qname.loc)
        val optUses = maybeUseMany.map(visitUseMany(_, nname))
        optUses match {
          // case: use many, if uses is empty the parser will have reported an error
          case Some(uses) => uses

          // case: use one, use the qname
          case None => List(UseOrImport.Use(qname, qname.ident, qname.loc))
        }
      }
    }
  }

  private def visitUseMany(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): List[UseOrImport] = {
    expect(tree, TreeKind.UsesOrImports.UseMany)
    val identUses = pickAll(TreeKind.Ident, tree).map(visitUseIdent(_, namespace))
    val aliasedUses = pickAll(TreeKind.UsesOrImports.Alias, tree).map(visitUseAlias(_, namespace))
    (identUses ++ aliasedUses).sortBy(_.loc)
  }

  private def visitUseIdent(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): UseOrImport.Use = {
    val ident = tokenToIdent(tree)
    UseOrImport.Use(Name.QName(namespace, ident, tree.loc), ident, ident.loc)
  }

  private def visitUseAlias(tree: Tree, namespace: Name.NName)(implicit sctx: SharedContext): UseOrImport.Use = {
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    idents match {
      case ident :: alias :: _ =>
        // Check for illegal alias
        val isIllegalAlias = (ident.name.nonEmpty && alias.name.nonEmpty) && ident.name.charAt(0).isUpper != alias.name.charAt(0).isUpper
        if (isIllegalAlias) {
          val error = IllegalUse(ident.name, alias.name, tree.loc)
          sctx.errors.add(error)
        }
        val qname = Name.QName(namespace, ident, tree.loc)
        UseOrImport.Use(qname, alias, tree.loc)

      // recover from missing alias by using ident
      case ident :: _ =>
        val error = Malformed(NamedTokenSet.Alias, SyntacticContext.Use, hint = Some(s"Give an alias after ${TokenKind.ArrowThickR.display}."), loc = tree.loc)
        sctx.errors.add(error)
        val qname = Name.QName(namespace, ident, tree.loc)
        UseOrImport.Use(qname, ident, ident.loc)

      case _ => throw InternalCompilerException("Parser passed malformed use with alias", tree.loc)
    }
  }

  private def visitImport(tree: Tree)(implicit sctx: SharedContext): Validation[List[UseOrImport], CompilationMessage] = {
    expect(tree, TreeKind.UsesOrImports.Import)
    mapN(pickJavaName(tree)) {
      jname =>
        val maybeImportMany = tryPick(TreeKind.UsesOrImports.ImportMany, tree).map(visitImportMany(_, jname.fqn))
        maybeImportMany match {
          // case: import many, if the list is empty parser has reported an error
          case Some(imports) => imports

          // case: import one, use the java name
          case None =>
            val ident = Name.Ident(jname.fqn.lastOption.getOrElse(""), jname.loc)
            List(UseOrImport.Import(jname, ident, tree.loc))
        }
    }
  }

  private def visitImportMany(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): List[UseOrImport.Import] = {
    expect(tree, TreeKind.UsesOrImports.ImportMany)
    val identImports = pickAll(TreeKind.Ident, tree).map(visitImportIdent(_, namespace))
    val aliasedImports = pickAll(TreeKind.UsesOrImports.Alias, tree).map(visitImportAlias(_, namespace))
    (identImports ++ aliasedImports).sortBy(_.loc)
  }

  private def visitImportIdent(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): UseOrImport.Import = {
    val ident = tokenToIdent(tree)
    UseOrImport.Import(Name.JavaName(namespace ++ Seq(ident.name), tree.loc), ident, ident.loc)
  }

  private def visitImportAlias(tree: Tree, namespace: Seq[String])(implicit sctx: SharedContext): UseOrImport.Import = {
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    idents match {
      case ident :: alias :: _ =>
        val jname = Name.JavaName(namespace :+ ident.name, tree.loc)
        UseOrImport.Import(jname, alias, tree.loc)
      // recover from missing alias by using ident
      case ident :: _ =>
        val error = Malformed(NamedTokenSet.Alias, SyntacticContext.Import, hint = Some(s"Give an alias after ${TokenKind.ArrowThickR.display}."), loc = tree.loc)
        sctx.errors.add(error)
        UseOrImport.Import(Name.JavaName(Seq(ident.name), tree.loc), ident, ident.loc)
      case _ => throw InternalCompilerException("Parser passed malformed use with alias", tree.loc)
    }
  }

  private object Decls {
    def pickAllDeclarations(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[List[Declaration], CompilationMessage] = {
      expectAny(tree, List(TreeKind.Root, TreeKind.Decl.Module))
      val modules = pickAll(TreeKind.Decl.Module, tree)
      val traits = pickAll(TreeKind.Decl.Trait, tree)
      val instances = pickAll(TreeKind.Decl.Instance, tree)
      val definitions = pickAll(TreeKind.Decl.Def, tree)
      val enums = pickAll(TreeKind.Decl.Enum, tree)
      val restrictableEnums = pickAll(TreeKind.Decl.RestrictableEnum, tree)
      val structs = pickAll(TreeKind.Decl.Struct, tree)
      val typeAliases = pickAll(TreeKind.Decl.TypeAlias, tree)
      val effects = pickAll(TreeKind.Decl.Effect, tree)
      mapN(
        traverse(modules)(visitModuleDecl),
        traverse(traits)(visitTraitDecl),
        traverse(instances)(visitInstanceDecl),
        traverse(definitions)(visitDefinitionDecl(_)),
        traverse(enums)(visitEnumDecl),
        traverse(structs)(visitStructDecl),
        traverse(restrictableEnums)(visitRestrictableEnumDecl),
        traverse(typeAliases)(visitTypeAliasDecl),
        traverse(effects)(visitEffectDecl)
      ) {
        case (modules, traits, instances, definitions, enums, rEnums, structs, typeAliases, effects) =>
          (modules ++ traits ++ instances ++ definitions ++ enums ++ rEnums ++ structs ++ typeAliases ++ effects).sortBy(_.loc)
      }
    }

    private def visitModuleDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[Declaration.Namespace, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Module)
      mapN(
        pickQName(tree),
        pickAllUsesAndImports(tree),
        pickAllDeclarations(tree)
      ) {
        (qname, usesAndImports, declarations) =>
          val base = Declaration.Namespace(qname.ident, usesAndImports, declarations, tree.loc)
          qname.namespace.idents.foldRight(base: Declaration.Namespace) {
            case (ident, acc) => Declaration.Namespace(ident, Nil, List(acc), tree.loc)
          }
      }
    }

    private def visitTraitDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Trait, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Trait)
      implicit val lctx: LocalContext = LocalContext.mk()
      val sigs = pickAll(TreeKind.Decl.Signature, tree)
      val laws = pickAll(TreeKind.Decl.Law, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordLawful, TokenKind.KeywordPub, TokenKind.KeywordSealed))
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickSingleParameter(tree),
        Types.pickConstraints(tree),
        traverse(sigs)(visitSignatureDecl),
        traverse(laws)(visitLawDecl)
      ) {
        (doc, ident, tparam, tconstr, sigs, laws) =>
          val assocs = pickAll(TreeKind.Decl.AssociatedTypeSig, tree)
          mapN(traverse(assocs)(visitAssociatedTypeSigDecl(_, tparam))) {
            assocs => Declaration.Trait(doc, ann, mod, ident, tparam, tconstr, assocs, sigs, laws, tree.loc)
          }
      }
    }

    private def visitInstanceDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[Declaration.Instance, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Instance)
      val allowedDefModifiers: Set[TokenKind] = if (flix.options.xnodeprecated) Set(TokenKind.KeywordPub) else Set(TokenKind.KeywordPub, TokenKind.KeywordOverride)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set.empty)
      flatMapN(
        pickDocumentation(tree),
        pickQName(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
        traverse(pickAll(TreeKind.Decl.Def, tree))(visitDefinitionDecl(_, allowedModifiers = allowedDefModifiers, mustBePublic = true)),
        traverse(pickAll(TreeKind.Decl.Redef, tree))(visitRedefinitionDecl),
      ) {
        (doc, clazz, tpe, tconstrs, defs, redefs) =>
          val assocs = pickAll(TreeKind.Decl.AssociatedTypeDef, tree)
          mapN(traverse(assocs)(visitAssociatedTypeDefDecl(_, tpe))) {
            assocs => Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, assocs, defs, redefs, tree.loc)
          }
      }
    }

    private def visitSignatureDecl(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Declaration.Sig, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Signature)
      val maybeExpression = tryPick(TreeKind.Expr.Expr, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub), mustBePublic = true)
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Types.pickType(tree),
        Types.tryPickEffect(tree),
        Types.pickConstraints(tree),
        pickEqualityConstraints(tree),
        traverseOpt(maybeExpression)(Exprs.visitExpr)
      ) {
        (doc, ident, tparams, fparams, tpe, eff, tconstrs, econstrs, expr) =>
          Declaration.Sig(doc, ann, mod, ident, tparams, fparams, expr, tpe, eff, tconstrs, econstrs, tree.loc)
      }
    }

    private def visitDefinitionDecl(tree: Tree, allowedModifiers: Set[TokenKind] = Set(TokenKind.KeywordPub), mustBePublic: Boolean = false)(implicit sctx: SharedContext): Validation[Declaration.Def, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Def)
      implicit val lctx: LocalContext = LocalContext.mk()
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = allowedModifiers, mustBePublic)
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Exprs.pickExpr(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
        pickEqualityConstraints(tree),
        Types.tryPickEffect(tree)
      ) {
        (doc, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
          Declaration.Def(doc, ann, mod, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
      }
    }

    private def visitRedefinitionDecl(tree: Tree)(implicit sctx: SharedContext, flix: Flix): Validation[Declaration.Redef, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Redef)
      implicit val lctx: LocalContext = LocalContext.mk()
      val allowedModifiers: Set[TokenKind] = if (flix.options.xnodeprecated) Set.empty else Set(TokenKind.KeywordPub)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = allowedModifiers)
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Exprs.pickExpr(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
        pickEqualityConstraints(tree),
        Types.tryPickEffect(tree)
      ) {
        (doc, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
          Declaration.Redef(doc, ann, mod, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
      }
    }

    private def visitLawDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Def, CompilationMessage] = {
      implicit val lctx: LocalContext = LocalContext.mk()
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set.empty)
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickConstraints(tree),
        pickEqualityConstraints(tree),
        Types.pickKindedParameters(tree),
        pickFormalParameters(tree),
        Exprs.pickExpr(tree)
      ) {
        (doc, ident, tconstrs, econstrs, tparams, fparams, expr) =>
          val eff = None
          val tpe = WeededAst.Type.Ambiguous(Name.mkQName("Bool"), ident.loc)
          // TODO: There is a `Declaration.Law` but old Weeder produces a Def
          Declaration.Def(doc, ann, mod, ident, tparams, fparams, expr, tpe, eff, tconstrs, econstrs, tree.loc)
      }
    }

    private def visitEnumDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Enum, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Enum)
      val shorthandTuple = tryPick(TreeKind.Type.Type, tree)
      val cases = pickAll(TreeKind.Case, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val derivations = Types.pickDerivations(tree)
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
        traverseOpt(shorthandTuple)(Types.visitType),
        traverse(cases)(visitEnumCase)
      ) {
        (doc, ident, tparams, tpe, cases) =>
          val casesVal = (tpe, cases) match {
            // Empty singleton enum
            case (Some(Type.Error(_)), Nil) =>
              // Fall back on no cases, parser has already reported an error
              Validation.Success(List.empty)
            // Singleton enum
            case (Some(t), cs) =>
              // Error if both singleton shorthand and cases provided
              // Treat this as an implicit case with the type t, e.g.,
              // enum Foo(Int32) { case Bar, case Baz }
              // ===>
              // enum Foo { case Foo(Int32), case Bar, case Baz }
              val syntheticCase = WeededAst.Case(ident, flattenEnumCaseType(t), ident.loc)
              val allCases = syntheticCase :: cs
              val errors = getDuplicates(allCases, (c: Case) => c.ident.name).map {
                case (left, right) => DuplicateTag(ident.name, left.ident, left.loc, right.loc)
              }
              errors.foreach(sctx.errors.add)
              Validation.Success(allCases)
            // Empty or Multiton enum
            case (None, cs) =>
              val errors = getDuplicates(cs, (c: Case) => c.ident.name).map {
                case (left, right) => DuplicateTag(ident.name, left.ident, left.loc, right.loc)
              }
              errors.foreach(sctx.errors.add)
              Validation.Success(cases)
          }
          mapN(casesVal) {
            cases => Declaration.Enum(doc, ann, mod, ident, tparams, derivations, cases.sortBy(_.loc), tree.loc)
          }
      }
    }

    private def visitEnumCase(tree: Tree)(implicit sctx: SharedContext): Validation[Case, CompilationMessage] = {
      expect(tree, TreeKind.Case)
      val maybeType = tryPick(TreeKind.Type.Type, tree)
      mapN(
        pickNameIdent(tree),
        traverseOpt(maybeType)(Types.visitType),
        // TODO: Doc comments on enum cases. It is not available on [[Case]] yet.
      ) {
        (ident, maybeType) =>
          val tpe = maybeType
            .map(flattenEnumCaseType)
            .getOrElse(Type.Unit(ident.loc))
          // Make a source location that spans the name and type, excluding 'case'.
          val loc = SourceLocation(isReal = true, ident.loc.sp1, tree.loc.sp2)
          Case(ident, tpe, loc)
      }
    }

    private def flattenEnumCaseType(tpe: Type): Type = {
      tpe match {
        // Single type in case -> flatten to ambiguous.
        case Type.Tuple(t :: Nil, _) => t
        // Multiple types in case -> do nothing
        case tpe => tpe
      }
    }

    private def visitRestrictableEnumDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.RestrictableEnum, CompilationMessage] = {
      expect(tree, TreeKind.Decl.RestrictableEnum)
      val shorthandTuple = tryPick(TreeKind.Type.Type, tree)
      val restrictionParam = flatMapN(pick(TreeKind.Parameter, tree))(Types.visitParameter)
      val cases = pickAll(TreeKind.Case, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      val derivations = Types.pickDerivations(tree)
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        restrictionParam,
        Types.pickParameters(tree),
        traverseOpt(shorthandTuple)(Types.visitType),
        traverse(cases)(visitRestrictableEnumCase)
      ) {
        (doc, ident, rParam, tparams, tpe, cases) =>
          val casesVal = (tpe, cases) match {
            // Empty singleton enum
            case (Some(Type.Error(_)), Nil) =>
              // Fall back on no cases, parser has already reported an error
              Validation.Success(List.empty)
            // Singleton enum
            case (Some(t), cs) =>
              // Error if both singleton shorthand and cases provided
              // Treat this as an implicit case with the type t, e.g.,
              // enum Foo(Int32) { case Bar, case Baz }
              // ===>
              // enum Foo { case Foo(Int32), case Bar, case Baz }
              val syntheticCase = WeededAst.RestrictableCase(ident, flattenEnumCaseType(t), ident.loc)
              val allCases = syntheticCase :: cs
              val errors = getDuplicates(allCases, (c: RestrictableCase) => c.ident.name).map {
                case (left, right) => DuplicateTag(ident.name, left.ident, left.loc, right.loc)
              }
              errors.foreach(sctx.errors.add)
              Validation.Success(allCases)
            // Empty or Multiton enum
            case (None, cs) =>
              val errors = getDuplicates(cs, (c: RestrictableCase) => c.ident.name).map {
                case (left, right) => DuplicateTag(ident.name, left.ident, left.loc, right.loc)
              }
              errors.foreach(sctx.errors.add)
              Validation.Success(cases)
          }
          mapN(casesVal) {
            cases => Declaration.RestrictableEnum(doc, ann, mod, ident, rParam, tparams, derivations, cases.sortBy(_.loc), tree.loc)
          }
      }
    }

    private def visitRestrictableEnumCase(tree: Tree)(implicit sctx: SharedContext): Validation[RestrictableCase, CompilationMessage] = {
      expect(tree, TreeKind.Case)
      val maybeType = tryPick(TreeKind.Type.Type, tree)
      mapN(
        pickNameIdent(tree),
        traverseOpt(maybeType)(Types.visitType),
        // TODO: Doc comments on enum cases. It is not available on [[Case]] yet.
      ) {
        (ident, maybeType) =>
          val tpe = maybeType
            .map(flattenEnumCaseType)
            .getOrElse(Type.Unit(ident.loc))
          RestrictableCase(ident, tpe, tree.loc)
      }
    }

    private def visitStructDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Struct, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Struct)
      val fields = pickAll(TreeKind.StructField, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
        traverse(fields)(visitStructField)
      ) {
        (doc, ident, tparams, fields) =>
          // Ensure that each name is unique
          val errors = getDuplicates(fields, (f: StructField) => f.name.name).map {
            case (field1, field2) => DuplicateStructField(ident.name, field1.name.name, field1.name.loc, field2.name.loc, ident.loc)
          }
          errors.foreach(sctx.errors.add)
          // For each field, only keep the first occurrence of the name
          val groupedByName = fields.groupBy(_.name.name)
          val filteredFields = groupedByName.values.map(_.head).toList
          Validation.Success(Declaration.Struct(doc, ann, mod, ident, tparams, filteredFields.sortBy(_.loc), tree.loc))
      }
    }

    private def visitStructField(tree: Tree)(implicit sctx: SharedContext): Validation[StructField, CompilationMessage] = {
      expect(tree, TreeKind.StructField)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub, TokenKind.KeywordMut))
      mapN(
        pickNameIdent(tree),
        Types.pickType(tree)
      ) {
        (ident, ttype) =>
          // Make a source location that spans the name and type
          val loc = SourceLocation(isReal = true, ident.loc.sp1, tree.loc.sp2)
          StructField(mod, Name.mkLabel(ident), ttype, loc)
      }
    }

    private def visitTypeAliasDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.TypeAlias, CompilationMessage] = {
      expect(tree, TreeKind.Decl.TypeAlias)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, Set(TokenKind.KeywordPub))
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
        Types.pickType(tree)
      ) {
        (doc, ident, tparams, tpe) =>
          Declaration.TypeAlias(doc, ann, mod, ident, tparams, tpe, tree.loc)
      }
    }

    private def visitAssociatedTypeSigDecl(tree: Tree, classTypeParam: TypeParam)(implicit sctx: SharedContext): Validation[Declaration.AssocTypeSig, CompilationMessage] = {
      expect(tree, TreeKind.Decl.AssociatedTypeSig)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickParameters(tree),
      ) {
        (doc, ident, tparams) =>
          val kind = Types.tryPickKind(tree).getOrElse(defaultKind(ident))
          val tpe = Types.tryPickTypeNoWild(tree)
          val tparam = tparams match {
            // Elided: Use class type parameter
            case Nil => Validation.Success(classTypeParam)
            // Single type parameter
            case head :: Nil => Validation.Success(head)
            // Multiple type parameters. Soft fail by picking the first parameter
            case ts@(head :: _ :: _) =>
              val error = NonUnaryAssocType(ts.length, ident.loc)
              sctx.errors.add(error)
              Validation.Success(head)
          }
          mapN(tparam, tpe) {
            (tparam, tpe) => Declaration.AssocTypeSig(doc, mod, ident, tparam, kind, tpe, tree.loc)
          }
      }
    }

    private def visitAssociatedTypeDefDecl(tree: Tree, instType: Type)(implicit sctx: SharedContext): Validation[Declaration.AssocTypeDef, CompilationMessage] = {
      expect(tree, TreeKind.Decl.AssociatedTypeDef)
      val mod = pickModifiers(tree, Set(TokenKind.KeywordPub))
      flatMapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        Types.pickArguments(tree),
        Types.pickType(tree)
      ) {
        (doc, ident, typeArgs, tpe) =>
          val typeArg = typeArgs match {
            // Use instance type if type arguments were elided
            case Nil => Validation.Success(instType)
            // Single argument: use that
            case head :: Nil => Validation.Success(head)
            // Multiple type arguments: recover by arbitrarily picking the first one
            case types =>
              val error = NonUnaryAssocType(types.length, ident.loc)
              sctx.errors.add(error)
              Validation.Success(types.head)
          }
          mapN(typeArg) {
            typeArg => Declaration.AssocTypeDef(doc, mod, ident, typeArg, tpe, tree.loc)
          }
      }
    }

    private def visitEffectDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Effect, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Effect)
      val ops = pickAll(TreeKind.Decl.Op, tree)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set(TokenKind.KeywordPub))
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        traverse(ops)(visitOperationDecl)
      ) {
        (doc, ident, ops) =>
          Declaration.Effect(doc, ann, mod, ident, ops, tree.loc)
      }
    }

    private def visitOperationDecl(tree: Tree)(implicit sctx: SharedContext): Validation[Declaration.Op, CompilationMessage] = {
      expect(tree, TreeKind.Decl.Op)
      val ann = pickAnnotations(tree)
      val mod = pickModifiers(tree, allowed = Set.empty)
      mapN(
        pickDocumentation(tree),
        pickNameIdent(tree),
        pickFormalParameters(tree),
        Types.pickType(tree),
        Types.pickConstraints(tree),
      ) {
        (doc, ident, fparams, tpe, tconstrs) =>
          Declaration.Op(doc, ann, mod, ident, fparams, tpe, tconstrs, tree.loc)
      }
    }

    private def pickDocumentation(tree: Tree): Validation[Doc, CompilationMessage] = {
      val docTree: Option[Tree] = tryPick(TreeKind.Doc, tree).flatMap(tryPick(TreeKind.CommentList, _))
      docTree match {
        case None => Validation.Success(Doc(List.empty, tree.loc))
        case Some(tree) =>
          // strip prefixing `///` and trim
          var lines = text(tree).map(_.stripPrefix("///").trim)
          // Drop first/last line if it is empty
          if (lines.headOption.exists(_.isEmpty)) {
            lines = lines.tail
          }
          if (lines.lastOption.exists(_.isEmpty)) {
            lines = lines.dropRight(1)
          }
          Validation.Success(Doc(lines, tree.loc))
      }
    }

    def pickAnnotations(tree: Tree)(implicit sctx: SharedContext): Annotations = {
      val optAnn = tryPick(TreeKind.AnnotationList, tree)
      val ann = optAnn.map(
          tree => {
            val tokens = pickAllTokens(tree)
            // Check for duplicate annotations
            val errors = getDuplicates(tokens.toSeq, (t: Token) => t.text).map(pair => {
              val name = pair._1.text
              val loc1 = pair._1.mkSourceLocation()
              val loc2 = pair._2.mkSourceLocation()
              DuplicateAnnotation(name.stripPrefix("@"), loc1, loc2)
            })
            errors.foreach(sctx.errors.add)
            tokens.toList.map(visitAnnotation)
          })
        .getOrElse(List.empty)

      Annotations(ann)
    }

    private def visitAnnotation(token: Token)(implicit sctx: SharedContext): Annotation = {
      val loc = token.mkSourceLocation()
      import Annotation.*
      token.text match {
        case "@Deprecated" => Deprecated(loc)
        case "@Experimental" => Experimental(loc)
        case "@Export" => Export(loc)
        case "@Internal" => Internal(loc)
        case "@Parallel" => Parallel(loc)
        case "@ParallelWhenPure" => ParallelWhenPure(loc)
        case "@Lazy" => Lazy(loc)
        case "@LazyWhenPure" => LazyWhenPure(loc)
        case "@MustUse" => MustUse(loc)
        case "@Skip" => Skip(loc)
        case "@Test" | "@test" => Test(loc)
        case "@TailRec" => TailRecursive(loc)
        case other =>
          val error = UndefinedAnnotation(other, loc)
          sctx.errors.add(error)
          Annotation.Error(other.stripPrefix("@"), loc)
      }
    }

    private def pickEqualityConstraints(tree: Tree)(implicit sctx: SharedContext): Validation[List[EqualityConstraint], CompilationMessage] = {
      val maybeConstraintList = tryPick(TreeKind.Decl.EqualityConstraintList, tree)
      val constraints = traverseOpt(maybeConstraintList)(t => {
        val constraintTrees = pickAll(TreeKind.Decl.EqualityConstraintFragment, t)
        traverse(constraintTrees)(visitEqualityConstraint)
      })

      mapN(constraints) {
        case maybeConstrs => maybeConstrs.getOrElse(List.empty).collect {
          case Some(constr) => constr
        }
      }
    }

    private def visitEqualityConstraint(tree: Tree)(implicit sctx: SharedContext): Validation[Option[EqualityConstraint], CompilationMessage] = {
      mapN(traverse(pickAll(TreeKind.Type.Type, tree))(Types.visitType)) {
        case Type.Apply(Type.Ambiguous(qname, _), t1, _) :: t2 :: Nil =>
          Some(EqualityConstraint(qname, t1, t2, tree.loc))

        case _ =>
          val error = IllegalEqualityConstraint(tree.loc)
          sctx.errors.add(error)
          None
      }
    }

    private val ALL_MODIFIERS: Set[TokenKind] = Set(
      TokenKind.KeywordSealed,
      TokenKind.KeywordLawful,
      TokenKind.KeywordPub,
      TokenKind.KeywordOverride,
      TokenKind.KeywordMut,
      TokenKind.KeywordInline)

    private def pickModifiers(tree: Tree, allowed: Set[TokenKind] = ALL_MODIFIERS, mustBePublic: Boolean = false)(implicit sctx: SharedContext): Modifiers = {
      tryPick(TreeKind.ModifierList, tree) match {
        case None => Modifiers(List.empty)
        case Some(modTree) =>
          var errors: List[CompilationMessage] = List.empty
          val tokens = pickAllTokens(modTree)
          // Check if pub is missing
          if (mustBePublic && !tokens.exists(_.kind == TokenKind.KeywordPub)) {
            mapN(pickNameIdent(tree)) {
              ident => errors :+= IllegalPrivateDeclaration(ident, ident.loc)
            }
          }
          // Check for duplicate modifiers
          errors = errors ++ getDuplicates(tokens.toSeq, (t: Token) => t.kind).map(pair => {
            val name = pair._1.text
            val loc1 = pair._1.mkSourceLocation()
            val loc2 = pair._2.mkSourceLocation()
            DuplicateModifier(name, loc2, loc1)
          })
          errors.foreach(sctx.errors.add)

          val mod = tokens.toList.map(visitModifier(_, allowed))
          Modifiers(mod)
      }
    }

    private def visitModifier(token: Token, allowed: Set[TokenKind])(implicit sctx: SharedContext): Modifier = {
      if (!allowed.contains(token.kind)) {
        val error = IllegalModifier(token.mkSourceLocation())
        sctx.errors.add(error)
      }
      token.kind match {
        // TODO: there is no Modifier for 'inline'
        case TokenKind.KeywordSealed => Modifier.Sealed
        case TokenKind.KeywordLawful => Modifier.Lawful
        case TokenKind.KeywordPub => Modifier.Public
        case TokenKind.KeywordMut => Modifier.Mutable
        case TokenKind.KeywordOverride => Modifier.Override
        case kind => throw InternalCompilerException(s"Parser passed unknown modifier '$kind'", token.mkSourceLocation())
      }
    }

    def unitFormalParameter(loc: SourceLocation): FormalParam = FormalParam(
      Name.Ident("_unit", SourceLocation.Unknown),
      Modifiers(List.empty),
      Some(Type.Unit(loc)),
      loc
    )

    def pickFormalParameters(tree: Tree, presence: Presence = Presence.Required)(implicit sctx: SharedContext): Validation[List[FormalParam], CompilationMessage] = {
      tryPick(TreeKind.ParameterList, tree) match {
        case Some(t) =>
          val params = pickAll(TreeKind.Parameter, t)
          if (params.isEmpty) {
            Validation.Success(List(unitFormalParameter(t.loc)))
          } else {
            mapN(traverse(params)(visitParameter(_, presence))) {
              params =>
                // Check for duplicates
                val paramsWithoutWildcards = params.filter(!_.ident.isWild)
                val errors = getDuplicates(paramsWithoutWildcards, (p: FormalParam) => p.ident.name)
                  .map(pair => DuplicateFormalParam(pair._1.ident.name, pair._1.loc, pair._2.loc))
                errors.foreach(sctx.errors.add)

                // Check missing or illegal type ascription
                params
            }
          }
        case None =>
          val error = UnexpectedToken(NamedTokenSet.FromKinds(Set(TokenKind.ParenL)), actual = None, SyntacticContext.Decl.Module, loc = tree.loc)
          sctx.errors.add(error)
          Validation.Success(List(unitFormalParameter(tree.loc)))
      }
    }

    private def visitParameter(tree: Tree, presence: Presence)(implicit sctx: SharedContext): Validation[FormalParam, CompilationMessage] = {
      expect(tree, TreeKind.Parameter)
      val mod = pickModifiers(tree)
      flatMapN(pickNameIdent(tree)) {
        case ident =>
          val maybeType = tryPick(TreeKind.Type.Type, tree)
          // Check for missing or illegal type ascription
          (maybeType, presence) match {
            case (None, Presence.Required) =>
              val error = MissingFormalParamAscription(ident.name, tree.loc)
              sctx.errors.add(error)
              Validation.Success(FormalParam(ident, mod, Some(Type.Error(tree.loc.asSynthetic)), tree.loc))
            case (Some(_), Presence.Forbidden) =>
              val error = IllegalFormalParamAscription(tree.loc)
              sctx.errors.add(error)
              Validation.Success(FormalParam(ident, mod, None, tree.loc))
            case (Some(typeTree), _) => mapN(Types.visitType(typeTree)) { tpe => FormalParam(ident, mod, Some(tpe), tree.loc) }
            case (None, _) => Validation.Success(FormalParam(ident, mod, None, tree.loc))
          }
      }
    }
  }

  private object Exprs {
    def pickExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      val maybeExpression = tryPick(TreeKind.Expr.Expr, tree)
      flatMapN(
        traverseOpt(maybeExpression)(visitExpr)
      ) {
        case Some(expr) => Validation.Success(expr)
        case None =>
          // Fall back on Expr.Error. Parser has reported an error here.
          val err = UnexpectedToken(expected = NamedTokenSet.Expression, actual = None, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          Validation.Success(Expr.Error(err))
      }
    }

    def visitExpr(exprTree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      assert(exprTree.kind == TreeKind.Expr.Expr)
      val tree = unfold(exprTree)

      // Increase the depth and check that it is under the limit.
      lctx.depth += 1
      if (lctx.depth >= ExpressionNestingLimit) {
        val hint = {
          val endsWithCons = tree.kind == TreeKind.Expr.Binary && tryPick(TreeKind.Operator, tree).exists(t => text(t).head == "::")
          if (endsWithCons) Some("Use 'List#{..}' instead of 'x :: y :: Nil' if applicable.")
          else None
        }
        val error = NestedTooDeeply(hint, exprTree.loc)
        sctx.errors.add(error)
        // Decrease depth now that the call is returning.
        lctx.depth -= 1
        return Validation.success(Expr.Error(error))
      }

      val result = tree.kind match {
        case TreeKind.Ident =>
          val ident = tokenToIdent(tree)
          Validation.Success(Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc))
        case TreeKind.QName => Validation.Success(visitQnameExpr(tree))
        case TreeKind.Expr.Paren => visitParenExpr(tree)
        case TreeKind.Expr.Block => visitBlockExpr(tree)
        case TreeKind.Expr.StringInterpolation => visitStringInterpolationExpr(tree)
        case TreeKind.Expr.OpenVariant => visitOpenVariantExpr(tree)
        case TreeKind.Expr.OpenVariantAs => visitOpenVariantAsExpr(tree)
        case TreeKind.Expr.Hole => Validation.Success(visitHoleExpr(tree))
        case TreeKind.Expr.HoleVariable => visitHoleWithExpExpr(tree)
        case TreeKind.Expr.Use => visitExprUseExpr(tree)
        case TreeKind.Expr.Literal => visitLiteralExpr(tree)
        case TreeKind.Expr.Apply => visitApplyExpr(tree)
        case TreeKind.Expr.Lambda => visitLambdaExpr(tree)
        case TreeKind.Expr.LambdaMatch => visitLambdaMatchExpr(tree)
        case TreeKind.Expr.Unary => visitUnaryExpr(tree)
        case TreeKind.Expr.Binary => visitBinaryExpr(tree)
        case TreeKind.Expr.IfThenElse => visitIfThenElseExpr(tree)
        case TreeKind.Expr.Statement => visitStatementExpr(tree)
        case TreeKind.Expr.LocalDef => visitLocalDefExpr(tree)
        case TreeKind.Expr.Scope => visitScopeExpr(tree)
        case TreeKind.Expr.Match => visitMatchExpr(tree)
        case TreeKind.Expr.TypeMatch => visitTypeMatchExpr(tree)
        case TreeKind.Expr.RestrictableChoose
             | TreeKind.Expr.RestrictableChooseStar => visitRestrictableChooseExpr(tree)
        case TreeKind.Expr.ForApplicative => visitForApplicativeExpr(tree)
        case TreeKind.Expr.Foreach => visitForeachExpr(tree)
        case TreeKind.Expr.ForMonadic => visitForMonadicExpr(tree)
        case TreeKind.Expr.GetField => visitGetFieldExpr(tree)
        case TreeKind.Expr.ForeachYield => visitForeachYieldExpr(tree)
        case TreeKind.Expr.LetMatch => visitLetMatchExpr(tree)
        case TreeKind.Expr.Tuple => visitTupleExpr(tree)
        case TreeKind.Expr.LiteralRecord => visitLiteralRecordExpr(tree)
        case TreeKind.Expr.RecordSelect => visitRecordSelectExpr(tree)
        case TreeKind.Expr.RecordOperation => visitRecordOperationExpr(tree)
        case TreeKind.Expr.LiteralArray => visitLiteralArrayExpr(tree)
        case TreeKind.Expr.LiteralVector => visitLiteralVectorExpr(tree)
        case TreeKind.Expr.LiteralList => visitLiteralListExpr(tree)
        case TreeKind.Expr.LiteralMap => visitLiteralMapExpr(tree)
        case TreeKind.Expr.LiteralSet => visitLiteralSetExpr(tree)
        case TreeKind.Expr.Ascribe => visitAscribeExpr(tree)
        case TreeKind.Expr.CheckedTypeCast => visitCheckedTypeCastExpr(tree)
        case TreeKind.Expr.CheckedEffectCast => visitCheckedEffectCastExpr(tree)
        case TreeKind.Expr.UncheckedCast => visitUncheckedCastExpr(tree)
        case TreeKind.Expr.UncheckedMaskingCast => visitUncheckedMaskingCastExpr(tree)
        case TreeKind.Expr.Unsafe => visitUnsafeExpr(tree)
        case TreeKind.Expr.Without => visitWithoutExpr(tree)
        case TreeKind.Expr.Try => visitTryExpr(tree)
        case TreeKind.Expr.Throw => visitThrow(tree)
        case TreeKind.Expr.Index => visitIndexExpr(tree)
        case TreeKind.Expr.IndexMut => visitIndexMutExpr(tree)
        case TreeKind.Expr.InvokeConstructor => visitInvokeConstructorExpr(tree)
        case TreeKind.Expr.InvokeMethod => visitInvokeMethodExpr(tree)
        case TreeKind.Expr.NewObject => visitNewObjectExpr(tree)
        case TreeKind.Expr.NewStruct => visitNewStructExpr(tree)
        case TreeKind.Expr.StructGet => visitStructGetExpr(tree)
        case TreeKind.Expr.StructPut => visitStructPutExpr(tree)
        case TreeKind.Expr.Static => visitStaticExpr(tree)
        case TreeKind.Expr.Select => visitSelectExpr(tree)
        case TreeKind.Expr.Spawn => visitSpawnExpr(tree)
        case TreeKind.Expr.ParYield => visitParYieldExpr(tree)
        case TreeKind.Expr.FixpointConstraintSet => visitFixpointConstraintSetExpr(tree)
        case TreeKind.Expr.FixpointLambda => visitFixpointLambdaExpr(tree)
        case TreeKind.Expr.FixpointInject => visitFixpointInjectExpr(tree)
        case TreeKind.Expr.FixpointSolveWithProject => visitFixpointSolveExpr(tree)
        case TreeKind.Expr.FixpointQuery => visitFixpointQueryExpr(tree)
        case TreeKind.Expr.Debug => visitDebugExpr(tree)
        case TreeKind.Expr.Intrinsic =>
          // Intrinsics must be applied to check that they have the right amount of arguments.
          // This means that intrinsics are not "first-class" like other functions.
          // Something like "let assign = $VECTOR_ASSIGN$" hits this case.
          val error = UnappliedIntrinsic(text(tree).mkString(""), tree.loc)
          sctx.errors.add(error)
          Validation.Success(Expr.Error(error))
        case TreeKind.ErrorTree(err) => Validation.Success(Expr.Error(err))
        case k =>
          throw InternalCompilerException(s"Expected expression, got '$k'.", tree.loc)
      }
      // Decrease depth now that the call is returning.
      lctx.depth -= 1
      result
    }

    private def visitQnameExpr(tree: Tree)(implicit sctx: SharedContext): Expr.Ambiguous = {
      expect(tree, TreeKind.QName)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      assert(idents.nonEmpty) // Require at least one ident
      val first = idents.head
      val ident = idents.last
      val nnameIdents = idents.dropRight(1)
      val loc = SourceLocation(isReal = true, first.loc.sp1, ident.loc.sp2)
      val nname = Name.NName(nnameIdents, loc)
      val qname = Name.QName(nname, ident, loc)
      Expr.Ambiguous(qname, qname.loc)
    }

    private def visitParenExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Paren)
      mapN(pickExpr(tree)) {
        expr => Expr.Tuple(List(expr), tree.loc)
      }
    }

    private def visitBlockExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Block)
      pickExpr(tree)
    }

    private def visitStringInterpolationExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.StringInterpolation)
      val init = WeededAst.Expr.Cst(Constant.Str(""), tree.loc)
      var isDebug = false
      // Check for empty interpolation
      if (tryPick(TreeKind.Expr.Expr, tree).isEmpty) {
        val error = EmptyInterpolatedExpression(tree.loc)
        sctx.errors.add(error)
        return Validation.Success(Expr.Error(error))
      }

      Validation.fold(tree.children, init: WeededAst.Expr) {
        // A string part: Concat it onto the result
        case (acc, token@Token(_, _, _, _, _, _)) =>
          isDebug = token.kind == TokenKind.LiteralDebugStringL
          val loc = token.mkSourceLocation()
          val lit0 = token.text.stripPrefix("\"").stripSuffix("\"").stripPrefix("}")
          val lit = if (isDebug) lit0.stripSuffix("%{") else lit0.stripSuffix("${")
          if (lit == "") {
            Validation.Success(acc)
          } else {
            val (processed, errors) = Constants.visitChars(lit, loc)
            errors.foreach(sctx.errors.add)
            val cst = Validation.Success(Expr.Cst(Constant.Str(processed), loc))
            mapN(cst) {
              cst => Expr.Binary(SemanticOp.StringOp.Concat, acc, cst, tree.loc.asSynthetic)
            }
          }
        // An expression part: Apply 'toString' to it and concat the result
        case (acc, tree: Tree) if tree.kind == TreeKind.Expr.Expr =>
          mapN(visitExpr(tree))(expr => {
            val loc = tree.loc.asSynthetic
            val funcName = if (isDebug) {
              isDebug = false
              "Debug.stringify"
            } else "ToString.toString"
            val str = Expr.Apply(Expr.Ambiguous(Name.mkQName(funcName), loc), List(expr), loc)
            Expr.Binary(SemanticOp.StringOp.Concat, acc, str, loc)
          })
        // Skip anything else (Parser will have produced an error.)
        case (acc, _) => Validation.Success(acc)
      }
    }

    private def visitOpenVariantExpr(tree: Tree)(implicit sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.OpenVariant)
      mapN(pickQName(tree))(Expr.Open(_, tree.loc))
    }

    private def visitOpenVariantAsExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.OpenVariantAs)
      mapN(pickQName(tree), pickExpr(tree))((name, expr) => Expr.OpenAs(name, expr, tree.loc))
    }

    private def visitHoleExpr(tree: Tree)(implicit sctx: SharedContext): Expr = {
      expect(tree, TreeKind.Expr.Hole)
      val ident = tryPickNameIdent(tree)
      val strippedIdent = ident.map(id => Name.Ident(id.name.stripPrefix("?"), id.loc))
      Expr.Hole(strippedIdent, tree.loc)
    }

    private def visitHoleWithExpExpr(tree: Tree)(implicit sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.HoleVariable)
      mapN(pickNameIdent(tree)) {
        ident =>
          // Strip '?' suffix and update source location
          val sp1 = ident.loc.sp1
          val sp2 = ident.loc.sp2.copy(col = (ident.loc.sp2.col - 1).toShort)
          val id = Name.Ident(ident.name.stripSuffix("?"), SourceLocation(isReal = true, sp1, sp2))
          val expr = Expr.Ambiguous(Name.QName(Name.RootNS, id, id.loc), id.loc)
          Expr.HoleWithExp(expr, tree.loc)
      }
    }

    private def visitExprUseExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Use)
      mapN(flatMapN(pick(TreeKind.UsesOrImports.Use, tree))(visitUse), pickExpr(tree)) {
        (use, expr) => Expr.Use(use, expr, tree.loc)
      }
    }

    def visitLiteralExpr(tree: Tree)(implicit sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      // Note: This visitor is used by both expression literals and pattern literals.
      expectAny(tree, List(TreeKind.Expr.Literal, TreeKind.Pattern.Literal))
      tree.children(0) match {
        case token@Token(_, _, _, _, _, _) => token.kind match {
          case TokenKind.KeywordNull => Validation.Success(Expr.Cst(Constant.Null, token.mkSourceLocation()))
          case TokenKind.KeywordTrue => Validation.Success(Expr.Cst(Constant.Bool(true), token.mkSourceLocation()))
          case TokenKind.KeywordFalse => Validation.Success(Expr.Cst(Constant.Bool(false), token.mkSourceLocation()))
          case TokenKind.LiteralString => Validation.Success(Constants.toStringCst(token))
          case TokenKind.LiteralChar => Validation.Success(Constants.toChar(token))
          case TokenKind.LiteralInt8 => Validation.Success(Constants.toInt8(token))
          case TokenKind.LiteralInt16 => Validation.Success(Constants.toInt16(token))
          case TokenKind.LiteralInt32 => Validation.Success(Constants.toInt32(token))
          case TokenKind.LiteralInt64 => Validation.Success(Constants.toInt64(token))
          case TokenKind.LiteralBigInt => Validation.Success(Constants.toBigInt(token))
          case TokenKind.LiteralFloat32 => Validation.Success(Constants.toFloat32(token))
          case TokenKind.LiteralFloat64 => Validation.Success(Constants.toFloat64(token))
          case TokenKind.LiteralBigDecimal => Validation.Success(Constants.toBigDecimal(token))
          case TokenKind.LiteralRegex => Validation.Success(Constants.toRegex(token))
          case TokenKind.NameLowerCase
               | TokenKind.NameUpperCase
               | TokenKind.NameMath
               | TokenKind.NameGreek => mapN(pickNameIdent(tree))(ident => Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc))
          case _ =>
            val error = UnexpectedToken(expected = NamedTokenSet.Literal, actual = None, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
            sctx.errors.add(error)
            Validation.Success(Expr.Error(error))
        }
        case _ => throw InternalCompilerException(s"Literal had tree child", tree.loc)
      }
    }

    private def visitApplyExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Apply)
      flatMapN(pick(TreeKind.Expr.Expr, tree), pickArguments(tree, synctx = SyntacticContext.Expr.OtherExpr)) {
        case (expr, args) =>
          val maybeIntrinsic = tryPick(TreeKind.Expr.Intrinsic, expr)
          maybeIntrinsic match {
            case Some(intrinsic) => visitIntrinsic(intrinsic, args)
            case None => mapN(visitExpr(expr))(Expr.Apply(_, args, tree.loc))
          }
      }
    }

    private def pickArguments(tree: Tree, synctx: SyntacticContext)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[Expr], CompilationMessage] = {
      flatMapN(pick(TreeKind.ArgumentList, tree, synctx = synctx))(visitArguments)
    }

    /**
      * This method is the same as pickArguments but considers Unit as no-argument. It calls visitMethodArguments instead.
      */
    private def pickRawArguments(tree: Tree, synctx: SyntacticContext)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[Expr], CompilationMessage] = {
      flatMapN(pick(TreeKind.ArgumentList, tree, synctx = synctx))(visitMethodArguments)
    }

    private def visitArguments(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[Expr], CompilationMessage] = {
      mapN(
        traverse(pickAll(TreeKind.Argument, tree))(pickExpr),
        traverse(pickAll(TreeKind.ArgumentNamed, tree))(visitArgumentNamed)
      ) {
        (unnamed, named) =>
          unnamed ++ named match {
            // Add synthetic unit arguments if there are none
            case Nil => List(Expr.Cst(Constant.Unit, tree.loc))
            case args => args.sortBy(_.loc)
          }
      }
    }

    /**
      * This method is the same as visitArguments but for InvokeMethod. It does not consider named arguments
      * as they are not allowed and it doesn't add unit arguments for empty arguments.
      */
    private def visitMethodArguments(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[Expr], CompilationMessage] = {
      traverse(pickAll(TreeKind.Argument, tree))(pickExpr)
    }

    private def visitArgumentNamed(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.ArgumentNamed)
      val expsVal = traverse(pickAll(TreeKind.Expr.Expr, tree))(visitExpr)
      mapN(expsVal) {
        case e1 :: e2 :: Nil =>
          // First expression must be a name
          e1 match {
            case Expr.Ambiguous(qname, _) =>
              Expr.RecordExtend(Name.mkLabel(qname.ident), e2, Expr.RecordEmpty(tree.loc), tree.loc)
            case _ =>
              val error = Malformed(NamedTokenSet.Name, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
        case _ =>
          val error = Malformed(NamedTokenSet.Name, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
      }
    }

    private def visitIndexExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Index)
      pickAll(TreeKind.Expr.Expr, tree) match {
        case e1 :: e2 :: Nil => mapN(visitExpr(e1), visitExpr(e2)) {
          case (exp1, exp2) =>
            Expr.Apply(Expr.Ambiguous(Name.mkQName("Indexable.get", exp1.loc), exp1.loc), List(exp1, exp2), tree.loc)
        }
        case other => throw InternalCompilerException(s"Expr.Index tree with ${other.length} operands", tree.loc)
      }
    }

    private def visitIndexMutExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.IndexMut)
      pickAll(TreeKind.Expr.Expr, tree) match {
        case e1 :: e2 :: e3 :: Nil => mapN(visitExpr(e1), visitExpr(e2), visitExpr(e3)) {
          case (exp1, exp2, exp3) =>
            Expr.Apply(Expr.Ambiguous(Name.mkQName("IndexableMut.put", exp1.loc), exp1.loc), List(exp1, exp2, exp3), tree.loc)
        }
        case other => throw InternalCompilerException(s"Expr.IndexMut tree with ${other.length} operands", tree.loc)
      }
    }

    private def visitLambdaExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Lambda)
      mapN(pickExpr(tree), Decls.pickFormalParameters(tree, presence = Presence.Optional)) {
        (expr, fparams) => {
          val l = tree.loc.asSynthetic
          fparams.foldRight(expr) {
            case (fparam, acc) => WeededAst.Expr.Lambda(fparam, acc, l)
          }
        }
      }
    }

    private def visitLambdaMatchExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LambdaMatch)
      mapN(Patterns.pickPattern(tree), pickExpr(tree)) {
        (pat, expr) => Expr.LambdaMatch(pat, expr, tree.loc)
      }
    }

    private def visitUnaryExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Unary)
      flatMapN(pick(TreeKind.Operator, tree), pick(TreeKind.Expr.Expr, tree))(
        (opTree, exprTree) => {
          opTree.children.headOption match {
            case Some(opToken@Token(_, _, _, _, _, _)) =>
              val literalToken = tryPickNumberLiteralToken(exprTree)
              literalToken match {
                // fold unary minus into a constant
                case Some(lit) if opToken.text == "-" =>
                  // Construct a synthetic literal tree with the unary minus and visit that like any other literal expression
                  val syntheticToken = Token(lit.kind, lit.src, opToken.start, lit.end, lit.sp1, lit.sp2)
                  val syntheticLiteral = Tree(TreeKind.Expr.Literal, Array(syntheticToken), exprTree.loc.asSynthetic)
                  visitLiteralExpr(syntheticLiteral)
                case _ => mapN(visitExpr(exprTree))(expr => {
                  opToken.text match {
                    case "discard" => Expr.Discard(expr, tree.loc)
                    case "force" => Expr.Force(expr, tree.loc)
                    case "lazy" => Expr.Lazy(expr, tree.loc)
                    case "not" => Expr.Unary(SemanticOp.BoolOp.Not, expr, tree.loc)
                    case "-" => Expr.Apply(Expr.Ambiguous(Name.mkQName("Neg.neg", tree.loc), opTree.loc), List(expr), tree.loc)
                    case "+" => expr
                    case op => Expr.Apply(Expr.Ambiguous(Name.mkQName(op, tree.loc), opTree.loc), List(expr), tree.loc)
                  }
                })
              }
            case Some(_) => throw InternalCompilerException(s"Expected unary operator but found tree", tree.loc)
            case None => throw InternalCompilerException(s"Parser produced tree of kind 'Op' without child", tree.loc)
          }
        }
      )
    }

    private def tryPickNumberLiteralToken(tree: Tree): Option[Token] = {
      val NumberLiteralKinds = List(TokenKind.LiteralInt8, TokenKind.LiteralInt16, TokenKind.LiteralInt32, TokenKind.LiteralInt64, TokenKind.LiteralBigInt, TokenKind.LiteralFloat32, TokenKind.LiteralFloat64, TokenKind.LiteralBigDecimal)
      val maybeTree = tryPick(TreeKind.Expr.Literal, tree)
      maybeTree.flatMap(_.children(0) match {
        case t@Token(_, _, _, _, _, _) if NumberLiteralKinds.contains(t.kind) => Some(t)
        case _ => None
      })
    }

    private def visitBinaryExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Binary)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      val op = pick(TreeKind.Operator, tree)
      flatMapN(op, traverse(exprs)(visitExpr)) {
        case (op, e1 :: e2 :: Nil) =>
          val isInfix = op.children.head match {
            case Token(kind, _, _, _, _, _) => kind == TokenKind.InfixFunction
            case _ => false
          }

          if (isInfix) {
            val infixName = text(op).head.stripPrefix("`").stripSuffix("`")
            val infixNameParts = infixName.split('.').toList
            val lastName = infixNameParts.lastOption.getOrElse("")
            val qname = Name.mkQName(infixNameParts.init, lastName, op.loc)
            val opExpr = Expr.Ambiguous(qname, op.loc)
            return Validation.Success(Expr.Infix(e1, opExpr, e2, tree.loc))
          }

          def mkApply(name: String): Expr.Apply = Expr.Apply(
            Expr.Ambiguous(Name.mkQName(name, op.loc), op.loc), List(e1, e2),
            tree.loc
          )

          text(op).head match {
            // BUILTINS
            case "+" => Validation.Success(mkApply("Add.add"))
            case "-" => Validation.Success(mkApply("Sub.sub"))
            case "*" => Validation.Success(mkApply("Mul.mul"))
            case "/" => Validation.Success(mkApply("Div.div"))
            case "<" => Validation.Success(mkApply("Order.less"))
            case "<=" => Validation.Success(mkApply("Order.lessEqual"))
            case ">" => Validation.Success(mkApply("Order.greater"))
            case ">=" => Validation.Success(mkApply("Order.greaterEqual"))
            case "==" => Validation.Success(mkApply("Eq.eq"))
            case "!=" => Validation.Success(mkApply("Eq.neq"))
            case "<=>" => Validation.Success(mkApply("Order.compare"))
            // SEMANTIC OPS
            case "and" => Validation.Success(Expr.Binary(SemanticOp.BoolOp.And, e1, e2, tree.loc))
            case "or" => Validation.Success(Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, tree.loc))
            // SPECIAL
            case "::" => Validation.Success(Expr.FCons(e1, e2, tree.loc))
            case ":::" => Validation.Success(Expr.FAppend(e1, e2, tree.loc))
            case "<+>" => Validation.Success(Expr.FixpointMerge(e1, e2, tree.loc))
            case "instanceof" =>
              tryPickQName(exprs(1)) match {
                case Some(qname) =>
                  if (qname.isUnqualified) Validation.Success(Expr.InstanceOf(e1, qname.ident, tree.loc))
                  else {
                    val error = IllegalQualifiedName(exprs(1).loc)
                    sctx.errors.add(error)
                    Validation.Success(Expr.Error(error))
                  }
                case None =>
                  val error = UnexpectedToken(
                    NamedTokenSet.FromTreeKinds(Set(TreeKind.QName)),
                    None,
                    SyntacticContext.Expr.OtherExpr,
                    hint = Some("Use a single unqualified Java type like 'Object' instead of 'java.lang.object'."),
                    loc = exprs(1).loc
                  )
                  sctx.errors.add(error)
                  Validation.Success(Expr.Error(error))
              }
            // UNRECOGNIZED
            case id =>
              val ident = Name.Ident(id, op.loc)
              Validation.Success(Expr.Apply(Expr.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), op.loc), List(e1, e2), tree.loc))
          }
        case (_, operands) => throw InternalCompilerException(s"Expr.Binary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitIfThenElseExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.IfThenElse)
      pickAll(TreeKind.Expr.Expr, tree) match {
        case exprCondition :: exprThen :: exprElse :: Nil =>
          mapN(visitExpr(exprCondition), visitExpr(exprThen), visitExpr(exprElse)) {
            (condition, tthen, eelse) => Expr.IfThenElse(condition, tthen, eelse, tree.loc)
          }
        case _ =>
          val error = UnexpectedToken(NamedTokenSet.FromKinds(Set(TokenKind.KeywordElse)), actual = None, SyntacticContext.Expr.OtherExpr, hint = Some("the else-branch is required in Flix."), tree.loc)
          sctx.errors.add(error)
          Validation.Success(Expr.Error(error))
      }
    }

    private def visitStatementExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Statement)
      mapN(traverse(pickAll(TreeKind.Expr.Expr, tree))(visitExpr)) {
        case ex1 :: ex2 :: Nil => Expr.Stm(ex1, ex2, tree.loc)
        case exprs => throw InternalCompilerException(s"Parser error. Expected 2 expressions in statement but found '${exprs.length}'.", tree.loc)
      }
    }

    private def visitLocalDefExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LocalDef)
      Decls.pickAnnotations(tree) match {
        case Annotations(as) =>
          // Check for annotations
          for (a <- as) {
            sctx.errors.add(IllegalAnnotation(a.loc))
          }
      }

      val exprs = mapN(pickExpr(tree)) {
        case Expr.Stm(exp1, exp2, _) => (exp1, exp2)
        case e =>
          // Fall back on Expr.Error. Parser has reported an error here.
          val error = Malformed(NamedTokenSet.FromKinds(Set(TokenKind.KeywordDef)), SyntacticContext.Expr.OtherExpr, hint = Some("Internal definitions must be followed by an expression"), loc = e.loc)
          (e, Expr.Error(error))
      }

      mapN(
        exprs,
        Decls.pickFormalParameters(tree, Presence.Optional),
        pickNameIdent(tree),
        Types.tryPickType(tree),
        Types.tryPickEffect(tree),
      ) {
        case ((exp1, exp2), fparams, ident, declaredTpe, declaredEff) =>
          Expr.LocalDef(ident, fparams, declaredTpe, declaredEff, exp1, exp2, tree.loc)
      }
    }

    private def visitScopeExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Scope)
      val block = flatMapN(pick(TreeKind.Expr.Block, tree))(visitBlockExpr)
      mapN(pickNameIdent(tree), block) {
        (ident, block) => Expr.Scope(ident, block, tree.loc)
      }
    }

    private def visitMatchExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Match)
      val rules = pickAll(TreeKind.Expr.MatchRuleFragment, tree)
      flatMapN(pickExpr(tree), traverse(rules)(visitMatchRule)) {
        // Case: no valid match rule found in match expr
        case (expr, Nil) =>
          val error = NeedAtleastOne(NamedTokenSet.MatchRule, SyntacticContext.Expr.OtherExpr, loc = expr.loc)
          // Fall back on Expr.Error. Parser has reported an error here.
          Validation.Success(Expr.Error(error))
        case (expr, rules) => Validation.Success(Expr.Match(expr, rules, tree.loc))
      }
    }

    private def visitMatchRule(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[MatchRule, CompilationMessage] = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      flatMapN(Patterns.pickPattern(tree), traverse(exprs)(visitExpr)) {
        // case pattern => expr
        case (pat, expr :: Nil) => Validation.Success(MatchRule(pat, None, expr))
        // case pattern if expr => expr
        case (pat, expr1 :: expr2 :: Nil) => Validation.Success(MatchRule(pat, Some(expr1), expr2))
        // Fall back on Expr.Error. Parser has reported an error here.
        case (_, _) =>
          val error = Malformed(NamedTokenSet.MatchRule, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          Validation.Success(MatchRule(Pattern.Error(tree.loc), None, Expr.Error(error)))
      }
    }

    private def visitTypeMatchExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.TypeMatch)
      val rules = pickAll(TreeKind.Expr.TypeMatchRuleFragment, tree)
      mapN(pickExpr(tree), traverse(rules)(visitTypeMatchRule)) {
        (expr, rules) => Expr.TypeMatch(expr, rules, tree.loc)
      }
    }

    private def visitTypeMatchRule(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[TypeMatchRule, CompilationMessage] = {
      expect(tree, TreeKind.Expr.TypeMatchRuleFragment)
      mapN(pickNameIdent(tree), pickExpr(tree), Types.pickType(tree)) {
        (ident, expr, ttype) => TypeMatchRule(ident, ttype, expr)
      }
    }

    private def visitRestrictableChooseExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expectAny(tree, List(TreeKind.Expr.RestrictableChoose, TreeKind.Expr.RestrictableChooseStar))
      val isStar = tree.kind == TreeKind.Expr.RestrictableChooseStar
      val rules = pickAll(TreeKind.Expr.MatchRuleFragment, tree)
      mapN(pickExpr(tree), traverse(rules)(t => visitRestrictableChooseRule(isStar, t))) {
        (expr, rules) => Expr.RestrictableChoose(isStar, expr, rules, tree.loc)
      }
    }

    private def visitRestrictableChooseRule(isStar: Boolean, tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[RestrictableChooseRule, CompilationMessage] = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      flatMapN(pickRestrictableChoosePattern(isStar, tree), pickExpr(tree)) {
        (pat, expr) => Validation.Success(RestrictableChooseRule(pat, expr))
      }
    }

    private def pickRestrictableChoosePattern(isStar: Boolean, tree: Tree)(implicit sctx: SharedContext): Validation[RestrictableChoosePattern, CompilationMessage] = {
      expect(tree, TreeKind.Expr.MatchRuleFragment)
      flatMapN(Patterns.pickPattern(tree)) {
        case Pattern.Tag(qname, pat, loc) =>
          val inner = pat match {
            case Pattern.Wild(loc) => Validation.Success(List(WeededAst.RestrictableChoosePattern.Wild(loc)))
            case Pattern.Var(ident, loc) => Validation.Success(List(WeededAst.RestrictableChoosePattern.Var(ident, loc)))
            case Pattern.Cst(Constant.Unit, loc) => Validation.Success(List(WeededAst.RestrictableChoosePattern.Wild(loc)))
            case Pattern.Tuple(elms, _) =>
              traverse(elms) {
                case Pattern.Wild(loc) => Validation.Success(WeededAst.RestrictableChoosePattern.Wild(loc))
                case Pattern.Var(ident, loc) => Validation.Success(WeededAst.RestrictableChoosePattern.Var(ident, loc))
                case Pattern.Cst(Constant.Unit, loc) => Validation.Success(WeededAst.RestrictableChoosePattern.Wild(loc))
                case other =>
                  val error = UnsupportedRestrictedChoicePattern(isStar, other.loc)
                  sctx.errors.add(error)
                  Validation.Success(WeededAst.RestrictableChoosePattern.Error(other.loc.asSynthetic))
              }
            case other =>
              val error = UnsupportedRestrictedChoicePattern(isStar, other.loc)
              sctx.errors.add(error)
              Validation.Success(List(WeededAst.RestrictableChoosePattern.Error(other.loc.asSynthetic)))
          }
          mapN(inner)(RestrictableChoosePattern.Tag(qname, _, loc))
        case other =>
          val error = UnsupportedRestrictedChoicePattern(isStar, other.loc)
          sctx.errors.add(error)
          Validation.Success(RestrictableChoosePattern.Error(other.loc))
      }
    }

    private def visitForApplicativeExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForApplicative)
      mapN(pickForFragments(tree), pickExpr(tree)) {
        case (Nil, _) =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case (fragments, expr) =>
          // Check that there are only generators
          val (generators, nonGeneratorFragments) = fragments.partition {
            case _: ForFragment.Generator => true
            case _ => false
          }
          if (nonGeneratorFragments.nonEmpty) {
            val errors = nonGeneratorFragments.map(f => IllegalForAFragment(f.loc))
            errors.foreach(sctx.errors.add)
            val error = IllegalForAFragment(nonGeneratorFragments.head.loc)
            Expr.Error(error)
          } else {
            Expr.ApplicativeFor(generators.asInstanceOf[List[ForFragment.Generator]], expr, tree.loc)
          }
      }
    }

    private def visitForeachExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Foreach)
      mapN(pickForFragments(tree), pickExpr(tree)) {
        case (Nil, _) =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case (fragments, expr) =>
          // It's okay to do head rather than headOption here because we check for Nil above.
          fragments.head match {
            // Check that fragments start with a generator.
            case _: ForFragment.Generator => Expr.ForEach(fragments, expr, tree.loc)
            case f =>
              val error = IllegalForFragment(f.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }

    private def visitForMonadicExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForMonadic)
      mapN(pickForFragments(tree), pickExpr(tree)) {
        case (Nil, _) =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case (fragments, expr) =>
          // It's okay to do head rather than headOption here because we check for Nil above.
          fragments.head match {
            // Check that fragments start with a generator.
            case _: ForFragment.Generator => Expr.MonadicFor(fragments, expr, tree.loc)
            case f =>
              val error = IllegalForFragment(f.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }

    private def visitForeachYieldExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForeachYield)
      mapN(pickForFragments(tree), pickExpr(tree)) {
        case (Nil, _) =>
          val error = EmptyForFragment(tree.loc)
          sctx.errors.add(error)
          Expr.Error(error)
        case (fragments, expr) =>
          // It's okay to do head rather than headOption here because we check for Nil above.
          fragments.head match {
            // Check that fragments start with a generator.
            case _: ForFragment.Generator => Expr.ForEachYield(fragments, expr, tree.loc)
            case f =>
              val error = IllegalForFragment(f.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }

    private def pickForFragments(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[ForFragment], CompilationMessage] = {
      val guards = pickAll(TreeKind.Expr.ForFragmentGuard, tree)
      val generators = pickAll(TreeKind.Expr.ForFragmentGenerator, tree)
      val lets = pickAll(TreeKind.Expr.ForFragmentLet, tree)
      mapN(
        traverse(guards)(visitForFragmentGuard),
        traverse(generators)(visitForFragmentGenerator),
        traverse(lets)(visitForFragmentLet),
      ) {
        (guards, generators, lets) => (generators ++ guards ++ lets).sortBy(_.loc)
      }
    }

    private def visitForFragmentGuard(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[ForFragment.Guard, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForFragmentGuard)
      mapN(pickExpr(tree))(ForFragment.Guard(_, tree.loc))
    }

    private def visitForFragmentGenerator(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[ForFragment.Generator, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForFragmentGenerator)
      mapN(Patterns.pickPattern(tree), pickExpr(tree)) {
        (pat, expr) => ForFragment.Generator(pat, expr, tree.loc)
      }
    }

    private def visitForFragmentLet(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[ForFragment.Let, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ForFragmentLet)
      mapN(Patterns.pickPattern(tree), pickExpr(tree)) {
        (pat, expr) => ForFragment.Let(pat, expr, tree.loc)
      }
    }

    private def visitLetMatchExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LetMatch)
      flatMapN(Patterns.pickPattern(tree), Types.tryPickType(tree), pickExpr(tree)) {
        (pattern, tpe, expr) =>
          // get expr1 and expr2 from the nested statement within expr.
          val exprs = expr match {
            case Expr.Stm(exp1, exp2, _) => Validation.Success((exp1, exp2))
            // Fall back on Expr.Error. Parser has reported an error here.
            case e =>
              val error = Malformed(NamedTokenSet.FromKinds(Set(TokenKind.KeywordLet)), SyntacticContext.Expr.OtherExpr, hint = Some("let-bindings must be followed by an expression"), e.loc)
              Validation.Success((e, Expr.Error(error)))
          }
          mapN(exprs)(exprs => Expr.LetMatch(pattern, tpe, exprs._1, exprs._2, tree.loc))
      }
    }

    private def visitTupleExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Tuple)
      mapN(traverse(pickAll(TreeKind.Argument, tree))(pickExpr), traverse(pickAll(TreeKind.ArgumentNamed, tree))(visitArgumentNamed)) {
        (unnamed, named) => Expr.Tuple((unnamed ++ named).sortBy(_.loc), tree.loc)
      }
    }

    private def visitLiteralRecordExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralRecord)
      val fields = pickAll(TreeKind.Expr.LiteralRecordFieldFragment, tree)
      mapN(traverse(fields)(visitLiteralRecordField)) {
        fields =>
          fields.foldRight(Expr.RecordEmpty(tree.loc.asSynthetic): Expr) {
            case ((label, expr, loc), acc) =>
              val SourceLocation(isReal, sp1, _) = loc
              val extendLoc = SourceLocation(isReal, sp1, tree.loc.sp2)
              Expr.RecordExtend(label, expr, acc, extendLoc)
          }
      }
    }

    private def visitLiteralRecordField(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[(Name.Label, Expr, SourceLocation), CompilationMessage] = {
      mapN(pickNameIdent(tree), pickExpr(tree)) {
        (ident, expr) => (Name.mkLabel(ident), expr, tree.loc)
      }
    }

    private def visitRecordSelectExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.RecordSelect)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      mapN(pickExpr(tree)) {
        expr =>
          idents.foldLeft(expr) {
            case (acc, ident) =>
              val loc = SourceLocation(ident.loc.isReal, tree.loc.sp1, ident.loc.sp2)
              Expr.RecordSelect(acc, Name.mkLabel(ident), loc)
          }
      }
    }

    private def visitRecordOperationExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.RecordOperation)
      val updates = pickAll(TreeKind.Expr.RecordOpUpdate, tree)
      val eextends = pickAll(TreeKind.Expr.RecordOpExtend, tree)
      val restricts = pickAll(TreeKind.Expr.RecordOpRestrict, tree)
      val ops = (updates ++ eextends ++ restricts).sortBy(_.loc)
      Validation.foldRight(ops)(pickExpr(tree))((op, acc) =>
        op.kind match {
          case TreeKind.Expr.RecordOpExtend => mapN(pickExpr(op), pickNameIdent(op))(
            (expr, id) => Expr.RecordExtend(Name.mkLabel(id), expr, acc, op.loc)
          )
          case TreeKind.Expr.RecordOpRestrict => mapN(pickNameIdent(op))(
            id => Expr.RecordRestrict(Name.mkLabel(id), acc, op.loc)
          )
          case TreeKind.Expr.RecordOpUpdate => mapN(pickExpr(op), pickNameIdent(op))(
            (expr, id) => {
              // An update is a restrict followed by an extension
              val restricted = Expr.RecordRestrict(Name.mkLabel(id), acc, op.loc)
              Expr.RecordExtend(Name.mkLabel(id), expr, restricted, op.loc)
            })
          case k => throw InternalCompilerException(s"'$k' is not a record operation", op.loc)
        }
      )
    }

    private def visitLiteralArrayExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralArray)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      val scopeName = tryPick(TreeKind.Expr.ScopeName, tree)
      mapN(traverse(exprs)(visitExpr), traverseOpt(scopeName)(visitScopeName)) {
        case (exprs, Some(scope)) => Expr.ArrayLit(exprs, scope, tree.loc)
        case (exprs, None) =>
          val error = MissingScope(TokenKind.ArrayHash, SyntacticContext.Expr.OtherExpr, tree.loc)
          sctx.errors.add(error)
          Expr.ArrayLit(exprs, Expr.Error(error), tree.loc)
      }
    }

    private def visitScopeName(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ScopeName)
      pickExpr(tree)
    }

    private def visitLiteralVectorExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralVector)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      mapN(traverse(exprs)(visitExpr))(Expr.VectorLit(_, tree.loc))
    }

    private def visitLiteralListExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralList)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      mapN(traverse(exprs)(visitExpr))(Expr.ListLit(_, tree.loc))
    }

    private def visitLiteralMapExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralMap)
      val pairs = pickAll(TreeKind.Expr.LiteralMapKeyValueFragment, tree)
      mapN(traverse(pairs)(visitKeyValuePair))(Expr.MapLit(_, tree.loc))
    }

    private def visitKeyValuePair(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[(Expr, Expr), CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralMapKeyValueFragment)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      mapN(traverse(exprs)(visitExpr)) {
        // case: k => v
        case k :: v :: Nil => (k, v)
        // case: k =>
        case k :: Nil =>
          val error = Malformed(NamedTokenSet.KeyValuePair, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          (k, Expr.Error(error))
        case xs => throw InternalCompilerException(s"Malformed KeyValue pair, found ${xs.length} expressions", tree.loc)
      }
    }

    private def visitLiteralSetExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralSet)
      val exprs = pickAll(TreeKind.Expr.Expr, tree)
      mapN(traverse(exprs)(visitExpr))(Expr.SetLit(_, tree.loc))
    }

    private def visitAscribeExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Ascribe)
      mapN(pickExpr(tree), Types.tryPickTypeNoWild(tree), Types.tryPickEffect(tree)) {
        (expr, tpe, eff) => Expr.Ascribe(expr, tpe, eff, tree.loc)
      }
    }

    private def visitCheckedTypeCastExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.CheckedTypeCast)
      mapN(pickExpr(tree)) {
        expr => Expr.CheckedCast(CheckedCastType.TypeCast, expr, tree.loc)
      }
    }

    private def visitCheckedEffectCastExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.CheckedEffectCast)
      mapN(pickExpr(tree)) {
        expr => Expr.CheckedCast(CheckedCastType.EffectCast, expr, tree.loc)
      }
    }

    private def visitUncheckedCastExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.UncheckedCast)
      mapN(pickExpr(tree), Types.tryPickTypeNoWild(tree), Types.tryPickEffect(tree)) {
        (expr, tpe, eff) => Expr.UncheckedCast(expr, tpe, eff, tree.loc)
      }
    }

    private def visitUncheckedMaskingCastExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.UncheckedMaskingCast)
      mapN(pickExpr(tree)) {
        expr => Expr.UncheckedMaskingCast(expr, tree.loc)
      }
    }

    private def visitUnsafeExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Unsafe)
      mapN(pickExpr(tree)) {
        expr => Expr.Unsafe(expr, tree.loc)
      }
    }

    private def visitWithoutExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Without)
      val effectSet = pick(TreeKind.Type.EffectSet, tree)
      val effects = mapN(effectSet)(effectSetTree => pickAll(TreeKind.QName, effectSetTree).map(visitQName))
      mapN(pickExpr(tree), effects) {
        case (expr, effect :: effects) =>
          val base = Expr.Without(expr, effect, tree.loc)
          effects.foldLeft(base) {
            case (acc, eff) => Expr.Without(acc, eff, tree.loc.asSynthetic)
          }
        case (_, Nil) =>
          // Fall back on Expr.Error, Parser has already reported this
          Expr.Error(Malformed(NamedTokenSet.Effect, SyntacticContext.Expr.OtherExpr, None, tree.loc))
      }
    }

    private def visitTryExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Try)
      val maybeCatch = pickAll(TreeKind.Expr.TryCatchBodyFragment, tree)
      val maybeWith = pickAll(TreeKind.Expr.TryWithBodyFragment, tree)
      flatMapN(
        pickExpr(tree),
        traverse(maybeCatch)(visitTryCatchBody),
        traverse(maybeWith)(visitTryWithBody),
      ) {
        // Bad case: try expr
        case (_, Nil, Nil) =>
          // Fall back on Expr.Error, Parser has already reported an error.
          val error = UnexpectedToken(
            expected = NamedTokenSet.FromKinds(Set(TokenKind.KeywordCatch, TokenKind.KeywordWith)),
            actual = None,
            SyntacticContext.Expr.OtherExpr,
            loc = tree.loc)
          Validation.Success(Expr.Error(error))
        // Bad case: try expr catch { rules... } with eff { handlers... }
        case (_, _ :: _, _ :: _) =>
          val error = Malformed(NamedTokenSet.FromKinds(Set(TokenKind.KeywordTry)), SyntacticContext.Expr.OtherExpr, hint = Some(s"Use either ${TokenKind.KeywordWith.display} or ${TokenKind.KeywordCatch.display} on ${TokenKind.KeywordTry.display}."), tree.loc)
          // Fall back on Expr.Error, Parser has already reported an error.
          Validation.Success(Expr.Error(error))
        // Case: try expr catch { rules... }
        case (expr, catches, Nil) => Validation.Success(Expr.TryCatch(expr, catches.flatten, tree.loc))
        // Case: try expr with eff { handlers... }
        case (expr, Nil, withs) => Validation.Success(Expr.TryWith(expr, withs, tree.loc))
      }
    }

    private def visitTryCatchBody(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[List[CatchRule], CompilationMessage] = {
      expect(tree, TreeKind.Expr.TryCatchBodyFragment)
      val rules = pickAll(TreeKind.Expr.TryCatchRuleFragment, tree)
      traverse(rules)(visitTryCatchRule)
    }

    private def visitTryCatchRule(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[CatchRule, CompilationMessage] = {
      expect(tree, TreeKind.Expr.TryCatchRuleFragment)
      mapN(pickNameIdent(tree), pickQName(tree), pickExpr(tree)) {
        (ident, qname, expr) => CatchRule(ident, javaQnameToFqn(qname), expr)
      }
    }

    private def visitTryWithBody(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[WithHandler, CompilationMessage] = {
      expect(tree, TreeKind.Expr.TryWithBodyFragment)
      val rules = pickAll(TreeKind.Expr.TryWithRuleFragment, tree)
      mapN(pickQName(tree), /* This qname is an effect */ traverse(rules)(visitTryWithRule)) {
        (eff, handlers) => WithHandler(eff, handlers)
      }
    }

    private def visitTryWithRule(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[HandlerRule, CompilationMessage] = {
      expect(tree, TreeKind.Expr.TryWithRuleFragment)
      mapN(
        pickNameIdent(tree),
        Decls.pickFormalParameters(tree, Presence.Forbidden),
        pickExpr(tree)
      )((ident, fparams, expr) => {
        // Add extra resumption argument as a synthetic unit parameter when there is exactly one parameter.
        val hasSingleNonUnitParam = fparams.sizeIs == 1 && fparams.exists(_.ident.name != "_unit")
        val syntheticUnitParam = if (hasSingleNonUnitParam) List(Decls.unitFormalParameter(tree.loc.asSynthetic)) else List.empty
        HandlerRule(ident, (syntheticUnitParam ++ fparams).sortBy(_.loc), expr)
      })
    }

    private def visitThrow(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Throw)
      mapN(pickExpr(tree))(e => Expr.Throw(e, tree.loc))
    }

    private def visitInvokeConstructorExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.InvokeConstructor)
      mapN(Types.pickType(tree), pickRawArguments(tree, synctx = SyntacticContext.Expr.New)) {
        (tpe, exps) =>
          tpe match {
            case WeededAst.Type.Ambiguous(qname, _) if qname.isUnqualified =>
              Expr.InvokeConstructor(qname.ident, exps, tree.loc)
            case _ =>
              val error = IllegalQualifiedName(tree.loc)
              sctx.errors.add(error)
              Expr.Error(error)
          }
      }
    }

    private def visitInvokeMethodExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.InvokeMethod)
      val baseExp = pickExpr(tree)
      val method = pickNameIdent(tree)
      val argsExps = pickRawArguments(tree, synctx = SyntacticContext.Expr.OtherExpr)
      mapN(baseExp, method, argsExps) {
        case (b, m, as) =>
          val result = Expr.InvokeMethod(b, m, as, tree.loc)
          result
      }
    }

    private def visitGetFieldExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.GetField)
      val baseExp = pickExpr(tree)
      val method = pickNameIdent(tree)
      mapN(baseExp, method) {
        case (b, m) => Expr.GetField(b, m, tree.loc)
      }
    }

    private def visitNewObjectExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.NewObject)
      val methods = pickAll(TreeKind.Expr.JvmMethod, tree)
      mapN(Types.pickType(tree), traverse(methods)(visitJvmMethod)) {
        (tpe, methods) => Expr.NewObject(tpe, methods, tree.loc)
      }
    }

    private def visitStructGetExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.StructGet)
      mapN(pickExpr(tree), pickNameIdent(tree)) {
        (expr, ident) => Expr.StructGet(expr, Name.mkLabel(ident), tree.loc)
      }
    }

    private def visitStructPutExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.StructPut)
      val struct = pickExpr(tree)
      val ident = pickNameIdent(tree)
      val rhs = flatMapN(pick(TreeKind.Expr.StructPutRHS, tree)) {
        tree => pickExpr(tree)
      }

      mapN(struct, ident, rhs) {
        (struct, ident, rhs) => Expr.StructPut(struct, Name.mkLabel(ident), rhs, tree.loc)
      }
    }

    private def visitJvmMethod(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[JvmMethod, CompilationMessage] = {
      expect(tree, TreeKind.Expr.JvmMethod)
      mapN(
        pickNameIdent(tree),
        pickExpr(tree),
        Decls.pickFormalParameters(tree),
        Types.pickType(tree),
        Types.tryPickEffect(tree),
      ) {
        (ident, expr, fparams, tpe, eff) => JvmMethod(ident, fparams, expr, tpe, eff, tree.loc)
      }
    }

    private def visitNewStructExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.NewStruct)
      val fields = pickAll(TreeKind.Expr.LiteralStructFieldFragment, tree)
      flatMapN(Types.pickType(tree), traverse(fields)(visitNewStructField), pickExpr(tree)) {
        (tpe, fields, region) =>
          tpe match {
            case WeededAst.Type.Ambiguous(qname, _) =>
              Validation.Success(Expr.StructNew(qname, fields, region, tree.loc))
            case _ =>
              val error = IllegalQualifiedName(tree.loc)
              sctx.errors.add(error)
              Validation.Success(Expr.Error(error))
          }
      }
    }

    private def visitNewStructField(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[(Name.Label, Expr), CompilationMessage] = {
      expect(tree, TreeKind.Expr.LiteralStructFieldFragment)
      mapN(pickNameIdent(tree), pickExpr(tree)) {
        (ident, expr) => (Name.mkLabel(ident), expr)
      }
    }

    private def visitStaticExpr(tree: Tree): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Static)
      Validation.Success(Expr.Static(tree.loc))
    }

    private def visitSelectExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Select)
      val rules0 = traverse(pickAll(TreeKind.Expr.SelectRuleFragment, tree))(visitSelectRule)
      val maybeDefault0 = traverseOpt(tryPick(TreeKind.Expr.SelectRuleDefaultFragment, tree))(pickExpr)
      mapN(rules0, maybeDefault0) {
        (rules, maybeDefault) =>
          Result.sequence(rules) match {
            case Result.Ok(rs) => Expr.SelectChannel(rs, maybeDefault, tree.loc)
            case Result.Err(error) => Expr.Error(error)
          }
      }
    }

    private def visitSelectRule(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Result[SelectChannelRule, UnexpectedSelectChannelRuleFunction], CompilationMessage] = {
      expect(tree, TreeKind.Expr.SelectRuleFragment)
      val exprs = traverse(pickAll(TreeKind.Expr.Expr, tree))(visitExpr)
      mapN(pickNameIdent(tree), pickQName(tree), exprs) {
        case (ident, qname, channel :: body :: Nil) => // Shape is correct
          val isRecvFunction = qname.toString == "Channel.recv" || qname.toString == "recv"
          if (isRecvFunction) {
            Result.Ok(SelectChannelRule(ident, channel, body))
          } else {
            val error = UnexpectedSelectChannelRuleFunction(qname)
            sctx.errors.add(error)
            Result.Err(error)
          }
        case _ => // Unreachable
          throw InternalCompilerException("unexpected invalid select channel rule", tree.loc)
      }
    }

    private def visitSpawnExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Spawn)
      val scopeName = tryPick(TreeKind.Expr.ScopeName, tree)
      mapN(pickExpr(tree), traverseOpt(scopeName)(visitScopeName)) {
        case (expr1, Some(expr2)) =>
          Expr.Spawn(expr1, expr2, tree.loc)
        case (expr1, None) =>
          val error = MissingScope(TokenKind.KeywordSpawn, SyntacticContext.Expr.OtherExpr, loc = tree.loc)
          sctx.errors.add(error)
          Expr.Spawn(expr1, Expr.Error(error), tree.loc)
      }
    }

    private def visitParYieldExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ParYield)
      val fragments = pickAll(TreeKind.Expr.ParYieldFragment, tree)
      mapN(traverse(fragments)(visitParYieldFragment), pickExpr(tree)) {
        (fragments, expr) => Expr.ParYield(fragments, expr, tree.loc)
      }
    }

    private def visitParYieldFragment(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[ParYieldFragment, CompilationMessage] = {
      expect(tree, TreeKind.Expr.ParYieldFragment)
      mapN(Patterns.pickPattern(tree), pickExpr(tree)) {
        (pat, expr) => ParYieldFragment(pat, expr, tree.loc)
      }
    }

    private def visitFixpointConstraintSetExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointConstraintSet)
      val constraints = pickAll(TreeKind.Expr.FixpointConstraint, tree)
      mapN(traverse(constraints)(visitFixpointConstraint)) {
        constraints => Expr.FixpointConstraintSet(constraints, tree.loc)
      }
    }

    private def visitFixpointConstraint(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Constraint, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointConstraint)
      val bodyItems = pickAll(TreeKind.Predicate.Body, tree)
      mapN(Predicates.pickHead(tree), traverse(bodyItems)(Predicates.visitBody)) {
        (head, body) => Constraint(head, body, tree.loc)
      }
    }

    private def visitFixpointLambdaExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointLambda)
      val params = mapN(pick(TreeKind.Predicate.ParamList, tree))(t =>
        (pickAll(TreeKind.Predicate.ParamUntyped, t) ++ pickAll(TreeKind.Predicate.Param, t)).sortBy(_.loc)
      )
      mapN(flatMapN(params)(ps => traverse(ps)(Predicates.visitParam)), pickExpr(tree)) {
        (params, expr) => Expr.FixpointLambda(params, expr, tree.loc)
      }
    }

    private def visitFixpointInjectExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointInject)
      val expressions = pickAll(TreeKind.Expr.Expr, tree)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      mapN(traverse(expressions)(visitExpr)) {
        case exprs if exprs.length != idents.length =>
          // Check for mismatched arity
          val error = MismatchedArity(exprs.length, idents.length, tree.loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)

        case exprs =>
          Expr.FixpointInjectInto(exprs, idents, tree.loc)
      }
    }

    private def visitFixpointSolveExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointSolveWithProject)
      val expressions = pickAll(TreeKind.Expr.Expr, tree)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      mapN(traverse(expressions)(visitExpr)) {
        exprs =>
          val optIdents = if (idents.isEmpty) None else Some(idents)
          Expr.FixpointSolveWithProject(exprs, optIdents, tree.loc)
      }
    }

    private def visitFixpointQueryExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.FixpointQuery)
      val expressions = traverse(pickAll(TreeKind.Expr.Expr, tree))(visitExpr)
      val selects = flatMapN(pick(TreeKind.Expr.FixpointSelect, tree))(
        selectTree => traverse(pickAll(TreeKind.Expr.Expr, selectTree))(visitExpr)
      )
      val froms = flatMapN(pick(TreeKind.Expr.FixpointFromFragment, tree))(
        fromTree => traverse(pickAll(TreeKind.Predicate.Atom, fromTree))(Predicates.visitAtom)
      )
      val where = traverseOpt(tryPick(TreeKind.Expr.FixpointWhere, tree))(pickExpr)
      mapN(expressions, selects, froms, where) {
        (expressions, selects, froms, where) =>
          val whereList = where.map(w => List(w)).getOrElse(List.empty)
          Expr.FixpointQueryWithSelect(expressions, selects, froms, whereList, tree.loc)
      }
    }

    private def visitDebugExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Debug)
      mapN(pickDebugKind(tree), pickExpr(tree)) {
        (kind, expr) => Expr.Debug(expr, kind, tree.loc)
      }
    }

    private def pickDebugKind(tree: Tree): Validation[DebugKind, CompilationMessage] = {
      tree.children.headOption match {
        case Some(Token(kind, _, _, _, _, _)) if kind == TokenKind.KeywordDebug => Validation.Success(DebugKind.Debug)
        case Some(Token(kind, _, _, _, _, _)) if kind == TokenKind.KeywordDebugBang => Validation.Success(DebugKind.DebugWithLoc)
        case Some(Token(kind, _, _, _, _, _)) if kind == TokenKind.KeywordDebugBangBang => Validation.Success(DebugKind.DebugWithLocAndSrc)
        case _ => throw InternalCompilerException(s"Malformed debug expression, could not find debug kind", tree.loc)
      }
    }

    private def visitIntrinsic(tree: Tree, args: List[Expr])(implicit sctx: SharedContext): Validation[Expr, CompilationMessage] = {
      expect(tree, TreeKind.Expr.Intrinsic)
      val intrinsic = text(tree).head.stripPrefix("$").stripSuffix("$")
      val loc = tree.loc
      (intrinsic, args) match {
        case ("BOOL_NOT", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.BoolOp.Not, e1, loc))
        case ("BOOL_AND", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.BoolOp.And, e1, e2, loc))
        case ("BOOL_OR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.BoolOp.Or, e1, e2, loc))
        case ("BOOL_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.BoolOp.Eq, e1, e2, loc))
        case ("BOOL_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.BoolOp.Neq, e1, e2, loc))
        case ("CHAR_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Eq, e1, e2, loc))
        case ("CHAR_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Neq, e1, e2, loc))
        case ("CHAR_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Lt, e1, e2, loc))
        case ("CHAR_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Le, e1, e2, loc))
        case ("CHAR_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Gt, e1, e2, loc))
        case ("CHAR_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.CharOp.Ge, e1, e2, loc))
        case ("FLOAT32_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Float32Op.Neg, e1, loc))
        case ("FLOAT32_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Add, e1, e2, loc))
        case ("FLOAT32_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Sub, e1, e2, loc))
        case ("FLOAT32_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Mul, e1, e2, loc))
        case ("FLOAT32_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Div, e1, e2, loc))
        case ("FLOAT32_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Exp, e1, e2, loc))
        case ("FLOAT32_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Eq, e1, e2, loc))
        case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Neq, e1, e2, loc))
        case ("FLOAT32_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Lt, e1, e2, loc))
        case ("FLOAT32_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Le, e1, e2, loc))
        case ("FLOAT32_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Gt, e1, e2, loc))
        case ("FLOAT32_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float32Op.Ge, e1, e2, loc))
        case ("FLOAT64_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Float64Op.Neg, e1, loc))
        case ("FLOAT64_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Add, e1, e2, loc))
        case ("FLOAT64_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Sub, e1, e2, loc))
        case ("FLOAT64_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Mul, e1, e2, loc))
        case ("FLOAT64_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Div, e1, e2, loc))
        case ("FLOAT64_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Exp, e1, e2, loc))
        case ("FLOAT64_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Eq, e1, e2, loc))
        case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Neq, e1, e2, loc))
        case ("FLOAT64_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Lt, e1, e2, loc))
        case ("FLOAT64_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Le, e1, e2, loc))
        case ("FLOAT64_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Gt, e1, e2, loc))
        case ("FLOAT64_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Float64Op.Ge, e1, e2, loc))
        case ("INT8_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int8Op.Neg, e1, loc))
        case ("INT8_NOT", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int8Op.Not, e1, loc))
        case ("INT8_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Add, e1, e2, loc))
        case ("INT8_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Sub, e1, e2, loc))
        case ("INT8_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Mul, e1, e2, loc))
        case ("INT8_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Div, e1, e2, loc))
        case ("INT8_REM", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Rem, e1, e2, loc))
        case ("INT8_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Exp, e1, e2, loc))
        case ("INT8_AND", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.And, e1, e2, loc))
        case ("INT8_OR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Or, e1, e2, loc))
        case ("INT8_XOR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Xor, e1, e2, loc))
        case ("INT8_SHL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Shl, e1, e2, loc))
        case ("INT8_SHR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Shr, e1, e2, loc))
        case ("INT8_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Eq, e1, e2, loc))
        case ("INT8_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Neq, e1, e2, loc))
        case ("INT8_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Lt, e1, e2, loc))
        case ("INT8_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Le, e1, e2, loc))
        case ("INT8_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Gt, e1, e2, loc))
        case ("INT8_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int8Op.Ge, e1, e2, loc))
        case ("INT16_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int16Op.Neg, e1, loc))
        case ("INT16_NOT", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int16Op.Not, e1, loc))
        case ("INT16_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Add, e1, e2, loc))
        case ("INT16_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Sub, e1, e2, loc))
        case ("INT16_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Mul, e1, e2, loc))
        case ("INT16_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Div, e1, e2, loc))
        case ("INT16_REM", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Rem, e1, e2, loc))
        case ("INT16_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Exp, e1, e2, loc))
        case ("INT16_AND", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.And, e1, e2, loc))
        case ("INT16_OR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Or, e1, e2, loc))
        case ("INT16_XOR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Xor, e1, e2, loc))
        case ("INT16_SHL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Shl, e1, e2, loc))
        case ("INT16_SHR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Shr, e1, e2, loc))
        case ("INT16_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Eq, e1, e2, loc))
        case ("INT16_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Neq, e1, e2, loc))
        case ("INT16_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Lt, e1, e2, loc))
        case ("INT16_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Le, e1, e2, loc))
        case ("INT16_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Gt, e1, e2, loc))
        case ("INT16_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int16Op.Ge, e1, e2, loc))
        case ("INT32_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int32Op.Neg, e1, loc))
        case ("INT32_NOT", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int32Op.Not, e1, loc))
        case ("INT32_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Add, e1, e2, loc))
        case ("INT32_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Sub, e1, e2, loc))
        case ("INT32_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Mul, e1, e2, loc))
        case ("INT32_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Div, e1, e2, loc))
        case ("INT32_REM", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Rem, e1, e2, loc))
        case ("INT32_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Exp, e1, e2, loc))
        case ("INT32_AND", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.And, e1, e2, loc))
        case ("INT32_OR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Or, e1, e2, loc))
        case ("INT32_XOR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Xor, e1, e2, loc))
        case ("INT32_SHL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Shl, e1, e2, loc))
        case ("INT32_SHR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Shr, e1, e2, loc))
        case ("INT32_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Eq, e1, e2, loc))
        case ("INT32_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Neq, e1, e2, loc))
        case ("INT32_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Lt, e1, e2, loc))
        case ("INT32_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Le, e1, e2, loc))
        case ("INT32_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Gt, e1, e2, loc))
        case ("INT32_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int32Op.Ge, e1, e2, loc))
        case ("INT64_NEG", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int64Op.Neg, e1, loc))
        case ("INT64_NOT", e1 :: Nil) => Validation.Success(Expr.Unary(SemanticOp.Int64Op.Not, e1, loc))
        case ("INT64_ADD", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Add, e1, e2, loc))
        case ("INT64_SUB", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Sub, e1, e2, loc))
        case ("INT64_MUL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Mul, e1, e2, loc))
        case ("INT64_DIV", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Div, e1, e2, loc))
        case ("INT64_REM", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Rem, e1, e2, loc))
        case ("INT64_EXP", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Exp, e1, e2, loc))
        case ("INT64_AND", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.And, e1, e2, loc))
        case ("INT64_OR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Or, e1, e2, loc))
        case ("INT64_XOR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Xor, e1, e2, loc))
        case ("INT64_SHL", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Shl, e1, e2, loc))
        case ("INT64_SHR", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Shr, e1, e2, loc))
        case ("INT64_EQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Eq, e1, e2, loc))
        case ("INT64_NEQ", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Neq, e1, e2, loc))
        case ("INT64_LT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Lt, e1, e2, loc))
        case ("INT64_LE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Le, e1, e2, loc))
        case ("INT64_GT", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Gt, e1, e2, loc))
        case ("INT64_GE", e1 :: e2 :: Nil) => Validation.Success(Expr.Binary(SemanticOp.Int64Op.Ge, e1, e2, loc))
        case ("CHANNEL_GET", e1 :: Nil) => Validation.Success(Expr.GetChannel(e1, loc))
        case ("CHANNEL_PUT", e1 :: e2 :: Nil) => Validation.Success(Expr.PutChannel(e1, e2, loc))
        case ("CHANNEL_NEW", e1 :: e2 :: Nil) => Validation.Success(Expr.NewChannel(e1, e2, loc))
        case ("ARRAY_NEW", e1 :: e2 :: e3 :: Nil) => Validation.Success(Expr.ArrayNew(e1, e2, e3, loc))
        case ("ARRAY_LENGTH", e1 :: Nil) => Validation.Success(Expr.ArrayLength(e1, loc))
        case ("ARRAY_LOAD", e1 :: e2 :: Nil) => Validation.Success(Expr.ArrayLoad(e1, e2, loc))
        case ("ARRAY_STORE", e1 :: e2 :: e3 :: Nil) => Validation.Success(Expr.ArrayStore(e1, e2, e3, loc))
        case ("VECTOR_GET", e1 :: e2 :: Nil) => Validation.Success(Expr.VectorLoad(e1, e2, loc))
        case ("VECTOR_LENGTH", e1 :: Nil) => Validation.Success(Expr.VectorLength(e1, loc))
        case _ =>
          val error = UndefinedIntrinsic(loc)
          sctx.errors.add(error)
          Validation.Success(Expr.Error(error))
      }
    }
  }

  private object Patterns {
    def pickPattern(tree: Tree)(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      flatMapN(pick(TreeKind.Pattern.Pattern, tree))(visitPattern(_))
    }

    def visitPattern(tree: Tree, seen: collection.mutable.Map[String, Name.Ident] = collection.mutable.Map.empty)(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Pattern)
      tree.children.headOption match {
        case Some(tree: Tree) => tree.kind match {
          case TreeKind.Pattern.Variable => visitVariablePat(tree, seen)
          case TreeKind.Pattern.Literal => visitLiteralPat(tree)
          case TreeKind.Pattern.Tag => visitTagPat(tree, seen)
          case TreeKind.Pattern.Tuple => visitTuplePat(tree, seen)
          case TreeKind.Pattern.Record => visitRecordPat(tree, seen)
          case TreeKind.Pattern.Unary => visitUnaryPat(tree)
          case TreeKind.Pattern.FCons => visitFConsPat(tree, seen)
          // Avoid double reporting errors by returning a success here
          case TreeKind.ErrorTree(_) => Validation.Success(Pattern.Error(tree.loc))
          case _ =>
            val error = UnexpectedToken(NamedTokenSet.Pattern, actual = None, SyntacticContext.Pat.OtherPat, loc = tree.loc)
            sctx.errors.add(error)
            Validation.Success(Pattern.Error(tree.loc))
        }
        case _ => throw InternalCompilerException(s"Expected Pattern.Pattern to have tree child", tree.loc)
      }
    }

    private def visitVariablePat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Variable)
      mapN(pickNameIdent(tree))(
        ident => {
          if (ident.name == "_")
            Pattern.Wild(tree.loc)
          else {
            seen.get(ident.name) match {
              case Some(other) =>
                val error = NonLinearPattern(ident.name, other.loc, tree.loc)
                sctx.errors.add(error)
                Pattern.Var(ident, tree.loc)
              case None =>
                seen += (ident.name -> ident)
                Pattern.Var(ident, tree.loc)
            }
          }
        }
      )
    }

    private def visitLiteralPat(tree: Tree)(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Literal)
      mapN(Exprs.visitLiteralExpr(tree)) {
        case Expr.Cst(cst, _) => cst match {
          case Constant.Null =>
            val error = IllegalNullPattern(tree.loc)
            sctx.errors.add(error)
            WeededAst.Pattern.Error(tree.loc)
          case Constant.Regex(_) =>
            val error = IllegalRegexPattern(tree.loc)
            sctx.errors.add(error)
            WeededAst.Pattern.Error(tree.loc)
          case c =>
            Pattern.Cst(c, tree.loc)
        }
        // Avoid double reporting errors
        case Expr.Error(_) => Pattern.Error(tree.loc)
        case e => throw InternalCompilerException(s"Malformed Pattern.Literal. Expected literal but found $e", e.loc)
      }
    }

    private def visitTagPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Tag)
      val maybePat = tryPick(TreeKind.Pattern.Tuple, tree)
      mapN(pickQName(tree), traverseOpt(maybePat)(visitTuplePat(_, seen))) {
        (qname, maybePat) =>
          maybePat match {
            case None =>
              // Synthetically add unit pattern to tag
              val lit = Pattern.Cst(Constant.Unit, tree.loc.asSynthetic)
              Pattern.Tag(qname, lit, tree.loc)
            case Some(pat) => Pattern.Tag(qname, pat, tree.loc)
          }
      }
    }

    private def visitTuplePat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Tuple)
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      mapN(traverse(patterns)(visitPattern(_, seen))) {
        case Nil => Pattern.Cst(Constant.Unit, tree.loc)
        case x :: Nil => x
        case xs => Pattern.Tuple(xs, tree.loc)
      }
    }

    private def visitRecordPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Record)
      val fields = pickAll(TreeKind.Pattern.RecordFieldFragment, tree)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree)
      mapN(traverse(fields)(visitRecordField(_, seen)), traverseOpt(maybePattern)(visitPattern(_, seen))) {

        // Pattern { ... }
        case (fs, None) =>
          Pattern.Record(fs, Pattern.RecordEmpty(tree.loc.asSynthetic), tree.loc)

        // Pattern { x, ... | r }
        case (x :: xs, Some(Pattern.Var(v, l))) =>
          Pattern.Record(x :: xs, Pattern.Var(v, l), tree.loc)

        // Pattern { x, ... | _ }
        case (x :: xs, Some(Pattern.Wild(l))) =>
          Pattern.Record(x :: xs, Pattern.Wild(l), tree.loc)

        // Illegal pattern: { | r }
        case (Nil, Some(r)) =>
          val error = EmptyRecordExtensionPattern(r.loc)
          sctx.errors.add(error)
          Pattern.Error(r.loc)

        // Illegal pattern: { x, ... | (1, 2, 3) }
        case (_, Some(r)) =>
          val error = IllegalRecordExtensionPattern(r.loc)
          sctx.errors.add(error)
          Pattern.Error(r.loc)
      }
    }

    private def visitRecordField(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern.Record.RecordLabelPattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.RecordFieldFragment)
      val maybePattern = tryPick(TreeKind.Pattern.Pattern, tree)
      mapN(pickNameIdent(tree), traverseOpt(maybePattern)(visitPattern(_, seen))) {
        case (ident, None) =>
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc)
            case Some(other) =>
              val error = NonLinearPattern(ident.name, other.loc, ident.loc)
              sctx.errors.add(error)
              Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), None, tree.loc)
          }
        case (ident, pat) =>
          Pattern.Record.RecordLabelPattern(Name.mkLabel(ident), pat, tree.loc)
      }
    }

    private def visitUnaryPat(tree: Tree)(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.Unary)
      val NumberLiteralKinds = List(TokenKind.LiteralInt8, TokenKind.LiteralInt16, TokenKind.LiteralInt32, TokenKind.LiteralInt64, TokenKind.LiteralBigInt, TokenKind.LiteralFloat32, TokenKind.LiteralFloat64, TokenKind.LiteralBigDecimal)
      val literalToken = tree.children(1) match {
        case t@Token(_, _, _, _, _, _) if NumberLiteralKinds.contains(t.kind) => Some(t)
        case _ => None
      }
      flatMapN(pick(TreeKind.Operator, tree))(_.children(0) match {
        case opToken@Token(_, _, _, _, _, _) =>
          literalToken match {
            // fold unary minus into a constant, and visit it like any other constant
            case Some(lit) if opToken.text == "-" =>
              // Construct a synthetic literal tree with the unary minus and visit that like any other literal expression
              val syntheticToken = Token(lit.kind, lit.src, opToken.start, lit.end, lit.sp1, lit.sp2)
              val syntheticLiteral = Tree(TreeKind.Pattern.Literal, Array(syntheticToken), tree.loc.asSynthetic)
              visitLiteralPat(syntheticLiteral)
            case _ => throw InternalCompilerException(s"No number literal found for unary '-'", tree.loc)
          }
        case _ => throw InternalCompilerException(s"Expected unary operator but found tree", tree.loc)
      })
    }

    private def visitFConsPat(tree: Tree, seen: collection.mutable.Map[String, Name.Ident])(implicit sctx: SharedContext): Validation[Pattern, CompilationMessage] = {
      expect(tree, TreeKind.Pattern.FCons)
      // FCons are rewritten into tag patterns
      val patterns = pickAll(TreeKind.Pattern.Pattern, tree)
      mapN(traverse(patterns)(visitPattern(_, seen))) {
        case pat1 :: pat2 :: Nil =>
          val qname = Name.mkQName("List.Cons", tree.loc)
          val pat = Pattern.Tuple(List(pat1, pat2), tree.loc)
          Pattern.Tag(qname, pat, tree.loc)
        case pats => throw InternalCompilerException(s"Pattern.FCons expected 2 but found '${pats.length}' sub-patterns", tree.loc)
      }
    }
  }

  private object Constants {
    private def tryParseFloat(token: Token, after: (String, SourceLocation) => Expr)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      try {
        after(token.text.filterNot(_ == '_'), loc)
      } catch {
        case _: NumberFormatException =>
          val error = MalformedFloat(loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    private def tryParseInt(token: Token, suffix: String, after: (Int, String, SourceLocation) => Expr)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      try {
        val radix = if (token.text.contains("0x")) 16 else 10
        val digits = token.text.replaceFirst("0x", "").stripSuffix(suffix).filterNot(_ == '_')
        after(radix, digits, loc)
      } catch {
        case _: NumberFormatException =>
          val error = MalformedInt(loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    /**
      * Attempts to parse the given tree to a float32.
      */
    def toFloat32(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token,
        (text, loc) => Expr.Cst(Constant.Float32(text.stripSuffix("f32").toFloat), loc)
      )

    /**
      * Attempts to parse the given tree to a float32.
      */
    def toFloat64(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token,
        (text, loc) => Expr.Cst(Constant.Float64(text.stripSuffix("f64").toDouble), loc)
      )

    /**
      * Attempts to parse the given tree to a big decimal.
      */
    def toBigDecimal(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseFloat(token, (text, loc) => {
        val bigDecimal = new java.math.BigDecimal(text.stripSuffix("ff"))
        Expr.Cst(Constant.BigDecimal(bigDecimal), loc)
      })

    /**
      * Attempts to parse the given tree to a int8.
      */
    def toInt8(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "i8", (radix, digits, loc) =>
        Expr.Cst(Constant.Int8(JByte.parseByte(digits, radix)), loc)
      )

    /**
      * Attempts to parse the given tree to a int16.
      */
    def toInt16(token: Token)(implicit sctx: SharedContext): Expr = {
      tryParseInt(token, "i16", (radix, digits, loc) =>
        Expr.Cst(Constant.Int16(JShort.parseShort(digits, radix)), loc)
      )
    }

    /**
      * Attempts to parse the given tree to a int32.
      */
    def toInt32(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "i32", (radix, digits, loc) =>
        Expr.Cst(Constant.Int32(JInt.parseInt(digits, radix)), loc)
      )

    /**
      * Attempts to parse the given tree to a int64.
      */
    def toInt64(token: Token)(implicit sctx: SharedContext): Expr = {
      tryParseInt(token, "i64", (radix, digits, loc) =>
        Expr.Cst(Constant.Int64(JLong.parseLong(digits, radix)), loc)
      )
    }

    /**
      * Attempts to parse the given tree to a int64.
      */
    def toBigInt(token: Token)(implicit sctx: SharedContext): Expr =
      tryParseInt(token, "ii", (radix, digits, loc) =>
        Expr.Cst(Constant.BigInt(new java.math.BigInteger(digits, radix)), loc)
      )

    /**
      * Attempts to compile the given regular expression into a Pattern.
      */
    def toRegex(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("regex\"").stripSuffix("\"")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      try {
        val pattern = JPattern.compile(processed)
        Expr.Cst(Constant.Regex(pattern), loc)
      } catch {
        case ex: PatternSyntaxException =>
          val error = MalformedRegex(token.text, ex.getMessage, loc)
          sctx.errors.add(error)
          WeededAst.Expr.Error(error)
      }
    }

    def visitChars(str: String, loc: SourceLocation): (String, List[CompilationMessage]) = {
      @tailrec
      def visit(chars: List[Char], acc: List[Char], accErr: List[CompilationMessage]): (String, List[CompilationMessage]) = {
        chars match {
          // Case 1: End of the sequence
          case Nil => (acc.reverse.mkString, accErr)
          // Case 2: Escaped character literal
          case esc :: c :: rest if esc == '\\' =>
            c match {
              case 'n' => visit(rest, '\n' :: acc, accErr)
              case 'r' => visit(rest, '\r' :: acc, accErr)
              case '\\' => visit(rest, '\\' :: acc, accErr)
              case '\"' => visit(rest, '\"' :: acc, accErr)
              case '\'' => visit(rest, '\'' :: acc, accErr)
              case 't' => visit(rest, '\t' :: acc, accErr)
              // Special flix escapes for string interpolations
              case '$' => visit(rest, '$' :: acc, accErr)
              case '%' => visit(rest, '%' :: acc, accErr)
              // Case unicode escape "\u1234".
              case 'u' => rest match {
                case d1 :: d2 :: d3 :: d4 :: rest2 =>
                  // Doing manual flatMap here to keep recursive call in tail-position
                  visitHex(d1, d2, d3, d4) match {
                    case Result.Ok(c) => visit(rest2, c :: acc, accErr)
                    case Result.Err(error) => visit(rest2, d1 :: d2 :: d3 :: d4 :: acc, error :: accErr)
                  }
                // less than 4 chars were left in the string
                case rest2 =>
                  val malformedCode = rest2.takeWhile(_ != '\\').mkString("")
                  val err = MalformedUnicodeEscapeSequence(malformedCode, loc)
                  visit(rest2, malformedCode.toList ++ acc, err :: accErr)
              }
              case c => visit(rest, c :: acc, IllegalEscapeSequence(c, loc) :: accErr)
            }
          // Case 2: Simple character literal
          case c :: rest => visit(rest, c :: acc, accErr)
        }
      }

      def visitHex(d1: Char, d2: Char, d3: Char, d4: Char): Result[Char, CompilationMessage] = {
        try {
          Result.Ok(Integer.parseInt(s"$d1$d2$d3$d4", 16).toChar)
        } catch {
          // Case: the four characters following "\u" does not make up a number
          case _: NumberFormatException => Result.Err(MalformedUnicodeEscapeSequence(s"$d1$d2$d3$d4", loc))
        }
      }

      visit(str.toList, Nil, Nil)
    }

    def toChar(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("\'").stripSuffix("\'")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      if (processed.length != 1) {
        val error = MalformedChar(processed, loc)
        sctx.errors.add(error)
        Expr.Error(error)
      } else {
        Expr.Cst(Constant.Char(processed.head), loc)
      }
    }

    def toStringCst(token: Token)(implicit sctx: SharedContext): Expr = {
      val loc = token.mkSourceLocation()
      val text = token.text.stripPrefix("\"").stripSuffix("\"")
      val (processed, errors) = visitChars(text, loc)
      errors.foreach(sctx.errors.add)
      Expr.Cst(Constant.Str(processed), loc)
    }
  }

  private object Predicates {
    def pickHead(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Predicate.Head.Atom, CompilationMessage] = {
      flatMapN(pick(TreeKind.Predicate.Head, tree))(tree => {
        flatMapN(pickNameIdent(tree), pick(TreeKind.Predicate.TermList, tree)) {
          (ident, tree) => {
            val exprs = traverse(pickAll(TreeKind.Expr.Expr, tree))(Exprs.visitExpr)
            val maybeLatTerm = tryPickLatticeTermExpr(tree)
            mapN(exprs, maybeLatTerm) {
              case (exprs, None) => Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, exprs, tree.loc)
              case (exprs, Some(term)) => Predicate.Head.Atom(Name.mkPred(ident), Denotation.Latticenal, exprs ::: term :: Nil, tree.loc)
            }
          }
        }
      })
    }

    def visitBody(parentTree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Predicate.Body, CompilationMessage] = {
      assert(parentTree.kind == TreeKind.Predicate.Body)
      val tree = unfold(parentTree)
      tree.kind match {
        case TreeKind.Predicate.Atom => visitAtom(tree)
        case TreeKind.Predicate.Guard => visitGuard(tree)
        case TreeKind.Predicate.Functional => visitFunctional(tree)
        case kind => throw InternalCompilerException(s"expected predicate body but found '$kind'", tree.loc)
      }
    }

    def visitAtom(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Predicate.Body.Atom, CompilationMessage] = {
      expect(tree, TreeKind.Predicate.Atom)
      val fixity = if (hasToken(TokenKind.KeywordFix, tree)) Fixity.Fixed else Fixity.Loose
      val polarity = if (hasToken(TokenKind.KeywordNot, tree)) Polarity.Negative else Polarity.Positive

      flatMapN(pickNameIdent(tree), pick(TreeKind.Predicate.PatternList, tree))(
        (ident, tree) => {
          val exprs = traverse(pickAll(TreeKind.Pattern.Pattern, tree))(tree => Patterns.visitPattern(tree))
          val maybeLatTerm = tryPickLatticeTermPattern(tree)
          mapN(exprs, maybeLatTerm) {
            case (pats, None) =>
              // Check for `[[IllegalFixedAtom]]`.
              val isNegativePolarity = polarity == Polarity.Negative
              val isFixedFixity = fixity == Fixity.Fixed
              val isIllegalFixedAtom = isNegativePolarity && isFixedFixity
              if (isIllegalFixedAtom) {
                val error = IllegalFixedAtom(tree.loc)
                sctx.errors.add(error)
              }
              Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, fixity, pats, tree.loc)

            case (pats, Some(term)) =>
              Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, fixity, pats ::: term :: Nil, tree.loc)
          }
        })
    }

    private def visitGuard(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Predicate.Body.Guard, CompilationMessage] = {
      expect(tree, TreeKind.Predicate.Guard)
      mapN(Exprs.pickExpr(tree))(Predicate.Body.Guard(_, tree.loc))
    }

    private def visitFunctional(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Predicate.Body.Functional, CompilationMessage] = {
      expect(tree, TreeKind.Predicate.Functional)
      val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
      mapN(Exprs.pickExpr(tree))(Predicate.Body.Functional(idents, _, tree.loc))
    }

    def visitParam(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[PredicateParam, CompilationMessage] = {
      expectAny(tree, List(TreeKind.Predicate.Param, TreeKind.Predicate.ParamUntyped))
      val types = pickAll(TreeKind.Type.Type, tree)
      val maybeLatTerm = tryPickLatticeTermType(tree)
      mapN(pickNameIdent(tree), traverse(types)(Types.visitType), maybeLatTerm) {
        case (ident, Nil, _) => PredicateParam.PredicateParamUntyped(Name.mkPred(ident), tree.loc)
        case (ident, types, None) => PredicateParam.PredicateParamWithType(Name.mkPred(ident), Denotation.Relational, types, tree.loc)
        case (ident, types, Some(latTerm)) => PredicateParam.PredicateParamWithType(Name.mkPred(ident), Denotation.Latticenal, types :+ latTerm, tree.loc)
      }
    }

    private def tryPickLatticeTermExpr(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Option[Expr], CompilationMessage] = {
      traverseOpt(tryPick(TreeKind.Predicate.LatticeTerm, tree))(Exprs.pickExpr)
    }

    private def tryPickLatticeTermType(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Option[Type], CompilationMessage] = {
      traverseOpt(tryPick(TreeKind.Predicate.LatticeTerm, tree))(Types.pickType)
    }

    private def tryPickLatticeTermPattern(tree: Tree)(implicit lctx: LocalContext, sctx: SharedContext): Validation[Option[Pattern], CompilationMessage] = {
      traverseOpt(tryPick(TreeKind.Predicate.LatticeTerm, tree))(Patterns.pickPattern)
    }

  }

  private object Types {
    def pickType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      val maybeExpression = tryPick(TreeKind.Type.Type, tree)
      flatMapN(
        traverseOpt(maybeExpression)(visitType)
      ) {
        case Some(tpe) => Validation.Success(tpe)
        // Fall back on Expr.Error. Parser has reported an error here.
        case None => Validation.Success(Type.Error(tree.loc))
      }
    }

    def tryPickTypeNoWild(tree: Tree)(implicit sctx: SharedContext): Validation[Option[Type], CompilationMessage] = {
      mapN(tryPickType(tree)) {
        case Some(Type.Var(ident, _)) if ident.isWild => None
        case t => t
      }
    }

    def tryPickType(tree: Tree)(implicit sctx: SharedContext): Validation[Option[Type], CompilationMessage] = {
      val maybeType = tryPick(TreeKind.Type.Type, tree)
      traverseOpt(maybeType)(visitType)
    }

    def tryPickEffect(tree: Tree)(implicit sctx: SharedContext): Validation[Option[Type], CompilationMessage] = {
      val maybeEffect = tryPick(TreeKind.Type.Effect, tree)
      traverseOpt(maybeEffect)(pickType)
    }

    def visitType(tree: Tree)(implicit sctx: SharedContext): Validation[WeededAst.Type, CompilationMessage] = {
      expectAny(tree, List(TreeKind.Type.Type, TreeKind.Type.Effect))
      // Visit first child and match its kind to know what to to
      val inner = unfold(tree)
      inner.kind match {
        case TreeKind.QName => Validation.Success(visitNameType(inner))
        case TreeKind.Ident => Validation.Success(visitIdentType(inner))
        case TreeKind.Type.Tuple => visitTupleType(inner)
        case TreeKind.Type.Record => visitRecordType(inner)
        case TreeKind.Type.RecordRow => visitRecordRowType(inner)
        case TreeKind.Type.Schema => visitSchemaType(inner)
        case TreeKind.Type.SchemaRow => visitSchemaRowType(inner)
        case TreeKind.Type.Native => visitNativeType(inner)
        case TreeKind.Type.Apply => visitApplyType(inner)
        case TreeKind.Type.Constant => visitConstantType(inner)
        case TreeKind.Type.Unary => visitUnaryType(inner)
        case TreeKind.Type.Binary => visitBinaryType(inner)
        case TreeKind.Type.CaseSet => Validation.Success(visitCaseSetType(inner))
        case TreeKind.Type.EffectSet => visitEffectType(inner)
        case TreeKind.Type.Ascribe => visitAscribeType(inner)
        case TreeKind.Type.Variable => Validation.Success(visitVariableType(inner))
        case TreeKind.ErrorTree(_) => Validation.Success(Type.Error(tree.loc))
        case kind => throw InternalCompilerException(s"Parser passed unknown type '$kind'", tree.loc)
      }
    }

    private def visitNameType(tree: Tree)(implicit sctx: SharedContext): Type.Ambiguous = {
      val qname = visitQName(tree)
      Type.Ambiguous(qname, tree.loc)
    }

    private def visitIdentType(tree: Tree)(implicit sctx: SharedContext): Type = {
      val ident = tokenToIdent(tree)
      if (ident.isWild)
        Type.Var(ident, tree.loc)
      else
        Type.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), tree.loc)
    }

    private def visitTupleType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Tuple)
      mapN(traverse(pickAll(TreeKind.Type.Type, tree))(visitType)) {
        case tpe :: Nil => tpe // flatten singleton tuple types
        case types => Type.Tuple(types, tree.loc)
      }
    }

    private def visitRecordType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Record)
      mapN(visitRecordRowType(tree))(Type.Record(_, tree.loc))
    }

    private def visitRecordRowType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expectAny(tree, List(TreeKind.Type.Record, TreeKind.Type.RecordRow))
      val maybeVar = tryPick(TreeKind.Type.Variable, tree).map(visitVariableType)
      val fields = pickAll(TreeKind.Type.RecordFieldFragment, tree)
      mapN(traverse(fields)(visitRecordField)) {
        fields =>
          val variable = maybeVar.getOrElse(Type.RecordRowEmpty(tree.loc))
          fields.foldRight(variable) { case ((label, tpe), acc) => Type.RecordRowExtend(label, tpe, acc, tree.loc) }
      }
    }

    private def visitRecordField(tree: Tree)(implicit sctx: SharedContext): Validation[(Name.Label, Type), CompilationMessage] = {
      expect(tree, TreeKind.Type.RecordFieldFragment)
      mapN(pickNameIdent(tree), pickType(tree))((ident, tpe) => (Name.mkLabel(ident), tpe))
    }

    private def visitSchemaType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Schema)
      val row = visitSchemaRowType(tree)
      mapN(row)(Type.Schema(_, tree.loc))
    }

    private def visitSchemaRowType(parentTree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      val rest = tryPick(TreeKind.Ident, parentTree).map(tokenToIdent) match {
        case None => WeededAst.Type.SchemaRowEmpty(parentTree.loc)
        case Some(name) => WeededAst.Type.Var(name, name.loc)
      }
      Validation.foldRight(pickAllTrees(parentTree))(Validation.Success(rest)) {
        case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithAlias =>
          mapN(pickQName(parentTree), Types.pickArguments(tree)) {
            (qname, targs) => Type.SchemaRowExtendByAlias(qname, targs, acc, tree.loc)
          }

        case (tree, acc) if tree.kind == TreeKind.Type.PredicateWithTypes =>
          val types = pickAll(TreeKind.Type.Type, tree)
          val maybeLatTerm = tryPick(TreeKind.Predicate.LatticeTerm, tree)
          mapN(pickQName(tree), traverse(types)(Types.visitType), traverseOpt(maybeLatTerm)(Types.pickType)) {
            case (qname, tps, None) => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Relational, tps, acc, tree.loc)
            case (qname, tps, Some(t)) => Type.SchemaRowExtendByTypes(qname.ident, Denotation.Latticenal, tps :+ t, acc, tree.loc)
          }

        case (_, acc) => Validation.Success(acc)
      }
    }

    private def visitNativeType(tree: Tree): Validation[Type.Native, CompilationMessage] = {
      expect(tree, TreeKind.Type.Native)
      text(tree) match {
        case head :: rest =>
          val fqn = (List(head.stripPrefix("##")) ++ rest).mkString("")
          Validation.Success(Type.Native(fqn, tree.loc))
        case Nil => throw InternalCompilerException("Parser passed empty Type.Native", tree.loc)
      }
    }

    private def visitApplyType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Apply)
      flatMapN(pickType(tree), pick(TreeKind.Type.ArgumentList, tree)) {
        (tpe, argsTree) =>
          // Curry type arguments
          val arguments = pickAll(TreeKind.Type.Argument, argsTree)
          mapN(traverse(arguments)(pickType)) {
            args => args.foldLeft(tpe) { case (acc, t2) => Type.Apply(acc, t2, tree.loc) }
          }
      }
    }

    private def visitConstantType(tree: Tree): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Constant)
      text(tree).head match {
        case "false" => Validation.Success(Type.False(tree.loc))
        case "true" => Validation.Success(Type.True(tree.loc))
        // TODO EFF-MIGRATION create dedicated Impure type
        case "Univ" => Validation.Success(Type.Complement(Type.Pure(tree.loc), tree.loc))
        case other => throw InternalCompilerException(s"'$other' used as Type.Constant ${tree.loc}", tree.loc)
      }
    }

    private def visitUnaryType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Unary)
      val types = traverse(pickAll(TreeKind.Type.Type, tree))(visitType)
      val op = pick(TreeKind.Operator, tree)
      flatMapN(op, types) {
        case (op, t :: Nil) =>
          text(op).head match {
            case "~" => Validation.Success(Type.Complement(t, tree.loc))
            case "rvnot" => Validation.Success(Type.CaseComplement(t, tree.loc))
            case "not" => Validation.Success(Type.Not(t, tree.loc))
            // UNRECOGNIZED
            case kind => throw InternalCompilerException(s"Parser passed unknown type operator '$kind'", tree.loc)
          }
        case (_, operands) => throw InternalCompilerException(s"Type.Unary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitBinaryType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Binary)
      val types = traverse(pickAll(TreeKind.Type.Type, tree))(visitType)
      val op = pick(TreeKind.Operator, tree)
      flatMapN(op, types) {
        case (op, t1 :: t2 :: Nil) =>
          text(op).head match {
            // ARROW FUNCTIONS
            case "->" => flatMapN(tryPickEffect(tree))(eff => {
              val l = tree.loc.asSynthetic
              val t1Revisitied = t1 match {
                // Normally singleton tuples `((a, b))` are treated as `(a, b)`. That's fine unless we are doing an arrow type!
                // In this case we need t1 "unflattened" so we redo the visit.
                case Type.Tuple(_, _) =>
                  val t1Tree = flatMapN(pick(TreeKind.Type.Type, tree))(t => pick(TreeKind.Type.Tuple, t))
                  val params = flatMapN(t1Tree)(t => traverse(pickAll(TreeKind.Type.Type, t))(visitType))
                  mapN(params)(params => (params.last, params.init))
                case t => Validation.Success((t, List.empty))
              }
              mapN(t1Revisitied) {
                case (lastParam, initParams) =>
                  val base = Type.Arrow(List(lastParam), eff, t2, l)
                  initParams.foldRight(base)((acc, tpe) => Type.Arrow(List(acc), None, tpe, l))
              }
            })
            // REGULAR TYPE OPERATORS
            case "+" => Validation.Success(Type.Union(t1, t2, tree.loc))
            case "-" => Validation.Success(Type.Difference(t1, t2, tree.loc))
            case "&" => Validation.Success(Type.Intersection(t1, t2, tree.loc))
            case "and" => Validation.Success(Type.And(t1, t2, tree.loc))
            case "or" => Validation.Success(Type.Or(t1, t2, tree.loc))
            case "rvadd" => Validation.Success(Type.CaseUnion(t1, t2, tree.loc))
            case "rvand" => Validation.Success(Type.CaseIntersection(t1, t2, tree.loc))
            case "rvsub" => Validation.Success(Type.CaseIntersection(t1, Type.CaseComplement(t2, tree.loc.asSynthetic), tree.loc))
            case "xor" => Validation.Success(Type.Or(
              Type.And(t1, Type.Not(t2, tree.loc), tree.loc),
              Type.And(Type.Not(t1, tree.loc), t2, tree.loc),
              tree.loc
            ))
            // UNRECOGNIZED
            case kind => throw InternalCompilerException(s"Parser passed unknown type operator '$kind'", tree.loc)
          }

        case (_, operands) => throw InternalCompilerException(s"Type.Binary tree with ${operands.length} operands", tree.loc)
      }
    }

    private def visitCaseSetType(tree: Tree)(implicit sctx: SharedContext): Type.CaseSet = {
      expect(tree, TreeKind.Type.CaseSet)
      val cases = pickAll(TreeKind.QName, tree).map(visitQName)
      Type.CaseSet(cases, tree.loc)
    }

    private def visitEffectType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.EffectSet)
      val effects = traverse(pickAll(TreeKind.Type.Type, tree))(visitType)
      mapN(effects) {
        // Default to Pure
        case Nil => Type.Pure(tree.loc)
        // Otherwise reduce effects into a union type
        case effects => effects.reduceLeft({
          case (acc, tpe) => Type.Union(acc, tpe, tree.loc)
        }: (Type, Type) => Type)
      }
    }

    private def visitAscribeType(tree: Tree)(implicit sctx: SharedContext): Validation[Type, CompilationMessage] = {
      expect(tree, TreeKind.Type.Ascribe)
      mapN(pickType(tree), pickKind(tree)) {
        (tpe, kind) => Type.Ascribe(tpe, kind, tree.loc)
      }
    }

    private def visitVariableType(tree: Tree)(implicit sctx: SharedContext): Type.Var = {
      expect(tree, TreeKind.Type.Variable)
      val ident = tokenToIdent(tree)
      Type.Var(ident, tree.loc)
    }

    def pickArguments(tree: Tree)(implicit sctx: SharedContext): Validation[List[Type], CompilationMessage] = {
      tryPick(TreeKind.Type.ArgumentList, tree)
        .map(argTree => traverse(pickAll(TreeKind.Type.Argument, argTree))(pickType))
        .getOrElse(Validation.Success(List.empty))
    }

    def pickDerivations(tree: Tree)(implicit sctx: SharedContext): Derivations = {
      val maybeDerivations = tryPick(TreeKind.DerivationList, tree)
      val loc = maybeDerivations.map(_.loc).getOrElse(SourceLocation.Unknown)
      val derivations = maybeDerivations.toList.flatMap {
        tree => pickAll(TreeKind.QName, tree).map(visitQName)
      }
      Derivations(derivations, loc)
    }

    def pickParameters(tree: Tree)(implicit sctx: SharedContext): Validation[List[TypeParam], CompilationMessage] = {
      tryPick(TreeKind.TypeParameterList, tree) match {
        case None => Validation.Success(Nil)
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree)
          mapN(traverse(parameters)(visitParameter)) {
            tparams =>
              val kinded = tparams.collect { case t: TypeParam.Kinded => t }
              val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
              (kinded, unkinded) match {
                // Only unkinded type parameters
                case (Nil, _ :: _) => tparams
                // Only kinded type parameters
                case (_ :: _, Nil) => tparams
                // Some kinded and some unkinded type parameters. Give an error and keep going.
                case (_ :: _, _ :: _) =>
                  val error = MismatchedTypeParameters(tparamsTree.loc)
                  sctx.errors.add(error)
                  tparams
                case (Nil, Nil) =>
                  throw InternalCompilerException("Parser produced empty type parameter tree", tparamsTree.loc)
              }
          }
      }
    }

    def pickKindedParameters(tree: Tree)(implicit sctx: SharedContext): Validation[List[TypeParam], CompilationMessage] = {
      tryPick(TreeKind.TypeParameterList, tree) match {
        case None => Validation.Success(Nil)
        case Some(tparamsTree) =>
          val parameters = pickAll(TreeKind.Parameter, tparamsTree)
          mapN(traverse(parameters)(visitParameter)) {
            tparams =>
              val kinded = tparams.collect { case t: TypeParam.Kinded => t }
              val unkinded = tparams.collect { case t: TypeParam.Unkinded => t }
              (kinded, unkinded) match {
                // Only kinded type parameters
                case (_ :: _, Nil) => tparams
                // Some kinded and some unkinded type parameters. We recover by kinding the unkinded ones as Ambiguous.
                case (_, _ :: _) =>
                  unkinded.foreach(t => sctx.errors.add(MissingTypeParamKind(t.ident.loc)))
                  tparams
                case (Nil, Nil) =>
                  throw InternalCompilerException("Parser produced empty type parameter tree", tparamsTree.loc)
              }
          }
      }
    }

    def pickSingleParameter(tree: Tree)(implicit sctx: SharedContext): Validation[TypeParam, CompilationMessage] = {
      val tparams = pick(TreeKind.TypeParameterList, tree)
      flatMapN(tparams) {
        tparams => flatMapN(pick(TreeKind.Parameter, tparams))(visitParameter)
      }
    }

    def visitParameter(tree: Tree)(implicit sctx: SharedContext): Validation[TypeParam, CompilationMessage] = {
      expect(tree, TreeKind.Parameter)
      mapN(pickNameIdent(tree)) {
        ident =>
          tryPickKind(tree)
            .map(kind => TypeParam.Kinded(ident, kind))
            .getOrElse(TypeParam.Unkinded(ident))
      }
    }

    def pickConstraints(tree: Tree)(implicit sctx: SharedContext): Validation[List[TraitConstraint], CompilationMessage] = {
      val maybeWithClause = tryPick(TreeKind.Type.ConstraintList, tree)
      maybeWithClause.map(
        withClauseTree => traverse(pickAll(TreeKind.Type.Constraint, withClauseTree))(visitTraitConstraint)
      ).getOrElse(Validation.Success(List.empty))
    }

    private def visitTraitConstraint(tree: Tree)(implicit sctx: SharedContext): Validation[TraitConstraint, CompilationMessage] = {
      def replaceIllegalTypesWithErrors(tpe: Type): (Type, List[SourceLocation]) = {
        val errorLocations = mutable.ArrayBuffer.empty[SourceLocation]

        def replace(tpe0: Type): Type = tpe0 match {
          case Type.Var(ident, loc) => Type.Var(ident, loc)
          case Type.Apply(t1, t2, loc) => Type.Apply(replace(t1), replace(t2), loc)
          case t =>
            errorLocations += t.loc
            Type.Error(t.loc)
        }

        (replace(tpe), errorLocations.toList)
      }

      expect(tree, TreeKind.Type.Constraint)
      mapN(pickQName(tree), Types.pickType(tree)) {
        (qname, tpe) =>
          // Check for illegal type constraint parameter
          val (tpe1, errors) = replaceIllegalTypesWithErrors(tpe)
          errors.headOption.map(loc => sctx.errors.add(IllegalTraitConstraintParameter(loc)))
          TraitConstraint(qname, tpe1, tree.loc)
      }
    }

    private def visitKind(tree: Tree)(implicit sctx: SharedContext): Validation[Kind, CompilationMessage] = {
      expect(tree, TreeKind.Kind)
      mapN(pickNameIdent(tree)) {
        ident => {
          val kind = Kind.Ambiguous(Name.QName(Name.RootNS, ident, ident.loc), ident.loc)
          tryPick(TreeKind.Kind, tree)
          tryPickKind(tree)
            .map(Kind.Arrow(kind, _, tree.loc))
            .getOrElse(kind)
        }
      }
    }

    private def pickKind(tree: Tree)(implicit sctx: SharedContext): Validation[Kind, CompilationMessage] = {
      flatMapN(pick(TreeKind.Kind, tree))(visitKind)
    }

    def tryPickKind(tree: Tree)(implicit sctx: SharedContext): Option[Kind] = {
      // Cast a missing kind to None because 'tryPick' means that it's okay not to find a kind here.
      tryPick(TreeKind.Kind, tree).flatMap(visitKind(_).toResult.toOption)
    }
  }

  private def pickQName(tree: Tree)(implicit sctx: SharedContext): Validation[Name.QName, CompilationMessage] = {
    mapN(pick(TreeKind.QName, tree))(visitQName)
  }

  private def tryPickQName(tree: Tree)(implicit sctx: SharedContext): Option[Name.QName] = {
    tryPick(TreeKind.QName, tree).map(visitQName)
  }

  private def visitQName(tree: Tree)(implicit sctx: SharedContext): Name.QName = {
    expect(tree, TreeKind.QName)
    val idents = pickAll(TreeKind.Ident, tree).map(tokenToIdent)
    assert(idents.nonEmpty) // We require at least one element to construct a qname
    val first = idents.head
    val ident = idents.last
    val nnameIdents = idents.dropRight(1)
    val loc = SourceLocation(isReal = true, first.loc.sp1, ident.loc.sp2)
    val nname = Name.NName(nnameIdents, loc)
    Name.QName(nname, ident, loc)
  }

  private def pickNameIdent(tree: Tree)(implicit sctx: SharedContext): Validation[Name.Ident, CompilationMessage] = {
    mapN(pick(TreeKind.Ident, tree))(tokenToIdent)
  }

  private def tryPickNameIdent(tree: Tree)(implicit sctx: SharedContext): Option[Name.Ident] = {
    tryPick(TreeKind.Ident, tree).map(tokenToIdent)
  }

  def pickJavaName(tree: Tree): Validation[Name.JavaName, CompilationMessage] = {
    val idents = pickQNameIdents(tree)
    mapN(idents) {
      idents => Name.JavaName(idents, tree.loc)
    }
  }

  private def pickQNameIdents(tree: Tree): Validation[List[String], CompilationMessage] = {
    flatMapN(pick(TreeKind.QName, tree)) {
      qname => mapN(traverse(pickAll(TreeKind.Ident, qname))(t => Validation.Success(text(t))))(_.flatten)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  /// HELPERS ////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////

  /**
    * Checks that tree has expected kind by wrapping assert.
    * This provides an isolated spot for deciding what to do with unexpected kinds.
    */
  private def expect(tree: Tree, kind: TreeKind): Unit = assert(tree.kind == kind)


  /**
    * Checks that tree has one of expected kinds by wrapping assert.
    * This provides an isolated spot for deciding what to do with unexpected kinds.
    */
  private def expectAny(tree: Tree, kinds: List[TreeKind]): Unit = assert(kinds.contains(tree.kind))

  /**
    * Picks first child from a tree and if it is a [[Token]] turns it into a Name.Ident.
    * Only use this if the structure of tree is well-known.
    * IE. Calling on a tree of kind [[TreeKind.Ident]] is fine, but if the kind is not known avoid using [[tokenToIdent]].
    */
  private def tokenToIdent(tree: Tree)(implicit sctx: SharedContext): Name.Ident = {
    tree.children.headOption match {
      case Some(token@Token(_, _, _, _, sp1, sp2)) =>
        Name.Ident(token.text, SourceLocation(isReal = true, sp1, sp2))
      // If child is an ErrorTree, that means the parse already reported and error.
      // We can avoid double reporting by returning a success here.
      // Doing it this way is most resilient, but phases down the line might have trouble with this sort of thing.
      case Some(t: Tree) if t.kind.isInstanceOf[TreeKind.ErrorTree] =>
        val name = text(tree).mkString("")
        Name.Ident(name, tree.loc)
      case Some(t: Tree) if t.kind == TreeKind.CommentList =>
        // We hit a misplaced comment.
        val name = text(tree).mkString("")
        val error = MisplacedComments(SyntacticContext.Unknown, t.loc)
        sctx.errors.add(error)
        Name.Ident(name, tree.loc)
      case _ => throw InternalCompilerException(s"Parse failure: expected first child of '${tree.kind}' to be Child.Token", tree.loc)
    }
  }

  /**
    * Turns a Name.QName into a string by removing prefix "##" and joining with ".".
    */
  private def javaQnameToFqn(qname: Name.QName): String = {
    (qname.namespace.idents.map(_.name.stripPrefix("##")) :+ qname.ident.name).mkString(".")
  }

  /**
    * When kinds are elided they default to the kind `Type`.
    */
  private def defaultKind(ident: Name.Ident): Kind = Kind.Ambiguous(Name.mkQName("Type"), ident.loc.asSynthetic)

  /**
    * Plucks the first inner tree in children.
    * This is intended to be used to unfold the inner tree on special marker [[TreeKind]]s,
    * such as [[TreeKind.Type.Type]] or [[TreeKind.Expr.Expr]].
    * The parser guarantees that these tree kinds have at least a single child.
    */
  private def unfold(tree: Tree): Tree = {
    assert(tree.kind match {
      case TreeKind.Type.Type | TreeKind.Type.Effect | TreeKind.Expr.Expr | TreeKind.Predicate.Body => true
      case _ => false
    })

    // Find the first sub-tree that isn't a comment
    tree.children.find {
      case tree: Tree if tree.kind != TreeKind.CommentList => true
      case _ => false
    }.map {
      case tree: Tree => tree
      case _ => throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    }.getOrElse(
      throw InternalCompilerException(s"expected '${tree.kind}' to have a tree child that is not a comment", tree.loc)
    )
  }

  /**
    * Tries to find a token child of a specific [[TokenKind]].
    */
  private def hasToken(kind: TokenKind, tree: Tree): Boolean = {
    tree.children.exists {
      case Token(k, _, _, _, _, _) => k == kind
      case _ => false
    }
  }

  /**
    * Collects all immediate child trees from a tree.
    */
  private def pickAllTrees(tree: Tree): List[Tree] = {
    tree.children.collect {
      case t: Tree => t
    }.toList
  }

  /**
    * Collects all immediate child tokens from a tree.
    */
  private def pickAllTokens(tree: Tree): Array[Token] = {
    tree.children.collect { case token@Token(_, _, _, _, _, _) => token }
  }

  /**
    * Collects the text in immediate token children
    */
  private def text(tree: Tree): List[String] = {
    tree.children.foldLeft[List[String]](List.empty)((acc, c) => c match {
      case token@Token(_, _, _, _, _, _) => acc :+ token.text
      case _ => acc
    })
  }

  /**
    * Picks out the first sub-tree of a specific [[TreeKind]].
    */
  private def pick(kind: TreeKind, tree: Tree, synctx: SyntacticContext = SyntacticContext.Unknown): Validation[Tree, CompilationMessage] = {
    tryPick(kind, tree) match {
      case Some(t) => Validation.Success(t)
      case None =>
        val error = NeedAtleastOne(NamedTokenSet.FromTreeKinds(Set(kind)), synctx, loc = tree.loc)
        Validation.Failure(Chain(error))
    }
  }

  /**
    * Tries to pick out the first sub-tree of a specific [[TreeKind]].
    */
  private def tryPick(kind: TreeKind, tree: Tree): Option[Tree] = {
    tree.children.find {
      case tree: Tree if tree.kind == kind => true
      case _ => false
    } match {
      case Some(tree: Tree) => Some(tree)
      case _ => None
    }
  }

  /**
    * Picks out all the sub-trees of a specific [[TreeKind]].
    */
  private def pickAll(kind: TreeKind, tree: Tree): List[Tree] = {
    tree.children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case tree: Tree if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }

  /**
    * Gets duplicate pairs from a list of items.
    * This is used to generate a list of pairs that can be mapped into Duplicate* errors.
    * What constitutes a "duplicate" is abstracted into the groupBy argument.
    * For instance, in the case of annotations, if the [[TokenKind]] of two annotations are equal then they form a duplicate pair.
    * But for enum variants, two variants are duplicates if they share names.
    */
  private def getDuplicates[A, K](items: Seq[A], groupBy: A => K): List[(A, A)] = {
    val duplicates = items.groupBy(groupBy).collect { case (_, is) if is.length > 1 => is }
    val pairs = duplicates.map(dups => {
      for {
        (x, idxX) <- dups.zipWithIndex
        (y, idxY) <- dups.zipWithIndex
        if (idxX + 1) == idxY
      } yield (x, y)
    })
    List.from(pairs.flatten)
  }

  /**
    * Ternary enumeration of constraints on the presence of something.
    */
  private sealed trait Presence

  private object Presence {
    /**
      * Indicates that the thing is required.
      */
    case object Required extends Presence

    /**
      * Indicates that the thing is optional.
      */
    case object Optional extends Presence

    /**
      * Indicates that the thing is forbidden.
      */
    case object Forbidden extends Presence
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
    * @param errors the [[WeederError]]s or [[ParserError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[CompilationMessage])

  /**
    * Companion object for [[LocalContext]]
    */
  private object LocalContext {
    /**
      * Returns a fresh local context.
      */
    def mk(): LocalContext = new LocalContext(0)
  }

  /**
    * A local context.
    *
    * @param depth tracks the nesting of expressions to avoid stack overflow in the compiler.
    */
  private case class LocalContext(var depth: Int)

}
