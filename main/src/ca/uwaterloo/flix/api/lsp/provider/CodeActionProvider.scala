/*
 * Copyright 2023 Holger Dal Mogensen
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionKind, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.Modifier
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope}
import ca.uwaterloo.flix.language.errors.{InstanceError, ResolutionError, TypeError}
import ca.uwaterloo.flix.util.Similarity

/**
  * The CodeActionProvider offers quickfix suggestions.
  *
  * The space of possible quickfixes is huge. Hence, we should only offer quickfixes that are meaningfully useful.
  *
  * That is, quickfix suggestions that will often be used: either because they offer good ergonomics or because they resolve a tricky issue.
  */
object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = {
    getActionsFromErrors(uri, range, errors) ++ getActionsFromRange(uri, range)
  }

  private def getActionsFromErrors(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[CodeAction] = errors.flatMap {
    case ResolutionError.UndefinedEffect(qn, ap,  _, loc) if overlaps(range, loc) =>
      mkUseEffect(qn.ident, uri, ap)

    case ResolutionError.UndefinedStruct(qn, ap, loc) if overlaps(range, loc) =>
      mkUseStruct(qn.ident, uri, ap) ++ mkNewStruct(qn.ident.name, uri, ap)

    case ResolutionError.UndefinedJvmClass(name, ap, _, loc) if overlaps(range, loc) =>
      mkImportJava(Name.mkQName(name), uri, ap)

    case ResolutionError.UndefinedName(qn, ap, env, loc) if overlaps(range, loc) =>
      mkFixMisspelling(qn, loc, env, uri) ++ mkUseDef(qn.ident, uri, ap) ++ mkImportJava(qn, uri, ap) ++ mkNewDef(qn.ident.name, uri, ap)

    case ResolutionError.UndefinedTrait(qn, ap,  _, _, loc) if overlaps(range, loc) =>
      mkUseTrait(qn.ident, uri, ap)

    case ResolutionError.UndefinedTag(name, ap, _, loc) if overlaps(range, loc) =>
      mkUseTag(name, uri, ap) ++ mkQualifyTag(name, uri, loc)

    case ResolutionError.UndefinedType(qn, ap, _, loc) if overlaps(range, loc) =>
      mkUseType(qn.ident, uri, ap) ++ mkImportJava(qn, uri, ap) ++ mkNewEnum(qn.ident.name, uri, ap) ++ mkNewStruct(qn.ident.name, uri, ap)

    case TypeError.MissingInstanceEq(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingEq(tpe, uri)

    case TypeError.MissingInstanceOrder(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingOrder(tpe, uri)

    case TypeError.MissingInstanceToString(tpe, _, loc) if overlaps(range, loc) =>
      mkDeriveMissingToString(tpe, uri)

    case InstanceError.MissingSuperTraitInstance(tpe, _, sup, loc) if overlaps(range, loc) =>
      mkDeriveMissingSuperTrait(tpe, sup, uri)

    case _ => Nil
  }

  /**
    * Returns code actions based on the current index and the given range.
    */
  private def getActionsFromRange(uri: String, range: Range)(implicit root: Root): List[CodeAction] = {
    root.enums.foldLeft(List.empty[CodeAction]) {
      case (acc, (sym, enm)) if overlaps(range, sym.loc) =>
        List(mkDeriveEq(enm, uri), mkDeriveOrder(enm, uri), mkDeriveToString(enm, uri)).flatten ::: acc
      case (acc, _) => acc
    }
  }

  /**
    * Returns a code action that proposes to `use` a def.
    *
    * For example, if we have:
    *
    * {{{
    *   map(x -> x + 1, Nil)
    * }}}
    *
    * then we propose to insert:
    *
    * {{{
    *   use List.map;
    * }}}
    */
  private def mkUseDef(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.defs.collect {
      case (sym, defi) if isPublic(defi.spec) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to `use` a trait.
    */
  private def mkUseTrait(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.traits.collect {
      case (sym, trt) if isPublic(trt) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to `use` an effect.
    */
  private def mkUseEffect(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.effects.collect {
      case (sym, eff) if isPublic(eff) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, ap)
  }

  /**
    * Returns a code action that proposes to use the tag of an enum.
    *
    * For example, if we have:
    *
    * {{{
    *   match color {
    *      case Red => ???
    *   }
    * }}}
    *
    * where the user actually want to refer to Color.Red, this code action proposes to add:
    * {{{
    *   use Color.Red
    * }}}
    */
  private def mkUseTag(tagName: String, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val candidateEnums = root.enums.filter{ case (_, enm) => enm.cases.keys.exists(_.name == tagName) && isPublic(enm)}
    candidateEnums.keys.map{ enumName =>
      CodeAction(
        title = s"use '$enumName.$tagName'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"use $enumName.$tagName"))))),
        command = None
      )
    }.toList
  }

  /**
    * Returns a code action that proposes to qualify the tag with the enum name.
    *
    * For example, if we have:
    *
    * {{{
    *   match color {
    *      case Red => ???
    *   }
    * }}}
    *
    * where the user actually want to refer to Color.Red, this code action proposes to replace the tag with:
    * {{{
    *   case Color.Red => ???
    * }}}
    */
  private def mkQualifyTag(tagName: String, uri: String, loc: SourceLocation)(implicit root: Root): List[CodeAction] = {
    val candidateEnums = root.enums.filter{ case (_, enm) => enm.cases.keys.exists(_.name == tagName) && isPublic(enm)}
    candidateEnums.keys.map{ enumName =>
      CodeAction(
        title = s"Prefix with '$enumName.'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(TextEdit(sourceLocation2Range(loc), s"$enumName.$tagName"))))),
        command = None
      )
    }.toList
  }

  /**
    * Returns a code action that proposes to `use` a type.
    */
  private def mkUseType(ident: Name.Ident, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val enumNames = root.enums.collect { case (sym, enm) if isPublic(enm) => sym.name }
    val enumSyms = root.enums.collect { case (sym, enm) if isPublic(enm) => sym }

    val restrictableEnumNames = root.restrictableEnums.collect { case (sym, enm) if isPublic(enm) => sym.name }
    val restrictableEnumSyms = root.restrictableEnums.collect { case (sym, enm) if isPublic(enm) => sym }

    val traitNames = root.traits.collect { case (sym, trt) if isPublic(trt) => sym.name }
    val traitSyms = root.traits.collect { case (sym, trt) if isPublic(trt) => sym }

    val typeAliasNames = root.typeAliases.collect { case (sym, alias) if isPublic(alias) => sym.name }
    val typeAliasSyms = root.typeAliases.collect { case (sym, alias) if isPublic(alias) => sym }

    val names = enumNames ++ restrictableEnumNames ++ traitNames ++ typeAliasNames
    val syms = enumSyms ++ restrictableEnumSyms ++ traitSyms ++ typeAliasSyms

    mkUseSym(ident, names, syms, uri, ap)
  }

  /**
    * Checks if the given spec is public.
    */
  private def isPublic(spec: TypedAst.Spec): Boolean = spec.mod.mod.contains(Modifier.Public)

  /**
    * Checks if the given trait is public.
    */
  private def isPublic(trt: TypedAst.Trait): Boolean = trt.mod.mod.contains(Modifier.Public)

  /**
    * Checks if the given effect is public.
    */
  private def isPublic(eff: TypedAst.Effect): Boolean = eff.mod.mod.contains(Modifier.Public)

  /**
    * Checks if the given enum is public.
    */
  private def isPublic(enm: TypedAst.Enum): Boolean = enm.mod.mod.contains(Modifier.Public)

  /**
    * Checks if the given restrictable enum is public.
    */
  private def isPublic(enm: TypedAst.RestrictableEnum): Boolean = enm.mod.mod.contains(Modifier.Public)

  /**
    * Checks if the given type alias is public.
    */
  private def isPublic(alias: TypedAst.TypeAlias): Boolean = alias.mod.mod.contains(Modifier.Public)

  /**
    * Returns a code action that proposes to `use` a struct.
    */
  private def mkUseStruct(ident: Name.Ident, uri: String, position: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    val syms = root.structs.map {
      case (sym, _) => sym
    }
    mkUseSym(ident, syms.map(_.name), syms, uri, position)
  }

  /**
    * Returns a TextEdit that is inserted and indented according to the given `ap`.
    * This function will:
    *   - add leadingSpaces before the given text.
    *   - add leadingSpaces after each newline.
    *   - add a newline at the end.
    *
    * Example:
    *   Given text = "\ndef foo(): =\n", ap = AnchorPosition(line=1, col=0, spaces=4)
    *   The result will be:
    *   TextEdit(Range(Position(1, 0), Position(1, 0)), "    \n    def foo(): =\n    \n")
    */
  private def mkTextEdit(ap: AnchorPosition, text: String): TextEdit = {
    val insertPosition = Position(ap.line, ap.col)
    val leadingSpaces = " " * ap.spaces
    TextEdit(
      Range(insertPosition, insertPosition),
      leadingSpaces + text.replace("\n", s"\n$leadingSpaces") + "\n"
    )
  }

  /**
    * Internal helper function for all `mkUseX`.
    * Returns a list of code action that proposes to `use` a symbol.
    *
    * For example, if we have:
    *
    * {{{
    *   map(x -> x + 1, Nil)
    * }}}
    *
    * and we pass in the list of defs in the root,
    * then we propose to insert:
    *
    * {{{
    *   use List.map;
    * }}}
    *
    * @param ident The identifier we are searching for.
    * @param syms  The symbols that are candidates for being used.
    * @param names The names of the symbols given as `syms`.
    *              These should include the same number and be in the same order.
    * @param uri   URI of the document the change should be made in.
    */
  // Names have to be included separately because symbols aren't guaranteed to have a name
  private def mkUseSym(ident: Name.Ident, names: Iterable[String], syms: Iterable[Symbol], uri: String, ap: AnchorPosition): List[CodeAction] =
    syms.zip(names).collect {
      case (sym, name) if name == ident.name =>
        CodeAction(
          title = s"use '$sym'",
          kind = CodeActionKind.QuickFix,
          edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"use $sym;"))))),
          command = None
        )
    }.toList.sortBy(_.title)

  /**
    * Returns a code action that proposes to create a new enum.
    *
    * For example, if we have:
    *
    * {{{
    *   def foo(): Abc = ???
    * }}}
    *
    * where the `Abc` type is not defined this code action proposes to add:
    * {{{
    *   enum Abc { }
    * }}}
    */
  private def mkNewEnum(name: String, uri: String, ap: AnchorPosition): List[CodeAction] = CodeAction(
    title = s"Create enum '$name'",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(mkTextEdit(ap,
        s"""
          |enum $name {
          |
          |}
          |""".stripMargin
      )))
    )),
    command = None
  ) :: Nil

  /**
    * Returns a code action that proposes to create a new function.
    *
    * For example, if we have:
    *
    * {{{
    *   let x = f()
    * }}}
    *
    * where the name `f` is not defined this code action proposes to add:
    * {{{
    *   def f(): =
    * }}}
    */
  private def mkNewDef(name: String, uri: String, ap: AnchorPosition): List[CodeAction] = CodeAction(
    title = s"Create def '$name'",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(mkTextEdit(ap,
        s"""
          |def $name(): =
          |""".stripMargin
      )))
    )),
    command = None
  ) :: Nil

  /**
    * Returns a code action that proposes to import the corresponding Java class.
    * First, we try to import the class with the name matching the head of the `qn.namespace.idents`.
    * If there is no namespace, we try to import the class with the name matching `qn.ident`.
    *
    * Example:
    * if we have
    *
    * {{{
    *  let a = Math.sin(1)
    * }}}
    *
    * where qn.ident is "sin" and qn.namespace.idents is ["Math"],  this code action proposes to add:
    *
    * {{{
    *  import java.lang.Math
    * }}}
    */
  private def mkImportJava(qn: Name.QName, uri: String, ap: AnchorPosition)(implicit root: Root): List[CodeAction] = {
    // If `qn.namespace.idents.headOption` returns None, we use the `qn.ident.name`. Otherwise, we use the head of the namespace.
    // In the example above, headOption would return Some("Math"), so we will use "Math".
    val className = qn.namespace.idents.headOption.map(_.name).getOrElse(qn.ident.name)
    root.availableClasses.byClass.get(className).toList.flatten.map { path =>
      val completePath = path.mkString(".") + "." + className
      CodeAction(
        title = s"import '$completePath'",
        kind = CodeActionKind.QuickFix,
        edit = Some(WorkspaceEdit(Map(uri -> List(mkTextEdit(ap, s"import $completePath"))))),
        command = None
      )
    }
  }

  /**
    * Returns a code action that proposes to create a new struct.
    *
    * For example, if we have:
    *
    * {{{
    *   def foo(): Abc = ???
    * }}}
    *
    * where the `Abc` type is not defined this code action proposes to add:
    * {{{
    *   struct Abc[r] { }
    * }}}
    */
  private def mkNewStruct(name: String, uri: String, ap:AnchorPosition): List[CodeAction] = CodeAction(
    title = s"Create struct '$name'",
    kind = CodeActionKind.QuickFix,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(mkTextEdit(ap,
        s"""
           |struct $name[r] {
           |
           |}
           |""".stripMargin
      )))
    )),
    command = None
  ) :: Nil

  /**
    * Returns a list of quickfix code action to suggest possibly correct spellings.
    *
    * Uses Levenshtein Distance to find close spellings.
    */
  private def mkFixMisspelling(qn: Name.QName, loc: SourceLocation, env: LocalScope,  uri: String): List[CodeAction] = {
    val minLength = 3
    val maxDistance = 3
    val possibleNames: List[String] = env.m.toList.map(_._1).filter(n => (n.length - qn.ident.name.length).abs < maxDistance)
      .filter(n => Similarity.levenshtein(n, qn.ident.name) < maxDistance)
    if (qn.ident.name.length > minLength)
      possibleNames.map(n => mkCorrectSpelling(n, loc, uri))
    else
      Nil
  }

  /**
    * Internal helper function for `mkFixMisspelling`.
    * Returns a quickfix code action for a possibly correct name.
    */
  private def mkCorrectSpelling(correctName: String, loc: SourceLocation, uri: String): CodeAction =
    CodeAction(
      title = s"Did you mean: '$correctName'?",
      kind = CodeActionKind.QuickFix,
      edit = Some(WorkspaceEdit(
        Map(uri -> List(TextEdit(
          Range(Position.fromBegin(loc), Position.fromEnd(loc)),
          correctName
        )))
      )),
      command = None
    )

  /**
    * Returns a quickfix code action to derive the `Eq` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingEq(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Eq", uri)

  /**
    * Returns a quickfix code action to derive the `Order` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingOrder(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "Order", uri)

  /**
    * Returns a quickfix code action to derive the `ToString` trait for the given type `tpe` if it is an enum.
    */
  private def mkDeriveMissingToString(tpe: Type, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, "ToString", uri)

  /**
    * Returns a quickfix code action to derive the missing supertrait for the given subtrait.
    */
  private def mkDeriveMissingSuperTrait(tpe: Type, superTrait: Symbol.TraitSym, uri: String)(implicit root: Root): Option[CodeAction] =
    mkDeriveMissing(tpe, superTrait.name, uri)

  /**
    * Internal helper function for all `mkDeriveMissingX`.
    * Returns a quickfix code action to derive the given trait `trt`
    * for the given type `tpe` if it is an enum in the root.
    */
  private def mkDeriveMissing(tpe: Type, trt: String, uri: String)(implicit root: Root): Option[CodeAction] = tpe.typeConstructor match {
    case Some(TypeConstructor.Enum(sym, _)) =>
      root.enums.get(sym).map { e =>
        CodeAction(
          title = s"Derive '$trt'",
          kind = CodeActionKind.QuickFix,
          edit = Some(addDerivation(e, trt, uri)),
          command = None
        )
      }
    case _ => None
  }

  /**
    * Returns a code action to derive the `Eq` trait.
    */
  private def mkDeriveEq(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Eq", uri)

  /**
    * Returns a code action to derive the `Order` trait.
    */
  private def mkDeriveOrder(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "Order", uri)

  /**
    * Returns a code action to derive the `ToString` trait.
    */
  private def mkDeriveToString(e: TypedAst.Enum, uri: String): Option[CodeAction] = mkDerive(e, "ToString", uri)

  /**
    * Returns a code action to derive the given trait `trt` for the given enum `e` if it isn't already.
    * `None` otherwise.
    */
  private def mkDerive(e: TypedAst.Enum, trt: String, uri: String): Option[CodeAction] = {
    val alreadyDerived = e.derives.traits.exists(d => d.trt.name == trt)
    if (alreadyDerived)
      None
    else Some(
      CodeAction(
        title = s"Derive '$trt'",
        kind = CodeActionKind.Refactor,
        edit = Some(addDerivation(e, trt, uri)),
        command = None
      )
    )
  }

  /**
    * Returns a workspace edit that properly derives the given trait, `trt`, for the enum, `e`.
    *
    * For example, if we have:
    * {{{
    *   enum Abc {}
    * }}}
    * we could derive the trait 'Eq' like so:
    * {{{
    *   enum Abc with Eq { }
    * }}}
    *
    * Or if there are already other derivations present:
    * {{{
    *   enum Abc with ToString {}
    * }}}
    * it will be appended at the end:
    * {{{
    *   enum Abc with ToString, Eq { }
    * }}}
    */
  private def addDerivation(e: TypedAst.Enum, trt: String, uri: String): WorkspaceEdit = {
    val text =
      if (e.derives.traits.isEmpty)
        s" with $trt"
      else
        s", $trt"

    // Compute the end source location.
    // If there is already a derives clause we use its source location.
    // Otherwise, we use the source location of the enum symbol.
    val end = if (e.derives.loc != SourceLocation.Unknown) e.derives.loc else e.sym.loc

    WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position.fromEnd(end), Position.fromEnd(end)),
        text
      )))
    )
  }

  /**
    * Returns `true` if the given `range` starts on the same line as the given source location `loc`.
    */
  private def overlaps(range: Range, loc: SourceLocation): Boolean = {
    val range2 = sourceLocation2Range(loc)
    range.overlapsWith(range2)
  }

  private def sourcePosition2Position(sourcePosition: SourcePosition): Position = {
    Position(sourcePosition.line, sourcePosition.col)
  }

  private def sourceLocation2Range(sourceLocation: SourceLocation): Range = {
    Range(sourcePosition2Position(sourceLocation.sp1), sourcePosition2Position(sourceLocation.sp2))
  }
}
