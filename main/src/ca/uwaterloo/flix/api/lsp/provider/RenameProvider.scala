/*
 * Copyright 2020 Magnus Madsen
 * Copyright 2024 Alexander Dybdahl Troelsen
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

import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.{Consumer, Position, Range, ResponseStatus, TextEdit, Visitor, WorkspaceEdit}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{AssocTypeConstructor, EqualityConstraint, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object RenameProvider {

  /**
    * Handles a Rename LSP request by constructing a corresponding LSP response for when the cursor is
    * at [[Position]] `pos` in the file given by `uri`.
    *
    * Note that the LSP request should provide both `pos` and `uri`. Additionally, `pos` is interpreted
    * as the [[Position]] to the immediate right of the thin cursor.
    *
    * The Rename LSP response takes one of two forms: a "success" response or an "invalid request" response.
    *
    * If there is a [[Symbol]] under the cursor for which Flix supports renaming, then we get a "success" LSP response
    * of the form
    *
    * `{'status': "success", 'result': <workspace edit>}`
    *
    * where <workspace edit> is a [[WorkspaceEdit]] containing a [[TextEdit]] for each occurrence of the
    * [[Symbol]] we're renaming to, each of which change the occurrence to `newName`.
    *
    * If there is no [[Symbol]] under the cursor or Flix doesn't support renaming for it, then we get an "invalid request"
    * LSP response of the form
    *
    * `{'status': "invalid_request", 'result': "Nothing found in <uri> at <pos>."}`.
    *
    * Since a thin cursor exists between character positions, it's associated with both
    * the [[Position]] to its immediate left and right. This means we need to consider what's under
    * both. If there are two different [[Symbol]]s under the left and right [[Position]],
    * we rename occurrences of the one on the right.
    *
    * @param newName  The new name for the [[Symbol]] we're renaming.
    * @param uri      The URI of the file where the cursor is, provided by the LSP request.
    * @param pos      The [[Position]] of the cursor within the file given by `uri`, provided by the LSP request.
    * @param root     The root AST node of the Flix project.
    * @return         A [[JObject]] representing a Rename LSP response.
    */
  def processRename(newName: String, uri: String, pos: Position)(implicit root: Root): JObject = {
    val left = searchLeftOfCursor(uri, pos).flatMap(getOccurs)
    val right = searchRightOfCursor(uri, pos).flatMap(getOccurs)

    right.orElse(left)
      .map(rename(newName))
      .getOrElse(mkNotFound(uri, pos))
  }

  /**
    * Returns the most precise AST node under the space immediately left of the thin cursor.
    *
    * The given [[Position]] `pos` is interpreted as the space to the immediate right of the thin cursor.
    *
    * Note that this search filters out AST nodes synthetic [[SourceLocation]]s
    *
    * @param uri  The URI of the path of the file where the cursor is.
    * @param pos  The space to the immediate right of the cursor.
    * @param root The root AST node of the Flix project.
    * @return     Returns the most precise AST node under the space immediately left of the thin cursor.
    */
  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    if (pos.character >= 2) {
      search(uri, Position(pos.line, pos.character - 1))
    } else {
      None
    }
  }

  /**
    * Returns the most precise AST node under the space immediately right of the thin cursor.
    *
    * The given [[Position]] `pos` is interpreted as the space to the immediate left of the thin cursor.
    *
    * Note that this search filters out AST node of the Flix project.
    *
    * @param uri  The URI of the path of the file where the cursor is.
    * @param pos  The [[Position]] to the immediate right of the thin cursor.
    * @param root The root AST node of the Flix project.
    * @return     Returns the most precise AST node under the space immediately right of the thin cursor.
    */
  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = search(uri, pos)

  /**
    * Returns the most precise AST node under a given [[Position]] `pos`.
    *
    * @param uri  The URI of the path of the file where the cursor is.
    * @param pos  The [[Position]] that we are looking for the most precise AST under.
    * @param root The root AST node of the Flix project.
    * @return     The most precise AST node udner a given [[Position]] `pos`.
    */
  private def search(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    val consumer = StackConsumer()
    Visitor.visitRoot(root, consumer, InsideAcceptor(uri, pos))
    consumer.getStack.filter(isReal).headOption
  }

  private def isReal(x: AnyRef): Boolean = x match {
    case TypedAst.Trait(_, _, _, _, _, _, _, _, _, loc) =>  loc.isReal
    case TypedAst.Instance(_, _, _, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.Sig(_, _, _, loc) => loc.isReal
    case TypedAst.Def(_, _, _, loc) => loc.isReal
    case TypedAst.Enum(_, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.Struct(_, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.RestrictableEnum(_, _, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.TypeAlias(_, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.AssocTypeSig(_, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.AssocTypeDef(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.Effect(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.Op(_, _, loc) => loc.isReal
    case exp: TypedAst.Expr => exp.loc.isReal
    case pat: TypedAst.Pattern => pat.loc.isReal
    case TypedAst.RestrictableChoosePattern.Wild(_, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Var(_, _, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Tag(_, _, _, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Error(_, loc) => loc.isReal
    case p: TypedAst.Predicate => p.loc.isReal
    case TypedAst.Binder(sym, _) => sym.loc.isReal
    case TypedAst.Case(_, _, _, loc) => loc.isReal
    case TypedAst.StructField(_, _, loc) => loc.isReal
    case TypedAst.RestrictableCase(_, _, _, loc) => loc.isReal
    case TypedAst.Constraint(_, _, _, loc) => loc.isReal
    case TypedAst.ConstraintParam(_, _, loc) => loc.isReal
    case TypedAst.FormalParam(_, _, _, _, loc) => loc.isReal
    case TypedAst.PredicateParam(_, _, loc) => loc.isReal
    case TypedAst.JvmMethod(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.CatchRule(_, _, _) => true
    case TypedAst.HandlerRule(_, _, _) => true
    case TypedAst.TypeMatchRule(_, _, _) => true
    case TypedAst.SelectChannelRule(_, _, _) => true
    case TypedAst.TypeParam(_, _, loc) => loc.isReal
    case TypedAst.ParYieldFragment(_, _, loc) => loc.isReal

    case SymUse.AssocTypeSymUse(_, loc) => loc.isReal
    case SymUse.CaseSymUse(_, loc) => loc.isReal
    case SymUse.DefSymUse(_, loc) => loc.isReal
    case SymUse.EffectSymUse(_, qname) => qname.loc.isReal
    case SymUse.LocalDefSymUse(_, loc) => loc.isReal
    case SymUse.OpSymUse(_, qname) => qname.loc.isReal
    case SymUse.RestrictableCaseSymUse(_, loc) => loc.isReal
    case SymUse.RestrictableEnumSymUse(_, loc) => loc.isReal
    case SymUse.SigSymUse(_, loc) => loc.isReal
    case SymUse.StructFieldSymUse(_, loc) => loc.isReal
    case SymUse.TraitSymUse(_, loc) => loc.isReal

    case TraitConstraint(_, _, loc) => loc.isReal
    case TraitConstraint.Head(_, loc) => loc.isReal

    case AssocTypeConstructor(_, loc) => loc.isReal
    case EqualityConstraint(_, _, _, loc) => loc.isReal

    case _: Symbol => true
    case tpe: Type => tpe.loc.isReal
    case _ => false
  }

  /**
    * If `x` is a [[Symbol]] for which we support renaming, returns all occurrences of it. Otherwise, returns [[None]].
    *
    * @param x    The object that might be a [[Symbol]] for which we search for occurrences.
    * @param root The root AST node for the Flix project.
    * @return     All occurrences of the [[Symbol]] we want to rename, if it's supported. Otherwise, [[None]].
    */
  private def getOccurs(x: AnyRef)(implicit root: Root): Option[Set[SourceLocation]] = x match {
    // Type Vars
    case TypedAst.TypeParam(_, sym, _) => Some(getTypeVarSymOccurs(sym))
    case Type.Var(sym, _) => Some(getTypeVarSymOccurs(sym))
    // Vars
    case TypedAst.Expr.Var(sym, _, _) => Some(getVarOccurs(sym))
    case TypedAst.Binder(sym, _) => Some(getVarOccurs(sym))

    case _ => None
  }

  private def getTypeVarSymOccurs(sym: Symbol.KindedTypeVarSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.KindedTypeVarSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }
    object TypeVarSymConsumer extends Consumer {
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Var(sym, loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeVarSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getVarOccurs(sym: Symbol.VarSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty
    def consider(s: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object VarSymConsumer extends Consumer {
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.Var(s, _, loc) => consider(s, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  /**
    * Constructs the JSON response for renaming all `occurences` to `newName`.
    *
    * NB: The occurrences must *NOT* overlap nor be repeated. Hence they are a set.
    */
  private def rename(newName: String)(occurrences: Set[SourceLocation]): JObject = {
    // Convert the set of occurrences to a sorted list.
    val targets = occurrences.toList.sorted

    // Group by URI.
    val groupedByUri = targets.groupBy(_.source.name)

    // Construct text edits.
    val textEdits = groupedByUri map {
      case (uri, locs) => uri -> locs.map(loc => TextEdit(Range.from(loc), newName))
    }

    // Assemble the workspace edit.
    val workspaceEdit = WorkspaceEdit(textEdits)

    // Construct the JSON result.
    ("status" -> ResponseStatus.Success) ~ ("result" -> workspaceEdit.toJSON)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
