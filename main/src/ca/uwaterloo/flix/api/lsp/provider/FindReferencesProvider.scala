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

import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Root}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object FindReferencesProvider {

  /**
    * Handles a "Find References" LSP request by constructing a corresponding LSP response.
    *
    * The "Find References" LSP response is either a "success" or "failure" response.
    *
    * If there is a [[Symbol]] for which Flix supports "find references" for under the cursor given by the request
    * then the response is a "success" response which takes the form
    *
    * `{'status': 'success', 'result': [...]}`
    *
    * where `[...]` is a list of LSP [[Location]]s for each occurrence of the aforementioned [[Symbol]] in the
    * Flix project.
    *
    * If there is no such [[Symbol]] or Flix doesn't support "find references" for it then the request is invalid and the
    * response takes the form
    *
    * `{'status': 'invalid_request', 'result': "Nothing found in <uri> at <pos>"}`
    *
    * The location of the cursor given by `uri` and the [[Position]] `pos` which resp. give the path to the file where
    * the cursor is, and it's location within said file. Both should be provided by the LSP request.
    *
    * We assume a thin cursor, meaning there are two [[Position]] associated with it: the on the immediate left of
    * the cursor and the one on the immediate right. This means that we have to consider both when determining what
    * element is under the cursor and thus what we find the references to. If there is an occurrence of a symbol that
    * Flix supports "find references" for under one of these [[Position]]s but not the other, then we consider this
    * under the cursor, and we find the references for it. If there is such an occurrence under both of these
    * [[Position]]s, we prioritise the one under the right [[Position]].
    *
    * @param uri  The URI of the file where the cursor is, provided by the LSP request.
    * @param pos  The [[Position]] of the cursor within the file given by `uri`, provided by the LSP request.
    * @param root The root AST node of the Flix project.
    * @return     A "Find References" LSP response.
    */
  def findRefs(uri: String, pos: Position)(implicit root: Root): JObject = {
    val left = searchLeftOfCursor(uri, pos).flatMap(getOccurs)
    val right = searchRightOfCursor(uri, pos).flatMap(getOccurs)

    right.orElse(left)
      .map(_.filter(isInProject))
      .map(mkResponse)
      .getOrElse(mkNotFound(uri, pos))
  }

  /**
    * Returns the most specific AST node under the [[Position]] to the immediate left of the thin cursor,
    * if there is one. Returns [[None]] otherwise.
    *
    * If the cursor is all the way to the left then there is no [[Position]] immediately left of the cursor.
    * In this case, trivially cannot be any element under that position, so we return [[None]].
    *
    * Note that the given [[Position]] `pos` that represents the cursors position is interpreted as the
    * [[Position]] to the immediate right of the cursor.
    *
    * @param uri  The URI of the file where the thin cursor is.
    * @param pos  The [[Position]] to the immediate right of the thin cursor.
    * @param root The root AST node of the Flix project.
    * @return     The most specific AST node under the [[Position]] to the immediate left of the thin cursor,
    *             if there is one. Otherwise, [[None]].
    */
  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    if (pos.character >= 2) {
      val left = Position(pos.line, pos.character - 1)
      search(uri, left)
    } else {
      search(uri, pos)
    }
  }

  /**
    * Returns the most specific AST node under the [[Position]] to the immediate right of the thin cursor,
    * if there is one. Returns [[None]] otherwise.
    *
    * Note that the given [[Position]] `pos` that represents the cursor's position is interpreted as the
    * [[Position]] to the immediate right of the cursor.
    *
    * @param uri  The URI of the file where the thin cursor is.
    * @param pos  The [[Position]] to the immediate right of the thin cursor.
    * @param root The root AST node of the Flix Project.
    * @return     The most specific AST node under the [[Position]] to the immediate right of the thin cursor,
    *             if there is one. Otherwise, [[None]].
    */
  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = search(uri, pos)

  /**
    * Returns the most specific AST node under the [[Position]] `pos` in the file given by `uri`.
    * Returns [[None]] otherwise.
    *
    * Note that we filter out elements with synthetic [[SourceLocation]]s.
    *
    * @param uri  The URI of the file where we're searching.
    * @param pos  The [[Position]] where we're searching within the file given by `uri`.
    * @param root The root AST node of the Flix Project.
    * @return     The most specific AST node under the [[Position]] `pos` in the file given by `uri`,
    *             if there is one. Otherwise, [[None]].
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

    case AssocTypeConstructor(_, loc) => loc.isReal
    case EqualityConstraint(_, _, _, loc) => loc.isReal
    case TraitConstraint(_, _, loc) => loc.isReal
    case TraitConstraint.Head(_, loc) => loc.isReal

    case tpe: Type => tpe.loc.isReal
    case _ => false
  }

  /**
    * Returns all the [[SourceLocation]]s of the occurrences of `x` if `x` is an element that Flix supports
    * "find references" for. Otherwise, returns [[None]].
    *
    * @param x    The element that we're finding occurrences of (if it's supported)
    * @param root The root AST node of the Flix project.
    * @return     The [[SourceLocation]]s of the occurrences of `x` if Flix supports "find references" for it.
    *             Otherwise, [[None]].
    */
  private def getOccurs(x: AnyRef)(implicit root: Root): Option[Set[SourceLocation]] = x match {
    // Assoc Types
    case TypedAst.AssocTypeSig(_, _, sym, _, _, _, _) => Some(getAssocTypeSymOccurs(sym))
    case SymUse.AssocTypeSymUse(sym, _) => Some(getAssocTypeSymOccurs(sym))
    case AssocTypeConstructor(sym, _) => Some(getAssocTypeSymOccurs(sym))
    case Type.AssocType(AssocTypeConstructor(sym, _), _, _, _) => Some(getAssocTypeSymOccurs(sym))
    // Cases
    case TypedAst.Case(sym, _, _, _) => Some(getCaseSymOccurs(sym))
    case SymUse.CaseSymUse(sym, _) => Some(getCaseSymOccurs(sym))
    // Defs
    case TypedAst.Def(sym, _, _, _) => Some(getDefnSymOccurs(sym))
    case SymUse.DefSymUse(sym, _) => Some(getDefnSymOccurs(sym))
    // Effects
    case TypedAst.Effect(_, _, _, sym, _, _) => Some(getEffectSymOccurs(sym))
    case SymUse.EffectSymUse(sym, _) => Some(getEffectSymOccurs(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Some(getEffectSymOccurs(sym))
    // Enums
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(getEnumSymOccurs(sym))
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(getEnumSymOccurs(sym))
    // Ops
    case TypedAst.Op(sym, _, _) => Some(getOpSymOccurs(sym))
    case SymUse.OpSymUse(sym, _) => Some(getOpSymOccurs(sym))
    // Sigs
    case TypedAst.Sig(sym, _, _, _) => Some(getSigSymOccurs(sym) ++ getSigImplLocs(sym))
    case SymUse.SigSymUse(sym, _) => Some(getSigSymOccurs(sym))
    // Structs
    case TypedAst.Struct(_, _, _, sym, _, _, _, _) => Some(getStructSymOccurs(sym))
    case Type.Cst(TypeConstructor.Struct(sym, _), _) => Some(getStructSymOccurs(sym))
    // Struct Fields
    case TypedAst.StructField(sym, _, _) => Some(getStructFieldSymOccurs(sym))
    case SymUse.StructFieldSymUse(sym, _) => Some(getStructFieldSymOccurs(sym))
    // Traits
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => Some(getTraitSymOccurs(sym))
    case SymUse.TraitSymUse(sym, _) => Some(getTraitSymOccurs(sym))
    case TraitConstraint.Head(sym, _) => Some(getTraitSymOccurs(sym))
    // Type Alias
    case TypedAst.TypeAlias(_, _, _, sym, _, _, _) => Some(getTypeAliasSymOccurs(sym))
    case Type.Alias(AliasConstructor(sym, _), _, _, _) => Some(getTypeAliasSymOccurs(sym))
    // Type Vars
    case TypedAst.TypeParam(_, sym, _) => Some(getTypeVarSymOccurs(sym))
    case Type.Var(sym, _) => Some(getTypeVarSymOccurs(sym))
    // Vars
    case TypedAst.Expr.Var(sym, _, _) => Some(getVarSymOccurs(sym))
    case Binder(sym, _) => Some(getVarSymOccurs(sym))

    case _ => None
  }

  private def getAssocTypeSymOccurs(sym: Symbol.AssocTypeSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.AssocTypeSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object AssocTypeSymConsumer extends Consumer {
      override def consumeAssocTypeSymUse(symUse: SymUse.AssocTypeSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeAssocTypeConstructor(tcst: AssocTypeConstructor): Unit = consider(tcst.sym, tcst.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.AssocType(AssocTypeConstructor(sym, loc), _, _, _) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, AssocTypeSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getCaseSymOccurs(sym: Symbol.CaseSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object CaseSymConsumer extends Consumer {
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getDefnSymOccurs(sym: Symbol.DefnSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object DefnSymConsumer extends Consumer {
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getEffectSymOccurs(sym: Symbol.EffectSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object EffectSymConsumer extends Consumer {
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = consider(effUse.sym, effUse.qname.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Effect(sym), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getEnumSymOccurs(sym: Symbol.EnumSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object EnumSymConsumer extends Consumer {
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getOpSymOccurs(sym: Symbol.OpSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.OpSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object OpSymConsumer extends Consumer {
      override def consumeOpSymUse(sym: SymUse.OpSymUse): Unit = consider(sym.sym, sym.qname.loc)
    }

    Visitor.visitRoot(root, OpSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getSigSymOccurs(sym: Symbol.SigSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object SigSymConsumer extends Consumer {
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getSigImplLocs(sym: Symbol.SigSym)(implicit root: Root): Set[SourceLocation] = {
    root.instances(sym.trt)
      .flatMap(_.defs)
      .filter(_.sym.text == sym.name)
      .map(_.sym.loc)
      .toSet
  }

  private def getStructSymOccurs(sym: Symbol.StructSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.StructSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object StructSymConsumer extends Consumer {
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Struct(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getStructFieldSymOccurs(sym: Symbol.StructFieldSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.StructFieldSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getTraitSymOccurs(sym: Symbol.TraitSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object TraitSymConsumer extends Consumer {
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = consider(tcHead.sym, tcHead.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getTypeAliasSymOccurs(sym: Symbol.TypeAliasSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.TypeAliasSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object TypeAliasSymConsumer extends Consumer {
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Alias(AliasConstructor(sym, loc), _, _, _) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeAliasSymConsumer, AllAcceptor)

    occurs + sym.loc
  }

  private def getTypeVarSymOccurs(sym: Symbol.KindedTypeVarSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.KindedTypeVarSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object TypeVarSymConsumer extends Consumer {
      override def consumeTypeParam(tparam: TypedAst.TypeParam): Unit = consider(tparam.sym, tparam.sym.loc)

      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Var(sym, loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeVarSymConsumer, AllAcceptor)

    occurs
  }

  private def getVarSymOccurs(sym: Symbol.VarSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object VarSymConsumer extends Consumer {
      override def consumeBinder(bnd: Binder): Unit = consider(bnd.sym, bnd.sym.loc)
      override def consumeExpr(exp: TypedAst.Expr): Unit = exp match {
        case TypedAst.Expr.Var(sym, _, loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, AllAcceptor)

    occurs
  }

  private def isInProject(loc: SourceLocation): Boolean = loc.source.input match {
    case Input.Text(_, _, _) => true // over-approximation
    case Input.TxtFile(_, _) => false
    case Input.PkgFile(_, _) => false
    case Input.FileInPackage(_, _, _, _) => false
    case Input.Unknown => false
  }


  /**
    * Returns a successful "find references" LSP response containing the LSP [[Location]] for each
    * [[SourceLocation]] of the occurrences of the element that we're finding references to.
    *
    * @param refs The [[SourceLocation]]s for the occurrences of the element that we're finding references to.
    * @return     A successful "find references" LSP response.
    */
  private def mkResponse(refs: Set[SourceLocation]): JObject = {
    ("status" -> ResponseStatus.Success) ~ ("result" -> refs.map(Location.from).map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
