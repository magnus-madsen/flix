/*
 * Copyright 2020 Magnus Madsen,
 *           2024 Alexander Dybdahl Troelsen
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

import ca.uwaterloo.flix.api.lsp.acceptors.{FileAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.{Acceptor, Consumer, DocumentHighlight, DocumentHighlightKind, Position, Range, ResponseStatus, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Expr, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.shared.{AliasConstructor, AssocTypeConstructor, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

/**
  * Provides the means to handle an LSP highlight request.
  */
object HighlightProvider {

  /**
    * Handles an LSP highlight request by constructing an LSP highlight response for
    * when the cursor is at `pos` in the file at `uri`.
    *
    * Note that we assume a thin cursor so `pos` is interpreted as the character position
    * to the immediate right of the cursor
    *
    * If there is a [[Symbol]] or [[Name.Label]] under the cursor, every occurrence of
    * it in the file at `uri` is collected and a [[DocumentHighlight]] is created for each.
    * These are then put in a [[JObject]] representing an LSP highlight response. It takes the form
    *
    * `{'status': 'success', 'message': [...]}` where `[...]` is a [[JArray]] containing each [[DocumentHighlight]].
    *
    * If there is no [[Symbol]] or [[Name.Label]] at `pos`, a [[JObject]] representing an LSP failure response is returned. It takes the form
    *
    * `{'status': 'failure', 'message': "Nothing found in <uri> at <pos>`.
    *
    * Since a thin cursor exists between character positions, it's associated with both
    * the [[Position]] to its immediate left and right. This means we need to consider what's under
    * both. If there are two different [[Symbol]]/[[Name.Label]]s (any combination of the two)
    * under the left and right [[Position]], we highlight occurrences of the one on the right.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the thin cursor.
    *
    * @param uri  the URI of the file in question.
    * @param pos  the [[Position]] of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     A [[JObject]] representing an LSP highlight response. On success, contains [[DocumentHighlight]]
    *             for each occurrence of the symbol under the cursor.
    */
  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val highlightRight = searchRightOfCursor(uri, pos).flatMap(x => getOccurs(x, uri))
    val highlightLeft = searchLeftOfCursor(uri, pos).flatMap(x => getOccurs(x, uri))

    highlightRight
      .orElse(highlightLeft)
      .map(mkHighlights)
      .getOrElse(mkNotFound(uri, pos))
  }

  /**
    * Returns the most precise AST node under the position immediately left of the cursor,
    * if there is one. Otherwise, returns `None`.
    *
    * If there is no [[Position]] to the left of the cursor, returns `None`.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the cursor.
    *
    * As an example, consider
    * {{{
    *   2 + y|
    * }}}
    * where `|` is the cursor. Then `searchLeftOfCursor` would return the
    * most specific AST node `Var(y, ...)`
    *
    * @param uri  the URI of the file in which the cursor is.
    * @param pos  the [[Position]] immediately right of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST node under the [[Position]] immediately left
    *             of the cursor, if there is one. Otherwise, returns `None`.
    */
  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = pos match {
      case Position(line, character) if character >= 2 =>
        val leftOfCursor = Position(line, character - 1)
        search(uri, leftOfCursor)
      case _ => None
    }

  /**
    * Returns the most precise AST node under the [[Position]] immediately right of the cursor,
    * if there is one. Otherwise, returns `None`.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the cursor.
    *
    * As an example, consider
    * {{{
    *   println(|"very important text")
    * }}}
    * where `|` is the cursor. Then `searchRightOfCursor` would return the most specific
    * AST node `Cst(Str("very important text"))`.
    *
    * @param uri  the URI of the file in which the cursor is.
    * @param pos  the [[Position]] immediatately right of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST node under the [[Position]] immediately right
    *             of the cursor, if there is one. Otherwise, returns `None`.
    */
  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    search(uri, pos)
  }

  /**
    * Searches for the most precise AST node at the [[Position]] `pos`.
    *
    * Certain AST nodes are filtered out. We filter out [[TypedAst.Def]], [[TypedAst.Sig]],
    * [[TypedAst.Op]] and [[TypedAst.Enum]] nodes if the cursor is in them but _not_ in their [[Symbol]].
    * Additionally, we filter out [[TypedAst.Expr.RecordEmpty]] nodes, since they're uninteresting
    * for highlighting and their [[SourceLocation]] overshadows [[TypedAst.Expr.RecordExtend]]
    * and [[TypedAst.Expr.RecordRestrict]] nodes. Lastly we filter out AST nodes with synthetic
    * [[SourceLocation]]s as these, like the previous, are uninteresting and might shadow other nodes.
    *
    * We return the most precise AST node under the right [[Position]] (after filtering), unless there is none,
    * then we return the most precise AST node under the left [[Position]] (after filtering). If we still
    * find nothing, we return `None`.
    *
    * As an example, consider
    * {{{
    *   2 + y|
    * }}}
    * where `|` is the cursor. Then `search` would first search to the right and find nothing.
    * Then it would search to the left and find the most specific AST `Var(y)`.
    *
    * @param uri  the URI of the file the cursor is in.
    * @param pos  the [[Position]] immediately right of the thin cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST under the cursor if there is one. Otherwise, returns `None`.
    */
  private def search(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, InsideAcceptor(uri, pos))
    stackConsumer
      .getStack
      .filter(isNotEmptyRecord)
      .filter(ifDefThenInSym(uri, pos))
      .filter(ifSigThenInSym(uri, pos))
      .filter(ifOpThenInSym(uri, pos))
      .filter(ifTraitThenInSym(uri, pos))
      .filter(ifEnumThenInSym(uri, pos))
      .filter(isReal)
      .headOption
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

    case tpe: Type => tpe.loc.isReal
    case _ => false
  }

  private def isNotEmptyRecord(x: AnyRef): Boolean = x match {
    case TypedAst.Expr.RecordEmpty(_, _) => false
    case _ => true
  }

  private def ifEnumThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifDefThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Def(sym, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifSigThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Sig(sym, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifOpThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Op(sym, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifTraitThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private case class Occurs(writes: Set[SourceLocation], reads: Set[SourceLocation])

  /**
    * Returns all occurrences of [[AnyRef]] `x`, if it's either a [[Symbol]] or [[Name.Label]].
    * Otherwise, returns [[None]]:
    *
    * @param x    the object under the cursor.
    * @param uri  the URI of the file in question.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     [[Occurs]] containing all write and read occurrences of `x`, if it's a [[Symbol]] or [[Name.Label]].
    *             Otherwise, [[None]]
    */
  private def getOccurs(x: AnyRef, uri: String)(implicit root: Root): Option[Occurs] = {
    implicit val acceptor: Acceptor = FileAcceptor(uri)
    x match {
      // Assoc Types
      case TypedAst.AssocTypeSig(_, _, sym, _, _, _, _) => Some(getAssocTypeSymOccurs(sym))
      case SymUse.AssocTypeSymUse(sym, _) => Some(getAssocTypeSymOccurs(sym))
      case Type.AssocType(AssocTypeConstructor(sym, _), _, _, _) => Some(getAssocTypeSymOccurs(sym))
      // Defs
      case TypedAst.Def(sym, _, _, _) => Some(getDefnSymOccurs(sym))
      case SymUse.DefSymUse(sym, _) => Some(getDefnSymOccurs(sym))
      // Effects
      case TypedAst.Effect(_, _, _, sym, _, _) => Some(getEffectSymOccurs(sym))
      case Type.Cst(TypeConstructor.Effect(sym), _) => Some(getEffectSymOccurs(sym))
      case SymUse.EffectSymUse(sym, _) => Some(getEffectSymOccurs(sym))
      // Enums & Cases
      case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(getEnumSymOccurs(sym))
      case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(getEnumSymOccurs(sym))
      case TypedAst.Case(sym, _, _, _) => Some(getCaseSymOccurs(sym))
      case SymUse.CaseSymUse(sym, _) => Some(getCaseSymOccurs(sym))
      // Ops
      case TypedAst.Op(sym, _, _) => Some(getOpSymOccurs(sym))
      case SymUse.OpSymUse(sym, _) => Some(getOpSymOccurs(sym))
      // Records
      case TypedAst.Expr.RecordExtend(label, _, _, _, _, _) => Some(getLabelOccurs(label))
      case TypedAst.Expr.RecordRestrict(label, _, _, _, _) => Some(getLabelOccurs(label))
      case TypedAst.Expr.RecordSelect(_, label, _, _, _) => Some(getLabelOccurs(label))
      // Signatures
      case TypedAst.Sig(sym, _, _, _) => Some(getSigSymOccurs(sym))
      case SymUse.SigSymUse(sym, _) => Some(getSigSymOccurs(sym))
      // Structs
      case TypedAst.Struct(_, _, _, sym, _, _, _, _) => Some(getStructSymOccurs(sym))
      case Type.Cst(TypeConstructor.Struct(sym, _), _) => Some(getStructSymOccurs(sym))
      case TypedAst.StructField(sym, _, _) => Some(getStructFieldSymOccurs(sym))
      case SymUse.StructFieldSymUse(sym, _) => Some(getStructFieldSymOccurs(sym))
      // Traits
      case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => Some(getTraitSymOccurs(sym))
      case SymUse.TraitSymUse(sym, _) => Some(getTraitSymOccurs(sym))
      case TraitConstraint.Head(sym, _) => Some(getTraitSymOccurs(sym))
      // Type Aliases
      case TypedAst.TypeAlias(_, _, _, sym, _, _, _) => Some(getTypeAliasSymOccurs(sym))
      case Type.Alias(AliasConstructor(sym, _), _, _, _) => Some(getTypeAliasSymOccurs(sym))
      // Type Variables
      case TypedAst.TypeParam(_, sym, _) => Some(getTypeVarSymOccurs(sym))
      case Type.Var(sym, _) => Some(getTypeVarSymOccurs(sym))
      // Variables
      case Binder(sym, _) => Some(getVarSymOccurs(sym))
      case TypedAst.Expr.Var(varSym, _, _) => Some(getVarSymOccurs(varSym))

      case _ => None
    }
  }


  private def getAssocTypeSymOccurs(sym: Symbol.AssocTypeSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.AssocTypeSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }
    def considerRead(s: Symbol.AssocTypeSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object AssocTypeSymConsumer extends Consumer {
      override def consumeAssocTypeSig(tsig: TypedAst.AssocTypeSig): Unit = considerWrite(tsig.sym, tsig.sym.loc)
      override def consumeAssocTypeSymUse(symUse: SymUse.AssocTypeSymUse): Unit = considerRead(symUse.sym, symUse.loc)
      // TODO missing type case?
    }

    Visitor.visitRoot(root, AssocTypeSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getDefnSymOccurs(sym: Symbol.DefnSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = considerWrite(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getEffectSymOccurs(sym: Symbol.EffectSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }
    
    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = considerWrite(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = considerRead(effUse.sym, effUse.qname.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Effect(sym), loc) => considerRead(sym, loc)
        case _ => ()
      }
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Do(_, _, _, eff, loc) if eff.effects.contains(sym) => reads += loc
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getEnumSymOccurs(sym: Symbol.EnumSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = considerWrite(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getCaseSymOccurs(sym: Symbol.CaseSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object CaseSymConsumer extends Consumer {
      override def consumeCase(cse: TypedAst.Case): Unit = considerWrite(cse.sym, cse.sym.loc)
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getOpSymOccurs(sym: Symbol.OpSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.OpSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.OpSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object OpSymConsumer extends Consumer {
      override def consumeOp(op: TypedAst.Op): Unit = considerWrite(op.sym, op.sym.loc)
      override def consumeOpSymUse(sym: SymUse.OpSymUse): Unit = considerRead(sym.sym, sym.qname.loc)
    }

    Visitor.visitRoot(root, OpSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getLabelOccurs(label: Name.Label)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(l: Name.Label, loc: SourceLocation): Unit = {
      if (l == label) { writes += loc }
    }

    def considerRead(l: Name.Label, loc: SourceLocation): Unit = {
      if (l == label) { reads += loc }
    }

    object LabelConsumer extends Consumer {
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.RecordExtend(l, _, _, _, _, _) => considerWrite(l, l.loc)
        case Expr.RecordSelect(_, l, _, _, _) => considerRead(l, l.loc)
        case Expr.RecordRestrict(l, _, _, _, _) => considerWrite(l, l.loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, LabelConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getSigSymOccurs(sym: Symbol.SigSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = considerWrite(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getStructSymOccurs(sym: Symbol.StructSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.StructSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.StructSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object StructSymConsumer extends Consumer {
      override def consumeStruct(struct: TypedAst.Struct): Unit = considerWrite(struct.sym, struct.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.StructNew(sym, _, _, _, _, loc) => considerRead(sym, loc)
        case _ => ()
      }
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Struct(sym, _), loc) => considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getStructFieldSymOccurs(sym: Symbol.StructFieldSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.StructFieldSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.StructFieldSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = considerWrite(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getTraitSymOccurs(sym: Symbol.TraitSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = considerWrite(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = considerRead(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = considerRead(tcHead.sym, tcHead.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getTypeAliasSymOccurs(sym: Symbol.TypeAliasSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.TypeAliasSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.TypeAliasSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object TypeAliasSymConsumer extends Consumer {
      override def consumeTypeAlias(alias: TypedAst.TypeAlias): Unit = considerWrite(alias.sym, alias.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Alias(AliasConstructor(sym, _), _, _, loc) => considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeAliasSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getTypeVarSymOccurs(sym: Symbol.KindedTypeVarSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.KindedTypeVarSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.KindedTypeVarSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object TypeVarSymConsumer extends Consumer {
      override def consumeTypeParam(tparam: TypedAst.TypeParam): Unit = considerWrite(tparam.sym, tparam.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Var(sym, loc) => considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeVarSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  private def getVarSymOccurs(sym: Symbol.VarSym)(implicit root: Root, acceptor: Acceptor): Occurs = {
    var writes: Set[SourceLocation] = Set.empty
    var reads: Set[SourceLocation] = Set.empty

    def considerWrite(s: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (s == sym) { writes += loc }
    }

    def considerRead(s: Symbol.VarSym, loc: SourceLocation): Unit = {
      if (s == sym) { reads += loc }
    }

    object VarSymConsumer extends Consumer {
      override def consumeBinder(bnd: TypedAst.Binder): Unit = considerWrite(bnd.sym, bnd.sym.loc)
      override def consumeLocalDefSym(symUse: SymUse.LocalDefSymUse): Unit = considerRead(symUse.sym, symUse.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Var(sym, _, loc) => considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, acceptor)

    Occurs(writes, reads)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject = {
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")
  }

  /**
    * Creates an LSP highlight response from `occurs`.
    *
    * The resulting LSP highlight response contains a [[DocumentHighlight]] for each occurrence in `occurs`.
    * The [[DocumentHighlight]] created from `occurs.writes` have [[DocumentHighlightKind.Write]] and
    * those created from `occurs.reads` have [[DocumentHighlightKind.Read]]
    *
    * @param occurs The [[Occurs]] to be highlighted.
    * @return       LSP highlight response containing [[DocumentHighlight]] for every occurrence in `occurs`.
    */
  private def mkHighlights(occurs: Occurs): JObject = {
    val writeHighlights = occurs.writes.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write))
    val readHighlights = occurs.reads.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

    val highlights = writeHighlights ++ readHighlights

    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON).toList))
  }
}
