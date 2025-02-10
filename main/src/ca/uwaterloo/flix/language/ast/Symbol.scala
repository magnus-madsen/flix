/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Name.{Ident, NName}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Scope, Source, VarText}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.util.Objects
import scala.collection.immutable.SortedSet

sealed trait Symbol

object Symbol {

  /**
    * The primitive effects defined in the Prelude.
    */
  val Chan: EffectSym = mkEffectSym(Name.RootNS, Ident("Chan", SourceLocation.Unknown))
  val Env: EffectSym = mkEffectSym(Name.RootNS, Ident("Env", SourceLocation.Unknown))
  val Exec: EffectSym = mkEffectSym(Name.RootNS, Ident("Exec", SourceLocation.Unknown))
  val FsRead: EffectSym = mkEffectSym(Name.RootNS, Ident("FsRead", SourceLocation.Unknown))
  val FsWrite: EffectSym = mkEffectSym(Name.RootNS, Ident("FsWrite", SourceLocation.Unknown))
  val IO: EffectSym = mkEffectSym(Name.RootNS, Ident("IO", SourceLocation.Unknown))
  val Net: EffectSym = mkEffectSym(Name.RootNS, Ident("Net", SourceLocation.Unknown))
  val NonDet: EffectSym = mkEffectSym(Name.RootNS, Ident("NonDet", SourceLocation.Unknown))
  val Sys: EffectSym = mkEffectSym(Name.RootNS, Ident("Sys", SourceLocation.Unknown))

  /**
    * The set of all primitive effects defined in the Prelude.
    */
  val PrimitiveEffs: SortedSet[EffectSym] = SortedSet.from(List(
    Chan, Env, Exec, FsRead, FsWrite, IO, Net, NonDet, Sys
  ))

  /**
    * Returns `true` if the given effect symbol is a primitive effect.
    */
  def isPrimitiveEff(sym: EffectSym): Boolean = sym match {
    case Chan => true
    case Env => true
    case Exec => true
    case FsRead => true
    case FsWrite => true
    case IO => true
    case Net => true
    case NonDet => true
    case Sys => true
    case _ => false
  }

  /**
    * Parses the given String `s` into an effect symbol.
    *
    * The String must be a valid name of a primitive effect.
    */
  def parsePrimitiveEff(s: String): Symbol.EffectSym = s match {
    case "Chan" => Chan
    case "Env" => Env
    case "Exec" => Exec
    case "FsRead" => FsRead
    case "FsWrite" => FsWrite
    case "IO" => IO
    case "Net" => Net
    case "NonDet" => NonDet
    case "Sys" => Sys
    case _ => throw InternalCompilerException(s"Unknown primitive effect: '$s'.", SourceLocation.Unknown)
  }

  /**
    * Returns a fresh def symbol based on the given symbol.
    */
  def freshDefnSym(sym: DefnSym)(implicit flix: Flix): DefnSym = {
    val id = Some(flix.genSym.freshId())
    new DefnSym(id, sym.namespace, sym.text, sym.loc)
  }

  /**
    * Returns a fresh hole symbol associated with the given source location `loc`.
    */
  def freshHoleSym(loc: SourceLocation)(implicit flix: Flix): HoleSym = {
    val id = flix.genSym.freshId()
    new HoleSym(Nil, "h" + id, loc)
  }

  /**
    * Returns a fresh variable symbol based on the given symbol.
    */
  def freshVarSym(sym: VarSym)(implicit flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), sym.text, sym.tvar, sym.boundBy, sym.loc)
  }

  /**
    * Returns a fresh variable symbol for the given identifier.
    */
  def freshVarSym(ident: Name.Ident, boundBy: BoundBy)(implicit scope: Scope, flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), ident.name, Type.freshVar(Kind.Star, ident.loc), boundBy, ident.loc)
  }

  /**
    * Returns a fresh variable symbol with the given text.
    */
  def freshVarSym(text: String, boundBy: BoundBy, loc: SourceLocation)(implicit scope: Scope, flix: Flix): VarSym = {
    new VarSym(flix.genSym.freshId(), text, Type.freshVar(Kind.Star, loc), boundBy, loc)
  }

  /**
    * Returns a fresh type variable symbol with the given text.
    */
  def freshKindedTypeVarSym(text: VarText, kind: Kind, isSlack: Boolean, loc: SourceLocation)(implicit scope: Scope, flix: Flix): KindedTypeVarSym = {
    new KindedTypeVarSym(flix.genSym.freshId(), text, kind, isSlack, scope, loc)
  }

  /**
    * Returns a fresh type variable symbol with the given text.
    */
  def freshUnkindedTypeVarSym(text: VarText, loc: SourceLocation)(implicit scope: Scope, flix: Flix): UnkindedTypeVarSym = {
    new UnkindedTypeVarSym(flix.genSym.freshId(), text, isSlack = false, scope, loc)
  }

  /**
    * Returns a label symbol with the given text.
    */
  def freshLabel(text: String)(implicit flix: Flix): LabelSym = {
    new LabelSym(flix.genSym.freshId(), text)
  }

  /**
    * Returns a fresh label symbol with the same text as the given label.
    */
  def freshLabel(sym: LabelSym)(implicit flix: Flix): LabelSym = {
    new LabelSym(flix.genSym.freshId(), sym.text)
  }

  /**
    * Returns a fresh region sym with the given text.
    */
  def freshRegionSym(ident: Ident, prop: RegionProperty)(implicit flix: Flix): RegionSym = {
    new RegionSym(flix.genSym.freshId(), ident.name, prop, ident.loc)
  }

  /**
    * Returns the definition symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkDefnSym(ns: NName, ident: Ident): DefnSym = {
    new DefnSym(None, ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the definition symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkDefnSym(ns: NName, ident: Ident, id: Option[Int]): DefnSym = {
    new DefnSym(id, ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the definition symbol for the given fully qualified name.
    */
  def mkDefnSym(fqn: String): DefnSym = split(fqn) match {
    case None => new DefnSym(None, Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new DefnSym(None, ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the definition symbol for the given fully qualified name and ID.
    */
  def mkDefnSym(fqn: String, id: Option[Int]): DefnSym = split(fqn) match {
    case None => new DefnSym(id, Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new DefnSym(id, ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the enum symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkEnumSym(ns: NName, ident: Ident): EnumSym = {
    new EnumSym(ns.parts, ident.name, ident.loc)
  }

  /**
   * Returns the struct symbol for the given name `ident` in the given namespace `ns`.
   */
  def mkStructSym(ns: NName, ident: Ident): StructSym = {
    new StructSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the restrictable enum symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkRestrictableEnumSym(ns: NName, ident: Ident, cases: List[Ident]): RestrictableEnumSym = {
    new RestrictableEnumSym(ns.parts, ident.name, cases, ident.loc)
  }

  /**
    * Returns the enum symbol for the given fully qualified name.
    */
  def mkEnumSym(fqn: String): EnumSym = split(fqn) match {
    case None => new EnumSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new EnumSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the case symbol for the given name `ident` in the given `enum`.
    */
  def mkCaseSym(sym: Symbol.EnumSym, ident: Ident): CaseSym = {
    new CaseSym(sym, ident.name, ident.loc)
  }

  /**
    * Returns the struct field symbol for the given name `name` which has position `idx` in the given struct `struct`
    */
  def mkStructFieldSym(struct: Symbol.StructSym, idx: Int, name: Name.Label): StructFieldSym = {
    new StructFieldSym(struct, name.name, idx, name.loc)
  }

  /**
    * Returns the restrictable case symbol for the given name `ident` in the given `enum`.
    */
  def mkRestrictableCaseSym(sym: Symbol.RestrictableEnumSym, ident: Ident): RestrictableCaseSym = {
    new RestrictableCaseSym(sym, ident.name, ident.loc)
  }

  /**
    * Returns the module symbol for the given fully qualified name.
    */
  def mkModuleSym(fqn: List[String]): ModuleSym = new ModuleSym(fqn)

  /**
    * Returns the trait symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkTraitSym(ns: NName, ident: Ident): TraitSym = {
    new TraitSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the trait symbol for the given fully qualified name
    */
  def mkTraitSym(fqn: String): TraitSym = split(fqn) match {
    case None => new TraitSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new TraitSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the hole symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkHoleSym(ns: NName, ident: Ident): HoleSym = {
    new HoleSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the hole symbol for the given fully qualified name.
    */
  def mkHoleSym(fqn: String): HoleSym = split(fqn) match {
    case None => new HoleSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new HoleSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the signature symbol for the given name `ident` in the trait associated with the given trait symbol `traitSym`.
    */
  def mkSigSym(traitSym: TraitSym, ident: Name.Ident): SigSym = {
    new SigSym(traitSym, ident.name, ident.loc)
  }

  /**
    * Returns the type alias symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkTypeAliasSym(ns: NName, ident: Ident): TypeAliasSym = {
    new TypeAliasSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the associated type symbol for the given name `ident` in the trait associated with the given trait symbol `traitSym`.
    */
  def mkAssocTypeSym(traitSym: TraitSym, ident: Name.Ident): AssocTypeSym = {
    new AssocTypeSym(traitSym, ident.name, ident.loc)
  }

  /**
    * Returns the type alias symbol for the given fully qualified name
    */
  def mkTypeAliasSym(fqn: String): TypeAliasSym = split(fqn) match {
    case None => new TypeAliasSym(Nil, fqn, SourceLocation.Unknown)
    case Some((ns, name)) => new TypeAliasSym(ns, name, SourceLocation.Unknown)
  }

  /**
    * Returns the effect symbol for the given name `ident` in the given namespace `ns`.
    */
  def mkEffectSym(ns: NName, ident: Ident): EffectSym = {
    new EffectSym(ns.parts, ident.name, ident.loc)
  }

  /**
    * Returns the operation symbol for the given name `ident` in the effect associated with the given effect symbol `effectSym`.
    */
  def mkOpSym(effectSym: EffectSym, ident: Name.Ident): OpSym = {
    new OpSym(effectSym, ident.name, ident.loc)
  }

  /**
    * Variable Symbol.
    *
    * @param id      the globally unique name of the symbol.
    * @param text    the original name, as it appears in the source code, of the symbol
    * @param tvar    the type variable associated with the symbol. This type variable always has kind `Star`.
    * @param boundBy the way the variable is bound.
    * @param loc     the source location associated with the symbol.
    */
  final class VarSym(val id: Int, val text: String, val tvar: Type.Var, val boundBy: BoundBy, val loc: SourceLocation) extends Ordered[VarSym] with Symbol {

    /**
      * The internal stack offset. Computed during variable numbering.
      */
    private var stackOffset: Option[Int] = None

    /**
      * Returns `true` if `this` symbol is a wildcard.
      */
    def isWild: Boolean = text.startsWith("_")

    /**
      * Returns the stack offset of `this` variable symbol.
      *
      * The local offset should be the number of jvm arguments for static
      * methods and one higher than that for instance methods.
      *
      * Throws [[InternalCompilerException]] if the stack offset has not been set.
      */
    def getStackOffset(localOffset: Int): Int = stackOffset match {
      case None => throw InternalCompilerException(s"Unknown offset for variable symbol $toString.", loc)
      case Some(offset) => offset + localOffset
    }

    /**
      * Sets the internal stack offset to given argument.
      */
    def setStackOffset(offset: Int): Unit = stackOffset match {
      case None => stackOffset = Some(offset)
      case Some(_) =>
        throw InternalCompilerException(s"Offset already set for variable symbol: '$toString'.", loc)
    }

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: VarSym => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = id

    /**
      * Return the comparison of `this` symbol to `that` symol.
      */
    override def compare(that: VarSym): Int = this.id.compare(that.id)

    /**
      * Human readable representation.
      */
    override def toString: String = text + Flix.Delimiter + id
  }

  /**
    * Kinded type variable symbol.
    */
  final class KindedTypeVarSym(val id: Int, val text: VarText, val kind: Kind, val isSlack: Boolean, val scope: Scope, val loc: SourceLocation) extends Symbol with Ordered[KindedTypeVarSym] with Locatable with Sourceable {

    /**
      * Returns `true` if `this` variable is non-synthetic.
      */
    def isReal: Boolean = loc.isReal

    /**
      * Returns the same symbol with the given kind.
      */
    def withKind(newKind: Kind): KindedTypeVarSym = new KindedTypeVarSym(id, text, newKind, isSlack, scope, loc)

    def withText(newText: VarText): KindedTypeVarSym = new KindedTypeVarSym(id, newText, kind, isSlack, scope, loc)

    override def compare(that: KindedTypeVarSym): Int = that.id - this.id

    override def equals(that: Any): Boolean = that match {
      case tvar: KindedTypeVarSym => this.id == tvar.id
      case _ => false
    }

    override val hashCode: Int = id

    /**
      * Returns a string representation of the symbol.
      */
    override def toString: String = {
      val string = text match {
        case VarText.Absent => "tvar"
        case VarText.SourceText(s) => s
      }
      string + Flix.Delimiter + id
    }

    /**
      * Returns true if this symbol is a wildcard.
      */
    def isWild: Boolean = text match {
      case VarText.Absent => false
      case VarText.SourceText(s) => s.startsWith("_")
    }
  }

  /**
    * Unkinded type variable symbol.
    */
  final class UnkindedTypeVarSym(val id: Int, val text: VarText, val isSlack: Boolean, val scope: Scope, val loc: SourceLocation) extends Symbol with Ordered[UnkindedTypeVarSym] with Locatable with Sourceable {

    /**
      * Ascribes this UnkindedTypeVarSym with the given kind.
      */
    def withKind(k: Kind): KindedTypeVarSym = new KindedTypeVarSym(id, text, k, isSlack, scope, loc)

    override def compare(that: UnkindedTypeVarSym): Int = that.id - this.id

    override def equals(that: Any): Boolean = that match {
      case tvar: UnkindedTypeVarSym => this.id == tvar.id
      case _ => false
    }

    override val hashCode: Int = id

    /**
      * Returns a string representation of the symbol.
      */
    override def toString: String = {
      val string = text match {
        case VarText.Absent => "tvar"
        case VarText.SourceText(s) => s
      }
      string + Flix.Delimiter + id
    }
  }

  /**
    * Definition Symbol.
    */
  final class DefnSym(val id: Option[Int], val namespace: List[String], val text: String, val loc: SourceLocation) extends Sourceable with Locatable with Symbol {

    /**
      * Returns the name of `this` symbol.
      */
    def name: String = id match {
      case None => text
      case Some(i) => text + Flix.Delimiter + i
    }

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: DefnSym => this.id == that.id && this.namespace == that.namespace && this.text == that.text
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 5 * id.hashCode() + 7 * namespace.hashCode() + 11 * text.hashCode()

    /**
      * Human readable representation.
      */
    override val toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name
  }

  /**
    * Enum Symbol.
    */
  final class EnumSym(val namespace: List[String], val text: String, val loc: SourceLocation) extends Sourceable with Symbol {

    /**
      * Returns the name of `this` symbol.
      */
    def name: String = text

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: EnumSym => this.namespace == that.namespace && this.text == that.text
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 5 * namespace.hashCode() + 7 * text.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name

    /**
      * Returns the source of `this` symbol.
      */
    override def src: Source = loc.source
  }

  /**
   * Struct Symbol.
   */
  final class StructSym(val namespace: List[String], val text: String, val loc: SourceLocation) extends Sourceable with Symbol {
    /**
      * Returns the name of `this` symbol.
      */
    def name: String = text

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: StructSym => this.namespace == that.namespace && this.text == that.text
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 5 * namespace.hashCode() + 7 * text.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name

    /**
      * Returns the source of `this` symbol.
      */
    override def src: Source = loc.source
  }

  /**
    * Restrictable Enum Symbol.
    */
  final class RestrictableEnumSym(val namespace: List[String], val name: String, cases: List[Name.Ident], val loc: SourceLocation) extends Symbol {

    // NB: it is critical that this be either a lazy val or a def, since otherwise `this` is not fully instantiated

    /**
      * The universe of cases associated with this restrictable enum.
      */
    def universe: SortedSet[Symbol.RestrictableCaseSym] = cases.map(Symbol.mkRestrictableCaseSym(this, _)).to(SortedSet)

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: RestrictableEnumSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name
  }

  /**
    * Case Symbol.
    */
  final class CaseSym(val enumSym: Symbol.EnumSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: CaseSym => this.enumSym == that.enumSym && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(enumSym, name)

    /**
      * Human readable representation.
      */
    override def toString: String = enumSym.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = enumSym.namespace :+ enumSym.name
  }

  /**
   * Struct Field Symbol.
   */
  final class StructFieldSym(val structSym: Symbol.StructSym, val name: String, val idx: Int, val loc: SourceLocation) extends Symbol {

    /**
     * Returns `true` if this symbol is equal to `that` symbol.
     */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: StructFieldSym => this.structSym == that.structSym && this.name == that.name && this.idx == that.idx
      case _ => false
    }

    /**
     * Returns the hash code of this symbol.
     */
    override val hashCode: Int = Objects.hash(structSym, name, idx)

    /**
     * Human readable representation.
     */
    override def toString: String = structSym.toString + "." + name

    /**
     * The symbol's namespace
     */
    def namespace: List[String] = structSym.namespace :+ structSym.name
  }

  /**
    * Restrictable Case Symbol.
    */
  final class RestrictableCaseSym(val enumSym: Symbol.RestrictableEnumSym, val name: String, val loc: SourceLocation) extends Symbol with Ordered[RestrictableCaseSym] {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: RestrictableCaseSym => this.enumSym == that.enumSym && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(enumSym, name)

    /**
      * Human readable representation.
      */
    override def toString: String = enumSym.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = enumSym.namespace :+ enumSym.name

    /**
      * Comparison.
      */
    override def compare(that: RestrictableCaseSym): Int = this.toString.compare(that.toString)

  }

  /**
    * Trait Symbol.
    */
  final class TraitSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Sourceable with Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: TraitSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name

    /**
      * Returns the source of `this`.
      */
    override def src: Source = loc.source
  }

  /**
    * Signature Symbol.
    */
  final class SigSym(val trt: Symbol.TraitSym, val name: String, val loc: SourceLocation) extends Sourceable with Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: SigSym => this.trt == that.trt && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * trt.hashCode + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = trt.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = trt.namespace :+ trt.name

    /**
      * Returns the source of `this`.
      */
    override def src: Source = loc.source
  }

  /**
    * Label Symbol.
    */
  final class LabelSym(val id: Int, val text: String) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: LabelSym => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * id

    /**
      * Human readable representation.
      */
    override def toString: String = text + Flix.Delimiter + id
  }

  /**
    * Hole Symbol.
    */
  final class HoleSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: HoleSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode()

    /**
      * Human readable representation.
      */
    override def toString: String = "?" + (if (namespace.isEmpty) name else namespace.mkString(".") + "." + name)
  }

  /**
    * TypeAlias Symbol.
    */
  final class TypeAliasSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Sourceable with Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: TypeAliasSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = 7 * namespace.hashCode() + 11 * name.hashCode

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name

    /**
      * Returns the source of `this`.
      */
    override def src: Source = loc.source
  }

  /**
    * Associated Type Symbol.
    */
  final class AssocTypeSym(val trt: Symbol.TraitSym, val name: String, val loc: SourceLocation) extends Symbol with Ordered[AssocTypeSym] {

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = trt.namespace :+ trt.name

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: AssocTypeSym => this.trt == that.trt && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(trt, name)

    /**
      * Compares `this` and `that` assoc type sym.
      */
    override def compare(that: AssocTypeSym): Int = {
      val s1 = this.namespace.mkString(".") + "." + this.name
      val s2 = that.namespace.mkString(".") + "." + that.name
      s1.compare(s2)
    }

    /**
      * Human readable representation.
      */
    override def toString: String = trt.toString + "." + name

  }

  /**
    * Effect symbol.
    */
  final class EffectSym(val namespace: List[String], val name: String, val loc: SourceLocation) extends Sourceable with Ordered[EffectSym] with Symbol {

    /**
      * Returns the source of `this`.
      */
    override def src: Source = loc.source

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: EffectSym => this.namespace == that.namespace && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(namespace, name)

    /**
      * Compares `this` and `that` effect sym.
      */
    override def compare(that: EffectSym): Int = {
      val s1 = this.namespace.mkString(".") + "." + this.name
      val s2 = that.namespace.mkString(".") + "." + that.name
      s1.compare(s2)
    }

    /**
      * Human readable representation.
      */
    override def toString: String = if (namespace.isEmpty) name else namespace.mkString(".") + "." + name
  }

  /**
    * Effect Operation Symbol.
    */
  final class OpSym(val eff: Symbol.EffectSym, val name: String, val loc: SourceLocation) extends Symbol {
    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: OpSym => this.eff == that.eff && this.name == that.name
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(eff, name)

    /**
      * Human readable representation.
      */
    override def toString: String = eff.toString + "." + name

    /**
      * The symbol's namespace.
      */
    def namespace: List[String] = eff.namespace :+ eff.name
  }

  /**
    * Region symbol.
    */
  final class RegionSym(val id: Int, val text: String, val prop: RegionProperty, val loc: SourceLocation) extends Symbol with Ordered[RegionSym] {

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: Any): Boolean = obj match {
      case that: RegionSym => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(id)

    /**
      * Human-readable representation.
      */
    override def toString: String = text + id

    /**
      * Compares `this` and `that` region sym.
      */
    override def compare(that: RegionSym): Int = this.id.compare(that.id)
  }

  /**
    * Module symbol.
    */
  final class ModuleSym(val ns: List[String]) extends Symbol {
    /**
      * Returns `true` if this is the root module.
      */
    def isRoot: Boolean = ns.isEmpty

    /**
      * Returns `true` if this symbol is equal to `that` symbol.
      */
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: ModuleSym => this.ns == that.ns
      case _ => false
    }

    /**
      * Returns the hash code of this symbol.
      */
    override val hashCode: Int = Objects.hash(ns)

    /**
      * Human readable representation.
      */
    override def toString: String = ns.mkString(".")
  }

  /**
    * Optionally returns the namespace part and name of the given fully qualified string `fqn`.
    *
    * Returns `None` if the `fqn` is not qualified.
    */
  private def split(fqn: String): Option[(List[String], String)] = {
    val split = fqn.split('.')
    if (split.length < 2)
      return None
    val namespace = split.init.toList
    val name = split.last
    Some((namespace, name))
  }

}
