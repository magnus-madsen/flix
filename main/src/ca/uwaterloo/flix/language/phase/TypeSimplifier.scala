package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.CofiniteEffSet

import scala.collection.mutable


/**
  * This type simplification is expensive and generally only meant for user display of types and
  * effects.
  */
object TypeSimplifier {

  /**
    * Simplifies types and especially effects to a user friendly format.
    *
    * This function will never crash, but non-well-kinded types are not simplified.
    */
  def simplify(tpe: Type): Type = {
    if (tpe.kind == Kind.Eff) {
      return toFormula(tpe)(new Counter(0)).toType(tpe.loc)
    }
    val (base0, args0) = tpe.fullApply
    base0 match {
      case tvar@Type.Var(_, _) =>
        val base = tvar
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case cst@Type.Cst(_, _) =>
        val base = cst
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.Alias(cst, aliasArgs, tpe, loc) =>
        val base = Type.Alias(cst, aliasArgs.map(simplify), simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.AssocType(cst, arg, kind, loc) =>
        val base = Type.AssocType(cst, simplify(arg), kind, loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.JvmToType(tpe, loc) =>
        val base = Type.JvmToType(simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.JvmToEff(tpe, loc) =>
        val base = Type.JvmToEff(simplify(tpe), loc)
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case jvmType@Type.UnresolvedJvmType(_, _) =>
        val base = jvmType
        val args = args0.map(simplify)
        Type.mkApply(base, args, tpe.loc)
      case Type.Apply(_, _, _) =>
        // `tpe.fullApply` never returns `Type.Apply` as the base.
        // Leave the input as is to avoid potentially throwing exceptions.
        tpe
    }
  }

  /** Convert well-formed formulas into [[Formula]], leaving nonsense as [[Chunk]]. */
  private def toFormula(tpe: Type)(implicit c: Counter): Union = {
    val (base0, args0) = tpe.fullApply
    (base0, args0) match {
      case (Type.Var(sym, _), List()) => Union(List(Intersection(List(Var(sym, isCompl = false)))))
      case (Type.Cst(TypeConstructor.Pure, _), Nil) => Empty
      case (Type.Cst(TypeConstructor.Univ, _), Nil) => Univ
      case (Type.Cst(TypeConstructor.Effect(sym), _), Nil) => Union(List(Intersection(List(Eff(CofiniteEffSet.mkSet(sym))))))
      case (Type.Cst(TypeConstructor.Complement, _), List(x)) => compl(toFormula(x))
      case (Type.Cst(TypeConstructor.Union, _), List(x, y)) => union(List(toFormula(x), toFormula(y)))
      case (Type.Cst(TypeConstructor.Intersection, _), List(x, y)) => intersection(List(toFormula(x), toFormula(y)))
      case (Type.Cst(TypeConstructor.Difference, _), List(x, y)) => difference(toFormula(x), toFormula(y))
      case (Type.Cst(TypeConstructor.SymmetricDiff, _), List(x, y)) => symmetricDifference(toFormula(x), toFormula(y))
      case (v@Type.Var(_, _), _) =>
        val base = v
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (cst@Type.Cst(_, _), _) =>
        val base = cst
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (Type.Alias(cst, aliasArgs, tpe, loc), _) =>
        val base = Type.Alias(cst, aliasArgs.map(simplify), simplify(tpe), loc)
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (Type.AssocType(cst, arg, kind, loc), _) =>
        val base = Type.AssocType(cst, simplify(arg), kind, loc)
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (Type.JvmToType(tpe, loc), _) =>
        val base = Type.JvmToType(simplify(tpe), loc)
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (Type.JvmToEff(tpe, loc), _) =>
        val base = Type.JvmToEff(simplify(tpe), loc)
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (jvmType@Type.UnresolvedJvmType(_, _), _) =>
        val base = jvmType
        val args = args0.map(simplify)
        mkChunk(base, args, tpe.loc)
      case (app@Type.Apply(_, _, _), _) =>
        // `tpe.fullApply` never returns `Type.Apply` as the base.
        // Leave the input as is to avoid potentially throwing exceptions.
        mkChunk(app, args0.map(simplify), tpe.loc)
    }
  }

  private def mkChunk(base: Type, args: List[Type], loc: SourceLocation)(implicit c: Counter): Union = {
    Union(List(Intersection(List(Chunk(Type.mkApply(base, args, loc), c.next(), isCompl = false)))))
  }

  private sealed trait Formula {
    def toType(loc: SourceLocation): Type = this match {
      case Var(sym, true) => Type.mkComplement(Type.Var(sym, loc), loc)
      case Var(sym, false) => Type.Var(sym, loc)
      case Eff(s) => TypeSimplifier.toType(s, loc)
      case Chunk(tpe, _, true) => Type.mkComplement(tpe, loc)
      case Chunk(tpe, _, false) => tpe
      case Union(fs) => Type.mkUnion(fs.map(_.toType(loc)), loc)
      case i@Intersection(_) =>
        val (cst, us) = i.split()
        val (neg0, pos0) = us.partition(_.isCompl)
        val (neg, pos) = cst match {
          case CofiniteEffSet.empty => (Nil, List(Eff(cst)))
          case CofiniteEffSet.universe => (neg0, pos0)
          case CofiniteEffSet.Set(_) => (neg0, Eff(cst) :: pos0)
          case CofiniteEffSet.Compl(_) => (Eff(cst) :: neg0, pos0)
        }
        val negUnion = Type.mkUnion(neg.map(compl).map(_.toType(loc)), loc)
        Type.mkDifference(Type.mkIntersection(pos.map(_.toType(loc)), loc), negUnion, loc)
      case _ => ??? // unreachable
    }
  }

  class Counter(var i: Int) {
    def next(): Int = {
      i += 1
      i
    }
  }

  private sealed trait Atom extends Formula

  private sealed trait Unknown extends Atom {
    def isCompl: Boolean
  }

  private case class Var(sym: Symbol.KindedTypeVarSym, isCompl: Boolean) extends Unknown {
    override def toString: String = sym.toString
  }

  private case class Eff(s: CofiniteEffSet) extends Atom {
    override def toString: String = s match {
      case CofiniteEffSet.empty => "Empty"
      case CofiniteEffSet.universe => "Univ"
      case CofiniteEffSet.Set(s) => s.mkString("+")
      case CofiniteEffSet.Compl(s) if s.sizeIs == 1 => s.mkString("!", "+", "")
      case CofiniteEffSet.Compl(s) => s.mkString("!(", "+", ")")
    }
  }

  private class Chunk(private val tpe: Type, val id: Int, val isCompl: Boolean) extends Unknown {
    /** No unknown is equal to another. */
    override def equals(obj: Any): Boolean = obj match {
      case Chunk(_, id, isCompl) => id == this.id && isCompl == this.isCompl
      case _ => false
    }

    override def toString: String = {
      if (isCompl) s"!chunk${id}"
      else s"chunk${id}"
    }
  }

  private object Chunk {
    def apply(tpe: Type, id: Int, isCompl: Boolean): Chunk = new Chunk(tpe, id, isCompl)

    def unapply(c: Chunk): Option[(Type, Int, Boolean)] = Some((c.tpe, c.id, c.isCompl))
  }

  /** An empty union is equivalent to the empty set. */
  private case class Union(fs: List[Intersection]) extends Formula {
    assert(fs.nonEmpty)

    override def toString: String = {
      if (fs.isEmpty) "Univ"
      else fs.mkString(" ∪ ")
    }
  }

  /** An empty intersection is equivalent to the universe set. */
  private case class Intersection(fs: List[Atom]) extends Formula {
    assert(fs.nonEmpty)

    def split(): (CofiniteEffSet, List[Unknown]) = {
      var ces = CofiniteEffSet.universe
      val us = mutable.Set[Unknown]()
      fs.foreach {
        case Var(sym, isCompl) if us.contains(Var(sym, !isCompl)) => return (CofiniteEffSet.empty, Nil)
        case v@Var(_, _) => us.add(v)
        case Chunk(tpe, id, isCompl) if us.contains(Chunk(tpe, id, !isCompl)) => return (CofiniteEffSet.empty, Nil)
        case c@Chunk(_, _, _) => us.add(c)
        case Eff(s) =>
          ces = CofiniteEffSet.intersection(ces, s)
          if (ces.isEmpty) return (ces, Nil)
        case _ => ??? // unreachable
      }
      (ces, us.toList)
    }

    override def toString: String = {
      if (fs.isEmpty) "Empty"
      else fs.mkString(" ∩ ")
    }
  }

  private val Univ: Union = Union(List(Intersection(List(Eff(CofiniteEffSet.universe)))))
  private val Empty: Union = Union(List(Intersection(List(Eff(CofiniteEffSet.empty)))))

  private def compl(f: Atom): Atom = f match {
    case Var(sym, isCompl) => Var(sym, !isCompl)
    case Eff(sym) => Eff(CofiniteEffSet.complement(sym))
    case Chunk(tpe, id, isCompl) => Chunk(tpe, id, !isCompl)
    case _ => ??? // unreachable
  }

  private def compl(f: Intersection): Union = f match {
    case Intersection(fs) => Union(fs.map(f => Intersection(List(compl(f)))))
  }

  private def compl(f: Union): Union = f match {
    case Union(fs) => intersection(fs.map(compl))
  }

  private def union(fs: List[Union]): Union = {
    unionIntersections(fs.iterator.flatMap(_.fs))
  }

  private def unionIntersections(fs: IterableOnce[Intersection]): Union = {
    val inters = fs.iterator.map(_.split()).toList.groupBy(_._2).map {
      case (uns, grouped) =>
        val cst = grouped.iterator.map(_._1).foldLeft(CofiniteEffSet.empty)(CofiniteEffSet.union)
        if (cst.isEmpty) Intersection(List(Eff(cst)))
        else if (cst.isUniverse && uns.nonEmpty) Intersection(uns)
        else Intersection(Eff(cst) :: uns)
    }.toList
    if (inters.contains(Intersection(List(Eff(CofiniteEffSet.universe)))))
      Univ
    else Union(inters.filter(i => i.fs.nonEmpty))
  }

  private def intersection(fs: List[Union]): Union = {
    fs.foldLeft(Univ) {
      case (acc, u) => union(acc.fs.map(f => intersection(f, u)))
    }
  }

  private def intersection(i: Intersection, u: Union): Union = {
    unionIntersections(u.fs.map(f => intersection(List(f, i))))
  }

  private def intersection(fs: List[Intersection]): Intersection = {
    val (cst, us) = Intersection(fs.iterator.flatMap(_.fs).toList).split()
    Intersection(Eff(cst) :: us)
  }

  private def difference(f1: Union, f2: Union): Union =
    intersection(List(f1, compl(f2)))

  private def symmetricDifference(f1: Union, f2: Union): Union = {
    val v1 = difference(f1, f2)
    val v2 = difference(f2, f1)
    union(List(v1, v2))
  }

  private def toType(s: CofiniteEffSet, loc: SourceLocation): Type = s match {
    case CofiniteEffSet.Set(s) =>
      Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc)
    case CofiniteEffSet.Compl(s) =>
      Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc), loc)
  }

}
