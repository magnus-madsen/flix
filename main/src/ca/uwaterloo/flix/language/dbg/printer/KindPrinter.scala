package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Kind
import ca.uwaterloo.flix.language.dbg.DocAst.Type

object KindPrinter {

  /** Returns the [[Type]] representation of `kind`. */
  def print(kind: Kind): Type = kind match {
    case Kind.Wild => Type.AsIs("???")
    case Kind.WildCaseSet => Type.AsIs("???")
    case Kind.Star => Type.AsIs("Type")
    case Kind.Eff => Type.AsIs("Eff")
    case Kind.Region => Type.AsIs("Region")
    case Kind.Bool => Type.AsIs("Bool")
    case Kind.RecordRow => Type.AsIs("RecordRow")
    case Kind.SchemaRow => Type.AsIs("SchemaRow")
    case Kind.Predicate => Type.AsIs("Predicate")
    case Kind.Jvm => Type.AsIs("Jvm")
    case Kind.CaseSet(sym) => Type.AsIs(s"CaseSet[${sym.name}]")
    case Kind.Arrow(k1, k2) => Type.Arrow(List(print(k1)), print(k2))
    case Kind.Error => Type.AsIs("Error")
  }

}
