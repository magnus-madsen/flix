/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.InstanceCompletion
import ca.uwaterloo.flix.language.ast.shared.{Scope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object InstanceCompleter {
  /**
    * Returns a List of Completion based on traits.
    */
  def getCompletions(context: CompletionContext)(implicit flix: Flix, root: TypedAst.Root): Iterable[InstanceCompletion] = {
    if (context.previousWord != "instance") {
      return Nil
    }

    /**
      * Replaces the given symbol with a variable named by the given `newText`.
      */
    def replaceText(oldSym: Symbol, tpe: Type, newText: String)(implicit flix: Flix): Type = {
      implicit val scope: Scope = Scope.Top
      tpe match {
        case Type.Var(sym, loc) if oldSym == sym => Type.Var(sym.withText(VarText.SourceText(newText)), loc)
        case Type.Var(_, _) => tpe
        case Type.Cst(_, _) => tpe

        case Type.Apply(tpe1, tpe2, loc) =>
          val t1 = replaceText(oldSym, tpe1, newText)
          val t2 = replaceText(oldSym, tpe2, newText)
          Type.Apply(t1, t2, loc)

        case Type.Alias(cst, args0, tpe0, loc) =>
          if (oldSym == cst.sym) {
            Type.freshVar(Kind.Star, loc, text = VarText.SourceText(newText))
          } else {
            val args = args0.map(replaceText(oldSym, _, newText))
            val t = replaceText(oldSym, tpe0, newText)
            Type.Alias(cst, args, t, loc)
          }

        case Type.AssocType(cst, args0, kind, loc) =>
          if (oldSym == cst.sym) {
            Type.freshVar(Kind.Star, loc, text = VarText.SourceText(newText))
          } else {
            val args = args0.map(replaceText(oldSym, _, newText))
            Type.AssocType(cst, args, kind, loc)
          }

        case Type.GetEff(action, t0, loc) =>
          val t = replaceText(oldSym, t0, newText)
          Type.GetEff(action, t, loc)

        // Jvm types should not be exposed to the user.
        case t: Type.JvmToType => t
        case t: Type.JvmToEff => t
        case t: Type.UnresolvedJvmType => t
      }
    }

    /**
      * Formats the given type `tpe`.
      */
    def fmtType(tpe: Type, holes: Map[Symbol, String])(implicit flix: Flix): String = {
      val replaced = holes.foldLeft(tpe) { case (t, (sym, hole)) => replaceText(sym, t, hole) }
      FormatType.formatType(replaced)
    }

    /**
      * Formats the given associated type `assoc`.
      */
    def fmtAssocType(assoc: TypedAst.AssocTypeSig, holes: Map[Symbol, String]): String = {
      s"    type ${assoc.sym.name} = ${holes(assoc.sym)}"
    }

    /**
      * Formats the given formal parameters in `spec`.
      */
    def fmtFormalParams(spec: TypedAst.Spec, holes: Map[Symbol, String])(implicit flix: Flix): String =
      spec.fparams.map(fparam => s"${fparam.bnd.sym.text}: ${fmtType(fparam.tpe, holes)}").mkString(", ")

    /**
      * Formats the given signature `sig`.
      */
    def fmtSignature(sig: TypedAst.Sig, holes: Map[Symbol, String])(implicit flix: Flix): String = {
      val fparams = fmtFormalParams(sig.spec, holes)
      val retTpe = fmtType(sig.spec.retTpe, holes)
      val eff = sig.spec.eff match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case e => raw" \ " + fmtType(e, holes)
      }
      s"    pub def ${sig.sym.name}($fparams): $retTpe$eff = ???"
    }

    root.traits.map {
      case (_, trt) =>
        val instanceHole = "${1:t}"
        val holes: Map[Symbol, String] = {
          (trt.tparam.sym -> instanceHole) +:
            trt.assocs.zipWithIndex.map { case (a, i) => a.sym -> s"$$${i + 2}" }
        }.toMap

        val traitSym = trt.sym
        val signatures = trt.sigs.filter(_.exp.isEmpty)

        val body = {
          trt.assocs.map(a => fmtAssocType(a, holes)) ++
            signatures.map(s => fmtSignature(s, holes))
        }.mkString("\n\n")

        val completion = s"$traitSym[$instanceHole] {\n\n$body\n\n}\n"

        InstanceCompletion(trt, completion)
    }.toList
  }

  /**
    * Formats the given trait `trt`.
    */
  def fmtTrait(trt: TypedAst.Trait): String = {
    s"trait ${trt.sym.name}[${trt.tparam.name.name}]"
  }
}
