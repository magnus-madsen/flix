/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Format

object PrettyPrinter {

  private implicit val audience: Audience = Audience.External

  object Lifted {

    def fmtRoot(root: Root): String = {
      val sb = new StringBuilder()
      for ((sym, defn) <- root.defs.toList.sortBy(_._1.loc)) {
        sb.append(s"${Format.bold("def")} ${Format.blue(sym.toString)}(")
        for (fparam <- defn.fparams) {
          sb.append(s"${fmtParam(fparam)}, ")
        }
        sb.append(") = ")
        sb.append(fmtDef(defn).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
        sb.append(System.lineSeparator() + System.lineSeparator())
      }
      sb.toString()
    }

    def fmtDef(defn: Def): String = {
      fmtExp(defn.exp)
    }

    def fmtExp(exp0: Expression): String = {
      def visitExp(e0: Expression): String = e0 match {
        case Expression.Unit(_) => "Unit"

        case Expression.Null(tpe, _) => "null"

        case Expression.True(_) => "true"

        case Expression.False(_) => "false"

        case Expression.Char(lit, _) => "'" + lit.toString + "'"

        case Expression.Float32(lit, _) => lit.toString + "f32"

        case Expression.Float64(lit, _) => lit.toString + "f32"

        case Expression.Int8(lit, _) => lit.toString + "i8"

        case Expression.Int16(lit, _) => lit.toString + "i16"

        case Expression.Int32(lit, _) => lit.toString + "i32"

        case Expression.Int64(lit, _) => lit.toString + "i64"

        case Expression.BigInt(lit, _) => lit.toString() + "ii"

        case Expression.Str(lit, _) => "\"" + lit + "\""

        case Expression.Var(sym, tpe, loc) => fmtSym(sym)

        case Expression.Closure(sym, freeVars, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("Closure(")
            .append(fmtSym(sym))
            .append(", [")
          for (freeVar <- freeVars) {
            sb.append(fmtSym(freeVar.sym))
              .append(", ")
          }
          sb.append("])")
            .toString()

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append("(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym))
            .append("(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .append(")")
            .toString()

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(fmtSym(sym))
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.ApplySelfTail(name, formals, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("ApplySelfTail")
            .append("*(")
          for (arg <- args) {
            sb.append(visitExp(arg))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          fmtUnaryOp(op) + visitExp(exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp1))
            .append(" ")
            .append(fmtBinaryOp(op))
            .append(" ")
            .append(visitExp(exp2))
            .toString()

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.bold("if") + " (")
            .append(visitExp(exp1))
            .append(") {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp2).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("} ")
            .append(Format.bold("else") + " {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp3).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("}")
            .toString()

        case Expression.Branch(exp, branches, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("branch {")
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
          for ((sym, b) <- branches) {
            sb.append((" " * 4) + fmtSym(sym).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(":")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(b).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.JumpTo(sym, tpe, loc) => s"jumpto ${fmtSym(sym)}"

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(Format.bold("let "))
            .append(fmtSym(sym))
            .append(" = ")
            .append(visitExp(exp1).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(";")
            .append(System.lineSeparator())
            .append(visitExp(exp2))
            .toString()

        case Expression.Is(sym, tag, exp, loc) => visitExp(exp) + " is " + tag.name

        case Expression.Tag(sym, tag, exp, tpe, loc) => exp match {
          case Expression.Unit(_) => tag.name
          case _ =>
            val sb = new StringBuilder()
            sb.append(tag.name)
              .append("(")
              .append(visitExp(exp))
              .append(")")
              .toString()
        }

        case Expression.Untag(sym, tag, exp, tpe, loc) => "Untag(" + visitExp(exp) + ")"

        case Expression.Index(exp, offset, tpe, loc) =>
          visitExp(exp) +
            "[" +
            offset.toString +
            "]"

        case Expression.Tuple(elms, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("(")
          for (elm <- elms) {
            sb.append(visitExp(elm))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.RecordEmpty(tpe, loc) => "{}"

        case Expression.RecordSelect(exp, field, tpe, loc) =>
          visitExp(exp) +
            "." +
            field.name

        case Expression.RecordExtend(field, value, rest, tpe, loc) =>
          "{ " +
            field.name +
            " = " +
            visitExp(value) +
            " | " +
            visitExp(rest) +
            " }"

        case Expression.RecordRestrict(field, rest, tpe, loc) =>
          "{ -" +
            field.name +
            " | " +
            visitExp(rest) +
            "}"

        case Expression.ArrayLit(elms, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("[")
          for (elm <- elms) {
            sb.append(visitExp(elm))
            sb.append(",")
          }
          sb.append("]")
            .toString()

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          "[" +
            visitExp(elm) +
            ";" +
            len.toString +
            "]"

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(index) +
            "]"

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(index) +
            "]" +
            " = " +
            visitExp(elm)

        case Expression.ArrayLength(base, tpe, loc) =>
          "length" +
            "[" +
            visitExp(base) +
            "]"

        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
          visitExp(base) +
            "[" +
            visitExp(beginIndex) +
            ".." +
            visitExp(endIndex) +
            "]"

        case Expression.Ref(exp, tpe, loc) => "ref " + visitExp(exp)

        case Expression.Deref(exp, tpe, loc) => "deref " + visitExp(exp)

        case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) + " := " + visitExp(exp2)

        case Expression.Existential(fparam, exp, loc) =>
          "∃(" +
            fmtParam(fparam) +
            "). " +
            visitExp(exp)

        case Expression.Universal(fparam, exp, loc) =>
          "∀(" +
            fmtParam(fparam) +
            "). " +
            visitExp(exp)

        case Expression.Cast(exp, tpe, loc) =>
          visitExp(exp) +
            " as " +
            tpe.toString

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("try {")
            .append(System.lineSeparator())
            .append((" " * 2) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
            .append(System.lineSeparator())
            .append("} catch {")
            .append(System.lineSeparator())
          for (CatchRule(sym, clazz, body) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym))
              .append(s": ${clazz.toString} => ")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(body).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          sb.append("}")
            .append(System.lineSeparator())
            .toString()

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(constructor.toString)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(visitExp(exp))
            .append(".")
            .append(method.getDeclaringClass.getCanonicalName + "." + method.getName)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append(method.getDeclaringClass.getCanonicalName + "." + method.getName)
            .append("(")
          for (e <- args) {
            sb.append(visitExp(e))
              .append(", ")
          }
          sb.append(")")
            .toString()

        case Expression.GetField(field, exp, tpe, loc) =>
          "get field " +
            field.getName +
            " of " +
            visitExp(exp)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          "put field " +
            field.getName +
            " of " +
            visitExp(exp1) +
            " value " +
            visitExp(exp2)

        case Expression.GetStaticField(field, tpe, loc) => "get static field " + field.getName

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          "put static field " +
            field.getName +
            " value " +
            visitExp(exp)

        case Expression.NewChannel(exp, tpe, loc) => "Channel" + " " + visitExp(exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) + " <- " + visitExp(exp2)

        case Expression.GetChannel(exp, tpe, loc) => "<- " + visitExp(exp)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val sb = new StringBuilder()
          sb.append("select {")
            .append(System.lineSeparator())
          for (SelectChannelRule(sym, chan, exp) <- rules) {
            sb.append("  case ")
              .append(fmtSym(sym))
              .append(" <- ")
              .append(visitExp(chan).replace(System.lineSeparator(), System.lineSeparator() + (" " * 2)))
              .append(" => ")
              .append(System.lineSeparator())
              .append((" " * 4) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
              .append(System.lineSeparator())
          }
          default match {
            case Some(exp) =>
              sb.append("  case _ => ")
                .append(System.lineSeparator())
                .append((" " * 4) + visitExp(exp).replace(System.lineSeparator(), System.lineSeparator() + (" " * 4)))
                .append(System.lineSeparator())
            case None =>
          }
          sb.append("}")
            .toString()

        case Expression.Spawn(exp, tpe, loc) => "spawn " + visitExp(exp)

        case Expression.Lazy(exp, tpe, loc) => "lazy " + visitExp(exp)

        case Expression.Force(exp, tpe, loc) => "force " + visitExp(exp)

        case Expression.HoleError(sym, tpe, loc) => Format.red("HoleError")
        case Expression.MatchError(tpe, loc) => Format.red("MatchError")
      }

      visitExp(exp0)
    }

    def fmtParam(p: FormalParam): String = {
      fmtSym(p.sym) + ": " + FormatType.formatType(p.tpe)
    }

    def fmtSym(sym: Symbol.VarSym): String = {
      Format.cyan(sym.toString)
    }

    def fmtSym(sym: Symbol.DefnSym): String = {
      Format.blue(sym.toString)
    }

    def fmtSym(sym: Symbol.LabelSym): String = {
      Format.magenta(sym.toString)
    }

    def fmtUnaryOp(op: UnaryOperator): String = op match {
      case UnaryOperator.LogicalNot => "not"
      case UnaryOperator.Plus => "+"
      case UnaryOperator.Minus => "-"
      case UnaryOperator.BitwiseNegate => "~~~"
    }

    def fmtBinaryOp(op: BinaryOperator): String = op match {
      case BinaryOperator.Plus => "+"
      case BinaryOperator.Minus => "-"
      case BinaryOperator.Times => "*"
      case BinaryOperator.Divide => "/"
      case BinaryOperator.Modulo => "%"
      case BinaryOperator.Exponentiate => "**"
      case BinaryOperator.Less => "<"
      case BinaryOperator.LessEqual => "<="
      case BinaryOperator.Greater => ">"
      case BinaryOperator.GreaterEqual => ">="
      case BinaryOperator.Equal => "=="
      case BinaryOperator.NotEqual => "!="
      case BinaryOperator.Spaceship => "<=>"
      case BinaryOperator.LogicalAnd => "and"
      case BinaryOperator.LogicalOr => "or"
      case BinaryOperator.BitwiseAnd => "&&&"
      case BinaryOperator.BitwiseOr => "|||"
      case BinaryOperator.BitwiseXor => "^^^"
      case BinaryOperator.BitwiseLeftShift => "<<<"
      case BinaryOperator.BitwiseRightShift => ">>>"
    }
  }
}
