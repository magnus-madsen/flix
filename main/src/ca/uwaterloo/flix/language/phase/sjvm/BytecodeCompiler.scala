/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import scala.language.implicitConversions

object BytecodeCompiler {

  sealed trait Stack

  sealed trait StackNil extends Stack

  sealed trait StackCons[R <: Stack, T <: PType] extends Stack

  // TODO(JLS): not optimal right now. It can be continued on
  // TODO(JLS): IRETURN and RETURN can both reach StackEnd, but they shouldn't be equivalent states
  sealed trait StackEnd extends Stack

  type **[R <: Stack, T <: PType] = StackCons[R, T]

  // TODO(JLS): experiment with this
  object F {
    def push[R <: Stack, T <: PType](tag: Tag[T])(f: F[R]): F[R ** T] = f.push[T]

    def pop[R <: Stack, T <: PType](tag: Tag[T])(f: F[R ** T]): F[R] = f.asInstanceOf[F[R]]
  }

  // TODO(JLS): maybe the methods should take JvmNames? Describable?
  sealed case class F[R <: Stack](visitor: MethodVisitor) {
    def visitMethodInsn(opcode: Int, owner: InternalName, name: String, descriptor: Descriptor, isInterface: Boolean = false): Unit =
      visitor.visitMethodInsn(opcode, owner.toString, name, descriptor.toString, isInterface)

    def visitTypeInsn(opcode: Int, tpe: InternalName): Unit = visitor.visitTypeInsn(opcode, tpe.toString)

    def visitInsn(opcode: Int): Unit = visitor.visitInsn(opcode)

    def visitLabel(label: Label): Unit = visitor.visitLabel(label)

    def visitEnd(): Unit = visitor.visitEnd()

    def visitCode(): Unit = visitor.visitCode()

    def visitFieldInsn(opcode: Int, owner: InternalName, name: String, descriptor: Descriptor): Unit = visitor.visitFieldInsn(opcode, owner.toString, name, descriptor.toString)

    def visitMaxs(maxStack: Int = 999, maxLocals: Int = 999): Unit = visitor.visitMaxs(maxStack, maxLocals)

    def visitVarInsn(opcode: Int, index: Int): Unit = visitor.visitVarInsn(opcode, index)

    def visitJumpInsn(opcode: Int, label: Label): Unit = visitor.visitJumpInsn(opcode, label)

    def visitLineNumber(line: Int, start: Label): Unit = visitor.visitLineNumber(line, start)

    def visitLdcInsn(value: Any): Unit = visitor.visitLdcInsn(value)

    def visitIntInsn(opcode: Int, operand: Int): Unit = visitor.visitIntInsn(opcode, operand)

    def visitTryCatchBlock(start: Label, end: Label, handler: Label, tpe: Class[_]): Unit = visitor.visitTryCatchBlock(start, end, handler, asm.Type.getInternalName(tpe))

    def push[T <: PType]: F[R ** T] = this.asInstanceOf[F[R ** T]]
  }

  trait Cat1[T]

  implicit val int8Cat1: PInt8 => Cat1[PInt8] = null
  implicit val int16Cat1: PInt16 => Cat1[PInt16] = null
  implicit val int32Cat1: PInt32 => Cat1[PInt32] = null
  implicit val charCat1: PChar => Cat1[PChar] = null
  implicit val float32Cat1: PFloat32 => Cat1[PFloat32] = null

  implicit def referenceCat1[T <: PRefType](t: PReference[T]): Cat1[PReference[T]] = null

  trait Cat2[T]

  implicit val int64Cat1: PInt64 => Cat2[PInt64] = null
  implicit val float64Cat1: PFloat64 => Cat2[PFloat64] = null

  trait Int32Usable[T]

  implicit val int8U: PInt8 => Int32Usable[PInt8] = null
  implicit val int16U: PInt16 => Int32Usable[PInt16] = null
  implicit val int32U: PInt32 => Int32Usable[PInt32] = null
  implicit val charU: PChar => Int32Usable[PChar] = null

  def compileExp[R <: Stack, T <: PType](exp: Expression[T], lenv0: Map[Symbol.LabelSym, Label]): F[R] => F[R ** T] = {
    def recurse[R0 <: Stack, T0 <: PType](exp0: Expression[T0]): F[R0] => F[R0 ** T0] = compileExp(exp0, lenv0)

    exp match {
      case Expression.Unit(loc) =>
        WithSource[R](loc) ~
          pushUnit

      case Expression.Null(tpe, loc) =>
        WithSource[R](loc) ~ pushNull(tpe)

      case Expression.True(loc) =>
        WithSource[R](loc) ~ pushBool(true)

      case Expression.False(loc) =>
        WithSource[R](loc) ~ pushBool(false)

      case Expression.Char(lit, loc) =>
        WithSource[R](loc) ~
          pushChar(lit)

      case Expression.Float32(lit, loc) =>
        WithSource[R](loc) ~ pushFloat32(lit)

      case Expression.Float64(lit, loc) =>
        WithSource[R](loc) ~ pushFloat64(lit)

      case Expression.Int8(lit, loc) =>
        WithSource[R](loc) ~ pushInt8(lit)

      case Expression.Int16(lit, loc) =>
        WithSource[R](loc) ~ pushInt16(lit)

      case Expression.Int32(lit, loc) =>
        WithSource[R](loc) ~ pushInt32(lit)

      case Expression.Int64(lit, loc) =>
        WithSource[R](loc) ~ pushInt64(lit)

      case Expression.BigInt(lit, loc) =>
        WithSource[R](loc) ~
          pushBigInt(lit)

      case Expression.Str(lit, loc) =>
        WithSource[R](loc) ~
          pushString(lit)

      case Expression.Var(sym, tpe, loc) =>
        WithSource[R](loc) ~
          XLOAD(tpe, sym.getStackOffset + symOffsetOffset) // TODO(JLS): make this offset exist in F, dependent on static(0)/object function(1)

      case Expression.Closure(sym, freeVars, tpe, loc) =>
        WithSource[R](loc) ~
          CREATECLOSURE(freeVars, sym.cloName, squeezeFunction(squeezeReference(tpe)).jvmName, lenv0)

      case Expression.ApplyClo(exp, args, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          CALL(args, squeezeFunction(squeezeReference(exp.tpe)), lenv0)

      case Expression.ApplyDef(sym, args, fnTpe, tpe, loc) =>
        val arrow = squeezeFunction(squeezeReference(fnTpe))
        WithSource[R](loc) ~
          CREATEDEF(sym.defName, arrow.jvmName, tpe.tagOf) ~
          CALL(args, arrow, lenv0)

      case Expression.ApplyCloTail(exp, args, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          TAILCALL(args, squeezeFunction(squeezeReference(exp.tpe)), lenv0)

      case Expression.ApplyDefTail(sym, args, fnTpe, tpe, loc) =>
        val arrow = squeezeFunction(squeezeReference(fnTpe))
        WithSource[R](loc) ~
          CREATEDEF(sym.defName, arrow.jvmName, tpe.tagOf) ~
          TAILCALL(args, arrow, lenv0)

      case Expression.ApplySelfTail(_, _, actuals, fnTpe, _, loc) =>
        WithSource[R](loc) ~
          THISLOAD(fnTpe) ~
          TAILCALL(actuals, squeezeFunction(squeezeReference(fnTpe)), lenv0)

      case Expression.BoolNot(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          IFNE(START[R] ~ pushBool(false))(START[R] ~ pushBool(true))

      // Unary expressions
      case Expression.Float32Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          FNEG

      case Expression.Float64Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          DNEG

      case Expression.Int8Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          INEG ~
          I2B

      case Expression.Int8Not(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          pushInt8(-1) ~
          BXOR

      case Expression.Int16Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          INEG ~
          I2S

      case Expression.Int16Not(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          pushInt16(-1) ~
          SXOR

      case Expression.Int32Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          INEG

      case Expression.Int32Not(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          pushInt32(-1) ~
          IXOR

      case Expression.Int64Neg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          LNEG

      case Expression.Int64Not(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          pushInt64(-1) ~
          LXOR

      case Expression.BigIntNeg(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          BigIntNeg

      case Expression.BigIntNot(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          BigIntNot

      case Expression.ObjEqNull(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          IFNULL(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))

      case Expression.ObjNeqNull(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          IFNONNULL(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))

      // Binary expressions
      case Expression.BoolLogicalOp(op, exp1, exp2, _, loc) =>
        op match {
          case LogicalOp.And =>
            // TODO(JLS): this is probaly not optimal when coded with capabilities
            WithSource[R](loc) ~
              recurse(exp1) ~
              IFEQ(START[R] ~ pushBool(false)) {
                START[R] ~
                  recurse(exp2) ~
                  IFEQ(START[R] ~ pushBool(false))(START[R] ~ pushBool(true))
              }
          case LogicalOp.Or =>
            WithSource[R](loc) ~
              recurse(exp1) ~
              IFNE(START[R] ~ pushBool(true)) {
                START[R] ~
                  recurse(exp2) ~
                  IFNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              }
        }

      case Expression.BoolEquality(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          })

      case Expression.CharComparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: ErasedAst.EqualityOp => op match {
              case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.Float32Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PFloat32 ** PFloat32] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ FADD
          case ArithmeticOp.Sub => compileOperands ~ FSUB
          case ArithmeticOp.Mul => compileOperands ~ FMUL
          case ArithmeticOp.Div => compileOperands ~ FDIV
          case ArithmeticOp.Rem => compileOperands ~ FREM
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~ F2D ~
                recurse(exp2) ~ F2D
            } ~ D2F
        })

      case Expression.Float32Comparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => FCMPG ~ IFLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => FCMPG ~ IFLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => FCMPL ~ IFGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => FCMPL ~ IFGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: EqualityOp => op match {
              case EqualityOp.Eq => FCMPG ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => FCMPG ~ IFNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.Float64Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PFloat64 ** PFloat64] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ DADD
          case ArithmeticOp.Sub => compileOperands ~ DSUB
          case ArithmeticOp.Mul => compileOperands ~ DMUL
          case ArithmeticOp.Div => compileOperands ~ DDIV
          case ArithmeticOp.Rem => compileOperands ~ DREM
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~
                recurse(exp2)
            }
        })

      case Expression.Float64Comparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => DCMPG ~ IFLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => DCMPG ~ IFLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => DCMPL ~ IFGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => DCMPL ~ IFGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: EqualityOp => op match {
              case EqualityOp.Eq => DCMPG ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => DCMPG ~ IFNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.Int8Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PInt8 ** PInt8] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ IADD ~ I2B
          case ArithmeticOp.Sub => compileOperands ~ ISUB ~ I2B
          case ArithmeticOp.Mul => compileOperands ~ IMUL ~ I2B
          case ArithmeticOp.Div => compileOperands ~ IDIV ~ I2B
          case ArithmeticOp.Rem => compileOperands ~ IREM ~ I2B
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~ I2D ~
                recurse(exp2) ~ I2D
            } ~
              D2I ~
              I2B
        })

      case Expression.Int16Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PInt16 ** PInt16] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ IADD ~ I2S
          case ArithmeticOp.Sub => compileOperands ~ ISUB ~ I2S
          case ArithmeticOp.Mul => compileOperands ~ IMUL ~ I2S
          case ArithmeticOp.Div => compileOperands ~ IDIV ~ I2S
          case ArithmeticOp.Rem => compileOperands ~ IREM ~ I2S
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~ I2D ~
                recurse(exp2) ~ I2D
            } ~
              D2I ~
              I2S
        })

      case Expression.Int32Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PInt32 ** PInt32] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ IADD
          case ArithmeticOp.Sub => compileOperands ~ ISUB
          case ArithmeticOp.Mul => compileOperands ~ IMUL
          case ArithmeticOp.Div => compileOperands ~ IDIV
          case ArithmeticOp.Rem => compileOperands ~ IREM
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~ I2D ~
                recurse(exp2) ~ I2D
            } ~
              D2I
        })

      case Expression.Int64Arithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PInt64 ** PInt64] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ LADD
          case ArithmeticOp.Sub => compileOperands ~ LSUB
          case ArithmeticOp.Mul => compileOperands ~ LMUL
          case ArithmeticOp.Div => compileOperands ~ LDIV
          case ArithmeticOp.Rem => compileOperands ~ LREM
          case ArithmeticOp.Exp =>
            DoublePow[R] {
              START[StackNil] ~
                recurse(exp1) ~ L2D ~
                recurse(exp2) ~ L2D
            } ~ D2L
        })

      case Expression.BigIntArithmetic(op, exp1, exp2, _, loc) =>
        val compileOperands: F[R] => F[R ** PReference[PBigInt] ** PReference[PBigInt]] =
          START[R] ~ recurse(exp1) ~ recurse(exp2)
        WithSource[R](loc) ~ (op match {
          case ArithmeticOp.Add => compileOperands ~ BigIntADD
          case ArithmeticOp.Sub => compileOperands ~ BigIntSUB
          case ArithmeticOp.Mul => compileOperands ~ BigIntMUL
          case ArithmeticOp.Div => compileOperands ~ BigIntDIV
          case ArithmeticOp.Rem => compileOperands ~ BigIntREM
          case ArithmeticOp.Exp => throw InternalCompilerException("exponentiation on big integers not implemented in backend")
        })

      case Expression.Int8Bitwise(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case BitwiseOp.And => BAND
            case BitwiseOp.Or => BOR
            case BitwiseOp.Xor => BXOR
            case BitwiseOp.Shl => START[R ** PInt8 ** PInt8] ~ ISHL ~ I2B
            case BitwiseOp.Shr => BSHR
          })

      case Expression.Int16Bitwise(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case BitwiseOp.And => SAND
            case BitwiseOp.Or => SOR
            case BitwiseOp.Xor => SXOR
            case BitwiseOp.Shl => START[R ** PInt16 ** PInt16] ~ ISHL ~ I2S
            case BitwiseOp.Shr => SSHR
          })

      case Expression.Int32Bitwise(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case BitwiseOp.And => IAND
            case BitwiseOp.Or => IOR
            case BitwiseOp.Xor => IXOR
            case BitwiseOp.Shl => START[R ** PInt32 ** PInt32] ~ ISHL
            case BitwiseOp.Shr => ISHR
          })

      case Expression.Int64Bitwise(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case BitwiseOp.And => LAND
            case BitwiseOp.Or => LOR
            case BitwiseOp.Xor => LXOR
            case BitwiseOp.Shl => LSHL
            case BitwiseOp.Shr => LSHR
          })

      case Expression.BigIntBitwise(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case BitwiseOp.And => BigIntAND
            case BitwiseOp.Or => BigIntOR
            case BitwiseOp.Xor => BigIntXOR
            case BitwiseOp.Shl => BigIntSHL
            case BitwiseOp.Shr => BigIntSHR
          })

      case Expression.Int8Comparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: ErasedAst.EqualityOp => op match {
              case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.Int16Comparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: ErasedAst.EqualityOp => op match {
              case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })


      case Expression.Int32Comparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => IF_ICMPLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => IF_ICMPLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => IF_ICMPGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => IF_ICMPGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: ErasedAst.EqualityOp => op match {
              case EqualityOp.Eq => IF_ICMPEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => IF_ICMPNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.Int64Comparison(op, exp1, exp2, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => LCMP ~ IFLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => LCMP ~ IFLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => LCMP ~ IFGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => LCMP ~ IFGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: EqualityOp => op match {
              case EqualityOp.Eq => LCMP ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => LCMP ~ IFNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.BigIntComparison(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case ComparisonOp.Lt => BigIntCompareTo ~ IFLT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Le => BigIntCompareTo ~ IFLE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Gt => BigIntCompareTo ~ IFGT(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case ComparisonOp.Ge => BigIntCompareTo ~ IFGE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            case op: EqualityOp => op match {
              case EqualityOp.Eq => BigIntCompareTo ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
              case EqualityOp.Ne => BigIntCompareTo ~ IFNE(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
            }
          })

      case Expression.StringConcat(exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          stringConcat

      case Expression.StringEquality(op, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (op match {
            case EqualityOp.Eq => ObjEquals
            case EqualityOp.Ne => ObjEquals ~ IFEQ(START[R] ~ pushBool(true))(START[R] ~ pushBool(false))
          })

      case Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          IFEQ(START[R] ~ recurse(exp3))(START[R] ~ recurse(exp2))

      case Expression.Branch(exp, branches, _, loc) =>
        // TODO(JLS): Labels could probably be typed
        f => {
          // Adding source line number for debugging
          WithSource(loc)(f)
          // Calculating the updated jumpLabels map
          val updatedJumpLabels = branches.foldLeft(lenv0)((map, branch) => map + (branch._1 -> new Label()))
          // Compiling the exp
          compileExp(exp, updatedJumpLabels)(f)
          // Label for the end of all branches
          val endLabel = new Label()
          // Skip branches if `exp` does not jump
          f.visitJumpInsn(Opcodes.GOTO, endLabel)
          // Compiling branches
          branches.foreach { case (sym, branchExp) =>
            // Label for the start of the branch
            f.visitLabel(updatedJumpLabels(sym))
            // evaluating the expression for the branch
            compileExp(branchExp, updatedJumpLabels)(f)
            // Skip the rest of the branches
            f.visitJumpInsn(Opcodes.GOTO, endLabel)
          }
          // label for the end of branches
          f.visitLabel(endLabel)
          f.asInstanceOf[F[R ** T]]
        }

      case Expression.JumpTo(sym, _, loc) =>
        WithSource[R](loc) ~ (f => {
          f.visitJumpInsn(Opcodes.GOTO, lenv0(sym))
          F.push(tagOf[T])(f)
        })

      case Expression.Let(sym, exp1, exp2, _, loc) =>
        //TODO(JLS): sym is unsafe. localvar stack + reg as types?
        WithSource[R](loc) ~
          recurse(exp1) ~
          XSTORE(sym, exp1.tpe) ~
          recurse(exp2)

      case Expression.Is(sym, tag, exp, loc) => ???
      case Expression.Tag(sym, tag, exp, tpe, loc) => ???
      case Expression.Untag(sym, tag, exp, tpe, loc) => ???
      case Expression.Index(base, offset, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(base) ~
          GETFIELD(squeezeReference(base.tpe), GenTupleClasses.indexFieldName(offset), tpe, undoErasure = true)

      case Expression.Tuple(elms, tpe, loc) =>
        val tupleRef = squeezeReference(tpe)
        WithSource[R](loc) ~
          NEW(tupleRef) ~
          DUP ~
          invokeSimpleConstructor(tupleRef) ~
          multiComposition(elms.zipWithIndex) { case (elm, elmIndex) =>
            START[R ** PReference[PTuple]] ~
              DUP ~
              PUTFIELD(tupleRef, GenTupleClasses.indexFieldName(elmIndex), elm, lenv0, erasedType = true)
          }

      case Expression.RecordEmpty(tpe, loc) => ???
      case Expression.RecordSelect(exp, field, tpe, loc) => ???
      case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
      case Expression.RecordRestrict(field, rest, tpe, loc) => ???
      case Expression.ArrayLit(elms, tpe, loc) =>
        def makeAndFillArray[R0 <: Stack, T0 <: PType]
        (elms: List[Expression[T0]], arrayType: RArray[T0]):
        F[R0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]]] = {
          START[R0 ** PReference[PArray[T0]]] ~
            multiComposition(elms.zipWithIndex) { case (elm, index) =>
              START[R0 ** PReference[PArray[T0]]] ~
                DUP ~
                pushInt32(index) ~
                recurse(elm) ~
                XASTORE(arrayType.tpe)
            }
        }

        WithSource[R](loc) ~
          pushInt32(elms.length) ~
          XNEWARRAY(tpe) ~
          makeAndFillArray(elms, squeezeArray(squeezeReference(tpe)))

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        def elmArraySwitch
        [R0 <: Stack, T0 <: PType]
        (elmType: RType[T0]):
        F[R0 ** T0 ** PReference[PArray[T0]]] => F[R0 ** PReference[PArray[T0]] ** PReference[PArray[T0]] ** T0] =
          elmType match {
            case RBool | RInt8 | RInt16 | RInt32 | RChar | RFloat32 | RReference(_) => // Cat1
              //TODO(JLS): note: start here is needed because of some implicit overshadowing
              START[R0 ** T0 ** PReference[PArray[T0]]] ~
                DUP_X1 ~
                SWAP
            case RInt64 | RFloat64 => // Cat2
              START[R0 ** T0 ** PReference[PArray[T0]]] ~
                DUP_X2_onCat2 ~
                DUP_X2_onCat2 ~
                POP
          }

        WithSource[R](loc) ~
          recurse(elm) ~
          recurse(len) ~
          XNEWARRAY(tpe) ~
          elmArraySwitch(elm.tpe) ~
          arraysFill(elm.tpe)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(base) ~
          recurse(index) ~
          XALOAD(tpe)

      case Expression.ArrayStore(base, index, elm, _, loc) =>
        WithSource[R](loc) ~
          recurse(base) ~
          recurse(index) ~
          recurse(elm) ~
          XASTORE(elm.tpe) ~
          pushUnit

      case Expression.ArrayLength(base, _, loc) =>
        WithSource[R](loc) ~
          recurse(base) ~
          arrayLength(squeezeArray(squeezeReference(base.tpe)).tpe.tagOf)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(base) ~
          recurse(beginIndex) ~
          recurse(endIndex) ~
          SWAP ~
          DUP_X1 ~
          ISUB ~
          DUP ~
          XNEWARRAY(tpe) ~
          DUP2_X2_cat1_onCat1 ~
          SWAP ~
          pushInt32(0) ~
          SWAP ~
          systemArrayCopy ~
          SWAP ~
          POP

      case Expression.Ref(exp, tpe, loc) =>
        val tpeRRef = squeezeReference(tpe)
        WithSource[R](loc) ~
          NEW(tpeRRef) ~
          DUP ~
          invokeSimpleConstructor(tpeRRef) ~
          DUP ~
          recurse(exp) ~
          PUTFIELD(tpeRRef, GenRefClasses.ValueFieldName, exp.tpe, erasedType = true)

      case Expression.Deref(exp, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          GETFIELD(squeezeReference(exp.tpe), GenRefClasses.ValueFieldName, tpe, undoErasure = true)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          PUTFIELD(squeezeReference(exp1.tpe), GenRefClasses.ValueFieldName, exp2.tpe, erasedType = true) ~
          pushUnit

      case Expression.Cast(exp, tpe, loc) =>
        // TODO(JLS): When is this used and can it be removed? Should it be a flix error? user or compiler error?
        WithSource[R](loc) ~
          recurse(exp) ~
          (f => {
            undoErasure(tpe, f.visitor)
            f.asInstanceOf[F[R ** T]]
          })

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val catchCases = rules.map {
          case CatchRule(sym, clazz, exp) =>
            val ins = START[R ** PReference[PAnyObject]] ~
              ASTORE(sym) ~
              recurse(exp)
            (new Label(), clazz, ins)
        }
        WithSource[R](loc) ~
          tryCatch(recurse(exp), catchCases)

      case Expression.InvokeConstructor(constructor, args, _, loc) =>
        // TODO(JLS): pretty messy
        val constructorDescriptor = Descriptor.of(asm.Type.getConstructorDescriptor(constructor))
        val className = JvmName.fromClass(constructor.getDeclaringClass)
        WithSource[R](loc) ~
          NEW(className, tagOf[PAnyObject]) ~
          DUP ~
          (f => {
            // TODO(JLS): cant use multiComposition since there is an effect on the stack each iteration
            for (arg <- args) {
              recurse(arg)(f)
              // TODO(JLS): maybe undoErasure here? genExpression matches on array types
            }
            f.visitMethodInsn(Opcodes.INVOKESPECIAL, className.internalName, JvmName.constructorMethod, constructorDescriptor)
            f.asInstanceOf[F[R ** T]]
          })

      case Expression.InvokeMethod(method, exp, args, tpe, loc) => f => {
        // TODO(JLS): fix this
        // Adding source line number for debugging
        WithSource(loc)(f)

        // Evaluate the receiver object.
        recurse(exp)(f)
        val className = JvmName.fromClass(method.getDeclaringClass)
        f.visitTypeInsn(Opcodes.CHECKCAST, className.internalName)

        // Evaluate arguments left-to-right and push them onto the stack.
        for (arg <- args) {
          recurse(arg)(f)
          // TODO(JLS): maybe undoErasure here? genExpression matches on array types (method.getParameterTypes)
        }
        val descriptor = Descriptor.of(asm.Type.getMethodDescriptor(method))

        // Check if we are invoking an interface or class.
        if (method.getDeclaringClass.isInterface) {
          f.visitMethodInsn(Opcodes.INVOKEINTERFACE, className.internalName, method.getName, descriptor, isInterface = true)
        } else {
          f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className.internalName, method.getName, descriptor)
        }

        // If the method is void, put a unit on top of the stack
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) pushUnit(f)
        f.asInstanceOf[F[R ** T]]
      }

      case Expression.InvokeStaticMethod(method, args, tpe, loc) => f => {
        // TODO(JLS): fix this
        // Adding source line number for debugging
        WithSource(loc)(f)

        // Evaluate arguments left-to-right and push them onto the stack.
        for (arg <- args) {
          recurse(arg)(f)
          // TODO(JLS): maybe undoErasure here? genExpression matches on array types
        }
        val className = JvmName.fromClass(method.getDeclaringClass)
        val descriptor = Descriptor.of(asm.Type.getMethodDescriptor(method))

        f.visitMethodInsn(Opcodes.INVOKESTATIC, className.internalName, method.getName, descriptor)

        // If the method is void, put a unit on top of the stack
        if (asm.Type.getType(method.getReturnType) == asm.Type.VOID_TYPE) pushUnit(f)
        f.asInstanceOf[F[R ** T]]
      }

      case Expression.GetField(field, exp, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          ((f: F[R ** PReference[PAnyObject]]) => {
            f.visitFieldInsn(Opcodes.GETFIELD, JvmName.fromClass(field.getDeclaringClass).internalName, field.getName, tpe.descriptor)
            f.asInstanceOf[F[R ** T]]
          })

      case Expression.PutField(field, exp1, exp2, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          recurse(exp2) ~
          (f => {
            f.visitFieldInsn(Opcodes.PUTFIELD, JvmName.fromClass(field.getDeclaringClass).internalName, field.getName, exp2.tpe.descriptor)
            f.asInstanceOf[F[R]]
          }) ~
          pushUnit

      case Expression.GetStaticField(field, tpe, loc) =>
        WithSource[R](loc) ~
          GETSTATIC(JvmName.fromClass(field.getDeclaringClass), field.getName, tpe, undoErasure = false)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          (f => {
            f.visitFieldInsn(Opcodes.PUTSTATIC, JvmName.fromClass(field.getDeclaringClass).internalName, field.getName, exp.tpe.descriptor)
            f.asInstanceOf[F[R]]
          }) ~
          pushUnit

      case Expression.NewChannel(exp, tpe, loc) =>
        val chanRef = squeezeReference(tpe)
        WithSource[R](loc) ~
          NEW(chanRef) ~
          DUP ~
          recurse(exp) ~
          (f => {
            f.visitMethodInsn(Opcodes.INVOKESPECIAL, chanRef.internalName, JvmName.constructorMethod,
              JvmName.getMethodDescriptor(List(RInt32), None), false)
            f.asInstanceOf[F[R ** T]]
          })

      case Expression.GetChannel(exp, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          getChannelValue(tpe)

      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        WithSource[R](loc) ~
          recurse(exp1) ~
          DUP ~
          recurse(exp2) ~
          putChannelValue(exp2.tpe)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        WithSource[R](loc) ~
          pushInt32(rules.size) ~
          ANEWARRAY(RReference(RChannel(RReference(RObject)))) ~
          multiComposition(rules.zipWithIndex) {
            case (rule, index) =>
              START[R ** PReference[PArray[PReference[PChan[PAnyObject]]]]] ~
                DUP ~
                pushInt32(index) ~
                recurse(rule.chan) ~
                ChannelSUBTYPE ~
                AASTORE
          } ~
          pushBool(default.isDefined) ~
          ((f: F[R ** PReference[PArray[PReference[PChan[PAnyObject]]]] ** PInt32]) => {
            f.visitMethodInsn(Opcodes.INVOKESTATIC, JvmName.Flix.Channel.internalName, "select",
              JvmName.getMethodDescriptor(List(JvmName.Flix.Channel, RBool), JvmName.Flix.SelectChoice), false)
            f.asInstanceOf[F[R ** PReference[PAnyObject]]]
          }) ~
          DUP ~
          (f => {
            f.visitFieldInsn(Opcodes.GETFIELD, JvmName.Flix.SelectChoice.internalName, "defaultChoice", RBool.descriptor)
            f.asInstanceOf[F[R ** PReference[PAnyObject] ** PInt32]]
          }) ~
          IFNE {
            START[R ** PReference[PAnyObject]] ~
              POP ~
              (if (default.isDefined) {
                recurse(default.get)
              } else {
                f => {
                  f.visitInsn(Opcodes.ACONST_NULL)
                  f.visitInsn(Opcodes.ATHROW)
                  f.asInstanceOf[F[R ** T]]
                }
              })
          } {
            START[R ** PReference[PAnyObject]] ~
              DUP ~
              GETFIELD(JvmName.Flix.SelectChoice, "branchNumber", RInt8, undoErasure = false) ~
              SCAFFOLD
          }


      case Expression.Spawn(exp, _, loc) =>
        def initThread[R0 <: Stack, T0 <: PType]: F[R0 ** PReference[PAnyObject] ** PReference[PFunction[T0]]] => F[R0] = f => {
          f.visitMethodInsn(Opcodes.INVOKESPECIAL, JvmName.Java.Thread.internalName, JvmName.constructorMethod, JvmName.Java.Runnable.thisToNothingDescriptor)
          f.asInstanceOf[F[R0]]
        }

        def startThread[R0 <: Stack]: F[R0 ** PReference[PAnyObject]] => F[R0] = f => {
          f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.Thread.internalName, "start", JvmName.nothingToVoid)
          f.asInstanceOf[F[R0]]
        }

        WithSource[R](loc) ~
          NEW(JvmName.Java.Thread, tagOf[PAnyObject]) ~
          DUP ~
          recurse(exp) ~
          initThread ~
          startThread ~
          pushUnit

      case Expression.Lazy(exp, tpe, loc) =>
        WithSource[R](loc) ~
          mkLazy(tpe, exp.tpe, recurse(exp))

      case Expression.Force(exp, _, loc) =>
        WithSource[R](loc) ~
          recurse(exp) ~
          FORCE(exp.tpe)

      case Expression.HoleError(sym, _, loc) =>
        WithSource[R](loc) ~
          throwStringedCompilerError(JvmName.Flix.HoleError, sym.toString, loc)

      case Expression.MatchError(_, loc) =>
        WithSource[R](loc) ~
          throwCompilerError(JvmName.Flix.MatchError, loc)

      case Expression.BoxBool(exp, loc) => ???
      case Expression.BoxInt8(exp, loc) => ???
      case Expression.BoxInt16(exp, loc) => ???
      case Expression.BoxInt32(exp, loc) => ???
      case Expression.BoxInt64(exp, loc) => ???
      case Expression.BoxChar(exp, loc) => ???
      case Expression.BoxFloat32(exp, loc) => ???
      case Expression.BoxFloat64(exp, loc) => ???
      case Expression.UnboxBool(exp, loc) => ???
      case Expression.UnboxInt8(exp, loc) => ???
      case Expression.UnboxInt16(exp, loc) => ???
      case Expression.UnboxInt32(exp, loc) => ???
      case Expression.UnboxInt64(exp, loc) => ???
      case Expression.UnboxChar(exp, loc) => ???
      case Expression.UnboxFloat32(exp, loc) => ???
      case Expression.UnboxFloat64(exp, loc) => ???
    }
  }

}
