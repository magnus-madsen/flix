/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

object BytecodeInstructions {

  /**
    * A Frame that represents the Jvm state and contains a visitor to emit code
    */
  sealed class F(visitor: MethodVisitor) {
    def visitTypeInstruction(opcode: Int, tpe: JvmName): Unit =
      visitor.visitTypeInsn(opcode, tpe.toInternalName)

    def visitInstruction(opcode: Int): Unit = visitor.visitInsn(opcode)

    def visitMethodInstruction(opcode: Int, owner: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean): Unit =
      visitor.visitMethodInsn(opcode, owner.toInternalName, methodName, descriptor.toDescriptor, isInterface)

    // TODO: sanitize varags
    def visitInvokeDynamicInstruction(methodName: String, descriptor: MethodDescriptor, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit =
      visitor.visitInvokeDynamicInsn(methodName, descriptor.toDescriptor, bootstrapMethodHandle.handle, bootstrapMethodArguments: _*)

    def visitFieldInstruction(opcode: Int, owner: JvmName, fieldName: String, fieldType: BackendType): Unit =
      visitor.visitFieldInsn(opcode, owner.toInternalName, fieldName, fieldType.toDescriptor)

    def visitVarInstruction(opcode: Int, v: Int): Unit =
      visitor.visitVarInsn(opcode, v)

    def visitJumpInstruction(opcode: Int, label: Label): Unit =
      visitor.visitJumpInsn(opcode, label)

    def visitLabel(label: Label): Unit =
      visitor.visitLabel(label)

    def visitLoadConstantInstruction(v: Any): Unit =
      visitor.visitLdcInsn(v)

    def visitTryCatchBlock(beforeTry: Label, afterTry: Label, handlerStart: Label): Unit =
      visitor.visitTryCatchBlock(beforeTry, afterTry, handlerStart, null)

    def cheat(command: MethodVisitor => Unit): Unit = command(visitor)
  }

  type InstructionSet = F => F

  /**
    * Returns the sequential composition of the two instructions.
    */
  def compose(i1: InstructionSet, i2: InstructionSet): InstructionSet =
    f => i2(i1(f))

  implicit class ComposeOps(i1: InstructionSet) {
    def ~(i2: InstructionSet): InstructionSet =
      compose(i1, i2)
  }

  sealed case class Handle(handle: asm.Handle)

  def mkStaticHandle(m: StaticMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, m.clazz.toInternalName, m.name, m.d.toDescriptor, false))
  }

  def mkStaticHandle(m: StaticInterfaceMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, m.clazz.toInternalName, m.name, m.d.toDescriptor, true))
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Structures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  sealed trait Condition

  object Condition {
    case object ACMPEQ extends Condition

    case object ACMPNE extends Condition

    case object Bool extends Condition

    case object EQ extends Condition

    case object ICMPEQ extends Condition

    case object ICMPNE extends Condition

    case object NE extends Condition

    case object LT extends Condition

    case object LE extends Condition

    case object GT extends Condition

    case object GE extends Condition

    case object NONNULL extends Condition

    case object NULL extends Condition
  }

  sealed trait Branch

  object Branch {
    case object TrueBranch extends Branch

    case object FalseBranch extends Branch
  }

  // TODO: do this for methods
  class Variable(val tpe: BackendType, index: Int) {
    def load(): InstructionSet = xLoad(tpe, index)

    def store(): InstructionSet = xStore(tpe, index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def AASTORE(): InstructionSet = f => {
    f.visitInstruction(Opcodes.AASTORE)
    f
  }

  def ACONST_NULL(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ACONST_NULL)
    f
  }

  def ALOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ALOAD, index)
    f
  }

  def ANEWARRAY(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.ANEWARRAY, className)
    f
  }

  def ARETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ARETURN)
    f
  }

  def ARRAYLENGTH(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ARRAYLENGTH)
    f
  }

  def ASTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ASTORE, index)
    f
  }

  def ATHROW(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ATHROW)
    f
  }

  def CHECKCAST(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.CHECKCAST, className)
    f
  }

  def DLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.DLOAD, index)
    f
  }

  def DRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DRETURN)
    f
  }

  def DSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.DSTORE, index)
    f
  }

  def DUP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP)
    f
  }

  def DUP2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP2)
    f
  }

  def DUP_X1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP_X1)
    f
  }

  def DUP_X2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP_X2)
    f
  }

  def DUP2_X1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP2_X1)
    f
  }

  def DUP2_X2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP2_X2)
    f
  }

  def FLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.FLOAD, index)
    f
  }

  def FRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.FRETURN)
    f
  }

  def FSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.FSTORE, index)
    f
  }

  def GETFIELD(field: InstanceField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETFIELD, field.clazz, field.name, field.tpe)
    f
  }

  def GETSTATIC(field: StaticField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe)
    f
  }

  def IADD(): InstructionSet = f => {
    f.visitInstruction(Opcodes.IADD)
    f
  }

  def ICONST_0(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_0)
    f
  }

  def ICONST_1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_1)
    f
  }

  def ICONST_2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_2)
    f
  }

  def ICONST_3(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_3)
    f
  }

  def ICONST_4(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_4)
    f
  }

  def ICONST_5(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_5)
    f
  }

  def ICONST_M1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_M1)
    f
  }

  def ILOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ILOAD, index)
    f
  }

  def INSTANCEOF(tpe: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.INSTANCEOF, tpe)
    f
  }

  /**
    * Make an object which the functional interface of `lambdaMethod`. The
    * implementation of the functional method will be the static method
    * represented by `callHandle`. `callD` is the method descriptor of the
    * static method.
    *
    * `drop` is used for partial application of the static function.
    * Lets say you want to implement the functional interface method of
    * `Function<String, String>` with the partial application of the static
    * function `String example(String, String)` with `"Hi"`. Then you can
    * partially apply the leftmost argument by having `drop = 1`. This then
    * means that the instruction returned will expect the missing string
    * argument on the op stack.
    *
    * for a function with `k` arguments, `drop = n` means that given the first
    * `k-n` arguments on the op stack, this will represent a function of the
    * last `n` arguments to the original return type. This must of course
    * correspond to the type of `lambdaMethod`.
    */
  def mkStaticLambda(lambdaMethod: InterfaceMethod, callD: MethodDescriptor, callHandle: Handle, drop: Int): InstructionSet = f => {
    f.visitInvokeDynamicInstruction(
      lambdaMethod.name,
      mkDescriptor(callD.arguments.dropRight(drop): _*)(lambdaMethod.clazz.toTpe),
      mkStaticHandle(BackendObjType.LambdaMetaFactory.MetaFactoryMethod),
      lambdaMethod.d.toAsmType,
      callHandle.handle,
      lambdaMethod.d.toAsmType
    )
    f
  }

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticMethod, drop: Int): InstructionSet =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticInterfaceMethod, drop: Int): InstructionSet =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def INVOKEINTERFACE(interfaceName: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor, isInterface = true)
    f
  }

  def INVOKEINTERFACE(m: InterfaceMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d, isInterface = true)
    f
  }

  def INVOKESPECIAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    val isInterface = false // OBS this is not technically true if you use it to call private interface methods(?)
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor, isInterface = isInterface)
    f
  }

  def INVOKESPECIAL(c: ConstructorMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d, isInterface = false)
    f
  }

  def INVOKESTATIC(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor, isInterface)
    f
  }

  def INVOKESTATIC(m: StaticMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = false)
    f
  }

  def INVOKESTATIC(m: StaticInterfaceMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = true)
    f
  }

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor, isInterface)
    f
  }

  def INVOKEVIRTUAL(m: AbstractMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)
    f
  }

  def INVOKEVIRTUAL(m: InstanceMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)
    f
  }

  def IRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.IRETURN)
    f
  }

  def ISTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ISTORE, index)
    f
  }

  def LCMP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LCMP)
    f
  }

  def LCONST_0(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LCONST_0)
    f
  }

  def LCONST_1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LCONST_1)
    f
  }

  def LSHL(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LSHL)
    f
  }

  def LADD(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LADD)
    f
  }

  def LLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.LLOAD, index)
    f
  }

  def LRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LRETURN)
    f
  }

  def LSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.LSTORE, index)
    f
  }

  def NEW(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.NEW, className)
    f
  }

  def POP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.POP)
    f
  }

  def POP2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.POP2)
    f
  }

  def PUTFIELD(field: InstanceField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe)
    f
  }

  def PUTSTATIC(field: StaticField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe)
    f
  }

  def RETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.RETURN)
    f
  }

  def SWAP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.SWAP)
    f
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def branch(c: Condition)(cases: Branch => InstructionSet): InstructionSet = f0 => {
    var f = f0
    val jumpLabel = new Label()
    val skipLabel = new Label()
    f.visitJumpInstruction(opcodeOf(c), jumpLabel)

    f = cases(FalseBranch)(f)
    f.visitJumpInstruction(Opcodes.GOTO, skipLabel)

    f.visitLabel(jumpLabel)
    f = cases(TrueBranch)(f)
    f.visitLabel(skipLabel)
    f
  }

  def cheat(command: MethodVisitor => Unit): InstructionSet = f => {
    f.cheat(command)
    f
  }

  /// do { i } while(c)
  def doWhile(c: Condition)(i: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val start = new Label()
    f.visitLabel(start)
    f = i(f)
    f.visitJumpInstruction(opcodeOf(c), start)
    f
  }

  /// while(c(t)) { i }
  def whileLoop(c: Condition)(t: InstructionSet)(i: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val startLabel = new Label()
    val doneLabel = new Label()
    f.visitLabel(startLabel)
    f = t(f)
    f.visitJumpInstruction(opcodeOf(negated(c)), doneLabel)
    f = i(f)
    f.visitJumpInstruction(Opcodes.GOTO, startLabel)
    f.visitLabel(doneLabel)
    f
  }

  def ifCondition(c: Condition)(i: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val jumpLabel = new Label()
    f.visitJumpInstruction(opcodeOf(negated(c)), jumpLabel)
    f = i(f)
    f.visitLabel(jumpLabel)
    f
  }

  /**
    * Using [[ifCondition]] uses less jumps, so use that if the conditional code
    * is returns or throws
    */
  def ifConditionElse(c: Condition)(i: InstructionSet)(otherwise: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val conditionLabel = new Label()
    val endLabel = new Label()
    f.visitJumpInstruction(opcodeOf(c), conditionLabel)
    f = otherwise(f)
    f.visitJumpInstruction(Opcodes.GOTO, endLabel)
    f.visitLabel(conditionLabel)
    f = i(f)
    f.visitLabel(endLabel)
    f
  }

  def tryCatch(body: InstructionSet)(catchI: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val beforeTry = new Label()
    val afterTry = new Label()
    val handlerStart = new Label()
    val afterEverything = new Label()
    f.visitTryCatchBlock(beforeTry, afterTry, handlerStart)
    f.visitLabel(beforeTry)
    f = body(f)
    f.visitLabel(afterTry)
    f.visitJumpInstruction(Opcodes.GOTO, afterEverything)
    f.visitLabel(handlerStart)
    f = catchI(f)
    f.visitLabel(afterEverything)
    f
  }

  def invokeConstructor(className: JvmName, descriptor: MethodDescriptor): InstructionSet =
    INVOKESPECIAL(className, JvmName.ConstructorMethod, descriptor)

  def nop(): InstructionSet =
    f => f

  def pushBool(b: Boolean): InstructionSet =
    if (b) ICONST_1() else ICONST_0()

  def pushNull(): InstructionSet =
    ACONST_NULL()

  def pushString(s: String): InstructionSet = f => {
    f.visitLoadConstantInstruction(s)
    f
  }

  def storeWithName(index: Int, tpe: BackendType)(body: Variable => InstructionSet): InstructionSet =
    xStore(tpe, index) ~ body(new Variable(tpe, index))

  // TODO: this should be "wrong" if used on F in a static context
  def thisLoad(): InstructionSet =
    ALOAD(0)

  def throwUnsupportedOperationException(msg: String): InstructionSet =
    NEW(JvmName.UnsupportedOperationException) ~
      DUP() ~
      pushString(msg) ~
      INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod,
        mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      ATHROW()

  def withName(index: Int, tpe: BackendType)(body: Variable => InstructionSet): InstructionSet =
    body(new Variable(tpe, index))

  def withNames(index: Int, tpes: List[BackendType])(body: (Int, List[Variable]) => InstructionSet): InstructionSet = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      val stackSize = if (tpe.is64BitWidth) 2 else 1
      runningIndex = runningIndex + stackSize
      variable
    })
    body(runningIndex, variables)
  }

  def xLoad(tpe: BackendType, index: Int): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ILOAD(index)
    case BackendType.Int64 => LLOAD(index)
    case BackendType.Float32 => FLOAD(index)
    case BackendType.Float64 => DLOAD(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ALOAD(index)
  }

  /**
    * Pops the top of the stack using `POP` or `POP2` depending on the value size.
    */
  def xPop(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 |
         BackendType.Float32 | BackendType.Array(_) | BackendType.Reference(_) => POP()
    case BackendType.Int64 | BackendType.Float64 => POP2()
  }

  def xReturn(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => IRETURN()
    case BackendType.Int64 => LRETURN()
    case BackendType.Float32 => FRETURN()
    case BackendType.Float64 => DRETURN()
    case BackendType.Array(_) | BackendType.Reference(_) => ARETURN()
  }

  def xStore(tpe: BackendType, index: Int): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ISTORE(index)
    case BackendType.Int64 => LSTORE(index)
    case BackendType.Float32 => FSTORE(index)
    case BackendType.Float64 => DSTORE(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ASTORE(index)
  }

  def xSwap(lowerLarge: Boolean, higherLarge: Boolean): InstructionSet = (lowerLarge, higherLarge) match {
    case (true, true) => DUP2_X2() ~ POP2()
    case (true, false) => DUP_X2() ~ POP()
    case (false, true) => DUP2_X1() ~ POP2()
    case (false, false) => SWAP()
  }

  /**
    * Converts the top of the stack to a string (including null), assuming that
    * `tpe` accurately represents its type.
    */
  def xToString(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool => INVOKESTATIC(BackendObjType.String.BoolValueOf)
    case BackendType.Char => INVOKESTATIC(BackendObjType.String.CharValueOf)
    case BackendType.Int8 => INVOKESTATIC(BackendObjType.String.Int8ValueOf)
    case BackendType.Int16 => INVOKESTATIC(BackendObjType.String.Int16ValueOf)
    case BackendType.Int32 => INVOKESTATIC(BackendObjType.String.Int32ValueOf)
    case BackendType.Int64 => INVOKESTATIC(BackendObjType.String.Int64ValueOf)
    case BackendType.Float32 => INVOKESTATIC(BackendObjType.String.Float32ValueOf)
    case BackendType.Float64 => INVOKESTATIC(BackendObjType.String.Float64ValueOf)
    case BackendType.Reference(_) => INVOKESTATIC(BackendObjType.String.ObjectValueOf)

    case BackendType.Array(BackendType.Bool) => INVOKESTATIC(BackendObjType.Arrays.BoolArrToString)
    case BackendType.Array(BackendType.Char) => INVOKESTATIC(BackendObjType.Arrays.CharArrToString)
    case BackendType.Array(BackendType.Int8) => INVOKESTATIC(BackendObjType.Arrays.Int8ArrToString)
    case BackendType.Array(BackendType.Int16) => INVOKESTATIC(BackendObjType.Arrays.Int16ArrToString)
    case BackendType.Array(BackendType.Int32) => INVOKESTATIC(BackendObjType.Arrays.Int32ArrToString)
    case BackendType.Array(BackendType.Int64) => INVOKESTATIC(BackendObjType.Arrays.Int64ArrToString)
    case BackendType.Array(BackendType.Float32) => INVOKESTATIC(BackendObjType.Arrays.Float32ArrToString)
    case BackendType.Array(BackendType.Float64) => INVOKESTATIC(BackendObjType.Arrays.Float64ArrToString)
    case BackendType.Array(BackendType.Reference(_) | BackendType.Array(_)) => INVOKESTATIC(BackendObjType.Arrays.DeepToString)
  }

  def composeN(ins: IterableOnce[InstructionSet]): InstructionSet =
    ins.iterator.foldLeft(nop())(compose)

  /**
    * Sequential composition with `sep` between elements.
    */
  def joinN(ins: List[InstructionSet], sep: InstructionSet): InstructionSet = ins match {
    case Nil => nop()
    case head :: next => head ~ composeN(next.map(e => sep ~ e))
  }


  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  private def opcodeOf(c: Condition): Int = c match {
    case Condition.ACMPEQ => Opcodes.IF_ACMPEQ
    case Condition.ACMPNE => Opcodes.IF_ACMPNE
    case Condition.Bool => opcodeOf(Condition.NE)
    case Condition.EQ => Opcodes.IFEQ
    case Condition.ICMPEQ => Opcodes.IF_ICMPEQ
    case Condition.ICMPNE => Opcodes.IF_ICMPNE
    case Condition.LT => Opcodes.IFLT
    case Condition.LE => Opcodes.IFLE
    case Condition.GT => Opcodes.IFGT
    case Condition.GE => Opcodes.IFGE
    case Condition.NE => Opcodes.IFNE
    case Condition.NONNULL => Opcodes.IFNONNULL
    case Condition.NULL => Opcodes.IFNULL
  }

  private def negated(c: Condition): Condition = c match {
    case Condition.ACMPEQ => Condition.ACMPNE
    case Condition.ACMPNE => Condition.ACMPEQ
    case Condition.Bool => negated(Condition.NE)
    case Condition.EQ => Condition.NE
    case Condition.ICMPEQ => Condition.ICMPNE
    case Condition.ICMPNE => Condition.ICMPEQ
    case Condition.LT => Condition.GE
    case Condition.LE => Condition.GT
    case Condition.GT => Condition.LE
    case Condition.GE => Condition.LT
    case Condition.NE => Condition.EQ
    case Condition.NONNULL => Condition.NULL
    case Condition.NULL => Condition.NONNULL
  }
}
