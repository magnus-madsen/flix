package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, FormalParam, FreeVar, Root}
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassWriter, Label}

/**
  * Generates byte code for the closure classes.
  */
object GenClosureClasses {

  /**
    * Returns the set of closures classes for the given set of definitions `defs`.
    */
  def gen(closures: Set[ClosureInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a closure class for each closure and collect the results in a map.
    //
    ParOps.parAgg(closures, Map.empty[JvmName, JvmClass])({
      case (macc, closure) =>
        val jvmType = JvmOps.getClosureClassType(closure)
        val jvmName = jvmType.name
        val bytecode = genByteCode(closure)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }, _ ++ _)
  }

  /**
    * Returns the byte code for the closure with the given symbol `sym` and free variables `freeVars`.
    *
    * For example, given the symbol `mkAdder` with type (Int32, Int32) -> Int32 and the free variable `x`, we create:
    *
    * public final class Clo$mkAdder implements Fn2$Int32$Int32$Int32 {
    * public int clo0;
    * public int arg0; // from Fn2$...
    * public int arg1; // from Fn2$...
    * public int result; // from Cont$...
    *
    * public Clo$mkAdder() { }
    *
    * public Cont$Int32 invoke() {
    *   this.res = this.x + this.arg0;
    *   return null;
    * }
    */
  private def genByteCode(closure: ClosureInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Args of the function
    val MonoType.Arrow(_, tresult) = closure.tpe

    // `JvmType` of the interface for `closure.tpe`
    val functionInterface = JvmOps.getFunctionInterfaceType(closure.tpe)

    // `JvmType` of the class for `defn`
    val classType = JvmOps.getClosureClassType(closure)

    // Class visitor
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null,
      functionInterface.name.toInternalName, null)

    // Generate a field for each captured variable.
    for ((freeVar, index) <- closure.freeVars.zipWithIndex) {
      // `JvmType` of `freeVar`
      val varType = JvmOps.getErasedJvmType(freeVar.tpe)

      // `clo$index` field
      AsmOps.compileField(visitor, s"clo$index", varType, isStatic = false, isPrivate = false)
    }

    // Invoke method of the class
    compileInvokeMethod(visitor, classType, root.defs(closure.sym), closure.freeVars, tresult)

    // Constructor of the class
    compileConstructor(visitor, functionInterface)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Invoke method for the given `defn`, `classType`, and `resultType`.
    */
  private def compileInvokeMethod(visitor: ClassWriter, classType: JvmType.Reference, defn: Def,
                                  freeVars: List[FreeVar], resultType: MonoType)(implicit root: Root, flix: Flix): Unit = {
    // Continuation class
    val continuationType = JvmOps.getContinuationInterfaceType(defn.tpe)

    // Method header
    val invokeMethod = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, GenContinuationInterfaces.InvokeMethodName,
      AsmOps.getMethodDescriptor(Nil, continuationType), null, null)
    invokeMethod.visitCode()

    // Free variables
    val frees = defn.formals.take(freeVars.length).map(x => FreeVar(x.sym, x.tpe))

    // Function parameters
    val params = defn.formals.takeRight(defn.formals.length - freeVars.length)

    // Enter label
    val enterLabel = new Label()

    // Saving free variables on variable stack
    for ((FreeVar(sym, tpe), ind) <- frees.zipWithIndex) {
      // Erased type of the free variable
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the free variable from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"clo$ind", erasedType.toDescriptor)

      // Saving the free variable on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 1)
    }

    // Saving parameters on variable stack
    for ((FormalParam(sym, tpe), ind) <- params.zipWithIndex) {
      // Erased type of the parameter
      val erasedType = JvmOps.getErasedJvmType(tpe)

      // Getting the parameter from IFO
      invokeMethod.visitVarInsn(ALOAD, 0)
      invokeMethod.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"arg$ind", erasedType.toDescriptor)

      // Saving the parameter on variable stack
      val iSTORE = AsmOps.getStoreInstruction(erasedType)
      invokeMethod.visitVarInsn(iSTORE, sym.getStackOffset + 1)
    }

    // Generating the expression
    GenExpression.compileExpression(defn.exp, invokeMethod, classType, Map(), enterLabel)

    // Loading `this`
    invokeMethod.visitVarInsn(ALOAD, 0)

    // Swapping `this` and result of the expression
    val resultJvmType = JvmOps.getErasedJvmType(resultType)
    if (AsmOps.getStackSize(resultJvmType) == 1) {
      invokeMethod.visitInsn(SWAP)
    } else {
      invokeMethod.visitInsn(DUP_X2)
      invokeMethod.visitInsn(POP)
    }

    // Saving the result on the `result` field of IFO
    invokeMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, GenContinuationInterfaces.ResultFieldName, resultJvmType.toDescriptor)

    // Return
    invokeMethod.visitInsn(ACONST_NULL)
    invokeMethod.visitInsn(ARETURN)
    invokeMethod.visitMaxs(999, 999)
    invokeMethod.visitEnd()
  }

  /**
    * Constructor of the class
    */
  private def compileConstructor(visitor: ClassWriter, superClass: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {
    // Constructor header
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    // Calling constructor of super
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass.name.toInternalName, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)
    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }
}
