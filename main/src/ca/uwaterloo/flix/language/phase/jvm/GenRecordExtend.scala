package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._
/**
  * Generates bytecode for the extended record class.
  */
object GenRecordExtend {

  /**
    * Returns a Map with a single entry, for the extended record class
    */
  //TODO: Miguel: Should return a set of extended record classes, one for each JvmType to avoid boxing
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
      val interfaceType = JvmOps.getRecordInterfaceType()
      val jvmType = JvmOps.getRecordExtendClassType()
      val jvmName = jvmType.name
      val targs = List[JvmType](JvmType.String, JvmType.Object, interfaceType)
      val bytecode = genByteCode(jvmType,interfaceType, targs)
      Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * This method creates the class for the RecordExtend.
    * Here, we first instantiate the visitor required to emit the code.
    *
    * Then we create the name of the class to be generated and store the result in `className`
    *
    * We then define the super of this class (Object is the super here) and interfaces which this class implements
    * (RecordEmpty).
    * Then using super and interfaces we will create the class header.
    *
    * Then we generate the code for each of the fields in the RecordExtend class, namely the label, value and the rest of the record
    *
    * Which means we generate the following field in the class:
    *
    * private String field0;
    * private Object field1;
    * private IRecord field2;
    *
    * Then we precede with generating the code for constructor.The constructor receives three arguments, the field label,
    * value and the rest of the record.
    * For example for RecordExtend(String, Object, IRecord) creates the following constructor:
    *
    * public RecordExtend(String var1, Object var2, IRecord var3) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    *   this.field2 = var3;
    * }
    *
    *
    * Then we
    *
    * Next, we will generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * The `toString` method is always the following:
    *
    * public string toString(Object var1) throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    *
    * Then, we will generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * The `hashCode` method is always the following:
    *
    * public int hashCode(Object var1) throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    *
    * Finally, we generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    *
    */
  private def genByteCode(classType: JvmType.Reference, interfaceType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // internal name of interfaces to be implemented
    val implementedInterfaces = Array(interfaceType.name.toInternalName)

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, implementedInterfaces)

    // Source of the class
    visitor.visitSource(classType.name.toInternalName, null)

    for ((field, ind) <- targs.zipWithIndex) {
      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      AsmOps.compileField(visitor, fieldName, field, isStatic = false, isPrivate = true)
    }

    // Emit the code for the constructor
    compileRecordExtendConstructor(visitor, classType, targs)

    // Emit code for the 'getField' method
    compileRecordExtendGetField(visitor, classType)

    // Emit code for the 'removeField' method
    compileRecordExtendremoveField(visitor, classType)

    // Generate `toString` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate `hashCode` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate `equals` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void),
      "equals method shouldn't be called")

    visitor.visitEnd()
    visitor.toByteArray
  }


  /**
    * This method generates the constructor for the RecordExtend class. This constructor receives three arguments, the field label,
    * value and the rest of the record.
    * For example for RecordExtend(String, Object, IRecord) creates the following constructor:
    *
    * public RecordExtend(String var1, Object var2, IRecord var3) {
    *   this.field0 = var1;
    *   this.field1 = var2;
    *   this.field2 = var3;
    * }
    *
    */
  def compileRecordExtendConstructor(visitor: ClassWriter, classType: JvmType.Reference, fields: List[JvmType])(implicit root: Root, flix: Flix): Unit = {

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(fields, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)


    var offset: Int = 1
    //Emit code to set each of the fields to the corresponding value provided in the arguments
    //e.g. for the first field it should emit this.field0 = var1
    for ((field, ind) <- fields.zipWithIndex) {
      val iLoad = AsmOps.getLoadInstruction(field)

      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitVarInsn(iLoad, offset)
      constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, s"field$ind", field.toDescriptor)

      field match {
        case JvmType.PrimLong | JvmType.PrimDouble => offset += 2
        case _ => offset += 1
      }
    }

    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }


  /**
    * This method generates code for the getField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return the value stored
    * in the field1 (the value of the field). In case the provided label is not equal we recursively call getField on the rest of the record,
    * and return the value provided by the recursive call.
    *
    */
  def compileRecordExtendGetField(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {

    val getField = visitor.visitMethod(ACC_PUBLIC, "getField",
      AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object), null, null)

    getField.visitCode()

    //Push "this" onto stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field0 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field0", JvmType.String.toDescriptor)

    //Push the function argument onto the stack
    getField.visitVarInsn(ALOAD, 1)

    //Compare both strings on the stack using equals.
    getField.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName,
      "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)


    //create new labels
    val falseCase = new Label
    val ret = new Label


    //if the strings are equal ...
    getField.visitJumpInsn(IFEQ, falseCase)

    //true case
    //return this.field1

    //Load 'this' onto the stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field1 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field1", JvmType.Object.toDescriptor)

    //Jump into the return label
    getField.visitJumpInsn(GOTO,ret)

    //Emit false case label
    getField.visitLabel(falseCase)

    //false case
    //recursively call this.field2.getField(var1)

    //Load 'this' onto the stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field2 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field2",
      JvmOps.getRecordInterfaceType().toDescriptor)

    //Push var1 onto the stack
    getField.visitVarInsn(ALOAD, 1)

    //call this.field2.getField(var1)
    getField.visitMethodInsn(INVOKEINTERFACE, JvmOps.getRecordInterfaceType().name.toInternalName,
      "getField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object), true)

    //Emit ret label
    getField.visitLabel(ret)

    getField.visitInsn(ARETURN)

    getField.visitMaxs(1, 1)
    getField.visitEnd()
  }



  /**
    * This method generates code for the removeField method in the RecordExtend class. The method receives one argument, the field label.
    * The method should check if the current record label is equal to the provided label. If it is equal it should return the rest of the record
    * (field2).In case the provided label is not equal we recursively call removeField on the rest of the record.
    * Then we should set our 'rest' field(field2) to what was returned by the recursive call.
    * Because we might need to update our 'rest' pointer since if the provided label is equal to the next field label,
    * then this field should no longer be in the record. We then return 'this'.
    */
  def compileRecordExtendremoveField(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {

    val removeField = visitor.visitMethod(ACC_PUBLIC, "removeField",
      AsmOps.getMethodDescriptor(List(JvmType.String), JvmOps.getRecordInterfaceType()), null, null)

    removeField.visitCode()

    //Push "this" onto stack
    removeField.visitVarInsn(ALOAD, 0)

    //Push this.field0 onto the stack
    removeField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field0", JvmType.String.toDescriptor)

    //Push the function argument onto the stack
    removeField.visitVarInsn(ALOAD, 1)

    //Compare both strings on the stack using equals.
    removeField.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName,
      "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)

    //create new labels
    val falseCase = new Label
    val ret = new Label


    //if the strings are equal ...
    removeField.visitJumpInsn(IFEQ, falseCase)

    //true case
    //return this.field2

    //Load 'this' onto the stack
    removeField.visitVarInsn(ALOAD, 0)

    //Push this.field2 onto the stack
    removeField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field2", JvmOps.getRecordInterfaceType().toDescriptor)

    //Jump into the return label
    removeField.visitJumpInsn(GOTO,ret)

    //Emit false case label
    removeField.visitLabel(falseCase)

    //false case
    //this.field2 = this.field2.removeField(var1);
    //return this;


    //Load 'this' onto the stack
    removeField.visitVarInsn(ALOAD, 0)

    //Duplicate this as we need to set this.field2
    removeField.visitInsn(DUP)

    //Push this.field2 onto the stack
    removeField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field2", JvmOps.getRecordInterfaceType().toDescriptor)

    //Push var1 onto the stack
    removeField.visitVarInsn(ALOAD, 1)

    //call this.field2.removeField(var1)
    removeField.visitMethodInsn(INVOKEINTERFACE, JvmOps.getRecordInterfaceType().name.toInternalName,
      "removeField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmOps.getRecordInterfaceType()), true)



    //this.field2 = this.field2.removeField(var1);
    removeField.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "field2", JvmOps.getRecordInterfaceType().toDescriptor)

    //push this onto the stack in order to return it.
    removeField.visitVarInsn(ALOAD, 0)


    //Emit return label
    removeField.visitLabel(ret)

    removeField.visitInsn(ARETURN)

    removeField.visitMaxs(1, 1)
    removeField.visitEnd()
  }

}
