/*
 * Copyright 2019 Miguel Fialho
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
package ca.uwaterloo.flix.language.phase.njvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.{AsmOps, JvmClass, JvmName, JvmOps, JvmType, NamespaceInfo}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Opcodes.{ALOAD, INVOKEVIRTUAL}

class Namespace(ns : NamespaceInfo)(implicit root: Root, flix : Flix) extends MnemonicsClass {
  //Setup
  private val ct: Reference = getNamespaceClassType(ns)
  private val cg: ClassGenerator = new ClassGenerator(ct, List())

  private val continuation : Field[Ref[MObject]] = cg.mkField("continuation")

  for ((sym, defn) <- ns.defs) {
    // JvmType of `defn`
    val fieldType = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)

    cg.mkUncheckedField(fieldName, fieldType)

    val methodName = getDefMethodNameInNamespaceClass(defn.sym)
    val MonoType.Arrow(targs, tresult) = defn.tpe

    // Erased argument and result type.
    val erasedArgs = targs map getErasedJvmType
    val contextConstructor = new VoidMethod1[Ref[Context]](JvmModifier.InvokeSpecial, NJvmType.Context, "<init>")
    // Length of args in local
    val stackSize = erasedArgs.map(getStackSize).sum
    val namespaceClassType = getNamespaceClassType(ns)

    // Address of continuation
    val contextLocal = new Local[Ref[Context]](stackSize)
    val ifoLocal = new Local[Ref[MObject]](stackSize + 1)
    val nsFieldName = getNamespaceFieldNameInContextClass(ns)
    val defnFieldName = getDefFieldNameInNamespaceClass(defn.sym)
    val nsField = new Field[Ref[Context]](nsFieldName)
    val nsIfoField = new UncheckedField(defnFieldName, namespaceClassType)
    val continuationField = new Field[Ref[MObject]]("continuation")
    val functionInterface = getFunctionInterfaceType(erasedArgs, getErasedJvmType(tresult))

    val continuationType = getErasedJvmType(tresult) match {
      case PrimBool => getContinuationInterfaceType[MBool]
      case PrimChar => getContinuationInterfaceType[MChar]
      case PrimByte => getContinuationInterfaceType[MByte]
      case PrimShort => getContinuationInterfaceType[MShort]
      case PrimInt => getContinuationInterfaceType[MInt]
      case PrimLong => getContinuationInterfaceType[MLong]
      case PrimFloat => getContinuationInterfaceType[MFloat]
      case PrimDouble => getContinuationInterfaceType[MDouble]
      case Reference(_) => getContinuationInterfaceType[Ref[MObject]]
      case _ => throw InternalCompilerException(s"Unexpected type " +  getErasedJvmType(tresult))
    }
    val invokeMethod = new UncheckedVoidMethod(InvokeInterface, continuationType, "invoke", List(NJvmType.Context))
    val getResultMethod = new UncheckedMethod(InvokeInterface, continuationType, "getResult", Nil, getErasedJvmType(tresult))
    // Compile the shim method.
    cg.mkUncheckedVoidMethod(methodName,
      sig =>
        NEW[StackNil, Ref[Context]](NJvmType.Context) |>>
        DUP |>>
        contextConstructor.INVOKE |>>
        DUP |>>
        contextLocal.STORE |>>
        nsField.GET_FIELD |>>
        nsIfoField.GET_FIELD[StackNil, Ref[Context], Ref[MObject]] |>>
        ifoLocal.STORE |>> {
          var ins = NO_OP[StackNil]
          // Offset for each parameter
          var offset: Int = 0

          // Set arguments for the IFO
          for ((arg, index) <- erasedArgs.zipWithIndex) {
            // Duplicate the IFO reference
            val argLocal = new UncheckedLocal(arg, offset)
            val ifoSetter = new UncheckedVoidMethod(JvmModifier.InvokeVirtual, fieldType,"setArg"+index, List(arg))
           ins = ins |>> ifoLocal.LOAD |>> argLocal.LOAD |>> ifoSetter.INVOKE
            // Incrementing the offset
            offset += getStackSize(arg)
          }
          ins} |>>
          contextLocal.LOAD |>>
          ifoLocal.LOAD |>>
          CHECK_CAST(functionInterface) |>>
          continuationField.PUT_FIELD |>>
          CONST_NULL |>>
//          ifoLocal.STORE |>>
//          IFNONNULL(
//            contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
//            continuationField.GET_FIELD[StackNil ** Ref[MObject] , Ref[Context]] |>>
//            contextLocal.LOAD[StackNil ** Ref[MObject]] |>>
//            CONST_NULL[StackNil** Ref[MObject], Ref[MObject]] |>>
//            continuationField.PUT_FIELD[StackNil** Ref[MObject], Ref[Context]] |>>
//            CHECK_CAST[StackNil](continuationType) |>>
//            DUP[StackNil, Ref[MObject]] |>>
//            ifoLocal.STORE[StackNil** Ref[MObject], Ref[MObject]] |>>
//            contextLocal.LOAD[[StackNil** Ref[MObject]] |>>
//            invokeMethod.INVOKE[StackNil** Ref[MObject], StackNil] |>>
//            continuationField.GET_FIELD[StackNil]
//          ) |>>
//          ifoLocal.LOAD |>>
//          getResultMethod.INVOKE |>>
          RETURN
      , erasedArgs, List(Public, Static, Final), true)
  }

  def getUncheckedField(sym:  Symbol.DefnSym): UncheckedField ={
    // JvmType of `defn`
    val fieldType = getFunctionDefinitionClassType(sym)

    // Name of the field on namespace
    val fieldName = getDefFieldNameInNamespaceClass(sym)
    new UncheckedField(fieldName, fieldType)
  }

  val defaultConstructor: VoidMethod1[Ref[Namespace]] =
    cg.mkConstructor1(
      sig =>
        sig.getArg1.LOAD[StackNil] |>>
          cg.SUPER |>> {
          var ins = NO_OP[StackNil]
          for ((sym, _) <- ns.defs) {

            val field = getUncheckedField(sym)
            val classType =  getFunctionDefinitionClassType(sym)
            val fieldConstructor  = new VoidMethod1[Ref[Namespace]](JvmModifier.InvokeSpecial, classType, "<init>")

            ins = ins |>>
              sig.getArg1.LOAD |>>
              NEW(classType) |>>
              DUP |>>
              fieldConstructor.INVOKE |>>
              field.PUT_FIELD
          }
          ins} |>>
          RETURN_VOID
    )

  /**
    * Variable which generates the JvmClass (contains the class bytecode)
    */
  private val jvmClass: JvmClass = JvmClass(ct.name, cg.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    ct.name -> this
}

