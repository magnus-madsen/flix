/*
 * Copyright 2019 Miguel Fialho
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{Finality, Instancing, Visibility}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

/**
  * Generates bytecode for the empty record class.
  */
object GenRecordEmpty {

  val InstanceFieldName: String = "INSTANCE"

  /**
    * Returns a Map with a single entry, for the empty record class.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(BackendObjType.RecordEmpty.jvmName -> JvmClass(BackendObjType.RecordEmpty.jvmName, genByteCode()))
  }

  /**
    * This method creates the class RecordEmpty that implements IRecord$.
    * It follows the singleton pattern.
    *
    * public static final RecordEmpty INSTANCE = new RecordEmpty();
    *
    * private RecordEmpty() { }
    *
    * public final IRecord$ lookupField(String var1) {
    * throw new UnsupportedOperationException("lookupField method shouldn't be called");
    * }
    *
    * public final IRecord$ restrictField(String var1) {
    * throw new UnsupportedOperationException("restrictField method shouldn't be called");
    * }
    */
  private def genByteCode()(implicit root: Root, flix: Flix): Array[Byte] = {
    val recordInterface = BackendObjType.RecordEmpty.interface
    val cm = ClassMaker.mkClass(BackendObjType.RecordEmpty.jvmName, Visibility.Public, Finality.Final, interfaces = List(recordInterface))

    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkObjectConstructor(Visibility.Private)
    cm.mkField(InstanceFieldName, BackendObjType.RecordEmpty.toTpe, Visibility.Public, Finality.Final, Instancing.Static)
    val stringToIRecord = mkDescriptor(BackendObjType.String.toTpe)(recordInterface.toObjTpe.toTpe)
    cm.mkMethod(genLookupFieldMethod(), GenRecordInterfaces.LookupFieldFunctionName, stringToIRecord, Visibility.Public, Finality.Final, Instancing.NonStatic)
    cm.mkMethod(genRestrictFieldMethod(), GenRecordInterfaces.RestrictFieldFunctionName, stringToIRecord, Visibility.Public, Finality.Final, Instancing.NonStatic)

    cm.closeClassMaker
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(BackendObjType.RecordEmpty.jvmName) ~
      DUP() ~
      invokeConstructor(BackendObjType.RecordEmpty.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTSTATIC(BackendObjType.RecordEmpty.jvmName, InstanceFieldName, BackendObjType.RecordEmpty.toTpe) ~
      RETURN()


  private def genLookupFieldMethod(): InstructionSet =
    NEW(JvmName.UnsupportedOperationException) ~
      DUP() ~
      pushString("lookupField method shouldn't be called") ~
      INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod, mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      ATHROW()

  private def genRestrictFieldMethod(): InstructionSet =
    NEW(JvmName.UnsupportedOperationException) ~
      DUP() ~
      pushString("restrictField method shouldn't be called") ~
      INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod, mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      ATHROW()
}
