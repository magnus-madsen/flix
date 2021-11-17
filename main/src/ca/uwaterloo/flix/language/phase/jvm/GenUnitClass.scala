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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Finality._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Instancing._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor

object GenUnitClass {

  val InstanceFieldName = "INSTANCE"

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    Map(BackendObjType.Unit.jvmName -> JvmClass(BackendObjType.Unit.jvmName, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(BackendObjType.Unit.jvmName, Public, Final)

    // Singleton instance
    cm.mkField(InstanceFieldName, BackendObjType.Unit.toTpe, Public, Final, Static)

    cm.mkStaticConstructor(genStaticConstructor())
    cm.mkObjectConstructor(Private)

    cm.closeClassMaker
  }

  private def genStaticConstructor(): InstructionSet =
    NEW(BackendObjType.Unit.jvmName) ~
      DUP() ~
      invokeConstructor(BackendObjType.Unit.jvmName) ~
      PUTSTATIC(BackendObjType.Unit.jvmName, InstanceFieldName, BackendObjType.Unit.toTpe) ~
      RETURN()
}
