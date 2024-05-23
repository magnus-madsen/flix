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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.mkClassName
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch._
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{DevFlixRuntime, JavaLang, JavaLangInvoke, JavaUtil, JavaUtilConcurrent, MethodDescriptor, RootPackage}
import org.objectweb.asm.Opcodes

/**
  * Represents all Flix types that are objects on the JVM (array is an exception).
  */
sealed trait BackendObjType {
  /**
    * The `JvmName` that represents the type `Ref(Int)` refers to `"Ref$Int"`.
    */
  val jvmName: JvmName = this match {
    case BackendObjType.Unit => JvmName(DevFlixRuntime, mkClassName("Unit"))
    case BackendObjType.Lazy(tpe) => JvmName(RootPackage, mkClassName("Lazy", tpe))
    case BackendObjType.Ref(tpe) => JvmName(RootPackage, mkClassName("Ref", tpe))
    case BackendObjType.Tuple(elms) => JvmName(RootPackage, mkClassName("Tuple", elms))
    case BackendObjType.Tagged => JvmName(RootPackage, mkClassName("Tagged"))
    case BackendObjType.Tag(tpe) => JvmName(RootPackage, mkClassName("Tag", tpe))
    case BackendObjType.Arrow(args, result) => JvmName(RootPackage, mkClassName(s"Fn${args.length}", args :+ result))
    case BackendObjType.RecordEmpty => JvmName(RootPackage, mkClassName(s"RecordEmpty"))
    case BackendObjType.RecordExtend(value) => JvmName(RootPackage, mkClassName("RecordExtend", value))
    case BackendObjType.Record => JvmName(RootPackage, mkClassName("Record"))
    case BackendObjType.ReifiedSourceLocation => JvmName(DevFlixRuntime, mkClassName("ReifiedSourceLocation"))
    case BackendObjType.Global => JvmName(DevFlixRuntime, "Global") // "Global" is fixed in source code, so should not be mangled and $ suffixed
    case BackendObjType.FlixError => JvmName(DevFlixRuntime, mkClassName("FlixError"))
    case BackendObjType.HoleError => JvmName(DevFlixRuntime, mkClassName("HoleError"))
    case BackendObjType.MatchError => JvmName(DevFlixRuntime, mkClassName("MatchError"))
    case BackendObjType.MutationError => JvmName(DevFlixRuntime, mkClassName("MutationError"))
    case BackendObjType.EQMutantException => JvmName(DevFlixRuntime, mkClassName("EQMutantException"))
    case BackendObjType.UnhandledEffectError => JvmName(DevFlixRuntime, mkClassName("UnhandledEffectError"))
    case BackendObjType.Region => JvmName(DevFlixRuntime, mkClassName("Region"))
    case BackendObjType.UncaughtExceptionHandler => JvmName(DevFlixRuntime, mkClassName("UncaughtExceptionHandler"))
    case BackendObjType.Main(_) => JvmName.Main
    // Java classes
    case BackendObjType.Native(className) => className
    case BackendObjType.Regex => JvmName(List("java", "util", "regex"), "Pattern")
    case BackendObjType.BigDecimal => JvmName(List("java", "math"), "BigDecimal")
    case BackendObjType.BigInt => JvmName(List("java", "math"), "BigInteger")
    case BackendObjType.JavaObject => JvmName(JavaLang, "Object")
    case BackendObjType.String => JvmName(JavaLang, "String")
    case BackendObjType.CharSequence => JvmName(JavaLang, "CharSequence")
    case BackendObjType.Arrays => JvmName(JavaUtil, "Arrays")
    case BackendObjType.StringBuilder => JvmName(JavaLang, "StringBuilder")
    case BackendObjType.Objects => JvmName(JavaLang, "Objects")
    case BackendObjType.LambdaMetaFactory => JvmName(JavaLangInvoke, "LambdaMetafactory")
    case BackendObjType.LinkedList => JvmName(JavaUtil, "LinkedList")
    case BackendObjType.Iterator => JvmName(JavaUtil, "Iterator")
    case BackendObjType.Runnable => JvmName(JavaLang, "Runnable")
    case BackendObjType.ConcurrentLinkedQueue => JvmName(JavaUtilConcurrent, "ConcurrentLinkedQueue")
    case BackendObjType.Thread => JvmName(JavaLang, "Thread")
    case BackendObjType.ThreadBuilderOfVirtual => JvmName(JavaLang, "Thread$Builder$OfVirtual")
    case BackendObjType.ThreadUncaughtExceptionHandler => JvmName(JavaLang, "Thread$UncaughtExceptionHandler")
    case BackendObjType.ReentrantLock => JvmName.ReentrantLock
    // Effects Runtime
    case BackendObjType.Result => JvmName(DevFlixRuntime, mkClassName("Result"))
    case BackendObjType.Value => JvmName(DevFlixRuntime, mkClassName("Value"))
    case BackendObjType.Frame => JvmName(DevFlixRuntime, mkClassName("Frame"))
    case BackendObjType.Thunk => JvmName(DevFlixRuntime, mkClassName("Thunk"))
    case BackendObjType.Suspension => JvmName(DevFlixRuntime, mkClassName("Suspension"))
    case BackendObjType.Frames => JvmName(DevFlixRuntime, mkClassName("Frames"))
    case BackendObjType.FramesCons => JvmName(DevFlixRuntime, mkClassName("FramesCons"))
    case BackendObjType.FramesNil => JvmName(DevFlixRuntime, mkClassName("FramesNil"))
    case BackendObjType.Resumption => JvmName(DevFlixRuntime, mkClassName("Resumption"))
    case BackendObjType.ResumptionCons => JvmName(DevFlixRuntime, mkClassName("ResumptionCons"))
    case BackendObjType.ResumptionNil => JvmName(DevFlixRuntime, mkClassName("ResumptionNil"))
    case BackendObjType.Handler => JvmName(DevFlixRuntime, mkClassName("Handler"))
    case BackendObjType.EffectCall => JvmName(DevFlixRuntime, mkClassName("EffectCall"))
    case BackendObjType.ResumptionWrapper(t) => JvmName(DevFlixRuntime, mkClassName("ResumptionWrapper", t))
  }

  /**
    * The JVM type descriptor of the form `"L<jvmName.toInternalName>;"`.
    */
  def toDescriptor: String = jvmName.toDescriptor

  /**
    * Returns `this` wrapped in `BackendType.Reference`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this)

  protected def nullarySuperConstructor(superClass: ConstructorMethod): ConstructorMethod = ConstructorMethod(
    this.jvmName,
    IsPublic,
    Nil,
    Some(_ => thisLoad() ~ INVOKESPECIAL(superClass) ~ RETURN())
  )

  protected def singletonStaticConstructor(thisConstructor: ConstructorMethod, singleton: StaticField): StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(_ =>
    NEW(this.jvmName) ~
      DUP() ~ INVOKESPECIAL(thisConstructor) ~
      PUTSTATIC(singleton) ~
      RETURN()
  ))
}

object BackendObjType {

  private def mkClassName(prefix: String, tpe: BackendType): String = {
    JvmName.mkClassName(prefix, tpe.toErasedString)
  }

  private def mkClassName(prefix: String, tpes: List[BackendType]): String = {
    JvmName.mkClassName(prefix, tpes.map(_.toErasedString))
  }

  private def mkClassName(prefix: String): String = {
    JvmName.mkClassName(prefix)
  }

  case object Unit extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(SingletonField)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = singletonStaticConstructor(Constructor, SingletonField)

    def SingletonField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      pushString("()") ~ ARETURN()
    ))
  }

  case object BigDecimal extends BackendObjType

  case object BigInt extends BackendObjType

  case class Lazy(tpe: BackendType) extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor)
      cm.mkField(ExpField)
      cm.mkField(ValueField)
      cm.mkField(LockField)
      cm.mkMethod(ForceMethod)

      cm.closeClassMaker()
    }

    def ExpField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, IsVolatile, "expression", JavaObject.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", tpe)

    private def LockField: InstanceField = InstanceField(this.jvmName, IsPrivate, NotFinal, NotVolatile, "lock", ReentrantLock.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(JavaObject.toTpe), Some(_ => {
      withName(1, JavaObject.toTpe)(exp =>
        // super()
        thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
        // this.exp = exp
        thisLoad() ~ exp.load() ~ PUTFIELD(ExpField) ~
        // this.lock = new ReentrantLock()
        thisLoad() ~
        NEW(ReentrantLock.jvmName) ~ DUP() ~ INVOKESPECIAL(ReentrantLock.Constructor) ~
        PUTFIELD(LockField) ~
        // return
        RETURN()
      )
    }))

    def ForceMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "force", mkDescriptor()(tpe), Some(_ => {
      val unlockLock = thisLoad() ~ GETFIELD(LockField) ~ INVOKEVIRTUAL(ReentrantLock.UnlockMethod)
      thisLoad() ~ GETFIELD(LockField) ~ INVOKEVIRTUAL(ReentrantLock.LockInterruptiblyMethod) ~
      tryCatch{
        thisLoad() ~ GETFIELD(ExpField) ~
        // if the expression is not null, compute the value and erase the expression
        ifCondition(Condition.NONNULL)(
          thisLoad() ~
          // get expression as thunk
          DUP() ~ GETFIELD(ExpField) ~ CHECKCAST(Thunk.jvmName) ~
          // this.value = thunk.unwind()
          Result.unwindSuspensionFreeThunkToType(tpe, "during call to Lazy.force", SourceLocation.Unknown) ~ PUTFIELD(ValueField) ~
          // this.exp = null
          thisLoad() ~ pushNull() ~ PUTFIELD(ExpField)
        ) ~
        thisLoad() ~ GETFIELD(ValueField)
      }{
         // catch
         unlockLock ~ ATHROW()
      } ~
        unlockLock ~ xReturn(tpe)
    }))

  }

  case class Ref(tpe: BackendType) extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkField(ValueField)
      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", tpe)
  }

  case class Tuple(elms: List[BackendType]) extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      elms.indices.foreach(i => cm.mkField(IndexField(i)))
      cm.mkConstructor(Constructor)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def IndexField(i: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, s"field$i", elms(i))

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, elms, Some(_ => {
      withNames(1, elms){ case (_, variables) =>
        thisLoad() ~
        // super()
        DUP() ~ INVOKESPECIAL(JavaObject.Constructor) ~
        // this.field$i = var$j
        // fields are numbered consecutively while variables skip indices based
        // on their stack size
        composeN(variables.zipWithIndex.map{case (elm, i) =>
          DUP() ~ elm.load() ~ PUTFIELD(IndexField(i))
        }) ~
        RETURN()
      }
    }))

    def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ => {
      // [...] -> [..., "(v1, v2, ...)"]
      def commaSepElmString(): InstructionSet = {
        // new String[elms.length] // referred to as `elms`
        cheat(mv => GenExpression.compileInt(elms.length)(mv)) ~ ANEWARRAY(String.jvmName) ~
        ICONST_M1() ~ // running index referred to as `j`
        // current stack [elms, j]
        composeN(elms.indices.map(i => {
          val field = IndexField(i)
          // j = j + 1
          ICONST_1() ~ IADD() ~
          // [elms, j] -> [elms, j, elms, j]
          DUP2() ~
          // this.field$i.toString
          thisLoad() ~ GETFIELD(field) ~ xToString(field.tpe) ~
          // [elms, j, elms, j, string] -> [elms, j]
          AASTORE()
        })) ~
        POP() ~
        // [elms] -> [", ", elms]
        pushString(", ") ~ SWAP() ~
        INVOKESTATIC(String.JoinMethod)
      }
      // new String[3] // referred to as `arr`
      ICONST_3() ~ ANEWARRAY(String.jvmName) ~
      // arr[0] = "("
      DUP() ~ ICONST_0() ~ pushString("(") ~ AASTORE() ~
      // arr[1] = "(v1, v2, v3)"
      DUP() ~ ICONST_1() ~ commaSepElmString() ~ AASTORE() ~
      // arr[2] = ")"
      DUP() ~ ICONST_2() ~ pushString(")") ~ AASTORE() ~
      // ["", arr]
      pushString("") ~ SWAP() ~
      INVOKESTATIC(String.JoinMethod) ~
      xReturn(String.toTpe)
    }))

  }

  case object Tagged extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName)

      cm.mkConstructor(Constructor)

      cm.mkField(NameField)

      cm.closeClassMaker()
    }

    def NameField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "tag", String.toTpe)

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    /** [...] -> [..., tagName] */
    def mkTagName(sym: Symbol.CaseSym): InstructionSet = pushString(JvmOps.getTagName(sym))

    /** [..., tagName1, tagName2] --> [..., tagName1 == tagName2] */
    def eqTagName(): InstructionSet = {
      // ACMP is okay since tag strings are loaded through ldc instructions
      ifConditionElse(Condition.ACMPEQ)(pushBool(true))(pushBool(false))
    }
  }

  case class Tag(tpe: BackendType) extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = Tagged.jvmName)

      cm.mkConstructor(Constructor)
      cm.mkField(ValueField)
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def NameField: InstanceField = Tagged.NameField

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", tpe)

    def Constructor: ConstructorMethod = nullarySuperConstructor(Tagged.Constructor)

    def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ => {
      // new String[4] // referred to as `arr`
      ICONST_4() ~ ANEWARRAY(String.jvmName) ~
      // arr[0] = "Enum"
      DUP() ~ ICONST_0() ~ thisLoad() ~ GETFIELD(NameField) ~ AASTORE() ~
      // arr[1] = "("
      DUP() ~ ICONST_1() ~ pushString("(") ~ AASTORE() ~
      // arr[2] = this.value.toString()
      DUP() ~ ICONST_2() ~ thisLoad() ~ GETFIELD(ValueField) ~ xToString(ValueField.tpe) ~ AASTORE() ~
      // arr[3] = ")"
      DUP() ~ ICONST_3() ~ pushString(")") ~ AASTORE() ~
      // ["", arr]
      pushString("") ~ SWAP() ~
      INVOKESTATIC(String.JoinMethod) ~
      xReturn(String.toTpe)
    }))
  }

  case class Arrow(args: List[BackendType], result: BackendType) extends BackendObjType with Generatable {

    /**
      * Represents a function interface from `java.util.function`.
      */
    sealed trait FunctionInterface {
      /**
        * The JvmName of the interface.
        */
      def jvmName: JvmName = this match {
        case ObjFunction => JvmName.ObjFunction
        case ObjConsumer => JvmName.ObjConsumer
        case ObjPredicate => JvmName.ObjPredicate
        case IntFunction => JvmName.IntFunction
        case IntConsumer => JvmName.IntConsumer
        case IntPredicate => JvmName.IntPredicate
        case IntUnaryOperator => JvmName.IntUnaryOperator
        case LongFunction => JvmName.LongFunction
        case LongConsumer => JvmName.LongConsumer
        case LongPredicate => JvmName.LongPredicate
        case LongUnaryOperator => JvmName.LongUnaryOperator
        case DoubleFunction => JvmName.DoubleFunction
        case DoubleConsumer => JvmName.DoubleConsumer
        case DoublePredicate => JvmName.DoublePredicate
        case DoubleUnaryOperator => JvmName.DoubleUnaryOperator
      }

      /**
        * The required method of the interface.
        * These methods should do the same as a non-tail call in genExpression.
        */
      def functionMethod: InstanceMethod = this match {
        case ObjFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(JavaObject.toTpe)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ ARETURN()
          ))
        case ObjConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(JavaObject.toTpe)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ RETURN()
          ))
        case ObjPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(JavaObject.toTpe)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ALOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ IRETURN()
          ))
        case IntFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int32)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ ARETURN()
          ))
        case IntConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int32)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ RETURN()
          ))
        case IntPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int32)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ IRETURN()
          ))
        case IntUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsInt",
          mkDescriptor(BackendType.Int32)(BackendType.Int32),
          Some(_ =>
            thisLoad() ~
              DUP() ~ ILOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Int32, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ IRETURN()
          ))
        case LongFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Int64)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ ARETURN()
          ))
        case LongConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Int64)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ RETURN()
          ))
        case LongPredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Int64)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ IRETURN()
          ))
        case LongUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsLong",
          mkDescriptor(BackendType.Int64)(BackendType.Int64),
          Some(_ =>
            thisLoad() ~
              DUP() ~ LLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Int64, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ LRETURN()
          ))
        case DoubleFunction => InstanceMethod(this.jvmName, IsPublic, IsFinal, "apply",
          mkDescriptor(BackendType.Float64)(JavaObject.toTpe),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ ARETURN()
          ))
        case DoubleConsumer => InstanceMethod(this.jvmName, IsPublic, IsFinal, "accept",
          mkDescriptor(BackendType.Float64)(VoidableType.Void),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(JavaObject.toTpe, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ RETURN()
          ))
        case DoublePredicate => InstanceMethod(this.jvmName, IsPublic, IsFinal, "test",
          mkDescriptor(BackendType.Float64)(BackendType.Bool),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Bool, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ IRETURN()
          ))
        case DoubleUnaryOperator => InstanceMethod(this.jvmName, IsPublic, IsFinal, "applyAsDouble",
          mkDescriptor(BackendType.Float64)(BackendType.Float64),
          Some(_ =>
            thisLoad() ~
              DUP() ~ DLOAD(1) ~ PUTFIELD(ArgField(0)) ~
              Result.unwindSuspensionFreeThunkToType(BackendType.Float64, s"in ${jvmName.toBinaryName}", SourceLocation.Unknown) ~ DRETURN()
          ))

      }
    }

    // JavaObject -> JavaObject
    case object ObjFunction extends FunctionInterface

    // JavaObject -> Unit
    case object ObjConsumer extends FunctionInterface

    // JavaObject -> Bool
    case object ObjPredicate extends FunctionInterface

    // Int32 -> JavaObject
    case object IntFunction extends FunctionInterface

    // Int32 -> Unit
    case object IntConsumer extends FunctionInterface

    // Int32 -> Bool
    case object IntPredicate extends FunctionInterface

    // Int32 -> Int32
    case object IntUnaryOperator extends FunctionInterface

    // Int64 -> JavaObject
    case object LongFunction extends FunctionInterface

    // Int64 -> Unit
    case object LongConsumer extends FunctionInterface

    // Int64 -> Bool
    case object LongPredicate extends FunctionInterface

    // Int64 -> Int64
    case object LongUnaryOperator extends FunctionInterface

    // Float64 -> JavaObject
    case object DoubleFunction extends FunctionInterface

    // Float64 -> Unit
    case object DoubleConsumer extends FunctionInterface

    // Float64 -> Bool
    case object DoublePredicate extends FunctionInterface

    // Float64 -> Float64
    case object DoubleUnaryOperator extends FunctionInterface

    /**
      * Returns the specialized java function interfaces of the function type.
      */
    def specialization(): List[FunctionInterface] = {
      (args, result) match {
        case (BackendType.Reference(BackendObjType.JavaObject) :: Nil, _) =>
          ObjFunction :: ObjConsumer :: ObjPredicate :: Nil
        case (BackendType.Int32 :: Nil, _) =>
          IntFunction :: IntConsumer :: IntPredicate :: IntUnaryOperator :: Nil
        case (BackendType.Int64 :: Nil, _) =>
          LongFunction :: LongConsumer :: LongPredicate :: LongUnaryOperator :: Nil
        case (BackendType.Float64 :: Nil, _) =>
          DoubleFunction :: DoubleConsumer :: DoublePredicate :: DoubleUnaryOperator :: Nil
        case _ => Nil
      }
    }

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val specializedInterface = specialization()
      val interfaces = Thunk.jvmName :: specializedInterface.map(_.jvmName)

      val cm = ClassMaker.mkAbstractClass(this.jvmName, superClass = JavaObject.jvmName, interfaces)

      cm.mkConstructor(Constructor)
      args.indices.foreach(argIndex => cm.mkField(ArgField(argIndex)))
      specializedInterface.foreach(i => cm.mkMethod(i.functionMethod))
      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def ArgField(index: Int): InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, s"arg$index", args(index))

    def ToStringMethod: InstanceMethod = {
      val argString = args match {
        case Nil => "()"
        case arg :: Nil => arg.toErasedString
        case _ => args.map(_.toErasedString).mkString("(", ", ", ")")
      }
      JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
        pushString(s"$argString -> ${result.toErasedString}") ~
          ARETURN()
      ))
    }
  }

  case object RecordEmpty extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(this.interface.jvmName))

      cm.mkStaticConstructor(StaticConstructor)
      cm.mkConstructor(Constructor)
      cm.mkField(SingletonField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)
      cm.mkMethod(ToStringMethod)
      cm.mkMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = singletonStaticConstructor(Constructor, SingletonField)

    def interface: Record.type = Record

    def SingletonField: StaticField = StaticField(this.jvmName, IsPublic, IsFinal, NotVolatile, "INSTANCE", this.toTpe)

    def LookupFieldMethod: InstanceMethod = interface.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      throwUnsupportedOperationException(
        s"${Record.LookupFieldMethod.name} method shouldn't be called")
    ))

    def RestrictFieldMethod: InstanceMethod = interface.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      throwUnsupportedOperationException(
        s"${Record.RestrictFieldMethod.name} method shouldn't be called")
    ))

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      pushString("{}") ~ ARETURN()
    ))

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, StringBuilder.toTpe) { sb =>
        sb.load() ~ pushString("}") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ ARETURN()
      }
    ))
  }

  case class RecordExtend(value: BackendType) extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, interfaces = List(Record.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkField(LabelField)
      cm.mkField(ValueField)
      cm.mkField(RestField)
      cm.mkMethod(LookupFieldMethod)
      cm.mkMethod(RestrictFieldMethod)
      cm.mkMethod(ToStringMethod)
      cm.mkMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def LabelField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "label", String.toTpe)

    def ValueField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "value", value)

    def RestField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "rest", Record.toTpe)

    def LookupFieldMethod: InstanceMethod = Record.LookupFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ ARETURN()
        case FalseBranch =>
          thisLoad() ~ GETFIELD(RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(Record.LookupFieldMethod) ~
            ARETURN()
      }
    ))

    def RestrictFieldMethod: InstanceMethod = Record.RestrictFieldMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      caseOnLabelEquality {
        case TrueBranch =>
          thisLoad() ~ GETFIELD(RestField) ~ ARETURN()
        case FalseBranch =>
          NEW(this.jvmName) ~ DUP() ~ INVOKESPECIAL(this.Constructor) ~
            DUP() ~ thisLoad() ~ GETFIELD(LabelField) ~ PUTFIELD(LabelField) ~
            DUP() ~ thisLoad() ~ GETFIELD(ValueField) ~ PUTFIELD(ValueField) ~
            DUP() ~ // get the new restricted rest to put
            thisLoad() ~ GETFIELD(RestField) ~
            ALOAD(1) ~
            INVOKEINTERFACE(Record.RestrictFieldMethod) ~
            PUTFIELD(RestField) ~ // put the rest field and return
            ARETURN()
      }
    ))

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      // save the `rest` for the last recursive call
      thisLoad() ~ GETFIELD(this.RestField) ~
        // build this segment of the string
        NEW(StringBuilder.jvmName) ~ DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("{") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(this.LabelField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        pushString(" = ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(this.ValueField) ~ xToString(this.ValueField.tpe) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEINTERFACE(Record.ToTailStringMethod) ~ ARETURN()
    ))

    private def ToTailStringMethod: InstanceMethod = Record.ToTailStringMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, StringBuilder.toTpe) { sb =>
        // save the `rest` for the last recursive call
        thisLoad() ~ GETFIELD(this.RestField) ~
          // build this segment of the string
          sb.load() ~ pushString(", ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          thisLoad() ~ GETFIELD(this.LabelField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          pushString(" = ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          thisLoad() ~ GETFIELD(this.ValueField) ~ xToString(this.ValueField.tpe) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
          // call the tailString of `rest`
          INVOKEINTERFACE(Record.ToTailStringMethod) ~ ARETURN()

      }
    ))

    /**
      * Compares the label of `this`and `ALOAD(1)` and executes the designated branch.
      */
    private def caseOnLabelEquality(cases: Branch => InstructionSet): InstructionSet =
      thisLoad() ~ GETFIELD(LabelField) ~
        ALOAD(1) ~
        INVOKEVIRTUAL(JavaObject.EqualsMethod) ~
        branch(Condition.Bool)(cases)
  }

  case object Record extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkInterface(this.jvmName)

      cm.mkInterfaceMethod(LookupFieldMethod)
      cm.mkInterfaceMethod(RestrictFieldMethod)
      cm.mkInterfaceMethod(ToTailStringMethod)

      cm.closeClassMaker()
    }

    def LookupFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "lookupField",
      mkDescriptor(String.toTpe)(this.toTpe))

    def RestrictFieldMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "restrictField",
      mkDescriptor(String.toTpe)(this.toTpe))

    def ToTailStringMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "toTailString",
      mkDescriptor(StringBuilder.toTpe)(String.toTpe))
  }

  /**
    * Represents a JVM type not represented in BackendObjType.
    * This should not be used for `java.lang.String` for example since `BackendObjType.String`
    * represents this type.
    */
  case class Native(className: JvmName) extends BackendObjType

  case object ReifiedSourceLocation extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor)

      cm.mkField(SourceField)
      cm.mkField(BeginLineField)
      cm.mkField(BeginColField)
      cm.mkField(EndLineField)
      cm.mkField(EndColField)

      cm.mkMethod(ToStringMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic,
      List(String.toTpe, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32), Some(_ =>
        thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
          thisLoad() ~ ALOAD(1) ~ PUTFIELD(SourceField) ~
          thisLoad() ~ ILOAD(2) ~ PUTFIELD(BeginLineField) ~
          thisLoad() ~ ILOAD(3) ~ PUTFIELD(BeginColField) ~
          thisLoad() ~ ILOAD(4) ~ PUTFIELD(EndLineField) ~
          thisLoad() ~ ILOAD(5) ~ PUTFIELD(EndColField) ~
          RETURN()
      ))

    def SourceField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "source", String.toTpe)

    def BeginLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "beginLine", BackendType.Int32)

    def BeginColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "beginCol", BackendType.Int32)

    def EndLineField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "endLine", BackendType.Int32)

    def EndColField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "endCol", BackendType.Int32)

    private def ToStringMethod: InstanceMethod = JavaObject.ToStringMethod.implementation(this.jvmName, Some(_ =>
      // create string builder
      NEW(StringBuilder.jvmName) ~ DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        // build string
        thisLoad() ~ GETFIELD(SourceField) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        pushString(":") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(BeginLineField) ~ INVOKEVIRTUAL(StringBuilder.AppendInt32Method) ~
        pushString(":") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        thisLoad() ~ GETFIELD(BeginColField) ~ INVOKEVIRTUAL(StringBuilder.AppendInt32Method) ~
        // create the string
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ ARETURN()
    ))
  }

  case object Global extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkConstructor(Constructor)
      cm.mkStaticConstructor(StaticConstructor)

      cm.mkField(StepCounterField)
      cm.mkField(InfiniteLoopException)
      cm.mkStaticMethod(DecAndCheckMethod)

      cm.mkField(EquivalentMutantException)
      cm.mkStaticMethod(ThrowEquivalentMutantMethod)

      cm.mkField(CounterField)
      cm.mkStaticMethod(NewIdMethod)

      cm.mkField(ArgsField)
      cm.mkStaticMethod(GetArgsMethod)
      cm.mkStaticMethod(SetArgsMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def StaticConstructor: StaticConstructorMethod = StaticConstructorMethod(this.jvmName, Some(_ =>
        NEW(JvmName.AtomicLong) ~
        DUP() ~ invokeConstructor(JvmName.AtomicLong, MethodDescriptor.NothingToVoid) ~
        PUTSTATIC(CounterField) ~

        NEW(JvmName.AtomicLong) ~
        DUP() ~
        cheat(mv => mv.visitLdcInsn(65_536L))~
        invokeConstructor(JvmName.AtomicLong, MethodDescriptor.mkDescriptor(BackendType.Int64)(VoidableType.Void)) ~
        PUTSTATIC(StepCounterField) ~

        ACONST_NULL() ~
        DUP() ~
        PUTSTATIC(InfiniteLoopException) ~

        ACONST_NULL() ~
        DUP() ~
        PUTSTATIC(EquivalentMutantException) ~

        ICONST_0() ~
        ANEWARRAY(String.jvmName) ~
        PUTSTATIC(ArgsField) ~
        RETURN()
    ))

    def NewIdMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "newId",
      mkDescriptor()(BackendType.Int64), Some(_ =>
        GETSTATIC(CounterField) ~
          INVOKEVIRTUAL(JvmName.AtomicLong, "getAndIncrement",
            MethodDescriptor(Nil, BackendType.Int64)) ~
          LRETURN()
      ))

    def ThrowEquivalentMutantMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "throwEquivalentMutant",
      mkDescriptor()(VoidableType.Void), Some(_ =>
            NEW(BackendObjType.EQMutantException.jvmName) ~
            DUP() ~
            INVOKESPECIAL(EQMutantException.Constructor) ~
            ATHROW() ~
          RETURN()
      ))

    def DecAndCheckMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "decAndCheck",
      mkDescriptor()(BackendType.Int64), Some(_ =>
          GETSTATIC(StepCounterField) ~
          INVOKEVIRTUAL(JvmName.AtomicLong, "getAndDecrement",
            MethodDescriptor(Nil, BackendType.Int64)) ~
          LCONST_0() ~
          LCMP() ~
          ifCondition(Condition.LT) {
            NEW(BackendObjType.MutationError.jvmName) ~
            DUP() ~
            INVOKESPECIAL(MutationError.Constructor) ~
            ATHROW()
          } ~
          LCONST_0() ~
          LRETURN()
        ))

    def GetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "getArgs",
      mkDescriptor()(BackendType.Array(String.toTpe)), Some(_ =>
        GETSTATIC(ArgsField) ~ ARRAYLENGTH() ~ ANEWARRAY(String.jvmName) ~ ASTORE(0) ~
          // the new array is now created, now to copy the args
          GETSTATIC(ArgsField) ~
          ICONST_0() ~
          ALOAD(0) ~
          ICONST_0() ~
          GETSTATIC(ArgsField) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(0) ~ ARETURN()
      ))

    def SetArgsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "setArgs",
      mkDescriptor(BackendType.Array(String.toTpe))(VoidableType.Void), Some(_ =>
        ALOAD(0) ~ ARRAYLENGTH() ~ ANEWARRAY(String.jvmName) ~ ASTORE(1) ~
          ALOAD(0) ~
          ICONST_0() ~
          ALOAD(1) ~
          ICONST_0() ~
          ALOAD(0) ~ ARRAYLENGTH() ~
          arrayCopy() ~
          ALOAD(1) ~ PUTSTATIC(ArgsField) ~ RETURN()
      ))

    def CounterField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "counter", JvmName.AtomicLong.toTpe)

    def InfiniteLoopException: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "infiniteLoopException", JvmName.Throwable.toTpe)

    def EquivalentMutantException: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "EquivalentMutantException", JvmName.Throwable.toTpe)

    def StepCounterField: StaticField =
      StaticField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "stepCounter", JvmName.AtomicLong.toTpe)

    def ArgsField: StaticField =
      StaticField(this.jvmName, IsPrivate, NotFinal, NotVolatile, "args", BackendType.Array(String.toTpe))

    private def arrayCopy(): InstructionSet = (f: F) => {
      f.visitMethodInstruction(Opcodes.INVOKESTATIC, JvmName.System, "arraycopy",
        MethodDescriptor(List(JavaObject.toTpe, BackendType.Int32, JavaObject.toTpe, BackendType.Int32,
          BackendType.Int32), VoidableType.Void), isInterface = false)
      f
    }
  }

  case object Regex extends BackendObjType

  case object FlixError extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkAbstractClass(this.jvmName, JvmName.Error)

      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(String.toTpe), Some(_ =>
      thisLoad() ~
        ALOAD(1) ~
        invokeConstructor(JvmName.Error, mkDescriptor(String.toTpe)(VoidableType.Void)) ~
        RETURN()
    ))
  }

  case object HoleError extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, FlixError.jvmName)

      cm.mkConstructor(Constructor)
      // These fields allow external equality checking.
      cm.mkField(HoleField)
      cm.mkField(LocationField)

      cm.closeClassMaker()
    }

    private def HoleField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "hole", String.toTpe)

    private def LocationField: InstanceField =
      InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic,
      List(String.toTpe, ReifiedSourceLocation.toTpe), Some(_ =>
        withName(1, String.toTpe) { hole =>
          withName(2, ReifiedSourceLocation.toTpe) { loc =>
            thisLoad() ~
              // create an error msg
              NEW(StringBuilder.jvmName) ~
              DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
              pushString("Hole '") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              hole.load() ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              pushString("' at ") ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              loc.load() ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
              INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
              INVOKESPECIAL(FlixError.Constructor) ~
              // save the arguments locally
              thisLoad() ~ hole.load() ~ PUTFIELD(HoleField) ~
              thisLoad() ~ loc.load() ~ PUTFIELD(LocationField) ~
              RETURN()
          }
        }
      ))
  }

  case object MatchError extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(MatchError.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)
      // This field allows external equality checking.
      cm.mkField(LocationField)

      cm.closeClassMaker()
    }

    def LocationField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(MatchError.jvmName, IsPublic, List(ReifiedSourceLocation.toTpe), Some(_ =>
      thisLoad() ~
        NEW(StringBuilder.jvmName) ~
        DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("Non-exhaustive match at ") ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        ALOAD(1) ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        // save argument locally
        thisLoad() ~
        ALOAD(1) ~
        PUTFIELD(this.LocationField) ~
        RETURN()
    ))
  }

  case object EQMutantException extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(EQMutantException.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }
    def Constructor: ConstructorMethod = ConstructorMethod(EQMutantException.jvmName, IsPublic, Nil, Some(_ =>
      thisLoad() ~
        NEW(StringBuilder.jvmName) ~
        DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("An equivalent mutant was run") ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        // save argument locally
        RETURN()
    ))
  }
  case object MutationError extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(MutationError.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)

      cm.closeClassMaker()
    }
    def Constructor: ConstructorMethod = ConstructorMethod(MutationError.jvmName, IsPublic, Nil, Some(_ =>
      thisLoad() ~
        NEW(StringBuilder.jvmName) ~
        DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("Resource ran out, cannot determine if method will terminate") ~
        INVOKEVIRTUAL(StringBuilder.AppendStringMethod) ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        // save argument locally
        RETURN()
    ))
  }

  case object UnhandledEffectError extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal, superClass = FlixError.jvmName)

      cm.mkConstructor(Constructor)
      // This field allows external equality checking.
      cm.mkField(EffectNameField)
      cm.mkField(LocationField)

      cm.closeClassMaker()
    }

    def EffectNameField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "effectName", String.toTpe)

    def LocationField: InstanceField = InstanceField(this.jvmName, IsPublic, IsFinal, NotVolatile, "location", ReifiedSourceLocation.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(Suspension.toTpe, String.toTpe, ReifiedSourceLocation.toTpe), Some(_ =>
      withName(1, Suspension.toTpe)(suspension => withName(2, String.toTpe)(info => withName(3, ReifiedSourceLocation.toTpe)(loc => {
        val appendString = INVOKEVIRTUAL(StringBuilder.AppendStringMethod)
        thisLoad() ~
        NEW(StringBuilder.jvmName) ~
        DUP() ~ INVOKESPECIAL(StringBuilder.Constructor) ~
        pushString("Unhandled effect '") ~ appendString ~
        suspension.load() ~ GETFIELD(Suspension.EffSymField) ~ appendString ~
        pushString("' (") ~ appendString ~
        info.load() ~ appendString ~
        pushString(") at ") ~ appendString ~
        loc.load() ~ INVOKEVIRTUAL(JavaObject.ToStringMethod) ~ appendString ~
        INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
        INVOKESPECIAL(FlixError.Constructor) ~
        // save arguments locally
        thisLoad() ~
        suspension.load() ~ GETFIELD(Suspension.EffSymField) ~
        PUTFIELD(EffectNameField) ~
        thisLoad() ~
        loc.load() ~
        PUTFIELD(LocationField) ~
        RETURN()
      })))
    ))

  }


  case object Region extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal)

      cm.mkField(ThreadsField)
      cm.mkField(RegionThreadField)
      cm.mkField(ChildExceptionField)
      cm.mkField(OnExitField)

      cm.mkConstructor(Constructor)

      cm.mkMethod(SpawnMethod)
      cm.mkMethod(ExitMethod)
      cm.mkMethod(ReportChildExceptionMethod)
      cm.mkMethod(ReThrowChildExceptionMethod)
      cm.mkMethod(RunOnExitMethod)

      cm.closeClassMaker()
    }

    // private final ConcurrentLinkedQueue<Thread> threads = new ConcurrentLinkedQueue<Thread>();
    def ThreadsField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "threads", BackendObjType.ConcurrentLinkedQueue.toTpe)

    // private final LinkedList<Runnable> onExit = new LinkedList<Runnable>();
    def OnExitField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "onExit", BackendObjType.LinkedList.toTpe)

    // private final Thread regionThread = Thread.currentThread();
    def RegionThreadField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "regionThread", JvmName.Thread.toTpe)

    // private volatile Throwable childException = null;
    def ChildExceptionField: InstanceField = InstanceField(this.jvmName, IsPrivate, NotFinal, IsVolatile, "childException", JvmName.Throwable.toTpe)

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, Some(_ =>
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
      thisLoad() ~ NEW(BackendObjType.ConcurrentLinkedQueue.jvmName) ~
      DUP() ~ invokeConstructor(BackendObjType.ConcurrentLinkedQueue.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTFIELD(ThreadsField) ~
      thisLoad() ~ INVOKESTATIC(Thread.CurrentThreadMethod) ~
      PUTFIELD(RegionThreadField) ~
      thisLoad() ~ ACONST_NULL() ~
      PUTFIELD(ChildExceptionField) ~
      thisLoad() ~ NEW(BackendObjType.LinkedList.jvmName) ~
      DUP() ~ invokeConstructor(BackendObjType.LinkedList.jvmName, MethodDescriptor.NothingToVoid) ~
      PUTFIELD(OnExitField) ~
      RETURN()
    ))

    // final public void spawn(Runnable r) {
    //   Thread t = new Thread(r);
    //   t.setUncaughtExceptionHandler(new UncaughtExceptionHandler(this));
    //   t.start();
    //   threads.add(t);
    // }
    def SpawnMethod(implicit flix: Flix): InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "spawn", mkDescriptor(JvmName.Runnable.toTpe)(VoidableType.Void), Some(_ =>
      INVOKESTATIC(Thread.OfVirtualMethod) ~ ALOAD(1) ~ INVOKEINTERFACE(ThreadBuilderOfVirtual.UnstartedMethod) ~
      storeWithName(2, BackendObjType.Thread.toTpe) { thread =>
        thread.load() ~ NEW(BackendObjType.UncaughtExceptionHandler.jvmName) ~
        DUP() ~ thisLoad() ~
        invokeConstructor(BackendObjType.UncaughtExceptionHandler.jvmName, mkDescriptor(BackendObjType.Region.toTpe)(VoidableType.Void)) ~
        INVOKEVIRTUAL(Thread.SetUncaughtExceptionHandlerMethod) ~
        thread.load() ~ INVOKEVIRTUAL(Thread.StartMethod) ~
        thisLoad() ~ GETFIELD(ThreadsField) ~ thread.load() ~
        INVOKEVIRTUAL(ConcurrentLinkedQueue.AddMethod) ~ POP() ~
        RETURN()
      }
    ))

    // final public void exit() throws InterruptedException {
    //   Thread t;
    //   while ((t = threads.poll()) != null)
    //     t.join();
    //   for (Runnable r: onExit)
    //     r.run();
    // }
    def ExitMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "exit", MethodDescriptor.NothingToVoid, Some(_ =>
      withName(1, BackendObjType.Thread.toTpe) { t =>
        whileLoop(Condition.NONNULL) {
          thisLoad() ~ GETFIELD(ThreadsField) ~
          INVOKEVIRTUAL(ConcurrentLinkedQueue.PollMethod) ~
          CHECKCAST(BackendObjType.Thread.jvmName) ~ DUP() ~ t.store()
        } {
          t.load() ~ INVOKEVIRTUAL(Thread.JoinMethod)
        } ~
        withName(2, BackendObjType.Iterator.toTpe) { i =>
          thisLoad() ~ GETFIELD(OnExitField) ~
          INVOKEVIRTUAL(LinkedList.IteratorMethod) ~
          i.store() ~
          whileLoop(Condition.NE) {
            i.load() ~ INVOKEINTERFACE(Iterator.HasNextMethod)
          } {
            i.load() ~ INVOKEINTERFACE(Iterator.NextMethod) ~
            CHECKCAST(Runnable.jvmName) ~
            INVOKEINTERFACE(Runnable.RunMethod)
          }
        } ~
        RETURN()
      }
    ))

    // final public void reportChildException(Throwable e) {
    //   childException = e;
    //   regionThread.interrupt();
    // }
    def ReportChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reportChildException", mkDescriptor(JvmName.Throwable.toTpe)(VoidableType.Void), Some(_ =>
      thisLoad() ~ ALOAD(1) ~
      PUTFIELD(ChildExceptionField) ~
      thisLoad() ~ GETFIELD(RegionThreadField) ~
      INVOKEVIRTUAL(Thread.InterruptMethod) ~
      RETURN()
    ))

    // final public void reThrowChildException() throws Throwable {
    //   if (childException != null)
    //     throw childException;
    // }
    def ReThrowChildExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "reThrowChildException", MethodDescriptor.NothingToVoid, Some(_ =>
      thisLoad() ~ GETFIELD(ChildExceptionField) ~
      ifCondition(Condition.NONNULL) {
        thisLoad() ~ GETFIELD(ChildExceptionField) ~
        ATHROW()
      } ~
      RETURN()
    ))

    // final public void runOnExit(Runnable r) {
    //   onExit.addFirst(r);
    // }
    def RunOnExitMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "runOnExit", mkDescriptor(BackendObjType.Runnable.toTpe)(VoidableType.Void), Some(_ =>
      thisLoad() ~ GETFIELD(OnExitField) ~ ALOAD(1) ~
      INVOKEVIRTUAL(LinkedList.AddFirstMethod) ~
      RETURN()
    ))
  }

  case object UncaughtExceptionHandler extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(ThreadUncaughtExceptionHandler.jvmName))

      cm.mkField(RegionField)
      cm.mkConstructor(Constructor)
      cm.mkMethod(UncaughtExceptionMethod)

      cm.closeClassMaker()
    }

    // private final Region r;
    def RegionField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "r", BackendObjType.Region.toTpe)

    // UncaughtExceptionHandler(Region r) { this.r = r; }
    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, BackendObjType.Region.toTpe :: Nil, Some(_ =>
      thisLoad() ~ INVOKESPECIAL(JavaObject.Constructor) ~
      thisLoad() ~ ALOAD(1) ~ PUTFIELD(RegionField) ~
      RETURN()
    ))

    // public void uncaughtException(Thread t, Throwable e) { r.reportChildException(e); }
    def UncaughtExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "uncaughtException", ThreadUncaughtExceptionHandler.UncaughtExceptionMethod.d, Some(_ =>
      thisLoad() ~ GETFIELD(RegionField) ~
      ALOAD(2) ~ INVOKEVIRTUAL(Region.ReportChildExceptionMethod) ~
      RETURN()
    ))
  }

  case class Main(sym: Symbol.DefnSym) extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = ClassMaker.mkClass(this.jvmName, IsFinal)

      cm.mkStaticMethod(MainMethod)

      cm.closeClassMaker()
    }

    def MainMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, NotFinal, "main", mkDescriptor(BackendType.Array(String.toTpe))(VoidableType.Void), Some(_ => {
      val defName = JvmOps.getFunctionDefinitionClassType(sym).name
      withName(0, BackendType.Array(String.toTpe))(args =>
        args.load() ~ INVOKESTATIC(Global.SetArgsMethod) ~
        NEW(defName) ~ DUP() ~ INVOKESPECIAL(defName, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid) ~
        DUP() ~ GETSTATIC(Unit.SingletonField) ~ PUTFIELD(InstanceField(defName, IsPublic, NotFinal, NotVolatile, "arg0", JavaObject.toTpe)) ~
        Result.unwindSuspensionFreeThunk(s"in ${this.jvmName.toBinaryName}", SourceLocation.Unknown) ~
        POP() ~ RETURN()
      )
    }))

  }

  //
  // Java Types
  //

  case object String extends BackendObjType {

    def JoinMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, NotFinal,
      "join", mkDescriptor(CharSequence.toTpe, BackendType.Array(CharSequence.toTpe))(String.toTpe), None)

    def BoolValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Bool)(this.jvmName.toTpe), None)

    def CharValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Char)(this.jvmName.toTpe), None)

    // implicit use of Int8 as Int32
    def Int8ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    // implicit use of Int16 as Int32
    def Int16ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    def Int32ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int32)(this.jvmName.toTpe), None)

    def Int64ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Int64)(this.jvmName.toTpe), None)

    def Float32ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Float32)(this.jvmName.toTpe), None)

    def Float64ValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendType.Float64)(this.jvmName.toTpe), None)

    def ObjectValueOf: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "valueOf", mkDescriptor(BackendObjType.JavaObject.toTpe)(this.jvmName.toTpe), None)
  }

  case object CharSequence extends BackendObjType

  case object Arrays extends BackendObjType {
    def BoolArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Bool))(BackendObjType.String.toTpe), None)

    def CharArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Char))(BackendObjType.String.toTpe), None)

    def Int8ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int8))(BackendObjType.String.toTpe), None)

    def Int16ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int16))(BackendObjType.String.toTpe), None)

    def Int32ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int32))(BackendObjType.String.toTpe), None)

    def Int64ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Int64))(BackendObjType.String.toTpe), None)

    def Float32ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Float32))(BackendObjType.String.toTpe), None)

    def Float64ArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendType.Float64))(BackendObjType.String.toTpe), None)

    def ObjArrToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "toString", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendObjType.String.toTpe), None)

    def DeepToString: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal,
      "deepToString", mkDescriptor(BackendType.Array(BackendObjType.JavaObject.toTpe))(BackendObjType.String.toTpe), None)
  }

  case object JavaObject extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def EqualsMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "equals",
      mkDescriptor(JavaObject.toTpe)(BackendType.Bool), None)

    def HashcodeMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "hashCode",
      mkDescriptor()(BackendType.Int32), None)

    def ToStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "toString",
      mkDescriptor()(String.toTpe), None)

    def GetClassMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "getClass",
      mkDescriptor()(JvmName.Class.toTpe), None)
  }

  case object StringBuilder extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def AppendStringMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "append",
      mkDescriptor(String.toTpe)(StringBuilder.toTpe), None)

    def AppendInt32Method: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "append",
      mkDescriptor(BackendType.Int32)(StringBuilder.toTpe), None)

  }

  case object Objects extends BackendObjType {

    def EqualsMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "equals",
      mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool), None)

    def HashMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "hash",
      mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32), None)

  }

  case object LambdaMetaFactory extends BackendObjType {
    private def methodHandlesLookup: BackendType = JvmName(List("java", "lang", "invoke"), "MethodHandles$Lookup").toTpe

    private def methodType: BackendType = JvmName(List("java", "lang", "invoke"), "MethodType").toTpe

    private def methodHandle: BackendType = JvmName(List("java", "lang", "invoke"), "MethodHandle").toTpe

    private def callSite: BackendType = JvmName(List("java", "lang", "invoke"), "CallSite").toTpe

    def MetaFactoryMethod: StaticMethod = StaticMethod(
      this.jvmName, IsPublic, IsFinal, "metafactory",
      mkDescriptor(methodHandlesLookup, String.toTpe, methodType, methodType, methodHandle, methodType)(callSite),
      None
    )
  }

  case object LinkedList extends BackendObjType {

    def AddFirstMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "addFirst",
      mkDescriptor(JavaObject.toTpe)(VoidableType.Void), None)

    def IteratorMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "iterator",
      mkDescriptor()(BackendObjType.Iterator.toTpe), None)
  }

  case object Iterator extends BackendObjType {

    def HasNextMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "hasNext",
      mkDescriptor()(BackendType.Bool))

    def NextMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "next",
      mkDescriptor()(JavaObject.toTpe))
  }

  case object Runnable extends BackendObjType {

    def RunMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "run",
      MethodDescriptor.NothingToVoid)
  }

  case object ConcurrentLinkedQueue extends BackendObjType {

    def AddMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "add",
      mkDescriptor(JavaObject.toTpe)(BackendType.Bool), None)

    def PollMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "poll",
      mkDescriptor()(JavaObject.toTpe), None)
  }

  case object Thread extends BackendObjType {

    def StartMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "start",
      MethodDescriptor.NothingToVoid, None)

    def JoinMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "join",
      MethodDescriptor.NothingToVoid, None)

    def CurrentThreadMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "currentThread",
      mkDescriptor()(this.toTpe), None)

    def InterruptMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "interrupt",
      MethodDescriptor.NothingToVoid, None)

    def SetUncaughtExceptionHandlerMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, IsFinal, "setUncaughtExceptionHandler",
      mkDescriptor(ThreadUncaughtExceptionHandler.toTpe)(VoidableType.Void), None)

    def OfVirtualMethod: StaticMethod = StaticMethod(this.jvmName, IsPublic, IsFinal, "ofVirtual",
      mkDescriptor()(ThreadBuilderOfVirtual.toTpe), None)
  }

  case object ThreadBuilderOfVirtual extends BackendObjType {

    def UnstartedMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "unstarted",
      mkDescriptor(JvmName.Runnable.toTpe)(BackendObjType.Thread.toTpe))
  }

  case object ThreadUncaughtExceptionHandler extends BackendObjType {

    def UncaughtExceptionMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "uncaughtException",
      mkDescriptor(Thread.toTpe, JvmName.Throwable.toTpe)(VoidableType.Void), None)
  }

  case object ReentrantLock extends BackendObjType {

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, Nil, None)

    def UnlockMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "unlock", MethodDescriptor.NothingToVoid, None)

    def LockInterruptiblyMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, "lockInterruptibly", MethodDescriptor.NothingToVoid, None)

  }

  case object Result extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.closeClassMaker()
    }

    /**
      * Expects a Result on the stack and leaves a non-Thunk Result.
      * [..., Result] --> [..., Suspension|Value]
      */
    def unwindThunk(): InstructionSet = {
      whileLoop(Condition.NE)(DUP() ~ INSTANCEOF(Thunk.jvmName)) {
        CHECKCAST(Thunk.jvmName) ~
          INVOKEINTERFACE(Thunk.InvokeMethod)
      }
    }

    /**
      * Expects a Result on the stack.
      * If the result is a Suspension, this will return a modified Suspension.
      * If the result in NOT a Suspension, this will leave it on the stack.
      * [..., Result] --> [..., Thunk|Value]
      * side effect: Will return a modified suspension if a suspension occurs
      */
    def handleSuspension(pc: Int, newFrame: InstructionSet, setPc: InstructionSet): InstructionSet = {
      DUP() ~ INSTANCEOF(Suspension.jvmName) ~
      ifCondition(Condition.NE) {
        DUP() ~ CHECKCAST(Suspension.jvmName) ~ // [..., s]
        // Add our new frame
        NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~ // [..., s, s']
        SWAP() ~ // [..., s', s]
        DUP2() ~ // [..., s', s, s', s]
        GETFIELD(Suspension.EffSymField) ~ PUTFIELD(Suspension.EffSymField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.EffOpField) ~ PUTFIELD(Suspension.EffOpField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.ResumptionField) ~ PUTFIELD(Suspension.ResumptionField) ~ // [..., s', s]
        DUP2() ~ GETFIELD(Suspension.PrefixField) ~ // [..., s', s, s', s.prefix]
        // Make the new frame and push it
        newFrame ~
        DUP() ~ cheat(mv => GenExpression.compileInt(pc)(mv)) ~ setPc ~
        INVOKEINTERFACE(Frames.PushMethod) ~ // [..., s', s, s', prefix']
        PUTFIELD(Suspension.PrefixField) ~ // [..., s', s]
        POP() ~ // [..., s']
        // Return the suspension up the stack
        xReturn(Suspension.toTpe)
      }
    }

    /**
      * Expects a Result on the stack and leaves a Value.
      * This might return if a Suspension is encountered.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: Will return any Suspension found
      */
    def unwindThunkToValue(pc: Int, newFrame: InstructionSet, setPc: InstructionSet): InstructionSet = {
      unwindThunk() ~
      handleSuspension(pc, newFrame, setPc) ~
      CHECKCAST(Value.jvmName) // Cannot fail
    }

    /**
      * Expects a Result on the stack and leaves something of the given tpe but erased.
      * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
      * [..., Result] --> [..., Value.value: tpe]
      * side effect: crashes on suspensions
      */
    def unwindSuspensionFreeThunkToType(tpe: BackendType, errorHint: String, loc: SourceLocation): InstructionSet = {
      unwindThunk() ~
      crashIfSuspension(errorHint, loc) ~
      CHECKCAST(Value.jvmName) ~ // Cannot fail
      GETFIELD(Value.fieldFromType(tpe))
    }

    /**
      * Expects a Result on the stack and leaves a Value.
      * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
      * [..., Result] --> [..., Value]
      * side effect: crashes on suspensions
      */
    def unwindSuspensionFreeThunk(errorHint: String, loc: SourceLocation): InstructionSet = {
      unwindThunk() ~ crashIfSuspension(errorHint, loc) ~ CHECKCAST(Value.jvmName)
    }

    /**
      * [..., Result] -> [..., Value|Thunk]
      * side effect: if the result is a suspension, a [[UnhandledEffectError]] is thrown.
      */
    def crashIfSuspension(errorHint: String, loc: SourceLocation): InstructionSet = {
      DUP() ~ INSTANCEOF(Suspension.jvmName) ~
      ifCondition(Condition.NE)(
        CHECKCAST(Suspension.jvmName) ~
        NEW(UnhandledEffectError.jvmName) ~
          // [.., suspension, UEE] -> [.., suspension, UEE, UEE, suspension]
          DUP2() ~ SWAP() ~
          pushString(errorHint) ~
          cheat(mv => AsmOps.compileReifiedSourceLocation(mv, loc)) ~
          // [.., suspension, UEE, UEE, suspension, info, rsl] -> [.., suspension, UEE]
          INVOKESPECIAL(UnhandledEffectError.Constructor) ~
          ATHROW()
      )
    }
  }

  case object Value extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

      // The fields of all erased types, only one will be relevant
      cm.mkConstructor(Constructor)
      cm.mkField(BoolField)
      cm.mkField(CharField)
      cm.mkField(Int8Field)
      cm.mkField(Int16Field)
      cm.mkField(Int32Field)
      cm.mkField(Int64Field)
      cm.mkField(Float32Field)
      cm.mkField(Float64Field)
      cm.mkField(ObjectField)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def BoolField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "b", BackendType.Bool)

    def CharField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "c", BackendType.Char)

    def Int8Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i8", BackendType.Int8)

    def Int16Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i16", BackendType.Int16)

    def Int32Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i32", BackendType.Int32)

    def Int64Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "i64", BackendType.Int64)

    def Float32Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "f32", BackendType.Float32)

    def Float64Field: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "f64", BackendType.Float64)

    def ObjectField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "o", BackendObjType.JavaObject.toTpe)

    /**
      * Returns the field of Value corresponding to the given type
      */
    def fieldFromType(tpe: BackendType): InstanceField = {
      import BackendType._
      tpe match {
        case Bool => BoolField
        case Char => CharField
        case Int8 => Int8Field
        case Int16 => Int16Field
        case Int32 => Int32Field
        case Int64 => Int64Field
        case Float32 => Float32Field
        case Float64 => Float64Field
        case Array(_) | BackendType.Reference(_) => ObjectField
      }
    }
  }

  /** Frame is really just java.util.Function<Value, Result> **/
  case object Frame extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)

      cm.mkInterfaceMethod(ApplyMethod)
      cm.mkStaticInterfaceMethod(StaticApplyMethod)

      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "applyFrame", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticApplyMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.jvmName,
      IsPublic,
      NotFinal,
      "applyFrameStatic",
      mkDescriptor(Frame.toTpe, Value.toTpe)(Result.toTpe),
      Some(_ => withName(0, Frame.toTpe){f => withName(1, Value.toTpe){resumeArg => {
        f.load() ~ resumeArg.load() ~ INVOKEINTERFACE(Frame.ApplyMethod) ~ ARETURN()
      }}}))
  }

  case object Thunk extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName, interfaces = List(Result.jvmName, Runnable.jvmName))

      cm.mkInterfaceMethod(InvokeMethod)
      cm.mkDefaultMethod(RunMethod)

      cm.closeClassMaker()
    }

    def InvokeMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "invoke", mkDescriptor()(Result.toTpe))

    def RunMethod: DefaultMethod = DefaultMethod(this.jvmName, IsPublic, NotFinal, "run", mkDescriptor()(VoidableType.Void), Some(_ =>
      thisLoad() ~ Result.unwindSuspensionFreeThunk(s"in ${JvmName.Runnable.toBinaryName}", SourceLocation.Unknown) ~ POP() ~ RETURN()
    ))
  }

  case object Suspension extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Result.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkField(EffSymField)
      cm.mkField(EffOpField)
      cm.mkField(PrefixField)
      cm.mkField(ResumptionField)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def EffSymField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "effSym", String.toTpe)
    def EffOpField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "effOp", EffectCall.toTpe)
    def PrefixField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "prefix", Frames.toTpe)
    def ResumptionField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "resumption", Resumption.toTpe)

  }

  case object Frames extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)

      cm.mkInterfaceMethod(PushMethod)
      cm.mkInterfaceMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def PushMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "push", mkDescriptor(Frame.toTpe)(Frames.toTpe))

    def ReverseOntoMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "reverseOnto", mkDescriptor(Frames.toTpe)(Frames.toTpe))

    val pushImplementation: Unit => InstructionSet = _ => {
      withName(1, Frame.toTpe)(frame =>
        NEW(FramesCons.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesCons.Constructor) ~
          DUP() ~ frame.load() ~ PUTFIELD(FramesCons.HeadField) ~
          DUP() ~ thisLoad() ~ PUTFIELD(FramesCons.TailField) ~
          xReturn(FramesCons.toTpe)
      )
    }
  }

  case object FramesCons extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkField(HeadField)
      cm.mkField(TailField)
      cm.mkConstructor(Constructor)
      cm.mkMethod(PushMethod)
      cm.mkMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def HeadField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "head", Frame.toTpe)

    def TailField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "tail", Frames.toTpe)

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName, IsFinal, Some(Frames.pushImplementation))

    def ReverseOntoMethod: InstanceMethod = Frames.ReverseOntoMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Frames.toTpe)(rest =>
        thisLoad() ~ GETFIELD(TailField) ~
        NEW(FramesCons.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesCons.Constructor) ~
        DUP() ~ thisLoad() ~ GETFIELD(HeadField) ~ PUTFIELD(HeadField) ~
        DUP() ~ rest.load() ~ PUTFIELD(TailField) ~
        INVOKEINTERFACE(Frames.ReverseOntoMethod) ~
        xReturn(Frames.toTpe)
      )
    ))
  }

  case object FramesNil extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Frames.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkMethod(PushMethod)
      cm.mkMethod(ReverseOntoMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def PushMethod: InstanceMethod = Frames.PushMethod.implementation(this.jvmName, IsFinal, Some(Frames.pushImplementation))

    def ReverseOntoMethod: InstanceMethod = Frames.ReverseOntoMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Frames.toTpe)(rest =>
        rest.load() ~ xReturn(rest.tpe)
      )
    ))
  }

  case object Resumption extends BackendObjType with Generatable {
    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(RewindMethod)
      cm.mkStaticInterfaceMethod(StaticRewindMethod)
      cm.closeClassMaker()
    }

    def RewindMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "rewind", mkDescriptor(Value.toTpe)(Result.toTpe))

    def StaticRewindMethod: StaticInterfaceMethod = StaticInterfaceMethod(this.jvmName, IsPublic, NotFinal, "staticRewind", mkDescriptor(Resumption.toTpe, Value.toTpe)(Result.toTpe), Some(_ =>
      withName(0, Resumption.toTpe) { resumption =>
        withName(1, Value.toTpe) { v => {
          resumption.load() ~ v.load() ~ INVOKEINTERFACE(Resumption.RewindMethod) ~ ARETURN()
        }
        }
      }
    ))
  }

  case object ResumptionCons extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor)

      cm.mkField(SymField)
      cm.mkField(HandlerField)
      cm.mkField(FramesField)
      cm.mkField(TailField)

      cm.mkMethod(RewindMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def SymField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "sym", String.toTpe)
    def HandlerField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "handler", Handler.toTpe)
    def FramesField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "frames", Frames.toTpe)
    def TailField: InstanceField = InstanceField(this.jvmName, IsPublic, NotFinal, NotVolatile, "tail", Resumption.toTpe)

    def RewindMethod: InstanceMethod = Resumption.RewindMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Value.toTpe) { v =>
        thisLoad() ~ GETFIELD(SymField) ~
          thisLoad() ~ GETFIELD(HandlerField) ~
          thisLoad() ~ GETFIELD(FramesField) ~
          // () -> tail.rewind(v)
          thisLoad() ~ GETFIELD(TailField) ~
          v.load() ~
          mkStaticLambda(Thunk.InvokeMethod, Resumption.StaticRewindMethod, drop = 0) ~
          mkStaticLambda(Thunk.InvokeMethod, Handler.InstallHandlerMethod, drop = 0) ~
          xReturn(Thunk.toTpe)
      }))
  }

  case object ResumptionNil extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, interfaces = List(Resumption.jvmName))

      cm.mkConstructor(Constructor)
      cm.mkMethod(RewindMethod)

      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = nullarySuperConstructor(JavaObject.Constructor)

    def RewindMethod: InstanceMethod = Resumption.RewindMethod.implementation(this.jvmName, IsFinal, Some(_ =>
      withName(1, Value.toTpe) { v =>
        v.load() ~ xReturn(v.tpe)
      }
    ))
  }

  case object Handler extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkStaticInterfaceMethod(InstallHandlerMethod)
      cm.closeClassMaker()
    }

    def InstallHandlerMethod: StaticInterfaceMethod = StaticInterfaceMethod(
      this.jvmName,
      IsPublic,
      NotFinal,
      "installHandler",
      mkDescriptor(String.toTpe, Handler.toTpe, Frames.toTpe, Thunk.toTpe)(Result.toTpe),
      Some(_ => withName(0, String.toTpe){effSym => withName(1, Handler.toTpe){handler =>
        withName(2, Frames.toTpe){frames => withName(3, Thunk.toTpe){thunk =>
          thunk.load() ~
          // Thunk|Value|Suspension
          Result.unwindThunk() ~
          // Value|Suspension
          { // handle suspension
            DUP() ~ INSTANCEOF(Suspension.jvmName) ~ ifCondition(Condition.NE) {
              DUP() ~ CHECKCAST(Suspension.jvmName) ~ storeWithName(4, Suspension.toTpe) {s =>
                NEW(ResumptionCons.jvmName) ~ DUP() ~ INVOKESPECIAL(ResumptionCons.Constructor) ~
                  DUP() ~ effSym.load() ~ PUTFIELD(ResumptionCons.SymField) ~
                  DUP() ~ handler.load() ~ PUTFIELD(ResumptionCons.HandlerField) ~
                  DUP() ~
                  s.load() ~ GETFIELD(Suspension.PrefixField) ~ frames.load() ~ INVOKEINTERFACE(Frames.ReverseOntoMethod) ~
                  PUTFIELD(ResumptionCons.FramesField) ~
                  DUP() ~ s.load() ~ GETFIELD(Suspension.ResumptionField) ~ PUTFIELD(ResumptionCons.TailField) ~
                  storeWithName(5, ResumptionCons.toTpe){r =>
                    s.load() ~ GETFIELD(Suspension.EffSymField) ~ effSym.load() ~ INVOKEVIRTUAL(JavaObject.EqualsMethod) ~
                    ifCondition(Condition.NE){
                      s.load() ~ GETFIELD(Suspension.EffOpField) ~ handler.load() ~ r.load() ~
                      INVOKEINTERFACE(EffectCall.ApplyMethod) ~ xReturn(Result.toTpe)
                    } ~
                    NEW(Suspension.jvmName) ~ DUP() ~ INVOKESPECIAL(Suspension.Constructor) ~
                    DUP() ~ s.load() ~ GETFIELD(Suspension.EffSymField) ~ PUTFIELD(Suspension.EffSymField) ~
                    DUP() ~ s.load() ~ GETFIELD(Suspension.EffOpField) ~ PUTFIELD(Suspension.EffOpField) ~
                    DUP() ~ NEW(FramesNil.jvmName) ~ DUP() ~ INVOKESPECIAL(FramesNil.Constructor) ~ PUTFIELD(Suspension.PrefixField) ~
                    DUP() ~ r.load() ~ PUTFIELD(Suspension.ResumptionField) ~
                    xReturn(Suspension.toTpe)
                  }
              }
            }
          } ~
          // Value
          CHECKCAST(Value.jvmName) ~ storeWithName(6, Value.toTpe) {res =>
            //
            // Case on frames
            // FramesNil
            frames.load() ~ INSTANCEOF(FramesNil.jvmName) ~ ifCondition(Condition.NE) {
              res.load() ~ xReturn(Value.toTpe)
            } ~
              // FramesCons
              frames.load() ~ CHECKCAST(FramesCons.jvmName) ~ storeWithName(7, FramesCons.toTpe) { cons => {
              effSym.load() ~
              handler.load() ~
              cons.load() ~ GETFIELD(FramesCons.TailField) ~
              // thunk
              cons.load() ~ GETFIELD(FramesCons.HeadField) ~
              res.load() ~
              mkStaticLambda(Thunk.InvokeMethod, Frame.StaticApplyMethod, drop = 0) ~
              INVOKESTATIC(InstallHandlerMethod) ~
              xReturn(Result.toTpe)
            }
          }}
      }}}}
      )
    )
  }

  case object EffectCall extends BackendObjType with Generatable {

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkInterface(this.jvmName)
      cm.mkInterfaceMethod(ApplyMethod)
      cm.closeClassMaker()
    }

    def ApplyMethod: InterfaceMethod = InterfaceMethod(this.jvmName, "apply", mkDescriptor(Handler.toTpe, Resumption.toTpe)(Result.toTpe))

  }

  case class ResumptionWrapper(tpe: BackendType) extends BackendObjType with Generatable {

    // tpe -> Result
    private val superClass: JvmType.Reference = JvmOps.getClosureAbstractClassType(List(tpe.toErasedJvmType), JvmType.Object)

    def genByteCode()(implicit flix: Flix): Array[Byte] = {
      val cm = mkClass(this.jvmName, IsFinal, superClass.name)
      cm.mkConstructor(Constructor)
      cm.mkField(ResumptionField)
      cm.mkMethod(InvokeMethod)
      cm.mkMethod(UniqueMethod)
      cm.closeClassMaker()
    }

    def Constructor: ConstructorMethod = ConstructorMethod(this.jvmName, IsPublic, List(Resumption.toTpe), Some(_ =>
      withName(1, Resumption.toTpe) { resumption =>
        thisLoad() ~ INVOKESPECIAL(superClass.name, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid) ~
          thisLoad() ~ resumption.load() ~ PUTFIELD(ResumptionField) ~
          RETURN()
      }
    ))

    def ResumptionField: InstanceField = InstanceField(this.jvmName, IsPrivate, IsFinal, NotVolatile, "resumption", Resumption.toTpe)

    def InvokeMethod: InstanceMethod = Thunk.InvokeMethod.implementation(this.jvmName, NotFinal, Some(_ =>
      thisLoad() ~ GETFIELD(ResumptionField) ~
        NEW(Value.jvmName) ~ DUP() ~ INVOKESPECIAL(Value.Constructor) ~
        DUP() ~
        thisLoad() ~ cheat(_.visitFieldInsn(Opcodes.GETFIELD, this.jvmName.toInternalName, "arg0", tpe.toErased.toDescriptor)) ~
        PUTFIELD(Value.fieldFromType(tpe.toErased)) ~
        INVOKEINTERFACE(Resumption.RewindMethod) ~
        xReturn(Result.toTpe)
    ))

    def UniqueMethod: InstanceMethod = InstanceMethod(this.jvmName, IsPublic, NotFinal, GenClosureAbstractClasses.GetUniqueThreadClosureFunctionName, mkDescriptor()(Native(this.superClass.name).toTpe), Some(_ =>
      thisLoad() ~ ARETURN()
    ))

  }
}

sealed trait Generatable extends BackendObjType {
  def genByteCode()(implicit flix: Flix): Array[Byte]
}
