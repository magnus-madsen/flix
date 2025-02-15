/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.{MonoType, ReducedAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.mangle
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.{Field, Method}
import java.nio.file.{Files, LinkOption, Path}

object JvmOps {

  /**
    * The root package name.
    */
  val RootPackage: List[String] = Nil

  /**
    * Returns the given Flix type `tpe` as JVM type.
    *
    * For example, if the type is:
    *
    * Bool                  =>      Boolean
    * Char                  =>      Char
    * Option$42             =>      Option$42
    * Result$123            =>      Result$123
    * Int -> Bool           =>      Fn1$Int$Bool
    * (Int, Int) -> Bool    =>      Fn2$Int$Int$Bool
    */
  def getJvmType(tpe: MonoType)(implicit root: ReducedAst.Root): JvmType = tpe match {
    // Primitives
    case MonoType.Void => JvmType.Object
    case MonoType.AnyType => JvmType.Object
    case MonoType.Unit => JvmType.Unit
    case MonoType.Bool => JvmType.PrimBool
    case MonoType.Char => JvmType.PrimChar
    case MonoType.Float32 => JvmType.PrimFloat
    case MonoType.Float64 => JvmType.PrimDouble
    case MonoType.BigDecimal => JvmType.BigDecimal
    case MonoType.Int8 => JvmType.PrimByte
    case MonoType.Int16 => JvmType.PrimShort
    case MonoType.Int32 => JvmType.PrimInt
    case MonoType.Int64 => JvmType.PrimLong
    case MonoType.BigInt => JvmType.BigInteger
    case MonoType.String => JvmType.String
    case MonoType.Regex => JvmType.Regex
    case MonoType.Region => JvmType.Object
    case MonoType.Null => JvmType.Object
    // Compound
    case MonoType.Array(_) => JvmType.Object
    case MonoType.Lazy(_) => JvmType.Object
    case MonoType.Tuple(elms) => JvmType.Reference(BackendObjType.Tuple(elms.map(BackendType.asErasedBackendType)).jvmName)
    case MonoType.RecordEmpty => JvmType.Reference(BackendObjType.Record.jvmName)
    case MonoType.RecordExtend(_, _, _) => JvmType.Reference(BackendObjType.Record.jvmName)
    case MonoType.Enum(_, _) => JvmType.Object
    case MonoType.Struct(sym, targs) =>
      val elms = instantiateStruct(sym, targs.map(MonoType.erase))
      JvmType.Reference(BackendObjType.Struct(elms).jvmName)
    case MonoType.Arrow(_, _) => getFunctionInterfaceType(tpe)
    case MonoType.Native(clazz) => JvmType.Reference(JvmName.ofClass(clazz))
  }


  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    *
    * Every primitive type is mapped to itself and every other type is mapped to Object.
    */
  def getErasedJvmType(tpe: MonoType): JvmType = {
    import MonoType.*
    tpe match {
      case Bool => JvmType.PrimBool
      case Char => JvmType.PrimChar
      case Float32 => JvmType.PrimFloat
      case Float64 => JvmType.PrimDouble
      case Int8 => JvmType.PrimByte
      case Int16 => JvmType.PrimShort
      case Int32 => JvmType.PrimInt
      case Int64 => JvmType.PrimLong
      case Void | AnyType | Unit | BigDecimal | BigInt | String | Regex |
           Region | Array(_) | Lazy(_) | Tuple(_) | Enum(_, _) |
           Struct(_, _) | Arrow(_, _) | RecordEmpty | RecordExtend(_, _, _) |
           Native(_) | Null =>
        JvmType.Object
    }
  }

  /**
    * Returns the erased JvmType of the given Flix type `tpe`.
    *
    * Every primitive type is mapped to itself and every other type is mapped to Object.
    */
  def asErasedJvmType(tpe: MonoType): JvmType = {
    import MonoType.*
    tpe match {
      case Bool => JvmType.PrimBool
      case Char => JvmType.PrimChar
      case Float32 => JvmType.PrimFloat
      case Float64 => JvmType.PrimDouble
      case Int8 => JvmType.PrimByte
      case Int16 => JvmType.PrimShort
      case Int32 => JvmType.PrimInt
      case Int64 => JvmType.PrimLong
      case Native(clazz) if clazz == classOf[Object] => JvmType.Object
      case Void | AnyType | Unit | BigDecimal | BigInt | String | Regex |
           Region | Array(_) | Lazy(_) | Tuple(_) | Enum(_, _) |
           Struct(_, _) | Arrow(_, _) | RecordEmpty | RecordExtend(_, _, _) |
           Native(_) | Null =>
        throw InternalCompilerException(s"Unexpected type $tpe", SourceLocation.Unknown)
    }
  }

  /**
    * Returns the function abstract class type `FnX$Y$Z` for the given type `tpe`.
    *
    * For example:
    *
    * Int -> Int          =>  Fn2$Int$Int
    * (Int, String) -> Int   =>  Fn3$Int$Obj$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getFunctionInterfaceType(tpe: MonoType): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      val arrowType = BackendObjType.Arrow(targs.map(BackendType.toErasedBackendType), BackendType.asErasedBackendType(tresult))
      JvmType.Reference(arrowType.jvmName)
    case _ =>
      throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }

  /**
    * Returns the closure abstract class type `CloX$Y$Z` for the given [[MonoType]].
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    *
    * NB: The given type `tpe` must be an arrow type.
    */
  def getClosureAbstractClassType(tpe: MonoType): JvmType.Reference = tpe match {
    case MonoType.Arrow(targs, tresult) =>
      getClosureAbstractClassType(targs.map(getErasedJvmType), asErasedJvmType(tresult))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", SourceLocation.Unknown)
  }


  /**
    * Returns the closure abstract class type `CloX$Y$Z` for the given signature.
    *
    * For example:
    *
    * Int -> Int          =>  Clo1$Int$Int
    * (Int, Int) -> Int   =>  Clo2$Int$Int$Int
    */
  def getClosureAbstractClassType(argTypes: List[JvmType], resType: JvmType): JvmType.Reference = {
    val arity = argTypes.length
    val args = (argTypes ::: resType :: Nil).map(_.toErased).map(stringify)
    val name = JvmName.mkClassName(s"Clo$arity", args)
    JvmType.Reference(JvmName(RootPackage, name))
  }

  /**
    * Returns the closure class `Clo$Name` for the given closure.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.length       =>    List/Clo$length
    * List.map          =>    List/Clo$map
    */
  def getClosureClassType(sym: Symbol.DefnSym): JvmType.Reference = {
    // The JVM name is of the form Clo$sym.name
    val name = JvmName.mkClassName(s"Clo", sym.name)

    // The JVM package is the namespace of the symbol.
    val pkg = sym.namespace

    // The result type.
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the function definition class for the given symbol.
    *
    * For example:
    *
    * print         =>  Def$print
    * List.length   =>  List.Def$length
    */
  def getFunctionDefinitionClassType(sym: Symbol.DefnSym): JvmType.Reference = {
    val pkg = sym.namespace
    val name = JvmName.mkClassName("Def", sym.name)
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the effect definition class for the given symbol.
    *
    * For example:
    *
    * Print       =>  Eff$Print
    * List.Crash  =>  List.Eff$Crash
    */
  def getEffectDefinitionClassType(sym: Symbol.EffectSym): JvmType.Reference = {
    val pkg = sym.namespace
    val name = JvmName.mkClassName("Eff", sym.name)
    JvmType.Reference(JvmName(pkg, name))
  }

  /**
    * Returns the op name of the given symbol.
    */
  def getEffectOpName(op: Symbol.OpSym): String = {
    mangle(op.name)
  }

  /**
    * Returns the namespace name for the given namespace `ns`.
    *
    * For example:
    *
    * <root>      =>  Root$
    * Foo         =>  Foo
    * Foo.Bar     =>  Foo.Bar
    * Foo.Bar.Baz =>  Foo.Bar.Baz
    */
  def getNamespaceClassType(ns: NamespaceInfo): JvmName = {
    getNamespaceName(ns.ns)
  }

  /**
    * Returns the namespace name of the given definition symbol `sym`.
    */
  def getNamespaceName(sym: Symbol.DefnSym): JvmName = {
    getNamespaceName(sym.namespace)
  }

  private def getNamespaceName(ns: List[String]): JvmName = {
    val last = ns.lastOption.getOrElse(s"Root${Flix.Delimiter}")
    val nsFixed = ns.dropRight(1)
    JvmName(nsFixed, last)
  }

  /**
    * Returns the method name of a defn as used in a namespace class.
    *
    * For example:
    *
    * find      =>  m_find
    * length    =>  m_length
    */
  def getDefMethodNameInNamespaceClass(defn: ReducedAst.Def): String = {
    /**
      * Exported names are checked in [[ca.uwaterloo.flix.language.phase.Safety]]
      * so no mangling is needed.
      */
    if (defn.ann.isExport) defn.sym.name
    else "m_" + mangle(defn.sym.name)
  }

  def getTagName(sym: Symbol.CaseSym): String = mangle(sym.name)

  /**
    * Returns stringified name of the given JvmType `tpe`.
    *
    * The stringified name is short hand used for generation of interface and class names.
    */
  def stringify(tpe: JvmType): String = tpe match {
    case JvmType.Void => "Void"
    case JvmType.PrimBool => "Bool"
    case JvmType.PrimChar => "Char"
    case JvmType.PrimFloat => "Float32"
    case JvmType.PrimDouble => "Float64"
    case JvmType.PrimByte => "Int8"
    case JvmType.PrimShort => "Int16"
    case JvmType.PrimInt => "Int32"
    case JvmType.PrimLong => "Int64"
    case JvmType.Reference(_) => "Obj"
  }

  /**
    * Returns the set of namespaces in the given AST `root`.
    */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        NamespaceInfo(ns, defs)
    }.toSet
  }

  /**
    * Returns the set of erased lazy types in `types` without searching recursively.
    */
  def getErasedLazyTypesOf(types: Iterable[MonoType]): Set[BackendObjType.Lazy] =
    types.foldLeft(Set.empty[BackendObjType.Lazy]) {
      case (acc, MonoType.Lazy(tpe)) => acc + BackendObjType.Lazy(BackendType.asErasedBackendType(tpe))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased record extend types in `types` without searching recursively.
    */
  def getErasedRecordExtendsOf(types: Iterable[MonoType]): Set[BackendObjType.RecordExtend] =
    types.foldLeft(Set.empty[BackendObjType.RecordExtend]) {
      case (acc, MonoType.RecordExtend(_, value, _)) =>
        acc + BackendObjType.RecordExtend(BackendType.asErasedBackendType(value))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased function types in `types` without searching recursively.
    */
  def getErasedArrowsOf(types: Iterable[MonoType]): Set[BackendObjType.Arrow] =
    types.foldLeft(Set.empty[BackendObjType.Arrow]) {
      case (acc, MonoType.Arrow(args, result)) =>
        acc + BackendObjType.Arrow(args.map(BackendType.toErasedBackendType), BackendType.toErasedBackendType(result))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased tuple types in `types` without searching recursively.
    */
  def getErasedTupleTypesOf(types: Iterable[MonoType]): Set[BackendObjType.Tuple] =
    types.foldLeft(Set.empty[BackendObjType.Tuple]) {
      case (acc, MonoType.Tuple(elms)) =>
        acc + BackendObjType.Tuple(elms.map(BackendType.asErasedBackendType))
      case (acc, _) => acc
    }

  /**
    * Returns the set of erased struct types in `types` without searching recursively.
    */
  def getErasedStructTypesOf(root: Root, types: Iterable[MonoType]): Set[BackendObjType.Struct] =
    types.foldLeft(Set.empty[BackendObjType.Struct]) {
      case (acc, MonoType.Struct(sym, targs)) =>
        acc + BackendObjType.Struct(instantiateStruct(sym, targs)(root))
      case (acc, _) => acc
    }

  /**
    * Returns the ordered list of struct fields based on the given type `sym[targs..]`. It is
    * assumed that both `targs` and the structs in `root` use erased types.
    *
    * Example:
    *   - `instantiateStruct(S, List(Int32, IO)) = List(Int32, Int32, Object)`
    *     for `struct S[t, r] { mut x: t, y: t, z: Object }`
    */
  def instantiateStruct(sym: Symbol.StructSym, targs: List[MonoType])(implicit root: ReducedAst.Root): List[BackendType] = {
    val struct = root.structs(sym)
    assert(struct.tparams.length == targs.length)
    val map = struct.tparams.map(_.sym).zip(targs).toMap
    struct.fields.map(field => instantiateType(map, field.tpe))
  }

  /**
    * Returns the set of erased tag types in `types` without searching recursively.
    */
  def getErasedTagTypesOf(root: Root, types: Iterable[MonoType]): Set[BackendObjType.TagType] =
    types.foldLeft(Set.empty[BackendObjType.TagType]) {
      case (acc, MonoType.Enum(sym, targs)) =>
        val tags = instantiateEnum(root.enums(sym), targs)
        tags.foldLeft(acc) {
          case (acc, (sym, Nil)) => acc + BackendObjType.NullaryTag(sym)
          case (acc, (_, tagElms)) => acc + BackendObjType.Tag(tagElms)
        }
      case (acc, _) => acc
    }

  /**
    * Returns the ordered list of enums terms based on the given type `sym[targs..]`. It is assumed
    * that both `targs` and the enums in `root` use erased types.
    *
    * Example:
    *   - `instantiateEnum(E, List(Char)) = Map(A -> List(Char, Object), B -> List(Int32))`
    *     for `enum E[t] { case A(t, Object) case B(Int32) }`
    */
  def instantiateEnum(enm: ReducedAst.Enum, targs: List[MonoType]): Map[Symbol.CaseSym, List[BackendType]] = {
    assert(enm.tparams.length == targs.length)
    val map = enm.tparams.map(_.sym).zip(targs).toMap
    enm.cases.map {
      case (_, caze) => (caze.sym, caze.tpes.map(instantiateType(map, _)))
    }
  }

  /**
    * Instantiates `tpe` given the variable map `m`.
    *
    * Examples:
    *   - `instantiateType([x -> Int32], x) = Int32`
    *   - `instantiateType(_, Int32) = Int32`
    *   - `instantiateType(_, Object) = Object`
    *   - `instantiateType([x -> String], x) = throw InternalCompilerException`
    *   - `instantiateType([x -> Int32], y) = throw InternalCompilerException`
    *   - `instantiateType(_, Option[Int32]) =  throw InternalCompilerException`
    *
    * @param m Decides types for variables, must only contain erased types.
    * @param tpe the type to instantiate, must be a polymorphic erased type
    *            (either [[Type.Var]], a primitive type, or `java.lang.Object`)
    */
  private def instantiateType(m: Map[Symbol.KindedTypeVarSym, MonoType], tpe: Type): BackendType = tpe match {
    case Type.Var(sym, _) => BackendType.asErasedBackendType(m(sym))
    case Type.Cst(tc, _) => tc match {
      case TypeConstructor.Bool => BackendType.Bool
      case TypeConstructor.Char => BackendType.Char
      case TypeConstructor.Float32 => BackendType.Float32
      case TypeConstructor.Float64 => BackendType.Float64
      case TypeConstructor.Int8 => BackendType.Int8
      case TypeConstructor.Int16 => BackendType.Int16
      case TypeConstructor.Int32 => BackendType.Int32
      case TypeConstructor.Int64 => BackendType.Int64
      case TypeConstructor.Native(clazz) if clazz == classOf[Object] => BackendObjType.JavaObject.toTpe
      case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    }
    case Type.Apply(_, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
    case Type.GetEff(_, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'", tpe.loc)
  }

  /**
    * Writes the given JVM class `clazz` to a sub path under the given `prefixPath`.
    *
    * For example, if the prefix path is `/tmp/` and the class name is Foo.Bar.Baz
    * then the bytecode is written to the path `/tmp/Foo/Bar/Baz.class` provided
    * that this path either does not exist or is already a JVM class file.
    */
  def writeClass(prefixPath: Path, clazz: JvmClass): Unit = {
    // Compute the absolute path of the class file to write.
    val path = prefixPath.resolve(clazz.name.toPath).toAbsolutePath

    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.", SourceLocation.Unknown)
      }

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isClassFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.", SourceLocation.Unknown)
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
    * Returns `true` if the given `path` is non-empty (i.e. contains data).
    */
  private def isEmpty(path: Path): Boolean = Files.size(path) == 0L

  /**
    * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
    */
  private def isClassFile(path: Path): Boolean = {
    if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(path)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match CAFE BABE.
      return b1 == 0xCA && b2 == 0xFE && b3 == 0xBA && b4 == 0xBE
    }
    false
  }

}
