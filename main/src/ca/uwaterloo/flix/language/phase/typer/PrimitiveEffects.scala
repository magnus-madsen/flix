/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{InternalCompilerException, LocalResource}
import org.json4s.JsonAST.*
import org.json4s.jvalue2monadic
import org.json4s.native.JsonMethods.parse

import java.lang.reflect.{Constructor, Method}

object PrimitiveEffects {

  /** The path to the package effects. */
  private val PackageEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Packages.json"

  /** The path to the class effects. */
  private val ClassEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Classes.json"

  /** The path to the constructor effects. */
  private val ConstructorEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Constructors.json"

  /** The path to the method effects. */
  private val MethodEffsPath = "/src/ca/uwaterloo/flix/language/phase/typer/PrimitiveEffects.Methods.json"

  /**
    * A pre-computed map from packages to effects.
    */
  private val packageEffs: Map[Package, Set[Symbol.EffectSym]] = loadPackageEffs()

  /**
    * A pre-computed map from classes to effects.
    *
    * If there is are specific effect(s) for a constructor or method then we use the effects for the entire class.
    */
  private val classEffs: Map[Class[?], Set[Symbol.EffectSym]] = loadClassEffs()

  /**
    * A pre-computed map from constructors to effects.
    */
  private val constructorEffs: Map[Constructor[?], Set[Symbol.EffectSym]] = loadConstructorEffs()

  /**
    * A pre-computed map from methods to effects.
    */
  private val methodEffs: Map[Method, Set[Symbol.EffectSym]] = loadMethodEffs()

  def getAnnotatedMethods: Set[Method] = methodEffs.keys.toSet

  def getAnnotatedConstructors: Set[Constructor[?]] = constructorEffs.keys.toSet

  def getAnnotatedClasses: Set[Class[?]] = classEffs.keys.toSet

  def getAnnotatedPackages: Set[Package] = packageEffs.keys.toSet

  /**
    * Returns the primitive effects of calling the given constructor `c`.
    */
  def getConstructorEffs(c: Constructor[?], loc: SourceLocation): Type = constructorEffs.get(c) match {
    case None =>
      // Case 1: No effects for the constructor. Try the class map.
      getClassAndPackageEffs(c.getDeclaringClass, loc)
    case Some(effs) =>
      // Case 2: We found the effects for the constructor.
      toEffSet(effs, loc)
  }

  /**
    * Returns the primitive effects of calling the given method `m`.
    */
  def getMethodEffs(m: Method, loc: SourceLocation): Type = methodEffs.get(m) match {
    case None =>
      // Case 1: No effects for the method. Try the class map.
      getClassAndPackageEffs(m.getDeclaringClass, loc)
    case Some(effs) =>
      // Case 2: We found the effects for the method.
      toEffSet(effs, loc)
  }

  /**
    * Returns the primitive effects of the class `c` if they exist.
    * Defaults to [[getPackageEffs]] if nothing was found.
    */
  private def getClassAndPackageEffs(c: Class[?], loc: SourceLocation): Type = {
    classEffs.get(c) match {
      case None =>
        // Case 1.1: No effects for the class. Try the package.
        getPackageEffs(c.getPackage, loc)
      case Some(effs) =>
        // Case 1.2: We use the class effects.
        toEffSet(effs, loc)
    }
  }

  /**
    * Returns the primitive effs of the package `p`.
    * Defaults to [[Type.IO]] if nothing was found.
    */
  private def getPackageEffs(p: Package, loc: SourceLocation): Type = {
    packageEffs.get(p) match {
      case None =>
        // Case 1.1.1: No effects for the package. Use the IO effect by default.
        Type.IO
      case Some(effs) =>
        // Case 1.1.2: We use the package effects.
        toEffSet(effs, loc)
    }
  }

  /**
    * Returns the set of effects represented by `effs`.
    */
  private def toEffSet(effs: Set[Symbol.EffectSym], loc: SourceLocation): Type = {
    val tpes = effs.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc))
    Type.mkUnion(tpes, loc)
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "packages": {
    *     "java.lang.net": "Net, IO"
    *   }
    * }
    * }}}
    */
  private def loadPackageEffs(): Map[Package, Set[Symbol.EffectSym]] = {
    val data = LocalResource.get(PackageEffsPath)
    val json = parse(data)

    val m = json \\ "packages" match {
      case JObject(l) => l.map {
        case (packageName, JString(s)) =>
          val clazz = ClassLoader.getPlatformClassLoader.getDefinedPackage(packageName)
          val effSet = parseEffSet(s)
          (clazz, effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "classes": {
    *     "java.lang.ProcessBuilder": "Exec, FsRead",
    *     "java.lang.reflect.Method": "Sys"
    *   }
    * }
    * }}}
    */
  private def loadClassEffs(): Map[Class[?], Set[Symbol.EffectSym]] = {
    val data = LocalResource.get(ClassEffsPath)
    val json = parse(data)

    val m = json \\ "classes" match {
      case JObject(l) => l.map {
        case (className, JString(s)) =>
          val clazz = Class.forName(className)
          val effSet = parseEffSet(s)
          (clazz, effSet)
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "constructors": {
    *     "java.lang.ProcessBuilder": "Exec",
    *     "java.net.URL": "Net"
    *   }
    * }
    * }}}
    *
    * Note: The effect set applies to *ALL* constructors of the class.
    */
  private def loadConstructorEffs(): Map[Constructor[?], Set[Symbol.EffectSym]] = {
    val data = LocalResource.get(ConstructorEffsPath)
    val json = parse(data)

    val m = json \\ "constructors" match {
      case JObject(l) => l.flatMap {
        case (className, JString(s)) =>
          val clazz = Class.forName(className)
          val effSet = parseEffSet(s)
          clazz.getConstructors.map(c => (c, effSet))
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Parses a JSON file of the form:
    *
    * {{{
    * {
    *   "methods": {
    *     "java.lang.System::exit": "Sys"
    *   }
    * }
    * }}}
    *
    * Note: The effect set applies to *ALL* constructors of the class.
    */
  private def loadMethodEffs(): Map[Method, Set[Symbol.EffectSym]] = {
    val data = LocalResource.get(MethodEffsPath)
    val json = parse(data)

    val m = json \\ "methods" match {
      case JObject(l) => l.flatMap {
        case (classNameAndMethod, JString(s)) =>
          val cc = classNameAndMethod.indexOf("::")
          val className = classNameAndMethod.substring(0, cc)
          val methodName = classNameAndMethod.substring(cc + 2)
          val clazz = Class.forName(className)
          val effSet = parseEffSet(s)
          clazz.getMethods.filter(_.getName == methodName).map(m => (m, effSet))
        case _ => throw InternalCompilerException("Unexpected field value.", SourceLocation.Unknown)
      }
      case _ => throw InternalCompilerException("Unexpected JSON format.", SourceLocation.Unknown)
    }

    m.toMap
  }

  /**
    * Returns the given comma-separated string of effect symbols as a set of [[Symbol.EffectSym]].
    *
    * Returns the empty set if the string is empty.
    */
  private def parseEffSet(s: String): Set[Symbol.EffectSym] = {
    if (s.trim.isEmpty)
      Set.empty
    else
      s.split(",").map(_.trim).map(Symbol.parsePrimitiveEff).toSet
  }
}
