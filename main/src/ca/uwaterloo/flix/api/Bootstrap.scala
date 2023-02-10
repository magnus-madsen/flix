/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, Manifest, ManifestParser, MavenPackageManager}
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable

// TODO: This class is ultimately to replace functionality in:
// SourceProvider, SourceFiles, Packager, etc.
// Feel free to look there for functionality that can be reused.
// The idea is to implement everything in here so it works 100% and then remove the old code.
// Hence its OK to copy paste some methods from the aforementioned classes in here.

object Bootstrap {
  def main(args: Array[String]): Unit = {
    // TODO: Do ad-hoc testing here.
    val b = new Bootstrap
    b.bootstrap(".")
  }

}

class Bootstrap {

  private val sourcePaths: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

  def bootstrap(path: String): Unit = { // TODO: Probably return Result or Validation
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    val tomlPath = Paths.get(path).resolve("flix.toml")
    println(tomlPath)
    if (Files.exists(tomlPath)) {
      projectMode(tomlPath)
    } else {
      folderMode()
    }
  }

  private def projectMode(tomlPath: Path): List[Path] = { // TODO: Probably return Result or Validation
    // 1. Read, parse, and validate flix.toml.
    val manifest = ManifestParser.parse(tomlPath) match {
      case Ok(m) => println(m); m
      case Err(_) => ??? //TODO: error handling
    }

    // 2. Check each dependency is available or download it.
    FlixPackageManager.installAll(manifest, Paths.get(""))(System.out)
    MavenPackageManager.installAll(manifest)(System.out)

    // 3. Compute the set of JAR paths and Flix fpkg paths.
    val filesFlix = manifest.getFlixPackages //TODO: implement
    val filesMaven = manifest.getMavenPackages //TODO: implement

    // 4. Add *.flix, src/**.flix and test/**.flix
    val p = Paths.get(".")
    val filesHere = createPathList(p, "flix")
    val filesSrc = createPathListRec(p.resolve("src"), "flix")
    val filesTest = createPathListRec(p.resolve("test"), "flix")

    val res = filesFlix ++ filesMaven ++ filesHere ++ filesSrc ++ filesTest
    println(res)
    res
  }

  private def folderMode(): List[Path] = { // TODO: Probably return Result or Validation
    // 1. Add *.flix, src/**.flix and test/**.flix
    val p = Paths.get(".")
    val filesHere = createPathList(p, "flix")
    val filesSrc = createPathListRec(p.resolve("src"), "flix")
    val filesTest = createPathListRec(p.resolve("test"), "flix")

    // 2. Grab all jars in lib/
    val jarFilesLib = createPathListRec(p.resolve("lib"), "jar")

    // 3. Grab all flix packages in lib/
    val flixFilesLib = createPathListRec(p.resolve("lib"), "fpkg")

    val res = filesHere ++ filesSrc ++ filesTest ++ jarFilesLib ++ flixFilesLib
    println(res)
    res
  }

  def reconfigureFlix(flix: Flix): Unit = { // TODO: Probably return Result or Validation

    val manifest: Manifest = ??? // TODO: Probably from a field.

    // TODO: Add logic to check if the file has changed.
    for (path <- sourcePaths) {
      flix.addSourcePath(path)
    }

    for (flixPackagePath <- manifest.getFlixPackages) {
      flix.addSourcePath(flixPackagePath)
    }

    for (mavenJarPath <- manifest.getMavenPackages) {
      flix.addSourcePath(mavenJarPath)
    }

  }

  private def createPathList(path: Path, ext: String): List[Path] = {
    val file = path.toFile
    val files = file.listFiles()
    if(files == null) {
      List.empty
    } else {
      files.toList.filter(file =>
        if (file.isFile) {
          val i = file.toString.lastIndexOf('.')
          val extension = if (i > 0) file.toString.substring(i + 1) else ""
          extension == ext
        } else {
          false
        }
      ).map(file => file.toPath)
    }
  }

  private def createPathListRec(path: Path, ext: String): List[Path] = {
    val filesHere = createPathList(path, ext)
    val dirs = path.toFile.listFiles()
    if(dirs == null) {
      List.empty
    } else {
      dirs.toList.filter(file => file.isDirectory)
      val filesRec = dirs.foldLeft[List[Path]](List.empty)((l, f) => l ++ createPathListRec(f.toPath, ext))
      filesHere ++ filesRec
    }
  }


}
