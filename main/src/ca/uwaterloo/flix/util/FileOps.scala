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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation
import org.json4s.JValue
import org.json4s.native.JsonMethods

import java.nio.file.{Files, LinkOption, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object FileOps {

  /**
    * Reads the first line of the file at the given path `p` if it is possible.
    */
  def readLine(p: Path): Option[String] = {
    try {
      Some(Files.lines(p).findFirst().get())
    } catch {
      case _: Throwable => None
    }
  }

  /**
   * Writes the given string `s` to the given file path `p`.
   *
   * Creates the parent directory of `p` if needed.
   *
   * @param append if set to true, the content will be appended to the file
   */
  def writeString(p: Path, s: String, append: Boolean = false): Unit = {
    Files.createDirectories(p.getParent)

    // Check if the file already exists.
    if (Files.exists(p)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(p, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$p'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(p)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$p'.", SourceLocation.Unknown)
      }
    }

    if (append) {
      Files.write(p, s.getBytes, StandardOpenOption.APPEND)
    } else {
      Files.write(p, s.getBytes)
    }
  }

  def readFile(p: Path): Result[String, String] = {
    if (!Files.exists(p)) {
      return Result.Err("File does not exist")
    }

    if (!Files.isRegularFile(p, LinkOption.NOFOLLOW_LINKS)) {
      return Result.Err("File is not regular")
    }

    if (!Files.isReadable(p)) {
      return Result.Err("File is not readable")
    }

    Result.Ok(Files.readString(p))

  }

  /**
   * Writes the given json `j` to the given file path `p`.
   *
   * Creates the parent directory of `p` if needed.
   */
  def writeJSON(p: Path, j: JValue): Unit = {
    FileOps.writeString(p, JsonMethods.pretty(JsonMethods.render(j)))
  }

  /**
    * Returns all files ending with `.flix` in `path`.
    *
    * The search is limited at `depth - 1` levels of subdirectories.
    *
    * E.g., if `depth = 1` then given the directory structure below,
    * `Subfile.flix` will not be included.
    *
    * {{{
    * path
    * ├── Main.flix
    * └── subdir
    *     └── Subfile.flix
    * }}}
    *
    *
    */
  def getFlixFilesIn(path: String, depth: Int): List[Path] = {
    Files.walk(Paths.get(path), depth)
      .iterator().asScala
      .filter(checkExt(_, "flix"))
      .toList.sorted
  }

  /**
    * Returns an iterator of all files in the given path, visited recursively.
    * The depth parameter is the maximum number of levels of directories to visit.
    *   Use a depth of 0 to only visit the given directory.
    *   Use a depth of 1 to only visit the files in the given directory.
    *   Use a depth of MaxValue to visit all files in the directory and its subdirectories.
    */
  def getFilesIn(path: Path, depth: Int): List[Path] = {
    if (Files.exists(path) && Files.isDirectory(path))
      Files.walk(path, depth).iterator()
        .asScala
        .filter(Files.isRegularFile(_))
        .toList
    else
      List.empty
  }

  /**
    * Checks if the given path is a regular file with the expected extension.
    */
  def checkExt(p: Path, expectedExt: String): Boolean = {
    val ext = if (expectedExt.startsWith(".")) expectedExt else s".$expectedExt"
    Files.isRegularFile(p) && p.getFileName.toString.endsWith(ext)
  }
}
