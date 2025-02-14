/*
 *  Copyright 2024 Holger Dal Mogensen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// An effect used to read from the file system.
///
pub eff FileReadWithResult {

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    def exists(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` is the given file `f` is a directory.
    ///
    def isDirectory(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is a regular file.
    ///
    def isRegularFile(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is readable.
    ///
    def isReadable(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is a symbolic link.
    ///
    def isSymbolicLink(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given path `p` is a valid path on the current platform.
    ///
    def isValidPath(p: String): Bool

    ///
    /// Returns `true` if the given file `f` is writable.
    ///
    def isWritable(f: String): Result[IoError, Bool]

    ///
    /// Returns `true` if the given file `f` is executable.
    ///
    def isExecutable(f: String): Result[IoError, Bool]

    ///
    /// Returns the last access time of the given file `f` in milliseconds since the epoch.
    ///
    def accessTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the creation time of the given file `f` in milliseconds since the epoch.
    ///
    def creationTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
    ///
    def modificationTime(f: String): Result[IoError, Int64]

    ///
    /// Returns the size of the given file `f` in bytes.
    ///
    def size(f: String): Result[IoError, Int64]

    ///
    /// Returns a string of all lines in the given file `f`.
    ///
    def read(f: String): Result[IoError, String]

    ///
    /// Returns a list of all lines in the given file `f`.
    ///
    def readLines(f: String): Result[IoError, List[String]]

    ///
    /// Returns a vector of all the bytes in the given file `f`.
    ///
    def readBytes(f: String): Result[IoError, Vector[Int8]]

    ///
    /// Returns a list with the names of all files and directories in the given directory `d`.
    ///
    def list(f: String): Result[IoError, List[String]]

}

mod FileReadWithResult {

    import java.io.File
    import java.io.IOException
    import java.lang.Class
    import java.lang.{String => JString}
    import java.lang.UnsupportedOperationException
    import java.nio.file.attribute.BasicFileAttributes
    import java.nio.file.Files
    import java.nio.file.InvalidPathException
    import java.nio.file.LinkOption
    import java.nio.file.NotDirectoryException
    import java.nio.file.Path
    import java.nio.file.Paths

    use IoError.IoError
    use IoError.ErrorKind

    ///
    /// Handles the `FileReadWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileReadWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileReadWithResult) + IO = x ->
        run {
            f(x)
        } with handler FileReadWithResult {
            def exists(f, k) = {
                let res = try {
                    Ok(Files.exists(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isDirectory(f, k) = {
                let res = try {
                    Ok(Files.isDirectory(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isRegularFile(f, k) = {
                let res = try {
                    Ok(Files.isRegularFile(Paths.get(f), (...{}: Vector[LinkOption])))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isReadable(f, k) = {
                let res = try {
                    Ok(Files.isReadable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isSymbolicLink(f, k) = {
                let res = try {
                    Ok(Files.isSymbolicLink(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isValidPath(p, k) = {
                try {
                    Paths.get(p);
                    k(true)
                } catch {
                    case _: InvalidPathException => k(false)
                }
            }

            def isWritable(f, k) = {
                let res = try {
                    Ok(Files.isWritable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def isExecutable(f, k) = {
                let res = try {
                    Ok(Files.isExecutable(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                };
                k(res)
            }

            def accessTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).lastAccessTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def creationTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).creationTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def modificationTime(f, k) = {
                let res = try {
                    Ok(fileAttributes(Paths.get(f)).lastModifiedTime().toMillis())
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def size(f, k) = {
                let res = try {
                    Ok(Files.size(Paths.get(f)))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def read(f, k) = {
                let res = try {
                    Ok(new JString(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readLines(f, k) = {
                let res = try {
                    Ok(ToFlix.toFlix(Files.readAllLines(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def readBytes(f, k) = {
                let res = try {
                    Ok(Array.toVector(Files.readAllBytes(Paths.get(f))))
                } catch {
                    case ex: InvalidPathException => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def list(f, k) = {
                let res = try {
                    // TODO: Use java.nio.file.Files.list when we have support for Java generics.
                    let file = new File(f);
                    Ok(Array.toList(file.list()))
                } catch {
                    case ex: InvalidPathException  => Err(IoError(ErrorKind.InvalidPath, ex.getMessage()))
                    case ex: NotDirectoryException => Err(IoError(ErrorKind.NotDirectory, ex.getMessage()))
                    case ex: IOException           => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `FileReadWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileReadWithResult` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileReadWithResult) + {IO} = handle(f)()

    ///
    /// Returns the attributes of the given file `f`.
    /// May throw `IOException` and `SecurityException`.
    ///
    def fileAttributes(path: Path): BasicFileAttributes \ IO =
        Files.readAttributes(
            path,
            // "[...] invoking this method with a type parameter of BasicFileAttributes.class will not throw UnsupportedOperationException."
            Class.forName("java.nio.file.attribute.BasicFileAttributes"),
            (...{}: Vector[LinkOption])
        )

}
