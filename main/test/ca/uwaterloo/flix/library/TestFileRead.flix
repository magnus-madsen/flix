/*
 * Copyright 2024 Holger Dal Mogensen
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

mod TestFileRead {

    use IoError.IoError
    use IoError.ErrorKind
    use Monad.{>>=}

    import java.io.IOException
    import java.nio.file.Path


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool \ IO = {
        (
            makeTempPath("flix-tmpfile-68976") >>= path ->
            FileWrite.handle(() -> FileWrite.write({ str = "content" }, path))() >>= _ ->
            FileRead.handle(() -> FileRead.exists(path))()
        ) == Ok(true)
    }

    @Test
    def exists02(): Bool \ IO = {
        FileRead.handle(() -> FileRead.exists("SomeFileThatDoesNotExist"))() == Ok(false)
    }

    @Test
    def exists03(): Bool \ IO = {
        match FileRead.handle(() -> FileRead.exists("Some\\Invalid:{\u0000Path}/"))() {
            case Ok(_) => false
            case Err(IoError(kind, _)) => kind == ErrorKind.InvalidPath
        }
    }


    ///
    /// Helper - make a filepath within the temp directory.
    /// Use `java.nio.file.Path` functions for platform independence.
    ///
    def makeTempPath(fileName: String): Result[IoError, String] = region rc {
        let tempdir = match Environment.getTemporaryDirectory() {
            case Some(dir)  => Ok(dir)
            case None       => Err(IoError(ErrorKind.Other, "No temporary directory"))
        };
        try {
            Result.flatMap(root ->  {
                let arr = List.toArray(rc, fileName :: Nil);
                let path = unsafely IO run Path.of(root, arr).normalize().toString();
                Ok(path)
            }, tempdir)
        } catch {
            case ex: IOException =>
                unsafely IO run Err(IoError(ErrorKind.Other, ex.getMessage()))
        }
    }

}
