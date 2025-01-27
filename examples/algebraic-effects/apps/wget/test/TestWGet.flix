/*
 * Copyright 2024 Jakob Schneider
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestWGet {

    @Test
    def downloadWritesTheOutputReceivedFromHttp(): Bool = {
        run {
            let opts = { targetFile = "myfile" };
            let url = "flix.dev";
            WGet.download(opts, url);
            false
        } with Http {
            def request(_, url, _, _, k) = k(Http.Response.Response({ status = 200, headers = Map.empty(), body = "this is the content" }))
        } with FileWrite {
            def write(data, _, _) =
                Assert.eq("this is the content", data#str)
            def writeLines(data, f, k) = ???
            def writeBytes(data, f, k) = ???
            def append(data, f, k) = ???
            def appendLines(data, f, k) = ???
            def appendBytes(data, f, k) = ???
            def truncate(f, k) = ???
            def mkDir(d, k) = ???
            def mkDirs(d, k) = ???
        }
    }

    @Test
    def downloadShouldWriteToGivenFile(): Bool = {
        run {
            let opts = { targetFile = "myfile" };
            let url = "flix.dev";
            WGet.download(opts, url);
            false
        } with Http {
            def request(_, url, _, _, k) = k(Http.Response.Response({ status = 200, headers = Map.empty(), body = "this is the content" }))
        } with FileWrite {
            def write(_, f, _) =
                Assert.eq("myfile", f)
            def writeLines(data, f, k) = ???
            def writeBytes(data, f, k) = ???
            def append(data, f, k) = ???
            def appendLines(data, f, k) = ???
            def appendBytes(data, f, k) = ???
            def truncate(f, k) = ???
            def mkDir(d, k) = ???
            def mkDirs(d, k) = ???
        }
    }

}
