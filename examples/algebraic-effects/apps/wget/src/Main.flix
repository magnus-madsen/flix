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

def main(): Unit \ IO + Net =
    let args = Environment.getArgs();
    match List.head(args) {
        case Some(url) =>
            let opts = { targetFile = "index.html" };
            let prog = () -> WGet.download(opts, url);
            match App.runAllWithResult(prog) {
                case Ok(()) => ()
                case Err(IoError.IoError(_, msg)) => println("An error occurred: '${msg}'")
            }
        case None => println("Please provide a URL")
    }
