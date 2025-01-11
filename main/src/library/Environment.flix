/*
 * Copyright 2021 Ramiro Calle
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
eff Environment {

    ///
    /// Returns the arguments passed to the program via the command line.
    ///
    def getArgs(): List[String]

    ///
    /// Returns an map of the current system environment.
    ///
    def getEnv(): Map[String, String]

    ///
    /// Returns the value of the specified environment variable.
    ///
    def getVar(name: String): Option[String]

    ///
    /// Returns the system property by name.
    ///
    def getProp(name: String): Option[String]

    ///
    /// Returns the operating system name
    ///
    def getOsName(): Option[String]

    ///
    ///  Returns the operating system architecture
    ///
    def getOsArch(): Option[String]

    ///
    ///  Returns the operating system version
    ///
    def getOsVersion(): Option[String]

    ///
    ///  Returns the file separator
    ///
    def getFileSeparator(): String

    ///
    /// Returns the path separator
    ///
    def getPathSeparator(): String

    ///
    /// Returns the system line separator
    ///
    def getLineSeparator(): String

    ///
    /// Returns the user's current working directory
    ///
    def getCurrentWorkingDirectory(): Option[String]

    ///
    /// Returns the default temp file path
    ///
    def getTemporaryDirectory(): Option[String]

    ///
    /// Returns the user's account name
    ///
    def getUserName(): Option[String]

    ///
    /// Returns the user's home directory
    ///
    def getUserHomeDirectory(): Option[String]

    ///
    /// Returns the number of virtual processors available to the JVM.
    ///
    /// The returned value is never less than one.
    ///
    def getVirtualProcessors(): Int32

}

mod Environment {

    import dev.flix.runtime.Global
    import java.lang.Exception
    import java.lang.Runtime
    import java.lang.System
    import java.io.{File => JFile}
    import java.util.{Iterator => JIterator}
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{Map$Entry => Entry}

    ///
    /// Handles the `Environment` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Environment` effect using the `Env`, `Sys` and `IO` effects.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Environment) + {Env, Sys, IO} = x ->
        run {
            f(x)
        } with handler Environment {
            def getArgs(k) =
                k(unsafe Array.toList(Global.getArgs()))

            def getEnv(k) = region rc {
                try {
                    let _ = rc;
                    let iter = unchecked_cast ((System.getenv().entrySet().iterator()) as _ \ rc);
                    k(getEnvHelper(iter, Map.empty()))
                } catch {
                    case _: Exception => k(Map.empty())
                }
            }

            def getVar(name, k) =
                try {
                    let result = unsafe System.getenv(name);
                    k(Object.toOption(result))
                } catch {
                    case _: Exception => k(None)
                }

            def getProp(name, k) = k(getPropHelper(name))

            def getOsName(k) = k(getPropHelper("os.name"))

            def getOsArch(k) = k(getPropHelper("os.arch"))

            def getOsVersion(k) = k(getPropHelper("os.version"))

            def getFileSeparator(k) = k(unsafe JFile.separator)

            def getPathSeparator(k) = k(unsafe JFile.pathSeparator)

            def getLineSeparator(k) = k(unsafe System.lineSeparator())

            def getCurrentWorkingDirectory(k) = k(getPropHelper("user.dir"))

            def getTemporaryDirectory(k) = k(getPropHelper("java.io.tmpdir"))

            def getUserName(k) = k(getPropHelper("user.name"))

            def getUserHomeDirectory(k) = k(getPropHelper("user.home"))

            def getVirtualProcessors(k) = k(unsafe Runtime.getRuntime().availableProcessors())
        }

    ///
    /// Runs the `Environment` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Environment` effect using the `Env`, `Sys` and `IO` effects.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Environment) + {Env, Sys, IO} = handle(f)()

    def getPropHelper(name: String): Option[String] \ {Env, Sys, IO} =
            try {
                let result = System.getProperty(name);
                Object.toOption(result)
            } catch {
                case _: Exception => None
            }

    def getEnvHelper(iter: JIterator, m: Map[String, String]): Map[String, String] =
        if (unsafe iter.hasNext())
            let e = unsafe unchecked_cast(iter.next() as Entry);
            let k = unsafe unchecked_cast(e.getKey() as String);
            let v = unsafe unchecked_cast(e.getValue() as String);
            getEnvHelper(iter, Map.insert(k, v, m))
        else
            m

}
