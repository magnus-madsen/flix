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

import ca.uwaterloo.flix.api.Bootstrap.{getArtifactDirectory, getManifestFile, getPkgFile}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.pkg.FlixPackageManager.findFlixDependencies
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, JarPackageManager, Manifest, ManifestParser, MavenPackageManager, PackageModules, ReleaseError}
import ca.uwaterloo.flix.tools.Tester
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.flatMapN
import ca.uwaterloo.flix.util.{Formatter, Result, Validation}

import java.io.PrintStream
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import java.util.{Calendar, GregorianCalendar}
import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Using}


object Bootstrap {

  /**
    * Initializes a new flix project at the given path `p`.
    *
    * The project must not already exist.
    */
  def init(p: Path)(implicit out: PrintStream): Validation[Unit, BootstrapError] = {
    //
    // Check that the current working directory is usable.
    //
    if (!Files.isDirectory(p) || !Files.isReadable(p) || !Files.isWritable(p)) {
      return Validation.Failure(BootstrapError.FileError(s"The directory: '$p' is not accessible. Aborting."))
    }

    //
    // Compute the name of the package based on the directory name.
    //
    val packageName = getPackageName(p)

    //
    // Compute all the directories and files we intend to create.
    //
    val sourceDirectory = getSourceDirectory(p)
    val testDirectory = getTestDirectory(p)

    val manifestFile = getManifestFile(p)
    val gitignoreFile = getGitIgnoreFile(p)
    val licenseFile = getLicenseFile(p)
    val readmeFile = getReadmeFile(p)
    val mainSourceFile = getMainSourceFile(p)
    val mainTestFile = getMainTestFile(p)

    //
    // Create the project directories and files.
    //
    newDirectoryIfAbsent(sourceDirectory)
    newDirectoryIfAbsent(testDirectory)

    newFileIfAbsent(manifestFile) {
      s"""[package]
         |name        = "$packageName"
         |description = "test"
         |version     = "0.1.0"
         |flix        = "${Version.CurrentVersion}"
         |authors     = ["John Doe <john@example.com>"]
         |""".stripMargin
    }

    newFileIfAbsent(gitignoreFile) {
      s"""*.fpkg
         |*.jar
         |.GITHUB_TOKEN
         |artifact/
         |build/
         |lib/
         |crash_report_*.txt
         |""".stripMargin
    }

    newFileIfAbsent(licenseFile) {
      """Enter license information here.
        |""".stripMargin
    }

    newFileIfAbsent(readmeFile) {
      s"""# $packageName
         |
         |Enter some useful information.
         |
         |""".stripMargin
    }

    newFileIfAbsent(mainSourceFile) {
      """// The main entry point.
        |def main(): Unit \ IO =
        |    println("Hello World!")
        |""".stripMargin
    }

    newFileIfAbsent(mainTestFile) {
      """@Test
        |def test01(): Bool = 1 + 1 == 2
        |""".stripMargin
    }
    Validation.Success(())
  }

  /**
    * Returns the path to the artifact directory relative to the given path `p`.
    */
  private def getArtifactDirectory(p: Path): Path = p.resolve("./artifact/").normalize()

  /**
    * Returns the path to the library directory relative to the given path `p`.
    */
  def getLibraryDirectory(p: Path): Path = p.resolve("./lib/").normalize()

  /**
    * Returns the path to the source directory relative to the given path `p`.
    */
  private def getSourceDirectory(p: Path): Path = p.resolve("./src/").normalize()

  /**
    * Returns the path to the test directory relative to the given path `p`.
    */
  private def getTestDirectory(p: Path): Path = p.resolve("./test/").normalize()

  /**
    * Returns the path to the build directory relative to the given path `p`.
    */
  private def getBuildDirectory(p: Path): Path = p.resolve("./build/").normalize()

  /**
    * Returns the directory of the output .class-files relative to the given path `p`.
    */
  private def getClassDirectory(p: Path): Path = getBuildDirectory(p).resolve("./class/")

  /**
    * Returns the path to the LICENSE file relative to the given path `p`.
    */
  private def getLicenseFile(p: Path): Path = p.resolve("./LICENSE.md").normalize()

  /**
    * Returns the path to the README file relative to the given path `p`.
    */
  private def getReadmeFile(p: Path): Path = p.resolve("./README.md").normalize()

  /**
    * Returns the path to the main source file relative to the given path `p`.
    */
  private def getMainSourceFile(p: Path): Path = getSourceDirectory(p).resolve("./Main.flix").normalize()

  /**
    * Returns the path to the main test file relative to the given path `p`.
    */
  private def getMainTestFile(p: Path): Path = getTestDirectory(p).resolve("./TestMain.flix").normalize()

  /**
    * Returns the path to the Manifest file relative to the given path `p`.
    */
  private def getManifestFile(p: Path): Path = p.resolve("./flix.toml").normalize()

  /**
    * Returns the path to the .gitignore file relative to the given path `p`.
    */
  private def getGitIgnoreFile(p: Path): Path = p.resolve("./.gitignore").normalize()

  /**
    * Returns the path to the jar file based on the given path `p`.
    */
  private def getJarFile(p: Path): Path = getArtifactDirectory(p).resolve(getPackageName(p) + ".jar").normalize()

  /**
    * Returns the package name based on the given path `p`.
    */
  private def getPackageName(p: Path): String = p.toAbsolutePath.normalize().getFileName.toString

  /**
    * Returns the path to the pkg file based on the given path `p`.
    */
  private def getPkgFile(p: Path): Path = getArtifactDirectory(p).resolve(getPackageName(p) + ".fpkg").normalize()

  /**
    * Returns `true` if the given path `p` is a jar-file.
    */
  private def isJarFile(p: Path): Boolean = p.getFileName.toString.endsWith(".jar") && isZipArchive(p)

  /**
    * Returns `true` if the given path `p` is a fpkg-file.
    */
  def isPkgFile(p: Path): Boolean = p.getFileName.toString.endsWith(".fpkg") && isZipArchive(p)

  /**
    * Creates a new directory at the given path `p`.
    *
    * The path must not already contain a non-directory.
    */
  private def newDirectoryIfAbsent(p: Path)(implicit out: PrintStream): Unit = {
    if (!Files.exists(p)) {
      out.println(s"Creating '$p'.")
      Files.createDirectories(p)
    }
  }

  /**
    * Creates a new text file at the given path `p` with the given content `s` if the file does not already exist.
    */
  private def newFileIfAbsent(p: Path)(s: String)(implicit out: PrintStream): Unit = {
    if (!Files.exists(p)) {
      out.println(s"Creating '$p'.")
      Files.writeString(p, s, StandardOpenOption.CREATE)
    }
  }

  /**
    * To support DOS time, Java 8+ treats dates before the 1980 January in special way.
    * Here we use 2014-06-27 (the date of the first commit to Flix) to avoid the complexity introduced by this hack.
    *
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-4759491">JDK-4759491 that introduced the hack around 1980 January from Java 8+</a>
    * @see <a href="https://bugs.openjdk.java.net/browse/JDK-6303183">JDK-6303183 that explains why the second should be even to create ZIP files in platform-independent way</a>
    * @see <a href="https://github.com/gradle/gradle/blob/445deb9aa988e506120b7918bf91acb421e429ba/subprojects/core/src/main/java/org/gradle/api/internal/file/archive/ZipCopyAction.java#L42-L57">A similar case from Gradle</a>
    */
  private val ENOUGH_OLD_CONSTANT_TIME: Long = new GregorianCalendar(2014, Calendar.JUNE, 27, 0, 0, 0).getTimeInMillis

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, p: Path): Unit = {
    if (Files.exists(p)) {
      addToZip(zip, name, Files.readAllBytes(p))
    }
  }

  /**
    * Adds an entry to the given zip file.
    */
  private def addToZip(zip: ZipOutputStream, name: String, d: Array[Byte]): Unit = {
    val entry = new ZipEntry(name)
    entry.setTime(ENOUGH_OLD_CONSTANT_TIME)
    zip.putNextEntry(entry)
    zip.write(d)
    zip.closeEntry()
  }

  /**
    * Returns `true` if the given path `p` is a zip-archive.
    */
  private def isZipArchive(p: Path): Boolean = {
    if (Files.exists(p) && Files.isReadable(p) && Files.isRegularFile(p)) {
      // Read the first four bytes of the file.
      return Using(Files.newInputStream(p)) { is =>
        val b1 = is.read()
        val b2 = is.read()
        val b3 = is.read()
        val b4 = is.read()
        // Check if the four first bytes match 0x50, 0x4b, 0x03, 0x04
        return b1 == 0x50 && b2 == 0x4b && b3 == 0x03 && b4 == 0x04
      }.get
    }
    false
  }

  /**
    * @param root the root directory to compute a relative path from the given path
    * @param path the path to be converted to a relative path based on the given root directory
    * @return relative file name separated by slashes, like `path/to/file.ext`
    */
  private def convertPathToRelativeFileName(root: Path, path: Path): String =
    root.relativize(path).toString.replace('\\', '/')

  /**
    * Returns all files in the given path `p` ending with .`ext`.
    */
  private def getAllFilesWithExt(p: Path, ext: String): List[Path] =
    getAllFiles(p).filter(p => p.getFileName.toString.endsWith(s".$ext"))

  /**
    * Returns all files in the given path `p`.
    */
  private def getAllFiles(p: Path): List[Path] = {
    if (Files.isReadable(p) && Files.isDirectory(p)) {
      val visitor = new FileVisitor
      Files.walkFileTree(p, visitor)
      visitor.result.toList
    } else {
      Nil
    }
  }

  /**
    * Returns all .flix files directly in the directory given by `p`.
    */
  private def getAllFlixFilesHere(path: Path): List[Path] = {
    val files = path.toFile.listFiles()
    if (files == null) {
      List.empty
    } else {
      files.toList.map(f => f.toPath).filter(p => p.getFileName.toString.endsWith(s".flix"))
    }
  }

  private class FileVisitor extends SimpleFileVisitor[Path] {
    val result: mutable.ListBuffer[Path] = mutable.ListBuffer.empty

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      result += file
      FileVisitResult.CONTINUE
    }
  }

  /**
    * Creates a new Bootstrap object and initializes it.
    * If a `flix.toml` file exists, parses that to a Manifest and
    * downloads all required files. Otherwise checks the /lib folder
    * to see what dependencies are already downloaded. Also finds
    * all .flix source files.
    * Then returns the initialized Bootstrap object or an error.
    */
  def bootstrap(path: Path, apiKey: Option[String])(implicit formatter: Formatter, out: PrintStream): Validation[Bootstrap, BootstrapError] = {
    //
    // Determine the mode: If `path/flix.toml` exists then "project" mode else "folder mode".
    //
    val bootstrap = new Bootstrap(path, apiKey)
    val tomlPath = getManifestFile(path)
    if (Files.exists(tomlPath)) {
      out.println(s"Found `${formatter.blue("flix.toml")}`. Checking dependencies...")
      Validation.mapN(bootstrap.projectMode())(_ => bootstrap)
    } else {
      out.println(s"""No `${formatter.blue("flix.toml")}`. Will load source files from `${formatter.blue("*.flix")}`, `${formatter.blue("src/**")}`, and `${formatter.blue("test/**")}`.""")
      Validation.mapN(bootstrap.folderMode())(_ => bootstrap)
    }
  }
}

class Bootstrap(val projectPath: Path, apiKey: Option[String]) {

  // The `flix.toml` manifest if in project mode, otherwise `None`
  private var optManifest: Option[Manifest] = None

  // Timestamps at the point the sources were loaded
  private var timestamps: Map[Path, Long] = Map.empty

  // Lists of paths to the source files, flix packages and .jar files used
  private var sourcePaths: List[Path] = List.empty
  private var flixPackagePaths: List[Path] = List.empty
  private var mavenPackagePaths: List[Path] = List.empty
  private var jarPackagePaths: List[Path] = List.empty

  /**
    * Parses `flix.toml` to a Manifest and downloads all required files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def projectMode()(implicit formatter: Formatter, out: PrintStream): Validation[Unit, BootstrapError] = {
    // 1. Read, parse, and validate flix.toml.
    val tomlPath = Bootstrap.getManifestFile(projectPath)
    val manifest = ManifestParser.parse(tomlPath) match {
      case Ok(m) => m
      case Err(e) => return Validation.Failure(BootstrapError.ManifestParseError(e))
    }
    optManifest = Some(manifest)

    // 2. Check each dependency is available or download it.
    val manifests: List[Manifest] = FlixPackageManager.findTransitiveDependencies(manifest, projectPath, apiKey) match {
      case Ok(l) => l
      case Err(e) => return Validation.Failure(BootstrapError.FlixPackageError(e))
    }
    FlixPackageManager.installAll(manifests, projectPath, apiKey) match {
      case Ok(l) => flixPackagePaths = l
      case Err(e) => return Validation.Failure(BootstrapError.FlixPackageError(e))
    }
    MavenPackageManager.installAll(manifests, projectPath) match {
      case Ok(l) => mavenPackagePaths = l
      case Err(e) => return Validation.Failure(BootstrapError.MavenPackageError(e))
    }
    JarPackageManager.installAll(manifests, projectPath) match {
      case Ok(l) => jarPackagePaths = l
      case Err(e) => return Validation.Failure(BootstrapError.JarPackageError(e))
    }
    out.println("Dependency resolution completed.")

    // 3. Add *.flix, src/**.flix and test/**.flix
    val filesHere = Bootstrap.getAllFlixFilesHere(projectPath)
    val filesSrc = Bootstrap.getAllFilesWithExt(Bootstrap.getSourceDirectory(projectPath), "flix")
    val filesTest = Bootstrap.getAllFilesWithExt(Bootstrap.getTestDirectory(projectPath), "flix")
    sourcePaths = filesHere ++ filesSrc ++ filesTest

    Validation.Success(())
  }

  /**
    * Checks the /lib folder to find existing flix packages and .jar files.
    * Then makes a list of all flix source files, flix packages
    * and .jar files that this project uses.
    */
  private def folderMode(): Validation[Unit, BootstrapError] = {
    // 1. Add *.flix, src/**.flix and test/**.flix
    val filesHere = Bootstrap.getAllFlixFilesHere(projectPath)
    val filesSrc = Bootstrap.getAllFilesWithExt(Bootstrap.getSourceDirectory(projectPath), "flix")
    val filesTest = Bootstrap.getAllFilesWithExt(Bootstrap.getTestDirectory(projectPath), "flix")
    sourcePaths = filesHere ++ filesSrc ++ filesTest

    // 2. Grab all jars in lib/external
    val mavenFilesLib = Bootstrap.getAllFilesWithExt(Bootstrap.getLibraryDirectory(projectPath).resolve(MavenPackageManager.FolderName), "jar")
    mavenPackagePaths = mavenFilesLib

    // 3. Grab all jars in lib/external
    val jarFilesLib = Bootstrap.getAllFilesWithExt(Bootstrap.getLibraryDirectory(projectPath).resolve(JarPackageManager.FolderName), "jar")
    jarPackagePaths = jarFilesLib

    // 3. Grab all flix packages in lib/
    val flixFilesLib = Bootstrap.getAllFilesWithExt(Bootstrap.getLibraryDirectory(projectPath), "fpkg")
    flixPackagePaths = flixFilesLib

    Validation.Success(())
  }

  /**
    * Checks to see if any source files or packages have been changed.
    * If they have, they are added to flix. Then updates the timestamps
    * map to reflect the current source files and packages.
    */
  def reconfigureFlix(flix: Flix): Unit = {
    val previousSources = timestamps.keySet

    implicit val defaultSctx: SecurityContext = SecurityContext.AllPermissions

    for (path <- sourcePaths if hasChanged(path)) {
      flix.addFlix(path)
    }

    for (path <- flixPackagePaths if hasChanged(path)) {
      flix.addPkg(path)
    }

    for (path <- mavenPackagePaths if hasChanged(path)) {
      flix.addJar(path)
    }

    for (path <- jarPackagePaths if hasChanged(path)) {
      flix.addJar(path)
    }

    val currentSources = (sourcePaths ++ flixPackagePaths ++ mavenPackagePaths ++ jarPackagePaths).filter(p => Files.exists(p))

    val deletedSources = previousSources -- currentSources
    for (path <- deletedSources) {
      flix.remSourceCode(path.toString)
    }

    timestamps = currentSources.map(f => f -> f.toFile.lastModified).toMap
  }

  /**
    * Returns true if the timestamp of the given source file has changed since the last reload.
    */
  private def hasChanged(file: Path) = {
    !(timestamps contains file) || (timestamps(file) != file.toFile.lastModified())
  }

  /**
    * Type checks the source files for the project.
    */
  def check(flix: Flix): Validation[TypedAst.Root, BootstrapError] = {
    // Add sources and packages.
    reconfigureFlix(flix)

    val (optRoot, errors) = flix.check()
    if (errors.isEmpty) {
      Validation.Success(optRoot.get)
    } else {
      Validation.Failure(BootstrapError.GeneralError(flix.mkMessages(errors)))
    }
  }

  /**
    * Builds (compiles) the source files for the project.
    */
  def build(flix: Flix): Validation[CompilationResult, BootstrapError] = {
    // Configure a new Flix object.
    val newOptions = flix.options.copy(output = Some(Bootstrap.getBuildDirectory(projectPath)))
    flix.setOptions(newOptions)

    // Add sources and packages.
    reconfigureFlix(flix)

    flix.compile().toResult match {
      case Result.Ok(r: CompilationResult) =>
        Validation.Success(r)
      case Result.Err(errors) =>
        Validation.Failure(BootstrapError.GeneralError(flix.mkMessages(errors.toList)))
    }
  }

  /**
    * Builds a jar package for the project.
    */
  def buildJar()(implicit formatter: Formatter): Validation[Unit, BootstrapError] = {

    // The path to the jar file.
    val jarFile = Bootstrap.getJarFile(projectPath)

    // Create the artifact directory, if it does not exist.
    Files.createDirectories(getArtifactDirectory(projectPath))

    // Check whether it is safe to write to the file.
    if (Files.exists(jarFile) && !Bootstrap.isJarFile(jarFile)) {
      return Validation.Failure(BootstrapError.FileError(s"The path '${formatter.red(jarFile.toString)}' exists and is not a jar-file. Refusing to overwrite."))
    }

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(jarFile))) { zip =>
      // META-INF/MANIFEST.MF
      val manifest =
        """Manifest-Version: 1.0
          |Main-Class: Main
          |""".stripMargin

      // Add manifest file.
      Bootstrap.addToZip(zip, "META-INF/MANIFEST.MF", manifest.getBytes)

      // Add all class files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((buildFile, fileNameWithSlashes) <- Bootstrap.getAllFiles(Bootstrap.getClassDirectory(projectPath))
        .map { path => (path, Bootstrap.convertPathToRelativeFileName(Bootstrap.getClassDirectory(projectPath), path)) }
        .sortBy(_._2)) {
        Bootstrap.addToZip(zip, fileNameWithSlashes, buildFile)
      }
    } match {
      case Success(()) => Validation.Success(())
      case Failure(e) => Validation.Failure(BootstrapError.GeneralError(List(e.getMessage)))
    }
  }

  /**
    * Builds a fatjar package for the project.
    * This function relies essentially on the same pattern as used in the buildJar function.
    * It searches dependencies in the lib folder and includes everything in the generated jar file in addition.
    *
    * Note: As the buildJar does the same, this build doesn't erase previous jar-file if existing.
    * It doesn't do a cleanup and as such remaining previous libraries may remain.
    */
  def buildFatJar()(implicit formatter: Formatter): Validation[Unit, BootstrapError] = {
    // The path to the jar file.
    val jarFile = Bootstrap.getJarFile(projectPath)

    // Create the artifact directory, if it does not exist.
    Files.createDirectories(getArtifactDirectory(projectPath))

    // Check whether it is safe to write to the file.
    if (Files.exists(jarFile) && !Bootstrap.isJarFile(jarFile)) {
      return Validation.Failure(BootstrapError.FileError(s"The path '${formatter.red(jarFile.toString)}' exists and is not a jar-file. Refusing to overwrite."))
    }

    // Get the lib folder and check if it is safe to read.
    val libFolder = Bootstrap.getLibraryDirectory(projectPath)
    if (Files.exists(libFolder) && (!Files.isDirectory(libFolder) || !Files.isReadable(libFolder))) {
      return Validation.Failure(BootstrapError.FileError(s"The lib folder isn't a directory or isn't readable. Refusing to build fatjar-file."))
    }

    // First, we get all jar files inside the lib folder.
    // If the lib folder doesn't exist, we suppose there is simply no dependency and trigger no error.
    val jarDependencies = if (libFolder.toFile.exists()) Bootstrap.getAllFilesWithExt(libFolder, "jar") else List[Path]()

    // Construct a new zip file, the built fatjar-file.
    Using(new ZipOutputStream(Files.newOutputStream(jarFile))) { zipOut =>
      // META-INF/MANIFEST.MF
      val manifest =
        """Manifest-Version: 1.0
          |Main-Class: Main
          |""".stripMargin

      // Add manifest file.
      Bootstrap.addToZip(zipOut, "META-INF/MANIFEST.MF", manifest.getBytes)

      // Add class files of the project.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((buildFile, fileNameWithSlashes) <- Bootstrap.getAllFiles(Bootstrap.getClassDirectory(projectPath))
        .map { path => (path, Bootstrap.convertPathToRelativeFileName(Bootstrap.getClassDirectory(projectPath), path)) }
        .sortBy(_._2)) {
        Bootstrap.addToZip(zipOut, fileNameWithSlashes, buildFile)
      }

      // Add jar dependencies.
      jarDependencies.foreach(dep => {
        if (!Bootstrap.isJarFile(dep))
          return Validation.Failure(BootstrapError.FileError(s"The jar file '${dep.toFile.getName} seems corrupted. Refusing to build fatjar-file."))

        // Extract the content of the classes to the jar file.
        Using(new ZipInputStream(Files.newInputStream(dep))) {
          zipIn =>
            var entry = zipIn.getNextEntry
            while (entry != null) {
              // Get the class files except module-info and META-INF classes which are specific to each library.
              if (entry.getName.endsWith(".class") && !entry.getName.equals("module-info.class") && !entry.getName.contains("META-INF/")) {
                // Write extracted class file to zip.
                val classContent = zipIn.readAllBytes()
                Bootstrap.addToZip(zipOut, entry.getName, classContent)
              }
              entry = zipIn.getNextEntry
            }
        }
      })
    } match {
      case Success(()) => Validation.Success(())
      case Failure(e) => Validation.Failure(BootstrapError.GeneralError(List(e.getMessage)))
    }
  }

  /**
    * Builds a flix package for the project.
    */
  def buildPkg()(implicit formatter: Formatter): Validation[Unit, BootstrapError] = {

    // Check that there is a `flix.toml` file.
    if (!Files.exists(getManifestFile(projectPath))) {
      return Validation.Failure(BootstrapError.FileError(s"Cannot create a Flix package without a `${formatter.red("flix.toml")}` file."))
    }

    // Create the artifact directory, if it does not exist.
    Files.createDirectories(getArtifactDirectory(projectPath))

    // The path to the fpkg file.
    val pkgFile = Bootstrap.getPkgFile(projectPath)

    // Check whether it is safe to write to the file.
    if (Files.exists(pkgFile) && !Bootstrap.isPkgFile(pkgFile)) {
      return Validation.Failure(BootstrapError.FileError(s"The path '${formatter.red(pkgFile.toString)}' exists and is not a fpkg-file. Refusing to overwrite."))
    }

    // Copy the `flix.toml` to the artifact directory.
    Files.copy(getManifestFile(projectPath), getArtifactDirectory(projectPath).resolve("flix.toml"), StandardCopyOption.REPLACE_EXISTING)

    // Construct a new zip file.
    Using(new ZipOutputStream(Files.newOutputStream(pkgFile))) { zip =>
      // Add required resources.
      Bootstrap.addToZip(zip, "flix.toml", Bootstrap.getManifestFile(projectPath))
      Bootstrap.addToZip(zip, "LICENSE.md", Bootstrap.getLicenseFile(projectPath))
      Bootstrap.addToZip(zip, "README.md", Bootstrap.getReadmeFile(projectPath))

      // Add all source files.
      // Here we sort entries by relative file name to apply https://reproducible-builds.org/
      for ((sourceFile, fileNameWithSlashes) <- Bootstrap.getAllFiles(Bootstrap.getSourceDirectory(projectPath))
        .map { path => (path, Bootstrap.convertPathToRelativeFileName(projectPath, path)) }
        .sortBy(_._2)) {
        Bootstrap.addToZip(zip, fileNameWithSlashes, sourceFile)
      }
    } match {
      case Success(()) => Validation.Success(())
      case Failure(e) => Validation.Failure(BootstrapError.FileError(e.getMessage))
    }
  }

  /**
    * Generates API documentation.
    */
  def doc(flix: Flix): Validation[Unit, BootstrapError] = {
    // Add sources and packages.
    reconfigureFlix(flix)

    val packageModules = optManifest match {
      case None => PackageModules.All
      case Some(manifest) => manifest.modules
    }

    val (result, errors) = flix.check()
    if (errors.isEmpty) {
      result.foreach(HtmlDocumentor.run(_, packageModules)(flix))
      Validation.Success(())
    } else {
      Validation.Failure(BootstrapError.GeneralError(flix.mkMessages(errors)))
    }
  }

  /**
    * Runs the main function in flix package for the project.
    */
  def run(flix: Flix, args: Array[String]): Validation[Unit, BootstrapError] = {
    Validation.mapN(build(flix)) {
      _.getMain match {
        case None => ()
        case Some(main) => main(args)
      }
    }
  }

  /**
    * Runs all tests in the flix package for the project.
    */
  def test(flix: Flix): Validation[Unit, BootstrapError] = {
    flatMapN(build(flix)) {
      compilationResult =>
        Tester.run(Nil, compilationResult)(flix) match {
          case Ok(_) => Validation.Success(())
          case Err(_) => Validation.Failure(BootstrapError.GeneralError(List("Tester Error")))
        }
    }
  }

  /**
    * Package the current project and release it on GitHub.
    */
  def release(flix: Flix)(implicit out: PrintStream): Validation[Unit, BootstrapError] = {
    implicit val formatter: Formatter = flix.getFormatter

    // Ensure that we have a manifest
    val manifest = optManifest match {
      case Some(m) => m
      case None => return Validation.Failure(BootstrapError.ReleaseError(ReleaseError.MissingManifest))
    }

    // Check if `github` option is present
    val githubRepo = manifest.repository match {
      case Some(r) => r
      case None => return Validation.Failure(BootstrapError.ReleaseError(ReleaseError.MissingRepository))
    }

    // Check if `--github-token` option is present
    val githubToken = flix.options.githubToken match {
      case Some(k) => k
      case None => return Validation.Failure(BootstrapError.ReleaseError(ReleaseError.MissingApiKey))
    }

    if (!flix.options.assumeYes) {
      // Ask for confirmation
      out.print(s"Release ${formatter.blue(s"github:$githubRepo")} ${formatter.yellow(s"v${manifest.version}")}? [y/N]: ")
      val response = readLine()
      response.toLowerCase match {
        case "y" => // Continue
        case "yes" => // Continue
        case _ => return Validation.Failure(BootstrapError.ReleaseError(ReleaseError.Cancelled))
      }
    }

    // Build artifacts
    out.println("Building project...")
    val buildResult = buildPkg()
    buildResult.toResult match {
      case Result.Ok(_) => // Continue
      case Result.Err(e) => return Validation.Failure(e)
    }

    // Publish to GitHub
    out.println("Publishing a new release...")
    val artifacts = List(getPkgFile(projectPath), getManifestFile(projectPath))
    val publishResult = GitHub.publishRelease(githubRepo, manifest.version, artifacts, githubToken)
    publishResult match {
      case Ok(()) => // Continue
      case Err(e) => return Validation.Failure(BootstrapError.ReleaseError(e))
    }

    out.println(formatter.green(
      s"""
         |Successfully released v${manifest.version}
         |${formatter.underline(s"https://github.com/${githubRepo.owner}/${githubRepo.repo}/releases/tag/v${manifest.version}")}
         |""".stripMargin
    ))

    Validation.Success(())
  }

  /**
    * Show dependencies which have newer versions available.
    *
    * @return `true` if any outdated dependencies were found, `false` if everything is up to date.
    */
  def outdated(flix: Flix)(implicit out: PrintStream): Validation[Boolean, BootstrapError] = {
    implicit val formatter: Formatter = flix.getFormatter

    val flixDeps = optManifest.map(findFlixDependencies).getOrElse(Nil)

    val rows = flixDeps.flatMap { dep =>
      val updates = FlixPackageManager.findAvailableUpdates(dep, flix.options.githubToken) match {
        case Ok(u) => u
        case Err(e) => return Validation.Failure(BootstrapError.FlixPackageError(e))
      }

      if (updates.isEmpty)
        None
      else
        Some(List(
          s"${dep.username}/${dep.projectName}",
          dep.version.toString,
          updates.major.map(v => v.toString).getOrElse(""),
          updates.minor.map(v => v.toString).getOrElse(""),
          updates.patch.map(v => v.toString).getOrElse(""),
        ))
    }

    if (rows.isEmpty) {
      out.println(formatter.green(
        """
          |All dependencies are up to date
          |""".stripMargin
      ))
      Validation.Success(false)
    } else {
      out.println("")
      out.println(formatter.table(
        List("package", "current", "major", "minor", "patch"),
        List(formatter.blue, formatter.cyan, formatter.yellow, formatter.yellow, formatter.yellow),
        rows
      ))
      out.println("")
      Validation.Success(true)
    }
  }
}
