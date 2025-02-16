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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.tools.pkg.Dependency.{FlixDependency, JarDependency, MavenDependency}
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import org.tomlj.*

import java.io.{IOException, StringReader}
import java.net.{URI, URL}
import java.nio.file.Path
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

object ManifestParser {
  /**
    * Regular expression defining a valid string for user name and project name.
    * Concretely, a valid name is a [[String]] consisting only of alphanumeric characters
    * or the symbols `.`,`:`,`/`,`_` and `-`.
    */
  private val ValidName = "[a-zA-Z0-9.:/_-]+".r

  /**
    * Creates a Manifest from the .toml file
    * at path `p` and returns an error if
    * there are parsing errors
    */
  def parse(p: Path): Result[Manifest, ManifestError] = {
    val parser = try {
      Toml.parse(p)
    } catch {
      case e: IOException => return Err(ManifestError.IOError(p, e.getMessage))
    }
    createManifest(parser, p)
  }

  /**
    * Creates a Manifest from the String `s`
    * which should have the .toml format and
    * returns an error if there are parsing
    * errors. The path `p` should be where `s`
    * comes from.
    */
  def parse(s: String, p: Path): Result[Manifest, ManifestError] = {
    val stringReader = new StringReader(s)
    val parser = try {
      Toml.parse(stringReader)
    } catch {
      case e: IOException => return Err(ManifestError.IOError(p, e.getMessage))
    }
    createManifest(parser, p)
  }

  /**
    * Creates a Manifest from the TomlParseResult
    * which should be at path `p` and returns an
    * error if there are parsing errors.
    */
  private def createManifest(parser: TomlParseResult, p: Path): Result[Manifest, ManifestError] = {
    val errors = parser.errors
    if (errors.size() > 0) {
      var errorString = ""
      errors.forEach(error => errorString = errorString + error.toString + ", ")
      return Err(ManifestError.ManifestParseError(p, errorString))
    }

    for (
      _ <- checkKeys(parser, p);

      name <- getRequiredStringProperty("package.name", parser, p);

      description <- getRequiredStringProperty("package.description", parser, p);

      version <- getRequiredStringProperty("package.version", parser, p);
      versionSemVer <- toFlixVer(version, p);

      repository <- getOptionalStringProperty("package.repository", parser, p);
      githubProject <- Result.traverseOpt(repository)(r => toGithubProject(r, p));

      modules <- getOptionalArrayProperty("package.modules", parser, p);
      moduleStrings <- Result.traverseOpt(modules)(m => convertTomlArrayToStringList(m, p));
      packageModules <- toPackageModules(moduleStrings, p);

      flix <- getRequiredStringProperty("package.flix", parser, p);
      flixSemVer <- toFlixVer(flix, p);

      license <- getOptionalStringProperty("package.license", parser, p);

      authors <- getRequiredArrayProperty("package.authors", parser, p);
      authorsList <- convertTomlArrayToStringList(authors, p);

      deps <- getOptionalTableProperty("dependencies", parser, p);
      depsList <- collectDependencies(deps, flixDep = true, jarDep = false, p);

      mvnDeps <- getOptionalTableProperty("mvn-dependencies", parser, p);
      mvnDepsList <- collectDependencies(mvnDeps, flixDep = false, jarDep = false, p);

      jarDeps <- getOptionalTableProperty("jar-dependencies", parser, p);
      jarDepsList <- collectDependencies(jarDeps, flixDep = false, jarDep = true, p)

    ) yield Manifest(name, description, versionSemVer, githubProject, packageModules, flixSemVer, license, authorsList, depsList ++ mvnDepsList ++ jarDepsList)
  }

  private def checkKeys(parser: TomlParseResult, p: Path): Result[Unit, ManifestError] = {
    val keySet: Set[String] = parser.keySet().asScala.toSet
    val allowedKeys = Set("package", "dependencies", "dev-dependencies", "mvn-dependencies", "dev-mvn-dependencies", "jar-dependencies")
    val illegalKeys = keySet.diff(allowedKeys)

    if (illegalKeys.nonEmpty) {
      return Err(ManifestError.IllegalTableFound(p, illegalKeys.head))
    }

    val dottedKeys = parser.dottedKeySet().asScala.toSet
    val packageKeys = dottedKeys.filter(s => s.startsWith("package."))
    val allowedPackageKeys = Set("package.name", "package.description", "package.version", "package.repository", "package.modules", "package.flix", "package.authors", "package.license")
    val illegalPackageKeys = packageKeys.diff(allowedPackageKeys)
    if (illegalPackageKeys.nonEmpty) {
      return Err(ManifestError.IllegalPackageKeyFound(p, illegalPackageKeys.head))
    }

    Ok(())
  }

  /**
    * Parses a String which should be at `propString`
    * and returns the String or an error if the result
    * cannot be found.
    */
  private def getRequiredStringProperty(propString: String, parser: TomlParseResult, p: Path): Result[String, ManifestError] = {
    try {
      val prop = parser.getString(propString)
      if (prop == null) {
        return Err(ManifestError.MissingRequiredProperty(p, propString, None))
      }
      Ok(prop)
    } catch {
      case e: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, propString, Some(e.getMessage)))
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "String", e.getMessage))
    }
  }

  /**
    * Parses a String which might be at `propString`
    * and returns the String as an Option.
    */
  private def getOptionalStringProperty(propString: String, parser: TomlParseResult, p: Path): Result[Option[String], ManifestError] = {
    try {
      val prop = parser.getString(propString)
      Ok(Option(prop))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "String", e.getMessage))
    }
  }

  /**
    * Parses an Array which should be at `propString`
    * and returns the Array or an error if the result
    * cannot be found.
    */
  private def getRequiredArrayProperty(propString: String, parser: TomlParseResult, p: Path): Result[TomlArray, ManifestError] = {
    try {
      val array = parser.getArray(propString)
      if (array == null) {
        return Err(ManifestError.MissingRequiredProperty(p, propString, None))
      }
      Ok(array)
    } catch {
      case e: IllegalArgumentException => Err(ManifestError.MissingRequiredProperty(p, propString, Some(e.getMessage)))
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "Array", e.getMessage))
    }
  }

  /**
    * Parses an Array which might be at `propString`
    * and returns the Array as an Option.
    */
  private def getOptionalArrayProperty(propString: String, parser: TomlParseResult, p: Path): Result[Option[TomlArray], ManifestError] = {
    try {
      val array = parser.getArray(propString)
      Ok(Option(array))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "Array", e.getMessage))
    }
  }

  /**
    * Parses a Table which should be at `propString`
    * and returns the Table or an error if the result
    * cannot be found.
    */
  private def getOptionalTableProperty(propString: String, parser: TomlParseResult, p: Path): Result[Option[TomlTable], ManifestError] = {
    try {
      val table = parser.getTable(propString)
      Ok(Option(table))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(ManifestError.RequiredPropertyHasWrongType(p, propString, "Table", e.getMessage))
    }
  }

  /**
    * Converts a String `s` to a semantic version and returns
    * an error if the String is not of the correct format.
    * The only allowed format is "x.x.x"
    */
  private def toFlixVer(s: String, p: Path): Result[SemVer, ManifestError] = {
    try {
      s.split('.') match {
        case Array(major, minor, patch) =>
          Ok(SemVer(major.toInt, minor.toInt, patch.toInt))
        case _ => Err(ManifestError.FlixVersionHasWrongLength(p, s))
      }
    } catch {
      case e: NumberFormatException => Err(ManifestError.VersionNumberWrong(p, s, e.getMessage))
    }
  }

  /**
    * Converts a String `s` to a reference to a GitHub project.
    * Returns an error if the string is not in the correct format.
    * The only allowed format is "github:<username>/<repository>".
    */
  private def toGithubProject(s: String, p: Path): Result[GitHub.Project, ManifestError] = {
    s.split(':') match {
      case Array("github", repo) =>
        GitHub.parseProject(repo)
          .mapErr(_ => ManifestError.RepositoryFormatError(p, s))
      case _ => Err(ManifestError.RepositoryFormatError(p, s))
    }
  }

  /**
    * Converts a TomlTable to a list of Dependencies. This requires
    * the value of each entry is a String which can be converted to a
    * semantic version. `flixDep` decides whether the Dependency is a Flix
    * or MavenDependency and `prodDep` decides whether it is for production
    * or development. `jarDep` decides whether it is an external jar. This
    * overrides `flixDep` and `prodDep`.
    * Returns an error if anything is not as expected.
    */
  private def collectDependencies(deps: Option[TomlTable], flixDep: Boolean, jarDep: Boolean, p: Path): Result[List[Dependency], ManifestError] = {
    deps match {
      case None => Ok(List.empty)
      case Some(deps) =>
        val depsEntries = deps.entrySet().asScala
        traverse(depsEntries)(entry => {
          val depKey = entry.getKey
          val depValue = entry.getValue
          if (jarDep) {
            createJarDep(depKey, depValue, p)
          } else if (flixDep) {
            // Key needs this format to do typed look-ups.
            val dottedDepKey = s"\"$depKey\""
            createFlixDep(deps, dottedDepKey, p)
          } else {
            createMavenDep(depKey, depValue, p)
          }
        })
    }
  }

  /**
    * Creates a MavenDependency.
    * Group id and artifact id are given by `depName`.
    * The version is given by `depVer`.
    * `p` is for reporting errors.
    */
  private def createMavenDep(depName: String, depVer: AnyRef, p: Path): Result[MavenDependency, ManifestError] = {
    for (
      groupId <- getGroupId(depName, p);
      artifactId <- getArtifactId(depName, p);
      version <- getMavenVersion(depVer, p)
    ) yield {
      Dependency.MavenDependency(groupId, artifactId, version)
    }
  }

  /**
    * Retrieves the group id for a Maven dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getGroupId(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split(':') match {
      case Array(groupId, _) => checkNameCharacters(groupId, p)
      case _ => Err(ManifestError.MavenDependencyFormatError(p, depName))
    }
  }

  /**
    * Retrieves the artifact id for a Maven dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getArtifactId(depName: String, p: Path): Result[String, ManifestError] = {
    depName.split(':') match {
      case Array(_, artifactId) => checkNameCharacters(artifactId, p)
      case _ => Err(ManifestError.MavenDependencyFormatError(p, depName))
    }
  }

  /**
    * A Maven version number is an uninterpreted tag. Maven (the repository) does not
    * enforce a format for version numbers so we must be liberal about what we accept.
    */
  private def getMavenVersion(depVer: AnyRef, p: Path): Result[String, ManifestError] = {
    try {
      val version = depVer.asInstanceOf[String]
      Ok(version)
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.DependencyFormatError(p, e.getMessage))
    }
  }

  /**
    * Create a [[FlixDependency]].
    *
    * @param deps   [[TomlTable]] of declared Flix dependencies.
    * @param depKey Repository address of the package.
    * @param p      [[Path]] of the project Toml file.
    * @return [[Result]] of the [[FlixDependency]] if succesful, otherwise a [[ManifestError]]
    */
  private def createFlixDep(deps: TomlTable, depKey: String, p: Path): Result[FlixDependency, ManifestError] = {
    // Regex for extracting repository, username, and project name.
    // (.+) is a capturing group, where . matches any character.
    val validPkg = s"^\"(.+):(.+)/(.+)\"$$".r
    depKey match {
      case validPkg(repoStr, username, projectName) =>
        val repo = Repository.mkRepository(repoStr) match {
          case Ok(r) => r
          case Err(_) => return Err(ManifestError.UnsupportedRepository(p, repoStr))
        }

        // Ensure the username is valid.
        if (!username.matches(s"^$ValidName$$"))
          return Err(ManifestError.IllegalName(p, depKey))

        // Ensure the project name is valid.
        if (!projectName.matches(s"^$ValidName$$"))
          return Err(ManifestError.IllegalName(p, depKey))

        // If the dependency maps to a string, parse the version.
        if (deps.isString(depKey)) {
          getFlixVersion(deps, depKey, p).map {
            Dependency.FlixDependency(repo, username, projectName, _, Permissions.FlixOnly)
          }


          // If the dependency maps to a table, get the version and permissions.
        } else if (deps.isTable(depKey)) {
          val depTbl = deps.getTable(depKey)
          val verKey = "version"
          val permKey = "permissions"

          for (
            ver <- getFlixVersion(depTbl, verKey, p);
            perm <- getPermissions(depTbl, permKey, p)
          ) yield FlixDependency(repo, username, projectName, ver, perm)
        } else {
          Err(ManifestError.VersionTypeError(Option.apply(p), depKey, deps.get(depKey)))
        }
      case _ => Err(ManifestError.FlixDependencyFormatError(p, depKey))
    }
  }

  /**
    * Attempt to retrieve a [[SemVer]] at `depKey` from the table `deps`.
    */
  private def getFlixVersion(deps: TomlTable, depKey: String, p: Path): Result[SemVer, ManifestError] = {
    // Ensure the version is a String.
    if (!deps.isString(depKey)) {
      Err(ManifestError.VersionTypeError(Option.apply(p), depKey, deps.get(depKey)))
    } else {
      val depVer = deps.getString(depKey)
      SemVer.ofString(depVer) match {
        case Some(v) => Ok(v)
        case None => Err(ManifestError.FlixVersionFormatError(Option.apply(p), depKey, depVer))
      }
    }
  }

  /**
    * Retrieve a permissions from a [[TomlTable]] `depTbl` at `key`.
    */
  private def getPermissions(depTbl: TomlTable, key: String, p: Path): Result[Permissions, ManifestError] = {
    // Ensure the permissions is a string.
    if (!depTbl.isString(key)) {
      val perms = depTbl.get(key)
      if (perms == null) {
        Ok(Permissions.FlixOnly)
      } else {
        Err(ManifestError.FlixDependencyPermissionTypeError(Some(p), key, perms))
      }
    } else {
      val permRaw = depTbl.getString(key)
      Permissions.fromString(permRaw) match {
        case Some(p) => Ok(p)
        case None => Err(ManifestError.FlixUnknownPermissionError(p, key, permRaw))
      }
    }
  }

  /**
    * Creates a JarDependency.
    * URL and website is given by `depUrl`.
    * The file name is given by `depName`.
    * `p` is for reporting errors.
    */
  private def createJarDep(depName: String, depUrl: AnyRef, p: Path): Result[JarDependency, ManifestError] = {
    for (
      url <- getUrl(depUrl, p);
      fileName <- getFileName(depName, p)
    ) yield {
      Dependency.JarDependency(url, fileName)
    }
  }

  /**
    * Converts `depUrl` to a String and retrieves the URL for a jar dependency.
    * Returns an error if it is not formatted correctly.
    */
  private def getUrl(depUrl: AnyRef, p: Path): Result[URL, ManifestError] = {
    try {
      val url = depUrl.asInstanceOf[String]
      try {
        if (url.startsWith("url:")) {
          val removeTag = url.substring(4)
          Ok(new URI(removeTag).toURL)
        } else {
          Err(ManifestError.JarUrlFormatError(p, url))
        }
      } catch {
        case e: IllegalArgumentException =>
          Err(ManifestError.WrongUrlFormat(p, url, e.getMessage))
      }
    } catch {
      case e: ClassCastException =>
        Err(ManifestError.JarUrlTypeError(p, e.getMessage))

    }
  }

  /**
    * Retrieves the file name for a jar dependency
    * and returns an error if it is not formatted correctly
    * or has characters that are not allowed.
    */
  private def getFileName(depName: String, p: Path): Result[String, ManifestError] = {
    val split = depName.split('.')
    if (split.length >= 2) {
      val extension = split.apply(split.length - 1)
      if (extension == "jar") {
        checkNameCharacters(depName, p)
      } else {
        Err(ManifestError.JarUrlExtensionError(p, depName, extension))
      }
    } else {
      Err(ManifestError.JarUrlFileNameError(p, depName))
    }
  }

  /**
    * Checks that a package name does not include any illegal characters.
    */
  private def checkNameCharacters(name: String, p: Path): Result[String, ManifestError] = {
    if (name.matches("^[a-zA-Z0-9.:/_-]+$"))
      Ok(name)
    else
      Err(ManifestError.IllegalName(p, name))
  }

  /**
    * Converts a TomlArray to a list of Strings. Returns
    * an error if anything in the array is not a String.
    */
  private def convertTomlArrayToStringList(array: TomlArray, p: Path): Result[List[String], ManifestError] = {
    val strings = array.toList.asScala.toList.map({
      case s: String => s
      case _ => return Err(ManifestError.AuthorNameError(p))
    })
    Ok(strings)
  }

  /**
    * Creates the `PackageModules` object from `optList`.
    */
  private def toPackageModules(optList: Option[List[String]], p: Path): Result[PackageModules, ManifestError] = {
    optList match {
      case None =>
        Ok(PackageModules.All)
      case Some(list) =>
        val moduleSet = list.map { string =>
          val namespace = string.split('.').toList
          Symbol.mkModuleSym(namespace)
        }.toSet
        Ok(PackageModules.Selected(moduleSet))
    }
  }

}
