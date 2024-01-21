package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{Dependency, DependencyKind, IncludedModules, ManifestError, ManifestParser, Repository, SemVer}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.net.URI
import java.nio.file.Paths

class TestManifestParser extends AnyFunSuite {

  val f: Formatter = Formatter.NoFormatter
  val s: String = File.separator
  val tomlCorrect: String = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |version = "0.1.0"
      |repository = "github:johnDoe/hello-world"
      |modules = ["FirstMod", "SecondMod.Foo"]
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |[dependencies]
      |"github:jls/tic-tac-toe" = "1.2.3"
      |"github:mlutze/flixball" = "3.2.1"
      |
      |[dev-dependencies]
      |"github:fuzzer/fuzzer" = "1.2.3"
      |
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3.4"
      |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
      |
      |[dev-mvn-dependencies]
      |"org.junit:junit" = "1.2"
      |
      |[jar-dependencies]
      |"myJar.jar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
      |
      |""".stripMargin
  }

  test("Ok.name") {
    assertResult(expected = "hello-world")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.name
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.description") {
    assertResult(expected = "A simple program")(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.description
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.version") {
    assertResult(expected = SemVer(0, 1, Some(0), None, None))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.version
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.Some") {
    assertResult(expected = Some(GitHub.Project("johnDoe", "hello-world")))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.repository
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.repository.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.repository
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.modules.Some") {
    assertResult(expected = IncludedModules.Selected(Set(Symbol.mkModuleSym(List("FirstMod")), Symbol.mkModuleSym(List("SecondMod", "Foo")))))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.modules
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.modules.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = IncludedModules.All)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.modules
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, Some(0), None, None))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.flix
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.license.Some") {
    assertResult(expected = Some("Apache-2.0"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.license
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.license.None") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(expected = None)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.license
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.authors") {
    assertResult(expected = List("John Doe <john@example.com>"))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.authors
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.dependencies") {
    assertResult(expected = List(Dependency.FlixDependency(Repository.GitHub, "jls", "tic-tac-toe", SemVer(1, 2, Some(3), None, None), DependencyKind.Production),
                                 Dependency.FlixDependency(Repository.GitHub, "mlutze", "flixball", SemVer(3, 2, Some(1), None, None), DependencyKind.Production),
                                 Dependency.FlixDependency(Repository.GitHub, "fuzzer", "fuzzer", SemVer(1, 2, Some(3), None, None), DependencyKind.Development),
                                 Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3.4", DependencyKind.Production),
                                 Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.0-M1", DependencyKind.Production),
                                 Dependency.MavenDependency("org.junit", "junit", "1.2", DependencyKind.Development),
                                 Dependency.JarDependency(new URI("https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar").toURL, "myJar.jar")))(actual = {
      ManifestParser.parse(tomlCorrect, null) match {
        case Ok(manifest) => manifest.dependencies
        case Err(e) => e.message(f)
      }
    })
  }

  test("Ok.mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "470"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3", DependencyKind.Production),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "470", DependencyKind.Production)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "47"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3", DependencyKind.Production),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "47", DependencyKind.Production)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "a.7.0"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3", DependencyKind.Production),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "a.7.0", DependencyKind.Production)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.b.0"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3", DependencyKind.Production),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.b.0", DependencyKind.Production)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.mvn-dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.c"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3", DependencyKind.Production),
                                  Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.c", DependencyKind.Production)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }
  test("Ok.dev-mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "12"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.junit", "junit", "12", DependencyKind.Development)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.dev-mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.3.4.5.6"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.junit", "junit", "1.2.3.4.5.6", DependencyKind.Development)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.dev-mvn-dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "a.2.3"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.junit", "junit", "a.2.3", DependencyKind.Development)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.dev-mvn-dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.b.3"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.junit", "junit", "1.b.3", DependencyKind.Development)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.dev-mvn-dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = "1.2.c"
        |
        |""".stripMargin
    }
    assertResult(expected = List(Dependency.MavenDependency("org.junit", "junit", "1.2.c", DependencyKind.Development)))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  /*
  * Errors
  * */
  //File does not exist
  test("Err.file.missing") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/missing.toml"
    val path = Paths.get(pathString)
    assertResult(ManifestError.IOError(path, s"main${s}test${s}ca${s}uwaterloo${s}flix${s}tools${s}missing.toml").message(f))(ManifestParser.parse(path) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Name
  test("Err.name.missing") {
    val toml = {
      """
        |[package]
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.name", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.name.misspelled") {
    val toml = {
      """
        |[package]
        |mane = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.mane").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.name.type") {
    val toml = {
      """
        |[package]
        |name = 1
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.name", "String", "Value of 'package.name' is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Description
  test("Err.description.missing") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |version = "0.1.0"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
  }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.description", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.description.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |desciption = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.desciption").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.description.type") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = 2
      |version = "0.1.0"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
  }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.description", "String", "Value of 'package.description' is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Version
  test("Err.version.missing") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
  }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.version", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |varsion = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.varsion").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.type") {
    val toml = {
    """
      |[package]
      |name = "hello-world"
      |description = "A simple program"
      |version = ["0.1.0"]
      |flix = "0.33.0"
      |license = "Apache-2.0"
      |authors = ["John Doe <john@example.com>"]
      |
      |""".stripMargin
  }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.version", "String", "Value of 'package.version' is a array").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "010"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "010").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0.1"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0.1.0.1").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "a.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "a.1.0", "For input string: \"a\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.b.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.b.0", "For input string: \"b\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.version.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.c"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.1.c", "For input string: \"c\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  // Repository
  test("Err.repository.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repsository = "github:johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.repsository").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "johnDoe/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:github/johnDoe/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:github/johnDoe/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:johnDoe/"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:johnDoe/").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.05") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:/hello-world"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:/hello-world").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.repository.wrongFormat.06") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |repository = "github:/"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RepositoryFormatError(null, "github:/").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  // Modules
  test("Err.modules.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |modjules = ["FirsMod", "SecondMod"]
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.modjules").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.modules.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |modules = 123
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.modules", "Array", "Value of 'package.modules' is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Flix
  test("Err.flix.missing") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.flix", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flux = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.flux").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = 330
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.flix", "String", "Value of 'package.flix' is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0330"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0330").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0,33,0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "0,33,0").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "?.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "?.33.0", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.?.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.?.0", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.flix.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.?"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "0.33.?", "For input string: \"?\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //License
  test("Err.license.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = 123
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.license", "String", "Value of 'package.license' is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.license.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |licence = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.licence").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Authors
  test("Err.authors.missing") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MissingRequiredProperty(null, "package.authors", None).message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.authors.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authars = ["John Doe <john@example.com>"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalPackageKeyFound(null, "package.authars").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.authors.type.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = "John Doe <john@example.com>"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.RequiredPropertyHasWrongType(null, "package.authors", "Array", "Value of 'package.authors' is a string").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.authors.type.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = [12345678]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.AuthorNameError(null, "key at index 0 is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.authors.type.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>", 159]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.AuthorNameError(null, "key at index 1 is a integer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Dependencies
  test("Err.dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = 123
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[depandencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "depandencies").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.name.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:ml&tze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "ml&tze").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.name.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic#tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "tic#tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "123"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "123").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.23"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "1.23").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls:tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github:jls:tic-tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.format.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github/jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github/jls/tic-tac-toe").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "a.2.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "a.2.1", "For input string: \"a\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.b.1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "3.b.1", "For input string: \"b\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = "1.2.3"
        |"github:mlutze/flixball" = "3.2.c"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "3.2.c", "For input string: \"c\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Dev-dependencies
  test("Err.dev-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = 789
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependences]
        |"github:fuzzer/fuzzer" = "7.8.9"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "dev-dependences").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "123"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "123").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.3.4.5"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixVersionHasWrongLength(null, "1.2.3.4.5").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.format.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github/fuzzer/fuzzer" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github/fuzzer/fuzzer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.format.04") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer-fuzzer" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.FlixDependencyFormatError(null, "github:fuzzer-fuzzer").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.numbers.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "a.2.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "a.2.3", "For input string: \"a\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.numbers.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.b.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "1.b.3", "For input string: \"b\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-dependencies.numbers.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-dependencies]
        |"github:fuzzer/fuzzer" = "1.2.c"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.VersionNumberWrong(null, "1.2.c", "For input string: \"c\"").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Mvn-dependencies
  test("Err.mvn-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = 470
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mwn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "mwn-dependencies").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.name.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.po)tgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "org.po)tgresql").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.name.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:post¤resql" = "1.2.3"
        |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalName(null, "post¤resql").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }


  test("Err.mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org.eclipse.jetty.jetty-server" = "4.7.0-M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org.eclipse.jetty.jetty-server").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dependencies]
        |"org.postgresql:postgresql" = "1.2.3"
        |"org:eclipse:jetty:jetty-server" = "4.7.0.M1"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org:eclipse:jetty:jetty-server").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }


  //Dev-mvn-dependencies
  test("Err.dev-mvn-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org.junit:junit" = 123456
        |
        |""".stripMargin
    }
    assertResult(ManifestError.DependencyFormatError(null, "class java.lang.Long cannot be cast to class java.lang.String (java.lang.Long and java.lang.String are in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-mvn-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[mvn-dev-dependencies]
        |"org.junit:junit" = "12.34.56"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "mvn-dev-dependencies").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-mvn-dependencies.format.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org/junit/junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org/junit/junit").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.dev-mvn-dependencies.format.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dev-mvn-dependencies]
        |"org:junit:junit" = "1.2.3"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.MavenDependencyFormatError(null, "org:junit:junit").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  //Jar-dependencies
  test("Err.jar-dependencies.type") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = ["url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"]
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlTypeError(null, "class org.tomlj.MutableTomlArray cannot be cast to class java.lang.String (org.tomlj.MutableTomlArray is in unnamed module of loader 'app'; java.lang.String is in module java.base of loader 'bootstrap')").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => println(e.message(f)); e.message(f)
    })
  }

  test("Err.jar-dependencies.misspelled") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependences]
        |"myJar.jar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.IllegalTableFound(null, "jar-dependences").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.filename.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlFileNameError(null, "myJar").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.filename.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jsr" = "url:https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlExtensionError(null, "myJar.jsr", "jsr").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.url.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.JarUrlFormatError(null, "https://repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

  test("Err.jar-dependencies.url.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[jar-dependencies]
        |"myJar.jar" = "url:repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar"
        |
        |""".stripMargin
    }
    assertResult(ManifestError.WrongUrlFormat(null, "url:repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.12.0/commons-lang3-3.12.0.jar", "URI is not absolute").message(f))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest
      case Err(e) => e.message(f)
    })
  }

}
