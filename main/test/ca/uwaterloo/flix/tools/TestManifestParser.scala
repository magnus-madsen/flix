package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.tools.pkg.{Dependency, ManifestError, ManifestParser, PackageModules, Repository, SemVer}
import ca.uwaterloo.flix.util.{Formatter, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite
import ca.uwaterloo.flix.tools.pkg.Manifest
import ca.uwaterloo.flix.tools.pkg.Permissions
import ca.uwaterloo.flix.tools.pkg.Permissions.{All, FlixOnly, Restricted}

import java.io.File
import java.net.URI
import java.nio.file.Paths
import scala.reflect.ClassTag

class TestManifestParser extends AnyFunSuite {

  def expectError[T](result: Result[Manifest, ManifestError])(implicit classTag: ClassTag[T]): Unit =
    result match {
      case Ok(_) => fail(s"Expected failure, but got success.")
      case Err(error) =>
        val expected = classTag.runtimeClass
        val actual = error.getClass
        if (!expected.isAssignableFrom(actual)) {
          fail(s"Expected an error of type ${expected.getSimpleName}, but found:\n\n${actual.getName}")
        }
    }

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
      |[mvn-dependencies]
      |"org.postgresql:postgresql" = "1.2.3.4"
      |"org.eclipse.jetty:jetty-server" = "4.7.0-M1"
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
    assertResult(expected = SemVer(0, 1, 0))(actual = {
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
    assertResult(expected = PackageModules.Selected(Set(Symbol.mkModuleSym(List("FirstMod")), Symbol.mkModuleSym(List("SecondMod", "Foo")))))(actual = {
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
    assertResult(expected = PackageModules.All)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) => m.modules
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix") {
    assertResult(expected = SemVer(0, 33, 0))(actual = {
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
    assertResult(expected = List(Dependency.FlixDependency(Repository.GitHub, "jls", "tic-tac-toe", SemVer(1, 2, 3), FlixOnly),
      Dependency.FlixDependency(Repository.GitHub, "mlutze", "flixball", SemVer(3, 2, 1), FlixOnly),
      Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3.4"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.0-M1"),
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
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "470")))(ManifestParser.parse(toml, null) match {
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
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "47")))(ManifestParser.parse(toml, null) match {
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
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "a.7.0")))(ManifestParser.parse(toml, null) match {
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
    assertResult(expected = Set(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.b.0")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies.toSet
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
    assertResult(expected = List(Dependency.MavenDependency("org.postgresql", "postgresql", "1.2.3"),
      Dependency.MavenDependency("org.eclipse.jetty", "jetty-server", "4.7.c")))(ManifestParser.parse(toml, null) match {
      case Ok(manifest) => manifest.dependencies
      case Err(e) => e.message(f)
    })
  }

  test("Ok.flix-dependency-permission.01") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = "none" }
        |""".stripMargin
    }
    assertResult(expected = Permissions.FlixOnly)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .permissions
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.02") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = "restricted" }
        |""".stripMargin
    }
    assertResult(expected = Permissions.Restricted)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .permissions
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.03") {
    val toml = {
      """
        |[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = "all" }
        |""".stripMargin
    }
    assertResult(expected = Permissions.All)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .permissions
        case Err(e) => e.message(f)
      }
    )
  }

  test("Ok.flix-dependency-permission.04") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3" }
        |""".stripMargin
    assertResult(expected = Permissions.FlixOnly)(actual =
      ManifestParser.parse(toml, null) match {
        case Ok(m) =>
          m.dependencies
            .head
            .asInstanceOf[Dependency.FlixDependency]
            .permissions
        case Err(e) => e.message(f)
      }
    )
  }

  /*
  * Errors
  * */
  //File does not exist
  test("ManifestError.IOError.01") {
    val pathString = "main/test/ca/uwaterloo/flix/tools/missing.toml"
    val path = Paths.get(pathString)
    val result = ManifestParser.parse(path)
    expectError[ManifestError.IOError](result)
  }

  //Name
  test("ManifestError.MissingRequiredProperty.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Description
  test("ManifestError.MissingRequiredProperty.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Version
  test("ManifestError.MissingRequiredProperty.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.VersionNumberWrong.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  // Repository
  test("ManifestError.IllegalPackageKeyFound.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RepositoryFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  test("ManifestError.RepositoryFormatError.06") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RepositoryFormatError](result)
  }

  // Modules
  test("ManifestError.IllegalPackageKeyFound.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  //Flix
  test("ManifestError.MissingRequiredProperty.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.06") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.FlixVersionHasWrongLength.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionHasWrongLength](result)
  }

  test("ManifestError.VersionNumberWrong.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  test("ManifestError.VersionNumberWrong.06") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionNumberWrong](result)
  }

  //License
  test("ManifestError.RequiredPropertyHasWrongType.06") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.IllegalPackageKeyFound.07") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  //Authors
  test("ManifestError.MissingRequiredProperty.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MissingRequiredProperty](result)
  }

  test("ManifestError.IllegalPackageKeyFound.08") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalPackageKeyFound](result)
  }

  test("ManifestError.RequiredPropertyHasWrongType.07") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.RequiredPropertyHasWrongType](result)
  }

  test("ManifestError.AuthorNameError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.AuthorNameError](result)
  }

  test("ManifestError.AuthorNameError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.AuthorNameError](result)
  }

  //Dependencies
  test("ManifestError.DependencyFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.VersionTypeError](result)
  }

  test("ManifestError.IllegalTableFound.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalTableFound](result)
  }
  test("ManifestError.IllegalName.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.IllegalName.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.FlixVersionFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixDependencyFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixDependencyFormatError](result)
  }

  test("ManifestError.FlixDependencyFormatError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixDependencyFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  test("ManifestError.FlixVersionFormatError.05") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixVersionFormatError](result)
  }

  //Mvn-dependencies
  test("ManifestError.DependencyFormatError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.DependencyFormatError](result)
  }

  test("ManifestError.IllegalTableFound.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalTableFound](result)
  }

  test("ManifestError.IllegalName.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalName](result)
  }

  test("ManifestError.IllegalName.04") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalName](result)
  }


  test("ManifestError.MavenDependencyFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MavenDependencyFormatError](result)
  }

  test("ManifestError.MavenDependencyFormatError.02") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.MavenDependencyFormatError](result)
  }

  //Jar-dependencies
  test("ManifestError.JarUrlTypeError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.JarUrlTypeError](result)
  }

  test("ManifestError.IllegalTableFound.03") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.IllegalTableFound](result)
  }

  test("ManifestError.JarUrlFileNameError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.JarUrlFileNameError](result)
  }

  test("ManifestError.JarUrlExtensionError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.JarUrlExtensionError](result)
  }

  test("ManifestError.JarUrlFormatError.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.JarUrlFormatError](result)
  }

  test("ManifestError.WrongUrlFormat.01") {
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
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.WrongUrlFormat](result)
  }

  test("ManifestError.FlixUnknownPermissionError.01") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = "netflix" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixUnknownPermissionError](result)
  }

  test("ManifestError.FlixUnknownPermissionError.02") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = "effect" }
        |""".stripMargin
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixUnknownPermissionError](result)
  }

  test("ManifestError.FlixDependencyPermissionTypeError.01") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = true }
        |""".stripMargin
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixDependencyPermissionTypeError](result)
  }

  test("ManifestError.FlixDependencyPermissionTypeError.02") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"github:jls/tic-tac-toe" = { version = "1.2.3", permissions = [] }
        |""".stripMargin
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.FlixDependencyPermissionTypeError](result)
  }

  test("ManifestError.UnsupportedRepository.01") {
    val toml =
      """[package]
        |name = "hello-world"
        |description = "A simple program"
        |version = "0.1.0"
        |flix = "0.33.0"
        |license = "Apache-2.0"
        |authors = ["John Doe <john@example.com>"]
        |
        |[dependencies]
        |"hubgit:jls/tic-tac-toe" = "1.2.3"
        |""".stripMargin
    val result = ManifestParser.parse(toml, null)
    expectError[ManifestError.UnsupportedRepository](result)
  }
}
