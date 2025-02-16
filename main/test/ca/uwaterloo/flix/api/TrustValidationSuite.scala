package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, TypedAst}
import ca.uwaterloo.flix.tools.pkg.Permissions
import ca.uwaterloo.flix.tools.pkg.{Dependency, Repository, SemVer}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class TrustValidationSuite extends AnyFunSuite with TestUtils {

  test("TrustValidation.01") {
    val input =
      """
        |pub def noEff(): Int32 = 2
        |""".stripMargin
    val (root, flix) = checkLib(input, "noEFf", Options.TestWithLibNix)
    val dep = mkDependency("testlib", Permissions.FlixOnly)
    val result = flix.validateTrust(root, Set(dep))
    assert(result.isEmpty)
  }

  test("TrustValidation.02") {
    val input =
      """
        |pub def f(): Int32 = unchecked_cast(2 as Int32 \ {})
        |""".stripMargin
    val (root, flix) = checkLib(input, "f", Options.TestWithLibNix)
    val dep = mkDependency("testlib", Permissions.FlixOnly)
    val result = flix.validateTrust(root, Set(dep))
    assert(result.nonEmpty)
  }

  private def mkDependency(name: String, perm: Permissions): Dependency.FlixDependency = {
    Dependency.FlixDependency(Repository.GitHub, "", name, SemVer(0, 1, 0), perm)
  }

  private def checkLib(input: String, lib: String, o: Options): (TypedAst.Root, Flix) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    val flix = new Flix().setOptions(o).addSourceCode("<test>", input)
    val (Some(root), _) = flix.check()
    val withLibs = root.defs.map {
      case (_, TypedAst.Def(sym, spec, exp, loc)) if sym.text == lib =>
        val loc1 = toLibLoc(loc)
        sym -> TypedAst.Def(sym, spec, exp, loc1)
      case sd => sd
    }
    (root.copy(defs = withLibs), flix)
  }

  private def toLibLoc(loc: SourceLocation): SourceLocation = {
    val (sp1, sp2) = toLibSp(loc.sp1, loc.sp2)
    loc.copy(sp1 = sp1, sp2 = sp2)
  }

  private def toLibSp(sp1: SourcePosition, sp2: SourcePosition): (SourcePosition, SourcePosition) = {
    val input = toLibInput(sp1.source.input)
    val source = sp1.source.copy(input = input)
    (sp1.copy(source = source), sp2.copy(source = source))
  }

  private def toLibInput(input: Input): Input = input match {
    case Input.Text(_, text, sctx) => Input.FileInPackage(Path.of("testlib"), "", text, sctx)
    case Input.TxtFile(_, _) => ???
    case Input.PkgFile(_, _) => ???
    case Input.FileInPackage(_, _, _, _) => input
    case Input.Unknown => ???
  }
}
