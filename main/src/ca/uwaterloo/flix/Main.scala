/*
 * Copyright 2019 Magnus Madsen
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

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.lsp.LanguageServer
import ca.uwaterloo.flix.api.{Bootstrap, Flix, Version}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools._
import ca.uwaterloo.flix.util.Validation.flatMapN
import ca.uwaterloo.flix.util._

import java.io.{File, PrintStream}
import java.net.BindException
import java.nio.file.Paths

/**
  * The main entry point for the Flix compiler and runtime.
  */
object Main {

  /**
    * The main method.
    */
  def main(argv: Array[String]): Unit = {

    val cwd = Paths.get(".").toAbsolutePath.normalize()

    // parse command line options.
    val cmdOpts: CmdOpts = parseCmdOpts(argv).getOrElse {
      Console.err.println("Unable to parse command line arguments. Will now exit.")
      System.exit(1)
      null
    }

    // check if the --listen flag was passed.
    if (cmdOpts.listen.nonEmpty) {
      var successfulRun: Boolean = false
      while (!successfulRun) {
        try {
          val socketServer = new SocketServer(cmdOpts.listen.get)
          socketServer.run()
          successfulRun = true
        } catch {
          case ex: BindException =>
            Console.println(ex.getMessage)
            Console.println("Retrying in 10 seconds.")
            Thread.sleep(10_000)
        }
      }
      System.exit(0)
    }

    // compute the main entry point
    val entryPoint = cmdOpts.entryPoint match {
      case None => Options.Default.entryPoint
      case Some(s) => Some(Symbol.mkDefnSym(s))
    }

    // get GitHub token
    val githubToken =
      cmdOpts.githubToken
        .orElse(FileOps.readLine(cwd.resolve("./.GITHUB_TOKEN")))
        .orElse(sys.env.get("GITHUB_TOKEN"))

    // construct flix options.
    var options = Options(
      lib = cmdOpts.xlib,
      entryPoint = entryPoint,
      explain = cmdOpts.explain,
      githubToken = githubToken,
      incremental = Options.Default.incremental,
      json = cmdOpts.json,
      progress = true,
      installDeps = cmdOpts.installDeps,
      output = None,
      target = Options.Default.target,
      test = Options.Default.test,
      threads = cmdOpts.threads.getOrElse(Options.Default.threads),
      loadClassFiles = Options.Default.loadClassFiles,
      assumeYes = cmdOpts.assumeYes,
      xnoverify = cmdOpts.xnoverify,
      xbddthreshold = cmdOpts.xbddthreshold,
      xnoboolcache = cmdOpts.xnoboolcache,
      xnoboolspecialcases = cmdOpts.xnoboolspecialcases,
      xnoboolunif = cmdOpts.xnoboolunif,
      xnoqmc = cmdOpts.xnoqmc,
      xnooptimizer = cmdOpts.xnooptimizer,
      xprintphases = cmdOpts.xprintphases,
      xsummary = cmdOpts.xsummary,
      xfuzzer = cmdOpts.xfuzzer,
      xparser = cmdOpts.xparser,
      xprinttyper = cmdOpts.xprinttyper,
      xverifyeffects = cmdOpts.xverifyeffects,
      XPerfFrontend = cmdOpts.XPerfFrontend,
      XPerfN = cmdOpts.XPerfN
    )

    // Don't use progress bar if benchmarking.
    if (cmdOpts.xbenchmarkCodeSize || cmdOpts.xbenchmarkIncremental || cmdOpts.xbenchmarkPhases || cmdOpts.xbenchmarkFrontend || cmdOpts.xbenchmarkThroughput) {
      options = options.copy(progress = false)
    }

    // Don't use progress bar if not attached to a console.
    if (System.console() == null) {
      options = options.copy(progress = false)
    }

    // check if command was passed.
    try {
      implicit val formatter: Formatter = Formatter.getDefault
      implicit val out: PrintStream = System.err

      cmdOpts.command match {
        case Command.None =>
          SimpleRunner.run(cwd, cmdOpts, options) match {
            case Result.Ok(_) =>
              System.exit(0)
            case _ =>
              System.exit(1)
          }

        case Command.Init =>
          Bootstrap.init(cwd).toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Check =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options)
              bootstrap.check(flix)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Build =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(loadClassFiles = false))
              bootstrap.build(flix)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.BuildJar =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap => bootstrap.buildJar()
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.BuildFatJar =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap => bootstrap.buildFatJar()
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.BuildPkg =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap => bootstrap.buildPkg()
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Doc =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options)
              bootstrap.doc(flix)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Run =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options)
              val args: Array[String] = cmdOpts.args match {
                case None => Array.empty
                case Some(a) => a.split(" ")
              }
              bootstrap.run(flix, args)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Benchmark =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.benchmark(flix)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Test =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.test(flix)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Repl =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'repl' command cannot be used with a list of files.")
            System.exit(1)
          }
          Bootstrap.bootstrap(cwd, options.githubToken).toHardResult match {
            case Result.Ok(bootstrap) =>
              val shell = new Shell(bootstrap, options)
              shell.loop()
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Lsp(port) =>
          val o = options.copy(progress = false)
          try {
            val languageServer = new LanguageServer(port, o)
            languageServer.run()
          } catch {
            case ex: BindException =>
              throw new RuntimeException(ex)
          }
          System.exit(0)

        case Command.Release =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.release(flix)(System.err)
          }.toHardResult match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.Mtest(testModule, productionModule, percentage) =>
            flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
                bootstrap =>
                    val flix = new Flix().setFormatter(formatter)
                    flix.setOptions(options)
                    bootstrap.mtest(flix, testModule, productionModule, percentage)
            }.toHardResult match {
                case Result.Ok(_) =>
                    System.exit(0)
                case Result.Err(errors) =>
                    errors.map(_.message(formatter)).foreach(println)
                    System.exit(1)
            }
            //MutationTester.run(cmdOpts.files, options, testModule, productionModule)

        case Command.Outdated =>
          flatMapN(Bootstrap.bootstrap(cwd, options.githubToken)) {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.outdated(flix)(System.err)
          }.toHardResult match {
            case Result.Ok(false) =>
              // Up to date
              System.exit(0)
            case Result.Ok(true) =>
              // Contains outdated dependencies
              System.exit(1)
            case Result.Err(errors) =>
              errors.map(_.message(formatter)).foreach(println)
              System.exit(1)
          }

        case Command.CompilerPerf =>
          CompilerPerf.run(options)

        case Command.CompilerMemory =>
          CompilerMemory.run(options)

      }
    }

    catch {
      case ex: RuntimeException =>
        ex.printStackTrace()
        System.exit(1)
    }
  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(command: Command = Command.None,
                     args: Option[String] = None,
                     entryPoint: Option[String] = None,
                     explain: Boolean = false,
                     installDeps: Boolean = true,
                     githubToken: Option[String] = None,
                     json: Boolean = false,
                     listen: Option[Int] = None,
                     threads: Option[Int] = None,
                     assumeYes: Boolean = false,
                     xnoverify: Boolean = false,
                     xbenchmarkCodeSize: Boolean = false,
                     xbenchmarkIncremental: Boolean = false,
                     xbenchmarkPhases: Boolean = false,
                     xbenchmarkFrontend: Boolean = false,
                     xbenchmarkThroughput: Boolean = false,
                     xbddthreshold: Option[Int] = None,
                     xlib: LibLevel = LibLevel.All,
                     xnoboolcache: Boolean = false,
                     xnoboolspecialcases: Boolean = false,
                     xnoboolunif: Boolean = false,
                     xnoqmc: Boolean = false,
                     xnooptimizer: Boolean = false,
                     xprintphases: Boolean = false,
                     xsummary: Boolean = false,
                     xfuzzer: Boolean = false,
                     xparser: Boolean = false,
                     xprinttyper: Option[String] = None,
                     xverifyeffects: Boolean = false,
                     XPerfN: Option[Int] = None,
                     XPerfFrontend: Boolean = false,
                     mtests_temp: String =  "",
                     mtests_temp2: String =  "",
                     mtests_temp3: Int = 100,
                     files: Seq[File] = Seq())

  /**
    * A case class representing possible commands.
    */
  sealed trait Command

  object Command {

    case object None extends Command

    case object Init extends Command

    case object Check extends Command

    case object Build extends Command

    case object BuildJar extends Command

    case object BuildFatJar extends Command

    case object BuildPkg extends Command

    case object Doc extends Command

    case object Run extends Command

    case object Benchmark extends Command

    case object Test extends Command

    case class Mtest(testModule: String, productionModule: String, percentage: Int) extends Command

    case object Repl extends Command

    case class Lsp(port: Int) extends Command

    case object Release extends Command

    case object Outdated extends Command

    case object CompilerPerf extends Command

    case object CompilerMemory extends Command

  }

  /**
    * Parse command line options.
    *
    * @param args the arguments array.
    */
  def parseCmdOpts(args: Array[String]): Option[CmdOpts] = {
    implicit val readInclusion: scopt.Read[LibLevel] = scopt.Read.reads {
      case "nix" => LibLevel.Nix
      case "min" => LibLevel.Min
      case "all" => LibLevel.All
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid library level. Valid options are 'all', 'min', and 'nix'.")
    }

    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Command
      cmd("init").action((_, c) => c.copy(command = Command.Init)).text("  creates a new project in the current directory.")

      cmd("check").action((_, c) => c.copy(command = Command.Check)).text("  checks the current project for errors.")

      cmd("build").action((_, c) => c.copy(command = Command.Build)).text("  builds (i.e. compiles) the current project.")

      cmd("build-jar").action((_, c) => c.copy(command = Command.BuildJar)).text("  builds a jar-file from the current project.")

      cmd("build-fatjar").action((_, c) => c.copy(command = Command.BuildFatJar)).text("  builds a fatjar-file from the current project.")

      cmd("build-pkg").action((_, c) => c.copy(command = Command.BuildPkg)).text("  builds a fpkg-file from the current project.")

      cmd("doc").action((_, c) => c.copy(command = Command.Doc)).text("  generates API documentation.")

      cmd("run").action((_, c) => c.copy(command = Command.Run)).text("  runs main for the current project.")

      cmd("benchmark").action((_, c) => c.copy(command = Command.Benchmark)).text("  runs the benchmarks for the current project.")

      cmd("test").action((_, c) => c.copy(command = Command.Test)).text("  runs the tests for the current project.")

      cmd("mtest").text("  runs mutation tests given testModule and productionModule modules. Can also take an optional percentage")
        .children(
          arg[String]("testModule").action((tes, c) => c.copy(mtests_temp = tes))
            .required(),
          arg[String]("productionModule").action((tes, c) => c.copy(mtests_temp2 = tes))
            .required()
            .action((t, c) => c.copy(command = Command.Mtest(c.mtests_temp, c.mtests_temp2, c.mtests_temp3))),
          arg[Int]("percentage").action((tes, c) => c.copy(mtests_temp3 = tes))
            .optional()
            .action((t, c) => c.copy(command = Command.Mtest(c.mtests_temp, c.mtests_temp2, c.mtests_temp3)))
        )

      cmd("repl").action((_, c) => c.copy(command = Command.Repl)).text("  starts a repl for the current project, or provided Flix source files.")

      cmd("lsp").text("  starts the LSP server and listens on the given port.")
        .children(
          arg[Int]("port").action((port, c) => c.copy(command = Command.Lsp(port)))
            .required(),
        )

      cmd("release").text("  releases a new version to GitHub.")
        .action((_, c) => c.copy(command = Command.Release))

      cmd("outdated").text("  shows dependencies which have newer versions available.")
        .action((_, c) => c.copy(command = Command.Outdated))

      cmd("Xperf").action((_, c) => c.copy(command = Command.CompilerPerf)).children(
        opt[Unit]("frontend")
          .action((_, c) => c.copy(XPerfFrontend = true))
          .text("benchmark only frontend"),
        opt[Int]("n")
          .action((v, c) => c.copy(XPerfN = Some(v)))
          .text("number of compilations")
      ).hidden()

      cmd("Xmemory").action((_, c) => c.copy(command = Command.CompilerMemory)).hidden()

      note("")

      opt[String]("args").action((s, c) => c.copy(args = Some(s))).
        valueName("<a1, a2, ...>").
        text("arguments passed to main. Must be a single quoted string.")

      opt[String]("entrypoint").action((s, c) => c.copy(entryPoint = Some(s))).
        text("specifies the main entry point.")

      opt[Unit]("explain").action((_, c) => c.copy(explain = true)).
        text("provides suggestions on how to solve a problem.")

      opt[String]("github-token").action((s, c) => c.copy(githubToken = Some(s))).
        text("API key to use for GitHub dependency resolution.")

      help("help").text("prints this usage information.")

      opt[Unit]("json").action((_, c) => c.copy(json = true)).
        text("enables json output.")

      opt[Int]("listen").action((s, c) => c.copy(listen = Some(s))).
        valueName("<port>").
        text("starts the socket server and listens on the given port.")

      opt[Unit]("no-install").action((_, c) => c.copy(installDeps = false)).
        text("disables automatic installation of dependencies.")

      opt[Int]("threads").action((n, c) => c.copy(threads = Some(n))).
        text("number of threads to use for compilation.")

      opt[Unit]("yes").action((_, c) => c.copy(assumeYes = true)).
        text("automatically answer yes to all prompts.")

      version("version").text("prints the version number.")

      // Experimental options:
      note("")
      note("The following options are experimental:")

      // Xnoverify
      opt[Unit]("Xnoverify").action((_, c) => c.copy(xnoverify = true)).
        text("disables verification of the last AST.")

      // Xbenchmark-code-size
      opt[Unit]("Xbenchmark-code-size").action((_, c) => c.copy(xbenchmarkCodeSize = true)).
        text("[experimental] benchmarks the size of the generated JVM files.")

      // Xbenchmark-incremental
      opt[Unit]("Xbenchmark-incremental").action((_, c) => c.copy(xbenchmarkIncremental = true)).
        text("[experimental] benchmarks the performance of each compiler phase in incremental mode.")

      // Xbenchmark-phases
      opt[Unit]("Xbenchmark-phases").action((_, c) => c.copy(xbenchmarkPhases = true)).
        text("[experimental] benchmarks the performance of each compiler phase.")

      // Xbenchmark-frontend
      opt[Unit]("Xbenchmark-frontend").action((_, c) => c.copy(xbenchmarkFrontend = true)).
        text("[experimental] benchmarks the performance of the frontend.")

      // Xbenchmark-throughput
      opt[Unit]("Xbenchmark-throughput").action((_, c) => c.copy(xbenchmarkThroughput = true)).
        text("[experimental] benchmarks the performance of the entire compiler.")

      // Xlib
      opt[LibLevel]("Xlib").action((arg, c) => c.copy(xlib = arg)).
        text("[experimental] controls the amount of std. lib. to include (nix, min, all).")

      // Xno-optimizer
      opt[Unit]("Xno-optimizer").action((_, c) => c.copy(xnooptimizer = true)).
        text("[experimental] disables compiler optimizations.")

      // Xprint-phase
      opt[Unit]("Xprint-phases").action((_, c) => c.copy(xprintphases = true)).
        text("[experimental] prints the ASTs after the each phase.")

      // Xbdd-threshold
      opt[Int]("Xbdd-threshold").action((n, c) => c.copy(xbddthreshold = Some(n))).
        text("[experimental] sets the threshold for when to use BDDs.")

      // Xno-bool-cache
      opt[Unit]("Xno-bool-cache").action((_, c) => c.copy(xnoboolcache = true)).
        text("[experimental] disables Boolean caches.")

      // Xno-bool-specialcases
      opt[Unit]("Xno-bool-specialcases").action((_, c) => c.copy(xnoboolspecialcases = true)).
        text("[experimental] disables hardcoded Boolean unification special cases.")

      // Xno-bool-unif
      opt[Unit]("Xno-bool-unif").action((_, c) => c.copy(xnoboolunif = true)).
        text("[experimental] disables Boolean unification. (DO NOT USE).")

      // Xno-qmc
      opt[Unit]("Xno-qmc").action((_, c) => c.copy(xnoqmc = true)).
        text("[experimental] disables Quine McCluskey when using BDDs.")

      // Xsummary
      opt[Unit]("Xsummary").action((_, c) => c.copy(xsummary = true)).
        text("[experimental] prints a summary of the compiled modules.")

      // Xfuzzer
      opt[Unit]("Xfuzzer").action((_, c) => c.copy(xfuzzer = true)).
        text("[experimental] enables compiler fuzzing.")

      // Xparser
      opt[Unit]("Xparser").action((_, c) => c.copy(xparser = true)).
        text("[experimental] disables new experimental lexer and parser.")

      // Xprint-typer
      opt[String]("Xprint-typer").action((sym, c) => c.copy(xprinttyper = Some(sym))).
        text("[experimental] writes constraints to dot files.")

      // Xverify-effects
      opt[String]("Xverify-effects").action((_, c) => c.copy(xverifyeffects = true)).
        text("[experimental] verifies consistency of effects after typechecking")

      note("")

      // Input files.
      arg[File]("<file>...").action((x, c) => c.copy(files = c.files :+ x))
        .optional()
        .unbounded()
        .text("input Flix source code files, Flix packages, and Java archives.")

    }

    parser.parse(args, CmdOpts())
  }

}
