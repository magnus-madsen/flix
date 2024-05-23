/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.shell

import ca.uwaterloo.flix.MutationTester
import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Flix, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.fmt._
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util._
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.io.PrintStream
import java.util.logging.{Level, Logger}
import scala.collection.mutable

class Shell(bootstrap: Bootstrap, options: Options) {

  /**
    * The mutable list of source code fragments.
    */
  private val fragments = mutable.Stack.empty[String]

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private implicit val flix: Flix = new Flix().setFormatter(AnsiTerminalFormatter)

  /**
    * The result of the most recent compilation
    */
  private var root: Option[Root] = None

  /**
    * Is this the first compile
    */
  private var isFirstCompile = true

  /**
    * Remove any line continuation backslashes from the given string
    */
  private def unescapeLine(s: String): String = {

    // (?s) enables dotall mode (so . matches newlines)
    val twoBackslashes = raw"(?s)\\\\\n(.*)\n\\\\".r

    s match {
      // First, check for a string with two backslashes at start and end
      case twoBackslashes(s) => s

      // If not, then replace all escaped line endings with \n
      case _ =>
        val escapedLineEnd = raw"\\\n".r
        escapedLineEnd.replaceAllIn(s, "\n")
    }
  }

  /**
    * Continuously reads a line of input from the terminal, parses and executes it.
    */
  def loop(): Unit = {
    // Silence JLine warnings about terminal type.
    Logger.getLogger("org.jline").setLevel(Level.OFF)

    // Initialize the terminal.
    implicit val terminal: Terminal = TerminalBuilder
      .builder()
      .system(true)
      .build()

    // Initialize the terminal line reader.
    val reader = LineReaderBuilder
      .builder()
      .appName("flix")
      .parser(new ShellParser)
      .terminal(terminal)
      .build()
    reader.setOpt(LineReader.Option.DISABLE_EVENT_EXPANSION)

    // Print the welcome banner.
    printWelcomeBanner()

    // Trigger a compilation of the source input files.
    execReload()

    try {
      // Repeatedly try to read an input from the line reader.
      while (!Thread.currentThread().isInterrupted) {
        // Try to read a command.
        val line = unescapeLine(reader.readLine(prompt))

        // Parse the command.
        val cmd = Command.parse(line)
        try {
          // Try to execute the command. Catch any exception.
          execute(cmd)
        } catch {
          case e: Exception =>
            terminal.writer().print(e.getMessage)
            e.printStackTrace(terminal.writer())
        }
      }
    } catch {
      case _: UserInterruptException => // nop, exit gracefully.
      case _: EndOfFileException => // nop, exit gracefully.
    }

    // Print goodbye message.
    terminal.writer().println("Thanks, and goodbye.")
  }

  /**
    * Prints the welcome banner to the terminal.
    */
  private def printWelcomeBanner()(implicit terminal: Terminal): Unit = {
    val banner =
      """     __   _   _
        |    / _| | | (_)            Welcome to Flix __VERSION__
        |   | |_  | |  _  __  __
        |   |  _| | | | | \ \/ /     Enter an expression to have it evaluated.
        |   | |   | | | |  >  <      Type ':help' for more information.
        |   |_|   |_| |_| /_/\_\     Type ':quit' or press 'ctrl + d' to exit.
      """.stripMargin

    terminal.writer().println(banner.replaceAll("__VERSION__", Version.CurrentVersion.toString))
    terminal.flush()
  }

  /**
    * Returns the Flix prompt.
    */
  private def prompt: String = "flix> "

  /**
    * Executes the given command `cmd`.
    */
  private def execute(cmd: Command)(implicit terminal: Terminal): Unit = {
    implicit val formatter: Formatter = flix.getFormatter
    implicit val out: PrintStream = new PrintStream(terminal.output())
    cmd match {
      case Command.Nop => // nop
      case Command.Reload => execReload()
      case Command.Info(s) => execInfo(s)
      case Command.Quit => execQuit()
      case Command.Help => execHelp()
      case Command.Praise => execPraise()
      case Command.Eval(s) => execEval(s)
      case Command.ReloadAndEval(s) => execReloadAndEval(s)
      case Command.Init => execBootstrap(Bootstrap.init(bootstrap.projectPath))
      case Command.Build => execBootstrap(bootstrap.build(flix))
      case Command.BuildJar => execBootstrap(bootstrap.buildJar())
      case Command.BuildFatJar => execBootstrap(bootstrap.buildFatJar())
      case Command.BuildPkg => execBootstrap(bootstrap.buildPkg())
      case Command.Release => execBootstrap(bootstrap.release(flix))
      case Command.Check => execBootstrap(bootstrap.check(flix))
      case Command.Doc => execBootstrap(bootstrap.doc(flix))
      case Command.Test => execBootstrap(bootstrap.test(flix))
      case Command.Mtest(a, b, c) => MutationTester.run(flix, a, b, c)
      case Command.Outdated => execBootstrap(bootstrap.outdated(flix))
      case Command.Unknown(s) => execUnknown(s)
    }
  }

  /**
    * Reloads every source path.
    */
  private def execReload()(implicit terminal: Terminal): Unit = {

    // Scan the disk to find changes, and add source to the flix object
    bootstrap.reconfigureFlix(flix)

    // Remove any previous definitions, as they may no longer be valid against the new source
    clearFragments()

    compile(progress = isFirstCompile)
    isFirstCompile = false
  }

  /**
    * Displays documentation for the given identifier
    */
  private def execInfo(s: String)(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()
    val traitSym = Symbol.mkTraitSym(s)
    val defnSym = Symbol.mkDefnSym(s)
    val enumSym = Symbol.mkEnumSym(s)
    val aliasSym = Symbol.mkTypeAliasSym(s)

    root match {
      case Some(r) =>
        if (r.traits.contains(traitSym)) {
          val traitDecl = r.traits(traitSym)
          w.println(FormatDoc.asMarkDown(traitDecl.doc))
        } else if (r.defs.contains(defnSym)) {
          val defDecl = r.defs(defnSym)
          w.println(FormatSignature.asMarkDown(defDecl))
          w.println(FormatDoc.asMarkDown(defDecl.spec.doc))
        } else if (r.enums.contains(enumSym)) {
          val enumDecl = r.enums(enumSym)
          w.println(FormatDoc.asMarkDown(enumDecl.doc))
        } else if (r.typeAliases.contains(aliasSym)) {
          val aliasDecl = r.typeAliases(aliasSym)
          w.println(FormatType.formatType(aliasDecl.tpe))
          w.println()
          w.println(FormatDoc.asMarkDown(aliasDecl.doc))
        } else {
          w.println(s"$s not found")
        }

      case None =>
        w.println("Error: No compilation results available")
    }
  }

  /**
    * Exits the shell.
    */
  private def execQuit(): Unit = {
    Thread.currentThread().interrupt()
  }

  /**
    * Executes the help command.
    */
  private def execHelp()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    w.println("  Command       Arguments                                  Purpose")
    w.println()
    w.println("  :reload :r                                               Recompiles every source file.")
    w.println("  :info :i      <fqn>                                      Displays documentation for <fqn>.")
    w.println("  :init                                                    Creates a new project in the current directory.")
    w.println("  :build :b                                                Builds (i.e. compiles) the current project.")
    w.println("  :build-jar :jar                                          Builds a jar-file from the current project.")
    w.println("  :build-fatjar :fatjar                                    Builds a fatjar-file from the current project.")
    w.println("  :build-pkg :pkg                                          Builds a fpkg-file from the current project.")
    w.println("  :release                                                 Publishes a release of the current project to GitHub.")
    w.println("  :check :c                                                Checks the current project for errors.")
    w.println("  :doc :d                                                  Generates API documentation for the current project.")
    w.println("  :test :t                                                 Runs the tests for the current project.")
    w.println("  :mtest :mt    <testMod> <prodMod> Opt<percentage>        Runs the mutation tests given a tester and testee.")
    w.println("  :outdated                                                Shows dependencies which have newer versions available.")
    w.println("  :quit :q                                                 Terminates the Flix shell.")
    w.println("  :help :h :?                                              Shows this helpful information.")
    w.println()
  }

  /**
    * Executes the praise command.
    */
  private def execPraise()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()
    w.print(Toucan.leToucan())
  }

  /**
    * Evaluates the given source code.
    */
  private def execEval(s: String)(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    //
    // Try to determine the category of the source line.
    //
    Category.categoryOf(s) match {
      case Category.Decl =>
        // The input is a declaration. Push it on the stack of fragments.
        fragments.push(s)

        // The name of the fragment is $n where n is the stack offset.
        val name = "$" + fragments.length

        // Add the source code fragment to Flix.
        flix.addSourceCode(name, s)

        // And try to compile!
        compile(progress = false).toHardResult match {
          case Result.Ok(_) =>
            // Compilation succeeded.
            w.println("Ok.")
          case Result.Err(_) =>
            // Compilation failed. Ignore the last fragment.
            fragments.pop()
            flix.remSourceCode(name)
            w.println("Error: Declaration ignored due to previous error(s).")
        }

      case Category.Expr =>
        // The input is an expression. Wrap it in main and run it.

        // The name of the generated main function.
        val main = Symbol.mkDefnSym("shell1")

        // Cast the println to allow escaping effects
        val src =
          s"""def ${main.name}(): Unit \\ IO =
             |unchecked_cast(println($s) as _ \\ IO)
             |""".stripMargin
        flix.addSourceCode("<shell>", src)
        run(main)
        // Remove immediately so it doesn't confuse subsequent compilations (e.g. reloads or declarations)
        flix.remSourceCode("<shell>")
        flix.setOptions(flix.options.copy(entryPoint = None))

      case Category.Unknown =>
        // The input is not recognized. Output an error message.
        w.println("Error: Input cannot be parsed.")
    }
  }

  /**
    * Reloads and evaluates the given source code.
    */
  private def execReloadAndEval(s: String)(implicit terminal: Terminal): Unit = {
    execReload()
    execEval(s)
  }

  /**
    * Executes the given bootstrap function and prints any errors.
    */
  private def execBootstrap[T](f: => Validation[T, BootstrapError])(implicit formatter: Formatter, out: PrintStream): Unit = {
    f.errors.map(_.message(formatter)).foreach(out.println)
  }

  /**
    * Removes all code fragments, restoring the REPL to an initial state
    */
  private def clearFragments(): Unit = {
    for (i <- 0 to fragments.length)
      flix.remSourceCode("$" + i)
    fragments.clear()
  }

  /**
    * Reports unknown command.
    */
  private def execUnknown(s: String)(implicit terminal: Terminal): Unit = {
    terminal.writer().println(s"Unknown command '$s'. Try `:help'.")
  }

  /**
    * Compiles the current files and packages (first time from scratch, subsequent times incrementally)
    */
  private def compile(entryPoint: Option[Symbol.DefnSym] = None, progress: Boolean = true)(implicit terminal: Terminal): Validation[CompilationResult, CompilationMessage] = {

    // Set the main entry point if there is one (i.e. if the programmer wrote an expression)
    flix.setOptions(options.copy(entryPoint = entryPoint, progress = progress))

    val checkResult = flix.check().toHardFailure
    checkResult.toHardResult match {
      case Result.Ok(root) => this.root = Some(root)
      case Result.Err(_) => // no-op
    }

    val result = Validation.flatMapN(checkResult)(flix.codeGen)
    result.toHardResult match {
      case Result.Ok(_) => // Compilation successful, no-op
      case Result.Err(errors) =>
        for (msg <- flix.mkMessages(errors)) {
          terminal.writer().print(msg)
        }
        terminal.writer().println()
    }

    result
  }

  /**
    * Run the given main function
    */
  private def run(main: Symbol.DefnSym)(implicit terminal: Terminal): Unit = {
    // Recompile the program.
    compile(entryPoint = Some(main), progress = false).toHardResult match {
      case Result.Ok(result) =>
        result.getMain match {
          case Some(m) =>
            // Evaluate the main function
            try {
              m(Array.empty)
            } catch {
              case ex: Throwable =>
                ex.printStackTrace(terminal.writer())
            }

          case None =>
        }

      case Result.Err(_) => ()
    }
  }
}
