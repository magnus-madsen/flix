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

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, TypedAst}
import ca.uwaterloo.flix.language.fmt.Audience
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util._

import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.nio.file._
import java.util.concurrent.Executors
import java.util.logging.{Level, Logger}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Shell(initialPaths: List[Path], options: Options) {

  /**
    * The audience is always external.
    */
  private implicit val audience: Audience = Audience.External

  /**
    * The executor service.
    */
  private val executorService = Executors.newSingleThreadExecutor()

  /**
    * The mutable set of paths to load.
    */
  private val sourcePaths = mutable.Set.empty[Path] ++ initialPaths

  /**
    * The name of the current entry point.
    */
  private var entryPoint: Option[Symbol.DefnSym] = None

  /**
    * The mutable list of source code fragments.
    */
  private val fragments = mutable.Stack.empty[String]

  /**
    * The set of changed sources.
    */
  private var changeSet: Set[Ast.Input] = Set.empty

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private val flix: Flix = new Flix().setFormatter(AnsiTerminalFormatter)

  /**
    * The current compilation result (initialized on startup).
    */
  private var compilationResult: CompilationResult = _

  /**
    * The current watcher (if any).
    */
  private var watcher: WatcherThread = _

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
      .terminal(terminal)
      .build()

    // Print the welcome banner.
    printWelcomeBanner()

    // Trigger a compilation of the source input files.
    execReload()

    try {
      // Repeatedly try to read an input from the line reader.
      while (!Thread.currentThread().isInterrupted) {
        // Try to read a command.
        val line = reader.readLine(prompt)

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
  private def execute(cmd: Command)(implicit terminal: Terminal): Unit = cmd match {
    case Command.Nop => // nop
    case Command.Reload => execReload()
    case Command.Watch => execWatch()
    case Command.Unwatch => execUnwatch()
    case Command.Quit => execQuit()
    case Command.Help => execHelp()
    case Command.Praise => execPraise()
    case Command.Eval(s) => execEval(s)
    case Command.Unknown(s) => execUnknown(s)
  }

  /**
    * Reloads every source path.
    */
  private def execReload()(implicit terminal: Terminal): Validation[TypedAst.Root, CompilationMessage] = {
    // Instantiate a fresh flix instance.
    this.flix.setOptions(options.copy(entryPoint = entryPoint))

    // Add each path to Flix.
    for (path <- this.sourcePaths) {
      val ext = path.toFile.getName.split('.').last
      ext match {
        case "flix" => flix.addSourcePath(path)
        case "fpkg" => flix.addSourcePath(path)
        case "jar" => flix.addJar(path)
        case _ => throw new IllegalStateException(s"Unrecognized file extension: '$ext'.")
      }
    }

    // Compute the TypedAst and store it.
    val result = this.flix.check()
    result match {
      case Validation.Success(root) =>

        // Generate code.
        flix.codeGen(root) match {
          case Validation.Success(m) =>
            compilationResult = m
          case Validation.Failure(errors) =>
            for (error <- errors) {
              terminal.writer().print(error)
            }
        }
      case Validation.Failure(errors) =>
        terminal.writer().println()
        flix.mkMessages(errors)
          .foreach(terminal.writer().print)
        terminal.writer().println()
        terminal.writer().print(prompt)
        terminal.writer().flush()
    }

    // Return the result.
    result
  }

  /**
    * Watches source paths for changes.
    */
  private def execWatch()(implicit terminal: Terminal): Unit = {
    // Check if the watcher is already initialized.
    if (this.watcher != null) {
      terminal.writer().println("Already watching for changes.")
      return
    }

    // Compute the set of directories to watch.
    val directories = sourcePaths.map(_.toAbsolutePath.getParent).toList

    // Print debugging information.
    terminal.writer().println("Watching Directories:")
    for (directory <- directories) {
      terminal.writer().println(s"  $directory")
    }

    this.watcher = new WatcherThread(directories)
    this.watcher.start()
  }

  /**
    * Unwatches source paths for changes.
    */
  private def execUnwatch()(implicit terminal: Terminal): Unit = {
    this.watcher.interrupt()
    this.watcher = null
    terminal.writer().println("No longer watching for changes.")
  }

  /**
    * Exits the shell.
    */
  private def execQuit()(implicit terminal: Terminal): Unit = {
    Thread.currentThread().interrupt()
  }

  /**
    * Executes the help command.
    */
  private def execHelp()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    w.println("  Command       Arguments         Purpose")
    w.println()
    w.println("  :reload :r                      Recompiles every source file.")
    w.println("  :watch :w                       Watches all source files for changes.")
    w.println("  :unwatch :uw                    Unwatches all source files for changes.")
    w.println("  :quit :q                        Terminates the Flix shell.")
    w.println("  :help :h :?                     Shows this helpful information.")
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
        execReload() match {
          case Validation.Success(_) =>
            // Compilation succeeded.
            w.println("Ok.")
          case Validation.Failure(_) =>
            // Compilation failed. Ignore the last fragment.
            fragments.pop()
            flix.remSourceCode(name, s)
            w.println("Error: Declaration ignored due to previous error(s).")
        }

      case Category.Expr =>
        // The input is an expression. Wrap it in main and run it.

        // The name of the generated main function.
        val main = Symbol.mkDefnSym("shell1")

        val src =
          s"""def ${main.name}(): Unit & Impure =
             |println($s)
             |""".stripMargin
        flix.addSourceCode("<shell>", src)
        entryPoint = Some(main)
        run()

      case Category.Unknown =>
        // The input is not recognized. Output an error message.
        w.println("Error: Input input cannot be parsed.")
    }
  }

  /**
    * Reports unknown command.
    */
  private def execUnknown(s: String)(implicit terminal: Terminal): Unit = {
    terminal.writer().println(s"Unknown command '$s'. Try `:run` or `:help'.")
  }

  /**
    * Executes the eval command.
    */
  private def run()(implicit terminal: Terminal): Unit = {
    // Recompile the program.
    execReload()

    // Evaluate the main function and get the result.
    this.compilationResult.getMain match {
      case None => terminal.writer().println("No main function to run.")
      case Some(main) => main(Array.empty)
    }
  }

  /**
    * A thread to watch over changes in a collection of directories.
    */
  class WatcherThread(paths: List[Path])(implicit terminal: Terminal) extends Thread {

    /**
      * The minimum amount of time between runs of the compiler.
      */
    private val Delay: Long = 1000 * 1000 * 1000

    // Initialize a new watcher service.
    val watchService: WatchService = FileSystems.getDefault.newWatchService

    // Register each directory.
    for (path <- paths) {
      if (Files.isDirectory(path)) {
        path.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
      }
    }

    override def run(): Unit = try {
      // Record the last timestamp of a change.
      var lastChanged = System.nanoTime()

      // Loop until interrupted.
      while (!Thread.currentThread().isInterrupted) {
        // Wait for a set of events.
        val watchKey = watchService.take()
        // Iterate through each event.
        val changed = mutable.ListBuffer.empty[Path]
        for (event <- watchKey.pollEvents().asScala) {
          // Check if a file with ".flix" extension changed.
          val changedPath = event.context().asInstanceOf[Path]
          if (changedPath.toString.endsWith(".flix")) {
            changed += changedPath
          }
        }

        if (changed.nonEmpty) {
          // Update the change set.
          changeSet = changed.map(Ast.Input.TxtFile).toSet

          // Print information to the user.
          terminal.writer().println()
          terminal.writer().println(s"Recompiling. File(s) changed: ${changed.mkString(", ")}")
          terminal.writer().print(prompt)
          terminal.writer().flush()

          // Check if sufficient time has passed since the last compilation.
          val currentTime = System.nanoTime()
          if ((currentTime - lastChanged) >= Delay) {
            lastChanged = currentTime
            // Allow a short delay before running the compiler.
            Thread.sleep(50)
            executorService.submit(new Runnable {
              def run(): Unit = execReload()
            })
          }

        }

        // Reset the watch key.
        watchKey.reset()
      }
    } catch {
      case _: InterruptedException => // nop, shutdown.
    }

  }

}

