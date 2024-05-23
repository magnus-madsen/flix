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

import org.jline.terminal.Terminal

/**
  * A common super-type for commands.
  */
sealed trait Command

object Command {

  /**
    * Does literally nothing.
    */
  case object Nop extends Command

  /**
    * Reloads all source paths.
    */
  case object Reload extends Command

  /**
    * Displays documentation about the fqn s
    */
  case class Info(s: String) extends Command

  /**
    * Creates a new project in the current directory
    */
  case object Init extends Command

  /**
    * Builds the current project.
    */
  case object Build extends Command

  /**
    * Builds a jar file from the current project.
    */
  case object BuildJar extends Command

  /**
    * Builds a fatjar file from the current project.
    * Status: working on.
    */
  case object BuildFatJar extends Command

  /**
    * Builds an fpkg file from the current project.
    */
  case object BuildPkg extends Command

  /**
    * Publishes a release of the current project to GitHub.
    */
  case object Release extends Command

  /**
    * Checks the current project for errors.
    */
  case object Check extends Command

  /**
    * Generates API document for the current project.
    */
  case object Doc extends Command

  /**
    * Runs the tests for the current project.
    */
  case object Test extends Command

  /**
    * Show dependencies which have newer versions available.
    */
  case object Outdated extends Command

  /**
    * Terminates the shell.
    */
  case object Quit extends Command

  /**
    * Prints helpful information about the available commands.
    */
  case object Help extends Command

  /**
    * Praise Le Toucan.
    */
  case object Praise extends Command

  /**
    * Eval source code.
    */
  case class Eval(s: String) extends Command

  /**
    * Reload and eval source code.
    */
  case class ReloadAndEval(s: String) extends Command

  /**
    * Unknown command.
    */
  case class Unknown(s: String) extends Command

  /**
    * Runs the mutation tests given a tester and testee.
    */
  case class Mtest(tester: String, testee: String, percentage: Int) extends Command
  /**
    * Parses the given `input` into a command.
    */
  def parse(input: String)(implicit terminal: Terminal): Command = {
    //
    // Eof
    //
    if (input == null)
      return Command.Quit

    if (input.trim == "")
      return Command.Nop

    if (input == ":r" || input == ":reload")
      return Command.Reload

    val infoPattern = raw":i(nfo)?\s+(\S+)\s*".r
    input match {
      case infoPattern(_, s) => return Command.Info(s)
      case _ => // no-op
    }

    if (input == ":init")
      return Command.Init

    if (input == ":build" || input == ":b")
      return Command.Build

    if (input == ":check" || input == ":c")
      return Command.Check

    if (input == ":doc" || input == ":d")
      return Command.Doc

    if (input == ":build-jar" || input == ":jar")
      return Command.BuildJar

    if (input == ":build-pkg" || input == ":pkg")
      return Command.BuildPkg

    if (input == ":release")
      return Command.Release

    if (input.startsWith(":eval"))
      return Command.ReloadAndEval(input.drop(":eval".length + 1))

    if (input == ":test" || input == ":t")
      return Command.Test


    if (input == ":quit" || input == ":q")
      return Command.Quit

    if (input == ":help" || input == ":h" || input == ":?")
      return Command.Help

    if (input == ":praise")
      return Command.Praise

    if (input.startsWith(":mtest") || input.startsWith(":mt")) {
      val args = input.split(" ")
      if (args.length == 3) return  Command.Mtest(args(1), args(2), 100)
      return Command.Mtest(args(1), args(2), args(3).toInt)
    }

      //
    // Eval or Unknown?
    //
    if (input.startsWith(":"))
      Command.Unknown(input)
    else
      Command.Eval(input)
  }

}
