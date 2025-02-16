/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

import ca.uwaterloo.flix.api.effectlock.{Reachability, TrustValidation}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.fmt.FormatOptions
import ca.uwaterloo.flix.language.phase.*
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Summary
import ca.uwaterloo.flix.tools.pkg.Dependency
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.{Chain, ListMap, MultiMap}
import ca.uwaterloo.flix.util.tc.Debug

import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import java.util.concurrent.ForkJoinPool
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.Using

object Flix {
  /**
    * The reserved Flix delimiter.
    */
  val Delimiter: String = "$"

  /**
    * The file extension for intermediate representation files.
    */
  val IrFileExtension = "flixir"

  /**
    * The maximum width of the intermediate representation files.
    */
  val IrFileWidth = 80

  /**
    * The number of spaces per indentation in the intermediate representation files.
    */
  val IrFileIndentation = 4
}

/**
  * Main programmatic interface for Flix.
  */
class Flix {

  /**
    * A sequence of inputs to be parsed into Flix ASTs.
    */
  private val inputs = mutable.Map.empty[String, Input]

  /**
    * The set of sources changed since last compilation.
    */
  private var changeSet: ChangeSet = ChangeSet.Everything

  /**
    * The set of known Java classes and interfaces.
    */
  private var availableClasses: AvailableClasses = AvailableClasses(getJavaPlatformClassesAndInterfaces())

  /**
    * A cache of ASTs for incremental compilation.
    */
  private var cachedLexerTokens: Map[Source, Array[Token]] = Map.empty
  private var cachedParserCst: SyntaxTree.Root = SyntaxTree.empty
  private var cachedWeederAst: WeededAst.Root = WeededAst.empty
  private var cachedDesugarAst: DesugaredAst.Root = DesugaredAst.empty
  private var cachedKinderAst: KindedAst.Root = KindedAst.empty
  private var cachedResolverAst: ResolvedAst.Root = ResolvedAst.empty
  private var cachedTyperAst: TypedAst.Root = TypedAst.empty

  /**
    * A cache of error messages for incremental compilation.
    */
  private var cachedErrors: List[CompilationMessage] = Nil

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The core library *must* be present for any program to compile.
    */
  private val coreLibrary = List(
    // Prelude
    "Prelude.flix" -> LocalResource.get("/src/library/Prelude.flix"),

    // Comparison
    "Comparison.flix" -> LocalResource.get("/src/library/Comparison.flix"),

    // Coerce
    "Coerce.flix" -> LocalResource.get("/src/library/Coerce.flix"),

    // Operators
    "Neg.flix" -> LocalResource.get("/src/library/Neg.flix"),
    "Add.flix" -> LocalResource.get("/src/library/Add.flix"),
    "Sub.flix" -> LocalResource.get("/src/library/Sub.flix"),
    "Mul.flix" -> LocalResource.get("/src/library/Mul.flix"),
    "Div.flix" -> LocalResource.get("/src/library/Div.flix"),
    "Bool.flix" -> LocalResource.get("/src/library/Bool.flix"),

    // Channels and Threads
    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Thread.flix" -> LocalResource.get("/src/library/Thread.flix"),
    "Time.flix" -> LocalResource.get("/src/library/Time.flix"),

    // Built-in
    "Eq.flix" -> LocalResource.get("/src/library/Eq.flix"),
    "Hash.flix" -> LocalResource.get("/src/library/Hash.flix"),
    "Order.flix" -> LocalResource.get("/src/library/Order.flix"),

    // Lattices
    "PartialOrder.flix" -> LocalResource.get("/src/library/PartialOrder.flix"),
    "LowerBound.flix" -> LocalResource.get("/src/library/LowerBound.flix"),
    "UpperBound.flix" -> LocalResource.get("/src/library/UpperBound.flix"),
    "JoinLattice.flix" -> LocalResource.get("/src/library/JoinLattice.flix"),
    "MeetLattice.flix" -> LocalResource.get("/src/library/MeetLattice.flix"),

    // String
    "ToString.flix" -> LocalResource.get("/src/library/ToString.flix"),

    // Reflect
    "Reflect.flix" -> LocalResource.get("/src/library/Reflect.flix"),

    // Debug
    "Debug.flix" -> LocalResource.get("/src/library/Debug.flix"),

    // References
    "Ref.flix" -> LocalResource.get("/src/library/Ref.flix"),
  )

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  private val standardLibrary = List(
    "Array.flix" -> LocalResource.get("/src/library/Array.flix"),
    "Assert.flix" -> LocalResource.get("/src/library/Assert.flix"),
    "BigDecimal.flix" -> LocalResource.get("/src/library/BigDecimal.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Box.flix" -> LocalResource.get("/src/library/Box.flix"),
    "Chain.flix" -> LocalResource.get("/src/library/Chain.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "CodePoint.flix" -> LocalResource.get("/src/library/CodePoint.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "DelayList.flix" -> LocalResource.get("/src/library/DelayList.flix"),
    "DelayMap.flix" -> LocalResource.get("/src/library/DelayMap.flix"),
    "Down.flix" -> LocalResource.get("/src/library/Down.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "Iterable.flix" -> LocalResource.get("/src/library/Iterable.flix"),
    "Iterator.flix" -> LocalResource.get("/src/library/Iterator.flix"),
    "KeyNotFound.flix" -> LocalResource.get("/src/library/KeyNotFound.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Nec.flix" -> LocalResource.get("/src/library/Nec.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "OutOfBounds.flix" -> LocalResource.get("/src/library/OutOfBounds.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),
    "MultiMap.flix" -> LocalResource.get("/src/library/MultiMap.flix"),

    "MutQueue.flix" -> LocalResource.get("/src/library/MutQueue.flix"),
    "MutDeque.flix" -> LocalResource.get("/src/library/MutDeque.flix"),
    "MutDisjointSets.flix" -> LocalResource.get("/src/library/MutDisjointSets.flix"),
    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),

    "IoError.flix" -> LocalResource.get("/src/library/IoError.flix"),
    "Reader.flix" -> LocalResource.get("/src/library/Reader.flix"),

    "Environment.flix" -> LocalResource.get("/src/library/Environment.flix"),

    "Applicative.flix" -> LocalResource.get("/src/library/Applicative.flix"),
    "CommutativeGroup.flix" -> LocalResource.get("/src/library/CommutativeGroup.flix"),
    "CommutativeMonoid.flix" -> LocalResource.get("/src/library/CommutativeMonoid.flix"),
    "CommutativeSemiGroup.flix" -> LocalResource.get("/src/library/CommutativeSemiGroup.flix"),
    "Foldable.flix" -> LocalResource.get("/src/library/Foldable.flix"),
    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "Filterable.flix" -> LocalResource.get("/src/library/Filterable.flix"),
    "Group.flix" -> LocalResource.get("/src/library/Group.flix"),
    "Identity.flix" -> LocalResource.get("/src/library/Identity.flix"),
    "Indexable.flix" -> LocalResource.get("/src/library/Indexable.flix"),
    "IndexableMut.flix" -> LocalResource.get("/src/library/IndexableMut.flix"),
    "Monad.flix" -> LocalResource.get("/src/library/Monad.flix"),
    "MonadZero.flix" -> LocalResource.get("/src/library/MonadZero.flix"),
    "MonadZip.flix" -> LocalResource.get("/src/library/MonadZip.flix"),
    "Monoid.flix" -> LocalResource.get("/src/library/Monoid.flix"),
    "Reducible.flix" -> LocalResource.get("/src/library/Reducible.flix"),
    "SemiGroup.flix" -> LocalResource.get("/src/library/SemiGroup.flix"),
    "Traversable.flix" -> LocalResource.get("/src/library/Traversable.flix"),
    "Witherable.flix" -> LocalResource.get("/src/library/Witherable.flix"),
    "UnorderedFoldable.flix" -> LocalResource.get("/src/library/UnorderedFoldable.flix"),
    "Collectable.flix" -> LocalResource.get("/src/library/Collectable.flix"),

    "Validation.flix" -> LocalResource.get("/src/library/Validation.flix"),

    "StringBuilder.flix" -> LocalResource.get("/src/library/StringBuilder.flix"),
    "RedBlackTree.flix" -> LocalResource.get("/src/library/RedBlackTree.flix"),
    "GetOpt.flix" -> LocalResource.get("/src/library/GetOpt.flix"),
    "Chalk.flix" -> LocalResource.get("/src/library/Chalk.flix"),

    "Concurrent/Channel.flix" -> LocalResource.get("/src/library/Concurrent/Channel.flix"),
    "Concurrent/Condition.flix" -> LocalResource.get("/src/library/Concurrent/Condition.flix"),
    "Concurrent/CyclicBarrier.flix" -> LocalResource.get("/src/library/Concurrent/CyclicBarrier.flix"),
    "Concurrent/ReentrantLock.flix" -> LocalResource.get("/src/library/Concurrent/ReentrantLock.flix"),

    "Time/Duration.flix" -> LocalResource.get("/src/library/Time/Duration.flix"),
    "Time/Epoch.flix" -> LocalResource.get("/src/library/Time/Epoch.flix"),
    "Time/Instant.flix" -> LocalResource.get("/src/library/Time/Instant.flix"),

    "Fixpoint/Phase/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Stratifier.flix"),
    "Fixpoint/Phase/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Compiler.flix"),
    "Fixpoint/Phase/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Simplifier.flix"),
    "Fixpoint/Phase/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/IndexSelection.flix"),
    "Fixpoint/Phase/VarsToIndices.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/VarsToIndices.flix"),
    "Fixpoint/Boxable.flix" -> LocalResource.get("/src/library/Fixpoint/Boxable.flix"),
    "Fixpoint/Boxed.flix" -> LocalResource.get("/src/library/Fixpoint/Boxed.flix"),
    "Fixpoint/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint/Debugging.flix"),
    "Fixpoint/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint/Interpreter.flix"),
    "Fixpoint/Options.flix" -> LocalResource.get("/src/library/Fixpoint/Options.flix"),
    "Fixpoint/PredSymsOf.flix" -> LocalResource.get("/src/library/Fixpoint/PredSymsOf.flix"),
    "Fixpoint/Solver.flix" -> LocalResource.get("/src/library/Fixpoint/Solver.flix"),
    "Fixpoint/SubstitutePredSym.flix" -> LocalResource.get("/src/library/Fixpoint/SubstitutePredSym.flix"),

    "Fixpoint/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Datalog.flix"),
    "Fixpoint/Ast/Shared.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Shared.flix"),
    "Fixpoint/Ast/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/PrecedenceGraph.flix"),
    "Fixpoint/Ast/Ram.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Ram.flix"),

    "Abort.flix" -> LocalResource.get("/src/library/Abort.flix"),
    "Clock.flix" -> LocalResource.get("/src/library/Clock.flix"),
    "Http.flix" -> LocalResource.get("/src/library/Http.flix"),
    "HttpWithResult.flix" -> LocalResource.get("/src/library/HttpWithResult.flix"),
    "Exit.flix" -> LocalResource.get("/src/library/Exit.flix"),
    "Eff/BiasedCoin.flix" -> LocalResource.get("/src/library/Eff/BiasedCoin.flix"),
    "Eff/RandomCoin.flix" -> LocalResource.get("/src/library/Eff/RandomCoin.flix"),
    "Logger.flix" -> LocalResource.get("/src/library/Logger.flix"),
    "FileRead.flix" -> LocalResource.get("/src/library/FileRead.flix"),
    "FileReadWithResult.flix" -> LocalResource.get("/src/library/FileReadWithResult.flix"),
    "FileWrite.flix" -> LocalResource.get("/src/library/FileWrite.flix"),
    "FileWriteWithResult.flix" -> LocalResource.get("/src/library/FileWriteWithResult.flix"),
    "ProcessHandle.flix" -> LocalResource.get("/src/library/ProcessHandle.flix"),
    "Process.flix" -> LocalResource.get("/src/library/Process.flix"),
    "ProcessWithResult.flix" -> LocalResource.get("/src/library/ProcessWithResult.flix"),
    "Severity.flix" -> LocalResource.get("/src/library/Severity.flix"),
    "TimeUnit.flix" -> LocalResource.get("/src/library/TimeUnit.flix"),

    "Graph.flix" -> LocalResource.get("/src/library/Graph.flix"),
    "Vector.flix" -> LocalResource.get("/src/library/Vector.flix"),
    "Regex.flix" -> LocalResource.get("/src/library/Regex.flix"),
    "Adaptor.flix" -> LocalResource.get("/src/library/Adaptor.flix"),
    "ToJava.flix" -> LocalResource.get("/src/library/ToJava.flix"),
    "ToFlix.flix" -> LocalResource.get("/src/library/ToFlix.flix"),
  )

  /**
    * A map to track the time spent in each phase and sub-phase.
    */
  var phaseTimers: ListBuffer[PhaseTime] = ListBuffer.empty

  /**
    * The current phase we are in. Initially null.
    */
  private var currentPhase: PhaseTime = _

  /**
    * The progress bar.
    */
  private val progressBar: ProgressBar = new ProgressBar

  /**
    * The currently registered event listeners.
    */
  private val listeners: ListBuffer[FlixListener] = ListBuffer.empty

  /**
    * The default assumed charset.
    */
  val defaultCharset: Charset = Charset.forName("UTF-8")

  /**
    * The current Flix options.
    */
  var options: Options = Options.Default

  /**
    * The thread pool executor service for `this` Flix instance.
    */
  var threadPool: java.util.concurrent.ForkJoinPool = _

  /**
    * The symbol generator associated with this Flix instance.
    */
  val genSym = new GenSym()

  /**
    * The default output formatter.
    */
  private var formatter: Formatter = NoFormatter

  /**
    * A class loader for loading external JARs.
    */
  val jarLoader = new ExternalJarLoader

  /**
    * Adds the given string `text` with the given `name`.
    */
  def addSourceCode(name: String, text: String)(implicit sctx: SecurityContext): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    addInput(name, Input.Text(name, text, sctx))
    this
  }

  /**
    * Removes the source code with the given `name`.
    */
  def remSourceCode(name: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    remInput(name, Input.Text(name, "", /* unused */ SecurityContext.NoPermissions))
    this
  }

  /**
    * Adds the given path `p` as Flix source file.
    */
  def addFlix(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    addInput(p.toString, Input.TxtFile(p, sctx))
    this
  }

  /**
    * Adds the given path `p` as a Flix package file.
    */
  def addPkg(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")
    if (!p.getFileName.toString.endsWith(".fpkg"))
      throw new IllegalArgumentException(s"'$p' must be a *.pkg file.")

    addInput(p.toString, Input.PkgFile(p, sctx))
    this
  }

  /**
    * Removes the given path `p` as a Flix source file.
    */
  def remFlix(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    remInput(p.toString, Input.TxtFile(p, sctx))
    this
  }

  /**
    * Adds the JAR file at path `p` to the class loader.
    */
  def addJar(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")

    jarLoader.addURL(p.toUri.toURL)
    extendKnownJavaClassesAndInterfaces(p)
    this
  }

  /**
    * Adds the given `input` under the given `name`.
    */
  private def addInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None =>
      inputs += name -> input
    case Some(_) =>
      changeSet = changeSet.markChanged(input, cachedTyperAst.dependencyGraph)
      inputs += name -> input
  }

  /**
    * Removes the given `input` under the given `name`.
    *
    * Note: Removing an input means to replace it by the empty string.
    */
  private def remInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None => // nop
    case Some(_) =>
      changeSet = changeSet.markChanged(input, cachedTyperAst.dependencyGraph)
      inputs += name -> Input.Text(name, "", /* unused */ SecurityContext.NoPermissions)
  }

  /**
    * Sets the options used for this Flix instance.
    */
  def setOptions(opts: Options): Flix = {
    if (opts == null)
      throw new IllegalArgumentException("'opts' must be non-null.")
    options = opts
    this
  }

  /**
    * Returns the format options associated with this Flix instance.
    */
  def getFormatOptions: FormatOptions = {
    FormatOptions(
      varNames = FormatOptions.VarName.NameBased // TODO add cli option
    )
  }

  /**
    * Returns the current formatter instance.
    */
  def getFormatter: Formatter = this.formatter

  /**
    * Sets the output formatter used for this Flix instance.
    */
  def setFormatter(formatter: Formatter): Flix = {
    if (formatter == null)
      throw new IllegalArgumentException("'formatter' must be non-null.")
    this.formatter = formatter
    this
  }

  /**
    * Converts a list of compiler error messages to a list of printable messages.
    * Decides whether or not to append the explanation.
    */
  def mkMessages(errors: List[CompilationMessage]): List[String] = {
    if (options.explain)
      errors.sortBy(_.loc).map(cm => cm.messageWithLoc(formatter) + cm.explain(formatter).getOrElse(""))
    else
      errors.sortBy(_.loc).map(cm => cm.messageWithLoc(formatter))
  }

  /**
    * Compiles the Flix program and returns a typed ast.
    * If the list of [[CompilationMessage]]s is empty, then the root is always `Some(root)`.
    */
  def check(): (Option[TypedAst.Root], List[CompilationMessage]) = try {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    // Reset the phase information.
    phaseTimers = ListBuffer.empty

    // Reset the phase list file if relevant
    if (this.options.xprintphases) {
      AstPrinter.resetPhaseFile()
    }

    // We mark all inputs that contains compilation errors as dirty.
    // Hence if a file contains an error it will be recompiled -- giving it a chance to disappear.
    for (e <- cachedErrors) {
      val i = e.loc.sp1.source.input
      changeSet = changeSet.markChanged(i, cachedTyperAst.dependencyGraph)
    }

    // The default entry point
    val entryPoint = flix.options.entryPoint

    // The global collection of errors
    val errors = mutable.ListBuffer.empty[CompilationMessage]

    val (afterReader, readerErrors) = Reader.run(getInputs, availableClasses)
    errors ++= readerErrors

    val (afterLexer, lexerErrors) = Lexer.run(afterReader, cachedLexerTokens, changeSet)
    errors ++= lexerErrors

    val (afterParser, parserErrors) = Parser2.run(afterLexer, cachedParserCst, changeSet)
    errors ++= parserErrors

    val (weederValidation, weederErrors) = Weeder2.run(afterReader, entryPoint, afterParser, cachedWeederAst, changeSet)
    errors ++= weederErrors

    val result = weederValidation match {
      case Validation.Failure(failures) =>
        errors ++= failures.toList
        None

      case Validation.Success(afterWeeder) =>
        val afterDesugar = Desugar.run(afterWeeder, cachedDesugarAst, changeSet)

        val (afterNamer, nameErrors) = Namer.run(afterDesugar)
        errors ++= nameErrors

        val (resolverValidation, resolutionErrors) = Resolver.run(afterNamer, cachedResolverAst, changeSet)
        errors ++= resolutionErrors

        resolverValidation match {
          case Validation.Failure(failures) =>
            errors ++= failures.toList
            None

          case Validation.Success(afterResolver) =>

            val (afterKinder, kindErrors) = Kinder.run(afterResolver, cachedKinderAst, changeSet)
            errors ++= kindErrors

            val (afterDeriver, derivationErrors) = Deriver.run(afterKinder)
            errors ++= derivationErrors

            val (afterTyper, typeErrors) = Typer.run(afterDeriver, cachedTyperAst, changeSet)
            errors ++= typeErrors

            EffectVerifier.run(afterTyper)

            val (afterEntryPoint, entryPointErrors) = EntryPoints.run(afterTyper)
            errors ++= entryPointErrors

            val (afterInstances, instanceErrors) = Instances.run(afterEntryPoint, cachedTyperAst, changeSet)
            errors ++= instanceErrors

            val (afterPredDeps, predDepErrors) = PredDeps.run(afterInstances, cachedTyperAst, changeSet)
            errors ++= predDepErrors

            val (afterStratifier, stratificationErrors) = Stratifier.run(afterPredDeps)
            errors ++= stratificationErrors

            val (afterPatMatch, patMatchErrors) = PatMatch.run(afterStratifier, cachedTyperAst, changeSet)
            errors ++= patMatchErrors

            val (afterRedundancy, redundancyErrors) = Redundancy.run(afterPatMatch)
            errors ++= redundancyErrors

            val (_, safetyErrors) = Safety.run(afterRedundancy, cachedTyperAst, changeSet)
            errors ++= safetyErrors

            val (afterDependencies, _) = Dependencies.run(afterRedundancy, cachedTyperAst, changeSet)

            if (options.incremental) {
              this.cachedLexerTokens = afterLexer
              this.cachedParserCst = afterParser
              this.cachedWeederAst = afterWeeder
              this.cachedDesugarAst = afterDesugar
              this.cachedKinderAst = afterKinder
              this.cachedResolverAst = afterResolver
              this.cachedTyperAst = afterDependencies

              // We record that no files are dirty in the change set.
              this.changeSet = ChangeSet.Dirty(Set.empty)

              // We save all the current errors.
              this.cachedErrors = errors.toList
            }

            Some(afterDependencies)
        }
    }
    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Print summary?
    if (options.xsummary) {
      result.foreach(root => {
        val table = Summary.fileSummaryTable(root, nsDepth = Some(1), minLines = Some(125))
        table.getMarkdownLines.foreach(println)
      })
    }

    // Return the result (which could contain soft failures).
    (result, errors.toList)
  } catch {
    case ex: InternalCompilerException =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def codeGen(typedAst: TypedAst.Root): Validation[CompilationResult, CompilationMessage] = try {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    val loweringAst = Lowering.run(typedAst)
    val treeShaker1Ast = TreeShaker1.run(loweringAst)
    val monomorpherAst = Monomorpher.run(treeShaker1Ast)
    val simplifierAst = Simplifier.run(monomorpherAst)
    val closureConvAst = ClosureConv.run(simplifierAst)
    val lambdaLiftAst = LambdaLift.run(closureConvAst)
    val optimizerAst = Optimizer.run(lambdaLiftAst)
    val treeShaker2Ast = TreeShaker2.run(optimizerAst)
    val effectBinderAst = EffectBinder.run(treeShaker2Ast)
    val tailPosAst = TailPos.run(effectBinderAst)
    Verifier.run(tailPosAst)
    val eraserAst = Eraser.run(tailPosAst)
    val reducerAst = Reducer.run(eraserAst)
    val varOffsetsAst = VarOffsets.run(reducerAst)
    val result = JvmBackend.run(varOffsetsAst)

    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Return the result.
    Validation.Success(result)
  } catch {
    case ex: InternalCompilerException =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
    case ex: Throwable =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Validation[CompilationResult, CompilationMessage] = {
    val (result, errors) = check()
    if (errors.isEmpty) {
      codeGen(result.get)
    } else {
      Validation.Failure(Chain.from(errors))
    }
  }

  /**
    * Enters the phase with the given name.
    */
  def phaseNew[A, B](phase: String)(f: => (A, B))(implicit d: Debug[A]): (A, B) = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "", sample = false)
    }

    // Measure the execution time.
    val t = System.nanoTime()
    val (root, errs) = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    if (this.options.xprintphases) {
      d.output(phase, root)(this)
    }

    // Return the result computed by the phase.
    (root, errs)
  }

  /**
    * Enters the phase with the given name.
    */
  def phase[A](phase: String)(f: => A)(implicit d: Debug[A]): A = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "", sample = false)
    }

    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    if (this.options.xprintphases) {
      d.output(phase, r)(this)
    }

    // Return the result computed by the phase.
    r
  }

  /**
    * Returns the total compilation time in nanoseconds.
    */
  def getTotalTime: Long = phaseTimers.foldLeft(0L) {
    case (acc, phase) => acc + phase.time
  }

  /**
    * A callback to indicate that work has started on the given subtask.
    */
  def subtask(subtask: String, sample: Boolean = false): Unit = {
    if (options.progress) {
      progressBar.observe(currentPhase.phase, subtask, sample)
    }
  }

  /**
    * Registers the given Flix event listener `l`.
    */
  def addListener(l: FlixListener): Unit = {
    listeners.addOne(l)
  }

  /**
    * Emits the given Flix event to all registered listeners.
    */
  def emitEvent(e: FlixEvent): Unit = {
    listeners.foreach(_.notify(e))
  }

  /**
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val lib = options.lib match {
      case LibLevel.Nix => Nil
      case LibLevel.Min => getLibraryInputs(coreLibrary)
      case LibLevel.All => getLibraryInputs(coreLibrary ++ standardLibrary)
    }
    inputs.values.toList ::: lib
  }

  /**
    * Returns the inputs for the given list of (path, text) pairs.
    */
  private def getLibraryInputs(xs: List[(String, String)]): List[Input] = xs.foldLeft(List.empty[Input]) {
    case (xs, (virtualPath, text)) => Input.Text(virtualPath, text, SecurityContext.AllPermissions) :: xs
  }

  /**
    * Initializes the fork-join thread pool.
    */
  private def initForkJoinPool(): Unit = {
    threadPool = new ForkJoinPool(options.threads)
  }

  /**
    * Shuts down the fork-join thread pools.
    */
  private def shutdownForkJoinPool(): Unit = {
    threadPool.shutdown()
  }

  /**
    * Extends the set of known Java classes and interfaces with those in the given JAR-file `p`.
    */
  private def extendKnownJavaClassesAndInterfaces(p: Path): Unit = {
    availableClasses = availableClasses ++ getPackageContent(getClassesAndInterfacesOfJar(p))
  }

  /**
    * Returns all Java classes and interfaces in the current Java Platform.
    */
  private def getJavaPlatformClassesAndInterfaces(): MultiMap[List[String], String] = {
    getPackageContent(ClassList.TheList)
  }

  /**
    * Returns the names of all classes and interfaces in the given JAR-file `p`.
    */
  private def getClassesAndInterfacesOfJar(p: Path): List[String] = {
    Using(new ZipFile(p.toFile)) { zip =>
      val result = mutable.ListBuffer.empty[String]
      val iterator = zip.entries()
      while (iterator.hasMoreElements) {
        val entry = iterator.nextElement()
        val name = entry.getName
        if (name.endsWith(".class")) {
          result += name
        }
      }
      result.toList
    }.get
  }

  /**
    * Returns a multimap from Java packages to sub-packages, classes, and interfaces.
    */
  private def getPackageContent(l: List[String]): MultiMap[List[String], String] = {
    l.foldLeft[MultiMap[List[String], String]](MultiMap.empty) {
      case (acc, clazz) =>
        // Given a string `java/util/zip/ZipUtils.class` we convert it to the list `java :: util :: zip :: ZipUtils`.
        // We strip both the ".class" and ".java" suffix. Order should not matter.
        val clazzPath = clazz.stripSuffix(".class").stripSuffix(".java").split('/').toList

        // Create a multimap from all package prefixes to their sub packages and classes.
        // For example, if we have `java.lang.String`, we want to compute:
        // Nil                  => {java}
        // List("java")         => {lang}
        // List("java", "lang") => {String}
        clazzPath.inits.foldLeft(acc) {
          // Case 1: Nonempty path: split prefix and package
          case (acc1, prefix :+ pkg) => acc1 + (prefix -> pkg)
          // Case 2: Empty path: skip it
          case (acc1, _) => acc1
        }
    }
  }

  /**
    * Temporary function for testing
    */
  def effectLockReachable(): Option[TypedAst.Root] = {
    val (optRoot, errors) = check()
    if (errors.isEmpty) {
      // Mark this object as implicit.
      implicit val flix: Flix = this

      // Initialize fork-join thread pool.
      initForkJoinPool()

      val (root, occurrenceInfo) = Reachability.run(optRoot.get)

      shutdownForkJoinPool()

      Some(root)

    } else {
      None
    }
  }

  def reachableLibraryFunctions(root: TypedAst.Root): (Map[Path, List[TypedAst.Def]], ListMap[Symbol, SourceLocation]) = {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()
    val (filteredRoot, uses) = Reachability.run(root)
    shutdownForkJoinPool()

    // Filter for public functions in libraries
    val reachable = filteredRoot.defs.foldLeft(Map.empty[Path, List[TypedAst.Def]]) {
      case (acc, (sym, defn)) if defn.spec.mod.isPublic => sym.loc.sp1.source.input match {
        case Input.FileInPackage(path, _, _, _) =>
          val defs = acc.getOrElse(path, List.empty)
          acc + (path -> (defn :: defs))

        case Input.Text(_, _, _) | Input.TxtFile(_, _) | Input.PkgFile(_, _) | Input.Unknown => acc
      }
      case (acc, _) => acc
    }
    (reachable, uses)
  }

  def validateTrust(root: TypedAst.Root, dependencies: Set[Dependency.FlixDependency]): List[BootstrapError.TrustError] = {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()
    val result = TrustValidation.run(root, dependencies)
    shutdownForkJoinPool()

    result
  }
}
