# Changelog

Version 0.58.0:
- Added support for plain LSP (thanks Chenhao!)

Version 0.57.1:
- Compiler: Extend list of semantic tokens (thanks Chenhao!)

Version 0.57.0:
- Compiler: Added new type inference implementation (thanks Matt!)
- Compiler: Improved incremental compilation (thanks Chenhao!)
- LSP: Improved dot autocompletion (thanks Chenhao!)

Version 0.56.0:
- Language: Added support for first-class handlers (thanks Jonathan!)
- Library: Extended the `Process` effect (thanks Maxim!)
- Compiler: Improved support for incremental compilation (thanks Magnus!)

Version 0.55.0:
- Compiler: Introduced fuzzy match for LSP completions (thanks Chenhao!)
- Compiler: Improved performance of LSP API (thanks Alexander!)

Version 0.54.0:
- Compiler: Improved auto-completion suggestions (thanks Chenhao!)
- Compiler: Added `AutoImportCompleter` (thanks Chenhao!)
- Compiler: Added `AutoUseCompleter` (thanks Chenhao!)
- Compiler: Added `LocalScopeCompleter` (thanks Chenhao!)
- Compiler: Reworked compilation of enums (thanks Jonathan!)

Version 0.53.0:
- Language: Added support for algebraic effects (thanks everyone!)
- Library: Added algebraic effects: Console, FileRead, FileWrite, Http, ... (thanks Holger and Magnus!)
- Compiler: Reworked and extended the HoverProvider (thanks Alexander!)
- Compiler: Reworked and extended the HighlightProvider (thanks Alexander!)
- Compiler: Added magic match completions (thanks Chenhao!)

Version 0.52.0:
- Compiler: Improved compilation of recursive functions, leading to significant speed-ups (thanks Jakob!)

Version 0.51.0:
- Language: Added support for mutable structs (thanks Joseph!)
- Language: Added support for `Indexable` and `IndexableMut` (thanks Jonathan!)
- Compiler: Improved keyword completion (thanks Alexander!)

Version 0.50.0:
- Language: Introduced `redef` as a short-hand to redefine functions in trait instances (thanks Jakob!)
- Compiler: Significantly improved performance of auto-complete (thanks Magnus!)

Version 0.49.0:
- Language: Significantly improved Java interoperability (thanks Chanattan!)
- Language: Added support for `throw` for Java interoperability (thanks Joseph!)

Version 0.48.0:
- Compiler: Added support for resilient lexing and parsing (thanks Herluf!)

Version 0.47.0:
- Language: Added support for associated effects (thanks Matt!)
- Library: Added `Iterable` instances for maps (thanks Stephen!)
- Library: Added associated effects to `Iterable` and `Foldable` (thanks Stephen!)
- Library: Added `FromJava` and `ToJava` and instances (thanks Stephen!)
- Library: Added missing `toVector` functions (thanks Stephen!)
- Compiler: Reduced memory usage of types (thanks Magnus!)

Version 0.46.0: 
- Language: Added support for associated types (thanks Matt!)
- Compiler: Reworked type inference to use constraints (thanks Matt!)
- Compiler: Reduced memory usage of types and source locations (thanks Magnus!)

Version 0.45.0:
- Compiler: Added support for the `build-fatjar` command (thanks Chanattan!)
- Compiler: Added support for the `outdated` command (thanks Holger!)
- Library: Added `Coerce` type class (thanks Magnus!)
- Library: Added `purityOf` functions (thanks Jakob!)

Version 0.44.0:
- Language: Reworked `IO` to be its own effect (thanks Matt!)
- Library: Added `purityOf` to simplify reflection on purity (thanks Jakob!)
- Compiler: Added support for the `release` command (thanks Holger!)
- Compiler: Added support for storing GITHUB_TOKEN in a file (thanks Holger!)
- Compiler: Added support for storing GITHUB_TOKEN in the environment (thanks Holger!)
- Compiler: Simplified compilation of enums (thanks Jonathan!)
- Compiler: Improved performance of backend (thanks Jonathan!)

Version 0.43.0:
- Language: Flix now targets Java 21 bytecode (thanks Magnus!)
- Language: Flix now uses `VirtualThread`s by default (thanks Magnus!) 
- Compiler: Flix now requires Java 21 (thanks Magnus!)
- Compiler: All `InstanceError`s are now recoverable (thanks Magnus!)
- Compiler: All `RedundancyError`s are now recoverable (thanks Magnus!)
- Compiler: All `SafetyErrors`s are now recoverable (thanks Magnus!)
- Compiler: All `TypeError`s are now recoverable (thanks Matt!)
- Compiler: Most `ResolutionError`s are now recoverable (thanks Magnus!)
- Compiler: Most `WeederError`s are now recoverable (thanks Magnus!)

Version 0.42.0:
- Compiler: Use parallel evaluation in `ClosureConv` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Deriver` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Inliner` (thanks Magnus!)
- Compiler: Use parallel evaluation in `LambdaLift` (thanks Magnus!)
- Compiler: Use parallel evaluation in `MonoDefs` (thanks Magnus!)
- Compiler: Use parallel evaluation in `MonoTypes` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Namer` (thanks Magnus!)
- Compiler: Use parallel evaluation in `PatMatch` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Reducer` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Resolver` (thanks Magnus!)
- Compiler: Use parallel evaluation in `Safety` (thanks Magnus!)
- Compiler: Use parallel evaluation in `TailRec` (thanks Magnus!)

Version 0.41.0:
- Library: Added `Adaptor` module (thanks Stephen!)
- Library: Added `Reader` module (thanks Stephen!)
- Compiler: Reworked type inference for regions and letrec (thanks Magnus, Matt!)
- Compiler: Major improvements to the HTML API documentation tool (thanks Holger!)
- Compiler: Added new experimental lexer (thanks Herluf!)

Version 0.40.0:

- Language: Added support for pattern matching on records (thanks Jakob!)
- Library: Added `MutPriorityQueue` (thanks Xavier!)
- VSCode: Added completion for `use` of signatures and operations (thanks Xavier!)
- VSCode: Added code actions to derive missing type class instances (thanks Holger!)
- VSCode: Added code actions for unused code (thanks Holger!)
- Compiler: Added new HTML API documentation tool (thanks Holger!)
  - Emits plain HTML and CSS.
  - Supports "light" and "dark" modes.

Version 0.39.0:

- VSCode: Added support for code actions (thanks Magnus, Holger!)
- VSCode: Added code actions to derive `Eq`, `Order`, and `ToString` (thanks Magnus!)
- VSCode: Added code actions to `use` an undefined name (thanks Holger!)
- VSCode: Added completion for `use` of enums, defs, modules, and types (thanks Xavier!)
- VSCode: Improved communication between the Flix compiler and VSCode in case of errors (thanks Holger!)
- Compiler: Improved source locations of several error messages (thanks Matthew, Ziyao!)
- Compiler: Relaxed Maven version string requirements (thanks Stephen, Magnus!)

Version 0.38.0:

- VSCode: Added preliminary support for code actions (thanks Holger!)
  - Added quickfix for unused local variable (thanks Magnus!)
  - Added quickfix to introduce new type (thanks Magnus!)
- Library: Added `toVector` functions (thanks Stephen!)
- Library: Use `Vector` in Datalog engine (thanks Holger!)
- Compiler: Significantly improved performance of compiler backend (thanks Jonathan, Matt!)

Version 0.37.0:

- Language: Uniform effect syntax (thanks Matthew!)
- Library: Added `CodePoint` module (thanks Stephen!)
- Library: Expanded `Regex` module (thanks Stephen!)
- Library: Added `String.stripMargin` (thanks Ziyao!)
- VSCode: Improved auto-completion for tags (thanks Lukas!)
- VSCode: Added support for auto-completion of modules (thanks Lukas!)
- VSCode: Use parser for auto-completion (thanks Magnus!)
- Compiler: Allow inclusion of external JARs (thanks Anna!)
- Compiler: Lots of internal improvements (thanks everyone!)

Version 0.36.0:

- Language: Added support for regular expressions literals (thanks Stephen!)
- Language: Added support for `functional` predicates (thanks Magnus!)
- Language: Reworked and removed the need for `Boxable` (thanks Magnus!)
- VSCode: Added support for parser-guided auto-completion (thanks Magnus)
- VSCode: Improved auto-completion of predicates (thanks Magnus!)
- VSCode: Improved "best pick" algorithm (thanks Lukas!)
- Compiler: Enabled package management command in repl (thanks Magnus!)

Version 0.35.0:

- Language: Added region capabilities (thanks Jonathan, Magnus)
- Language: Added initial support for structured concurrency (thanks Paul!)
- Language: Added `foreach` and `foreach-yield` syntactic sugar for imperative iteration (thanks Jakob!)
- Language: Added `forA` and `forM` syntactic sugar for applicative and monadic expressions (thanks Jakob!)
- Language: Added `checked_cast` and `checked_ecast` (thanks Magnus!)
- Language: Reworked the module system (thanks Matthew!)
- Library: Added `Iterator` module (thanks Stephen!)
- Library: Added `Regex` module (thanks Stephen!)
- Library: Added `Vector` module (thanks Stephen!)
- Library: Added region capabilities to all mutable data structures (thanks Jakob, Paul, Stephen!)
- VSCode: Allow most IDE features to work in the presence of program errors (thanks Jakob, Paul, Magnus!)
- VSCode: Added support for hole completions (thanks Daniel!)
- Compiler: Added initial support for Flix package management (thanks Anna!)
- Compiler: Added initial support for integration with Maven (thanks Anna!)
- Compiler: Improved type inference for function applications (thanks Magnus!)
- Compiler: Use BDDs to represent effects (thanks Anna!)
- Compiler: Improved performance of JVM code generation (thanks Magnus!)

Version 0.34.0:
- Language: Reworked channels into a `Sender` and `Receiver` part (thanks Paul!)
- Language: Added `Sendable` type class and require it for messages sent on channels (thanks Paul!)
- Language: Removed old deprecated record syntax (thanks Magnus!)
- Library: Added new `Graph` namespace (thanks Jonathan!)
- VSCode: Added support for auto-complete of fields (thanks Daniel!)
- VSCode: Added support for auto-complete of uses (thanks Daniel!)
- VSCode: Reworked prioritization of auto-complete suggestions (thanks Daniel!)
- Compiler: Improved performance of Boolean unification (thanks Magnus!)
- Compiler: Flix closures are now instances of many Java function types (thanks Stephen!)
- Compiler: Improved type errors reported for function applications (thanks Magnus!)

Version 0.33.0:
- Language: Added support for `BigDecimal` (thanks Stephen!)
- Language: Simplified the channel API (thanks Magnus, Paul!)
- Compiler: The channel implementation is now independent of the compiler (thanks Jonathan, Paul!)
- VSCode: Added support for auto-complete of imports (thanks Daniel!)
- VSCode: Fixed a bug where VSCode and the REPL could get out sync (thanks Magnus!)

Version 0.32.0:
- Language: Added new debugging facilities (thanks Jonathan, Matt, Magnus, Paul!)
  - Added `stringify` construct.
  - Added `debug`, `debug!` and `debug!!` constructs.
  - Added debug string interpolation.
- Language: Allow labelled arguments in enums and tuples (thanks Matt!)
- Language: Added experimental support for Virtual Threads (thanks Magnus!)
- Compiler: Fixed a bug in code generation for `NewObject` (thanks Paul!)
- Compiler: Fixed a bug in code generation for `RecordRestrict` (thanks Jonathan!)
- VSCode: Added support for match completion (thanks Daniel!)
- VSCode: Added support for auto-completion of cases (thanks Daniel!)
- VSCode: Improved integration between VSCode and the REPL (thanks Magnus!)

Version 0.31.0:
- Language: Added new object expressions (thanks Paul!)
- Language: Added `par` expression (thanks Jakob!)
- Language: Added safe `upcast` expression (thanks Jakob!)
- Language: Added support for top-level imports (thanks Magnus!)
- Language: Renamed `Impure` to `IO` (thanks Matt!)
- Language: Changed effect syntax (thanks Matt!)
  - old syntax: `& Impure`, new syntax: `\ IO`.
- Language: Changed record syntax (thanks Jakob!)
  - old syntax: `{x :: Int32`, new syntax: `{x = Int32}`.
- Compiler: Re-implemented Test Framework (thanks Magnus!)
- Compiler: Improved performance of unification (thanks Matt!)
- Compiler: Incrementalize Instances phase (thanks Magnus!)
- Library: Added `MonadZip` type class (thanks Stephen!)
- Library: Various minor improvements to the Standard Library (thanks Nina, Jakob, Stephen!)
- VSCode: Improved integration with Flix REPL (thanks Magnus!)

Version 0.30.0:

- Language: `query` now returns a `List` (thanks Jonathan!)
- Library: Added iterators for sets and maps (thanks Jakob and Stephen!)
- Library: Added `iterator` and `enumerator` to Prelude (thanks Jakob!)
- Library: Added `nth` functions (thanks Nina!)
- Library: Miscellaneous improvements to the library  (thanks Nina, Jacob!)
- LSP: Added support for inlay hints (thanks Nicola!)
- LSP: Added "Open in REPL" as a Code Lens (thanks Magnus!)
- LSP: Improved auto-completion (thanks Magnus and Paul!)
- REPL: Allow build commands to be executed from the REPL (thanks Paul!)
- REPL: Added :doc command (thanks Paul!)
- REPL: Added support for multi-line input with \\ (thanks Paul!)

Version 0.29.0:

- Language: Added syntactic sugar for singleton enums (thanks Matt!)
- Language: Added a specific error message for when a function is under applied (thanks Jonathan!)
- Language: Added a specific error message about missing dependencies (thanks Matt!)
- Compiler: Improved type and effect formatting (thanks Matt!)
- Library: Refactored integer namespaces to use subject-last (thanks Nina!)
- Library: Refactored all sort functions to internally use arrays (thanks Nina!)
- Library: Refactored more functions to be effect polymorphic (thanks Nina!)
- LSP: Added support for auto-completion of types (thanks Paul!)
- LSP: Improved auto-completion in pipelines (thanks Paul!)
- LSP: Improved auto-completion prioritization (thanks Paul!)
- Tooling: Resurrected the REPL with more improvements to come (thanks Magnus!)

Version 0.28.0:
- Language: Allow any unit function to be used as main (thanks Matt, Jonathan, and Magnus!)
- Language: Mark root namespace declarations as implicitly public (thanks Matt!)
- Compiler: Added support for minimization of Booleans via tabling (thanks Jaco and Magnus!)
- Compiler: Added a CrashHandler for easier error reporting (thanks Magnus!)
- LSP: Added auto-complete for local variables and formal parameters (thanks Magnus!)
- LSP: Added code hints for `@Deprecated` and `@Experimental` (thanks Nicola!)
- LSP: Added support for finding references to type variables (thanks Matt!)
- LSP: Added support for renaming type variables (thanks Matt!)
- Lib: Added `Environment.getArgs` for retrieving the program arguments (thanks Matt!)
- Lib: Added `Group` and `CommutativeGroup` (thanks Jakob!)
- Lib: Added `foldRightWithCont` to collections and `Foldable` (thanks Nina!) 
- Lib: Added examples for working with files (thanks Nina!)

Version 0.27.0:
- Incrementalized large parts of the front-end, including: 
  - `Parser`, `Weeder`, `Kinder`, and `Typer` (thanks Magnus and Matt!).
- Added `CommutativeSemiGroup` and `CommutativeMonoid` (Thanks Jakob!)
- Added `++` and `|+|` operators for `SemiGroup` and `CommutativeSemiGroup` (thanks Magnus!)
- Added `Foldable.joinWith` (thanks Jakob!)
- Added `sumWith` and `productWith` functions (thanks Jakob!)
- Added Identity `Functor` / `Monad` (thanks Stephen!)
- Added `fix` keyword for using lattice values in relations (thanks Jonathan!) 
- Added various extensions to `Foldable` and `Reducible` (thanks Jakob!)
- Added support for annotations on classes and enums (thanks Nicola!)
- Improved Find References support in LSP (thanks Nicola!)
- Refactored various `replace` methods to use record arguments (thanks Nina!)
- Updated online documentation and home page (thanks Nina!)
- Fixed an issue with entailment checking (thanks Matt!)

Version 0.26.0:

- Released a completely revamped version of https://api.flix.dev/ (thanks Magnus!)
- Improved type error messages (thanks Magnus!)
- Rewrote the `Console` API (thanks Nina!)
- Added monadic folds to `Foldable` (thanks Stephen!)
- Added additional monadic operators (thanks Stephen!)
- Added `Traversable` type class (thanks Stephen!)
- Added support for local defs (let-rec) (thanks Jonathan!)
- Applied subject-last principle more uniformly (thanks Dylan!)
- Added additional code hints for effects (thanks Magnus!)
- Added experimental support for incremental compilation (thanks Magnus!)
    - Incrementalized Parser, Weeder, and Typer (thanks Magnus!)
- Added `@Parallel` and `@Lazy` annotations (thanks Magnus!)
- Various improvements to the standard library (thanks Nina!)

Version 0.25.0:

- Added support for semantic tokens (thanks Magnus, Jacob, and Matthew!)
- Added support for inclusion of external JARs (thanks Matthew!)
- Added Monad type class (thanks Felix!)
- Added examples with derive (thanks Matthew!)
- Added experimental support for reifyEff (thanks Magnus!)
- Updates to String functions (thanks Nina!)
- Updates to SemiGroup (thanks Nina!)
- Updates to Koans (thanks Jonathan!)
- Tweaks to code generation (thanks Jonathan!)
- Reduced memory usage (thanks Magnus!)
- Several bug fixes (thanks everyone!)

Version 0.24.0:
- Added support for schema rows (thanks Matthew!)
- Added support for labelled records (thanks Matthew!)
- Added `rem` and `mod` keywords (thanks Matthew!)
- Added `MutDeque` (thanks Jakob!)
- Added extra Datalog examples (thanks Jonathan!)
- Improvements to monomorphization (thanks Magnus!)
- Updated String signatures (thanks Nina!)
- Fixed a performance bug in the parser (thanks Magnus!)

Version 0.23.0:
- Added support for go to implementation (thanks Nicola!)
- Added code hint for complex effects (thanks Magnus!)
- Added support for `install` package (thanks Matthew!)

Version 0.22.0:
- Added support for Workspace Symbols (thanks Nicola!)
- Added support for Code Hints (thanks Magnus!)

Version 0.21.1:
- Fixed a NPE.

Version 0.21.0:
- Added support for derivations for opaque types (thanks Matthew!)
- Added safety checks for wildcards in Datalog constraints (thanks Jonathan!)
- Added SymbolProvider for enums (thanks Nicola!)
- Improved code generation for Lazy (thanks Jonathan!)
- Improved inference of kinds (thanks Matthew!)
- Updated record syntax (thanks Matthew!)
- Removed () syntax for the Unit type (thanks Matthew!)

Version 0.20.0:
- Added support for derivation of Hash (thanks Matthew!)
- Added support for derivation of Boxable (thanks Matthew!)
- Added support for reification of type-level Booleans.
- Fixed a bug related to try-catch (thanks Jonathan!) 

Version 0.19.0:
- Added support for derivation of Eq (thanks Matthew!)
- Added support for derivation of Order (thanks Matthew!)
- Added support for derivation of ToString (thanks Matthew!)
- LSP: Use Markdown for hovering (thanks Jacob Kragh!)
- LSP: Improved autocomplete for with clauses.
- Added more instances of PartialOrder, JoinLattice, and MeetLattice (thanks Jonathan Starup!)
- Removed dependency on flix-runtime.jar (thanks Jonathan Starup!)

Version 0.18.0:
- Improved auto-complete.
- Minor bug fixes.

Version 0.17.0:
- Added proper kind checking (thanks Matthew!)
- Added support for string escapes (thanks Matthew!)
- Added environment API (thanks Ramiro!)
- Fixed numerous bugs.

Version 0.16.1:
- Minor improvements to LSP.

Version 0.16.0:
- Added experimental support for auto-complete (thanks Manoj!)
- Added sortBy and sortWith to most collections (thanks Felix!)

Version 0.15.0:
- Added experimental support for type classes (thanks Matthew!)
- Added new Datalog backend (thanks Benjamin!)
- Added `query` and `project` expressions.
- Numerous bug fixes.

Version 0.14.6:
- Fix a bug in the parser.

Version 0.14.5:
- Critical bug fixes.

Version 0.14.4:
- Minor bug fixes.

Version 0.14.3:
- Minor improvements to LSP.

Version 0.14.2:
- Minor improvements to LSP.

Version 0.14.1:
- Minor fixes to LSP.

Version 0.14.0:
- Added support for Visual Studio Code via LSP.
- Allow `use` directly in namespaces.
- Added `default` expression.
- Added `MutList` to standard library.
- Improved type inference w.r.t. kinds.
- Various bug fixes.

Version 0.13.0:
- Significantly improved compilation time.
- Significantly improved the execution time of some Flix programs.
- Added @Time and @Space annotations.
- Added keywords for the logical operators: not, and, or.
- Reintroduce multi-threaded compilation.
- Various minor improvements to the standard library.
- Various minor bug fixes to the compiler.
- Removed binary literals from the language.

Version 0.12.0:

- Sets and Maps are now backed by Red-Black Trees. 
- Added mutable sets and maps (backed by immutable sets and maps for now).
- Added NonEmptyList (Nel) and Validation.
- Added spaceship operator.
- Added binary, octal, and hex literals.
- Added support for underscores in number literals.
- Added use construct to import defs/types/tags from other namespaces.
- Added a check for poorly named type variables.
- Reworked relation and lattice types. They now work more like type aliases.
- Improved the performance of the compiler.
- Extended the standard library.

Version 0.11.0:

- Added light-weight polymorphic effects.
- Defined division by zero to yield zero.
- Added check for unconditional recursion.
- Reworked array length from `length[x]` to `x.length`.
- Removed switch expression.

Version 0.10.0:

- Reworked and improved Java interoperability.
- Added support for folding on Datalog constraints.

Version 0.9.1:

- Fixed several bugs. 

Version 0.9.0:

- Added support for opaque types.
- Added support for type aliases.
- Added support for pattern matching on arrays.
- Added experimental support for monadic let (let*).
- Fixed a few bugs.

Version 0.8.1:

- Fixed several bugs. 

Version 0.8:

- Added Array, Channel, Random, and Path to the standard library.
- Rewrote --doc to output JSON.
- Accurately report negative cycles in the presence of stratified negation.

Version 0.7:

- Changed the syntax of constraint sets.
- Changed syntax for sets and maps.
- Improvements to tree shaking.
- Check for dead and unreachable code.
- Improved stratification of first-class constraints based on types.
- Multiple bug fixes.
- Upgrade to Scala 2.13
- Added support for Gradle builds.

Version 0.6:
- Added preliminary support for a simple build system.

Version 0.5:
- Added support for unbuffered channels.
- Added support for extensible records.

Version 0.4:
- Added support for first-class Datalog constraints.
- Added support for processes and channels.
- Many other improvements.

Version 0.3:
- Added completely new backend with full tail call support (thanks Ramin Zarifi)
- Added support for arrays and vectors.
- Added pub keyword.
- Added expression holes.
- Added full support for curried definitions.
- Vastly improved repl. (--interactive).

Version 0.2:
- Added stratified negation (thanks Jason Mittertreiner).
- Pattern matching now compiled to labels and jumps (~1.5x speedup).
- Typechecking is now performed in parallel.
- Parsing is now performed in parallel.
- Added operations on character and strings.
- Pattern match exhaustiveness enforced.
- Added interop from Flix to Java.
- Added simple intra-procedural optimiser.
- Constructors (tags) may now be used as functions.
- Added references (ref and deref).
- Upgrade to Scala 2.12.
- Many other small improvements.

Version 0.1:
- Added standard library.
- Added support for pattern matching in rules.
- Added if guards.
- Improved error messages.
- Improved the performance of the solver.
- Fixed numerous bugs.

2016-10-25:
- Added interpreter tutorial.
- Added polymorphic types.
- Added unification-based type system.
- Added quick checker.
- Added tail recursion.

2016-07-14:
- Added delta debugging (see --delta and --tutorial delta-debugging).
- Added parallel rule evaluation. (Use --threads to control the number of concurrent threads).
- Added tutorial. (see --tutorial introduction).
- Added --timeout parameter.
- Improved command line parsing. Added a --help flag to display available options.

2016-06-08: 
- Initial release.
