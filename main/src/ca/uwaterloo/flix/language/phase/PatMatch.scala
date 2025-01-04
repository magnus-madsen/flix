/*
 * Copyright 2017 Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, ParYieldFragment, Pattern, Root}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.NonExhaustiveMatchError
import ca.uwaterloo.flix.language.fmt.FormatConstant
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.language.errors.{NonExhaustiveMatchError, PatMatchError, UselessPatternError}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import scala.collection.mutable

/**
  * The Pattern Exhaustiveness phase checks pattern matches for exhaustiveness
  *
  * A pattern match is exhaustive if there exists a type correct expression which is not matched by any of the patterns
  * in the pattern match
  *
  * Implemented as described in MARANGET, L. (2007). Warnings for pattern matching. Journal of Functional Programming, 17(3), 387-421. doi:10.1017/S0956796807006223
  *
  * The algorithm works by recursively "peeling off layers" of the pattern match. It check if the outermost type
  * constructors are exhaustive, then if so, recursively calls itself on the arguments. If it finds a pattern which
  * is not exhaustive during a recursive case, it uses it to built up a pattern that is not matched by the full
  * pattern match and returns the result.
  *
  */
object PatMatch {

  /**
    * An ADT to make matching Type Constructors easier. We need to
    * support both user created constructors and the implicit built in
    * types. This allows us to handle True, False, Tuples etc in the same
    * way we would a user defined enum.
    */
  private sealed trait TyCon

  private object TyCon {
    case class Cst(cst: Constant) extends TyCon

    case object Wild extends TyCon

    case class Tuple(args: List[TyCon]) extends TyCon

    case object Array extends TyCon

    case object Vector extends TyCon

    case class Enum(sym: Symbol.CaseSym, args: List[TyCon]) extends TyCon

    case class Record(labels: List[(Name.Label, TyCon)], tail: TyCon) extends TyCon

    case object RecordEmpty extends TyCon
  }

  /**
    * A small ADT to track if we've found a non exhaustive pattern
    *
    * Essentially a reverse Optional: we only include a parameter if there is an error
    */
  private sealed trait Exhaustiveness

  private case object Exhaustive extends Exhaustiveness

  private case class NonExhaustive(pat: List[TyCon]) extends Exhaustiveness

  private case class Useless(pat: Pattern) extends Exhaustiveness

  /**
    * Returns an error message if a pattern match is not exhaustive
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): (Root, List[PatMatchError]) =
    flix.phaseNew("PatMatch") {
      implicit val r: TypedAst.Root = root

      val classDefExprs = root.traits.values.flatMap(_.sigs).flatMap(_.exp)
      val classDefErrs = ParOps.parMap(classDefExprs)(visitExp).flatten

      val defErrs = ParOps.parMap(root.defs.values)(defn => visitExp(defn.exp)).flatten
      val instanceDefErrs = ParOps.parMap(TypedAstOps.instanceDefsOf(root))(defn => visitExp(defn.exp)).flatten
      // Only need to check sigs with implementations
      val sigsErrs = root.sigs.values.flatMap(_.exp).flatMap(visitExp)

      val errors = classDefErrs ++ defErrs ++ instanceDefErrs ++ sigsErrs

      (root, errors.toList)
    }

  /**
    * Check that all patterns in an expression are exhaustive
    *
    * @param tast The expression to check
    * @param root The AST root
    */
  private def visitExp(tast: TypedAst.Expr)(implicit root: TypedAst.Root, flix: Flix): List[PatMatchError] = {
    tast match {
      case Expr.Var(_, _, _) => Nil
      case Expr.Hole(_, _, _, _) => Nil
      case Expr.HoleWithExp(exp, _, _, _) => visitExp(exp)
      case Expr.OpenAs(_, exp, _, _) => visitExp(exp)
      case Expr.Use(_, _, exp, _) => visitExp(exp)
      case Expr.Cst(_, _, _) => Nil
      case Expr.Lambda(_, body, _, _) => visitExp(body)
      case Expr.ApplyClo(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.ApplyDef(_, exps, _, _, _, _) => exps.flatMap(visitExp)
      case Expr.ApplyLocalDef(_, exps, _, _, _, _) => exps.flatMap(visitExp)
      case Expr.ApplySig(_, exps, _, _, _, _) => exps.flatMap(visitExp)
      case Expr.Unary(_, exp, _, _, _) => visitExp(exp)
      case Expr.Binary(_, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.Let(_, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.LocalDef(_, _, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.Region(_, _) => Nil
      case Expr.Scope(_, _, exp, _, _, _) => visitExp(exp)
      case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => List(exp1, exp2, exp3).flatMap(visitExp)
      case Expr.Stm(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.Discard(exp, _, _) => visitExp(exp)

      case Expr.Match(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val guards = rules.flatMap(_.guard)
        val expsErrs = (exp :: ruleExps ::: guards).flatMap(visitExp)
        val rulesErrs = checkRules(exp, rules, root)
        expsErrs ::: rulesErrs

      case Expr.TypeMatch(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val expsErrs = (exp :: ruleExps).flatMap(visitExp)
        expsErrs

      case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp)

      case Expr.Tag(_, exps, _, _, _) => exps.flatMap(visitExp)
      case Expr.RestrictableTag(_, exps, _, _, _) => exps.flatMap(visitExp)
      case Expr.Tuple(elms, _, _, _) => elms.flatMap(visitExp)
      case Expr.RecordEmpty(_, _) => Nil
      case Expr.RecordSelect(base, _, _, _, _) => visitExp(base)
      case Expr.RecordExtend(_, value, rest, _, _, _) => List(value, rest).flatMap(visitExp)
      case Expr.RecordRestrict(_, rest, _, _, _) => visitExp(rest)
      case Expr.ArrayLit(exps, exp, _, _, _) => (exp :: exps).flatMap(visitExp)
      case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) => List(exp1, exp2, exp3).flatMap(visitExp)
      case Expr.ArrayLoad(base, index, _, _, _) => List(base, index).flatMap(visitExp)
      case Expr.ArrayStore(base, index, elm, _, _) => List(base, index, elm).flatMap(visitExp)
      case Expr.ArrayLength(base, _, _) => visitExp(base)
      case Expr.StructNew(_, fields, region, t, _, _) =>
        val fieldExps = fields.map { case (_, v) => v }
        (region :: fieldExps).flatMap(visitExp)
      case Expr.StructGet(e, _, _, _, _) => visitExp(e)
      case Expr.StructPut(e1, _, e2, _, _, _) => List(e1, e2).flatMap(visitExp)
      case Expr.VectorLit(exps, _, _, _) => exps.flatMap(visitExp)
      case Expr.VectorLoad(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.VectorLength(exp, _) => visitExp(exp)
      case Expr.Ascribe(exp, _, _, _) => visitExp(exp)
      case Expr.InstanceOf(exp, _, _) => visitExp(exp)
      case Expr.CheckedCast(_, exp, _, _, _) => visitExp(exp)
      case Expr.UncheckedCast(exp, _, _, _, _, _) => visitExp(exp)
      case Expr.Unsafe(exp, _, _, _, _) => visitExp(exp)
      case Expr.Without(exp, _, _, _, _) => visitExp(exp)

      case Expr.TryCatch(exp, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp)

      case TypedAst.Expr.Throw(exp, _, _, _) => visitExp(exp)

      case Expr.TryWith(exp, _, rules, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        (exp :: ruleExps).flatMap(visitExp)

      case Expr.Do(_, exps, _, _, _) => exps.flatMap(visitExp)
      case Expr.InvokeConstructor(_, args, _, _, _) => args.flatMap(visitExp)
      case Expr.InvokeMethod(_, exp, args, _, _, _) => (exp :: args).flatMap(visitExp)
      case Expr.InvokeStaticMethod(_, args, _, _, _) => args.flatMap(visitExp)
      case Expr.GetField(_, exp, _, _, _) => visitExp(exp)
      case Expr.PutField(_, exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.GetStaticField(_, _, _, _) => Nil
      case Expr.PutStaticField(_, exp, _, _, _) => visitExp(exp)
      case Expr.NewObject(_, _, _, _, methods, _) => methods.flatMap(m => visitExp(m.exp))
      case Expr.NewChannel(exp, _, _, _) => visitExp(exp)
      case Expr.GetChannel(exp, _, _, _) => visitExp(exp)
      case Expr.PutChannel(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)

      case Expr.SelectChannel(rules, default, _, _, _) =>
        val ruleExps = rules.map(_.exp)
        val chans = rules.map(_.chan)
        (ruleExps ::: chans ::: default.toList).flatMap(visitExp)

      case Expr.Spawn(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)

      case Expr.ParYield(frags, exp, _, _, loc) =>
        val fragsExps = frags.map(_.exp)
        val expsErrs = (exp :: fragsExps).flatMap(visitExp)
        val fragsErrs = checkFrags(frags, root, loc)
        expsErrs ::: fragsErrs

      case Expr.Lazy(exp, _, _) => visitExp(exp)
      case Expr.Force(exp, _, _, _) => visitExp(exp)
      case Expr.FixpointConstraintSet(cs, _, _) => cs.flatMap(visitConstraint)
      case Expr.FixpointLambda(_, exp, _, _, _) => visitExp(exp)
      case Expr.FixpointMerge(exp1, exp2, _, _, _) => List(exp1, exp2).flatMap(visitExp)
      case Expr.FixpointSolve(exp, _, _, _) => visitExp(exp)
      case Expr.FixpointFilter(_, exp, _, _, _) => visitExp(exp)
      case Expr.FixpointInject(exp, _, _, _, _) => visitExp(exp)
      case Expr.FixpointProject(_, exp, _, _, _) => visitExp(exp)
      case Expr.Error(_, _, _) => Nil
    }
  }

  /**
    * Performs exhaustive checking on the given constraint `c`.
    */
  private def visitConstraint(c0: TypedAst.Constraint)(implicit root: TypedAst.Root, flix: Flix): List[PatMatchError] = c0 match {
    case TypedAst.Constraint(_, head0, body0, _) =>
      val headErrs = visitHeadPred(head0)
      val bodyErrs = body0.flatMap(visitBodyPred)
      headErrs ::: bodyErrs
  }

  private def visitHeadPred(h0: TypedAst.Predicate.Head)(implicit root: TypedAst.Root, flix: Flix): List[PatMatchError] = h0 match {
    case TypedAst.Predicate.Head.Atom(_, _, terms, _, _) => terms.flatMap(visitExp)
  }

  private def visitBodyPred(b0: TypedAst.Predicate.Body)(implicit root: TypedAst.Root, flix: Flix): List[PatMatchError] = b0 match {
    case TypedAst.Predicate.Body.Atom(_, _, _, _, _, _, _) => Nil
    case TypedAst.Predicate.Body.Guard(exp, _) => visitExp(exp)
    case TypedAst.Predicate.Body.Functional(_, exp, _) => visitExp(exp)
  }

  /**
    * Check that the given ParYield fragments are exhaustive for their corresponding expressions.
    *
    * @param frags The fragments to check
    * @param root  The root of the tree
    * @param loc   the source location of the ParYield expression.
    * @return
    */
  private def checkFrags(frags: List[ParYieldFragment], root: TypedAst.Root, loc: SourceLocation): List[PatMatchError] = {
    // Call findNonMatchingPat for each pattern individually
    frags.flatMap(f => findNonMatchingPat(List(List(f.pat)), 1, root) match {
      case Exhaustive => Nil
      case NonExhaustive(ctors) => NonExhaustiveMatchError(prettyPrintCtor(ctors.head), loc) :: Nil
      case Useless(pat) => UselessPatternError(pat.loc) :: Nil
    })
  }

  /**
    * Check that the given rules are exhaustive for the given expression
    *
    * @param root  The root of the tree
    * @param exp   The expression to check
    * @param rules The rules to check
    * @return
    */
  private def checkRules(exp: TypedAst.Expr, rules: List[TypedAst.MatchRule], root: TypedAst.Root): List[PatMatchError] = {
    // Filter down to the unguarded rules.
    // Guarded rules cannot contribute to exhaustiveness (the guard could be e.g. `false`)
    val unguardedRules = rules.filter(r => r.guard.isEmpty)
    findNonMatchingPat(unguardedRules.map(r => List(r.pat)), 1, root) match {
      case Exhaustive => Nil
      case NonExhaustive(ctors) => List(NonExhaustiveMatchError(prettyPrintCtor(ctors.head), exp.loc))
      case Useless(pat) => UselessPatternError(pat.loc) :: Nil
    }
  }

  /**
    * Given a list of patterns, computes a pattern vector of size n such
    * that p doesn't match any rule in rules
    *
    * @param rules The rules to check for exhaustion
    * @param n     The size of resulting pattern vector
    * @param root  The AST root of the expression
    * @return If no such pattern exists, returns Exhaustive, else returns NonExhaustive(a matching pattern)
    */
  private def findNonMatchingPat(rules: List[List[Pattern]], n: Int, root: TypedAst.Root): Exhaustiveness = {
    if (n == 0) {
      if (rules.isEmpty) {
        return NonExhaustive(List.empty[TyCon])
      }
      return Exhaustive
    }

    val sigma = rootCtors(rules)
    val missing = missingFromSig(sigma, root)
    val useless = uselessInSig(rules)

    val complete = missing.isEmpty && sigma.nonEmpty

    (complete, useless.isEmpty) match {
      // Complete signature
      case (true, true) =>
        /* If the constructors are complete, then we check that the arguments to the constructors and the remaining
         * patterns are complete
         *
         * e.g. If we have
         * enum Option[a] {
         *    case Some(a),
         *    case None
         *  }
         *
         * case Some True =>
         * case Some False =>
         * case None =>
         *
         * {Some, None} is a complete signature, but the "Some" case is exhaustive only if the {True, False} case is
         *
         * exhaustive. So we create a "Specialized" matrix for "Some" with {True, False} as rows and check that.
         */
        val checkAll: List[Exhaustiveness] = sigma.map(c => {
          val res: Exhaustiveness = findNonMatchingPat(specialize(c, rules, root), countCtorArgs(c) + n - 1, root)
          res match {
            case Exhaustive => Exhaustive
            case NonExhaustive(ctors) => NonExhaustive(rebuildPattern(c, ctors))
            case Useless(pat) => Useless(pat)
          }
        })
        mergeAllExhaustive(checkAll)

      // Missing pattern
      case (false, _) =>
        /* If the constructors are not complete, then we will fall to the wild/default case. In that case, we need to
         * check for non matching patterns in the wild/default matrix.
         */
        findNonMatchingPat(defaultMatrix(rules), n - 1, root) match {
          case Exhaustive => Exhaustive
          case NonExhaustive(ctors) => sigma match {
            // If sigma is not empty, pick one of the missing constructors and return it
            case _ :: _ => NonExhaustive(rebuildPattern(missing.head, ctors))
            // Else, prepend a wildcard
            case Nil => NonExhaustive(rebuildPattern(TyCon.Wild, ctors))
          }
          case Useless(pat) => Useless(pat)
        }

      // Useless pattern
      case (true, false) =>
        // Just point out the first useless pattern
        Useless(useless.head)

    }
  }

  /**
    * Specialize a matrix of patterns for the Constructor ctor For a constructor of C(r1,...ra) and a matrix of width
    * n, we return a matrix of width n+a-1
    *
    * e.g. If we have
    * enum Option {
    * case Some a,
    * case Nothing
    * }
    *
    * And the pattern matrix
    *
    * case Some True =>
    * case Some False =>
    * case Nothing =>
    *
    * Specializing for Some gives use the matrix
    * True =>
    * False =>
    *
    * Where' we've taken a matrix had made of the arguments to the Some rows. Specializing for Nothing would give
    * an empty matrix as it has no arguments.
    *
    * @param ctor  The constructor to specialize for
    * @param rules The rules matrix to specialize
    * @param root  The ast root
    * @return The specialized matrix of rules
    */
  private def specialize(ctor: TyCon, rules: List[List[Pattern]], root: TypedAst.Root): List[List[TypedAst.Pattern]] = {
    // First figure out how many arguments are needed by the ctor
    val numArgs = countCtorArgs(ctor)

    def specializeRow(pat: List[TypedAst.Pattern]): Option[List[Pattern]] = pat.head match {
      // A wild constructor is the same as the constructor
      // with all its arguments as wild
      case a: TypedAst.Pattern.Wild =>
        Some(List.fill(numArgs)(a) ::: pat.tail)
      case a: TypedAst.Pattern.Var =>
        Some(List.fill(numArgs)(a) ::: pat.tail)

      // If it's a pattern with the constructor that we are
      // specializing for, we break it up into it's arguments
      // If it's not our constructor, we ignore it
      case TypedAst.Pattern.Tag(CaseSymUse(sym, _), pats, _, _) =>
        ctor match {
          case TyCon.Enum(ctorSym, _) =>
            if (sym == ctorSym) {
              Some(pats ::: pat.tail)
            } else {
              None
            }
          case _ => None
        }
      case TypedAst.Pattern.Tuple(pats, _, _) =>
        if (ctor.isInstanceOf[TyCon.Tuple]) {
          Some(pats ::: pat.tail)
        } else {
          None
        }
      case TypedAst.Pattern.Record(pats, tail, _, _) => ctor match {
        case TyCon.Record(_, _) =>
          val ps = pats.map(_.pat)
          val p = tail match {
            case TypedAst.Pattern.RecordEmpty(_, _) => Nil
            case _ => List(tail)
          }
          Some(ps ::: p ::: pat.tail)
        case _ => None
      }
      // Also handle the non tag constructors
      case p =>
        if (patToCtor(p) == ctor) {
          Some(pat.tail)
        } else None
    }

    rules.flatMap(specializeRow)
  }

  /**
    * Extract a default matrix of width n-1
    *
    * defaultMatrix is called by findNonMatchingPat when the given constructor patterns aren't exhaustive.
    * We then want to ensure that the wild card patterns cover the remaining cases.
    *
    * DefaultMatrix constructs a matrix which only has the patterns that begin with a wildcard, then removes the
    * first wildcard since we know all the patterns start with it.
    *
    * e.g.
    * case Red =>
    * case Blue =>
    * case _ =>
    *
    * The default matrix is
    * _ =>
    *
    */
  private def defaultMatrix(rules: List[List[TypedAst.Pattern]]): List[List[TypedAst.Pattern]] = {
    def defaultRow(pat: List[TypedAst.Pattern]): Option[List[TypedAst.Pattern]] = {
      pat.headOption.flatMap {
        // If it's a wild card, we take the rest of the pattern
        case _: TypedAst.Pattern.Wild => Some(pat.tail)
        case _: TypedAst.Pattern.Var => Some(pat.tail)

        // If it's a constructor, we don't include a row
        case _ => None
      }
    }

    rules.flatMap(defaultRow)
  }


  /**
    * Computes the set of constructors that appear at the root of the
    * patterns, skipping out on wilds
    *
    * e.g.
    *
    * Foo(1, 3) => ....
    * Bar(Foo(1,3), 'a', 3) => ....
    * Baz() => ....
    * _ => ...
    *
    * returns
    * Foo
    * Bar
    * Baz
    *
    */
  private def rootCtors(rules: List[List[TypedAst.Pattern]]): List[TyCon] = {
    rules.flatMap(rootCtor)
  }

  // MATT docs
  private def rootCtor(pat: List[TypedAst.Pattern]): Option[TyCon] = {
    pat.headOption.flatMap {
      case _: Pattern.Wild => None
      case _: Pattern.Var => None
      case p => Some(patToCtor(p))
    }
  }

  /**
    * Returns the list of type constructors of the same type as the given tycon.
    */
  private def getAllCtors(tycon: TyCon, root: Root): List[TyCon] = tycon match {
    // Structural types have just one constructor
    case a: TyCon.Tuple => List(a)
    case a: TyCon.Record => List(a)

    // For Enums, we have to figure out what base enum is, then look it up in the enum definitions to get the
    // other cases
    case TyCon.Enum(sym, _) => {
      root.enums(sym.enumSym).cases.map {
        case (otherSym, caze) => TyCon.Enum(otherSym, List.fill(caze.tpes.length)(TyCon.Wild))
      }
    }.toList

    // For Unit and Bool constants are enumerable
    case TyCon.Cst(Constant.Unit) => List(TyCon.Cst(Constant.Unit))
    case TyCon.Cst(Constant.Bool(_)) => List(TyCon.Cst(Constant.Bool(true)), TyCon.Cst(Constant.Bool(false)))

    /* For numeric types, we consider them as "infinite" types union
     * Int = ...| -1 | 0 | 1 | 2 | 3 | ...
     * The only way we get a match is through a wild. Technically, you could, for example, cover a Char by
     * having a case for [0 255], but we'll ignore that case for now
     */
    case _ => List(TyCon.Wild)
  }

  /**
    * True if ctors is a complete signature for exp. A complete signature is when all constructors of a type are
    * present. E.g. for
    *
    * enum Color {
    * case Red,
    * case Blue,
    * case Yellow
    * }
    *
    * {Red, Blue, Yellow} is a complete signature, but {Red, Blue} is not. Additionally, {Red, Blue, _} is also not
    * If the constructors are a complete signature, then they are exhaustive for the type, and we just have to
    * check that their arguments are also exhaustive
    *
    * Wildcards are exhaustive, but we need to do some additional checking in that case (@see defaultMatrix)
    *
    * @param ctors The ctors that we match with
    * @param root  Root of the expression tree
    * @return
    */
  private def missingFromSig(ctors: List[TyCon], root: TypedAst.Root): List[TyCon] = {

    val expCtors = ctors.flatMap(getAllCtors(_, root))
    /* We cover the needed constructors if there is a wild card in the
     * root constructor set, or if we match every constructor for the
     * expression
     */
    expCtors.filterNot {
      ctor => ctors.exists(y => sameCtor(ctor, y))
    }
  }

  /**
    * Returns the list of useless constructors.
    *
    * MATT more doc
    */
  private def uselessInSig(rules: List[List[TypedAst.Pattern]]): List[TypedAst.Pattern] = {
    val seen = mutable.Set.empty[TyCon]
    var wildSeen = false

    // MATT this isn't enough: we need to check whether the previous stuff is exhaustive
    // MATT it's probably best if we refactor to use types instead of tycon -> List(tycon) stuff

    // The useless constructors are the ones that are previously seen
    val roots = rules.map(rootCtor)
    roots.zip(rules).filter {
      // If we've seen a wild already, then everything is unreachable.
      case _ if wildSeen =>
        true

      // If it's a wildcard, we've seen "everything"
      case (None, _) =>
        wildSeen = true
        false

      // Otherwise it's a proper constructor. Just check if it's been seen.
      case (Some(tycon), _) =>
        val unseen = seen.add(tycon)
        !unseen

    }.map {
      case (_, rule) => rule.head // MATT dangerous head?
    }
  }

  /**
    * Gets the number of arguments for a constructor, takes into account the "fake constructors"
    *
    * @param ctor The constructor to get from
    * @return The number of arguments for the constructor
    */
  private def countCtorArgs(ctor: TyCon): Int = ctor match {
    case TyCon.Cst(_) => 0
    case TyCon.Wild => 0
    case TyCon.Tuple(args) => args.size
    case TyCon.Array => 0
    case TyCon.Vector => 0
    case TyCon.Enum(_, args) => args.length
    case TyCon.Record(labels, tail) => tail match {
      case TyCon.RecordEmpty => labels.length
      case _ => labels.length + 1
    }
    case TyCon.RecordEmpty => 0
  }

  /**
    * Pretty print a constructor
    *
    * @param ctor The TypeConstructor to print
    * @return A human readable string of the constructor
    */
  private def prettyPrintCtor(ctor: TyCon): String = ctor match {
    case TyCon.Cst(cst) => FormatConstant.formatConstant(cst)
    case TyCon.Wild => "_"
    case TyCon.Tuple(args) => args.map(prettyPrintCtor).mkString("(", ", ", ")")
    case TyCon.Array => "Array"
    case TyCon.Vector => "Vector"
    case TyCon.Enum(sym, args) => if (args.isEmpty) sym.name else sym.name + prettyPrintCtor(TyCon.Tuple(args))
    case TyCon.Record(labels, tail) =>
      val labelStr = labels.map {
        case (f, p) => s"$f = ${prettyPrintCtor(p)}"
      }.mkString(", ")
      val tailStr = tail match {
        case TyCon.RecordEmpty => ""
        case r => s" | ${prettyPrintCtor(r)}"
      }
      "{ " + labelStr + tailStr + " }"
    case TyCon.RecordEmpty => ""
  }


  /**
    * Checks if two TypeConstructors refers to the same constructor.
    *
    * @param c1 First constructor to compare
    * @param c2 Second constructor to compare
    * @return True if they are the same constructor
    */
  private def sameCtor(c1: TyCon, c2: TyCon): Boolean = (c1, c2) match {
    // Two enums are the same constructor if they have the same case symbol
    case (TyCon.Enum(s1, _), TyCon.Enum(s2, _)) => s1 == s2
    // Everything else is the same constructor if they are the same type
    case (a: TyCon.Tuple, b: TyCon.Tuple) => true
    case (a: TyCon.Record, b: TyCon.Record) => true
    case (a, b) => a == b;
  }

  /**
    * Convert a pattern to a TyCon
    *
    * @param pattern The pattern to convert
    * @return a TyCon representing the given pattern
    */
  private def patToCtor(pattern: TypedAst.Pattern): TyCon = pattern match {
    case Pattern.Wild(_, _) => TyCon.Wild
    case Pattern.Var(_, _, _) => TyCon.Wild
    case Pattern.Cst(cst, _, _) => TyCon.Cst(cst)
    case Pattern.Tag(CaseSymUse(sym, _), pats, _, _) => TyCon.Enum(sym, pats.map(patToCtor))
    case Pattern.Tuple(elms, _, _) => TyCon.Tuple(elms.map(patToCtor))
    case Pattern.Record(pats, pat, _, _) =>
      val patsVal = pats.map {
        case TypedAst.Pattern.Record.RecordLabelPattern(label, pat1, _, _) =>
          (label, patToCtor(pat1))
      }
      val pVal = patToCtor(pat)
      TyCon.Record(patsVal, pVal)
    case Pattern.RecordEmpty(_, _) => TyCon.RecordEmpty

    case Pattern.Error(_, _) => TyCon.Wild
  }

  /**
    * Adds a `TyCon` to a list of `TyCon`s, using up items in the list if it requires arguments
    *
    * e.g. rebuildPattern(Foo(a,b), [1,2,3]) => [Foo(1,2), 3]
    *
    * @param tc  The type constructor to add
    * @param lst The list to add to
    * @return The new list
    */
  private def rebuildPattern(tc: TyCon, lst: List[TyCon]): List[TyCon] = tc match {
    case TyCon.Tuple(args) => TyCon.Tuple(lst.take(args.size)) :: lst.drop(args.size)
    case TyCon.Enum(sym, args) =>
      val rebuiltArgs = lst.take(args.length)
      TyCon.Enum(sym, rebuiltArgs) :: lst.drop(args.length)
    case TyCon.Record(labels, _) =>
      val all = lst.take(labels.length + 1)
      val ls = labels.map {
        case (l, _) => l
      }.zip(all.take(labels.length))
      val t = all.takeRight(1).head
      TyCon.Record(ls, t) :: lst.drop(labels.length + 1)
    case TyCon.RecordEmpty => lst
    case a => a :: lst
  }

  /**
    * Flatten/join for Exhaust. Accumulates failures if they are there.
    *
    * @param x   The Exhaustiveness under consideration
    * @param acc The Exhaustiveness accumulated so far
    * @return The merged result
    */
  private def mergeExhaustive(x: Exhaustiveness, acc: Exhaustiveness): Exhaustiveness = {
    def index(x: Exhaustiveness): Int = x match {
      case Useless(_) => 0
      case Exhaustive => 1
      case NonExhaustive(_) => 2
    }

    if (index(x) >= index(acc)) {
      x
    } else {
      acc
    }
  }

  /**
    * Merges all the exhaustivenesses in the list. Accumulates failures if they are there.
    */
  private def mergeAllExhaustive(l: List[Exhaustiveness]): Exhaustiveness = {
    l.foldRight(Exhaustive: Exhaustiveness)(mergeExhaustive)
  }
}
