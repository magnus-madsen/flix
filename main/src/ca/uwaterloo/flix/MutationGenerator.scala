/*
 * Copyright 2024 Lukas Schröder, Samuel Skovbakke & Alexander Sommer
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

import ca.uwaterloo.flix.language.ast.Ast.{CaseSymUse, Constant}
import ca.uwaterloo.flix.language.ast.Type.{False, Int32, Str, True}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr.Cst
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, MutationType, Pattern}
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import dev.flix.runtime.Global

import java.math.BigInteger
import scala.util.Random


///
/// The Mutation Generator has a single purpose,
/// which is to generate mutations that MutationTester can evaluate
///
/// It works by moving through the root and definitions and creating every reasonable mutation.
/// This is returned as a list of mutated definitions.
///

object MutationGenerator {
  /**
    * Generates all possible mutants of the given module
    *
    * @param root             : The TypedAST
    * @param productionModule : The name of the module that is to be mutated
    * @return List[(Symbol.DefnSym, List[TypedAst.Def])]: the list of the symbols of the defs along with a
    *         list that contains a def for all mutations of that def
    */
  case class MutatedDef(df: TypedAst.Def, mutType: MutationType)
  def mutateRoot(root: TypedAst.Root, productionModule: String): List[(Symbol.DefnSym, List[MutatedDef])] = {
    val defs = root.defs
    val defSyms = root.modules.filter(pair => pair._1.toString.equals(productionModule)).values.toList.flatten
    mutateDefs( defs, defSyms).flatten
  }

  def insertDACAndMutInExp(mutated: TypedAst.Expr, original: TypedAst.Expr): TypedAst.Expr = {
    val loc = mutated.loc
    val method = classOf[Global].getMethods.find(m => m.getName.equals("decAndCheck")).get
    val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Int64, Type.IO, loc)
    val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
    Expr.Stm(mask, mutated, mutated.tpe, mutated.eff, mutated.loc)
  }

  def mutateSig(sig: Expr.Sig): List[TypedAst.Expr.Sig] = {
    val tpe = sig.tpe
    val sym = sig.sym
    val sub = sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Sub"), Name.Ident(sym.loc.sp1, "sub", sym.loc.sp2)))
    val mul = sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Mul"), Name.Ident(sym.loc.sp1, "mul", sym.loc.sp2)))
    val div = sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Div"), Name.Ident(sym.loc.sp1, "div", sym.loc.sp2)))
    val add = sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Add"), Name.Ident(sym.loc.sp1, "add", sym.loc.sp2)))
    val clazz = Symbol.mkTraitSym("Order")
    val le = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "less", sym.loc.sp2)))
    val leq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "lessEqual", sym.loc.sp2)))
    val gre = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greater", sym.loc.sp2)))
    val greq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greaterEqual", sym.loc.sp2)))
    val compare = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "compare", sym.loc.sp2)))

    (sym.toString(), tpe.arrowArgTypes) match {
      case ("Add.add", List(Str, Str)) => Nil
      case ("Add.add", _) =>
        sub :: div :: mul :: Nil
      case ("Sub.sub", _) =>
        add :: div :: mul :: Nil
      case ("Div.div", _) =>
        mul :: add :: sub :: Nil
      case ("Mul.mul", _) =>
        div :: add :: sub :: Nil
      case ("Eq.eq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Eq"), Name.Ident(sym.loc.sp1, "neq", sym.loc.sp2))) :: Nil
      case ("Eq.neq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkTraitSym("Eq"), Name.Ident(sym.loc.sp1, "eq", sym.loc.sp2))) :: Nil
      case ("Order.less", _) =>
        leq :: gre :: greq  :: Nil
      case ("Order.lessEqual", _) =>
        le :: gre :: greq :: Nil
      case ("Order.greaterEqual", _) =>
        leq :: le :: gre  :: Nil
      case ("Order.greater", _) =>
        leq :: le :: greq :: Nil
      case ("Order.compare", _) =>
        Nil
      case _ => Nil
    }
  }

  /**
    *
    * @param defs    : a map of the all defs
    * @param defSyms : the symbols of the defs that are to be mutated
    * @return List[(Symbol.DefnSym, List[TypedAst.Def])]: the list of the symbols of the defs along with a
    *         list that contains a def for all mutations of that def
    */
  private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
    defs.toList.map(d => (d._1, d._2) match {
      case (s, fun) =>
        if (defSyms.contains(s)) {
          val mutExps = mutateExpr(fun.exp)
          val mutDefs = mutExps.map(mexp => {
            val a = MutationTester.insertDecAndCheckInDef(fun.copy(exp = mexp))
            MutatedDef(a, mexp.mutationType)
          })
          Some(d._1 -> mutDefs)
        } else None
      case _ => None
    })
  }




  /**
    * Goes through an expression and its subtree and returns a list of all possible mutations and permutations
    *
    * @param e : the expression to be mutated
    * @return List[TypedAst.Expr]: all possible mutants from the given expression and its subtree
    *
    *         For instance for an IfThenElse expression we generate three lists of mutations. The first includes the two mutants
    *         where the condition has been changed to either to true or false. The second list includes all generated mutants of the
    *         subtree of the then-branch, the same is done for the else-branch respectively.
    *
    *         Moreover for MatchCase expression we generate a mutant for each possible permutation of the cases and mutants
    *         where there in each mutant has been removed a case (expect the las of course). Furthermore we generate a list of
    *         mutants for each case, which includes all mutants of the respective subtree.
    */
  private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr.Mutated] = e match {
    case original@Expr.Cst(cst, tpe, loc) =>
      mutateCst(cst).map(c => Expr.Mutated(Expr.Cst(c, tpe, loc),  MutationType.CstMut(c), original.tpe, original.eff, original.loc))
    case original@Expr.Var(sym, tpe,_) =>
      val maybeNil = mutateVarToNil(original)
      mutateVar(original).map{case (c, mutType)  => Expr.Mutated(c, mutType, c.tpe, c.eff, c.loc)} ::: maybeNil.toList
    case Expr.Def(_, _, _) => Nil
    case original@Expr.Sig(_, _, _) =>
      mutateSig(original).map(c => Expr.Mutated(c, MutationType.SigMut(c.sym), original.tpe, original.eff, c.loc))
    case Expr.Hole(_, _, _) => Nil
    case Expr.HoleWithExp(_, _, _, _) => Nil
    case Expr.OpenAs(_, _, _, _) => Nil
    case original@Expr.Use(_, _, exp, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Lambda(_, exp, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Apply(exp, exps, _, _, _) =>
      val mut = mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mutateExps = exps.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => Expr.Mutated(original.copy(exps = exps.updated(index, m)), m.mutationType, original.tpe, original.eff, m.loc))
      }
      mut ::: mutateExps

    case original@Expr.Unary(sop, exp, tpe, eff, loc) =>
      val mut1 = Expr.Mutated(Expr.Unary(sop, original, tpe, eff, loc), MutationType.CaseSwitch(0, 0), original.tpe, original.eff, loc)
      mut1 :: mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Binary(_, exp1, exp2, _, _, _) =>
      val mut2 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut3 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut2 ::: mut3
    case original@Expr.Let(_, _, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.LetRec(_, _, _, exp1, exp2, tpe, eff, loc) =>
      val method = classOf[Global].getMethods.find(m => m.getName.equals("decAndCheck")).get
      val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Int64, Type.IO, loc)
      val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
      val mut1 = mutateExpr(exp1).map(m => {
        // the reason for the following matches are to avoid adding decAndCheck to the FormalParams of the Lambda
        val mm: Option[Expr.Mutated] = m match {
          case mutated@Expr.Mutated(mutExp, _, _, _, _) =>
            val statement = mutExp match {
              case lambda@Expr.Lambda(_, exp, _, _) =>
                Some(lambda.copy(exp = Expr.Stm(mask, exp, exp.tpe, exp.eff, exp.loc)))
              case _ => None
            }
            Some(mutated.copy(mutExp = statement.get))
          case _ => None
        }
        Expr.Mutated(original.copy(exp1 = mm.get), mm.get.mutationType, tpe, eff, mm.get.loc)
      })
      val mut2 = mutateExpr(exp2).map(m => {
        val statement = Expr.Stm(mask, m, m.tpe, m.eff, m.loc)
        val copy = original.copy(exp2 = statement)
        Expr.Mutated(copy, m.mutationType, tpe, eff, m.loc)
      })
      mut1 ::: mut2
    case Expr.Region(_, _) => Nil
    case original@Expr.Scope(_, _, exp, tpe, eff, loc) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, tpe, eff, m.loc))
    case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val ifTrue = Expr.Mutated(original.copy(exp1 = Expr.Cst(Constant.Bool(true), True, exp1.loc)), MutationType.IfMut(true), original.tpe, original.eff, exp1.loc)
      val ifFalse = Expr.Mutated(original.copy(exp1 = Expr.Cst(Constant.Bool(false), False, exp1.loc)), MutationType.IfMut(false), original.tpe, original.eff, exp1.loc)
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, m.tpe, m.eff, m.loc))
      val mut3 = mutateExpr(exp3).map(m => Expr.Mutated(original.copy(exp3 = m), m.mutationType, m.tpe, m.eff, m.loc))
      ifTrue :: ifFalse :: mut2 ::: mut3
    case original@Expr.Stm(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case Expr.Discard(_, _, _) => Nil
    case original@Expr.Match(_, rules, _, _, _) =>
      val switches = legalSwitches(rules).map{case (s, (x, y)) => Expr.Mutated(original.copy(rules = s), MutationType.CaseSwitch(x, y), original.tpe, original.eff, original.loc)}
      val deletedCasesMutation = rules.indices
        .map(index =>
          (rules.filter(e => rules.indexOf(e) != index || rules.indexOf(e) == rules.length - 1), index))
            .toList.map{case (m, ind) => Expr.Mutated(original.copy(rules = m), MutationType.CaseDeletion(ind), original.tpe, original.eff, original.loc)}
      val mutateRules = rules.zipWithIndex.flatMap {
        case (rule, index) =>
          val mutations = mutateMatchrule(rule)
          mutations.map{case (m, mloc, mt) => Expr.Mutated(original.copy(rules = rules.updated(index, m)), mt, original.tpe, original.eff, mloc)}
      }

      switches ::: mutateRules ::: deletedCasesMutation.reverse.tail
    case Expr.TypeMatch(exp, rules, _, _, _) => Nil
    case Expr.RestrictableChoose(_, _, _, _, _, _) => Nil
    case Expr.Tag(_, exp, _, _, _) => Nil
    case Expr.RestrictableTag(_, _, _, _, _) => Nil
    case original@Expr.Tuple(elms, _, _, _) =>
      elms.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => Expr.Mutated(original.copy(elms = elms.updated(index, m)), m.mutationType, original.tpe, original.eff, m.loc))
      }
    case Expr.RecordEmpty(_, _) => Nil
    case original@Expr.RecordSelect(exp, label, tpe, _, _) =>
      def nameAndType(xs: List[TypeConstructor]): List[(Name.Label, TypeConstructor)] = xs match {
        case TypeConstructor.RecordRowExtend(label) :: tp :: tail => (label, tp) :: nameAndType(tail)
        case _ :: tail => nameAndType(tail)
        case _ => Nil
      }

      val types = nameAndType(exp.tpe.typeConstructors)
      val res = types.flatMap {
        case (name, tp) =>
          if (!name.equals(label) && tp.equals(tpe.typeConstructor.get))
            Some(Expr.Mutated(original.copy(label = name), MutationType.RecordSelectMut(name), original.tpe, original.eff, original.loc))
          else None
      }
      res
    case original@Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case Expr.RecordRestrict(_, exp, _, _, _) => Nil
    case original@Expr.ArrayLit(exps, exp, _, _, _) =>
      val mut = mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mutateExps = exps.zipWithIndex.flatMap {
        case (ex, index) =>
          val mutations = mutateExpr(ex)
          mutations.map(m => Expr.Mutated(original.copy(exps = exps.updated(index, m)), m.mutationType, original.tpe, original.eff, m.loc))
      }
      mut ::: mutateExps
    case original@Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut3 = mutateExpr(exp3).map(m => Expr.Mutated(original.copy(exp3 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2 ::: mut3
    case original@Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.ArrayLength(exp, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut3 = mutateExpr(exp3).map(m => Expr.Mutated(original.copy(exp3 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2 ::: mut3
    case original@Expr.VectorLit(exps, _, _, _) =>
      exps.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => Expr.Mutated(original.copy(exps = exps.updated(index, m)), m.mutationType, original.tpe, original.eff, m.loc))
      }
    case original@Expr.VectorLoad(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.VectorLength(exp, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Ref(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.Deref(exp, _, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Assign(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.Ascribe(exp, _, _, _) => Nil
    case original@Expr.InstanceOf(exp, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.CheckedCast(_, exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.UncheckedCast(exp, _, _, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.UncheckedMaskingCast(exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Without(exp, _, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.TryCatch(exp, _, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.TryWith(exp, _, _, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Do(_, exps, _, _, _) =>
      exps.zipWithIndex.flatMap {
        case (exp, index) =>
          val mutations = mutateExpr(exp)
          mutations.map(m => Expr.Mutated(original.copy(exps = exps.updated(index, m)), m.mutationType, original.tpe, original.eff, m.loc))
      }
    case original@Expr.InvokeConstructor(_, exps, _, _, _) => Nil
      //val mutateExps = exps.map(e => mutateExpr(e))
      //mutateExps.map(m => Expr.Mutated(original.copy(exps = m), original, original.tpe, original.eff, original.loc))
    case original@Expr.InvokeMethod(_, exp, exps, _, _, _) => Nil
      //val mut = mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
      //val mutateExps = exps.map(e => mutateExpr(e))
      //mutateExps.map(m => Expr.Mutated(original.copy(exps = m), original, original.tpe, original.eff, original.loc)) ::: mut
    case original@Expr.InvokeStaticMethod(_, exps, _, _, _) => Nil
      //val mutateExps = exps.map(e => mutateExpr(e))
      //mutateExps.map(m => Expr.Mutated(original.copy(exps = m), original, original.tpe, original.eff, original.loc))
    case original@Expr.GetField(_, exp, _, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.PutField(_, exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case Expr.GetStaticField(_, _, _, _) => Nil
    case original@Expr.PutStaticField(_, exp, _, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case Expr.NewObject(_, _, _, _, _, _) => Nil
    case original@Expr.NewChannel(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.GetChannel(exp, _, _, _) =>
      mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.PutChannel(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case Expr.SelectChannel(_, _, _, _, _) => Nil
    case original@Expr.Spawn(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.ParYield(_, exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Lazy(exp, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.Force(exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case Expr.FixpointConstraintSet(_, _, _) => Nil
    case original@Expr.FixpointLambda(_, exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      val mut1 = mutateExpr(exp1).map(m => Expr.Mutated(original.copy(exp1 = m), m.mutationType, original.tpe, original.eff, m.loc))
      val mut2 = mutateExpr(exp2).map(m => Expr.Mutated(original.copy(exp2 = m), m.mutationType, original.tpe, original.eff, m.loc))
      mut1 ::: mut2
    case original@Expr.FixpointSolve(exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.FixpointFilter(_, exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.FixpointInject(exp, _, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case original@Expr.FixpointProject(_, exp, _, _, _) => mutateExpr(exp).map(m => Expr.Mutated(original.copy(exp = m), m.mutationType, original.tpe, original.eff, m.loc))
    case Expr.Error(_, _, _) => Nil
    case _ => Nil
  }

  private def mutateMatchrule(mr: TypedAst.MatchRule): List[(TypedAst.MatchRule, SourceLocation, MutationType)] = {
    val mut1 = mutateExpr(mr.exp).map(m => (mr.copy(exp = m), m.loc, m.mutationType))
    val patterns = mutatePattern(mr.pat).map{case (m, mt) => (mr.copy(pat = m), m.loc, mt)}
    patterns ::: mut1
  }

  private def mutatePattern(pattern: TypedAst.Pattern): List[(TypedAst.Pattern, MutationType)] = {
    pattern match {
      case original@Pattern.Var(sym, tpe, loc) => Nil
        /*
        mutateVar(Expr.Var(sym, tpe, loc)).flatMap {
          case (e, _) => e match {
            case Cst(cst, tpe, loc) => Some((Pattern.Cst(cst, tpe, loc), MutationType.CstMut(cst)))
            case _ => None
          }
        }
        */
      case original@Pattern.Cst(cst, _, _) => mutateCst(cst).map(m => (original.copy(cst = m), MutationType.CstMut(m)))
      case original@Pattern.Tuple(elms, _, _) => Nil
        /*
        elms.zipWithIndex.flatMap {
          case (pat, index) =>
            val mutations = mutatePattern(pat)
            mutations.map{case (m, mt) => (original.copy(elms = elms.updated(index, m)), mt)}
          }
         */
      case original@Pattern.Tag(sym, pat, tpe, loc) => Nil
      case Pattern.Record(pats, pat, tpe, loc) => Nil
      case _ => Nil
    }
  }

  private def switchMutations(mrs: List[TypedAst.MatchRule]): List[List[TypedAst.MatchRule]] = {
    mrs match {
      case x::xs =>
        Nil
      case _ => Nil
    }
  }

  private def legalSwitches(matches: List[TypedAst.MatchRule]): List[(List[TypedAst.MatchRule], (Int, Int))] = {
    def comparePatterns(candPat: TypedAst.Pattern, otherPat: TypedAst.Pattern): Boolean = {
      candPat match {
        case Pattern.Wild(_, _) => otherPat match {
          case Pattern.Wild(_,_) => false
          case _ => true
        }
        case Pattern.Var(_, _, _) => otherPat match {
          case Pattern.Var(_, _, _) => false
          case _ => true
        }
        case Pattern.Cst(_, _, _) => otherPat match {
          case Pattern.Cst(_,_,_) => false
          case _ => true
        }
        case Pattern.Tag(cSym , pat, tpe, _) => otherPat match {
          case Pattern.Tag(oCSym, oPat, oTpe, _) =>
            (cSym.sym.name, oCSym.sym.name) match {
              case ("Cons", "Cons") => false
              case ("Cons", "Nil") => false
              case ("Nil", "Cons") => false
              case ("Nil", "Nil") => true
              case _ => !tpe.equals(oTpe)
            }
          case _ => true
        }
        case Pattern.Tuple(elms, _, _) => otherPat match {
          case Pattern.Tuple(oElms, _, _) => elms.zip(oElms).forall {case (cand, other) => comparePatterns(cand, other)}
          case _ => true
        }
        case Pattern.Record(pats, _, _, _) => otherPat match {
          case Pattern.Record(oPats, _, _, _) => pats.zip(oPats).forall {case (cand, other) => comparePatterns(cand.pat, other.pat)}
          case Pattern.RecordEmpty(_,_) => false
          case _ => true
        }
        case Pattern.RecordEmpty(_, _) => otherPat match {
          case Pattern.Record(_, _, _, _) => false
          case _ => true
        }
        case Pattern.Error(tpe, loc) => otherPat match {
          case _ => true
        }
      }
    }
    val swapIndex = matches.zipWithIndex.flatMap {case (x, xindex) =>
      matches.zipWithIndex.flatMap { case (y, yindex) =>
        if (xindex < yindex && comparePatterns(x.pat, y.pat) && x.guard.isEmpty)
          Some(xindex, yindex)
        else None
      }
    }
    swapIndex.map{case (x, y) =>
      (matches.updated(x, matches(y)).updated(y, matches(x)), (x, y))
    }

  }

  /**
    * Generates mutants where a variable is treated like the constant of the respective value
    * and is overwritten with sensible constants values of the respective type, including of by one values.
    *
    * @param varexp : The Var expression to be mutated
    * @return List of expressions.
    */
  private def mutateVar(varexp: Expr.Var): List[(Expr, MutationType)] = varexp match {
    case Expr.Var(_, tpe, loc) =>
      val one = tpe match {
        case Type.Cst(tc, _) => tc match {
          case TypeConstructor.Char => Nil // Char causes stillborn mutants to be created
            //Expr.Cst(Constant.Char(1), tpe, loc) :: Nil
          case TypeConstructor.Float32 =>
            Expr.Cst(Constant.Float32(1), tpe, loc) :: Nil
          case TypeConstructor.Float64 =>
            Expr.Cst(Constant.Float64(1), tpe, loc) :: Nil
          case TypeConstructor.BigDecimal =>
            Expr.Cst(Constant.BigDecimal(java.math.BigDecimal.valueOf(1)), tpe, loc) :: Nil
          case TypeConstructor.Int8 =>
            Expr.Cst(Constant.Int8(1), tpe, loc) :: Nil
          case TypeConstructor.Int16 =>
            Expr.Cst(Constant.Int16(1), tpe, loc) :: Nil
          case TypeConstructor.Int32 =>
            Expr.Cst(Constant.Int32(1), tpe, loc) :: Nil
          case TypeConstructor.Int64 =>
            Expr.Cst(Constant.Int64(1), tpe, loc) :: Nil
          case TypeConstructor.BigInt =>
            Expr.Cst(Constant.BigInt(java.math.BigInteger.valueOf(1)), tpe, loc) :: Nil
          case e => println(s"some unexpected type: $e"); Nil
        }
        case _ => Nil
      }
      if (one == Nil) return equivalentVarCheck(varexp)
      val newtpe = Type.mkPureUncurriedArrow(tpe :: tpe :: Nil, tpe, loc)
      val sub = Expr.Sig(Symbol.mkSigSym(Symbol.mkTraitSym("Sub"), Name.Ident(loc.sp1, "sub", loc.sp2)), newtpe, loc)
      val add = Expr.Sig(Symbol.mkSigSym(Symbol.mkTraitSym("Add"), Name.Ident(loc.sp1, "add", loc.sp2)), newtpe, loc)
      val appSub = Expr.Apply(sub, varexp :: one, tpe, Type.Pure, loc)
      val appAdd = Expr.Apply(add, varexp :: one, tpe, Type.Pure, loc)
      val ret = (appSub, MutationType.VarMut(sub, varexp.sym)) :: (appAdd, MutationType.VarMut(add, varexp.sym)) :: Nil


      val constants = equivalentVarCheck(varexp)
      ret ::: constants
    case _ => Nil
  }

  private def equivalentVarCheck(varexp: Expr.Var): List[(Expr, MutationType)]  = {
      val loc = varexp.loc
      val tpe = varexp.tpe
      mutateVarToConstantByType(tpe).map(c => {
        val method = classOf[Global].getMethods.find(m => m.getName.equals("throwEquivalentMutant")).get
        val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Null, Type.IO, loc)
        val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
        val condType = Type.mkPureUncurriedArrow(tpe :: tpe :: Nil, Type.Bool, loc)
        val condition = {
          val eq = Expr.Sig(Symbol.mkSigSym(Symbol.mkTraitSym("Eq"), Name.Ident(loc.sp1, "eq", loc.sp2)), condType, loc)
          Expr.Apply(eq, List(varexp, c) , Type.Bool, Type.Pure, loc)
        }
        val elseBranch = Expr.Cst(Constant.Unit, Type.Unit, loc)

        val ifThenElse = Expr.IfThenElse(condition, mask, elseBranch, Type.Unit, Type.IO, loc)

        val expr = Expr.Stm(ifThenElse, c.copy(loc = loc), c.tpe, c.eff, loc)

        (expr, MutationType.CstMut(c.cst))
      }
    )

  }

  private def mutateVarToNil(varExp: Expr.Var): Option[Expr.Mutated] = {
    varExp.tpe.typeConstructors match {
      case x::y::_ => x match {
        case TypeConstructor.Enum(sym, kind) =>
          //println("in mutateVarToNil", x, y)
          val loc = varExp.loc
          if (sym.toString.equals("List")) {
            val ident = Name.Ident(loc.sp1, "Nil", loc.sp2)
            val cSym = Ast.CaseSymUse(Symbol.mkCaseSym(Symbol.mkEnumSym("List"), ident), loc)
            val tag = Expr.Tag(cSym, Expr.Cst(Constant.Unit, Type.Unit, loc), varExp.tpe, Type.Pure, loc)
            //println(tag)
            Some(Expr.Mutated(tag, MutationType.ListMut(), tag.tpe, tag.eff, tag.loc))
          } else None
        case _ => None
      }
      case _ => None
    }
  }

  private def mutateVarToConstantByType(constType: Type): List[Expr.Cst] = constType match {
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Bool =>
        val mut = Constant.Bool(true)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Bool, loc))
      case TypeConstructor.Char =>
        val mut = Constant.Char(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Char, loc))
      case TypeConstructor.Float32 =>
        val mut = Constant.Float32(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Float32, loc))
      case TypeConstructor.Float64 =>
        val mut = Constant.Float64(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Float64, loc))
      case TypeConstructor.BigDecimal =>
        val mut = Constant.BigDecimal(java.math.BigDecimal.ZERO)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.BigDecimal, loc))
      case TypeConstructor.Int8 =>
        val mut = Constant.Int8(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int8, loc))
      case TypeConstructor.Int16 =>
        val mut = Constant.Int16(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int16, loc))
      case TypeConstructor.Int32 =>
        val mut = Constant.Int32(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int32, loc))
      case TypeConstructor.Int64 =>
        val mut = Constant.Int64(0)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Int64, loc))
      case TypeConstructor.BigInt =>
        val mut = Constant.BigInt(BigInteger.ZERO)
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.BigInt, loc))
      case TypeConstructor.Str =>
        val mut = Constant.Str("")
        val mutations = mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Str, loc))
      case TypeConstructor.Regex =>
        val mut = Constant.Regex(java.util.regex.Pattern.compile("b"))
        val mutations = mut :: mutateCst(mut)
        mutations.map(m => Expr.Cst(m, Type.Regex, loc))
      case e => println(s"$e not implemented in mutateConstantByType"); Nil
    }
    case _ => Nil
  }

  /**
    * Matches the constant to the specific type and returns a list of mutants.
    *
    * @param cst : The constant that is to be mutated
    * @return A list of constants to replace cst with
    */
  private def mutateCst(cst: Ast.Constant): List[Ast.Constant] = {
    cst match {
      case Constant.Unit => Nil
      case Constant.Null => Nil
      case Constant.Bool(lit) => Constant.Bool(!lit) :: Nil
      case Constant.Char(lit) => Constant.Char((lit ^ Char.MaxValue).toChar) :: Nil
      case Constant.Float32(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Float32(i))
      case Constant.Float64(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Float64(i))
      case Constant.BigDecimal(lit) =>
        val litCand = Set(java.math.BigDecimal.ZERO,
          java.math.BigDecimal.valueOf(-1),
          java.math.BigDecimal.ONE,
          lit.subtract(java.math.BigDecimal.ONE),
          java.math.BigDecimal.valueOf(2),
          java.math.BigDecimal.valueOf(4),
          java.math.BigDecimal.valueOf(6),
          java.math.BigDecimal.valueOf(8),
          java.math.BigDecimal.valueOf(16),
          lit.add(java.math.BigDecimal.ONE),
          lit.subtract(java.math.BigDecimal.ONE)).filter(i => lit != i)
        litCand.toList.map(i => Constant.BigDecimal(i))
      case Constant.Int8(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, lit + 1, lit - 1).map(i => i.toByte).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int8(i))
      case Constant.Int16(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, lit + 1, lit - 1).map(i => i.toShort).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int16(i))
      case Constant.Int32(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int32(i))
      case Constant.Int64(lit) =>
        val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, lit + 1, lit - 1).filter(i => lit != i)
        litCand.toList.map(i => Constant.Int64(i))
      case Constant.BigInt(lit) =>
        val litCand = Set(java.math.BigInteger.ZERO,
          java.math.BigInteger.valueOf(-1),
          java.math.BigInteger.ONE,
          lit.subtract(java.math.BigInteger.ONE),
          java.math.BigInteger.TWO,
          java.math.BigInteger.valueOf(4),
          java.math.BigInteger.valueOf(6),
          java.math.BigInteger.valueOf(8),
          java.math.BigInteger.valueOf(16),
          lit.add(java.math.BigInteger.ONE),
          lit.subtract(java.math.BigInteger.ONE)).filter(i => lit != i)
        litCand.toList.map(i => Constant.BigInt(i))
      case Constant.Str(lit) => List("", "Mutated").filter(i => !lit.equals(i)).map(i => Constant.Str(i))
      case Constant.Regex(_) => Constant.Regex(java.util.regex.Pattern.compile(".*")) :: Nil
    }
  }

}
