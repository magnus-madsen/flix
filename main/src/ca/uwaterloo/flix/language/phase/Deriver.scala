/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.Ast.TypeSource.Ascribed
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Name, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugKindedAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugValidation
import ca.uwaterloo.flix.language.errors.DerivationError
import ca.uwaterloo.flix.language.phase.util.PredefinedTraits
import ca.uwaterloo.flix.util.Validation.mapN
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * Constructs instances derived from enums.
  *
  * No errors are thrown in this phase: constructed instances must be well-formed.
  * Errors with overlapping instances or unfulfilled type constraints must be caught in later phases.
  */
object Deriver {

  // We don't use regions, so we are safe to use the global scope everywhere in this phase.
  private implicit val S: Scope = Scope.Top

  val EqSym = new Symbol.TraitSym(Nil, "Eq", SourceLocation.Unknown)
  val OrderSym = new Symbol.TraitSym(Nil, "Order", SourceLocation.Unknown)
  val ToStringSym = new Symbol.TraitSym(Nil, "ToString", SourceLocation.Unknown)
  val HashSym = new Symbol.TraitSym(Nil, "Hash", SourceLocation.Unknown)
  val SendableSym = new Symbol.TraitSym(Nil, "Sendable", SourceLocation.Unknown)
  val CoerceSym = new Symbol.TraitSym(Nil, "Coerce", SourceLocation.Unknown)

  val DerivableSyms = List(EqSym, OrderSym, ToStringSym, HashSym, SendableSym, CoerceSym)

  val Param1Name = "a"
  val Param2Name = "b"
  val GetMethodName = "get"
  val PutMethodName = "put"

  def run(root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, DerivationError] = flix.phase("Deriver") {
    val derivedInstances = ParOps.parTraverse(root.enums.values)(getDerivedInstances(_, root))
    val structFieldInstances = root.structs.values.map(getInstancesOfStruct(_, root))
    val fieldNames = root.structs.values.flatMap(struct => struct.fields).map(field => Name.Label(field.sym.name, field.sym.loc)).toSet

    mapN(derivedInstances) {
      instances =>
        val newInstances = (instances ++ structFieldInstances).flatten.foldLeft(root.instances) {
          case (acc, inst) =>
            val accInsts = acc.getOrElse(inst.trt.sym, Nil)
            acc + (inst.trt.sym -> (inst :: accInsts))
        }
        root.copy(instances = newInstances)
    }
  }(DebugValidation())

  /**
    * Build the name of the get trait for this struct field
    */
  def structFieldGetTraitName(fieldName: String): String = "DotGet_" + fieldName

  /**
    * Build the name of the put trait for this struct field
    */
  def structFieldPutTraitName(fieldName: String): String = "DotPut_" + fieldName

  /**
    * Builds the instances for this struct
    */
  private def getInstancesOfStruct(struct0: KindedAst.Struct, root: KindedAst.Root)(implicit flix: Flix): List[KindedAst.Instance] = {
    val getInstances = struct0.fields.map(fieldGetInstance(struct0, _, root))
    val putInstances = struct0.fields.filter(_.mod.isMutable).map(fieldPutInstance(struct0, _, root))
    getInstances ++ putInstances
  }

  /**
    * Builds the instances for the `get` operation of this struct field
    */
  private def fieldGetInstance(struct0: KindedAst.Struct, field: KindedAst.StructField, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = {
    val loc = field.loc
    val (fields, structType, eff) = Kinder.instantiateStruct(struct0.sym, root.structs)
    val fieldType = fields(field.sym)
    val traitSym = Symbol.mkTraitSym(structFieldGetTraitName(field.sym.name))
    val assocTypeSym = Symbol.mkAssocTypeSym(traitSym, Name.Ident("FieldType", loc))
    val assocEffSym = Symbol.mkAssocTypeSym(traitSym, Name.Ident("Aef", loc))
    val assocTypeSymUse = Ast.AssocTypeSymUse(assocTypeSym, loc)
    val assocEffSymUse = Ast.AssocTypeSymUse(assocEffSym, loc)
    val assocTpe = fieldAssocTypeDef(assocTypeSymUse, structType, fieldType, loc)
    val assocEff = fieldAssocTypeDef(assocEffSymUse, structType, eff, loc)
    val param1Symbol = Symbol.freshVarSym(Param1Name, BoundBy.FormalParam, loc)
    val getSpec = fieldGetSpec(structType, eff, fieldType, Nil, param1Symbol, loc)
    val getExpr = fieldGetImpl(param1Symbol, field, loc)
    val getDef = KindedAst.Def(fieldDefnSymbol(structFieldGetTraitName(field.sym.name), GetMethodName), getSpec, getExpr)
    KindedAst.Instance(
      doc = Ast.Doc(Nil, loc),
      ann = Ast.Annotations.Empty,
      mod = Ast.Modifiers.Empty,
      trt = Ast.TraitSymUse(traitSym, loc),
      tpe = structType,
      tconstrs = List(),
      assocs = List(assocTpe, assocEff),
      defs = List(getDef),
      ns = Name.RootNS,
      loc = loc)
  }

  /**
    * Builds the instance for this `put` operation of this struct field
    */
  private def fieldPutInstance(struct0: KindedAst.Struct, field: KindedAst.StructField, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Instance = {
    val loc = field.loc
    val (fields, structType, eff) = Kinder.instantiateStruct(struct0.sym, root.structs)
    val fieldType = fields(field.sym)
    val traitSym = Symbol.mkTraitSym(structFieldPutTraitName(field.sym.name))
    val assocTypeSym = Symbol.mkAssocTypeSym(traitSym, Name.Ident("FieldType", loc))
    val assocEffSym = Symbol.mkAssocTypeSym(traitSym, Name.Ident("Aef", loc))
    val assocTypeSymUse = Ast.AssocTypeSymUse(assocTypeSym, loc)
    val assocEffSymUse = Ast.AssocTypeSymUse(assocEffSym, loc)
    val assocTpe = fieldAssocTypeDef(assocTypeSymUse, structType, fieldType, loc)
    val assocEff = fieldAssocTypeDef(assocEffSymUse, structType, eff, loc)
    val param1Symbol = Symbol.freshVarSym(Param1Name, BoundBy.FormalParam, loc)
    val param2Symbol = Symbol.freshVarSym(Param2Name, BoundBy.FormalParam, loc)
    val putSpec = fieldPutSpec(structType, eff, fieldType, Nil, param1Symbol, param2Symbol, loc)
    val putExpr = fieldPutImpl(param1Symbol, param2Symbol, field, loc)
    val putDef = KindedAst.Def(fieldDefnSymbol(structFieldPutTraitName(field.sym.name), PutMethodName), putSpec, putExpr)
    KindedAst.Instance(
      doc = Ast.Doc(Nil, loc),
      ann = Ast.Annotations.Empty,
      mod = Ast.Modifiers.Empty,
      trt = Ast.TraitSymUse(traitSym, loc),
      tpe = structType,
      tconstrs = List(),
      assocs = List(assocTpe, assocEff),
      defs = List(putDef),
      ns = Name.RootNS,
      loc = loc)
  }

  /**
    * Builds the definition for the associated type
    */
  private def fieldAssocTypeDef(sym: Ast.AssocTypeSymUse, arg: Type, tpe: Type, loc: SourceLocation): KindedAst.AssocTypeDef =
    KindedAst.AssocTypeDef(
      doc = Ast.Doc(Nil, loc),
      mod = Ast.Modifiers.Empty,
      sym = sym,
      arg = arg,
      tpe = tpe,
      loc = loc
    )

  /**
    * Builds the definition symbol for the struct instance method
    */
  def fieldDefnSymbol(traitName: String, methodName: String)(implicit flix: Flix): Symbol.DefnSym =
    Symbol.mkDefnSym(traitName + "." + methodName, Some(flix.genSym.freshId()))

  /**
    * Builds the body for the `put` operation
    */
  private def fieldPutImpl(param1Symbol: Symbol.VarSym, param2Symbol: Symbol.VarSym, field: KindedAst.StructField, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr.StructPut =
    KindedAst.Expr.StructPut(
      exp1 = KindedAst.Expr.Var(param1Symbol, loc),
      sym = field.sym,
      exp2 = KindedAst.Expr.Var(param2Symbol, loc),
      tvar = Type.freshVar(Kind.Star, loc),
      evar = Type.freshVar(Kind.Eff, loc),
      loc = loc
    )

  /**
    * Builds the body for the `get` operation
    */
  private def fieldGetImpl(param1Symbol: Symbol.VarSym, field: KindedAst.StructField, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr.StructGet =
    KindedAst.Expr.StructGet(
      exp = KindedAst.Expr.Var(param1Symbol, loc),
      sym = field.sym,
      tvar = Type.freshVar(Kind.Star, loc),
      evar = Type.freshVar(Kind.Eff, loc),
      loc = loc
    )

  /**
    * Builds the spec for this struct field's `get` operation
    */
  private def fieldGetSpec(structType: Type, structEff: Type, fieldType: Type, tparams: List[KindedAst.TypeParam], param1Symbol: Symbol.VarSym, loc: SourceLocation)(implicit flix: Flix): KindedAst.Spec =
    KindedAst.Spec(
      doc = Ast.Doc(Nil, loc),
      ann = Ast.Annotations.Empty,
      mod = Ast.Modifiers.Empty,
      tparams = tparams,
      fparams = List(KindedAst.FormalParam(param1Symbol, Ast.Modifiers.Empty, structType, Ast.TypeSource.Ascribed, loc)),
      sc = Scheme(
        tparams.map(_.sym),
        Nil,
        Nil,
        Type.mkUncurriedArrowWithEffect(List(structType), structEff, fieldType, loc)
      ),
      tpe = fieldType,
      eff = structEff,
      tconstrs = List(),
      econstrs = List(),
      loc = loc
    )

  /**
    * Builds the spec for this struct field's `put` operation
    */
  private def fieldPutSpec(structType: Type, structEff: Type, fieldType: Type, tparams: List[KindedAst.TypeParam], param1Symbol: Symbol.VarSym, param2Symbol: Symbol.VarSym, loc: SourceLocation)(implicit flix: Flix): KindedAst.Spec =
    KindedAst.Spec(
      doc = Ast.Doc(Nil, loc),
      ann = Ast.Annotations.Empty,
      mod = Ast.Modifiers.Empty,
      tparams = tparams,
      fparams = List(
        KindedAst.FormalParam(param1Symbol, Ast.Modifiers.Empty, structType, Ast.TypeSource.Ascribed, loc),
        KindedAst.FormalParam(param2Symbol, Ast.Modifiers.Empty, fieldType, Ast.TypeSource.Ascribed, loc)),
      sc = Scheme(
        tparams.map(_.sym),
        Nil,
        Nil,
        Type.mkUncurriedArrowWithEffect(List(structType, fieldType), structEff, Type.Unit, loc)
      ),
      tpe = Type.Unit,
      eff = structEff,
      tconstrs = List(),
      econstrs = List(),
      loc = loc
    )

  /**
    * Builds the instances derived from this enum.
    */
  private def getDerivedInstances(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): Validation[List[KindedAst.Instance], DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, enumSym, _, derives, cases, _, _) =>

      val instanceVals = Validation.traverse(derives.traits) {
        case Ast.Derivation(traitSym, loc) if cases.isEmpty => Validation.toSoftFailure(None, DerivationError.IllegalDerivationForEmptyEnum(enumSym, traitSym, loc))
        case Ast.Derivation(sym, loc) if sym == EqSym => mapN(mkEqInstance(enum0, loc, root))(Some(_))
        case Ast.Derivation(sym, loc) if sym == OrderSym => mapN(mkOrderInstance(enum0, loc, root))(Some(_))
        case Ast.Derivation(sym, loc) if sym == ToStringSym => mapN(mkToStringInstance(enum0, loc, root))(Some(_))
        case Ast.Derivation(sym, loc) if sym == HashSym => mapN(mkHashInstance(enum0, loc, root))(Some(_))
        case Ast.Derivation(sym, loc) if sym == SendableSym => mapN(mkSendableInstance(enum0, loc, root))(Some(_))
        case Ast.Derivation(sym, loc) if sym == CoerceSym => mkCoerceInstance(enum0, loc, root)
        case Ast.Derivation(sym, loc) => Validation.toSoftFailure(None, DerivationError.IllegalDerivation(sym, DerivableSyms, loc))
      }
      mapN(instanceVals)(_.flatten)
  }

  /**
    * Creates an Eq instance for the given enum.
    *
    * {{{
    * enum E[a] with Eq {
    *   case C1
    *   case C2(a)
    *   case C3(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Eq[E[a]] with Eq[a] {
    *   pub def eq(x: E[a], y: E[a]): Bool = (x, y) match {
    *     case (C0, C0) => true
    *     case (C1(x0), C1(y0)) => x0 == y0
    *     case (C2(x0, x1), C2(y0, y1)) => x0 == y0 and x1 == y1
    *     case _ => false
    *   }
    * }
    * }}}
    */
  private def mkEqInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val eqTraitSym = PredefinedTraits.lookupTraitSym("Eq", root)
      val eqDefSym = Symbol.mkDefnSym("Eq.eq", Some(flix.genSym.freshId()))

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkEqImpl(enum0, param1, param2, loc, root)
      val spec = mkEqSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(eqDefSym, spec, exp)

      val tconstrs = getTraitConstraintsForTypeParams(tparams, eqTraitSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        trt = Ast.TraitSymUse(eqTraitSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the eq implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val mainMatchRules = getCasesInStableOrder(cases).map(mkEqMatchRule(_, loc, root))

      // create a default rule
      // `case _ => false`
      val defaultRule = KindedAst.MatchRule(KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc), None, KindedAst.Expr.Cst(Ast.Constant.Bool(false), loc))

      // group the match rules in an expression
      KindedAst.Expr.Match(
        KindedAst.Expr.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        (mainMatchRules ++ List(defaultRule)),
        loc
      )
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkEqSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val eqTraitSym = PredefinedTraits.lookupTraitSym("Eq", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(
          KindedAst.FormalParam(param1, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc),
          KindedAst.FormalParam(param2, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        ),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TraitConstraint(Ast.TraitConstraint.Head(eqTraitSym, loc), tpe, loc)),
          Nil,
          Type.mkPureUncurriedArrow(List(tpe, tpe), Type.mkBool(loc), loc)
        ),
        tpe = Type.mkBool(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TraitConstraint(Ast.TraitConstraint.Head(eqTraitSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates an Eq match rule for the given enum case.
    */
  private def mkEqMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val eqSym = PredefinedTraits.lookupSigSym("Eq", "eq", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat1, varSyms1) = mkPattern(sym, tpe, "x", loc)
      val (pat2, varSyms2) = mkPattern(sym, tpe, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      // call eq on each variable pair
      // `x0 == y0`, `x1 == y1`
      val eqs = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(eqSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      // put it all together
      // `x0 == y0 and x1 == y1`
      val exp = eqs match {
        // Case 1: no arguments: return true
        case Nil => KindedAst.Expr.Cst(Ast.Constant.Bool(true), loc)
        // Case 2: at least one argument: join everything with `and`
        case head :: tail => tail.foldLeft(head: KindedAst.Expr) {
          case (acc, eq) => KindedAst.Expr.Binary(SemanticOp.BoolOp.And, acc, eq, Type.freshVar(Kind.Star, loc), loc)
        }
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates an Order instance for the given enum.
    *
    * {{{
    * enum E[a] with Order {
    *   case C1
    *   case C2(a)
    *   case C3(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Order[E[a]] with Order[a] {
    *   pub def compare(x: E[a], y: E[a]): Comparison = {
    *     let indexOf = e -> match e {
    *       case C0(_) -> 0
    *       case C1(_) -> 1
    *       case C2(_) -> 2
    *     };
    *     match (x, y) {
    *       case (C0, C0) => Comparison.EqualTo
    *       case (C1(x0), C1(y0)) => Order.compare(x0, y0)
    *       case (C2(x0, x1), C2(y0, y1)) => Order.compare(x0, y0) `Order.thenCompare` lazy Order.compare(x1, y1)
    *       case _ => Order.compare(indexOf(x), indexOf(y))
    *     }
    *   }
    * }
    * }}}
    */
  private def mkOrderInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val orderTraitSym = PredefinedTraits.lookupTraitSym("Order", root)
      val compareDefSym = Symbol.mkDefnSym("Order.compare", Some(flix.genSym.freshId()))

      val param1 = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val param2 = Symbol.freshVarSym("y", BoundBy.FormalParam, loc)
      val exp = mkCompareImpl(enum0, param1, param2, loc, root)
      val spec = mkCompareSpec(enum0, param1, param2, loc, root)

      val defn = KindedAst.Def(compareDefSym, spec, exp)

      val tconstrs = getTraitConstraintsForTypeParams(tparams, orderTraitSym, loc)
      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        trt = Ast.TraitSymUse(orderTraitSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the compare implementation for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareImpl(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      val compareSigSym = PredefinedTraits.lookupSigSym("Order", "compare", root)

      val lambdaVarSym = Symbol.freshVarSym("indexOf", BoundBy.Let, loc)

      // Create the lambda mapping tags to indices
      val lambdaParamVarSym = Symbol.freshVarSym("e", BoundBy.FormalParam, loc)
      val indexMatchRules = getCasesInStableOrder(cases).zipWithIndex.map { case (caze, index) => mkCompareIndexMatchRule(caze, index, loc) }
      val indexMatchExp = KindedAst.Expr.Match(mkVarExpr(lambdaParamVarSym, loc), indexMatchRules, loc)
      val lambda = KindedAst.Expr.Lambda(
        KindedAst.FormalParam(lambdaParamVarSym, Ast.Modifiers.Empty, lambdaParamVarSym.tvar, Ast.TypeSource.Ascribed, loc),
        indexMatchExp,
        loc
      )

      // Create the main match expression
      val matchRules = getCasesInStableOrder(cases).map(mkComparePairMatchRule(_, loc, root))

      // Create the default rule:
      // `case _ => compare(indexOf(x), indexOf(y))`
      val defaultMatchRule = KindedAst.MatchRule(
        KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc),
        None,
        KindedAst.Expr.Apply(
          KindedAst.Expr.Sig(compareSigSym, Type.freshVar(Kind.Star, loc), loc),
          List(
            KindedAst.Expr.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param1, loc)),
              Type.freshVar(Kind.Star, loc),
              Type.freshVar(Kind.Eff, loc),
              loc
            ),
            KindedAst.Expr.Apply(
              mkVarExpr(lambdaVarSym, loc),
              List(mkVarExpr(param2, loc)),
              Type.freshVar(Kind.Star, loc),
              Type.freshVar(Kind.Eff, loc),
              loc),
          ),
          Type.freshVar(Kind.Star, loc),
          Type.freshVar(Kind.Eff, loc),
          loc
        )
      )

      // Wrap the cases in a match expression
      val matchExp = KindedAst.Expr.Match(
        KindedAst.Expr.Tuple(List(mkVarExpr(param1, loc), mkVarExpr(param2, loc)), loc),
        matchRules.toList :+ defaultMatchRule,
        loc
      )

      // Put the expressions together in a let
      KindedAst.Expr.Let(lambdaVarSym, Ast.Modifiers.Empty, lambda, matchExp, loc)
  }

  /**
    * Creates the eq spec for the given enum, where `param1` and `param2` are the parameters to the function.
    */
  private def mkCompareSpec(enum0: KindedAst.Enum, param1: Symbol.VarSym, param2: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val orderTraitSym = PredefinedTraits.lookupTraitSym("Order", root)
      val comparisonEnumSym = PredefinedTraits.lookupEnumSym("Comparison", root)

      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(
          KindedAst.FormalParam(param1, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc),
          KindedAst.FormalParam(param2, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)
        ),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TraitConstraint(Ast.TraitConstraint.Head(orderTraitSym, loc), tpe, loc)),
          Nil,
          Type.mkPureUncurriedArrow(List(tpe, tpe), Type.mkEnum(comparisonEnumSym, Kind.Star, loc), loc)
        ),
        tpe = Type.mkEnum(comparisonEnumSym, Kind.Star, loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TraitConstraint(Ast.TraitConstraint.Head(orderTraitSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates an indexing match rule, mapping the given case to the given index, e.g.
    * `case C2(_) => 2`
    */
  private def mkCompareIndexMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation)(implicit Flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, _, _, _) =>
      val pat = KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Wild(Type.freshVar(Kind.Star, loc), loc), Type.freshVar(Kind.Star, loc), loc)
      val exp = KindedAst.Expr.Cst(Ast.Constant.Int32(index), loc)
      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a comparison match rule, comparing the elements of two tags of the same type.
    * {{{ case (C2(x0, x1), C2(y0, y1)) => compare(x0, y0) thenCompare lazy(x1, y1) }}}
    */
  private def mkComparePairMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val equalToSym = PredefinedTraits.lookupCaseSym("Comparison", "EqualTo", root)
      val compareSigSym = PredefinedTraits.lookupSigSym("Order", "compare", root)
      val thenCompareDefSym = PredefinedTraits.lookupDefSym(List("Order"), "thenCompare", root)

      // Match on the tuple
      // `case (C2(x0, x1), C2(y0, y1))
      val (pat1, varSyms1) = mkPattern(sym, tpe, "x", loc)
      val (pat2, varSyms2) = mkPattern(sym, tpe, "y", loc)
      val pat = KindedAst.Pattern.Tuple(List(pat1, pat2), loc)

      // Call compare on each variable pair
      // `compare(x0, y0)`, `compare(x1, y1)`
      val compares = varSyms1.zip(varSyms2).map {
        case (varSym1, varSym2) =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(compareSigSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              mkVarExpr(varSym1, loc),
              mkVarExpr(varSym2, loc)
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      /**
        * Joins the two expressions via `Compare.thenCompare`, making the second expression lazy.
        * (Cannot be inlined due to issues with Scala's type inference.
        */
      def thenCompare(exp1: KindedAst.Expr, exp2: KindedAst.Expr): KindedAst.Expr = {
        KindedAst.Expr.Apply(
          KindedAst.Expr.Def(thenCompareDefSym, Type.freshVar(Kind.Star, loc), loc),
          List(
            exp1,
            KindedAst.Expr.Lazy(exp2, loc)
          ),
          Type.freshVar(Kind.Star, loc),
          Type.freshVar(Kind.Eff, loc),
          loc
        )
      }

      // Put it all together
      // compare(x0, y0) `thenCompare` lazy compare(x1, y1)
      val exp = compares match {
        // Case 1: no variables to compare; just return true
        case Nil => KindedAst.Expr.Tag(Ast.CaseSymUse(equalToSym, loc), KindedAst.Expr.Cst(Ast.Constant.Unit, loc), Type.freshVar(Kind.Star, loc), loc)
        // Case 2: multiple comparisons to be done; wrap them in Order.thenCompare
        case cmps => cmps.reduceRight(thenCompare)
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a ToString instance for the given enum.
    *
    * {{{
    * enum E[a] with ToString {
    *   case C0
    *   case C1(a)
    *   case C2(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance ToString[E[a]] with ToString[a] {
    *   pub def toString(x: E[a]): String = match x {
    *     case C0 => "C0"
    *     case C1(x0) => "C1" + "(" + ToString.toString(x0) + ")"
    *     case C2(x0, x1) => "C2" + "(" + ToString.toString(x0) + ", " + ToString.toString(x1) + ")"
    *   }
    * }
    * }}}
    */
  private def mkToStringInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val toStringTraitSym = PredefinedTraits.lookupTraitSym("ToString", root)
      val toStringDefSym = Symbol.mkDefnSym("ToString.toString", Some(flix.genSym.freshId()))

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkToStringImpl(enum0, param, loc, root)
      val spec = mkToStringSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(toStringDefSym, spec, exp)

      val tconstrs = getTraitConstraintsForTypeParams(tparams, toStringTraitSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        trt = Ast.TraitSymUse(toStringTraitSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        assocs = Nil,
        defs = List(defn),
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the toString implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val matchRules = getCasesInStableOrder(cases).map(mkToStringMatchRule(_, loc, root))

      // group the match rules in an expression
      KindedAst.Expr.Match(
        mkVarExpr(param, loc),
        matchRules.toList,
        loc
      )
  }

  /**
    * Creates the toString spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkToStringSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val toStringTraitSym = PredefinedTraits.lookupTraitSym("ToString", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TraitConstraint(Ast.TraitConstraint.Head(toStringTraitSym, loc), tpe, loc)),
          Nil,
          Type.mkPureArrow(tpe, Type.mkString(loc), loc)
        ),
        tpe = Type.mkString(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TraitConstraint(Ast.TraitConstraint.Head(toStringTraitSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates a ToString match rule for the given enum case.
    */
  private def mkToStringMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val toStringSym = PredefinedTraits.lookupSigSym("ToString", "toString", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sym, tpe, "x", loc)

      // "C2"
      val tagPart = KindedAst.Expr.Cst(Ast.Constant.Str(sym.name), loc)

      // call toString on each variable,
      // `toString(x0)`, `toString(x1)`
      val toStrings = varSyms.map {
        varSym =>
          KindedAst.Expr.Apply(
            KindedAst.Expr.Sig(toStringSym, Type.freshVar(Kind.Star, loc), loc),
            List(mkVarExpr(varSym, loc)),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      // put commas between the arguments
      // `toString(x0)`, `", "`, `toString(x1)`
      val sep = mkStrExpr(", ", loc)
      val valuePart = intersperse(toStrings, sep)

      // put it all together
      // `"C2" + "(" + toString(x0) + ", " + toString(x1) + ")"`
      val exp = valuePart match {
        // Case 1: no arguments: just show the tag
        case Nil => tagPart
        // Case 2: at least one argument: concatenate the tag with the values wrapped in parens
        case exps => concatAll(tagPart :: mkStrExpr("(", loc) :: (exps :+ mkStrExpr(")", loc)), loc)
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates a Hash instance for the given enum.
    *
    * {{{
    * enum E[a] with Hash {
    *   case C0
    *   case C1(a)
    *   case C2(a, a)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Hash[E[a]] with Hash[a] {
    *   pub def hash(x: E[a]): Int = match x {
    *     case C0 => 1
    *     case C1(x0) => 2 `combine` hash(x0)
    *     case C2(x0, x1) => 3 `combine` hash(x0) `combine` hash(x1)
    *   }
    * }
    * }}}
    */
  private def mkHashInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val hashTraitSym = PredefinedTraits.lookupTraitSym("Hash", root)
      val hashDefSym = Symbol.mkDefnSym("Hash.hash", Some(flix.genSym.freshId()))

      val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
      val exp = mkHashImpl(enum0, param, loc, root)
      val spec = mkHashSpec(enum0, param, loc, root)

      val defn = KindedAst.Def(hashDefSym, spec, exp)

      val tconstrs = getTraitConstraintsForTypeParams(tparams, hashTraitSym, loc)
      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        trt = Ast.TraitSymUse(hashTraitSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        defs = List(defn),
        assocs = Nil,
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates the hash implementation for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      // create a match rule for each case
      val matchRules = getCasesInStableOrder(cases).zipWithIndex.map {
        case (caze, index) => mkHashMatchRule(caze, index, loc, root)
      }

      // group the match rules in an expression
      KindedAst.Expr.Match(
        mkVarExpr(param, loc),
        matchRules,
        loc
      )
  }

  /**
    * Creates the hash spec for the given enum, where `param` is the parameter to the function.
    */
  private def mkHashSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val hashTraitSym = PredefinedTraits.lookupTraitSym("Hash", root)
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TraitConstraint(Ast.TraitConstraint.Head(hashTraitSym, loc), tpe, loc)),
          Nil,
          Type.mkPureArrow(tpe, Type.mkInt32(loc), loc)
        ),
        tpe = Type.mkInt32(loc),
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TraitConstraint(Ast.TraitConstraint.Head(hashTraitSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates a Hash match rule for the given enum case.
    */
  private def mkHashMatchRule(caze: KindedAst.Case, index: Int, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      val hashSigSym = PredefinedTraits.lookupSigSym("Hash", "hash", root)
      val combineDefSym = PredefinedTraits.lookupDefSym(List("Hash"), "combine", root)

      // get a pattern corresponding to this case, e.g.
      // `case C2(x0, x1)`
      val (pat, varSyms) = mkPattern(sym, tpe, "x", loc)

      // build a hash code by repeatedly adding elements via the combine function
      // the first hash is the index + 1
      // `3 `combine` hash(x0) `combine` hash(y0)`
      val exp = varSyms.foldLeft(KindedAst.Expr.Cst(Ast.Constant.Int32(index + 1), loc): KindedAst.Expr) {
        case (acc, varSym) =>
          // `acc `combine` hash(varSym)
          KindedAst.Expr.Apply(
            KindedAst.Expr.Def(combineDefSym, Type.freshVar(Kind.Star, loc), loc),
            List(
              acc,
              KindedAst.Expr.Apply(
                KindedAst.Expr.Sig(hashSigSym, Type.freshVar(Kind.Star, loc), loc),
                List(mkVarExpr(varSym, loc)),
                Type.freshVar(Kind.Star, loc),
                Type.freshVar(Kind.Eff, loc),
                loc
              ),
            ),
            Type.freshVar(Kind.Star, loc),
            Type.freshVar(Kind.Eff, loc),
            loc
          )
      }

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Creates an Sendable instance for the given enum.
    *
    * {{{
    * enum E[a] with Sendable {
    *   case C1
    *   case C2(a)
    *   case C3(a, Int32)
    * }
    * }}}
    *
    * yields
    *
    * {{{
    * instance Sendable[E[a]] with Sendable[a]
    * }}}
    *
    * The instance is empty: we check for immutability by checking for the absence of region kinded type parameters.
    */
  private def mkSendableInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, _, tpe, _) =>
      val sendableTraitSym = PredefinedTraits.lookupTraitSym("Sendable", root)

      val tconstrs = getTraitConstraintsForTypeParams(tparams, sendableTraitSym, loc)

      Validation.success(KindedAst.Instance(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        trt = Ast.TraitSymUse(sendableTraitSym, loc),
        tpe = tpe,
        tconstrs = tconstrs,
        defs = Nil,
        assocs = Nil,
        ns = Name.RootNS,
        loc = loc
      ))
  }

  /**
    * Creates a Coerce instance for the given enum.
    *
    * The enum must be a singleton.
    *
    * {{{
    * enum Box[a](a, String)
    * }}}
    *
    * yields
    *
    * {{{
    * instance Coerce[Box[a]] {
    *   type Out = (a, String)
    *   pub def coerce(x: Box[a]): (a, String) = {
    *     match x {
    *       case Box.Box(y) => y
    *     }
    *   }
    * }}}
    */
  private def mkCoerceInstance(enum0: KindedAst.Enum, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): Validation[Option[KindedAst.Instance], DerivationError] = enum0 match {
    case KindedAst.Enum(_, _, _, sym, _, _, cases, tpe, _) =>
      if (cases.size == 1) {
        val coerceTraitSym = PredefinedTraits.lookupTraitSym("Coerce", root)
        val coerceDefSym = Symbol.mkDefnSym("Coerce.coerce", Some(flix.genSym.freshId()))

        val (_, caze) = cases.head

        val outSym = new Symbol.AssocTypeSym(coerceTraitSym, "Out", loc)
        val outTpe = caze.tpe
        val out = KindedAst.AssocTypeDef(
          Ast.Doc(Nil, loc),
          Ast.Modifiers.Empty,
          Ast.AssocTypeSymUse(outSym, loc),
          tpe,
          outTpe,
          loc
        )

        val param = Symbol.freshVarSym("x", BoundBy.FormalParam, loc)
        val exp = mkCoerceImpl(enum0, param, loc, root)
        val spec = mkCoerceSpec(enum0, param, loc, root)

        val defn = KindedAst.Def(coerceDefSym, spec, exp)

        Validation.success(Some(KindedAst.Instance(
          doc = Ast.Doc(Nil, loc),
          ann = Ast.Annotations.Empty,
          mod = Ast.Modifiers.Empty,
          trt = Ast.TraitSymUse(coerceTraitSym, loc),
          tpe = tpe,
          tconstrs = Nil,
          defs = List(defn),
          assocs = List(out),
          ns = Name.RootNS,
          loc = loc
        )))
      } else {
        Validation.toSoftFailure(None, DerivationError.IllegalNonSingletonCoerce(sym, loc))
      }
  }

  /**
    * Creates the coerce implementation for the given enum.
    */
  private def mkCoerceImpl(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Expr = enum0 match {
    case KindedAst.Enum(_, _, _, _, _, _, cases, _, _) =>
      val (_, caze) = cases.head
      val matchRule = mkCoerceMatchRule(caze, loc, root)

      KindedAst.Expr.Match(
        KindedAst.Expr.Var(param, loc),
        List(matchRule),
        loc
      )
  }

  /**
    * Creates the coerce specification for the given enum.
    */
  private def mkCoerceSpec(enum0: KindedAst.Enum, param: Symbol.VarSym, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.Spec = enum0 match {
    case KindedAst.Enum(_, _, _, _, tparams, _, cases, tpe, _) =>
      val coerceTraitSym = PredefinedTraits.lookupTraitSym("Coerce", root)
      val (_, caze) = cases.head
      val retTpe = caze.tpe
      KindedAst.Spec(
        doc = Ast.Doc(Nil, loc),
        ann = Ast.Annotations.Empty,
        mod = Ast.Modifiers.Empty,
        tparams = tparams,
        fparams = List(KindedAst.FormalParam(param, Ast.Modifiers.Empty, tpe, Ast.TypeSource.Ascribed, loc)),
        sc = Scheme(
          tparams.map(_.sym),
          List(Ast.TraitConstraint(Ast.TraitConstraint.Head(coerceTraitSym, loc), tpe, loc)),
          Nil,
          Type.mkPureArrow(tpe, retTpe, loc)
        ),
        tpe = retTpe,
        eff = Type.Cst(TypeConstructor.Pure, loc),
        tconstrs = List(Ast.TraitConstraint(Ast.TraitConstraint.Head(coerceTraitSym, loc), tpe, loc)),
        econstrs = Nil,
        loc = loc
      )
  }

  /**
    * Creates a Coerce match rule for the given enum case.
    */
  private def mkCoerceMatchRule(caze: KindedAst.Case, loc: SourceLocation, root: KindedAst.Root)(implicit flix: Flix): KindedAst.MatchRule = caze match {
    case KindedAst.Case(sym, tpe, _, _) =>
      // get a pattern corresponding to this case, e.g.
      // `case C(x0)`
      // Unlike other derivations, we do not unpack tuples
      val varSym = Symbol.freshVarSym("x0", BoundBy.Pattern, loc)
      val pat = KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), mkVarPattern(varSym, loc), Type.freshVar(Kind.Star, loc), loc)

      // the body is just whatever we extracted
      val exp = KindedAst.Expr.Var(varSym, loc)

      KindedAst.MatchRule(pat, None, exp)
  }

  /**
    * Returns the cases in `m` in a *stable order* that relies on the order of their source locations.
    */
  private def getCasesInStableOrder(m: Map[Symbol.CaseSym, KindedAst.Case]): List[KindedAst.Case] = {
    m.values.toList.sortBy(_.loc)
  }

  /**
    * Creates trait constraints for the given type parameters.
    * Filters out non-star type parameters and wild type parameters.
    */
  private def getTraitConstraintsForTypeParams(tparams: List[KindedAst.TypeParam], trt: Symbol.TraitSym, loc: SourceLocation): List[Ast.TraitConstraint] = tparams.collect {
    case tparam if tparam.sym.kind == Kind.Star && !tparam.name.isWild =>
      Ast.TraitConstraint(Ast.TraitConstraint.Head(trt, loc), Type.Var(tparam.sym, loc), loc)
  }

  /**
    * Builds a string expression from the given string.
    */
  private def mkStrExpr(str: String, loc: SourceLocation): KindedAst.Expr = KindedAst.Expr.Cst(Ast.Constant.Str(str), loc)

  /**
    * Builds a var expression from the given var sym.
    */
  private def mkVarExpr(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Expr.Var = KindedAst.Expr.Var(varSym, loc)

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concat(exp1: KindedAst.Expr, exp2: KindedAst.Expr, loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr = {
    KindedAst.Expr.Binary(SemanticOp.StringOp.Concat, exp1, exp2, Type.freshVar(Kind.Star, loc), loc)
  }

  /**
    * Builds a string concatenation expression from the given expressions.
    */
  private def concatAll(exps: List[KindedAst.Expr], loc: SourceLocation)(implicit flix: Flix): KindedAst.Expr = {
    exps match {
      case Nil => KindedAst.Expr.Cst(Ast.Constant.Str(""), loc)
      case head :: tail => tail.foldLeft(head)(concat(_, _, loc))
    }
  }

  /**
    * Extracts the types from the given aggregate type.
    * A Unit unpacks to an empty list.
    * A Tuple unpacks to its member types.
    * Anything else unpacks to the singleton list of itself.
    */
  private def unpack(tpe: Type): List[Type] = tpe.typeConstructor match {
    case Some(TypeConstructor.Unit) => Nil
    case Some(TypeConstructor.Tuple(_)) => tpe.typeArguments
    case _ => List(tpe)
  }

  /**
    * Creates a pattern corresponding to the given tag type.
    */
  private def mkPattern(sym: Symbol.CaseSym, tpe: Type, varPrefix: String, loc: SourceLocation)(implicit flix: Flix): (KindedAst.Pattern, List[Symbol.VarSym]) = {
    unpack(tpe) match {
      case Nil => (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Cst(Ast.Constant.Unit, loc), Type.freshVar(Kind.Star, loc), loc), Nil)
      case _ :: Nil =>
        val varSym = Symbol.freshVarSym(s"${varPrefix}0", BoundBy.Pattern, loc)
        (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), mkVarPattern(varSym, loc), Type.freshVar(Kind.Star, loc), loc), List(varSym))
      case tpes =>
        val varSyms = tpes.zipWithIndex.map { case (_, index) => Symbol.freshVarSym(s"$varPrefix$index", BoundBy.Pattern, loc) }
        val subPats = varSyms.map(varSym => mkVarPattern(varSym, loc))
        (KindedAst.Pattern.Tag(Ast.CaseSymUse(sym, loc), KindedAst.Pattern.Tuple(subPats, loc), Type.freshVar(Kind.Star, loc), loc), varSyms)
    }
  }

  /**
    * Creates a variable pattern using the given variable symbol.
    */
  private def mkVarPattern(varSym: Symbol.VarSym, loc: SourceLocation): KindedAst.Pattern = KindedAst.Pattern.Var(varSym, varSym.tvar, loc)

  /**
    * Inserts `sep` between every two elements of `list`.
    */
  private def intersperse[A](list: List[A], sep: A): List[A] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: neck :: tail => head :: sep :: intersperse(neck :: tail, sep)
  }
}
