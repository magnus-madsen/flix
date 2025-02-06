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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Modifiers, Scope}
import ca.uwaterloo.flix.language.ast.{Purity, Symbol, *}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.tailrec

/**
  * A phase that simplifies the MonoAst by elimination of pattern matching and other rewritings.
  */
object Simplifier {

  // We are safe to use the top scope everywhere because we do not use unification in this or future phases.
  private implicit val S: Scope = Scope.Top

  def run(root: MonoAst.Root)(implicit flix: Flix): SimplifiedAst.Root = flix.phase("Simplifier") {
    implicit val universe: Set[Symbol.EffectSym] = root.effects.keys.toSet
    implicit val r: MonoAst.Root = root
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    SimplifiedAst.Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  private def visitDef(decl: MonoAst.Def)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Def = decl match {
    case MonoAst.Def(sym, spec, exp, _) =>
      val fs = spec.fparams.map(visitFormalParam)
      val e = visitExp(exp)
      val funType = spec.functionType
      val retType = visitType(funType.arrowResultType)
      val eff = simplifyEffect(funType.arrowEffectType)
      SimplifiedAst.Def(spec.ann, spec.mod, sym, fs, e, retType, eff, sym.loc)
  }

  private def visitEnum(enm: MonoAst.Enum): SimplifiedAst.Enum = enm match {
    case MonoAst.Enum(_, ann, mod, sym, tparams0, cases0, loc) =>
      val tparams = tparams0.map(param => SimplifiedAst.TypeParam(param.name, param.sym, param.loc))
      val cases = MapOps.mapValues(cases0)(visitEnumCase)
      SimplifiedAst.Enum(ann, mod, sym, tparams, cases, loc)
  }

  private def visitEnumCase(caze: MonoAst.Case): SimplifiedAst.Case = caze match {
    case MonoAst.Case(sym, tpes, loc) => SimplifiedAst.Case(sym, tpes.map(visitPolyType), loc)
  }

  private def visitStruct(struct: MonoAst.Struct): SimplifiedAst.Struct = struct match {
    case MonoAst.Struct(_, ann, mod, sym, tparams0, fields0, loc) =>
      val tparams = tparams0.map(param => SimplifiedAst.TypeParam(param.name, param.sym, param.loc))
      val fields = fields0.map(visitStructField)
      SimplifiedAst.Struct(ann, mod, sym, tparams, fields, loc)
  }

  private def visitStructField(field: MonoAst.StructField): SimplifiedAst.StructField = field match {
    case MonoAst.StructField(sym, tpe, loc) => SimplifiedAst.StructField(sym, visitPolyType(tpe), loc)
  }

  private def visitEffect(decl: MonoAst.Effect)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Effect = decl match {
    case MonoAst.Effect(_, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffOp)
      SimplifiedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitExp(exp0: MonoAst.Expr)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Expr = exp0 match {
    case MonoAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Var(sym, t, loc)

    case MonoAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Cst(cst, t, loc)

    case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.Lambda(List(p), e, t, loc)

    case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyClo(e1, e2, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.ApplyDef(sym, exps, _, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyDef(sym, es, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyLocalDef(sym, es, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val purity = simplifyEffect(eff)
      op match {
        case AtomicOp.Binary(SemanticOp.StringOp.Concat) =>
          // Translate to InvokeMethod exp
          val strClass = Class.forName("java.lang.String")
          val method = strClass.getMethod("concat", strClass)
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(AtomicOp.InvokeMethod(method), es, t, purity, loc)

        case AtomicOp.ArrayLit | AtomicOp.ArrayNew =>
          // The region expression is dropped (head of exps / es)
          val es1 = es.tail
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(op, es1, t, purity, loc)

        case AtomicOp.Spawn =>
          // Wrap the expression in a closure: () -> tpe \ ef
          val List(e1, e2) = es
          val lambdaTyp = MonoType.Arrow(List(MonoType.Unit), e1.tpe)
          val fp = SimplifiedAst.FormalParam(Symbol.freshVarSym("_spawn", BoundBy.FormalParam, loc), Modifiers.Empty, MonoType.Unit, loc)
          val lambdaExp = SimplifiedAst.Expr.Lambda(List(fp), e1, lambdaTyp, loc)
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(lambdaExp, e2), t, Purity.Impure, loc)

        case AtomicOp.Lazy =>
          // Wrap the expression in a closure: () -> tpe \ Pure
          val e = es.head
          val lambdaTyp = MonoType.Arrow(List(MonoType.Unit), e.tpe)
          val fp = SimplifiedAst.FormalParam(Symbol.freshVarSym("_lazy", BoundBy.FormalParam, loc), Modifiers.Empty, MonoType.Unit, loc)
          val lambdaExp = SimplifiedAst.Expr.Lambda(List(fp), e, lambdaTyp, loc)
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Lazy, List(lambdaExp), t, Purity.Pure, loc)

        case AtomicOp.HoleError(_) | AtomicOp.Throw =>
          // Simplify purity to impure, must be done after Monomorph
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(op, es, t, Purity.Impure, loc)

        case _ =>
          val t = visitType(tpe)
          SimplifiedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
      }

    case MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), t, simplifyEffect(eff), loc)

    case MonoAst.Expr.Stm(e1, e2, tpe, eff, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Stm(visitExp(e1), visitExp(e2), t, simplifyEffect(eff), loc)

    case d@MonoAst.Expr.Discard(exp, eff, loc) =>
      val sym = Symbol.freshVarSym("_", BoundBy.Let, loc)
      val t = visitType(d.tpe)
      SimplifiedAst.Expr.Let(sym, visitExp(exp), SimplifiedAst.Expr.Cst(Constant.Unit, MonoType.Unit, loc), t, simplifyEffect(eff), loc)

    case MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Let(sym, visitExp(e1), visitExp(e2), t, simplifyEffect(eff), loc)

    case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val ef = simplifyEffect(eff)
      SimplifiedAst.Expr.LocalDef(sym, fps, e1, e2, t, ef, loc)

    case MonoAst.Expr.Scope(sym, _, exp, tpe, eff, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Scope(sym, visitExp(exp), t, simplifyEffect(eff), loc)

    case MonoAst.Expr.Match(exp0, rules, tpe, _, loc) =>
      patternMatchWithLabels(exp0, rules, tpe, loc)

    case MonoAst.Expr.VectorLit(exps, tpe, _, loc) =>
      // Note: We simplify Vectors to Arrays.
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLit, es, t, Purity.Pure, loc)

    case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, _, loc) =>
      // Note: We simplify Vectors to Arrays.
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLoad, List(e1, e2), t, Purity.Pure, loc)

    case MonoAst.Expr.VectorLength(exp, loc) =>
      // Note: We simplify Vectors to Arrays.
      val e = visitExp(exp)
      val purity = e.purity
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.ArrayLength, List(e), MonoType.Int32, purity, loc)

    case MonoAst.Expr.Ascribe(exp, _, _, _) => visitExp(exp)

    case MonoAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Cast, List(e), t, simplifyEffect(eff), loc)

    case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case MonoAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          SimplifiedAst.CatchRule(sym, clazz, b)
      }
      val t = visitType(tpe)
      SimplifiedAst.Expr.TryCatch(e, rs, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case MonoAst.HandlerRule(sym, fparams, body) =>
          val fps = fparams.map(visitFormalParam)
          val b = visitExp(body)
          SimplifiedAst.HandlerRule(sym, fps, b)
      }
      val t = visitType(tpe)
      SimplifiedAst.Expr.TryWith(e, effUse, rs, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      SimplifiedAst.Expr.Do(op, es, t, simplifyEffect(eff), loc)

    case MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val t = visitType(tpe)
      val methods = methods0 map visitJvmMethod
      SimplifiedAst.Expr.NewObject(name, clazz, t, simplifyEffect(eff), methods, loc)

  }

  /**
    * Returns the [[MonoType]] representation of `tpe`.
    *
    * This includes:
    *   - Removing regions from Array.
    *   - Removing effects from Arrow.
    *   - Removing type arguments from Region.
    *   - Converting Vector into Array (with no region as above).
    *   - Converting restrictable enums into regular enums.
    *   - Flattening schema- and record rows into types without their Schema/Record constructor.
    *   - Converting set/caseset/bool formulas into Unit.
    */
  private def visitType(tpe: Type): MonoType = {
    val base = tpe.typeConstructor
    base match {
      case None => tpe match {
        case _ => throw InternalCompilerException(s"Unexpected type: $tpe", tpe.loc)
      }

      case Some(tc) =>
        tc match {
          case TypeConstructor.Void => MonoType.Void

          case TypeConstructor.AnyType => MonoType.AnyType

          case TypeConstructor.Unit => MonoType.Unit

          case TypeConstructor.Null => MonoType.Null

          case TypeConstructor.Bool => MonoType.Bool

          case TypeConstructor.Char => MonoType.Char

          case TypeConstructor.Float32 => MonoType.Float32

          case TypeConstructor.Float64 => MonoType.Float64

          case TypeConstructor.BigDecimal => MonoType.BigDecimal

          case TypeConstructor.Int8 => MonoType.Int8

          case TypeConstructor.Int16 => MonoType.Int16

          case TypeConstructor.Int32 => MonoType.Int32

          case TypeConstructor.Int64 => MonoType.Int64

          case TypeConstructor.BigInt => MonoType.BigInt

          case TypeConstructor.Str => MonoType.String

          case TypeConstructor.Regex => MonoType.Regex

          case TypeConstructor.RecordRowEmpty => MonoType.RecordEmpty

          case TypeConstructor.Sender => throw InternalCompilerException("Unexpected Sender", tpe.loc)

          case TypeConstructor.Receiver => throw InternalCompilerException("Unexpected Receiver", tpe.loc)

          case TypeConstructor.Lazy =>
            val List(elm) = tpe.typeArguments
            MonoType.Lazy(visitType(elm))

          case TypeConstructor.Enum(sym, _) =>
            val targs = tpe.typeArguments
            MonoType.Enum(sym, targs.map(visitType))

          case TypeConstructor.Struct(sym, _) =>
            val targs = tpe.typeArguments
            MonoType.Struct(sym, targs.map(visitType))

          case TypeConstructor.RestrictableEnum(sym, _) =>
            val targs = tpe.typeArguments
            val enumSym = new Symbol.EnumSym(sym.namespace, sym.name, sym.loc)
            MonoType.Enum(enumSym, targs.map(visitType))

          case TypeConstructor.Native(clazz) => MonoType.Native(clazz)

          case TypeConstructor.Array =>
            // Remove the region from the array.
            val List(elm, _) = tpe.typeArguments
            MonoType.Array(visitType(elm))

          case TypeConstructor.Vector =>
            val List(elm) = tpe.typeArguments
            MonoType.Array(visitType(elm))

          case TypeConstructor.RegionToStar =>
            // Remove the type argument.
            MonoType.Region

          case TypeConstructor.RegionToEff =>
            MonoType.Unit

          case TypeConstructor.Tuple(_) =>
            val targs = tpe.typeArguments
            MonoType.Tuple(targs.map(visitType))

          case TypeConstructor.Arrow(_) =>
            // Remove the effect from the arrow.
            // Arrow type arguments are ordered (effect, args.., result type).
            val _ :: targs = tpe.typeArguments
            val (args, List(res)) = targs.splitAt(targs.length - 1)
            MonoType.Arrow(args.map(visitType), visitType(res))

          case TypeConstructor.RecordRowExtend(label) =>
            val List(labelType, restType) = tpe.typeArguments
            MonoType.RecordExtend(label.name, visitType(labelType), visitType(restType))

          case TypeConstructor.Record =>
            val List(elm) = tpe.typeArguments
            // The row types themselves return monotype records, so we do nothing here.
            visitType(elm)

          case TypeConstructor.Region(_) => MonoType.Unit

          case TypeConstructor.True => MonoType.Unit
          case TypeConstructor.False => MonoType.Unit
          case TypeConstructor.Not => MonoType.Unit
          case TypeConstructor.And => MonoType.Unit
          case TypeConstructor.Or => MonoType.Unit

          case TypeConstructor.Pure => MonoType.Unit
          case TypeConstructor.Univ => MonoType.Unit
          case TypeConstructor.Complement => MonoType.Unit
          case TypeConstructor.Union => MonoType.Unit
          case TypeConstructor.Intersection => MonoType.Unit
          case TypeConstructor.Difference => MonoType.Unit
          case TypeConstructor.SymmetricDiff => MonoType.Unit
          case TypeConstructor.Effect(_) => MonoType.Unit
          case TypeConstructor.CaseSet(_, _) => MonoType.Unit
          case TypeConstructor.CaseComplement(_) => MonoType.Unit
          case TypeConstructor.CaseIntersection(_) => MonoType.Unit
          case TypeConstructor.CaseUnion(_) => MonoType.Unit

          case TypeConstructor.Relation =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Lattice =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.SchemaRowEmpty =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.SchemaRowExtend(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Schema =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmConstructor(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmMethod(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmField(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Error(_, _) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.ArrowWithoutEffect(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.ArrayWithoutRegion =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.RegionWithoutRegion =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.GenericRegion(prop) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
        }
    }
  }

  /**
    * Adaptation of [[visitType]] that returns [[Type]] instead.
    *
    * The change from [[visitType]] is that here we might have type variables and we also cannot
    * eliminate rows (a type could be `a[#(x=Char | b)]` for example).
    */
  private def visitPolyType(tpe: Type): Type = {
    val base = tpe.baseType
    base match {
      case v@Type.Var(_, _) =>
        val targs = tpe.typeArguments
        Type.mkApply(v, targs.map(visitPolyType), tpe.loc)

      case cst@Type.Cst(tc, loc) =>
        tc match {
          case TypeConstructor.Void => cst

          case TypeConstructor.AnyType => cst

          case TypeConstructor.Unit => cst

          case TypeConstructor.Null => cst

          case TypeConstructor.Bool => cst

          case TypeConstructor.Char => cst

          case TypeConstructor.Float32 => cst

          case TypeConstructor.Float64 => cst

          case TypeConstructor.BigDecimal => cst

          case TypeConstructor.Int8 => cst

          case TypeConstructor.Int16 => cst

          case TypeConstructor.Int32 => cst

          case TypeConstructor.Int64 => cst

          case TypeConstructor.BigInt => cst

          case TypeConstructor.Str => cst

          case TypeConstructor.Regex => cst

          case TypeConstructor.RecordRowEmpty => cst

          case TypeConstructor.Sender => throw InternalCompilerException("Unexpected Sender", tpe.loc)

          case TypeConstructor.Receiver => throw InternalCompilerException("Unexpected Receiver", tpe.loc)

          case TypeConstructor.Lazy =>
            val List(elm) = tpe.typeArguments
            Type.mkLazy(visitPolyType(elm), loc)

          case TypeConstructor.Enum(sym, _) =>
            val targs = tpe.typeArguments
            Type.mkEnum(sym, targs.map(visitPolyType), loc)

          case TypeConstructor.Struct(sym, _) =>
            val targs = tpe.typeArguments
            Type.mkStruct(sym, targs.map(visitPolyType), loc)

          case TypeConstructor.RestrictableEnum(sym, _) =>
            val targs = tpe.typeArguments
            val enumSym = new Symbol.EnumSym(sym.namespace, sym.name, sym.loc)
            Type.mkEnum(enumSym, targs.map(visitPolyType), loc)

          case TypeConstructor.Native(_) => cst

          case TypeConstructor.Array =>
            // Remove the region from the array.
            val List(elm, _) = tpe.typeArguments
            Type.mkArrayWithoutRegion(visitPolyType(elm), loc)

          case TypeConstructor.Vector =>
            val List(elm) = tpe.typeArguments
            Type.mkArrayWithoutRegion(visitPolyType(elm), loc)

          case TypeConstructor.RegionToStar =>
            // Remove the type argument.
            Type.Cst(TypeConstructor.RegionWithoutRegion, loc)

          case TypeConstructor.RegionToEff =>
            Type.mkUnit(loc)

          case TypeConstructor.Tuple(_) =>
            val targs = tpe.typeArguments
            Type.mkTuple(targs.map(visitPolyType), loc)

          case TypeConstructor.Arrow(_) =>
            // Remove the effect from the arrow.
            // Arrow type arguments are ordered (effect, args.., result type).
            val eff :: targs = tpe.typeArguments
            val (args, List(res)) = targs.splitAt(targs.length - 1)
            Type.mkArrowWithoutEffect(args.map(visitPolyType), visitPolyType(res), loc)

          case TypeConstructor.RecordRowExtend(label) =>
            val List(labelType, restType) = tpe.typeArguments
            Type.mkRecordRowExtend(label, visitPolyType(labelType), visitPolyType(restType), loc)

          case TypeConstructor.Record =>
            val List(elm) = tpe.typeArguments
            Type.mkRecord(visitPolyType(elm), loc)

          case TypeConstructor.Region(_) => Type.mkUnit(loc)

          case TypeConstructor.True => Type.mkUnit(loc)
          case TypeConstructor.False => Type.mkUnit(loc)
          case TypeConstructor.Not => Type.mkUnit(loc)
          case TypeConstructor.And => Type.mkUnit(loc)
          case TypeConstructor.Or => Type.mkUnit(loc)

          case TypeConstructor.Pure => Type.mkUnit(loc)
          case TypeConstructor.Univ => Type.mkUnit(loc)
          case TypeConstructor.Complement => Type.mkUnit(loc)
          case TypeConstructor.Union => Type.mkUnit(loc)
          case TypeConstructor.Intersection => Type.mkUnit(loc)
          case TypeConstructor.Difference => Type.mkUnit(loc)
          case TypeConstructor.SymmetricDiff => Type.mkUnit(loc)
          case TypeConstructor.Effect(_) => Type.mkUnit(loc)
          case TypeConstructor.CaseSet(_, _) => Type.mkUnit(loc)
          case TypeConstructor.CaseComplement(_) => Type.mkUnit(loc)
          case TypeConstructor.CaseIntersection(_) => Type.mkUnit(loc)
          case TypeConstructor.CaseUnion(_) => Type.mkUnit(loc)

          case TypeConstructor.SchemaRowEmpty => Type.mkSchemaRowEmpty(loc)
          case TypeConstructor.SchemaRowExtend(pred) =>
            val List(predType, restType) = tpe.typeArguments
            Type.mkSchemaRowExtend(pred, visitPolyType(predType), visitPolyType(restType), loc)

          case TypeConstructor.Relation =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Lattice =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Schema =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmConstructor(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmMethod(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.JvmField(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Error(_, _) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.ArrowWithoutEffect(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.ArrayWithoutRegion =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.RegionWithoutRegion =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.GenericRegion(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
        }

      case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
      case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
      case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
      case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
      case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
      case Type.GetEff(_, _, _) => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
    }
  }

  private def visitFormalParam(p: MonoAst.FormalParam): SimplifiedAst.FormalParam = {
    val t = visitType(p.tpe)
    SimplifiedAst.FormalParam(p.sym, p.mod, t, p.loc)
  }

  private def visitJvmMethod(method: MonoAst.JvmMethod)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.JvmMethod = method match {
    case MonoAst.JvmMethod(ident, fparams0, exp0, retTpe, eff, loc) =>
      val fparams = fparams0 map visitFormalParam
      val exp = visitExp(exp0)
      val rt = visitType(retTpe)
      SimplifiedAst.JvmMethod(ident, fparams, exp, rt, simplifyEffect(eff), loc)
  }

  private def pat2exp(pat0: MonoAst.Pattern): SimplifiedAst.Expr = pat0 match {
    case MonoAst.Pattern.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      SimplifiedAst.Expr.Cst(cst, t, loc)
    case MonoAst.Pattern.Tag(CaseSymUse(sym, _), ps, tpe, loc) =>
      val es = ps.map(pat2exp)
      val t = visitType(tpe)
      val purity = Purity.combineAll(es.map(_.purity))
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Tag(sym), es, t, purity, loc)
    case MonoAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(pat2exp)
      val t = visitType(tpe)
      val purity = Purity.combineAll(es.map(_.purity))
      SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Tuple, es, t, purity, loc)
    case _ => throw InternalCompilerException(s"Unexpected non-literal pattern $pat0.", pat0.loc)
  }

  private def isPatLiteral(pat0: MonoAst.Pattern): Boolean = pat0 match {
    case MonoAst.Pattern.Cst(_, _, _) => true
    case _ => false
  }

  private def mkEqual(e1: SimplifiedAst.Expr, e2: SimplifiedAst.Expr, loc: SourceLocation): SimplifiedAst.Expr = {
    /*
     * Special Case 1: Unit
     * Special Case 2: String - must be desugared to String.equals
     */
    (e1.tpe, e2.tpe) match {
      case (MonoType.Unit, MonoType.Unit) =>
        // Unit is always equal to itself.
        return SimplifiedAst.Expr.Cst(Constant.Bool(true), MonoType.Bool, loc)

      case (MonoType.String, _) =>
        val strClass = Class.forName("java.lang.String")
        val objClass = Class.forName("java.lang.Object")
        val method = strClass.getMethod("equals", objClass)
        val op = AtomicOp.InvokeMethod(method)
        return SimplifiedAst.Expr.ApplyAtomic(op, List(e1, e2), MonoType.Bool, Purity.combine(e1.purity, e2.purity), loc)

      case (MonoType.BigInt, _) =>
        val bigIntClass = Class.forName("java.math.BigInteger")
        val objClass = Class.forName("java.lang.Object")
        val method = bigIntClass.getMethod("equals", objClass)
        val op = AtomicOp.InvokeMethod(method)
        return SimplifiedAst.Expr.ApplyAtomic(op, List(e1, e2), MonoType.Bool, Purity.combine(e1.purity, e2.purity), loc)

      case _ => // fallthrough
    }

    /*
     * Compute the semantic operator.
     */
    val sop = e1.tpe match {
      case MonoType.Bool => SemanticOp.BoolOp.Eq
      case MonoType.Char => SemanticOp.CharOp.Eq
      case MonoType.Float32 => SemanticOp.Float32Op.Eq
      case MonoType.Float64 => SemanticOp.Float64Op.Eq
      case MonoType.Int8 => SemanticOp.Int8Op.Eq
      case MonoType.Int16 => SemanticOp.Int16Op.Eq
      case MonoType.Int32 => SemanticOp.Int32Op.Eq
      case MonoType.Int64 => SemanticOp.Int64Op.Eq
      case t => throw InternalCompilerException(s"Unexpected type: '$t'.", e1.loc)
    }
    val purity = Purity.combine(e1.purity, e2.purity)
    SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Binary(sop), List(e1, e2), MonoType.Bool, purity, loc)
  }

  /**
    * Eliminates pattern matching by translations to labels and jumps.
    */
  private def patternMatchWithLabels(exp0: MonoAst.Expr, rules: List[MonoAst.MatchRule], tpe: Type, loc: SourceLocation)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Expr = {
    //
    // Given the code:
    //
    // match x {
    //   case PATTERN_1 => BODY_1
    //   case PATTERN_2 => BODY_2
    //   ...
    //   case PATTERN_N => BODY_N
    // }
    //
    // The structure of the generated code is as follows:
    //
    // let matchVar = x ;
    //
    //   branch {
    //     jumpto label$1
    //
    //     label$1:
    //       ...
    //     label$2:
    //       ...
    //     default:
    //       MatchError
    //   }
    //

    // Generate a fresh variable to hold the result of the match expression.
    val matchVar = Symbol.freshVarSym("matchVar" + Flix.Delimiter, BoundBy.Let, loc)

    // Translate the match expression.
    val matchExp = visitExp(exp0)

    // Generate a fresh label for the default fall through case.
    val defaultLab = Symbol.freshLabel("default")

    // Generate a label for each rule.
    val ruleLabels = rules.map(_ => Symbol.freshLabel("case"))

    // Construct a map from each label to the label of the next case.
    // The default label is the next label of the last case.
    val nextLabel = (ruleLabels zip (ruleLabels.drop(1) ::: defaultLab :: Nil)).toMap

    val t = visitType(tpe)

    // TODO Intermediate solution (which is correct, but imprecise): Compute the purity of every match rule in rules
    val jumpPurity = Purity.combineAll(rules.map(r => simplifyEffect(r.exp.eff)))

    // Create a branch for each rule.
    val branches = (ruleLabels zip rules) map {
      // Process each (label, rule) pair.
      case (label, MonoAst.MatchRule(pat, guard, body)) =>
        // Retrieve the label of the next rule.
        // If this rule is the last, the next label is the default label.
        val next = nextLabel(label)

        // Success case: evaluate the match body.
        val success = visitExp(body)

        // Failure case: Jump to the next label.
        val failure = SimplifiedAst.Expr.JumpTo(next, t, jumpPurity, loc)

        // Return the branch with its label.
        label -> patternMatchList(List(pat), List(matchVar), guard.getOrElse(MonoAst.Expr.Cst(Constant.Bool(true), Type.Bool, SourceLocation.Unknown)), success, failure
        )
    }
    // Construct the error branch.
    val errorExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.MatchError, List.empty, t, Purity.Impure, loc)
    val errorBranch = defaultLab -> errorExp

    // The initial expression simply jumps to the first label.
    val entry = SimplifiedAst.Expr.JumpTo(ruleLabels.head, t, jumpPurity, loc)

    // The purity of the branch
    val branchPurity = Purity.combineAll(branches.map { case (_, exp) => exp.purity })

    // Assemble all the branches together.
    val branch = SimplifiedAst.Expr.Branch(entry, branches.toMap + errorBranch, t, branchPurity, loc)

    // The purity of the match exp
    val matchPurity = Purity.combine(matchExp.purity, branch.purity)

    // Wrap the branches inside a let-binding for the match variable.
    SimplifiedAst.Expr.Let(matchVar, matchExp, branch, t, matchPurity, loc)
  }

  /**
    * Returns an expression that matches the given list of patterns `xs` against the given list of variables `ys`.
    *
    * Checks the `guard` when all patterns have been matched.
    *
    * Evaluates `succ` on success and `fail` otherwise.
    */
  private def patternMatchList(xs: List[MonoAst.Pattern], ys: List[Symbol.VarSym], guard: MonoAst.Expr, succ: SimplifiedAst.Expr, fail: SimplifiedAst.Expr)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Expr =
    ((xs, ys): @unchecked) match {
      /**
        * There are no more patterns and variables to match.
        *
        * The pattern was match successfully. Test the guard.
        */
      case (Nil, Nil) =>
        val g = visitExp(guard)
        // Only produce IfThenElse if g is non-trivial
        g match {
          case SimplifiedAst.Expr.Cst(Constant.Bool(true), _, _) => succ
          case SimplifiedAst.Expr.Cst(Constant.Bool(false), _, _) => fail
          case e => SimplifiedAst.Expr.IfThenElse(e, succ, fail, succ.tpe, g.purity, g.loc)
        }

      /**
        * Matching a wildcard is guaranteed to succeed.
        *
        * We proceed by recursion on the remaining patterns and variables.
        */
      case (MonoAst.Pattern.Wild(_, _) :: ps, _ :: vs) =>
        patternMatchList(ps, vs, guard, succ, fail)

      /**
        * Matching a variable is guaranteed to succeed.
        *
        * We proceed by constructing a let-binding that binds the value
        * of the match variable `ident` to the variable `v`.
        * The body of the let-binding is computed by recursion on the
        * remaining patterns and variables.
        */
      case (MonoAst.Pattern.Var(sym, tpe, loc) :: ps, v :: vs) =>
        val t = visitType(tpe)
        val exp = patternMatchList(ps, vs, guard, succ, fail)
        SimplifiedAst.Expr.Let(sym, SimplifiedAst.Expr.Var(v, t, loc), exp, succ.tpe, exp.purity, loc)

      /**
        * Matching a literal may succeed or fail.
        *
        * We generate a binary expression testing whether the literal `lit`
        * matches the variable `v` and then we generate an if-then-else
        * expression where the consequent expression is determined by
        * recursion on the remaining patterns and variables and the
        * alternative expression is `fail`.
        */
      case (lit :: ps, v :: vs) if isPatLiteral(lit) =>
        val exp = patternMatchList(ps, vs, guard, succ, fail)
        val t = visitType(lit.tpe)
        val cond = mkEqual(pat2exp(lit), SimplifiedAst.Expr.Var(v, t, lit.loc), lit.loc)
        val purity = Purity.combine3(cond.purity, exp.purity, fail.purity)
        SimplifiedAst.Expr.IfThenElse(cond, exp, fail, succ.tpe, purity, lit.loc)

      /**
        * Matching a tag may succeed or fail.
        *
        * We generate a binary expression testing whether the tag name `tag`
        * matches the tag extracted from the variable `v` and then we generate
        * an if-then-else expression where the consequent expression is determined
        * by recursion on the remaining patterns and variables together with the
        * nested pattern of the tag added in front and new fresh variables holding
        * the values of the tag.
        *
        * Simple Example:
        * {{{
        * match x {
        *   ...
        *   case Foo(y, z) => exp
        *   ...
        * }
        * }}}
        *
        * becomes
        *
        * {{{
        *   if (x is Foo) {
        *     let y = untag 0 x;
        *     let z = untag 1 x;
        *     exp
        *   } else {
        *     ...
        *   }
        * }}}
        */
      case (MonoAst.Pattern.Tag(CaseSymUse(sym, _), pats, tpe, loc) :: ps, v :: vs) =>
        val varExp = SimplifiedAst.Expr.Var(v, visitType(tpe), loc)
        val cond = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Is(sym), List(varExp), MonoType.Bool, Purity.Pure, loc)
        val freshVars = pats.map(_ => Symbol.freshVarSym("innerTag" + Flix.Delimiter, BoundBy.Let, loc))
        val zero = patternMatchList(pats ::: ps, freshVars ::: vs, guard, succ, fail)
        val consequent = pats.zip(freshVars).zipWithIndex.foldRight(zero) {
          case (((pat, name), idx), exp) =>
            val varExp = SimplifiedAst.Expr.Var(v, visitType(tpe), loc)
            val indexExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Untag(sym, idx), List(varExp), visitType(pat.tpe), Purity.Pure, loc)
            SimplifiedAst.Expr.Let(name, indexExp, exp, succ.tpe, exp.purity, loc)
        }
        val purity2 = Purity.combine3(cond.purity, consequent.purity, fail.purity)
        SimplifiedAst.Expr.IfThenElse(cond, consequent, fail, succ.tpe, purity2, loc)

      /**
        * Matching a tuple may succeed or fail.
        *
        * We generate a fresh variable and let-binding for each component of the
        * tuple and then we recurse on the nested patterns and freshly generated
        * variables.
        */
      case (MonoAst.Pattern.Tuple(elms, tpe, loc) :: ps, v :: vs) =>
        val freshVars = elms.map(_ => Symbol.freshVarSym("innerElm" + Flix.Delimiter, BoundBy.Let, loc))
        val zero = patternMatchList(elms ::: ps, freshVars ::: vs, guard, succ, fail)
        elms.zip(freshVars).zipWithIndex.foldRight(zero) {
          case (((pat, name), idx), exp) =>
            val varExp = SimplifiedAst.Expr.Var(v, visitType(tpe), loc)
            val indexExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Index(idx), List(varExp), visitType(pat.tpe), Purity.Pure, loc)
            SimplifiedAst.Expr.Let(name, indexExp, exp, succ.tpe, exp.purity, loc)
        }

      /**
        * Matching a record may succeed or fail.
        *
        * We generate a fresh variable and let-binding for each component of the
        * record (label or extension) and then we recurse on the nested patterns
        * and freshly generated variables.
        */
      case (MonoAst.Pattern.Record(pats, pat, tpe, loc) :: ps, v :: vs) =>
        val freshVars = pats.map(_ => Symbol.freshVarSym("innerLabel" + Flix.Delimiter, BoundBy.Let, loc))
        val labelPats = pats.map(_.pat)
        val varExp = SimplifiedAst.Expr.Var(v, visitType(tpe), loc)
        val zero = patternMatchList(labelPats ::: ps, freshVars ::: vs, guard, succ, fail)
        // Let-binders are built in reverse, but it does not matter since binders are independent and pure
        val (one, restrictedMatchVar) = pats.zip(freshVars).foldLeft((zero, varExp): (SimplifiedAst.Expr, SimplifiedAst.Expr)) {
          case ((exp, matchVarExp), (MonoAst.Pattern.Record.RecordLabelPattern(label, pat, _, loc1), name)) =>
            val recordSelectExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.RecordSelect(label), List(matchVarExp), visitType(pat.tpe), Purity.Pure, loc1)
            val restrictedMatchVarExp = SimplifiedAst.Expr.ApplyAtomic(AtomicOp.RecordRestrict(label), List(matchVarExp), mkRecordRestrict(label, matchVarExp.tpe), matchVarExp.purity, loc1)
            val labelLetBinding = SimplifiedAst.Expr.Let(name, recordSelectExp, exp, succ.tpe, exp.purity, loc1)
            (labelLetBinding, restrictedMatchVarExp)
        }
        pat match {
          case MonoAst.Pattern.Var(sym, _, varLoc) =>
            // Extension is { ... | sym } so we generate a let-binding `let sym = matchVar`
            SimplifiedAst.Expr.Let(sym, restrictedMatchVar, one, succ.tpe, restrictedMatchVar.purity, varLoc)
          case _ =>
            // Extension is either wild or non-existent
            one
        }

      case p => throw InternalCompilerException(s"Unsupported pattern '$p'.", xs.head.loc)
    }

  private def visitEffOp(op: MonoAst.Op)(implicit universe: Set[Symbol.EffectSym], root: MonoAst.Root, flix: Flix): SimplifiedAst.Op = op match {
    case MonoAst.Op(sym, MonoAst.Spec(_, ann, mod, fparams0, _, retTpe0, eff0), loc) =>
      val fparams = fparams0.map(visitFormalParam)
      val retTpe = visitType(retTpe0)
      val eff = simplifyEffect(eff0)
      SimplifiedAst.Op(sym, ann, mod, fparams, retTpe, eff, loc)
  }

  /**
    * Returns the purity of an expression.
    */
  private def simplifyEffect(eff: Type)(implicit universe: Set[Symbol.EffectSym]): Purity = {
    Purity.fromType(eff)
  }

  /**
    * Performs record restriction on `tpe` by removing the first occurrence of `RecordRowExtend(label)` from `tpe`.
    *
    * @param label the label / record row to remove from `tpe`.
    * @param tpe   the record type.
    */
  private def mkRecordRestrict(label: Name.Label, tpe: MonoType): MonoType = {
    @tailrec
    def visit(t: MonoType, cont: MonoType => MonoType): MonoType = t match {
      case MonoType.RecordExtend(f, _, tail) if label.name == f => cont(tail)
      case MonoType.RecordExtend(f, tp, tail) => visit(tail, ty => cont(MonoType.RecordExtend(f, tp, ty)))
      case ty => cont(ty)
    }

    visit(tpe, t => t)
  }
}
