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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.JvmMember
import ca.uwaterloo.flix.language.ast.shared.VarText
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.{Constructor, Field, Method}

/**
  * A well-kinded type in an easily-printable format.
  */
sealed trait SimpleType

object SimpleType {

  private class OverAppliedType(loc: SourceLocation) extends InternalCompilerException("Unexpected over-applied type.", loc)

  ///////
  // Hole
  ///////

  /**
    * An unfilled parameter in a partially-applied type-level function.
    * For example, `Not` (applied to nothing) is `Not(Hole)`.
    */
  case object Hole extends SimpleType

  /////////////
  // Primitives
  /////////////

  case object Void extends SimpleType

  case object AnyType extends SimpleType

  case object Unit extends SimpleType

  case object Null extends SimpleType

  case object Bool extends SimpleType

  case object Char extends SimpleType

  case object Float32 extends SimpleType

  case object Float64 extends SimpleType

  case object BigDecimal extends SimpleType

  case object Int8 extends SimpleType

  case object Int16 extends SimpleType

  case object Int32 extends SimpleType

  case object Int64 extends SimpleType

  case object BigInt extends SimpleType

  case object Str extends SimpleType

  case object Regex extends SimpleType

  case object Array extends SimpleType

  case object ArrayWithoutRegion extends SimpleType

  case object Vector extends SimpleType

  case object Sender extends SimpleType

  case object Receiver extends SimpleType

  case object Lazy extends SimpleType

  case object Pure extends SimpleType

  case object Univ extends SimpleType

  case object False extends SimpleType

  case object True extends SimpleType

  case object RegionToStar extends SimpleType

  case object RegionWithoutRegion extends SimpleType

  //////////
  // Records
  //////////

  /**
    * A record constructor. `arg` should be a variable or a Hole.
    */
  case class RecordConstructor(arg: SimpleType) extends SimpleType

  /**
    * An unextended record.
    */
  case class Record(labels: List[RecordLabelType]) extends SimpleType

  /**
    * An extended record. `arg` should be a variable or a Hole.
    */
  case class RecordExtend(labels: List[RecordLabelType], rest: SimpleType) extends SimpleType

  /**
    * An unextended record row.
    */
  case class RecordRow(labels: List[RecordLabelType]) extends SimpleType

  /**
    * An extended record row. `arg` should be a variable or a Hole.
    */
  case class RecordRowExtend(labels: List[RecordLabelType], rest: SimpleType) extends SimpleType

  //////////
  // Schemas
  //////////

  /**
    * A schema constructor. `arg` should be a variable or a Hole.
    */
  case class SchemaConstructor(arg: SimpleType) extends SimpleType

  /**
    * An unextended schema.
    */
  case class Schema(fields: List[PredicateFieldType]) extends SimpleType

  /**
    * An extended schema. `arg` should be a variable or a Hole.
    */
  case class SchemaExtend(fields: List[PredicateFieldType], rest: SimpleType) extends SimpleType

  /**
    * An unextended schema row.
    */
  case class SchemaRow(fields: List[PredicateFieldType]) extends SimpleType

  /**
    * An extended schema row. `arg` should be a variable or a Hole.
    */
  case class SchemaRowExtend(fields: List[PredicateFieldType], rest: SimpleType) extends SimpleType

  ////////////////////
  // Boolean Operators
  ////////////////////

  /**
    * Boolean negation.
    */
  case class Not(tpe: SimpleType) extends SimpleType

  /**
    * A chain of types connected by `and`.
    */
  case class And(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `or`.
    */
  case class Or(tpes: List[SimpleType]) extends SimpleType

  ////////////////
  // Set Operators
  ////////////////

  /**
    * Set complement.
    */
  case class Complement(tpe: SimpleType) extends SimpleType

  /**
    * A chain of types in a set.
    */
  case class Union(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `&`.
    */
  case class Plus(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `&`.
    */
  case class Intersection(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `⊕`.
    */
  case class SymmetricDiff(tpes: List[SimpleType]) extends SimpleType

  /**
    * A chain of types connected by `-`.
    */
  case class Difference(tpes: List[SimpleType]) extends SimpleType

  /////////////
  // Predicates
  /////////////

  case object RelationConstructor extends SimpleType

  /**
    * A relation over a list of types.
    */
  case class Relation(tpes: List[SimpleType]) extends SimpleType

  case object LatticeConstructor extends SimpleType

  /**
    * A lattice over a list of types.
    */
  case class Lattice(tpes: List[SimpleType], lat: SimpleType) extends SimpleType

  ////////////
  // Functions
  ////////////

  /**
    * A pure function.
    */
  case class PureArrow(arg: SimpleType, ret: SimpleType) extends SimpleType

  /**
    * A function with a purity.
    */
  case class PolyArrow(arg: SimpleType, eff: SimpleType, ret: SimpleType) extends SimpleType

  /**
    * A backend function (no effect).
    */
  case class ArrowWithoutEffect(arg: SimpleType, ret: SimpleType) extends SimpleType

  ///////
  // Tags
  ///////

  case class TagConstructor(name: String) extends SimpleType

  ////////////
  // JVM Types
  ////////////

  case class JvmToType(tpe: SimpleType) extends SimpleType

  case class JvmToEff(tpe: SimpleType) extends SimpleType

  case class JvmUnresolvedConstructor(name: String, tpes: List[SimpleType]) extends SimpleType

  case class JvmUnresolvedField(tpe: SimpleType, name: String) extends SimpleType

  case class JvmUnresolvedMethod(tpe: SimpleType, name: String, tpes: List[SimpleType]) extends SimpleType

  case class JvmUnresolvedStaticMethod(clazz: String, name: String, tpes: List[SimpleType]) extends SimpleType

  case class JvmConstructor(constructor: Constructor[?]) extends SimpleType

  case class JvmField(field: Field) extends SimpleType

  case class JvmMethod(method: Method) extends SimpleType

  //////////////////////
  // Miscellaneous Types
  //////////////////////

  /**
    * A simple named type (e.g., enum or type alias).
    */
  case class Name(name: String) extends SimpleType

  /**
    * A type applied to one or more types.
    */
  case class Apply(tpe: SimpleType, tpes: List[SimpleType]) extends SimpleType

  /**
    * A type variable.
    */
  case class Var(id: Int, kind: Kind, text: VarText) extends SimpleType

  /**
    * A tuple.
    */
  case class Tuple(tpes: List[SimpleType]) extends SimpleType

  /**
    * A region.
    */
  case class Region(name: String) extends SimpleType

  /**
    * An error type.
    */
  case object Error extends SimpleType

  /////////
  // Fields
  /////////

  /**
    * A record label name and its type.
    */
  case class RecordLabelType(name: String, tpe: SimpleType)

  /**
    * A common supertype for schema predicates.
    */
  sealed trait PredicateFieldType {
    val name: String
  }

  /**
    * A relation field name and its types.
    */
  case class RelationFieldType(name: String, tpes: List[SimpleType]) extends PredicateFieldType

  /**
    * A lattice field name, its types, and its lattice.
    */
  case class LatticeFieldType(name: String, tpes: List[SimpleType], lat: SimpleType) extends PredicateFieldType

  /**
    * A predicate field type that's not actually a predicate.
    */
  case class NonPredFieldType(name: String, tpe: SimpleType) extends PredicateFieldType

  /**
    * Creates a simple type from the well-kinded type `t`.
    */
  def fromWellKindedType(t0: Type): SimpleType = {

    def visit(t: Type): SimpleType = t.baseType match {
      case Type.Var(sym, _) =>
        mkApply(Var(sym.id, sym.kind, sym.text), t.typeArguments.map(visit))
      case Type.Alias(cst, args, _, _) =>
        mkApply(Name(cst.sym.name), (args ++ t.typeArguments).map(visit))
      case Type.AssocType(cst, arg, _, _) =>
        mkApply(Name(cst.sym.name), (arg :: t.typeArguments).map(visit))
      case Type.JvmToType(tpe, _) =>
        mkApply(SimpleType.JvmToType(visit(tpe)), t.typeArguments.map(visit))
      case Type.JvmToEff(tpe, _) =>
        mkApply(SimpleType.JvmToEff(visit(tpe)), t.typeArguments.map(visit))
      case Type.UnresolvedJvmType(member, _) => member match {
        case JvmMember.JvmConstructor(clazz, tpes) => SimpleType.JvmUnresolvedConstructor(clazz.getSimpleName, tpes.map(visit))
        case JvmMember.JvmMethod(tpe, name, tpes) => SimpleType.JvmUnresolvedMethod(visit(tpe), name.name, tpes.map(visit))
        case JvmMember.JvmField(_, tpe, name) => SimpleType.JvmUnresolvedField(visit(tpe), name.name)
        case JvmMember.JvmStaticMethod(clazz, name, tpes) => SimpleType.JvmUnresolvedStaticMethod(clazz.getSimpleName, name.name, tpes.map(visit))
      }
      case Type.GetEff(action, tpe, _) =>
        val name = action match {
          case RegionAction.Alloc => "GetAlloc"
          case RegionAction.Read => "GetRead"
          case RegionAction.Write => "GetWrite"
        }
        Apply(SimpleType.Name(name), List(visit(tpe)))

      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Void => Void
        case TypeConstructor.AnyType => AnyType
        case TypeConstructor.Unit => Unit
        case TypeConstructor.Null => Null
        case TypeConstructor.Bool => Bool
        case TypeConstructor.Char => Char
        case TypeConstructor.Float32 => Float32
        case TypeConstructor.Float64 => Float64
        case TypeConstructor.BigDecimal => BigDecimal
        case TypeConstructor.Int8 => Int8
        case TypeConstructor.Int16 => Int16
        case TypeConstructor.Int32 => Int32
        case TypeConstructor.Int64 => Int64
        case TypeConstructor.BigInt => BigInt
        case TypeConstructor.Str => Str
        case TypeConstructor.Regex => Regex

        case TypeConstructor.Arrow(arity) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. Fill everything with a hole.
            case Nil =>
              val lastArrow: SimpleType = PolyArrow(Hole, Hole, Hole)
              // NB: safe to subtract 2 since arity is always at least 2
              List.fill(arity - 2)(Hole).foldRight(lastArrow)(PureArrow.apply)

            // Case 2: Pure function.
            case eff :: tpes if eff == Pure =>
              // NB: safe to reduce because arity is always at least 2
              tpes.padTo(arity, Hole).reduceRight(PureArrow.apply)

            // Case 3: Impure function.
            case eff :: tpes =>
              // NB: safe to take last 2 because arity is always at least 2
              val allTpes = tpes.padTo(arity, Hole)
              val List(lastArg, ret) = allTpes.takeRight(2)
              val lastArrow: SimpleType = PolyArrow(lastArg, eff, ret)
              allTpes.dropRight(2).foldRight(lastArrow)(PureArrow.apply)
          }

        case TypeConstructor.ArrowWithoutEffect(arity) =>
          val args = t.typeArguments.map(visit)
          args.padTo(arity, Hole).reduceRight(ArrowWithoutEffect.apply)

        case TypeConstructor.RecordRowEmpty => RecordRow(Nil)

        case TypeConstructor.RecordRowExtend(label) =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. ( name: ? | ? )
            case Nil => RecordRowExtend(RecordLabelType(label.name, Hole) :: Nil, Hole)
            // Case 2: One arg. ( name: tpe | ? )
            case tpe :: Nil => RecordRowExtend(RecordLabelType(label.name, tpe) :: Nil, Hole)
            // Case 3: Fully applied. Dispatch to proper record handler.
            case _ :: _ :: Nil => fromRecordRow(t)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(label.loc)
          }

        case TypeConstructor.Record =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. { ? }
            case Nil => RecordConstructor(Hole)
            // Case 2: One row argument. Extract its values.
            case tpe :: Nil => tpe match {
              case RecordRow(labels) => Record(labels)
              case RecordRowExtend(labels, rest) => RecordExtend(labels, rest)
              case nonRecord => RecordConstructor(nonRecord)
            }
            // Case 3: Too many args. Error.
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.SchemaRowEmpty => SchemaRow(Nil)

        case TypeConstructor.SchemaRowExtend(pred) =>
          // erase aliases over the Schema/Relation
          val args = mapHead(t.typeArguments, Type.eraseTopAliases).map(visit)
          args match {
            // Case 1: No args. #( Name(?) | ? )
            case Nil => SchemaRowExtend(RelationFieldType(pred.name, Hole :: Nil) :: Nil, Hole)
            // Case 2: One relation arg. #( Name(tpe1, tpe2) | ? )
            case Relation(tpes) :: Nil => SchemaRowExtend(RelationFieldType(pred.name, tpes) :: Nil, Hole)
            // Case 3: One lattice arg. #( Name(tpe1; tpe2) | ? )
            case Lattice(tpes, lat) :: Nil => SchemaRowExtend(LatticeFieldType(pred.name, tpes, lat) :: Nil, Hole)
            // Case 4: Some non-predicate type.
            case _ :: Nil => SchemaRowExtend(NonPredFieldType(pred.name, args.head) :: Nil, Hole)
            // Case 5: Fully applied. Dispatch to proper schema handler.
            case _ :: _ :: Nil => fromSchemaRow(t)
            // Case 6: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(pred.loc)
          }

        case TypeConstructor.Schema =>
          val args = t.typeArguments.map(visit)
          args match {
            // Case 1: No args. { ? }
            case Nil => SchemaConstructor(Hole)
            // Case 2: One row argument. Extract its values.
            case tpe :: Nil => tpe match {
              case SchemaRow(fields) => Schema(fields)
              case SchemaRowExtend(fields, rest) => SchemaExtend(fields, rest)
              case nonSchema => SchemaConstructor(nonSchema)
            }
            // Case 3: Too many args. Error.
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }
        case TypeConstructor.Array => mkApply(Array, t.typeArguments.map(visit))
        case TypeConstructor.ArrayWithoutRegion => mkApply(ArrayWithoutRegion, t.typeArguments.map(visit))
        case TypeConstructor.Vector => mkApply(Vector, t.typeArguments.map(visit))
        case TypeConstructor.Sender => mkApply(Sender, t.typeArguments.map(visit))
        case TypeConstructor.Receiver => mkApply(Receiver, t.typeArguments.map(visit))
        case TypeConstructor.Lazy => mkApply(Lazy, t.typeArguments.map(visit))
        case TypeConstructor.Enum(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Struct(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.RestrictableEnum(sym, _) => mkApply(Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Native(clazz) => mkApply(Name(clazz.getName), t.typeArguments.map(visit))
        case TypeConstructor.JvmConstructor(constructor) => mkApply(JvmConstructor(constructor), t.typeArguments.map(visit))
        case TypeConstructor.JvmMethod(method) => mkApply(JvmMethod(method), t.typeArguments.map(visit))
        case TypeConstructor.JvmField(field) => mkApply(JvmField(field), t.typeArguments.map(visit))
        case TypeConstructor.Tuple(l) =>
          val tpes = t.typeArguments.map(visit).padTo(l, Hole)
          Tuple(tpes)
        case TypeConstructor.Relation =>
          val args = t.typeArguments.map(visit)
          args match {
            case Nil => RelationConstructor
            case tpe :: Nil => Relation(destructTuple(tpe))
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }
        case TypeConstructor.Lattice =>
          val args = t.typeArguments.map(visit)
          args match {
            case Nil => LatticeConstructor
            case tpe :: Nil =>
              val tpesAndLat = destructTuple(tpe)
              // NB: safe to take init/last since every lattice has a lattice field
              // MATT not safe in case of alias!
              val tpes = tpesAndLat.init
              val lat = tpesAndLat.last
              Lattice(tpes, lat)
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }
        case TypeConstructor.Pure => Pure
        case TypeConstructor.Univ => Univ

        case TypeConstructor.True => True
        case TypeConstructor.False => False

        case TypeConstructor.And =>
          // collapse into a chain of ands
          t.typeArguments.map(visit).map(splitAnds) match {
            // Case 1: No args. ? and ?
            case Nil => And(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 and tpe2 and ?
            case args :: Nil => And(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 and tpe2 and tpe3 and tpe4
            case args1 :: args2 :: Nil => And(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Or =>
          // collapse into a chain of ors
          t.typeArguments.map(visit).map(splitOrs) match {
            // Case 1: No args. ? or ?
            case Nil => Or(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 or tpe2 or ?
            case args :: Nil => Or(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 or tpe2 or tpe3 or tpe4
            case args1 :: args2 :: Nil => Or(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Not => SimpleType.Not(visit(t.typeArguments.head))

        case TypeConstructor.Complement =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Union =>
          // collapse into a chain of pluses
          t.typeArguments.map(visit).map(splitPluses) match {
            // Case 1: No args. ? + ?
            case Nil => Plus(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 + tpe2 + ?
            case args :: Nil => Plus(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 + tpe2 + tpe3 + tpe4
            case args1 :: args2 :: Nil => Plus(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Intersection =>
          // collapse into a chain of intersections
          t.typeArguments.map(visit).map(splitIntersections) match {
            // Case 1: No args. ? & ?
            case Nil => Intersection(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 & tpe2 & ?
            case args :: Nil => Intersection(args :+ Hole)
            // Case 3: Multiple args. Concatenate them: tpe1 & tpe2 & tpe3 & tpe4
            case args1 :: args2 :: Nil => Intersection(args1 ++ args2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Difference =>
          // collapse into a chain of differences
          // take care not to change A - (B - C) into A - B - C
          t.typeArguments.map(visit) match {
            // Case 1: No args. ? - ?
            case Nil => Difference(Hole :: Hole :: Nil)
            // Case 2: One arg. Take the left and put a hole at the end: tpe1 - tpe2 - ?
            case arg :: Nil => Difference(splitDifference(arg) :+ Hole)
            // Case 3: Multiple args. Concatenate the left differences: tpe1 - tpe2 - tpe3 - tpe4
            case arg1 :: arg2 :: Nil => Difference(splitDifference(arg1) :+ arg2)
            // Case 4: Too many args. Error.
            case _ :: _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.SymmetricDiff =>
          val args = t.typeArguments.map(visit)
          SymmetricDiff(args)

        case TypeConstructor.CaseSet(syms, _) =>
          val names = syms.toList.map(sym => SimpleType.Name(sym.name))
          val set = SimpleType.Union(names)
          mkApply(set, t.typeArguments.map(visit))

        case TypeConstructor.CaseComplement(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Complement(Hole)
            case arg :: Nil => Complement(arg)
            case _ :: _ :: _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.CaseIntersection(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Intersection(Hole :: Hole :: Nil)
            case arg :: Nil => Intersection(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Intersection(arg1 :: arg2 :: Nil)
            case _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.CaseUnion(_) =>
          t.typeArguments.map(visit) match {
            case Nil => Plus(Hole :: Hole :: Nil)
            case arg :: Nil => Plus(arg :: Hole :: Nil)
            case arg1 :: arg2 :: Nil => Plus(arg1 :: arg2 :: Nil)
            case _ => throw new OverAppliedType(t.loc)
          }

        case TypeConstructor.Effect(sym) => mkApply(SimpleType.Name(sym.name), t.typeArguments.map(visit))
        case TypeConstructor.Region(sym) => mkApply(SimpleType.Region(sym.text), t.typeArguments.map(visit))
        case TypeConstructor.RegionToStar => mkApply(RegionToStar, t.typeArguments.map(visit))
        case TypeConstructor.RegionToEff(action) => mkApply(SimpleType.Name(action.map(_.toString).getOrElse("Heap")), t.typeArguments.map(visit))
        case TypeConstructor.RegionWithoutRegion => mkApply(RegionWithoutRegion, t.typeArguments.map(visit))
        case TypeConstructor.GenericRegion(prop) => mkApply(SimpleType.Name("GenericRegion"), t.typeArguments.map(visit))

        case TypeConstructor.Error(_, _) => SimpleType.Error
      }
    }

    visit(t0)
  }

  /**
    * Builds an Apply type.
    */
  private def mkApply(base: SimpleType, args: List[SimpleType]): SimpleType = args match {
    case Nil => base
    case _ :: _ => Apply(base, args)
  }

  /**
    * Extracts the types from a tuple, treating non-tuples as singletons.
    */
  private def destructTuple(tpe: SimpleType): List[SimpleType] = tpe match {
    case Tuple(fields) => fields
    case Unit => Nil
    case t => t :: Nil
  }

  /**
    * Transforms the given type, assuming it is a record row.
    */
  private def fromRecordRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // Case 1: A fully applied record row.
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(name), _), tpe, _), rest, _) =>
        val labelType = RecordLabelType(name.name, fromWellKindedType(tpe))
        visit(rest) match {
          // Case 1.1: Unextended row. Put the labels together.
          case SimpleType.RecordRow(labels) => SimpleType.RecordRow(labelType :: labels)
          // Case 1.2: Extended row. Put the labels together.
          case SimpleType.RecordRowExtend(labels, restOfRest) => SimpleType.RecordRowExtend(labelType :: labels, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonRecord => SimpleType.RecordRowExtend(labelType :: Nil, nonRecord)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) => SimpleType.RecordRow(Nil)
      // Case 3: Non-row.
      case nonRecord => fromWellKindedType(nonRecord)
    }

    // sort the labels after converting
    visit(row0) match {
      case RecordRowExtend(labels, rest) => RecordRowExtend(labels.sortBy(_.name), rest)
      case RecordRow(labels) => RecordRow(labels.sortBy(_.name))
      case nonRecord => nonRecord
    }
  }

  /**
    * Transforms the given type, assuming it is a schema row.
    */
  private def fromSchemaRow(row0: Type): SimpleType = {
    def visit(row: Type): SimpleType = row match {
      // Case 1: A fully applied record row.
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(name), _), tpe, _), rest, _) =>
        // create the right field/type for the field
        val fieldType = fromWellKindedType(Type.eraseTopAliases(tpe)) match {
          case Relation(tpes) => RelationFieldType(name.name, tpes)
          case Lattice(tpes, lat) => LatticeFieldType(name.name, tpes, lat)
          // If it's not a relation or lattice, keep any aliases.
          case _ => NonPredFieldType(name.name, fromWellKindedType(tpe))
        }
        visit(rest) match {
          // Case 1.1: Unextended row. Put the fields together.
          case SimpleType.SchemaRow(fields) => SimpleType.SchemaRow(fieldType :: fields)
          // Case 1.2: Extended row. Put the fields together.
          case SimpleType.SchemaRowExtend(fields, restOfRest) => SimpleType.SchemaRowExtend(fieldType :: fields, restOfRest)
          // Case 1.3: Non-row. Put it in the "rest" position.
          case nonSchema => SimpleType.SchemaRowExtend(fieldType :: Nil, nonSchema)
        }
      // Case 2: Empty record row.
      case Type.Cst(TypeConstructor.SchemaRowEmpty, _) => SimpleType.SchemaRow(Nil)
      // Case 3: Non-row.
      case nonSchema => fromWellKindedType(nonSchema)
    }

    // sort the fields after converting
    visit(row0) match {
      case SchemaRow(fields) => SchemaRow(fields.sortBy(_.name))
      case SchemaRowExtend(fields, rest) => SchemaRowExtend(fields.sortBy(_.name), rest)
      case nonSchema => nonSchema
    }
  }

  /**
    * Splits `t1 and t2` into `t1 :: t2 :: Nil`,
    * and leaves non-and types as singletons.
    */
  private def splitAnds(tpe: SimpleType): List[SimpleType] = tpe match {
    case And(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 or t2` into `t1 :: t2 :: Nil`,
    * and leaves non-or types as singletons.
    */
  private def splitOrs(tpe: SimpleType): List[SimpleType] = tpe match {
    case Or(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 + t2` into `t1 :: t2 :: Nil`,
    * and leaves non-plus types as singletons.
    */
  private def splitPluses(tpe: SimpleType): List[SimpleType] = tpe match {
    case Plus(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 & t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitIntersections(tpe: SimpleType): List[SimpleType] = tpe match {
    case Intersection(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 - t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitDifference(tpe: SimpleType): List[SimpleType] = tpe match {
    case Difference(tpes) => tpes
    case t => List(t)
  }

  /**
    * Splits `t1 ⊕ t2` into `t1 :: t2 :: Nil`,
    * and leaves other types as singletons.
    */
  private def splitSymmetricDiff(tpe: SimpleType): List[SimpleType] = tpe match {
    case SymmetricDiff(tpes) => tpes
    case t => List(t)
  }

  /**
    * Map over the first element in the list, if it exists.
    */
  private def mapHead[A](l: List[A], f: A => A): List[A] = l match {
    case Nil => Nil
    case hd :: tl => f(hd) :: tl
  }
}
