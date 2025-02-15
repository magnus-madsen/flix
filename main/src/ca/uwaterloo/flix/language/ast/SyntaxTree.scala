/*
 * Copyright 2024 Herluf Baggesen
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.Source

/**
 * Represents the source code of a compilation unit.
 *
 * A [[SyntaxTree]] is unstructured: it allows much more flexibility than later
 * abstract syntax trees. This flexibility is used to capture source code that may
 * contain faulty syntax. The tree has nodes that hold a [[TreeKind]] and zero or
 * more children. Each child is either a [[Child.TokenChild]] or a [[Child.TreeChild]].
 *
 * Note that [[SyntaxTree]] offers few guarantees. In particular:
 *   - There is no guarantee that a specific node is present or absent as a child.
 *   - There is no guarantee that a specific node has a specific number of children.
 */
object SyntaxTree {

  /**
    * A root containing syntax trees for multiple sources.
    */
  case class Root(units: Map[Source, Tree], tokens: Map[Source, Array[Token]])

  /**
    * The empty SyntaxTree
    */
  val empty: Root = Root(Map.empty, Map.empty)

  /**
    * A marker trait for a child node in a syntax tree.
    * In practice this is implemented by [[Tree]] and [[Token]].
    */
  trait Child

  /**
   * A node in a [[SyntaxTree]]
   *
   * @param kind     The kind of the node.
   * @param loc      The location that the node spans in the source file.
   * @param children The children of the node.
   */
  case class Tree(kind: TreeKind, var children: Array[Child], var loc: SourceLocation) extends Child


  /**
   * A common super-type for [[TreeKind]]s
   */
  sealed trait TreeKind

  /**
   * Different kinds of syntax nodes in a [[SyntaxTree]].
   *
   * The only error kind that holds data is the special [[TreeKind.ErrorTree]].
   */
  object TreeKind {
    /**
     * A special error kind wrapping a [[CompilationMessage]].
     */
    case class ErrorTree(error: CompilationMessage) extends TreeKind

    /**
      * A special [[TreeKind]] used as a placeholder when a new mark is opened and the actual kind is not yet known.
      * [[UnclosedMark]] always gets overwritten with another [[TreeKind]] as parsing happens.
      * Failure to do so is a compiler error and gets caught when building the syntax tree.
      */
    case object UnclosedMark extends TreeKind

    case object AnnotationList extends TreeKind

    case object Argument extends TreeKind

    case object ArgumentNamed extends TreeKind

    case object ArgumentList extends TreeKind

    case object Case extends TreeKind

    case object CommentList extends TreeKind

    case object DerivationList extends Type

    case object Doc extends TreeKind

    case object Ident extends TreeKind

    case object Kind extends TreeKind

    case object ModifierList extends TreeKind

    case object Operator extends TreeKind

    case object Parameter extends TreeKind

    case object ParameterList extends TreeKind

    case object QName extends TreeKind

    case object RegionTag extends TreeKind

    case object Root extends TreeKind

    case object StructField extends TreeKind

    case object TrailingDot extends TreeKind

    case object TypeParameter extends TreeKind

    case object TypeParameterList extends TreeKind

    //////////////////////////////////////////////////////////////////////////////////////////
    // DECLARATIONS                                                                         //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Decl extends TreeKind

    object Decl {
      case object AssociatedTypeDef extends Decl

      case object AssociatedTypeSig extends Decl

      case object Trait extends Decl

      case object Def extends Decl

      case object Redef extends Decl

      case object Effect extends Decl

      case object Enum extends Decl

      case object EqualityConstraintList extends Decl

      case object EqualityConstraintFragment extends Decl

      case object Instance extends Decl

      case object Law extends Decl

      case object Module extends Decl

      case object Op extends Decl

      case object RestrictableEnum extends Decl

      case object Signature extends Decl

      case object Struct extends Decl

      case object TypeAlias extends Decl
    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS                                                                         //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Expr extends TreeKind

    object Expr {

      /**
       * A marker kind used to wrap nested expressions.
       */
      // For instance on a binary expression "1 + 2" you would do
      // Expr
      //   Binary
      //     Expr
      //       LiteralNumber
      //   Operator
      //     Expr
      //       LiteralNumber
      case object Expr extends Expr

      case object Apply extends Expr

      case object Ascribe extends Expr

      case object Binary extends Expr

      case object Block extends Expr

      case object CheckedEffectCast extends Expr

      case object CheckedTypeCast extends Expr

      case object Index extends Expr

      case object IndexMut extends Expr

      case object InvokeConstructor extends Expr

      case object InvokeMethod extends Expr

      case object Debug extends Expr

      case object FixpointConstraint extends Expr

      case object FixpointConstraintSet extends Expr

      case object FixpointLambda extends Expr

      case object FixpointFromFragment extends Expr

      case object FixpointInject extends Expr

      case object FixpointQuery extends Expr

      case object FixpointSelect extends Expr

      case object FixpointSolveWithProject extends Expr

      case object FixpointWhere extends Expr

      case object ForApplicative extends Expr

      case object Foreach extends Expr

      case object ForeachYield extends Expr

      case object ForMonadic extends Expr

      case object ForFragmentGenerator extends Expr

      case object ForFragmentGuard extends Expr

      case object ForFragmentLet extends Expr

      case object GetField extends Expr

      case object Handler extends Expr

      case object Hole extends Expr

      case object HoleVariable extends Expr

      case object IfThenElse extends Expr

      case object InstanceOf extends Expr

      case object Intrinsic extends Expr

      case object JvmMethod extends Expr

      case object Lambda extends Expr

      case object LambdaMatch extends Expr

      case object LetMatch extends Expr

      case object Literal extends Expr

      case object LiteralArray extends Expr

      case object LiteralList extends Expr

      case object LiteralMap extends Expr

      case object LiteralMapKeyValueFragment extends Expr

      case object LiteralRecord extends Expr

      case object LiteralRecordFieldFragment extends Expr

      case object LiteralStructFieldFragment extends Expr

      case object LiteralSet extends Expr

      case object LiteralVector extends Expr

      case object LocalDef extends Expr

      case object Match extends Expr

      case object MatchRuleFragment extends Expr

      case object NewObject extends Expr

      case object NewStruct extends Expr

      case object StructGet extends Expr

      case object StructPut extends Expr

      case object StructPutRHS extends Expr

      case object OpenVariant extends Expr

      case object OpenVariantAs extends Expr

      case object Paren extends Expr

      case object ParYield extends Expr

      case object ParYieldFragment extends Expr

      case object RecordOperation extends Expr

      case object RecordOpExtend extends Expr

      case object RecordOpRestrict extends Expr

      case object RecordOpUpdate extends Expr

      case object RecordSelect extends Expr

      case object RestrictableChoose extends Expr

      case object RestrictableChooseStar extends Expr

      case object Run extends Expr

      case object Scope extends Expr

      case object ScopeName extends Expr

      case object Select extends Expr

      case object SelectRuleFragment extends Expr

      case object SelectRuleDefaultFragment extends Expr

      case object Spawn extends Expr

      case object Statement extends Expr

      case object Static extends Expr

      case object StringInterpolation extends Expr

      case object Try extends Expr

      case object Throw extends Expr

      case object TryCatchBodyFragment extends Expr

      case object TryCatchRuleFragment extends Expr

      case object RunWithBodyExpr extends Expr

      case object TryWithRuleFragment extends Expr

      case object Tuple extends Expr

      case object TypeMatch extends Expr

      case object TypeMatchRuleFragment extends Expr

      case object Unary extends Expr

      case object UncheckedCast extends Expr

      case object Unsafe extends Expr

      case object UnsafeOld extends Expr

      case object Use extends Expr

      case object Without extends Expr

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// TYPES                                                                               //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Type extends TreeKind

    object Type {
      /**
       * A marker kind used to wrap nested types.
       */
      // For instance on a tuple type "(Int32, Bool)" you would do
      // Type
      //   Tuple
      //     Type
      //       Ident
      //     Type
      //       Ident
      case object Type extends Type

      case object Apply extends Type

      case object Argument extends Type

      case object ArgumentList extends Type

      case object Ascribe extends Type

      case object Binary extends Type

      case object CaseSet extends Type

      case object Constant extends Type

      case object Constraint extends Type

      case object ConstraintList extends Type

      case object Effect extends Type

      case object EffectSet extends Type

      case object Function extends Type

      case object PredicateWithAlias extends Type

      case object PredicateWithTypes extends Type

      case object Record extends Type

      case object RecordFieldFragment extends Type

      case object RecordRow extends Type

      case object Schema extends Type

      case object SchemaRow extends Type

      case object Tuple extends Type

      case object Unary extends Type

      case object Variable extends Type

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// PATTERNS                                                                            //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Pattern extends TreeKind

    object Pattern {
      /**
       * A marker kind used to wrap nested patterns.
       */
      // For instance on cons pattern "0 :: xs" you would do
      // Pattern
      //   FCons
      //     Pattern
      //       Literal
      //     Pattern
      //       Ident
      case object Pattern extends Pattern

      case object FCons extends Pattern

      case object Literal extends Pattern

      case object Record extends Pattern

      case object RecordFieldFragment extends Pattern

      case object Tag extends Pattern

      case object Tuple extends Pattern

      case object Variable extends Pattern

      case object Unary extends Pattern

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// PREDICATES                                                                          //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait Predicate extends TreeKind

    object Predicate {

      case object Atom extends Predicate

      case object Body extends Predicate

      case object Functional extends Predicate

      case object Guard extends Predicate

      case object Head extends Predicate

      case object LatticeTerm extends Predicate

      case object PatternList extends Predicate

      case object ParamList extends  Predicate

      case object Param extends Predicate

      case object ParamUntyped extends Predicate

      case object TermList extends Predicate

    }

    //////////////////////////////////////////////////////////////////////////////////////////
    /// IMPORTS                                                                             //
    //////////////////////////////////////////////////////////////////////////////////////////
    sealed trait UsesOrImports extends TreeKind

    object UsesOrImports {
      case object Alias extends UsesOrImports

      case object Import extends UsesOrImports

      case object ImportMany extends UsesOrImports

      case object Use extends UsesOrImports

      case object UseMany extends UsesOrImports

      case object UseOrImportList extends UsesOrImports
    }
  }
}
