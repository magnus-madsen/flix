/*
 * Copyright 2023 Herluf Baggesen
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

import ca.uwaterloo.flix.language.errors.LexerError

sealed trait TokenKind {
  /**
    * A string representation of the [[TokenKind]] suitable for printing.
    * For instance TokenKind.Ampersand.display gives "'&'".
    */
  def display: String = {
    this match {
      case TokenKind.Ampersand => "'&'"
      case TokenKind.AngledEqual => "'<=>'"
      case TokenKind.AngledPlus => "'<+>'"
      case TokenKind.AngleL => "'<'"
      case TokenKind.AngleLEqual => "'<='"
      case TokenKind.AngleR => "'>'"
      case TokenKind.AngleREqual => "'>='"
      case TokenKind.ArrayHash => "'Array#'"
      case TokenKind.ArrowThickR => "'=>'"
      case TokenKind.ArrowThinL => "'<-'"
      case TokenKind.ArrowThinR => "'->'"
      case TokenKind.At => "'@'"
      case TokenKind.Backslash => "'\\'"
      case TokenKind.Bang => "'!'"
      case TokenKind.BangEqual => "'!='"
      case TokenKind.Bar => "'|'"
      case TokenKind.BracketL => "'['"
      case TokenKind.BracketR => "']'"
      case TokenKind.Caret => "'^'"
      case TokenKind.Colon => "':'"
      case TokenKind.ColonColon => "'::'"
      case TokenKind.ColonEqual => "':='"
      case TokenKind.ColonMinus => "':-'"
      case TokenKind.Comma => "','"
      case TokenKind.CurlyL => "'{'"
      case TokenKind.CurlyR => "'}'"
      case TokenKind.Dollar => "'$'"
      case TokenKind.Dot => "'.'"
      case TokenKind.DotDotDot => "'...'"
      case TokenKind.DotWhiteSpace => "'. '"
      case TokenKind.DotCurlyL => "'.{'"
      case TokenKind.Equal => "'='"
      case TokenKind.EqualEqual => "'=='"
      case TokenKind.Hash => "'#'"
      case TokenKind.HashCurlyL => "'#{'"
      case TokenKind.HashParenL => "'#('"
      case TokenKind.HoleAnonymous => "'???'"
      case TokenKind.KeywordAlias => "'alias'"
      case TokenKind.KeywordAnd => "'and'"
      case TokenKind.KeywordAs => "'as'"
      case TokenKind.KeywordCase => "'case'"
      case TokenKind.KeywordCatch => "'catch'"
      case TokenKind.KeywordCheckedCast => "'checked_cast'"
      case TokenKind.KeywordCheckedECast => "'checked_ecast'"
      case TokenKind.KeywordChoose => "'choose'"
      case TokenKind.KeywordChooseStar => "'choose*'"
      case TokenKind.KeywordDebug => "'dbg'"
      case TokenKind.KeywordDebugBang => "'dbg!'"
      case TokenKind.KeywordDebugBangBang => "'dbg!!'"
      case TokenKind.KeywordDef => "'def'"
      case TokenKind.KeywordDiscard => "'discard'"
      case TokenKind.KeywordEff => "'eff'"
      case TokenKind.KeywordElse => "'else'"
      case TokenKind.KeywordEnum => "'enum'"
      case TokenKind.KeywordFalse => "'false'"
      case TokenKind.KeywordFix => "'fix'"
      case TokenKind.KeywordForA => "'forA'"
      case TokenKind.KeywordForall => "'forall'"
      case TokenKind.KeywordForce => "'force'"
      case TokenKind.KeywordForeach => "'foreach'"
      case TokenKind.KeywordForM => "'forM'"
      case TokenKind.KeywordFrom => "'from'"
      case TokenKind.KeywordHandler => "'handler'"
      case TokenKind.KeywordIf => "'if'"
      case TokenKind.KeywordImport => "'import'"
      case TokenKind.KeywordInject => "'inject'"
      case TokenKind.KeywordInline => "'inline'"
      case TokenKind.KeywordInstance => "'instance'"
      case TokenKind.KeywordInstanceOf => "'instanceof'"
      case TokenKind.KeywordInto => "'into'"
      case TokenKind.KeywordLaw => "'law'"
      case TokenKind.KeywordLawful => "'lawful'"
      case TokenKind.KeywordLazy => "'lazy'"
      case TokenKind.KeywordLet => "'let'"
      case TokenKind.KeywordMatch => "'match'"
      case TokenKind.KeywordMod => "'mod'"
      case TokenKind.KeywordMut => "'mut'"
      case TokenKind.KeywordNew => "'new'"
      case TokenKind.KeywordNot => "'not'"
      case TokenKind.KeywordNull => "'null'"
      case TokenKind.KeywordOpenVariant => "'open_variant'"
      case TokenKind.KeywordOpenVariantAs => "'open_variant_as'"
      case TokenKind.KeywordOr => "'or'"
      case TokenKind.KeywordOverride => "'override'"
      case TokenKind.KeywordPar => "'par'"
      case TokenKind.KeywordPub => "'pub'"
      case TokenKind.KeywordProject => "'project'"
      case TokenKind.KeywordQuery => "'query'"
      case TokenKind.KeywordRedef => "'redef'"
      case TokenKind.KeywordRegion => "'region'"
      case TokenKind.RegionTag => "'region tag'"
      case TokenKind.KeywordRestrictable => "'restrictable'"
      case TokenKind.KeywordRun => "'run'"
      case TokenKind.KeywordRvadd => "'rvadd'"
      case TokenKind.KeywordRvand => "'rvand'"
      case TokenKind.KeywordRvnot => "'rvnot'"
      case TokenKind.KeywordRvsub => "'rvsub'"
      case TokenKind.KeywordSealed => "'sealed'"
      case TokenKind.KeywordSelect => "'select'"
      case TokenKind.KeywordSolve => "'solve'"
      case TokenKind.KeywordSpawn => "'spawn'"
      case TokenKind.KeywordStatic => "'static'"
      case TokenKind.KeywordStaticUppercase => "'Static'"
      case TokenKind.KeywordStruct => "'struct'"
      case TokenKind.KeywordThrow => "'throw'"
      case TokenKind.KeywordTrait => "'trait'"
      case TokenKind.KeywordTrue => "'true'"
      case TokenKind.KeywordTry => "'try'"
      case TokenKind.KeywordType => "'type'"
      case TokenKind.KeywordTypeMatch => "'typematch'"
      case TokenKind.KeywordUncheckedCast => "'unchecked_cast'"
      case TokenKind.KeywordUniv => "'univ'"
      case TokenKind.KeywordUnsafe => "'unsafe'"
      case TokenKind.KeywordUnsafely => "'unsafely'"
      case TokenKind.KeywordUse => "'use'"
      case TokenKind.KeywordWhere => "'where'"
      case TokenKind.KeywordWith => "'with'"
      case TokenKind.KeywordWithout => "'without'"
      case TokenKind.KeywordYield => "'yield'"
      case TokenKind.KeywordXor => "'xor'"
      case TokenKind.ListHash => "'List#'"
      case TokenKind.MapHash => "'Map#'"
      case TokenKind.Minus => "'-'"
      case TokenKind.ParenL => "'('"
      case TokenKind.ParenR => "')'"
      case TokenKind.Plus => "'+'"
      case TokenKind.Semi => "';'"
      case TokenKind.SetHash => "'Set#'"
      case TokenKind.Slash => "'/'"
      case TokenKind.Star => "'*'"
      case TokenKind.StarStar => "'**'"
      case TokenKind.StructArrow => "'->'"
      case TokenKind.Tilde => "'~'"
      case TokenKind.TripleAmpersand => "'&&&'"
      case TokenKind.TripleAngleL => "'<<<'"
      case TokenKind.TripleAngleR => "'>>>'"
      case TokenKind.TripleBar => "'|||'"
      case TokenKind.TripleCaret => "'^^^'"
      case TokenKind.TripleColon => "':::'"
      case TokenKind.TripleTilde => "'~~~'"
      case TokenKind.Underscore => "'_'"
      case TokenKind.VectorHash => "'Vector#'"
      case TokenKind.Eof => "<end-of-file>"
      case TokenKind.NameLowerCase => "<name>"
      case TokenKind.NameUpperCase => "<Name>"
      case TokenKind.NameMath => "<math name>"
      case TokenKind.NameGreek => "<greek name>"
      case TokenKind.UserDefinedOperator => "<user-defined operator>"
      case TokenKind.Annotation => "<annotation>"
      case TokenKind.BuiltIn => "<built in>"
      case TokenKind.CommentBlock => "<block comment>"
      case TokenKind.CommentDoc => "<doc comment>"
      case TokenKind.CommentLine => "<comment>"
      case TokenKind.HoleNamed => "<named hole>"
      case TokenKind.HoleVariable => "<variable hole>"
      case TokenKind.InfixFunction => "<infix function>"
      case TokenKind.LiteralBigDecimal => "'<digits>ff'"
      case TokenKind.LiteralBigInt => "'<digits>ii'"
      case TokenKind.LiteralDebugStringL => "'%{'"
      case TokenKind.LiteralDebugStringR => "'}'"
      case TokenKind.LiteralFloat32 => "'<digits>f32'"
      case TokenKind.LiteralFloat64 => "'<digits>f64'"
      case TokenKind.LiteralInt8 => "'<digits>i8'"
      case TokenKind.LiteralInt16 => "'<digits>i16'"
      case TokenKind.LiteralInt32 => "'<digits>i32'"
      case TokenKind.LiteralInt64 => "'<digits>i64'"
      case TokenKind.LiteralRegex => "<regex>"
      case TokenKind.LiteralString => "<string>"
      case TokenKind.LiteralChar => "<char>"
      case TokenKind.LiteralStringInterpolationL => "'${'"
      case TokenKind.LiteralStringInterpolationR => "'}\"'"
      case TokenKind.Err(_) => "<error>"
    }
  }

  /**
    * Checks if this token is a line or block comment.
    */
  def isCommentNonDoc: Boolean = this match {
    case TokenKind.CommentLine | TokenKind.CommentBlock => true
    case _ => false
  }

  /**
    * Checks if this token is a doc, line or block comment.
    */
  def isComment: Boolean = this == TokenKind.CommentDoc || this.isCommentNonDoc

  /**
    * Checks if this token is a keyword.
    */
  def isKeyword: Boolean = this match {
    case TokenKind.KeywordAlias => true
    case TokenKind.KeywordAnd => true
    case TokenKind.KeywordAs => true
    case TokenKind.KeywordCase => true
    case TokenKind.KeywordCatch => true
    case TokenKind.KeywordCheckedCast => true
    case TokenKind.KeywordCheckedECast => true
    case TokenKind.KeywordChoose => true
    case TokenKind.KeywordChooseStar => true
    case TokenKind.KeywordDebug => true
    case TokenKind.KeywordDebugBang => true
    case TokenKind.KeywordDebugBangBang => true
    case TokenKind.KeywordDef => true
    case TokenKind.KeywordDiscard => true
    case TokenKind.KeywordEff => true
    case TokenKind.KeywordElse => true
    case TokenKind.KeywordEnum => true
    case TokenKind.KeywordFalse => true
    case TokenKind.KeywordFix => true
    case TokenKind.KeywordForA => true
    case TokenKind.KeywordForall => true
    case TokenKind.KeywordForce => true
    case TokenKind.KeywordForeach => true
    case TokenKind.KeywordForM => true
    case TokenKind.KeywordFrom => true
    case TokenKind.KeywordHandler => true
    case TokenKind.KeywordIf => true
    case TokenKind.KeywordImport => true
    case TokenKind.KeywordInject => true
    case TokenKind.KeywordInline => true
    case TokenKind.KeywordInstance => true
    case TokenKind.KeywordInstanceOf => true
    case TokenKind.KeywordInto => true
    case TokenKind.KeywordLaw => true
    case TokenKind.KeywordLawful => true
    case TokenKind.KeywordLazy => true
    case TokenKind.KeywordLet => true
    case TokenKind.KeywordMatch => true
    case TokenKind.KeywordMod => true
    case TokenKind.KeywordNew => true
    case TokenKind.KeywordNot => true
    case TokenKind.KeywordNull => true
    case TokenKind.KeywordOpenVariant => true
    case TokenKind.KeywordOpenVariantAs => true
    case TokenKind.KeywordOr => true
    case TokenKind.KeywordOverride => true
    case TokenKind.KeywordPar => true
    case TokenKind.KeywordPub => true
    case TokenKind.KeywordProject => true
    case TokenKind.KeywordQuery => true
    case TokenKind.KeywordRegion => true
    case TokenKind.KeywordRestrictable => true
    case TokenKind.KeywordRun => true
    case TokenKind.KeywordRvadd => true
    case TokenKind.KeywordRvand => true
    case TokenKind.KeywordRvnot => true
    case TokenKind.KeywordRvsub => true
    case TokenKind.KeywordSealed => true
    case TokenKind.KeywordSelect => true
    case TokenKind.KeywordSolve => true
    case TokenKind.KeywordSpawn => true
    case TokenKind.KeywordStatic => true
    case TokenKind.KeywordStruct => true
    case TokenKind.KeywordThrow => true
    case TokenKind.KeywordTrait => true
    case TokenKind.KeywordTrue => true
    case TokenKind.KeywordTry => true
    case TokenKind.KeywordType => true
    case TokenKind.KeywordTypeMatch => true
    case TokenKind.KeywordUncheckedCast => true
    case TokenKind.KeywordUniv => true
    case TokenKind.KeywordUnsafe => true
    case TokenKind.KeywordUnsafely => true
    case TokenKind.KeywordUse => true
    case TokenKind.KeywordWhere => true
    case TokenKind.KeywordWith => true
    case TokenKind.KeywordWithout => true
    case TokenKind.KeywordYield => true
    case TokenKind.KeywordXor => true
    case TokenKind.Ampersand
         | TokenKind.AngleL
         | TokenKind.AngleLEqual
         | TokenKind.AngleR
         | TokenKind.AngleREqual
         | TokenKind.AngledEqual
         | TokenKind.AngledPlus
         | TokenKind.Annotation
         | TokenKind.ArrayHash
         | TokenKind.ArrowThickR
         | TokenKind.ArrowThinL
         | TokenKind.ArrowThinR
         | TokenKind.At
         | TokenKind.Backslash
         | TokenKind.Bang
         | TokenKind.BangEqual
         | TokenKind.Bar
         | TokenKind.BracketL
         | TokenKind.BracketR
         | TokenKind.BuiltIn
         | TokenKind.Caret
         | TokenKind.Colon
         | TokenKind.ColonColon
         | TokenKind.ColonEqual
         | TokenKind.ColonMinus
         | TokenKind.Comma
         | TokenKind.CommentBlock
         | TokenKind.CommentDoc
         | TokenKind.CommentLine
         | TokenKind.CurlyL
         | TokenKind.CurlyR
         | TokenKind.Dollar
         | TokenKind.Dot
         | TokenKind.DotCurlyL
         | TokenKind.DotDotDot
         | TokenKind.DotWhiteSpace
         | TokenKind.Eof
         | TokenKind.Equal
         | TokenKind.EqualEqual
         | TokenKind.Err(_)
         | TokenKind.Hash
         | TokenKind.HashCurlyL
         | TokenKind.HashParenL
         | TokenKind.HoleAnonymous
         | TokenKind.HoleNamed
         | TokenKind.HoleVariable
         | TokenKind.InfixFunction
         | TokenKind.KeywordMut
         | TokenKind.KeywordRedef
         | TokenKind.KeywordStaticUppercase
         | TokenKind.ListHash
         | TokenKind.LiteralBigDecimal
         | TokenKind.LiteralBigInt
         | TokenKind.LiteralChar
         | TokenKind.LiteralDebugStringL
         | TokenKind.LiteralDebugStringR
         | TokenKind.LiteralFloat32
         | TokenKind.LiteralFloat64
         | TokenKind.LiteralInt16
         | TokenKind.LiteralInt32
         | TokenKind.LiteralInt64
         | TokenKind.LiteralInt8
         | TokenKind.LiteralRegex
         | TokenKind.LiteralString
         | TokenKind.LiteralStringInterpolationL
         | TokenKind.LiteralStringInterpolationR
         | TokenKind.MapHash
         | TokenKind.Minus
         | TokenKind.NameGreek
         | TokenKind.NameLowerCase
         | TokenKind.NameMath
         | TokenKind.NameUpperCase
         | TokenKind.ParenL
         | TokenKind.ParenR
         | TokenKind.Plus
         | TokenKind.RegionTag
         | TokenKind.Semi
         | TokenKind.SetHash
         | TokenKind.Slash
         | TokenKind.Star
         | TokenKind.StarStar
         | TokenKind.StructArrow
         | TokenKind.Tilde
         | TokenKind.TripleAmpersand
         | TokenKind.TripleAngleL
         | TokenKind.TripleAngleR
         | TokenKind.TripleBar
         | TokenKind.TripleCaret
         | TokenKind.TripleColon
         | TokenKind.TripleTilde
         | TokenKind.Underscore
         | TokenKind.UserDefinedOperator
         | TokenKind.VectorHash => false
  }

  /**
    * Checks if this token is a modifier.
    */
  def isModifier: Boolean = this match {
    case TokenKind.KeywordSealed
         | TokenKind.KeywordLawful
         | TokenKind.KeywordPub
         | TokenKind.KeywordMut
         | TokenKind.KeywordInline
         | TokenKind.KeywordOverride => true
    case _ => false
  }

  /**
    * Checks if this token is one of the [[TokenKind]]s that can validly appear as the first token of any declaration.
    * Note that a CommentDoc, a Modifier and/or an annotation may lead a declaration.
    */
  def isFirstDecl: Boolean = this.isModifier || (this match {
    case TokenKind.CommentDoc
         | TokenKind.Annotation
         | TokenKind.KeywordMod
         | TokenKind.KeywordDef
         | TokenKind.KeywordEnum
         | TokenKind.KeywordStruct
         | TokenKind.KeywordTrait
         | TokenKind.KeywordInstance
         | TokenKind.KeywordType
         | TokenKind.KeywordEff
         | TokenKind.KeywordRestrictable => true
    case _ => false
  })

  /**
    * Checks if this token is one of the [[TokenKind]]s that can validly appear after a doc-comment.
    * Doc-comments may only annotate declarations and enum-cases.
    */
  def isDocumentable: Boolean = this.isFirstDecl || (this match {
    case TokenKind.KeywordLaw => true
    case TokenKind.KeywordCase => true
    case _ => false
  })

  /**
    * Checks if this token is one of the [[TokenKind]]s that can validly appear as the first token in a declaration within an effect.
    * Note that a CommentDoc, a Modifier and/or an annotation may lead such a declaration.
    */
  def isFirstEff: Boolean = this.isModifier || (this match {
    case TokenKind.CommentDoc
         | TokenKind.Annotation
         | TokenKind.KeywordDef => true
    case _ => false
  })

  /**
    * Checks if this token is one of the [[TokenKind]]s that can validly appear as the first token in a declaration within an instance.
    * Note that a CommentDoc, a Modifier and/or an annotation may lead such a declaration.
    */
  def isFirstInstance: Boolean = this.isModifier || (this match {
    case TokenKind.CommentDoc
         | TokenKind.Annotation
         | TokenKind.KeywordDef
         | TokenKind.KeywordType => true
    case _ => false
  })

  /**
    * Checks if this token is one of the [[TokenKind]]s that can validly appear as the first token in a declaration within a trait.
    * Note that a CommentDoc, a Modifier and/or an annotation may lead such a declaration.
    */
  def isFirstTrait: Boolean = this.isModifier || (this match {
    case TokenKind.CommentDoc
         | TokenKind.Annotation
         | TokenKind.KeywordDef
         | TokenKind.KeywordLaw
         | TokenKind.KeywordType => true
    case _ => false
  })

  /**
    * Checks if kind is one of the [[TokenKind]]s that can validly appear as the first token of any expression.
    */
  def isFirstExpr: Boolean = this match {
    case TokenKind.Annotation
         | TokenKind.ArrayHash
         | TokenKind.BuiltIn
         | TokenKind.CurlyL
         | TokenKind.DotDotDot
         | TokenKind.HashCurlyL
         | TokenKind.HashParenL
         | TokenKind.HoleAnonymous
         | TokenKind.HoleNamed
         | TokenKind.HoleVariable
         | TokenKind.KeywordCheckedCast
         | TokenKind.KeywordCheckedECast
         | TokenKind.KeywordChoose
         | TokenKind.KeywordChooseStar
         | TokenKind.KeywordDebug
         | TokenKind.KeywordDebugBang
         | TokenKind.KeywordDebugBangBang
         | TokenKind.KeywordDef
         | TokenKind.KeywordDiscard
         | TokenKind.KeywordFalse
         | TokenKind.KeywordForA
         | TokenKind.KeywordForM
         | TokenKind.KeywordForce
         | TokenKind.KeywordForeach
         | TokenKind.KeywordHandler
         | TokenKind.KeywordIf
         | TokenKind.KeywordImport
         | TokenKind.KeywordInject
         | TokenKind.KeywordLazy
         | TokenKind.KeywordLet
         | TokenKind.KeywordMatch
         | TokenKind.KeywordNew
         | TokenKind.KeywordNot
         | TokenKind.KeywordNull
         | TokenKind.KeywordOpenVariant
         | TokenKind.KeywordOpenVariantAs
         | TokenKind.KeywordPar
         | TokenKind.KeywordQuery
         | TokenKind.KeywordRegion
         | TokenKind.KeywordRun
         | TokenKind.KeywordSelect
         | TokenKind.KeywordSolve
         | TokenKind.KeywordSpawn
         | TokenKind.KeywordStaticUppercase
         | TokenKind.KeywordTrue
         | TokenKind.KeywordTry
         | TokenKind.KeywordTypeMatch
         | TokenKind.KeywordUncheckedCast
         | TokenKind.KeywordUnsafe
         | TokenKind.KeywordUnsafely
         | TokenKind.KeywordUse
         | TokenKind.ListHash
         | TokenKind.LiteralBigDecimal
         | TokenKind.LiteralBigInt
         | TokenKind.LiteralChar
         | TokenKind.LiteralDebugStringL
         | TokenKind.LiteralFloat32
         | TokenKind.LiteralFloat64
         | TokenKind.LiteralInt16
         | TokenKind.LiteralInt32
         | TokenKind.LiteralInt64
         | TokenKind.LiteralInt8
         | TokenKind.LiteralRegex
         | TokenKind.LiteralString
         | TokenKind.LiteralStringInterpolationL
         | TokenKind.MapHash
         | TokenKind.Minus
         | TokenKind.NameGreek
         | TokenKind.NameLowerCase
         | TokenKind.NameMath
         | TokenKind.NameUpperCase
         | TokenKind.ParenL
         | TokenKind.Plus
         | TokenKind.SetHash
         | TokenKind.TripleTilde
         | TokenKind.Underscore
         | TokenKind.VectorHash => true
    case TokenKind.Ampersand
         | TokenKind.AngleL
         | TokenKind.AngleLEqual
         | TokenKind.AngleR
         | TokenKind.AngleREqual
         | TokenKind.AngledEqual
         | TokenKind.AngledPlus
         | TokenKind.ArrowThickR
         | TokenKind.ArrowThinL
         | TokenKind.ArrowThinR
         | TokenKind.At
         | TokenKind.Backslash
         | TokenKind.Bang
         | TokenKind.BangEqual
         | TokenKind.Bar
         | TokenKind.BracketL
         | TokenKind.BracketR
         | TokenKind.Caret
         | TokenKind.Colon
         | TokenKind.ColonColon
         | TokenKind.ColonEqual
         | TokenKind.ColonMinus
         | TokenKind.Comma
         | TokenKind.CommentBlock
         | TokenKind.CommentDoc
         | TokenKind.CommentLine
         | TokenKind.CurlyR
         | TokenKind.Dollar
         | TokenKind.Dot
         | TokenKind.DotCurlyL
         | TokenKind.DotWhiteSpace
         | TokenKind.Eof
         | TokenKind.Equal
         | TokenKind.EqualEqual
         | TokenKind.Err(_)
         | TokenKind.Hash
         | TokenKind.InfixFunction
         | TokenKind.KeywordAlias
         | TokenKind.KeywordAnd
         | TokenKind.KeywordAs
         | TokenKind.KeywordCase
         | TokenKind.KeywordCatch
         | TokenKind.KeywordEff
         | TokenKind.KeywordElse
         | TokenKind.KeywordEnum
         | TokenKind.KeywordFix
         | TokenKind.KeywordForall
         | TokenKind.KeywordFrom
         | TokenKind.KeywordInline
         | TokenKind.KeywordInstance
         | TokenKind.KeywordInstanceOf
         | TokenKind.KeywordInto
         | TokenKind.KeywordLaw
         | TokenKind.KeywordLawful
         | TokenKind.KeywordMod
         | TokenKind.KeywordMut
         | TokenKind.KeywordOpenVariant
         | TokenKind.KeywordOr
         | TokenKind.KeywordOverride
         | TokenKind.KeywordProject
         | TokenKind.KeywordPub
         | TokenKind.KeywordRedef
         | TokenKind.KeywordRestrictable
         | TokenKind.KeywordRvadd
         | TokenKind.KeywordRvand
         | TokenKind.KeywordRvnot
         | TokenKind.KeywordRvsub
         | TokenKind.KeywordSealed
         | TokenKind.KeywordStatic
         | TokenKind.KeywordStruct
         | TokenKind.KeywordThrow
         | TokenKind.KeywordTrait
         | TokenKind.KeywordType
         | TokenKind.KeywordUniv
         | TokenKind.KeywordWhere
         | TokenKind.KeywordWith
         | TokenKind.KeywordWithout
         | TokenKind.KeywordXor
         | TokenKind.KeywordYield
         | TokenKind.LiteralDebugStringR
         | TokenKind.LiteralStringInterpolationR
         | TokenKind.ParenR
         | TokenKind.RegionTag
         | TokenKind.Semi
         | TokenKind.Slash
         | TokenKind.Star
         | TokenKind.StarStar
         | TokenKind.StructArrow
         | TokenKind.Tilde
         | TokenKind.TripleAmpersand
         | TokenKind.TripleAngleL
         | TokenKind.TripleAngleR
         | TokenKind.TripleBar
         | TokenKind.TripleCaret
         | TokenKind.TripleColon
         | TokenKind.UserDefinedOperator => false
  }

  /**
    * Checks if kind is one of the [[TokenKind]]s that can validly appear as the first token of any type.
    */
  def isFirstType: Boolean = this match {
    case TokenKind.NameUpperCase
         | TokenKind.NameMath
         | TokenKind.NameGreek
         | TokenKind.Underscore
         | TokenKind.NameLowerCase
         | TokenKind.KeywordUniv
         | TokenKind.KeywordFalse
         | TokenKind.KeywordTrue
         | TokenKind.ParenL
         | TokenKind.CurlyL
         | TokenKind.HashCurlyL
         | TokenKind.HashParenL
         | TokenKind.AngleL
         | TokenKind.KeywordNot
         | TokenKind.Tilde
         | TokenKind.KeywordRvnot
         | TokenKind.KeywordStaticUppercase => true
    case _ => false
  }

  /**
    * Checks if kind is one of the [[TokenKind]]s that can validly appear as the first token of any pattern.
    */
  def isFirstPattern: Boolean = this match {
    case TokenKind.NameLowerCase
         | TokenKind.NameGreek
         | TokenKind.NameMath
         | TokenKind.Underscore
         | TokenKind.KeywordQuery
         | TokenKind.LiteralString
         | TokenKind.LiteralChar
         | TokenKind.LiteralFloat32
         | TokenKind.LiteralFloat64
         | TokenKind.LiteralBigDecimal
         | TokenKind.LiteralInt8
         | TokenKind.LiteralInt16
         | TokenKind.LiteralInt32
         | TokenKind.LiteralInt64
         | TokenKind.LiteralBigInt
         | TokenKind.KeywordTrue
         | TokenKind.KeywordFalse
         | TokenKind.LiteralRegex
         | TokenKind.KeywordNull
         | TokenKind.NameUpperCase
         | TokenKind.ParenL
         | TokenKind.CurlyL
         | TokenKind.Minus => true
    case _ => false
  }

  def isFirstRecordOp: Boolean = this match {
    case TokenKind.Plus
         | TokenKind.Minus
         | TokenKind.NameLowerCase => true
    case _ => false
  }

  /*
    * The following isRecover* functions represent sets of TokenKinds used for error-recovery in the parser.
    * When parsing sequences of expressions in a loop and an unexpected token is found,
    * a recovery means breaking the loop.
    *
    * An example would be:
    * def foo(): Unit = bar(1,
    * enum Legumes { case Beans, Chickpeas }
    * Here we recover from parsing argument expressions of the unfinished call to `bar`
    * and still discover `Legumes`, because isRecoverDecl return true for TokenKind.KeywordEnum.
    */

  /**
    * Checks if kind is a TokenKind that warrants breaking a declaration parsing loop.
    * This is used to skip tokens until the start of a declaration is found,
    * in case no other error-recovery could be applied.
    */
  def isRecoverDecl: Boolean = this.isFirstDecl

  /**
    * Checks if kind is a TokenKind that warrants breaking a module parsing loop.
    * This is used to skip tokens until the start of a declaration is found,
    * in case no other error-recovery could be applied.
    */
  def isRecoverMod: Boolean = this == TokenKind.CurlyR || this.isFirstDecl

  /**
    * Checks if kind is a TokenKind that warrants breaking an expression parsing loop.
    * For instance, if we find an 'trait' keyword in the middle of an expression,
    * we can assume that there is no more expression to parse.
    * Because isRecoverExpr returns true for KeywordTrait the expression loop stops,
    * making the parse return back into the declaration loop which can capture the trait.
    */
  def isRecoverExpr: Boolean = this.isFirstDecl || this == TokenKind.Semi

  /**
    * Checks if kind is a TokenKind that warrants breaking a type parsing loop.
    */
  def isRecoverType: Boolean = this.isFirstDecl || (this match {
    case TokenKind.Semi | TokenKind.Equal => true
    case _ => false
  })

  /**
    * Checks if kind is a TokenKind that warrants breaking a pattern parsing loop.
    */
  def isRecoverPattern: Boolean = this.isFirstDecl || (this match {
    case TokenKind.ArrowThickR | TokenKind.KeywordCase => true
    case _ => false
  })

  /**
    * Checks if kind is a TokenKind that warrants breaking the top-level use or import parsing loop.
    */
  def isRecoverUseOrImport: Boolean = this.isFirstDecl || (this match {
    case TokenKind.Semi | TokenKind.KeywordUse | TokenKind.KeywordImport => true
    case _ => false
  })

  /**
    * Checks if kind is a TokenKind that warrants breaking a function parameter parsing loop.
    */
  def isRecoverParameters: Boolean = this.isFirstDecl || (this match {
    case TokenKind.Colon | TokenKind.Equal => true
    case _ => false
  })
}

/**
  * Tokens are named for 'what they are' rather than 'what they represent'.
  * So '::' is not named 'Cons' but instead 'ColonColon' as the lexer should be oblivious to the concept of cons
  *
  * Tokens belonging to some conceptual group should have the group name as prefix.
  * So 'LiteralInt32' is preferred over 'Int32Literal'
  */
object TokenKind {
  case object Ampersand extends TokenKind

  case object AngledEqual extends TokenKind

  case object AngledPlus extends TokenKind

  case object AngleL extends TokenKind

  case object AngleLEqual extends TokenKind

  case object AngleR extends TokenKind

  case object AngleREqual extends TokenKind

  case object Annotation extends TokenKind

  case object ArrayHash extends TokenKind

  case object ArrowThickR extends TokenKind

  case object ArrowThinL extends TokenKind

  case object ArrowThinR extends TokenKind

  case object At extends TokenKind

  case object Backslash extends TokenKind

  case object Bang extends TokenKind

  case object BangEqual extends TokenKind

  case object Bar extends TokenKind

  case object BracketL extends TokenKind

  case object BracketR extends TokenKind

  case object BuiltIn extends TokenKind

  case object Caret extends TokenKind

  case object Colon extends TokenKind

  case object ColonColon extends TokenKind

  case object ColonEqual extends TokenKind

  case object ColonMinus extends TokenKind

  case object Comma extends TokenKind

  case object CommentBlock extends TokenKind

  case object CommentLine extends TokenKind

  case object CommentDoc extends TokenKind

  case object CurlyL extends TokenKind

  case object CurlyR extends TokenKind

  case object Dollar extends TokenKind

  case object Dot extends TokenKind

  case object DotDotDot extends TokenKind

  case object DotWhiteSpace extends TokenKind

  case object DotCurlyL extends TokenKind

  case object Equal extends TokenKind

  case object EqualEqual extends TokenKind

  case object Hash extends TokenKind

  case object HashCurlyL extends TokenKind

  case object HashParenL extends TokenKind

  case object HoleAnonymous extends TokenKind

  case object HoleNamed extends TokenKind

  case object HoleVariable extends TokenKind

  case object InfixFunction extends TokenKind

  case object KeywordAlias extends TokenKind

  case object KeywordAnd extends TokenKind

  case object KeywordAs extends TokenKind

  case object KeywordCase extends TokenKind

  case object KeywordCatch extends TokenKind

  case object KeywordCheckedCast extends TokenKind

  case object KeywordCheckedECast extends TokenKind

  case object KeywordChoose extends TokenKind

  case object KeywordChooseStar extends TokenKind

  case object KeywordDebug extends TokenKind

  case object KeywordDebugBang extends TokenKind

  case object KeywordDebugBangBang extends TokenKind

  case object KeywordDef extends TokenKind

  case object KeywordDiscard extends TokenKind

  case object KeywordEff extends TokenKind

  case object KeywordElse extends TokenKind

  case object KeywordEnum extends TokenKind

  case object KeywordFalse extends TokenKind

  case object KeywordFix extends TokenKind

  case object KeywordForA extends TokenKind

  case object KeywordForall extends TokenKind

  case object KeywordForce extends TokenKind

  case object KeywordForeach extends TokenKind

  case object KeywordForM extends TokenKind

  case object KeywordFrom extends TokenKind

  case object KeywordHandler extends TokenKind

  case object KeywordIf extends TokenKind

  case object KeywordImport extends TokenKind

  case object KeywordInject extends TokenKind

  case object KeywordInline extends TokenKind

  case object KeywordInstance extends TokenKind

  case object KeywordInstanceOf extends TokenKind

  case object KeywordInto extends TokenKind

  case object KeywordLaw extends TokenKind

  case object KeywordLawful extends TokenKind

  case object KeywordLazy extends TokenKind

  case object KeywordLet extends TokenKind

  case object KeywordMatch extends TokenKind

  case object KeywordMod extends TokenKind

  case object KeywordMut extends TokenKind

  case object KeywordNew extends TokenKind

  case object KeywordNot extends TokenKind

  case object KeywordNull extends TokenKind

  case object KeywordOpenVariant extends TokenKind

  case object KeywordOpenVariantAs extends TokenKind

  case object KeywordOr extends TokenKind

  case object KeywordOverride extends TokenKind

  case object KeywordPar extends TokenKind

  case object KeywordPub extends TokenKind

  case object KeywordProject extends TokenKind

  case object KeywordQuery extends TokenKind

  case object KeywordRedef extends TokenKind

  case object KeywordRegion extends TokenKind

  case object KeywordRestrictable extends TokenKind

  case object KeywordRun extends TokenKind

  case object KeywordRvadd extends TokenKind

  case object KeywordRvand extends TokenKind

  case object KeywordRvnot extends TokenKind

  case object KeywordRvsub extends TokenKind

  case object KeywordSealed extends TokenKind

  case object KeywordSelect extends TokenKind

  case object KeywordSolve extends TokenKind

  case object KeywordSpawn extends TokenKind

  case object KeywordStatic extends TokenKind

  case object KeywordStaticUppercase extends TokenKind

  case object KeywordStruct extends TokenKind

  case object KeywordThrow extends TokenKind

  case object KeywordTrait extends TokenKind

  case object KeywordTrue extends TokenKind

  case object KeywordTry extends TokenKind

  case object KeywordType extends TokenKind

  case object KeywordTypeMatch extends TokenKind

  case object KeywordUncheckedCast extends TokenKind

  case object KeywordUniv extends TokenKind

  case object KeywordUnsafe extends TokenKind

  case object KeywordUnsafely extends TokenKind

  case object KeywordUse extends TokenKind

  case object KeywordWhere extends TokenKind

  case object KeywordWith extends TokenKind

  case object KeywordWithout extends TokenKind

  case object KeywordYield extends TokenKind

  case object KeywordXor extends TokenKind

  case object ListHash extends TokenKind

  case object LiteralBigDecimal extends TokenKind

  case object LiteralBigInt extends TokenKind

  case object LiteralChar extends TokenKind

  case object LiteralDebugStringL extends TokenKind

  case object LiteralDebugStringR extends TokenKind

  case object LiteralFloat32 extends TokenKind

  case object LiteralFloat64 extends TokenKind

  case object LiteralInt8 extends TokenKind

  case object LiteralInt16 extends TokenKind

  case object LiteralInt32 extends TokenKind

  case object LiteralInt64 extends TokenKind

  case object LiteralRegex extends TokenKind

  case object LiteralString extends TokenKind

  case object LiteralStringInterpolationL extends TokenKind

  case object LiteralStringInterpolationR extends TokenKind

  case object MapHash extends TokenKind

  case object Minus extends TokenKind

  case object NameGreek extends TokenKind

  case object NameLowerCase extends TokenKind

  case object NameMath extends TokenKind

  case object NameUpperCase extends TokenKind

  case object ParenL extends TokenKind

  case object ParenR extends TokenKind

  case object Plus extends TokenKind

  case object RegionTag extends TokenKind

  case object Semi extends TokenKind

  case object SetHash extends TokenKind

  case object Slash extends TokenKind

  case object Star extends TokenKind

  case object StarStar extends TokenKind

  case object StructArrow extends TokenKind

  case object Tilde extends TokenKind

  case object TripleAmpersand extends TokenKind

  case object TripleAngleL extends TokenKind

  case object TripleAngleR extends TokenKind

  case object TripleBar extends TokenKind

  case object TripleCaret extends TokenKind

  case object TripleColon extends TokenKind

  case object TripleTilde extends TokenKind

  case object Underscore extends TokenKind

  case object UserDefinedOperator extends TokenKind

  case object VectorHash extends TokenKind

  /** A special token emitted instead of halting the lexer when an error is encountered.
    *
    * @param error the actual error related to this token
    */
  case class Err(error: LexerError) extends TokenKind

  /** A virtual token signalling END-OF-FILE.
    */
  case object Eof extends TokenKind
}
