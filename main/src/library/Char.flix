/*
 * Copyright 2019 Magnus Madsen
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

instance LowerBound[Char] {
    pub def minValue(): Char = Char.minValue()
}

instance UpperBound[Char] {
    pub def maxValue(): Char = Char.maxValue()
}

mod Char {

    import java.lang.Character

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(c: Char): Bool =
        c <= '\u0080'

    ///
    /// Returns `true` if the given char `c` is a letter character.
    ///
    pub def isLetter(c: Char): Bool =
        unsafely IO run Character.isLetter(c)

    ///
    /// Returns `true` if the given char `c` is a recognized Unicode digit.
    /// This includes the ASCII range 0..9 but also Arabic-Indic digits, Devagari digits and Fullwidth digits.
    ///
    pub def isDigit(c: Char): Bool =
        unsafely IO run Character.isDigit(c)

    ///
    /// Returns `true` if the given char `c` is a recognized Unicode letter or digit.
    ///
    pub def isLetterOrDigit(c: Char): Bool =
        unsafely IO run Character.isLetterOrDigit(c)

    ///
    /// Returns `true` if the given char `c` is strictly in the range of ASCII digits 0...9.
    ///
    pub def isAsciiDigit(c: Char): Bool =
        isAscii(c) and isDigit(c)

    ///
    /// Returns `true` if the given char `c` is in the range 0...7.
    ///
    pub def isOctDigit(c: Char): Bool =
        '0' <= c and c <= '7'     // '0'..'7'

    ///
    /// Returns `true` if the given char `c` is in the range 0...F.
    ///
    pub def isHexDigit(c: Char): Bool = match c {
        case i if '0' <= i and i <= '9' => true        // '0'..'9'
        case i if 'A' <= i and i <= 'F' => true        // 'A'..'F'
        case i if 'a' <= i and i <= 'f' => true        // 'a'..'f'
        case _ => false
    }

    ///
    /// Returns `true` if the given char `c` is lowercase.
    ///
    pub def isLowerCase(c: Char): Bool =
        unsafely IO run Character.isLowerCase(c)

    ///
    /// Returns `true` if the given char `c` is uppercase.
    ///
    pub def isUpperCase(c: Char): Bool =
        unsafely IO run Character.isUpperCase(c)

    ///
    /// Returns `true` if the given char `c` is titlecase, i.e. in the Unicode
    /// category of title case letters.
    ///
    pub def isTitleCase(c: Char): Bool =
        unsafely IO run Character.isTitleCase(c)

    ///
    /// Returns `true` if the given char `c` is a white space character.
    ///
    pub def isWhiteSpace(c: Char): Bool =
        unsafely IO run Character.isWhitespace(c)

    ///
    /// Returns `true` if the given char `c` is defined either as a entry in the
    /// UnicodeData file or a value within a range defined in the UnicodeData file.
    ///
    pub def isDefined(c: Char): Bool =
        unsafely IO run Character.isDefined(c)

    ///
    /// Returns `true` if the given char `c` is an ISO control character.
    ///
    pub def isISOControl(c: Char): Bool =
        unsafely IO run Character.isISOControl(c)

    ///
    /// Returns `true` if the given char `c` is mirrored.
    ///
    pub def isMirrored(c: Char): Bool =
        unsafely IO run Character.isMirrored(c)

    ///
    /// Returns `true` if the given char `c` is a surrogate code unit.
    ///
    pub def isSurrogate(c: Char): Bool =
        unsafely IO run Character.isSurrogate(c)

    ///
    /// Returns `true` if the given characters `high` and `low` represent a valid
    /// Unicode surrogate pair.
    ///
    pub def isSurrogatePair(high: {high = Char}, low: {low = Char}): Bool =
        unsafely IO run Character.isSurrogatePair(high#high, low#low)

    ///
    /// Converts a letter to its lowercase version.
    ///
    /// Returns the original character if it does not have a lowercase version.
    ///
    pub def toLowerCase(c: Char): Char =
        unsafely IO run Character.toLowerCase(c)

    ///
    /// Converts a letter to its uppercase version.
    ///
    /// Returns the original character if it does not have a uppercase version.
    ///
    pub def toUpperCase(c: Char): Char =
        unsafely IO run Character.toUpperCase(c)

    ///
    /// Converts a letter to its titlecase version.
    ///
    /// Returns the original character if it does not have either a titlecase
    /// version or a mapping to uppercase.
    ///
    pub def toTitleCase(c: Char): Char =
        unsafely IO run Character.toTitleCase(c)

    ///
    /// Returns the code point representation of Char `c`.
    ///
    pub def toBmpCodePoint(c: Char): Int32 =
        unchecked_cast(c as Int32)

    ///
    /// Returns the supplementary code point value of the surrogate pair `high` and `low`.
    ///
    /// Caution - this function does no validation, use `isSurrogatePair` to check `high` and `low` are valid.
    ///
    pub def toSupplementaryCodePoint(high: {high = Char}, low: {low = Char}): Int32 =
        unsafely IO run Character.toCodePoint(high#high, low#low)

    ///
    /// Returns the character `c` as a string.
    ///
    pub def toString(c: Char): String = ToString.toString(c)

    ///
    /// Returns the character given by the all zero byte.
    ///
    pub def minValue(): Char = '\u0000'

    ///
    /// Returns the character given by the maximum valued byte.
    ///
    pub def maxValue(): Char = '\uffff'

    ///
    /// Returns the integer value the Char `c` represents (e.g. '1' => `Some(1)``)
    /// or `None` if `c` does not represent a number.
    ///
    /// This function cannot handle supplementary characters.
    ///
    pub def getNumericValue(c: Char): Option[Int32] =
        match (unsafely IO run Character.getNumericValue(c)) {
            case i if i < 0 => None
            case i          => Some(i)
        }

    ///
    /// Returns the integer value Char `c` represents with respect to `radix`. E.g.
    /// `digit(radix = 10, '1') => Some(1)`
    /// `digit(radix = 16, 'a') => Some(11)`
    ///
    /// Returns `None` if `c` does not represent a number.
    ///
    pub def digit(radix: {radix = Int32}, c: Char): Option[Int32] =
        match (unsafely IO run Character.digit(c, radix#radix)) {
            case i if i < 0 => None
            case i          => Some(i)
        }

    ///
    /// Returns a character representation of the integer `n` with respect to `radix`. E.g.
    /// `forDigit(radix = 10, 1) => Some('1')`
    /// `forDigit(radix = 16, 11) => Some('a')`
    ///
    /// Returns `None` if `n` is not representable as single character in the radix.
    ///
    pub def forDigit(radix: {radix = Int32}, n: Int32): Option[Char] =
        match (unsafely IO run Character.forDigit(n, radix#radix)) {
            case c if c == '\u0000' => None
            case c                  => Some(c)
        }

    ///
    /// Get the primitive Char value from its object representation, i.e., java.lang.Character.
    ///
    /// This function is expected to be used when marshaling Chars from Java. Generally in Flix
    /// code you should not need to use `java.lang.Character`.
    ///
    pub def charValue(c: Character): Char =
        unsafely IO run c.charValue()

    ///
    /// Convert an Char value to its its object representation (i.e. java.lang.Character).
    ///
    /// This function is expected to be used when marshaling Chars to Java. Generally in Flix
    /// code you should not need to use `java.lang.Character`.
    ///
    pub def valueOf(c: Char): Character =
        unsafely IO run Character.valueOf(c)

}
