/*
 * Copyright 2023 Stephen Tetley
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

///
/// The `CodePoint` module complements the `Char` module. `Char` only
/// represents characters in Unicode's _Basic Multilingual Plane_
/// (U+0000 to U+FFFF).
/// CodePoint extends access to include _supplementary characters_ so all
/// Unicode characters can be represented.
///
mod CodePoint {

    import java.lang.Character
    import java.lang.Exception
    import java.lang.Throwable

    ///
    /// Returns the minimum integer value of a Unicode code point.
    ///
    pub def minValue(): Int32 = unsafely IO run Character.MIN_CODE_POINT

    ///
    /// Returns the maximum integer value of a Unicode code point.
    ///
    pub def maxValue(): Int32 = unsafely IO run Character.MAX_CODE_POINT

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(cp: Int32): Bool =
        0x0000 <= cp  and cp <= 0x0080

    ///
    /// Returns `true` if the given code point `cp` represents a letter character.
    ///
    pub def isLetter(cp: Int32): Bool =
        unsafely IO run Character.isLetter(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode digit.
    /// This includes the ASCII range 0..9 but also Arabic-Indic digits, Devagari digits and Fullwidth digits.
    ///
    pub def isDigit(cp: Int32): Bool =
        unsafely IO run Character.isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode letter or digit.
    ///
    pub def isLetterOrDigit(cp: Int32): Bool =
        unsafely IO run Character.isLetterOrDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character strictly in the
    /// range of ASCII digits 0...9.
    ///
    pub def isAsciiDigit(cp: Int32): Bool =
        isAscii(cp) and isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...7.
    ///
    pub def isOctDigit(cp: Int32): Bool =
        0x0030 <= cp and cp <= 0x0037     // '0'..'7'

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...F.
    ///
    pub def isHexDigit(cp: Int32): Bool = match cp {
        case i if 0x0030 <= i and i <= 0x0039 => true        // '0'..'9'
        case i if 0x0041 <= i and i <= 0x0046 => true        // 'A'..'F'
        case i if 0x0061 <= i and i <= 0x0066 => true        // 'a'..'f'
        case _ => false
    }

    ///
    /// Returns `true` if the given code point `cp` represents a lowercase letter.
    ///
    pub def isLowerCase(cp: Int32): Bool =
        unsafely IO run Character.isLowerCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an uppercase letter.
    ///
    pub def isUpperCase(cp: Int32): Bool =
        unsafely IO run Character.isUpperCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a titlecase letter.
    ///
    pub def isTitleCase(cp: Int32): Bool =
        unsafely IO run Character.isTitleCase(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a white space character.
    ///
    pub def isWhiteSpace(cp: Int32): Bool =
        unsafely IO run Character.isWhitespace(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an alphabetic character.
    ///
    pub def isAlphabetic(cp: Int32): Bool =
        unsafely IO run Character.isAlphabetic(cp)

    ///
    /// Returns `true` if the given code point `cp` is within Unicode's Basic Multilingual Plane.
    ///
    /// If the code point is within the BMP it can be represented by Flix's `Char` datatype.
    ///
    pub def isBmpCodePoint(cp: Int32): Bool =
        unsafely IO run Character.isBmpCodePoint(cp)

    ///
    /// Returns `true` if the given code point `cp` is defined either as a entry in the
    /// UnicodeData file or a value within a range defined in the UnicodeData file.
    ///
    pub def isDefined(cp: Int32): Bool =
        unsafely IO run Character.isDefined(cp)

    ///
    /// Returns `true` if the given code point `cp` is Chinese, Japanase, Korean
    /// or Vietnamese ideograph.
    ///
    pub def isIdeographic(cp: Int32): Bool =
        unsafely IO run Character.isIdeographic(cp)

    ///
    /// Returns `true` if the given code point `cp` is an ISO control character.
    ///
    pub def isISOControl(cp: Int32): Bool =
        unsafely IO run Character.isISOControl(cp)

    ///
    /// Returns `true` if the given code point `cp` is mirrored.
    ///
    pub def isMirrored(cp: Int32): Bool =
        unsafely IO run Character.isMirrored(cp)

    ///
    /// Returns `true` if the given code point `cp` is in Unicode's supplementary character range.
    ///
    pub def isSupplementaryCodePoint(cp: Int32): Bool =
        unsafely IO run Character.isSupplementaryCodePoint(cp)

    ///
    /// Returns `true` if the code point `cp` is between `U+0000` and `U+10FFFF`.
    ///
    pub def isValidCodePoint(cp: Int32): Bool =
        unsafely IO run Character.isValidCodePoint(cp)

    ///
    /// Converts a code point representing a letter to its lowercase version.
    ///
    /// Returns the original code point if it does not have a lowercase version.
    ///
    pub def toLowerCase(cp: Int32): Int32 =
        unsafely IO run Character.toLowerCase(cp)

    ///
    /// Converts a code point representing a letter to its uppercase version.
    ///
    /// Returns the original code point if it does not have a uppercase version.
    ///
    pub def toUpperCase(cp: Int32): Int32 =
        unsafely IO run Character.toUpperCase(cp)

    ///
    /// Converts a code point representing a letter to its titlecase version.
    ///
    /// Returns the original character if it does not have either a titlecase
    /// version or a mapping to uppercase.
    ///
    pub def toTitleCase(cp: Int32): Int32 =
        unsafely IO run Character.toTitleCase(cp)

    ///
    /// Returns the name of the code point `cp`.
    ///
    /// See the Java JDK documentation of the method `java.lang.Character.getName` for a full
    /// description of how names are derived.
    ///
    pub def getName(cp: Int32): Option[String] =
        try {
            unsafely IO run Character.getName(cp) |> Object.toOption
        } catch {
            case _: Exception => None
        }

    ///
    /// Returns the integer value representated by the code point `cp` e.g. code point 0x0031
    /// which is the char '1' returns `Some(1)`.
    ///
    /// Returns `None` if `cp` does not represent a number.
    ///
    /// This function handles supplementary characters.
    ///
    pub def getNumericValue(cp: Int32): Option[Int32] =
        match unsafely IO run Character.getNumericValue(cp) {
            case i if i < 0 => None
            case i          => Some(i)
        }

    ///
    /// Returns the number of Chars needed to represent the code point `cp`.
    ///
    /// The answer is either 2 for a supplementary character or 1 for any other character.
    ///
    pub def charCount(cp: Int32): Int32 =
        unsafely IO run Character.charCount(cp)

    ///
    /// Optionally returns a vector of Chars representing the code point `cp`.
    ///
    /// A valid `Some(_)` answer is either a length 2 Vector for a supplementary character
    /// or a length 1 Vector for any other valid character.
    ///
    /// Returns 'None' if `cp` is not a valid code point.
    ///
    pub def toChars(cp: Int32): Option[Vector[Char]] = try {
        region rc {
            let arr : Array[Char, rc] = Array.empty(rc, 2);
            let size = unchecked_cast(Character.toChars(cp, arr, 0) as _ \ rc);
            Array.toVector(arr) |> Vector.takeLeft(size) |> Some
        }
    } catch {
        case _: Throwable => None
    }

    ///
    /// Optionally returns a Char represention the code point `cp` if `cp` is within
    /// Unicode's Basic Multilingual Plane.
    ///
    pub def toBmpChar(cp: Int32): Option[Char] = try {
        region rc {
            let arr : Array[Char, rc] = Array.empty(rc, 2);
            let i = unsafely IO run Character.toChars(cp, arr, 0);
            if (i == 1)
                Array.nth(0, arr)
            else
                None
        }
    } catch {
        case _: Exception => None
    }

    ///
    /// Optionally returns a pair of Chars representing the high and low surrogate characters of the
    /// code point `cp` if `cp` is a supplementary character.
    ///
    pub def toSupplementaryChars(cp: Int32): Option[(Char, Char)] = try {
        region rc {
            let arr : Array[Char, rc] = Array.empty(rc, 2);
            let i = unsafely IO run Character.toChars(cp, arr, 0);
            if (i == 2)
                Some((Array.get(0, arr), Array.get(1, arr)))
            else
                None
        }
    } catch {
        case _: Exception => None
    }

    ///
    /// Returns a String representing the code point `cp`.
    ///
    pub def toString(cp: Int32): String =
        try {
            unsafely IO run Character.toString(cp)
        } catch {
            case _: Exception => ""
        }

    ///
    /// Optionally returns the high surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def highSurrogate(cp: Int32): Option[Char] =
        if (isSupplementaryCodePoint(cp))
            Some(unsafely IO run Character.highSurrogate(cp))
        else
            None

    ///
    /// Optionally returns the low surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def lowSurrogate(cp: Int32): Option[Char] =
        if (isSupplementaryCodePoint(cp))
            Some(unsafely IO run Character.lowSurrogate(cp))
        else
            None

}
