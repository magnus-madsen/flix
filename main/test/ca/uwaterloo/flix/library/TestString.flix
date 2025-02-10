/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
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

mod TestString {

/////////////////////////////////////////////////////////////////////////////
// charAt                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def charAt01(): Bool = String.charAt(0, "Hello World!") == 'H'

@test
def charAt02(): Bool = String.charAt(1, "Hello World!") == 'e'

@test
def charAt03(): Bool = String.charAt(2, "Hello World!") == 'l'

@test
def charAt04(): Bool = String.charAt(3, "Hello World!") == 'l'

@test
def charAt05(): Bool = String.charAt(4, "Hello World!") == 'o'

@test
def charAt06(): Bool = String.charAt(5, "Hello World!") == ' '

/////////////////////////////////////////////////////////////////////////////
// Indexable                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def index01(): Bool = OutOfBounds.asRes(() -> "Hello World!"[0])() == Ok('H')

@test
def index02(): Bool = OutOfBounds.asRes(() -> "Hello World!"[1]) == Ok('e')

@test
def index03(): Bool = OutOfBounds.asRes(() -> "Hello World!"[2]) == Ok('l')

@test
def index04(): Bool = OutOfBounds.asRes(() -> "Hello World!"[3]) == Ok('l')

@test
def index05(): Bool = OutOfBounds.asRes(() -> "Hello World!"[4]) == Ok('o')

@test
def index06(): Bool = OutOfBounds.asRes(() -> "Hello World!"[5]) == Ok(' ')

/////////////////////////////////////////////////////////////////////////////
// nth                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def nth01(): Bool = String.nth(0, "") == None

@test
def nth02(): Bool = String.nth(-1, "Hello World!") == None

@test
def nth03(): Bool = String.nth(15, "Hello World!") == None

@test
def nth04(): Bool = String.nth(0, "Hello World!") == Some('H')

@test
def nth05(): Bool = String.nth(1, "Hello World!") == Some('e')

@test
def nth06(): Bool = String.nth(2, "Hello World!") == Some('l')

@test
def nth07(): Bool = String.nth(3, "Hello World!") == Some('l')

@test
def nth08(): Bool = String.nth(4, "Hello World!") == Some('o')

@test
def nth09(): Bool = String.nth(5, "Hello World!") == Some(' ')

@test
def nth10(): Bool = String.nth(11, "Hello World!") == Some('!')

/////////////////////////////////////////////////////////////////////////////
// codePointAt                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def codePointAt01(): Bool = String.codePointAt(0, CodePoint.toString(0x1f3dd)) == 0x1f3dd

@test
def codePointAt02(): Bool = String.codePointAt(2, "\uD83C\uDFDD.") == 0x002E

@test
def codePointAt03(): Bool = String.codePointAt(1, ".\uD83C\uDFDD") == 0x1f3dd

/////////////////////////////////////////////////////////////////////////////
// nthCodePoint                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def nthCodePoint01(): Bool = String.nthCodePoint(0, "") == None

@test
def nthCodePoint02(): Bool = String.nthCodePoint(0, CodePoint.toString(0x1f3dd)) == Some(0x1f3dd)

@test
def nthCodePoint03(): Bool = String.nthCodePoint(2, CodePoint.toString(0x1f3dd)) == None

@test
def nthCodePoint04(): Bool = String.nthCodePoint(2, "\uD83C\uDFDD.") == Some(0x002E)

@test
def nthCodePoint05(): Bool = String.nthCodePoint(1, ".\uD83C\uDFDD") == Some(0x1f3dd)

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def concat01(): Bool = "Hello World!" == "Hello World!"

@test
def concat02(): Bool = "Hello" + " World!" == "Hello World!"

@test
def concat03(): Bool = "Hello" + " World!" == "Hello" + " World!"

@test
def concat04(): Bool = "Hello World!" + "" == "Hello World!"

@test
def concat05(): Bool = "Hello" + " " + "World" + "!" == "Hello World!"

@test
def concat06(): Bool =
    let f = x -> (x + " World!");
    f("Hello") == "Hello World!"

@test
def concat07(): Bool =
    let f = (x, y) -> (x + y);
    f("Hello", " World!") == "Hello World!"

@test
def concat08(): Bool =
    let f = flip((x, y) -> (x + y));
    f(" World!", "Hello") == "Hello World!"

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isEmpty01(): Bool = String.isEmpty("") == true

@test
def isEmpty02(): Bool = String.isEmpty("Hello") == false

/////////////////////////////////////////////////////////////////////////////
// nonEmpty                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def nonEmpty01(): Bool = String.nonEmpty("") == false

@test
def nonEmpty02(): Bool = String.nonEmpty("Hello") == true

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def length01(): Bool = String.length("") == 0

@test
def length02(): Bool = String.length("0123456789") == 10

/////////////////////////////////////////////////////////////////////////////
// size                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def size01(): Bool = String.size("") == 0

@test
def size02(): Bool = String.size("0123456789") == 10

/////////////////////////////////////////////////////////////////////////////
// split                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def split01(): Bool =
   String.split({regex = "\\s+"}, "") == "" :: Nil

@test
def split02(): Bool =
    List.length(String.split({regex = "\\s+"}, "1 3")) == 2

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def toLowerCase01(): Bool = String.toLowerCase("") == ""

@test
def toLowerCase02(): Bool = String.toLowerCase("Hello World!") == "hello world!"

@test
def toLowerCase03(): Bool = String.toLowerCase("â†’Hello Worldâˆ€!") == "â†’hello worldâˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def toUpperCase01(): Bool = String.toUpperCase("") == ""

@test
def toUpperCase02(): Bool = String.toUpperCase("Hello World!") == "HELLO WORLD!"

@test
def toUpperCase03(): Bool = String.toUpperCase("â†’Hello Worldâˆ€!") == "Â†’HELLO WORLDÂˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def toList01(): Bool = String.toList("") == Nil

@test
def toList02(): Bool = String.toList("ABC") == 'A' :: 'B' :: 'C' :: Nil

@test
def toList03(): Bool = String.toList("1 2 3") == '1' :: ' ' :: '2' :: ' ' :: '3' :: Nil

/////////////////////////////////////////////////////////////////////////////
// trim                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def trim01(): Bool = "Hello World" == String.trim("Hello World")

@test
def trim02(): Bool = "abc" == String.trim(" abc")

@test
def trim03(): Bool = "abc" == String.trim("abc ")

@test
def trim04(): Bool = "abc" == String.trim(" abc ")

@test
def trim05(): Bool = "abc" == String.trim("  abc")

@test
def trim06(): Bool = "" == String.trim("  ")

/////////////////////////////////////////////////////////////////////////////
// lineSeparator                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def lineSeparator01(): Bool =
    let newline = String.lineSeparator();
    Char.isWhiteSpace(String.charAt(0, newline)) == true

@test
def lineSeparator02(): Bool =
    let newline = String.lineSeparator();
    String.length(newline) == 1 or String.length(newline) == 2

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def toArray01(): Bool = region rc {
    Array.sameElements(String.toArray(rc, ""), Array#{} @ rc)
}

@test
def toArray02(): Bool = region rc {
    Array.sameElements(String.toArray(rc, "ABC"), Array#{'A', 'B', 'C'} @ rc)
}

@test
def toArray03(): Bool = region rc {
    Array.sameElements(String.toArray(rc, "1 2 3"), Array#{'1', ' ', '2', ' ', '3'} @ rc)
}

/////////////////////////////////////////////////////////////////////////////
// toVector                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toVector01(): Bool =
    String.toVector("") == Vector#{}

@test
def toVector02(): Bool =
    String.toVector("ABC") == Vector#{'A', 'B', 'C'}

@test
def toVector03(): Bool =
    String.toVector("1 2 3") == Vector#{'1', ' ', '2', ' ', '3'}

/////////////////////////////////////////////////////////////////////////////
// toRegex                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def toRegex01(): Bool =
    String.toRegex("\\p{Alpha}+") |> Result.isOk

@test
def toRegex02(): Bool =
    String.toRegex("\\p{Alpha+") |> Result.isErr

/////////////////////////////////////////////////////////////////////////////
// toRegexWithFlags                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def compileWithFlags01(): Bool =
    use Regex.Flag.{CaseInsenstive};
    String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha}+") |> Result.isOk

@test
def compileWithFlags02(): Bool =
    use Regex.Flag.{CaseInsenstive};
    String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha+") |> Result.isErr


@test
def compileWithFlags03(): Bool =
    use Regex.Flag.{CaseInsenstive, Multiline};
    String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Alpha}+") |> Result.isOk

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def init01(): Bool = String.init(_ -> 'A', 0) == ""

@test
def init02(): Bool = String.init(_ -> 'A', -1) == ""

@test
def init03(): Bool = String.init(_ -> 'A', 1) == "A"

@test
def init04(): Bool = String.init(_ -> 'A', 2) == "AA"

@test
def init05(): Bool =
    let s = String.init(_ -> 'A', 50);
    String.length(s) == 50

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def flatten01(): Bool = String.flatten(Nil) == ""

@test
def flatten02(): Bool = String.flatten("" :: Nil) == ""

@test
def flatten03(): Bool = String.flatten("a" :: Nil) == "a"

@test
def flatten04(): Bool = String.flatten("ab" :: Nil) == "ab"

@test
def flatten05(): Bool = String.flatten("" :: "" :: Nil) == ""

@test
def flatten06(): Bool = String.flatten("a" :: "" :: Nil) == "a"

@test
def flatten07(): Bool = String.flatten("" :: "a" :: Nil) == "a"

@test
def flatten08(): Bool = String.flatten("a" :: "b" :: Nil) == "ab"

@test
def flatten09(): Bool = String.flatten("ab" :: "cde" :: Nil) == "abcde"

@test
def flatten10(): Bool = String.flatten("a" :: "bc" :: "d" :: Nil) == "abcd"

/////////////////////////////////////////////////////////////////////////////
// intercalate                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def intercalate01(): Bool = String.intercalate("-", Nil) == ""

@test
def intercalate02(): Bool = String.intercalate("-", "" :: Nil) == ""

@test
def intercalate03(): Bool = String.intercalate("-", "a" :: Nil) == "a"

@test
def intercalate04(): Bool = String.intercalate("-", "ab" :: Nil) == "ab"

@test
def intercalate05(): Bool = String.intercalate("-", "" :: "" :: Nil) == "-"

@test
def intercalate06(): Bool = String.intercalate("-", "a" :: "" :: Nil) == "a-"

@test
def intercalate07(): Bool = String.intercalate("-", "" :: "a" :: Nil) == "-a"

@test
def intercalate08(): Bool = String.intercalate("-", "a" :: "b" :: Nil) == "a-b"

@test
def intercalate09(): Bool = String.intercalate("-", "ab" :: "cde" :: Nil) == "ab-cde"

@test
def intercalate10(): Bool = String.intercalate("-", "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@test
def intercalate11(): Bool = String.intercalate("", "a" :: "bc" :: "d" :: Nil) == "abcd"

@test
def intercalate12(): Bool = String.intercalate("<>", "a" :: "bc" :: "d" :: Nil) == "a<>bc<>d"

/////////////////////////////////////////////////////////////////////////////
// intercalateChar                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def intercalateChar01(): Bool = String.intercalateChar('-', Nil) == ""

@test
def intercalateChar02(): Bool = String.intercalateChar('-', "" :: Nil) == ""

@test
def intercalateChar03(): Bool = String.intercalateChar('-', "a" :: Nil) == "a"

@test
def intercalateChar04(): Bool = String.intercalateChar('-', "ab" :: Nil) == "ab"

@test
def intercalateChar05(): Bool = String.intercalateChar('-', "" :: "" :: Nil) == "-"

@test
def intercalateChar06(): Bool = String.intercalateChar('-', "a" :: "" :: Nil) == "a-"

@test
def intercalateChar07(): Bool = String.intercalateChar('-', "" :: "a" :: Nil) == "-a"

@test
def intercalateChar08(): Bool = String.intercalateChar('-', "a" :: "b" :: Nil) == "a-b"

@test
def intercalateChar09(): Bool = String.intercalateChar('-', "ab" :: "cde" :: Nil) == "ab-cde"

@test
def intercalateChar10(): Bool = String.intercalateChar('-', "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@test
def intercalateChar11(): Bool = String.intercalateChar(' ', "a" :: "bc" :: "d" :: Nil) == "a bc d"

@test
def intercalateChar12(): Bool = String.intercalateChar('/', "a" :: "bc" :: "d" :: Nil) == "a/bc/d"

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "") == ""

@test
def map02(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "a") == "f"

@test
def map03(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "A") == "t"

@test
def map04(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "ab") == "ff"

@test
def map05(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "aB") == "ft"

@test
def map06(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "Ab") == "tf"

@test
def map07(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "AB") == "tt"

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def mapWithIndex01(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "") == ""

@test
def mapWithIndex02(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "a") == "f"

@test
def mapWithIndex03(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "A") == "t"

@test
def mapWithIndex04(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "aB") == "fT"

@test
def mapWithIndex05(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "ab") == "fF"

@test
def mapWithIndex06(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "AB") == "tT"

@test
def mapWithIndex07(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "Ab") == "tF"

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def reverse01(): Bool =
    String.reverse("") == ""

@test
def reverse02(): Bool =
    String.reverse("A") == "A"

@test
def reverse03(): Bool =
    String.reverse("Hello World") == "dlroW olleH"

@test
def reverse04(): Bool =
    String.reverse("Hello World!") == "!dlroW olleH"

@test
def reverse05(): Bool =
    String.reverse("Hello World!") != "!dlroW OLLEH"

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def rotateLeft01(): Bool = String.rotateLeft(0, "") == ""

@test
def rotateLeft02(): Bool = String.rotateLeft(1, "") == ""

@test
def rotateLeft03(): Bool = String.rotateLeft(0, "a") == "a"

@test
def rotateLeft04(): Bool = String.rotateLeft(0, "ab") == "ab"

@test
def rotateLeft05(): Bool = String.rotateLeft(1, "ab") == "ba"

@test
def rotateLeft06(): Bool = String.rotateLeft(2, "ab") == "ab"

@test
def rotateLeft07(): Bool = String.rotateLeft(3, "ab") == "ba"

@test
def rotateLeft08(): Bool = String.rotateLeft(-1, "ab") == "ba"

@test
def rotateLeft09(): Bool = String.rotateLeft(0, "abc") == "abc"

@test
def rotateLeft10(): Bool = String.rotateLeft(1, "abc") == "bca"

@test
def rotateLeft11(): Bool = String.rotateLeft(2, "abc") == "cab"

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def rotateRight01(): Bool = String.rotateRight(0, "") == ""

@test
def rotateRight02(): Bool = String.rotateRight(1, "") == ""

@test
def rotateRight03(): Bool = String.rotateRight(0, "a") == "a"

@test
def rotateRight04(): Bool = String.rotateRight(0, "ab") == "ab"

@test
def rotateRight05(): Bool = String.rotateRight(1, "ab") == "ba"

@test
def rotateRight06(): Bool = String.rotateRight(2, "ab") == "ab"

@test
def rotateRight07(): Bool = String.rotateRight(3, "ab") == "ba"

@test
def rotateRight08(): Bool = String.rotateRight(-1, "ab") == "ba"

@test
def rotateRight09(): Bool = String.rotateRight(0, "abc") == "abc"

@test
def rotateRight10(): Bool = String.rotateRight(1, "abc") == "cab"

@test
def rotateRight11(): Bool = String.rotateRight(2, "abc") == "bca"

/////////////////////////////////////////////////////////////////////////////
// isMatch                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isMatch01(): Bool = String.isMatch({regex = ".*"}, "") == true

@test
def isMatch02(): Bool = String.isMatch({regex = ".+"}, "") == false

@test
def isMatch03(): Bool = String.isMatch({regex = "[a]*"}, "") == true

@test
def isMatch04(): Bool = String.isMatch({regex = "[a]+"}, "") == false

@test
def isMatch05(): Bool = String.isMatch({regex = "[a]*"}, "a") == true

@test
def isMatch06(): Bool = String.isMatch({regex = "[a]*"}, "aa") == true

@test
def isMatch07(): Bool = String.isMatch({regex = "[a]*"}, "aab") == false

@test
def isMatch08(): Bool = String.isMatch({regex = "[a]*b"}, "aab") == true

@test
def isSMatch09(): Bool = String.isMatch({regex = "[b]+"}, "aab") == false

@test
def isMatch10(): Bool = String.isMatch({regex = "[b]+"}, "bbbc") == false

@test
def isMatch11(): Bool = String.isMatch({regex = "[b]+"}, "aabbbc") == false

/////////////////////////////////////////////////////////////////////////////
// isSubmatch                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isSubmatch01(): Bool = String.isSubmatch({regex = ".*"}, "") == true

@test
def isSubmatch02(): Bool = String.isSubmatch({regex = ".+"}, "") == false

@test
def isSubmatch03(): Bool = String.isSubmatch({regex = "[a]*"}, "") == true

@test
def isSubmatch04(): Bool = String.isSubmatch({regex = "[a]+"}, "") == false

@test
def isSubmatch05(): Bool = String.isSubmatch({regex = "[a]*"}, "a") == true

@test
def isSubmatch06(): Bool = String.isSubmatch({regex = "[a]*"}, "aa") == true

@test
def isSubmatch07(): Bool = String.isSubmatch({regex = "[a]*"}, "aab") == true

@test
def isSubmatch08(): Bool = String.isSubmatch({regex = "[a]*b"}, "aab") == true

@test
def isSubmatch09(): Bool = String.isSubmatch({regex = "[b]+"}, "aab") == true

@test
def isSubmatch10(): Bool = String.isSubmatch({regex = "[b]+"}, "bbbc") == true

@test
def isSubmatch11(): Bool = String.isSubmatch({regex = "[b]+"}, "aabbbc") == true

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def slice01(): Bool = String.slice(start = 0, end = 1, "") == ""

@test
def slice02(): Bool = String.slice(start = 0, end = 1, "a") == "a"

@test
def slice03(): Bool = String.slice(start = 1, end = 2, "a") == ""

@test
def slice04(): Bool = String.slice(start = 0, end = 2, "a") == ""

@test
def slice05(): Bool = String.slice(start = -1, end = 0, "a") == ""

@test
def slice06(): Bool = String.slice(start = 0, end = 1, "ab") == "a"

@test
def slice07(): Bool = String.slice(start = 1, end = 2, "ab") == "b"

@test
def slice08(): Bool = String.slice(start = 0, end = 2, "ab") == "ab"

@test
def slice09(): Bool = String.slice(start = 0, end = 3, "ab") == ""

@test
def slice10(): Bool = String.slice(start = -1, end = 2, "ab") == ""

@test
def slice11(): Bool = String.slice(start = 0, end = 1, "abc") == "a"

@test
def slice12(): Bool = String.slice(start = 1, end = 2, "abc") == "b"

@test
def slice13(): Bool = String.slice(start = 2, end = 3, "abc") == "c"

@test
def slice14(): Bool = String.slice(start = 0, end = 2, "abc") == "ab"

@test
def slice15(): Bool = String.slice(start = 1, end = 3, "abc") == "bc"

@test
def slice16(): Bool = String.slice(start = 0, end = 3, "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// sliceLeft                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def sliceLeft01(): Bool = String.sliceLeft(end = 0, "") == ""

@test
def sliceLeft02(): Bool = String.sliceLeft(end = 1, "") == ""

@test
def sliceLeft03(): Bool = String.sliceLeft(end = 1, "a") == "a"

@test
def sliceLeft04(): Bool = String.sliceLeft(end = 0, "a") == ""

@test
def sliceLeft05(): Bool = String.sliceLeft(end = 2, "a") == ""

@test
def sliceLeft06(): Bool = String.sliceLeft(end = 1, "ab") == "a"

@test
def sliceLeft07(): Bool = String.sliceLeft(end = 2, "ab") == "ab"

@test
def sliceLeft08(): Bool = String.sliceLeft(end = 0, "ab") == ""

@test
def sliceLeft09(): Bool = String.sliceLeft(end = 3, "ab") == ""

@test
def sliceLeft10(): Bool = String.sliceLeft(end = -1, "ab") == ""

@test
def sliceLeft11(): Bool = String.sliceLeft(end = 1, "abc") == "a"

@test
def sliceLeft12(): Bool = String.sliceLeft(end = 2, "abc") == "ab"

@test
def sliceLeft13(): Bool = String.sliceLeft(end = 3, "abc") == "abc"

@test
def sliceLeft14(): Bool = String.sliceLeft(end = 0, "abc") == ""

@test
def sliceLeft15(): Bool = String.sliceLeft(end = 4, "abc") == ""

@test
def sliceLeft16(): Bool = String.sliceLeft(end = -1, "abc") == ""

/////////////////////////////////////////////////////////////////////////////
// sliceRight                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def sliceRight01(): Bool = String.sliceRight(start = 0, "") == ""

@test
def sliceRight02(): Bool = String.sliceRight(start = 1, "") == ""

@test
def sliceRight03(): Bool = String.sliceRight(start = 1, "a") == ""

@test
def sliceRight04(): Bool = String.sliceRight(start = 0, "a") == "a"

@test
def sliceRight05(): Bool = String.sliceRight(start = 1, "a") == ""

@test
def sliceRight06(): Bool = String.sliceRight(start = 0, "ab") == "ab"

@test
def sliceRight07(): Bool = String.sliceRight(start = 1, "ab") == "b"

@test
def sliceRight08(): Bool = String.sliceRight(start = -1, "ab") == ""

@test
def sliceRight09(): Bool = String.sliceRight(start = 2, "ab") == ""

@test
def sliceRight10(): Bool = String.sliceRight(start = 0, "abc") == "abc"

@test
def sliceRight11(): Bool = String.sliceRight(start = 1, "abc") == "bc"

@test
def sliceRight12(): Bool = String.sliceRight(start = 2, "abc") == "c"

@test
def sliceRight13(): Bool = String.sliceRight(start = 3, "abc") == ""

@test
def sliceRight14(): Bool = String.sliceRight(start = -1, "abc") == ""

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfLeft01(): Bool = String.findIndexOfLeft(Char.isUpperCase, "") == None

@test
def findIndexOfLeft02(): Bool = String.findIndexOfLeft(Char.isUpperCase, "a") == None

@test
def findIndexOfLeft03(): Bool = String.findIndexOfLeft(Char.isUpperCase, "A") == Some(0)

@test
def findIndexOfLeft04(): Bool = String.findIndexOfLeft(Char.isUpperCase, "ab") == None

@test
def findIndexOfLeft05(): Bool = String.findIndexOfLeft(Char.isUpperCase, "Ab") == Some(0)

@test
def findIndexOfLeft06(): Bool = String.findIndexOfLeft(Char.isUpperCase, "aB") == Some(1)

@test
def findIndexOfLeft07(): Bool = String.findIndexOfLeft(Char.isUpperCase, "AB") == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfRight01(): Bool = String.findIndexOfRight(Char.isUpperCase, "") == None

@test
def findIndexOfRight02(): Bool = String.findIndexOfRight(Char.isUpperCase, "a") == None

@test
def findIndexOfRight03(): Bool = String.findIndexOfRight(Char.isUpperCase, "A") == Some(0)

@test
def findIndexOfRight04(): Bool = String.findIndexOfRight(Char.isUpperCase, "ab") == None

@test
def findIndexOfRight05(): Bool = String.findIndexOfRight(Char.isUpperCase, "Ab") == Some(0)

@test
def findIndexOfRight06(): Bool = String.findIndexOfRight(Char.isUpperCase, "aB") == Some(1)

@test
def findIndexOfRight07(): Bool = String.findIndexOfRight(Char.isUpperCase, "AB") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// findIndices                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndices01(): Bool = String.findIndices(Char.isDigit, "") == Nil

@test
def findIndices02(): Bool = String.findIndices(Char.isDigit, "a") == Nil

@test
def findIndices03(): Bool = String.findIndices(Char.isDigit, "1") == 0 :: Nil

@test
def findIndices04(): Bool = String.findIndices(Char.isDigit, "ab") == Nil

@test
def findIndices05(): Bool = String.findIndices(Char.isDigit, "1b") == 0 :: Nil

@test
def findIndices06(): Bool = String.findIndices(Char.isDigit, "a2") == 1 :: Nil

@test
def findIndices07(): Bool = String.findIndices(Char.isDigit, "12") == 0 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// indices                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indices01(): Bool = String.indices({substr = ""}, "") == Nil

@test
def indices02(): Bool = String.indices({substr = ""}, "a::b::c") == Nil

@test
def indices03(): Bool = String.indices({substr = "::"}, "") == Nil

@test
def indices04(): Bool = String.indices({substr = "::"}, "a") == Nil

@test
def indices05(): Bool = String.indices({substr = "::"}, "::") == 0 :: Nil

@test
def indices06(): Bool = String.indices({substr = "::"}, "ab") == Nil

@test
def indices07(): Bool = String.indices({substr = "::"}, "::a") == 0 :: Nil

@test
def indices08(): Bool = String.indices({substr = "::"}, "a::") == 1 :: Nil

@test
def indices09(): Bool = String.indices({substr = "::"}, "a::b::") == 1 :: 4 :: Nil

@test
def indices10(): Bool = String.indices({substr = "::"}, "a::b::c") == 1 :: 4 :: Nil

@test
def indices11(): Bool = String.indices({substr = "::"}, "a::b::c:") == 1 :: 4 :: Nil

@test
def indices12(): Bool = String.indices({substr = "::"}, ":a::b::c") == 2 :: 5 :: Nil

@test
def indices13(): Bool = String.indices({substr = "::"}, "::::") == 0 :: 2 :: Nil


/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def take01(): Bool = String.take(-1, "") == ""

@test
def take02(): Bool = String.take(0, "") == ""

@test
def take03(): Bool = String.take(1, "") == ""

@test
def take04(): Bool = String.take(-1, "a") == ""

@test
def take05(): Bool = String.take(0, "a") == ""

@test
def take06(): Bool = String.take(1, "a") == "a"

@test
def take07(): Bool = String.take(2, "a") == "a"

@test
def take08(): Bool = String.take(0, "ab") == ""

@test
def take09(): Bool = String.take(1, "ab") == "a"

@test
def take10(): Bool = String.take(2, "ab") == "ab"

@test
def take11(): Bool = String.take(2, "abcdef") == "ab"

@test
def take12(): Bool = String.take(4, "abcdef") == "abcd"

@test
def take13(): Bool = String.take(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def takeLeft01(): Bool = String.takeLeft(-1, "") == ""

@test
def takeLeft02(): Bool = String.takeLeft(0, "") == ""

@test
def takeLeft03(): Bool = String.takeLeft(1, "") == ""

@test
def takeLeft04(): Bool = String.takeLeft(-1, "a") == ""

@test
def takeLeft05(): Bool = String.takeLeft(0, "a") == ""

@test
def takeLeft06(): Bool = String.takeLeft(1, "a") == "a"

@test
def takeLeft07(): Bool = String.takeLeft(2, "a") == "a"

@test
def takeLeft08(): Bool = String.takeLeft(0, "ab") == ""

@test
def takeLeft09(): Bool = String.takeLeft(1, "ab") == "a"

@test
def takeLeft10(): Bool = String.takeLeft(2, "ab") == "ab"

@test
def takeLeft11(): Bool = String.takeLeft(2, "abcdef") == "ab"

@test
def takeLeft12(): Bool = String.takeLeft(4, "abcdef") == "abcd"

@test
def takeLeft13(): Bool = String.takeLeft(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeRight01(): Bool = String.takeRight(-1, "") == ""

@test
def takeRight02(): Bool = String.takeRight(0, "") == ""

@test
def takeRight03(): Bool = String.takeRight(1, "") == ""

@test
def takeRight04(): Bool = String.takeRight(-1, "a") == ""

@test
def takeRight05(): Bool = String.takeRight(0, "a") == ""

@test
def takeRight06(): Bool = String.takeRight(1, "a") == "a"

@test
def takeRight07(): Bool = String.takeRight(2, "a") == "a"

@test
def takeRight08(): Bool = String.takeRight(0, "ab") == ""

@test
def takeRight09(): Bool = String.takeRight(1, "ab") == "b"

@test
def takeRight10(): Bool = String.takeRight(2, "ab") == "ab"

@test
def takeRight11(): Bool = String.takeRight(2, "abcdef") == "ef"

@test
def takeRight12(): Bool = String.takeRight(4, "abcdef") == "cdef"

@test
def takeRight13(): Bool = String.takeRight(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def drop01(): Bool = String.drop(-1, "") == ""

@test
def drop02(): Bool = String.drop(0, "") == ""

@test
def drop03(): Bool = String.drop(1, "") == ""

@test
def drop04(): Bool = String.drop(-1, "a") == "a"

@test
def drop05(): Bool = String.drop(0, "a") == "a"

@test
def drop06(): Bool = String.drop(1, "a") == ""

@test
def drop07(): Bool = String.drop(2, "a") == ""

@test
def drop08(): Bool = String.drop(0, "ab") == "ab"

@test
def drop09(): Bool = String.drop(1, "ab") == "b"

@test
def drop10(): Bool = String.drop(2, "ab") == ""

@test
def drop11(): Bool = String.drop(2, "abcdef") == "cdef"

@test
def drop12(): Bool = String.drop(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def dropLeft01(): Bool = String.dropLeft(-1, "") == ""

@test
def dropLeft02(): Bool = String.dropLeft(0, "") == ""

@test
def dropLeft03(): Bool = String.dropLeft(1, "") == ""

@test
def dropLeft04(): Bool = String.dropLeft(-1, "a") == "a"

@test
def dropLeft05(): Bool = String.dropLeft(0, "a") == "a"

@test
def dropLeft06(): Bool = String.dropLeft(1, "a") == ""

@test
def dropLeft07(): Bool = String.dropLeft(2, "a") == ""

@test
def dropLeft08(): Bool = String.dropLeft(0, "ab") == "ab"

@test
def dropLeft09(): Bool = String.dropLeft(1, "ab") == "b"

@test
def dropLeft10(): Bool = String.dropLeft(2, "ab") == ""

@test
def dropLeft11(): Bool = String.dropLeft(2, "abcdef") == "cdef"

@test
def dropLeft12(): Bool = String.dropLeft(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropRight01(): Bool = String.dropRight(-1, "") == ""

@test
def dropRight02(): Bool = String.dropRight(0, "") == ""

@test
def dropRight03(): Bool = String.dropRight(1, "") == ""

@test
def dropRight04(): Bool = String.dropRight(-1, "a") == "a"

@test
def dropRight05(): Bool = String.dropRight(0, "a") == "a"

@test
def dropRight06(): Bool = String.dropRight(1, "a") == ""

@test
def dropRight07(): Bool = String.dropRight(2, "a") == ""

@test
def dropRight08(): Bool = String.dropRight(0, "ab") == "ab"

@test
def dropRight09(): Bool = String.dropRight(1, "ab") == "a"

@test
def dropRight10(): Bool = String.dropRight(2, "ab") == ""

@test
def dropRight11(): Bool = String.dropRight(2, "abcdef") == "abcd"

@test
def dropRight12(): Bool = String.dropRight(4, "abcdef") == "ab"

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhile01(): Bool = String.takeWhile(Char.isUpperCase, "") == ""

@test
def takeWhile02(): Bool = String.takeWhile(Char.isUpperCase, "a") == ""

@test
def takeWhile03(): Bool = String.takeWhile(Char.isUpperCase, "A") == "A"

@test
def takeWhile04(): Bool = String.takeWhile(Char.isUpperCase, "ab") == ""

@test
def takeWhile05(): Bool = String.takeWhile(Char.isUpperCase, "aB") == ""

@test
def takeWhile06(): Bool = String.takeWhile(Char.isUpperCase, "Ab") == "A"

@test
def takeWhile07(): Bool = String.takeWhile(Char.isUpperCase, "AB") == "AB"

@test
def takeWhile08(): Bool = String.takeWhile(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileLeft01(): Bool = String.takeWhileLeft(Char.isUpperCase, "") == ""

@test
def takeWhileLeft02(): Bool = String.takeWhileLeft(Char.isUpperCase, "a") == ""

@test
def takeWhileLeft03(): Bool = String.takeWhileLeft(Char.isUpperCase, "A") == "A"

@test
def takeWhileLeft04(): Bool = String.takeWhileLeft(Char.isUpperCase, "ab") == ""

@test
def takeWhileLeft05(): Bool = String.takeWhileLeft(Char.isUpperCase, "aB") == ""

@test
def takeWhileLeft06(): Bool = String.takeWhileLeft(Char.isUpperCase, "Ab") == "A"

@test
def takeWhileLeft07(): Bool = String.takeWhileLeft(Char.isUpperCase, "AB") == "AB"

@test
def takeWhileLeft08(): Bool = String.takeWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileRight01(): Bool = String.takeWhileRight(Char.isUpperCase, "") == ""

@test
def takeWhileRight02(): Bool = String.takeWhileRight(Char.isUpperCase, "a") == ""

@test
def takeWhileRight03(): Bool = String.takeWhileRight(Char.isUpperCase, "A") == "A"

@test
def takeWhileRight04(): Bool = String.takeWhileRight(Char.isUpperCase, "ab") == ""

@test
def takeWhileRight05(): Bool = String.takeWhileRight(Char.isUpperCase, "aB") == "B"

@test
def takeWhileRight06(): Bool = String.takeWhileRight(Char.isUpperCase, "Ab") == ""

@test
def takeWhileRight07(): Bool = String.takeWhileRight(Char.isUpperCase, "AB") == "AB"

@test
def takeWhileRight08(): Bool = String.takeWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "K"

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhile01(): Bool = String.dropWhile(Char.isUpperCase, "") == ""

@test
def dropWhile02(): Bool = String.dropWhile(Char.isUpperCase, "a") == "a"

@test
def dropWhile03(): Bool = String.dropWhile(Char.isUpperCase, "A") == ""

@test
def dropWhile04(): Bool = String.dropWhile(Char.isUpperCase, "ab") == "ab"

@test
def dropWhile05(): Bool = String.dropWhile(Char.isUpperCase, "aB") == "aB"

@test
def dropWhile06(): Bool = String.dropWhile(Char.isUpperCase, "Ab") == "b"

@test
def dropWhile07(): Bool = String.dropWhile(Char.isUpperCase, "AB") == ""

@test
def dropWhile08(): Bool = String.dropWhile(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileLeft01(): Bool = String.dropWhileLeft(Char.isUpperCase, "") == ""

@test
def dropWhileLeft02(): Bool = String.dropWhileLeft(Char.isUpperCase, "a") == "a"

@test
def dropWhileLeft03(): Bool = String.dropWhileLeft(Char.isUpperCase, "A") == ""

@test
def dropWhileLeft04(): Bool = String.dropWhileLeft(Char.isUpperCase, "ab") == "ab"

@test
def dropWhileLeft05(): Bool = String.dropWhileLeft(Char.isUpperCase, "aB") == "aB"

@test
def dropWhileLeft06(): Bool = String.dropWhileLeft(Char.isUpperCase, "Ab") == "b"

@test
def dropWhileLeft07(): Bool = String.dropWhileLeft(Char.isUpperCase, "AB") == ""

@test
def dropWhileLeft08(): Bool = String.dropWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileRight01(): Bool = String.dropWhileRight(Char.isUpperCase, "") == ""

@test
def dropWhileRight02(): Bool = String.dropWhileRight(Char.isUpperCase, "a") == "a"

@test
def dropWhileRight03(): Bool = String.dropWhileRight(Char.isUpperCase, "A") == ""

@test
def dropWhileRight04(): Bool = String.dropWhileRight(Char.isUpperCase, "ab") == "ab"

@test
def dropWhileRight05(): Bool = String.dropWhileRight(Char.isUpperCase, "aB") == "a"

@test
def dropWhileRight06(): Bool = String.dropWhileRight(Char.isUpperCase, "Ab") == "Ab"

@test
def dropWhileRight07(): Bool = String.dropWhileRight(Char.isUpperCase, "AB") == ""

@test
def dropWhileRight08(): Bool = String.dropWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "ABcDefgHIj"

/////////////////////////////////////////////////////////////////////////////
// dropWhileAround                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileAround01(): Bool = String.dropWhileRight(Char.isUpperCase, "") == ""

@test
def dropWhileAround02(): Bool = String.dropWhileAround(Char.isUpperCase, "a") == "a"

@test
def dropWhileAround03(): Bool = String.dropWhileAround(Char.isUpperCase, "A") == ""

@test
def dropWhileAround04(): Bool = String.dropWhileAround(Char.isUpperCase, "ab") == "ab"

@test
def dropWhileAround05(): Bool = String.dropWhileAround(Char.isUpperCase, "aB") == "a"

@test
def dropWhileAround06(): Bool = String.dropWhileAround(Char.isUpperCase, "Ab") == "b"

@test
def dropWhileAround07(): Bool = String.dropWhileAround(Char.isUpperCase, "AB") == ""

@test
def dropWhileAround08(): Bool = String.dropWhileAround(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIj"

/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def splitAt01(): Bool = String.splitAt(0, "") == ("", "")

@test
def splitAt02(): Bool = String.splitAt(1, "") == ("", "")

@test
def splitAt03(): Bool = String.splitAt(-1, "") == ("", "")

@test
def splitAt04(): Bool = String.splitAt(0, "a") == ("", "a")

@test
def splitAt05(): Bool = String.splitAt(1, "a") == ("a", "")

@test
def splitAt06(): Bool = String.splitAt(-1, "a") == ("", "a")

@test
def splitAt07(): Bool = String.splitAt(0, "ab") == ("", "ab")

@test
def splitAt08(): Bool = String.splitAt(1, "ab") == ("a", "b")

@test
def splitAt09(): Bool = String.splitAt(2, "ab") == ("ab", "")

@test
def splitAt10(): Bool = String.splitAt(-1, "ab") == ("", "ab")

@test
def splitAt11(): Bool = String.splitAt(3, "ab") == ("ab", "")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft01(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "", "")  == ""

@test
def foldLeft02(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "a") == "a"

@test
def foldLeft03(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "ab") == "ab"

@test
def foldLeft04(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abc") == "abc"

@test
def foldLeft05(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcd") == "abcd"

@test
def foldLeft06(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight01(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "")  == ""

@test
def foldRight02(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "a") == "a"

@test
def foldRight03(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "ab") == "ab"

@test
def foldRight04(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abc") == "abc"

@test
def foldRight05(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcd") == "abcd"

@test
def foldRight06(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft201(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "", "") == ""

@test
def foldLeft202(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "_") == "a"

@test
def foldLeft203(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "A") == "A"

@test
def foldLeft204(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "__") == "ab"

@test
def foldLeft205(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A_") == "Ab"

@test
def foldLeft206(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "AB") == "AB"

@test
def foldLeft207(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "") == ""

@test
def foldLeft208(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "_") == "a"

@test
def foldLeft209(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A") == "A"

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight201(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "", "") == ""

@test
def foldRight202(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "_") == "a"

@test
def foldRight203(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "A") == "A"

@test
def foldRight204(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "__") == "ab"

@test
def foldRight205(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "A_") == "Ab"

@test
def foldRight206(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "AB") == "AB"

@test
def foldRight207(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "") == ""

@test
def foldRight208(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "_") == "b"

@test
def foldRight209(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "B") == "B"

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 0) == "aaaaa"

@test
def unfold02(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 1) == "aaaa"

@test
def unfold03(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 2) == "aaa"

@test
def unfold04(): Bool =
    String.unfold(x -> if (x < 5) Some((('a', x+1))) else None, 3) == "aa"

@test
def unfold05(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 4) == "a"

@test
def unfold06(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 5) == ""

@test
def unfold07(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 6) == ""

@test
def unfold08(): Bool =
    String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, -1) == "aaaaaa"

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

///
/// Converts Int32 in ranges [65;90] (inclusive) and [97;122] (inclusive) to a Char.
///
def toChar(step: Int32): Char = match step {
    case 65  => 'A'
    case 66  => 'B'
    case 67  => 'C'
    case 68  => 'D'
    case 69  => 'E'
    case 70  => 'F'
    case 71  => 'G'
    case 72  => 'H'
    case 73  => 'I'
    case 74  => 'J'
    case 75  => 'K'
    case 76  => 'L'
    case 77  => 'M'
    case 78  => 'N'
    case 79  => 'O'
    case 80  => 'P'
    case 81  => 'Q'
    case 82  => 'R'
    case 83  => 'S'
    case 84  => 'T'
    case 85  => 'U'
    case 86  => 'V'
    case 87  => 'W'
    case 88  => 'X'
    case 89  => 'Y'
    case 90  => 'Z'
    case 97  => 'a'
    case 98  => 'b'
    case 99  => 'c'
    case 100 => 'd'
    case 101 => 'e'
    case 102 => 'f'
    case 103 => 'g'
    case 104 => 'h'
    case 105 => 'i'
    case 106 => 'j'
    case 107 => 'k'
    case 108 => 'l'
    case 109 => 'm'
    case 110 => 'n'
    case 111 => 'o'
    case 112 => 'p'
    case 113 => 'q'
    case 114 => 'r'
    case 115 => 's'
    case 116 => 't'
    case 117 => 'u'
    case 118 => 'v'
    case 119 => 'w'
    case 120 => 'x'
    case 121 => 'y'
    case 122 => 'z'
    case _   => bug!("toChar called with bad input!")
}

@test
def unfoldWithIter01(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (true)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 1, x);
            c
        };
    String.unfoldWithIter(step) == ""
}

@test
def unfoldWithIter02(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) > 0)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 1, x);
            c
        };
    String.unfoldWithIter(step) == "a"
}

@test
def unfoldWithIter03(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) > 1)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 1, x);
            c
        };
    String.unfoldWithIter(step) == "ab"
}

@test
def unfoldWithIter04(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 1, x);
            c
        };
    String.unfoldWithIter(step) == "abcdefghij"
}

@test
def unfoldWithIter05(): Bool = region rc {
    let x = Ref.fresh(rc, 5);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 1, x);
            c
        };
    String.unfoldWithIter(step) == "fghij"
}

@test
def unfoldWithIter06(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c = Some(toChar(Ref.get(x) + 97));
            Ref.put(Ref.get(x) + 2, x);
            c
        };
    String.unfoldWithIter(step) == "acegi"
}

/////////////////////////////////////////////////////////////////////////////
// unfoldString                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldString01(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 0) == "ab.ab.ab.ab.ab."

@test
def unfoldString02(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 1) == "ab.ab.ab.ab."

@test
def unfoldString03(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 2) == "ab.ab.ab."

@test
def unfoldString04(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 3) == "ab.ab."

@test
def unfoldString05(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 4) == "ab."

@test
def unfoldString06(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("", x+1)) else None, 5) == ""

@test
def unfoldString07(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 6) == ""

@test
def unfoldString08(): Bool =
    String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, -1) == "ab.ab.ab.ab.ab.ab."


/////////////////////////////////////////////////////////////////////////////
// unfoldStringWithIter                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def unfoldStringWithIter01(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (true)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 1, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == ""
}

@test
def unfoldStringWithIter02(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) > 0)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 1, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa."
}

@test
def unfoldStringWithIter03(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) > 1)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 1, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Bb."
}

@test
def unfoldStringWithIter04(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 1, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Bb.Cc.Dd.Ee.Ff.Gg.Hh.Ii.Jj."
}

@test
def unfoldStringWithIter05(): Bool = region rc {
    let x = Ref.fresh(rc, 5);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 1, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Ff.Gg.Hh.Ii.Jj."
}

@test
def unfoldStringWithIter06(): Bool = region rc {
    let x = Ref.fresh(rc, 0);
    let step = () ->
        if (Ref.get(x) >= 10)
            None
        else {
            let c1 = toChar(Ref.get(x) + 65);
            let c2 = toChar(Ref.get(x) + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            Ref.put(Ref.get(x) + 2, x);
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Cc.Ee.Gg.Ii."
}

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def exists01(): Bool = String.exists(Char.isUpperCase, "") == false

@test
def exists02(): Bool = String.exists(Char.isUpperCase, "a") == false

@test
def exists03(): Bool = String.exists(Char.isUpperCase, "A") == true

@test
def exists04(): Bool = String.exists(Char.isUpperCase, "ab") == false

@test
def exists05(): Bool = String.exists(Char.isUpperCase, "aB") == true

@test
def exists06(): Bool = String.exists(Char.isUpperCase, "Ab") == true

@test
def exists07(): Bool = String.exists(Char.isUpperCase, "AB") == true

@test
def exists08(): Bool = String.exists(Char.isUpperCase, "a*c") == false

@test
def exists09(): Bool = String.exists(Char.isUpperCase, "aBc") == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def forAll01(): Bool = String.forAll(Char.isUpperCase, "") == true

@test
def forAll02(): Bool = String.forAll(Char.isUpperCase, "a") == false

@test
def forAll03(): Bool = String.forAll(Char.isUpperCase, "A") == true

@test
def forAll04(): Bool = String.forAll(Char.isUpperCase, "a") == false

@test
def forAll05(): Bool = String.forAll(Char.isUpperCase, "aB") == false

@test
def forAll06(): Bool = String.forAll(Char.isUpperCase, "Ab") == false

@test
def forAll07(): Bool = String.forAll(Char.isUpperCase, "AB") == true

@test
def forAll08(): Bool = String.forAll(Char.isUpperCase, "a*b") == false

@test
def forAll09(): Bool = String.forAll(Char.isUpperCase, "aBc") == false

@test
def forAll10(): Bool = String.forAll(Char.isUpperCase, "ABC") == true

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isAscii01(): Bool = String.isAscii("") == true

@test
def isAscii02(): Bool = String.isAscii("a") == true

@test
def isAscii03(): Bool = String.isAscii("A") == true

@test
def isAscii04(): Bool = String.isAscii("0") == true

@test
def isAscii05(): Bool = String.isAscii("~") == true

@test
def isAscii06(): Bool = String.isAscii("Â") == false

@test
def isAscii07(): Bool = String.isAscii("†") == false

@test
def isAscii08(): Bool = String.isAscii("ab") == true

@test
def isAscii09(): Bool = String.isAscii("a†") == false

@test
def isAscii10(): Bool = String.isAscii("†b") == false

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isWhiteSpace01(): Bool = String.isWhiteSpace("") == true

@test
def isWhiteSpace02(): Bool = String.isWhiteSpace(" ") == true

@test
def isWhiteSpace03(): Bool = String.isWhiteSpace("A") == false

@test
def isWhiteSpace04(): Bool = String.isWhiteSpace("0") == false

@test
def isWhiteSpace05(): Bool = String.isWhiteSpace("~") == false

@test
def isWhiteSpace06(): Bool = String.isWhiteSpace(String.lineSeparator()) == true

@test
def isWhiteSpace07(): Bool = String.isWhiteSpace("  ") == true

/////////////////////////////////////////////////////////////////////////////
// trimLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def trimLeft01(): Bool = String.trimLeft("") == ""

@test
def trimLeft02(): Bool = String.trimLeft(" ") == ""

@test
def trimLeft03(): Bool = String.trimLeft("a") == "a"

@test
def trimLeft04(): Bool = String.trimLeft(" a") == "a"

@test
def trimLeft05(): Bool = String.trimLeft("a ") == "a "

@test
def trimLeft06(): Bool = String.trimLeft("aa") == "aa"

@test
def trimLeft07(): Bool = String.trimLeft("  aa") == "aa"

@test
def trimLeft08(): Bool = String.trimLeft("aa  ") == "aa  "

@test
def trimLeft09(): Bool = String.trimLeft(" aa ") == "aa "

@test
def trimLeft10(): Bool = String.trimLeft("  aa  ") == "aa  "

@test
def trimLeft11(): Bool = String.trimLeft(" a a ") == "a a "

@test
def trimLeft12(): Bool = String.trimLeft(String.lineSeparator() + "aa") == "aa"


/////////////////////////////////////////////////////////////////////////////
// trimRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def trimRight01(): Bool = String.trimRight("") == ""

@test
def trimRight02(): Bool = String.trimRight(" ") == ""

@test
def trimRight03(): Bool = String.trimRight("a") == "a"

@test
def trimRight04(): Bool = String.trimRight(" a") == " a"

@test
def trimRight05(): Bool = String.trimRight("a ") == "a"

@test
def trimRight06(): Bool = String.trimRight("aa") == "aa"

@test
def trimRight07(): Bool = String.trimRight("  aa") == "  aa"

@test
def trimRight08(): Bool = String.trimRight("aa  ") == "aa"

@test
def trimRight09(): Bool = String.trimRight(" aa ") == " aa"

@test
def trimRight10(): Bool = String.trimRight("  aa  ") == "  aa"

@test
def trimRight11(): Bool = String.trimRight(" a a ") == " a a"

@test
def trimRight12(): Bool = String.trimRight("aa" + String.lineSeparator()) == "aa"

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def repeat01(): Bool = String.repeat(-1, "a") == ""

@test
def repeat02(): Bool = String.repeat(0, "a") == ""

@test
def repeat03(): Bool = String.repeat(1, "a") == "a"

@test
def repeat04(): Bool = String.repeat(2, "a") == "aa"

@test
def repeat05(): Bool = String.repeat(3, "a") == "aaa"

@test
def repeat06(): Bool = String.repeat(0, "ab") == ""

@test
def repeat07(): Bool = String.repeat(1, "ab") == "ab"

@test
def repeat08(): Bool = String.repeat(2, "ab") == "abab"

@test
def repeat09(): Bool = String.repeat(3, "ab") == "ababab"

/////////////////////////////////////////////////////////////////////////////
// padLeft                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def padLeft01(): Bool = String.padLeft(0, ' ', "") == ""

@test
def padLeft02(): Bool = String.padLeft(1, ' ', "") == " "

@test
def padLeft03(): Bool = String.padLeft(2, ' ', "") == "  "

@test
def padLeft04(): Bool = String.padLeft(3, ' ', "") == "   "

@test
def padLeft05(): Bool = String.padLeft(0, ' ', "a") == "a"

@test
def padLeft06(): Bool = String.padLeft(1, ' ', "a") == "a"

@test
def padLeft07(): Bool = String.padLeft(2, ' ', "a") == " a"

@test
def padLeft08(): Bool = String.padLeft(3, ' ', "a") == "  a"

@test
def padLeft09(): Bool = String.padLeft(0, ' ', "ab") == "ab"

@test
def padLeft10(): Bool = String.padLeft(1, ' ', "ab") == "ab"

@test
def padLeft11(): Bool = String.padLeft(2, ' ', "ab") == "ab"

@test
def padLeft12(): Bool = String.padLeft(3, ' ', "ab") == " ab"

@test
def padLeft13(): Bool = String.padLeft(3, '_', "a") == "__a"

@test
def padLeft14(): Bool = String.padLeft(3, '_', "ab") == "_ab"

@test
def padLeft15(): Bool = String.padLeft(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// padRight                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def padRight01(): Bool = String.padRight(0, ' ', "") == ""

@test
def padRight02(): Bool = String.padRight(1, ' ', "") == " "

@test
def padRight03(): Bool = String.padRight(2, ' ', "") == "  "

@test
def padRight04(): Bool = String.padRight(3, ' ', "") == "   "

@test
def padRight05(): Bool = String.padRight(0, ' ', "a") == "a"

@test
def padRight06(): Bool = String.padRight(1, ' ', "a") == "a"

@test
def padRight07(): Bool = String.padRight(2, ' ', "a") == "a "

@test
def padRight08(): Bool = String.padRight(3, ' ', "a") == "a  "

@test
def padRight09(): Bool = String.padRight(0, ' ', "ab") == "ab"

@test
def padRight10(): Bool = String.padRight(1, ' ', "ab") == "ab"

@test
def padRight11(): Bool = String.padRight(2, ' ', "ab") == "ab"

@test
def padRight12(): Bool = String.padRight(3, ' ', "ab") == "ab "

@test
def padRight13(): Bool = String.padRight(3, '_', "a") == "a__"

@test
def padRight14(): Bool = String.padRight(3, '_', "ab") == "ab_"

@test
def padRight15(): Bool = String.padRight(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// center                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def center01(): Bool = String.center(0, ' ', "") == ""

@test
def center02(): Bool = String.center(1, ' ', "") == " "

@test
def center03(): Bool = String.center(2, ' ', "") == "  "

@test
def center04(): Bool = String.center(3, ' ', "") == "   "

@test
def center05(): Bool = String.center(0, ' ', "a") == "a"

@test
def center06(): Bool = String.center(1, ' ', "a") == "a"

@test
def center07(): Bool = String.center(2, ' ', "a") == "a "

@test
def center08(): Bool = String.center(3, ' ', "a") == " a "

@test
def center09(): Bool = String.center(0, ' ', "ab") == "ab"

@test
def center10(): Bool = String.center(1, ' ', "ab") == "ab"

@test
def center11(): Bool = String.center(2, ' ', "ab") == "ab"

@test
def center12(): Bool = String.center(3, ' ', "ab") == "ab "

@test
def center13(): Bool = String.center(3, '_', "a") == "_a_"

@test
def center14(): Bool = String.center(3, '_', "ab") == "ab_"

@test
def center15(): Bool = String.center(3, '_', "abc") == "abc"

@test
def center16(): Bool = String.center(7, '_', "abc") == "__abc__"

@test
def center17(): Bool = String.center(8, '_', "abc") == "__abc___"

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def replace01(): Bool = String.replace({src = "c"}, {dst = "d"}, "") == ""

@test
def replace02(): Bool = String.replace({src = "c"}, {dst = "d"}, "a") == "a"

@test
def replace03(): Bool = String.replace({src = "c"}, {dst = "d"}, "c") == "d"

@test
def replace04(): Bool = String.replace({src = "c"}, {dst = "d"}, "d") == "d"

@test
def replace05(): Bool = String.replace({src = "c"}, {dst = "d"}, "ab") == "ab"

@test
def replace06(): Bool = String.replace({src = "c"}, {dst = "d"}, "abc") == "abd"

@test
def replace07(): Bool = String.replace({src = "c"}, {dst = "d"}, "cd") == "dd"

@test
def replace08(): Bool = String.replace({src = "c"}, {dst = "d"}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceChar                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceChar01(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "") == ""

@test
def replaceChar02(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "a") == "a"

@test
def replaceChar03(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "c") == "d"

@test
def replaceChar04(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "d") == "d"

@test
def replaceChar05(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "ab") == "ab"

@test
def replaceChar06(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "abc") == "abd"

@test
def replaceChar07(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "cd") == "dd"

@test
def replaceChar08(): Bool = String.replaceChar({src = 'c'}, {dst = 'd'}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceMatches                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceMatches01(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "") == ""

@test
def replaceMatches02(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "a") == "a"

@test
def replaceMatches03(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "c") == "d"

@test
def replaceMatches04(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "d") == "d"

@test
def replaceMatches05(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "ab") == "ab"

@test
def replaceMatches06(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "abc") == "abd"

@test
def replaceMatches07(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cd") == "dd"

@test
def replaceMatches08(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceFirstMatch                                                       //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceFirstMatch01(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "") == ""

@test
def replaceFirstMatch02(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "a") == "a"

@test
def replaceFirstMatch03(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "c") == "d"

@test
def replaceFirstMatch04(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "d") == "d"

@test
def replaceFirstMatch05(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "ab") == "ab"

@test
def replaceFirstMatch06(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "abc") == "abd"

@test
def replaceFirstMatch07(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cd") == "dd"

@test
def replaceFirstMatch08(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cc") == "dc"

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def update01(): Bool = String.update(0, 'b', "") == ""

@test
def update02(): Bool = String.update(-1, 'b', "a") == "a"

@test
def update03(): Bool = String.update(0, 'b', "a") == "b"

@test
def update04(): Bool = String.update(1, 'b', "a") == "a"

@test
def update05(): Bool = String.update(0, 'e', "ab") == "eb"

@test
def update06(): Bool = String.update(1, 'e', "ab") == "ae"

@test
def update07(): Bool = String.update(2, 'e', "ab") == "ab"

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def patch01(): Bool = String.patch(0, 0, "", "") == ""

@test
def patch02(): Bool = String.patch(0, 2, "ab", "") == ""

@test
def patch03(): Bool = String.patch(0, 2, "", "ab") == "ab"

@test
def patch04(): Bool = String.patch(-3, 3, "abd", "ab") == "ab"

@test
def patch05(): Bool = String.patch(2, 3, "abd", "ab") == "ab"

@test
def patch06(): Bool = String.patch(0, 0, "", "a") == "a"

@test
def patch07(): Bool = String.patch(1, 0, "b", "a") == "a"

@test
def patch08(): Bool = String.patch(0, 1, "b", "a") == "b"

@test
def patch09(): Bool = String.patch(0, 2, "bd", "a") == "b"

@test
def patch10(): Bool = String.patch(-1, 2, "bd", "a") == "d"

@test
def patch11(): Bool = String.patch(-1, 2, "cd", "ab") == "db"

@test
def patch12(): Bool = String.patch(1, 2, "cd", "ab") == "ac"

@test
def patch13(): Bool = String.patch(-2, 2, "cd", "ab") == "ab"

@test
def patch14(): Bool = String.patch(2, 2, "cd", "ab") == "ab"

@test
def patch15(): Bool = String.patch(1, 1, "c", "ab") == "ac"

@test
def patch16(): Bool = String.patch(0, 2, "cd", "ab") == "cd"

@test
def patch17(): Bool = String.patch(0, 1, "d", "abc") == "dbc"

@test
def patch18(): Bool = String.patch(1, 1, "d", "abc") == "adc"

@test
def patch19(): Bool = String.patch(2, 1, "d", "abc") == "abd"

@test
def patch20(): Bool = String.patch(0, 2, "de", "abc") == "dec"

@test
def patch21(): Bool = String.patch(1, 2, "de", "abc") == "ade"

@test
def patch22(): Bool = String.patch(0, 2, "def", "abc") == "dec"

@test
def patch23(): Bool = String.patch(0, 3, "def", "abc") == "def"

@test
def patch24(): Bool = String.patch(2, 4, "nopq", "abcdefg") == "abnopqg"

@test
def patch25(): Bool = String.patch(-2, 4, "nopq", "abcdefg") == "pqcdefg"

@test
def patch26(): Bool = String.patch(4, 5, "nopq", "abcdefg") == "abcdnop"

@test
def patch27(): Bool = String.patch(4, 2, "nopq", "abcdefg") == "abcdnog"

@test
def patch28(): Bool = String.patch(-1, 10, "ABCDEFGH", "abcdefg") == "BCDEFGH"

/////////////////////////////////////////////////////////////////////////////
// indent                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def indent01(): Bool =
    String.indent(3, "") == ""

@test
def indent02(): Bool =
    String.indent(3, String.unlines("abc" :: Nil)) == String.unlines("   abc" :: Nil)

@test
def indent03(): Bool =
    String.indent(3, String.unlines("abc" :: "def" :: Nil)) == String.unlines("   abc" :: "   def" :: Nil)

@test
def indent04(): Bool =
    String.indent(3, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)

@test
def indent05(): Bool =
    String.indent(1, String.unlines("abc" :: Nil)) == String.unlines(" abc" :: Nil)

@test
def indent06(): Bool =
    String.indent(1, String.unlines("abc" :: "def" :: Nil)) == String.unlines(" abc" :: " def" :: Nil)

@test
def indent07(): Bool =
    String.indent(1, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines(" ab" :: " cd" :: " ef" :: Nil)

@test
def indent08(): Bool =
    String.indent(0, String.unlines("abc" :: Nil)) == String.unlines("abc" :: Nil)

@test
def indent09(): Bool =
    String.indent(0, String.unlines("abc" :: "def" :: Nil)) == String.unlines("abc" :: "def" :: Nil)

@test
def indent10(): Bool =
    String.indent(0, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines("ab" :: "cd" :: "ef" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// stripIndent                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def stripIndent01(): Bool =
    String.stripIndent(3, "") == ""

@test
def stripIndent02(): Bool =
    String.stripIndent(3, String.unlines("   abc" :: Nil)) == String.unlines("abc" :: Nil)

@test
def stripIndent03(): Bool =
    String.stripIndent(3, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("abc" :: "def" :: Nil)

@test
def stripIndent04(): Bool =
    String.stripIndent(3, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("ab" :: "cd" :: "ef" :: Nil)

@test
def stripIndent05(): Bool =
    String.stripIndent(1, String.unlines("   abc" :: Nil)) == String.unlines("  abc" :: Nil)

@test
def stripIndent06(): Bool =
    String.stripIndent(1, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("  abc" :: "  def" :: Nil)

@test
def stripIndent07(): Bool =
    String.stripIndent(1, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("  ab" :: "  cd" :: "  ef" :: Nil)

@test
def stripIndent08(): Bool =
    String.stripIndent(0, String.unlines("   abc" :: Nil)) == String.unlines("   abc" :: Nil)

@test
def stripIndent09(): Bool =
    String.stripIndent(0, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("   abc" :: "   def" :: Nil)

@test
def stripIndent10(): Bool  =
    String.stripIndent(0, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// lines                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lines01(): Bool = String.lines("") == Nil

@test
def lines02(): Bool = String.lines("a")  == "a" :: Nil

@test
def lines03(): Bool = String.lines("a" + String.lineSeparator()) == "a" :: Nil

@test
def lines04(): Bool = String.lines(String.lineSeparator() + "a") == "" :: "a" :: Nil

@test
def lines05(): Bool = String.lines("a" + String.lineSeparator() + "b") == "a" :: "b" :: Nil

@test
def lines06(): Bool = String.lines("a" + Char.toString('\n') + "b") == "a" :: "b" :: Nil

@test
def lines07(): Bool = String.lines("a" + Char.toString('\r') + Char.toString('\n') + "b") ==  "a" :: "b" :: Nil

/////////////////////////////////////////////////////////////////////////////
// unlines                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def unlines01(): Bool = String.unlines(Nil) == ""

@test
def unlines02(): Bool =
    String.unlines("" :: Nil) == String.lineSeparator()

@test
def unlines03(): Bool =
    String.unlines("a" :: Nil) == "a" + String.lineSeparator()

@test
def unlines04(): Bool =
    String.unlines("a" :: "" :: Nil) == "a" + String.lineSeparator() + String.lineSeparator()

@test
def unlines05(): Bool =
    String.unlines("" :: "b" :: Nil) == String.lineSeparator() + "b" + String.lineSeparator()

@test
def unlines06(): Bool =
    String.unlines("a" :: "b" :: Nil) == "a" + String.lineSeparator() + "b" + String.lineSeparator()

@test
def unlines07(): Bool =
    String.unlines("a" :: String.lineSeparator() :: Nil) == "a" + String.lineSeparator() + String.lineSeparator() + String.lineSeparator()

@test
def unlines08(): Bool =
    String.unlines(String.lineSeparator() :: "b" :: Nil) ==  String.lineSeparator() + String.lineSeparator() + "b" + String.lineSeparator()

/////////////////////////////////////////////////////////////////////////////
// words                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def words01(): Bool = String.words("") == Nil

@test
def words02(): Bool = String.words("a") == "a" :: Nil

@test
def words03(): Bool = String.words("a ") == "a" :: Nil

@test
def words04(): Bool = String.words(" a") == "a" :: Nil

@test
def words05(): Bool = String.words("a b") == "a" :: "b" :: Nil

@test
def words06(): Bool = String.words("a  b") == "a" :: "b" :: Nil

@test
def words07(): Bool = String.words("a" + Char.toString('\t') + "b") == "a" :: "b" :: Nil

/////////////////////////////////////////////////////////////////////////////
// unwords                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def unwords01(): Bool = String.unwords(Nil) == ""

@test
def unwords02(): Bool = String.unwords("" :: Nil) == ""

@test
def unwords03(): Bool = String.unwords("a" :: Nil) == "a"

@test
def unwords04(): Bool = String.unwords("a" :: "" :: Nil) == "a "

@test
def unwords05(): Bool = String.unwords("" :: "b" :: Nil) == " b"

@test
def unwords06(): Bool = String.unwords("a" :: "b" :: Nil) == "a b"

@test
def unwords07(): Bool = String.unwords("a" :: String.lineSeparator() :: Nil) == "a " + String.lineSeparator()

@test
def unwords08(): Bool = String.unwords(String.lineSeparator() :: "b" :: Nil) ==  String.lineSeparator() + " b"

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOf01(): Bool = String.indexOf({substr = "a"}, "") == None

@test
def indexOf02(): Bool = String.indexOf({substr = "a"}, "A") == None

@test
def indexOf03(): Bool = String.indexOf({substr = "a"}, "a") == Some(0)

@test
def indexOf04(): Bool = String.indexOf({substr = "a"}, "AB") == None

@test
def indexOf05(): Bool = String.indexOf({substr = "a"}, "ab") == Some(0)

@test
def indexOf06(): Bool = String.indexOf({substr = "b"}, "ab") == Some(1)

@test
def indexOf07(): Bool = String.indexOf({substr = "a"}, "aa") == Some(0)

@test
def indexOf08(): Bool = String.indexOf({substr = "ab"}, "ab") == Some(0)

@test
def indexOf09(): Bool = String.indexOf({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfLeft01(): Bool = String.indexOfLeft({substr = "a"}, "") == None

@test
def indexOfLeft02(): Bool = String.indexOfLeft({substr = "a"}, "A") == None

@test
def indexOfLeft03(): Bool = String.indexOfLeft({substr = "a"}, "a") == Some(0)

@test
def indexOfLeft04(): Bool = String.indexOfLeft({substr = "a"}, "AB") == None

@test
def indexOfLeft05(): Bool = String.indexOfLeft({substr = "a"}, "ab") == Some(0)

@test
def indexOfLeft06(): Bool = String.indexOfLeft({substr = "b"}, "ab") == Some(1)

@test
def indexOfLeft07(): Bool = String.indexOfLeft({substr = "a"}, "aa") == Some(0)

@test
def indexOfLeft08(): Bool = String.indexOfLeft({substr = "ab"}, "ab") == Some(0)

@test
def indexOfLeft09(): Bool = String.indexOfLeft({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfRight01(): Bool = String.indexOfRight({substr = "a"}, "") == None

@test
def indexOfRight02(): Bool = String.indexOfRight({substr = "a"}, "A") == None

@test
def indexOfRight03(): Bool = String.indexOfRight({substr = "a"}, "a") == Some(0)

@test
def indexOfRight04(): Bool = String.indexOfRight({substr = "a"}, "AB") == None

@test
def indexOfRight05(): Bool = String.indexOfRight({substr = "a"}, "ab") == Some(0)

@test
def indexOfRight06(): Bool = String.indexOfRight({substr = "b"}, "ab") == Some(1)

@test
def indexOfRight07(): Bool = String.indexOfRight({substr = "a"}, "aa") == Some(1)

@test
def indexOfRight08(): Bool = String.indexOfRight({substr = "ab"}, "ab") == Some(0)

@test
def indexOfRight09(): Bool = String.indexOfRight({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def startsWith01(): Bool = String.startsWith({prefix = ""}, "") == true

@test
def startsWith02(): Bool = String.startsWith({prefix = ""}, "a") == true

@test
def startsWith03(): Bool = String.startsWith({prefix = ""}, "abc") == true

@test
def startsWith04(): Bool = String.startsWith({prefix = "a"}, "") == false

@test
def startsWith05(): Bool = String.startsWith({prefix = "a"}, "a") == true

@test
def startsWith06(): Bool = String.startsWith({prefix = "a"}, "b") == false

@test
def startsWith07(): Bool = String.startsWith({prefix = "a"}, "aa") == true

@test
def startsWith08(): Bool = String.startsWith({prefix = "ab"}, "ab") == true

@test
def startsWith09(): Bool = String.startsWith({prefix = "bs"}, "abc") == false

@test
def startsWith10(): Bool = String.startsWith({prefix = ""}, "Hello World!") == true

@test
def startsWith11(): Bool = String.startsWith({prefix = "Hello"}, "Hello World!") == true

@test
def startsWith12(): Bool = String.startsWith({prefix = "HELLO"}, "Hello World!") == false

/////////////////////////////////////////////////////////////////////////////
// contains                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def contains01(): Bool = String.contains({substr = ""}, "") == true

@test
def contains02(): Bool = String.contains({substr = ""}, "a") == true

@test
def contains03(): Bool = String.contains({substr = ""}, "abc") == true

@test
def contains04(): Bool = String.contains({substr = "a"}, "") == false

@test
def contains05(): Bool = String.contains({substr = "a"}, "a") == true

@test
def contains06(): Bool = String.contains({substr = "a"}, "b") == false

@test
def contains07(): Bool = String.contains({substr = "a"}, "aa") == true

@test
def contains08(): Bool = String.contains({substr = "ab"}, "ab") == true

@test
def contains09(): Bool = String.contains({substr = "ab"}, "abc") == true

/////////////////////////////////////////////////////////////////////////////
// endsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def endsWith01(): Bool = String.endsWith({suffix = ""}, "") == true

@test
def endsWith02(): Bool = String.endsWith({suffix = ""}, "a") == true

@test
def endsWith03(): Bool = String.endsWith({suffix = ""}, "abc") == true

@test
def endsWith04(): Bool = String.endsWith({suffix = "a"}, "") == false

@test
def endsWith05(): Bool = String.endsWith({suffix = "a"}, "a") == true

@test
def endsWith06(): Bool = String.endsWith({suffix = "a"}, "b") == false

@test
def endsWith07(): Bool = String.endsWith({suffix = "a"}, "aa") == true

@test
def endsWith08(): Bool = String.endsWith({suffix = "ab"}, "ab") == true

@test
def endsWith09(): Bool = String.endsWith({suffix = "bc"}, "abc") == true

@test
def endsWith10(): Bool = String.endsWith({suffix = "ab"}, "abc") == false

@test
def endsWith11(): Bool = String.endsWith({suffix = ""}, "Hello World!") == true

@test
def endsWith12(): Bool = String.endsWith({suffix = "World!"}, "Hello World!") == true

@test
def endsWith13(): Bool = String.endsWith({suffix = "WORLD!"}, "Hello World!") == false

/////////////////////////////////////////////////////////////////////////////
// commonPrefix                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def commonPrefix01(): Bool =
    String.commonPrefix("", "") == ""

@test
def commonPrefix02(): Bool =
    String.commonPrefix("a", "") == ""

@test
def commonPrefix03(): Bool =
    String.commonPrefix("", "a") == ""

@test
def commonPrefix04(): Bool =
    String.commonPrefix("a", "a") == "a"

@test
def commonPrefix05(): Bool =
    String.commonPrefix("a", "b") == ""

@test
def commonPrefix06(): Bool =
    String.commonPrefix("ab", "bc") == ""

@test
def commonPrefix07(): Bool =
    String.commonPrefix("ab", "ac") == "a"

@test
def commonPrefix08(): Bool =
    String.commonPrefix("ab", "ab") == "ab"

@test
def commonPrefix09(): Bool =
    String.commonPrefix("abc", "ABC") == ""

@test
def commonPrefix10(): Bool =
    String.commonPrefix("abc", "abcd") == "abc"

@test
def commonPrefix11(): Bool =
    String.commonPrefix("abcd", "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// commonSuffix                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def commonSuffix01(): Bool =
    String.commonSuffix("", "") == ""

@test
def commonSuffix02(): Bool =
    String.commonSuffix("a", "") == ""

@test
def commonSuffix03(): Bool =
    String.commonSuffix("", "a") == ""

@test
def commonSuffix04(): Bool =
    String.commonSuffix("a", "a") == "a"

@test
def commonSuffix05(): Bool =
    String.commonSuffix("a", "b") == ""

@test
def commonSuffix06(): Bool =
    String.commonSuffix("ab", "bc") == ""

@test
def commonSuffix07(): Bool =
    String.commonSuffix("ab", "_b") == "b"

@test
def commonSuffix08(): Bool =
    String.commonSuffix("ab", "ab") == "ab"

@test
def commonSuffix09(): Bool =
    String.commonSuffix("abc", "ABC") == ""

@test
def commonSuffix10(): Bool =
    String.commonSuffix("bcd", "abcd") == "bcd"

@test
def commonSuffix11(): Bool =
    String.commonSuffix("abcd", "bcd") == "bcd"

/////////////////////////////////////////////////////////////////////////////
// abbreviateLeft                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def abbreviateLeft01(): Bool =
    String.abbreviateLeft(0, "") == ""

@test
def abbreviateLeft02(): Bool =
    String.abbreviateLeft(0, "a") == ""

@test
def abbreviateLeft03(): Bool =
    String.abbreviateLeft(0, "abcde") == ""

@test
def abbreviateLeft04(): Bool =
    String.abbreviateLeft(1, "") == ""

@test
def abbreviateLeft05(): Bool =
    String.abbreviateLeft(1, "a") == ""

@test
def abbreviateLeft06(): Bool =
    String.abbreviateLeft(1, "abcde") == ""

@test
def abbreviateLeft07(): Bool =
    String.abbreviateLeft(3, "") == ""

@test
def abbreviateLeft08(): Bool =
    String.abbreviateLeft(3, "a") == "a"

@test
def abbreviateLeft09(): Bool =
    String.abbreviateLeft(3, "abcde") == "..."

@test
def abbreviateLeft10(): Bool =
    String.abbreviateLeft(4, "") == ""

@test
def abbreviateLeft11(): Bool =
    String.abbreviateLeft(4, "a") == "a"

@test
def abbreviateLeft12(): Bool =
    String.abbreviateLeft(4, "abcde") == "...e"

@test
def abbreviateLeft13(): Bool =
    String.abbreviateLeft(5, "") == ""

@test
def abbreviateLeft14(): Bool =
    String.abbreviateLeft(5, "a") == "a"

@test
def abbreviateLeft15(): Bool =
    String.abbreviateLeft(5, "abcde") == "...de"


/////////////////////////////////////////////////////////////////////////////
// abbreviateRight                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def abbreviateRight01(): Bool =
    String.abbreviateRight(0, "") == ""

@test
def abbreviateRight02(): Bool =
    String.abbreviateRight(0, "a") == ""

@test
def abbreviateRight03(): Bool =
    String.abbreviateRight(0, "abcde") == ""

@test
def abbreviateRight04(): Bool =
    String.abbreviateRight(1, "") == ""

@test
def abbreviateRight05(): Bool =
    String.abbreviateRight(1, "a") == ""

@test
def abbreviateRight06(): Bool =
    String.abbreviateRight(1, "abcde") == ""

@test
def abbreviateRight07(): Bool =
    String.abbreviateRight(3, "") == ""

@test
def abbreviateRight08(): Bool =
    String.abbreviateRight(3, "a") == "a"

@test
def abbreviateRight09(): Bool =
    String.abbreviateRight(3, "abcde") == "..."

@test
def abbreviateRight10(): Bool =
    String.abbreviateRight(4, "") == ""

@test
def abbreviateRight11(): Bool =
    String.abbreviateRight(4, "a") == "a"

@test
def abbreviateRight12(): Bool =
    String.abbreviateRight(4, "abcde") == "a..."

@test
def abbreviateRight13(): Bool =
    String.abbreviateRight(5, "") == ""

@test
def abbreviateRight14(): Bool =
    String.abbreviateRight(5, "a") == "a"

@test
def abbreviateRight15(): Bool =
    String.abbreviateRight(5, "abcde") == "ab..."

/////////////////////////////////////////////////////////////////////////////
// levenshteinDistance                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def levenshteinDistance01(): Bool =
    String.levenshteinDistance("", "") == 0

@test
def levenshteinDistance02(): Bool =
    String.levenshteinDistance("A", "") == 1

@test
def levenshteinDistance03(): Bool =
    String.levenshteinDistance("", "A") == 1

@test
def levenshteinDistance04(): Bool =
    String.levenshteinDistance("A", "A") == 0

@test
def levenshteinDistance05(): Bool =
    String.levenshteinDistance("kitten", "sitting") == 3

@test
def levenshteinDistance06(): Bool =
    String.levenshteinDistance("Saturday", "Sunday") == 3

@test
def levenshteinDistance07(): Bool =
    String.levenshteinDistance("Gumbo", "Gambol") == 2

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def zip01(): Bool =
    String.zip("", "") == (Nil: List[(Char,Char)])

@test
def zip02(): Bool =
    String.zip("a", "") == (Nil: List[(Char, Char)])

@test
def zip03(): Bool =
    String.zip("", "1") == (Nil: List[(Char, Char)])

@test
def zip04(): Bool =
    String.zip("a", "1") == ('a', '1') :: Nil

@test
def zip05(): Bool =
    String.zip("ab", "12") == ('a', '1') :: ('b', '2') :: Nil

@test
def zip06(): Bool =
    String.zip("abc", "123") == ('a', '1') :: ('b', '2') :: ('c', '3') :: Nil

@test
def zip07(): Bool =
    String.zip("abcd", "1234") == ('a', '1') :: ('b', '2') :: ('c', '3') :: ('d', '4') :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def zipWith01(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "");
    s == ""

@test
def zipWith02(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "");
    s == ""

@test
def zipWith03(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "1");
    s == ""

@test
def zipWith04(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_", "1");
    s == "1"

@test
def zipWith05(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "1");
    s == "a"

@test
def zipWith06(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "12345678");
    s == "1bc45fgh"

@test
def zipWith07(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fghijkl", "12345678");
    s == "1bc45fgh"

@test
def zipWith08(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "1234567890");
    s == "1bc45fgh"

/////////////////////////////////////////////////////////////////////////////
// splitOn                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def splitOn01(): Bool = String.splitOn({substr = ""}, "") == "" :: Nil

@test
def splitOn02(): Bool = String.splitOn({substr = ""}, "a::b") == "a::b" :: Nil

@test
def splitOn03(): Bool = String.splitOn({substr = "::"}, "") == "" :: Nil

@test
def splitOn04(): Bool = String.splitOn({substr = "::"}, "a") == "a" :: Nil

@test
def splitOn05(): Bool = String.splitOn({substr = "::"}, "a:b") == "a:b" :: Nil

@test
def splitOn06(): Bool = String.splitOn({substr = "::"}, "a::b") == "a" :: "b" :: Nil

@test
def splitOn07(): Bool = String.splitOn({substr = "::"}, "::a::b") == "" :: "a" :: "b" :: Nil

@test
def splitOn08(): Bool = String.splitOn({substr = "::"}, "a::b::") == "a" :: "b" :: "" :: Nil

@test
def splitOn09(): Bool = String.splitOn({substr = "::"}, "::a::b::") == "" :: "a" :: "b" :: "" :: Nil

@test
def splitOn10(): Bool = String.splitOn({substr = "::"}, "a:b::c") == "a:b" :: "c" :: Nil

@test
def splitOn11(): Bool = String.splitOn({substr = "::"}, "a::b:c") == "a" :: "b:c" :: Nil

/////////////////////////////////////////////////////////////////////////////
// indexOfLeftWithOffset                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfLeftWithOffset01(): Bool = String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, "") == None

@test
def indexOfLeftWithOffset02(): Bool = String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, "a") == None

@test
def indexOfLeftWithOffset03(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "a") == Some(0)

@test
def indexOfLeftWithOffset04(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "a") == None

@test
def indexOfLeftWithOffset05(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "ab") == Some(0)

@test
def indexOfLeftWithOffset06(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "aa") == Some(1)

@test
def indexOfLeftWithOffset07(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "ab") == Some(0)

@test
def indexOfLeftWithOffset08(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "abab") == Some(0)

@test
def indexOfLeftWithOffset09(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 1}, "abab") == Some(2)

@test
def indexOfLeftWithOffset10(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 2}, "abab") == Some(2)

@test
def indexOfLeftWithOffset11(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 3}, "abab") == None

/////////////////////////////////////////////////////////////////////////////
// indexOfRightWithOffset                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfRightWithOffset01(): Bool = String.indexOfRightWithOffset({substr = ""}, {offset = 0}, "") == None

@test
def indexOfRightWithOffset02(): Bool = String.indexOfRightWithOffset({substr = ""}, {offset = 0}, "a") == None

@test
def indexOfRightWithOffset03(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 0}, "a") == Some(0)

@test
def indexOfRightWithOffset04(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 1}, "a") == Some(0)

@test
def indexOfRightWithOffset05(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "ab") == Some(0)

@test
def indexOfRightWithOffset06(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "aa") == Some(1)

@test
def indexOfRightWithOffset07(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "ab") == Some(0)

@test
def indexOfRightWithOffset08(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 3}, "abab") == Some(2)

@test
def indexOfRightWithOffset09(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "abab") == Some(2)

@test
def indexOfRightWithOffset10(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 1}, "abab") == Some(0)

@test
def indexOfRightWithOffset11(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 0}, "abab") == Some(0)

@test
def indexOfRightWithOffset12(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = -1}, "abab") == None

/////////////////////////////////////////////////////////////////////////////
// toChunks                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toChunks01(): Bool = String.toChunks(0, "") == Nil

@test
def toChunks02(): Bool = String.toChunks(0, "aaa") == Nil

@test
def toChunks03(): Bool = String.toChunks(3, "") == Nil

@test
def toChunks04(): Bool = String.toChunks(3, "a") == "a" :: Nil

@test
def toChunks05(): Bool = String.toChunks(3, "aa") == "aa" :: Nil

@test
def toChunks06(): Bool = String.toChunks(3, "aaa") == "aaa" :: Nil

@test
def toChunks07(): Bool = String.toChunks(3, "aaab") == "aaa" :: "b" :: Nil

@test
def toChunks08(): Bool = String.toChunks(3, "aaabb") == "aaa" :: "bb" :: Nil

@test
def toChunks09(): Bool = String.toChunks(3, "aaabbb") == "aaa" :: "bbb" :: Nil

@test
def toChunks10(): Bool = String.toChunks(3, "aaabbbc") == "aaa" :: "bbb" :: "c" :: Nil

@test
def toChunks11(): Bool = String.toChunks(1, "aaabbbc") == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: "c" :: Nil

@test
def toChunks12(): Bool = String.toChunks(2, "aaabbbc") == "aa" :: "ab" :: "bb" :: "c" :: Nil

@test
def toChunks13(): Bool = String.toChunks(4, "aaabbbc") == "aaab" :: "bbc" :: Nil

@test
def toChunks14(): Bool = String.toChunks(10, "aaabbbc") == "aaabbbc" :: Nil

/////////////////////////////////////////////////////////////////////////////
// breakOnLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def breakOnLeft01(): Bool = String.breakOnLeft({substr = ""}, "") == ("", "")

@test
def breakOnLeft02(): Bool = String.breakOnLeft({substr = ""}, "aaa") == ("aaa", "")

@test
def breakOnLeft03(): Bool = String.breakOnLeft({substr = "::"}, "") == ("", "")

@test
def breakOnLeft04(): Bool = String.breakOnLeft({substr = "::"}, "aaa") == ("aaa", "")

@test
def breakOnLeft05(): Bool = String.breakOnLeft({substr = "::"}, "aaa:") == ("aaa:", "")

@test
def breakOnLeft06(): Bool = String.breakOnLeft({substr = "::"}, "aaa::") == ("aaa", "::")

@test
def breakOnLeft07(): Bool = String.breakOnLeft({substr = "::"}, "aaa::bbb") == ("aaa", "::bbb")

@test
def breakOnLeft08(): Bool = String.breakOnLeft({substr = "::"}, "aaa::bbb::ccc") == ("aaa", "::bbb::ccc")

/////////////////////////////////////////////////////////////////////////////
// breakOnRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def breakOnRight01(): Bool = String.breakOnRight({substr = ""}, "") == ("", "")

@test
def breakOnRight02(): Bool = String.breakOnRight({substr = ""}, "aaa") == ("aaa", "")

@test
def breakOnRight03(): Bool = String.breakOnRight({substr = "::"}, "") == ("", "")

@test
def breakOnRight04(): Bool = String.breakOnRight({substr = "::"}, "aaa") == ("aaa", "")

@test
def breakOnRight05(): Bool = String.breakOnRight({substr = "::"}, "aaa:") == ("aaa:", "")

@test
def breakOnRight06(): Bool = String.breakOnRight({substr = "::"}, "aaa::") == ("aaa::", "")

@test
def breakOnRight07(): Bool = String.breakOnRight({substr = "::"}, "aaa::bbb") == ("aaa::", "bbb")

@test
def breakOnRight08(): Bool = String.breakOnRight({substr = "::"}, "aaa::bbb::ccc") == ("aaa::bbb::", "ccc")

/////////////////////////////////////////////////////////////////////////////
// countSubstring                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def countSubstring01(): Bool = String.countSubstring({substr = ""}, "") == 0

@test
def countSubstring02(): Bool = String.countSubstring({substr = ""}, "abc abc") == 0

@test
def countSubstring03(): Bool = String.countSubstring({substr = "abc"}, "") == 0

@test
def countSubstring04(): Bool = String.countSubstring({substr = "abc"}, "abc") == 1

@test
def countSubstring05(): Bool = String.countSubstring({substr = "abc"}, "aabc") == 1

@test
def countSubstring06(): Bool = String.countSubstring({substr = "abc"}, "abcc") == 1

@test
def countSubstring07(): Bool = String.countSubstring({substr = "abc"}, "abc def ") == 1

@test
def countSubstring08(): Bool = String.countSubstring({substr = "abc"}, " abc def") == 1

@test
def countSubstring09(): Bool = String.countSubstring({substr = "abc"}, " abc abc") == 2

@test
def countSubstring10(): Bool = String.countSubstring({substr = "abc"}, " abc ABC abc") == 2

@test
def countSubstring11(): Bool = String.countSubstring({substr = "abc"}, "ABC abc def abc") == 2

@test
def countSubstring12(): Bool = String.countSubstring({substr = "abc"}, "aBC::abc::AbC::abc::Abc") == 2

/////////////////////////////////////////////////////////////////////////////
// stripMargin                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def stripMargin01(): Bool = String.stripMargin("") == ""

@test
def stripMargin02(): Bool = String.stripMargin("abc") == "abc"

@test
def stripMargin03(): Bool = String.stripMargin("${'\t'}") == "${'\t'}"

@test
def stripMargin04(): Bool = String.stripMargin("    |abc") == "abc"

@test
def stripMargin05(): Bool = String.stripMargin("${'\t'} |abc") == "abc"

@test
def stripMargin06(): Bool = String.stripMargin(" | abc") == " abc"

@test
def stripMargin07(): Bool = String.stripMargin(" ||abc") == "|abc"

@test
def stripMargin08(): Bool = String.stripMargin(String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

@test
def stripMargin09(): Bool = String.stripMargin(String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// stripMarginWith                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def stripMarginWith01(): Bool = String.stripMarginWith({margin = "|"}, "") == ""

@test
def stripMarginWith02(): Bool = String.stripMarginWith({margin = "|"}, "abc") == "abc"

@test
def stripMarginWith03(): Bool = String.stripMarginWith({margin = "|"}, "${'\t'}") == "${'\t'}"

@test
def stripMarginWith04(): Bool = String.stripMarginWith({margin = "|"}, "    |abc") == "abc"

@test
def stripMarginWith05(): Bool = String.stripMarginWith({margin = "|"}, "${'\t'} |abc") == "abc"

@test
def stripMarginWith06(): Bool = String.stripMarginWith({margin = "|"}, " | abc") == " abc"

@test
def stripMarginWith07(): Bool = String.stripMarginWith({margin = "|"}, " ||abc") == "|abc"

@test
def stripMarginWith08(): Bool = String.stripMarginWith({margin = "| "}, " | abc") == "abc"

@test
def stripMarginWith09(): Bool = String.stripMarginWith({margin = "|"}, String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

@test
def stripMarginWith10(): Bool = String.stripMarginWith({margin = "|"}, String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil)


/////////////////////////////////////////////////////////////////////////////
// stripPrefix                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def stripPrefix01(): Bool = String.stripPrefix({substr = ""}, "") == Some("")

@test
def stripPrefix02(): Bool = String.stripPrefix({substr = ""}, "abc") == Some("abc")

@test
def stripPrefix03(): Bool = String.stripPrefix({substr = "abc"}, "") == None

@test
def stripPrefix04(): Bool = String.stripPrefix({substr = "abc"}, "abc") == Some("")

@test
def stripPrefix05(): Bool = String.stripPrefix({substr = "abc"}, "abcdef") == Some("def")

@test
def stripPrefix06(): Bool = String.stripPrefix({substr = "def"}, "abcdef") == None

@test
def stripPrefix07(): Bool = String.stripPrefix({substr = "abc"}, "ABCDEF") == None

/////////////////////////////////////////////////////////////////////////////
// stripSuffix                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def stripSuffix01(): Bool = String.stripSuffix({substr = ""}, "") == Some("")

@test
def stripSuffix02(): Bool = String.stripSuffix({substr = ""}, "abc") == Some("abc")

@test
def stripSuffix03(): Bool = String.stripSuffix({substr = "abc"}, "") == None

@test
def stripSuffix04(): Bool = String.stripSuffix({substr = "abc"}, "abc") == Some("")

@test
def stripSuffix05(): Bool = String.stripSuffix({substr = "abc"}, "abcdef") == None

@test
def stripSuffix06(): Bool = String.stripSuffix({substr = "def"}, "abcdef") == Some("abc")

@test
def stripSuffix07(): Bool = String.stripPrefix({substr = "abc"}, "ABCDEF") == None

/////////////////////////////////////////////////////////////////////////////
// wrap                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def wrap01(): Bool = String.wrap(0, "a") == ""

@test
def wrap02(): Bool = String.wrap(1, "a") == "a"

@test
def wrap03(): Bool = String.wrap(2, "a") == "a"

@test
def wrap04(): Bool = String.wrap(2, "ab") == "ab"

@test
def wrap05(): Bool = String.wrap(1, "ab") == "a" + String.lineSeparator() + "b"

@test
def wrap06(): Bool = String.wrap(2, "a b") == "a" + String.lineSeparator() + "b"

@test
def wrap07(): Bool = String.wrap(2, "a b c") == "a" + String.lineSeparator() + "b" + String.lineSeparator() + "c"

@test
def wrap08(): Bool = String.wrap(2, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@test
def wrap09(): Bool = String.wrap(3, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@test
def wrap10(): Bool = String.wrap(5, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@test
def wrap11(): Bool =
    String.wrap(5, "Lorem ipsum dolor sitamet")
        == "Lorem" + String.lineSeparator()
        + "ipsum" + String.lineSeparator()
        + "dolor" + String.lineSeparator()
        + "sitam" + String.lineSeparator()
        + "et"

/////////////////////////////////////////////////////////////////////////////
// unwrap                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unwrap01(): Bool = String.unwrap("") == ""

@test
def unwrap02(): Bool = String.unwrap("a\nb") == "a b"

@test
def unwrap03(): Bool = String.unwrap("a\nb\nc") == "a b c"

@test
def unwrap04(): Bool = String.unwrap("ab\nc\nd") == "ab c d"

@test
def unwrap05(): Bool = String.unwrap("a\rb") == "a b"

@test
def unwrap06(): Bool = String.unwrap("a\rb\rc") == "a b c"

@test
def unwrap07(): Bool = String.unwrap("ab\rc\rd") == "ab c d"

@test
def unwrap08(): Bool = String.unwrap("a\r\nb") == "a b"

@test
def unwrap09(): Bool = String.unwrap("a\r\nb\r\nc") == "a b c"

@test
def unwrap10(): Bool = String.unwrap("ab\r\nc\r\nd") == "ab c d"

/////////////////////////////////////////////////////////////////////////////
// escapeInterpolation                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def escapeInterpolation01(): Bool = "\${" == ("$" + "{")

@test
def escapeInterpolation02(): Bool = "\${1 + {2}" == ("$" + "{1 + {2}")

@test
def escapeInterpolation03(): Bool = "\${}" == ("$" + "{}")

@test
def escapeInterpolation04(): String = "$"


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        String.iterator(rc, "") |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region rc {
        String.iterator(rc, "abc") |> Iterator.toList == 'a' :: 'b' :: 'c' :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region rc {
        String.enumerator(rc, "") |> Iterator.toList == Nil
    }

    @test
    def enumerator02(): Bool = region rc {
        String.enumerator(rc, "abc") |> Iterator.toList == (0, 'a') :: (1, 'b') :: (2, 'c') :: Nil
    }

}

