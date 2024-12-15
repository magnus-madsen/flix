/*
 * Copyright 2017 Magnus Madsen, Stephen Tetley
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

mod String {

    import java.lang.System
    import java.util.regex.Pattern
    import java.util.regex.PatternSyntaxException
    import java.lang.IndexOutOfBoundsException

    use Regex.Flag

    ///
    /// Returns the character at position `i` in the string `s`.
    ///
    pub def charAt(i: Int32, s: String): Char = unsafely IO run s.charAt(i)

    ///
    /// Optionally return the character at position `i` in the string `s`.
    ///
    pub def nth(i: Int32, s: String): Option[Char] =
        if (0 <= i and i < length(s))
            Some(charAt(i, s))
        else
            None

    ///
    /// Returns the code point at position `i` in the string `s`.
    ///
    pub def codePointAt(i: Int32, s: String): Int32 = unsafely IO run s.codePointAt(i)

    ///
    /// Optionally returns the code point at position `i` in the string `s`.
    ///
    pub def nthCodePoint(i: Int32, s: String): Option[Int32] =
        if (0 <= i and i < length(s))
            Some(codePointAt(i, s))
        else
            None

    ///
    /// Returns the string `s1` followed by the string `s2`.
    ///
    pub def concat(s1: String, s2: String): String =
        unsafely IO run s1.concat(s2)

    ///
    /// Returns `true` if the string `s` is the empty string.
    ///
    pub def isEmpty(s: String): Bool =
        unsafely IO run s.isEmpty()

    ///
    /// Returns `true` if the string `s` is non-empty.
    ///
    pub def nonEmpty(s: String): Bool = not isEmpty(s)

    ///
    /// Returns the number of characters the string `s`.
    ///
    pub def length(s: String): Int32 =
        unsafely IO run s.length()

    ///
    /// Returns the number of characters the string `s`.
    ///
    pub def size(s: String): Int32 = length(s)

    ///
    /// Returns `true` if the string `s` starts with the string `prefix`.
    ///
    pub def startsWith(prefix: {prefix = String}, s: String): Bool =
        unsafely IO run s.startsWith(prefix#prefix)

    ///
    /// Returns `true` if the string `s` ends with the string `suffix`.
    ///
    pub def endsWith(suffix: {suffix = String}, s: String): Bool =
        unsafely IO run s.endsWith(suffix#suffix)

    ///
    /// Splits the string `s` around matches of the regular expression `regex`.
    ///
    pub def split(regex: {regex = String}, s: String): List[String] =
        unsafely IO run Array.toList(unsafely IO run s.split(regex#regex))

    ///
    /// Returns the lower case version of the string `s`.
    ///
    pub def toLowerCase(s: String): String =
        unsafely IO run s.toLowerCase()

    ///
    /// Returns the upper case version of the string `s`.
    ///
    pub def toUpperCase(s: String): String =
        unsafely IO run s.toUpperCase()

    ///
    /// Returns the given string `s` as a list of characters.
    ///
    pub def toList(s: String): List[Char] = List.map(i -> charAt(i, s), List.range(0, length(s)))

    ///
    /// Returns a copy of the string `s` without trailing and leading whitespaces.
    ///
    /// Returns a new empty string if there is no characters in `s`.
    ///
    pub def trim(s: String): String =
        unsafely IO run s.trim()

    ///
    /// Get the system line separator.
    ///
    pub def lineSeparator(): String = unsafely {IO, Sys} run System.lineSeparator()

    ///
    /// Returns the given string `s` as an array of characters.
    ///
    pub def toArray(rc: Region[r], s: String): Array[Char, r] \ r =
        Array.init(rc, i -> charAt(i, s), length(s))

    ///
    /// Returns the given string `s` as a vector of characters.
    ///
    pub def toVector(s: String): Vector[Char] = region rc {
        let arr = toArray(rc, s);
        Array.toVector(arr)
    }

    ///
    /// Compile the regular expression `regex` into a Regex object (internally
    /// a Regex is `java.util.regex.Pattern`).
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    /// The syntax of regular expressions is the same as Java, see the JDK Javadocs
    /// of the class `util.regex.Pattern` for reference.
    ///
    /// Warning: It is strongly recommended to use a Regex literal instead of `toRegex`.
    /// Regex literals are checked at compile time.
    ///
    pub def toRegex(regex: String): Result[String, Regex] =
        Result.tryCatch(_ -> {
            unsafely IO run Pattern.compile(regex)
        })


    ///
    /// Compile the regular expression `regex` into a Regex with the supplied flags.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    /// Warning: It is strongly recommended to use a Regex literal instead of `toRegexWithFlags`.
    /// Regex literals are checked at compile time. See the JDK documentation of
    /// `java.util.regex.Pattern` for guidance on enabling flags with embedded flag expressions.
    ///
    pub def toRegexWithFlags(flags: Set[Flag], regex: String): Result[String, Regex] =
        Result.tryCatch(_ -> {
            unsafely IO run Pattern.compile(regex, Regex.sumFlags(flags))
        })


    ///
    /// Build a string of length `len` by applying `f` to the successive indices.
    ///
    pub def init(f: Int32 -> Char \ ef, len: Int32): String \ ef =
        if (len <= 0) "" else initHelper(f, len)

    ///
    /// Helper function for `init`.
    ///
    /// Assumes that `len > 0`.
    ///
    def initHelper(f: Int32 -> Char \ ef, len: Int32): String \ ef = region rc {
        let sb = StringBuilder.empty(rc);
        def loop(i) = {
            if (i >= len)
                StringBuilder.toString(sb)
            else {
                let c = f(i);
                StringBuilder.append(c, sb);
                loop(i + 1)
            }
        };
        loop(0)
    }

    ///
    /// Concatenate a list of strings into a single string.
    ///
    pub def flatten(xs: List[String]): String = region rc {
        let sb = StringBuilder.empty(rc);
        List.forEach(x -> StringBuilder.appendString(x, sb), xs);
        StringBuilder.toString(sb)
    }

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    pub def intercalate(sep: String, xs: List[String]): String = region rc {
        let sb = StringBuilder.empty(rc);
        let arr = List.toArray(rc, xs);
        StringBuilder.intercalate(sep, arr, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Concatenate a list of strings into a single string, inserting the separator `sep` between
    /// each pair of strings.
    ///
    pub def intercalateChar(sep: Char, xs: List[String]): String = match xs {
        case Nil     => ""
        case x :: rs => region rc {
            let sb = StringBuilder.empty(rc);
            StringBuilder.appendString(x, sb);
            let appendStep = s -> { StringBuilder.append(sep, sb); StringBuilder.appendString(s, sb) };
            List.forEach(appendStep, rs);
            StringBuilder.toString(sb)
        }
    }

    ///
    /// Returns the result of applying `f` to every character in `s`.
    ///
    pub def map(f: Char -> Char \ ef, s: String): String \ ef =
        let len = length(s);
        init(i -> f(charAt(i, s)), len)

    ///
    /// Returns the result of applying `f` to every character in `s` along with that character's index.
    ///
    pub def mapWithIndex(f: (Int32, Char) -> Char \ ef, s: String): String \ ef =
        let len = length(s);
        init(i -> f(i, charAt(i, s)), len)

    ///
    /// Returns the reverse of `s`.
    ///
    pub def reverse(s: String) : String =
        let start = length(s) - 1;
        init(i -> charAt(start - i, s), start + 1)

    ///
    /// Rotate the contents of string `s` by `n` steps to the left.
    ///
    pub def rotateLeft(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else if (n < 0)
            rotateRightHelper(Int32.abs(n), s)
        else
            rotateLeftHelper(n, s)

    ///
    /// Helper function for `rotateLeft`, `n` must be positive.
    ///
    def rotateLeftHelper(n: Int32, s: String): String =
        let len = length(s);
        let f = i -> { let i1 = n + i; charAt(i1 `Int32.remainder` len, s) };
        init(f, len)

    ///
    /// Rotate the contents of string `s` by `n` steps to the right.
    ///
    pub def rotateRight(n: Int32, s: String): String =
        if (length(s) < 1)
            ""
        else if (n < 0)
            rotateLeftHelper(Int32.abs(n), s)
        else
            rotateRightHelper(n, s)

    ///
    /// Helper function for `rotateRight`, `n` must be positive.
    ///
    def rotateRightHelper(n: Int32, s: String): String =
        let len = length(s);
        let n1 = n `Int32.remainder` len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; charAt(i1 `Int32.remainder` len, s)};
        init(f, len)

    ///
    /// Returns `true` if the entire string `s` is matched by the regular expression `regex`.
    ///
    /// Note - use `isSubmatch` to search for a substring.
    ///
    pub def isMatch(regex: {regex = String}, s: String): Bool =
        try {
            unsafely IO run s.matches(regex#regex)
        } catch {
            case _: PatternSyntaxException => false
        }

    ///
    /// Returns `true` if the string `s` is matched by the regular expression `regex` at any point.
    ///
    pub def isSubmatch(regex: {regex = String}, s: String): Bool =
        isSubmatchHelper(regex#regex, s)

    ///
    /// Helper function for `isSubmatch`.
    ///
    def isSubmatchHelper(pattern: String, s: String): Bool =
        try {
            let p1 = unsafely IO run Pattern.compile(pattern);
            let m1 = unsafely IO run p1.matcher(s);
            unsafely IO run m1.find(0)
        } catch {
            case _: PatternSyntaxException => false
        }

    ///
    /// Returns the substring of `s` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// If `b` or `e` are out-of-bounds, return the empty string.
    ///
    pub def slice(start: {start = Int32}, end: {end = Int32}, s: String): String = try {
        unsafely IO run s.substring(start#start, end#end)
    } catch {
        case _: IndexOutOfBoundsException => ""
    }

    ///
    /// Get the substring of `s` to the left of index `end` (exclusive).
    ///
    /// `sliceLeft == slice(0 , e, s)`
    ///
    pub def sliceLeft(end: {end = Int32}, s: String): String = match length(s) {
        case len if end#end > len => ""
        case _   if end#end < 0   => ""
        case _                    => slice(start = 0, end, s)
    }

    ///
    /// Get the substring of `s` to the right starting at index `start` (inclusive).
    ///
    /// `sliceRight == slice(start , length(s), s)`
    ///
    pub def sliceRight(start: {start = Int32}, s: String): String = match length(s) {
        case len if start#start >= len => ""
        case _   if start#start < 0    => ""
        case len                       => slice(start, end = len, s)
    }

    ///
    /// Optionally returns the position of the first character in `x` satisfying `f`.
    ///
    /// Returns `None` if no character in `s` satisfies `f`.
    ///
    pub def findIndexOfLeft(f: Char -> Bool \ ef, s: String): Option[Int32] \ ef =
        let len = length(s);
        def loop(i) = {
            if (i >= len)
                -1
            else if (f(charAt(i, s)))
                i
            else
                loop(i + 1)
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally return the position of the first character in `s` satisfying `f`, reading right to left.
    ///
    /// If nothing satisfies `f` return None.
    ///
    pub def findIndexOfRight(f: Char -> Bool, s: String): Option[Int32] =
        def loop(i) = {
            if (i < 0)
                -1
            else if (f(charAt(i, s)))
                i
            else
                loop(i - 1)
        };
        let end = length(s) - 1;
        let i = loop(end);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `s` satisfying `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def findIndices(f: Char -> Bool, s: String): List[Int32] =
        let len = length(s);
        def loop(i, k) = {
            if (i >= len)
                k(Nil)
            else if (f(charAt(i, s)))
                loop(i + 1, ks -> k(i :: ks))
            else
                loop(i + 1, k)
        };
        loop(0, identity)

    ///
    /// Returns the positions of the all the occurrences of `substr` in `s`.
    ///
    /// Returns `Nil` if `substr` is the empty string.
    ///
    pub def indices(substr: {substr = String}, s: String): List[Int32] =
        let sublen = length(substr#substr);
        let step = ix ->
            match indexOfLeftWithOffset({substr = substr#substr}, {offset = ix}, s) {
                case None => None
                case Some(ix2) => Some((ix2, ix2 + sublen))
            };
        List.unfold(step, 0)

    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(n: Int32, s: String): String = takeLeft(n, s)

    ///
    /// Take the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string `s`, return all the characters
    /// of `s`.
    ///
    pub def takeLeft(n: Int32, s: String): String =
        if (n >= length(s)) s else slice(start = 0, end = n, s)

    ///
    /// Take the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return all the characters
    /// of `s`.
    ///
    pub def takeRight(n: Int32, s: String): String =
        let len = length(s);
        if (n >= len) s else slice(start = len - n, end = len, s)

    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(n: Int32, s: String): String = dropLeft(n, s)

    ///
    /// Drop the first `n` characters of string `s` from the left.
    ///
    /// If `n` extends past the end of string s, return the empty string.
    ///
    pub def dropLeft(n: Int32, s: String): String =
        if (n <= 0)
            s
        else
            slice(start = n, end = length(s), s)

    ///
    /// Drop the last `n` characters of string `s` from the right.
    ///
    /// If `n` is greater than the length of string `s`, return the empty string.
    ///
    pub def dropRight(n: Int32, s: String): String =
        if (n > 0)
            let len = length(s);
            slice(start = 0, end = len - n, s)
        else
            s

    ///
    /// Alias for `takeWileLeft`.
    ///
    pub def takeWhile(f: Char -> Bool, s: String): String = takeWhileLeft(f, s)

    ///
    /// Returns the initial prefix of string `s` where all the chars satisfy
    /// the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x -> not (f(x)), s) {
            case None    => s
            case Some(i) => take(i, s)
        }

    ///
    /// Returns the suffix of string `s` where all the characters satisfy
    /// the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> not (f(x)), s) {
            case None    => s
            case Some(i) => slice(start = i + 1, end = length(s), s)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    pub def dropWhile(f: Char -> Bool, s: String): String = dropWhileLeft(f, s)

    ///
    /// Returns the tail of string `s` after dropping all the initial chars
    /// that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileLeft(f: Char -> Bool, s: String): String =
        match findIndexOfLeft(x -> not (f(x)), s) {
            case None    => ""
            case Some(i) => sliceRight(start = i, s)
        }

    ///
    /// Returns the front of string `s` after dropping all the characters
    /// from the right end that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileRight(f: Char -> Bool, s: String): String =
        match findIndexOfRight(x -> not (f(x)), s) {
            case None    => ""
            case Some(i) => sliceLeft(end = i + 1, s)     // must include i
        }

    ///
    /// Returns the middle of string `s` after dropping all the characters
    /// from both ends that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileAround(f: Char -> Bool, s: String): String =
        match (findIndexOfLeft(x -> not (f(x)), s), findIndexOfRight(x -> not (f(x)), s)) {
            case (None, None)       => ""
            case (Some(l), None)    => sliceRight(start = l, s)
            case (None, Some(r))    => sliceLeft(end = r, s)
            case (Some(l), Some(r)) => slice(start = l, end = r + 1, s)
        }

    ///
    /// Split the string `s` at the position `n` returning the left and
    /// right (inclusive `n`) parts.
    ///
    /// If `n` exceeds the length of string `s`, return the whole string
    /// paired with the empty string. Symmetrically for `n` less than 0.
    ///
    pub def splitAt(n: Int32, s: String): (String, String) =
        match length(s) {
            case _   if n <= 0  => ("", s)
            case len if n > len => (s, "")
            case _              => (sliceLeft(end = n, s), sliceRight(start = n, s))
        }


    ///
    /// Applies `f` to a start value `x` and all elements in `s` going from left to right.
    ///
    pub def foldLeft(f: (b, Char) -> b \ ef, x: b, s: String): b \ ef =
        let len = length(s);
        def loop(acc, i) = {
            if (i >= len)
                acc
            else
                let acc1 = f(acc, charAt(i, s));
                loop(acc1, i + 1)
        };
        loop(x, 0)

    ///
    /// Applies `f` to a start value `x and all elements in `s` going from right to left.
    ///
    pub def foldRight(f: (Char, b) -> b \ ef, x: b, s: String): b \ ef =
        def loop(acc, i) = {
            if (i < 0)
                acc
            else
                let acc1 = f(charAt(i, s), acc);
                loop(acc1, i - 1)
        };
        let end = length(s) - 1;
        loop(x, end)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from left to right.
    ///
    pub def foldLeft2(f: (b, Char, Char) -> b \ ef, x: b, s: String, t: String): b \ ef =
        let len = Int32.min(length(s), length(t));
        def loop(acc, i) = {
            if (i >= len)
                acc
            else
                let acc1 = f(acc, charAt(i, s), charAt(i, t));
                loop(acc1, i + 1)
        };
        loop(x, 0)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `s` and `t`
    /// starting with the initial value `x` and going from right to left.
    ///
    pub def foldRight2(f: (Char, Char, b) -> b \ ef, x: b, s: String, t: String): b \ ef =
        let sn = length(s) - 1;
        let tn = length(t) - 1;
        def loop(acc, i, j) = {
            if (i < 0 or j < 0)
                acc
            else
                let acc1 = f(charAt(i, s), charAt(j, t), acc);
                loop(acc1, i - 1, j - 1)
        };
        loop(x, sn, tn)

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    pub def unfold(f: b -> Option[(Char, b)] \ ef, x: b): String \ ef = region rc {
        let sb = StringBuilder.empty(rc);
        def loop(a) = {
            let a1 = f(a);
            match a1 {
                case None         => StringBuilder.toString(sb)
                case Some((c, st1)) => {
                    StringBuilder.append(c, sb);
                    loop(st1)
                }
            }
        };
        loop(x)
    }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(c)` to signal a new char `c`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldWithIter(f: Unit -> Option[Char] \ ef): String \ ef = region rc {
        let sb = StringBuilder.empty(rc);
        def loop() = {
            match f() {
                case None    => StringBuilder.toString(sb)
                case Some(c) => {
                    StringBuilder.append(c, sb);
                    loop()
                }
            }
        };
        loop()
    }

    ///
    /// Build a string from the seed value `x` applying the function `f` until `f` returns `None`.
    ///
    /// This is a version of `unfold` where `f` generates substrings rather than chars.
    ///
    pub def unfoldString(f: b -> Option[(String, b)] \ ef, x: b) : String \ ef = region rc {
        let sb = StringBuilder.empty(rc);
        def loop(a) = {
            let a1 = f(a);
            match a1 {
                case None         => StringBuilder.toString(sb)
                case Some(((s, st1))) => {
                    StringBuilder.appendString(s, sb);
                    loop(st1)
                }
            }
        };
        loop(x)
    }

    ///
    /// Build a string by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(s)` to signal a new substring `s`.
    ///
    /// `next` should return `None` to signal the end of building the string.
    ///
    pub def unfoldStringWithIter(f: Unit -> Option[String] \ ef): String \ ef = region rc {
        let sb = StringBuilder.empty(rc);
        def loop() = {
            match f() {
                case None    => StringBuilder.toString(sb)
                case Some(s) => {
                    StringBuilder.appendString(s, sb);
                    loop()
                }
            }
        };
        loop()
    }

    ///
    /// Returns `true` if and only if at least one char in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `a` is empty.
    ///
    pub def exists(f: Char -> Bool \ ef, s: String): Bool \ ef = match findIndexOfLeft(f, s) {
        case None    => false
        case Some(_) => true
    }

    ///
    /// Returns `true` if and only if all chars in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def forAll(f: Char -> Bool \ ef, s: String): Bool \ ef = match findIndexOfLeft(x -> not (f(x)), s) {
        case None    => true
        case Some(_) => false
    }

    ///
    /// Returns `true` if and only if all chars in `s` are ascii characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isAscii(s: String): Bool =
        forAll(Char.isAscii, s)

    ///
    /// Returns `true` if and only if all chars in `s` are white space characters.
    ///
    /// Returns `true` if `s` is empty.
    ///
    pub def isWhiteSpace(s: String): Bool =
        forAll(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all leading space characters removed.
    ///
    pub def trimLeft(s: String): String = dropWhileLeft(Char.isWhiteSpace, s)

    ///
    /// Returns string `s` with all trailing space characters removed.
    ///
    pub def trimRight(s: String): String = dropWhileRight(Char.isWhiteSpace, s)

    ///
    /// Returns a string with the string `s` repeated `n` times.
    ///
    /// Returns the empty string if `n < 0`.
    ///
    pub def repeat(n: Int32, s: String): String =
        if (n < 0)
            ""
        else
            unsafely IO run s.repeat(n)

    ///
    /// Pad the string `s` at the left with the supplied char `c` to fit the width `w`.
    ///
    pub def padLeft(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            repeat(w - len, Char.toString(c)) + s

    ///
    /// Pad the string `s` at the right with the supplied char `c` to fit the width `w`.
    ///
    pub def padRight(w: Int32, c: Char, s: String): String =
        let len = length(s);
        if (len >= w)
            s
        else
            s + repeat(w - len, Char.toString(c))

    ///
    /// Center the string `s` by padding with the supplied char `c` on both sides to fit the width `w`.
    ///
    /// If the length of the string exceeds the given width, the string will be returned unchanged.
    ///
    /// Requiring an uneven number of spaces, the string will be left biased,
    /// e.g `String.center(4, '_', "a") -> "_a__"`.
    ///
    pub def center(w: Int32, c: Char, s: String): String = region rc {
        let space = w - length(s);
        let floor = space / 2;
        let ceil = space / 2 + Int32.remainder(space, 2);

        let prefix = repeat(floor, Char.toString(c));
        let postfix = repeat(ceil, Char.toString(c));

        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString(prefix, sb);
        StringBuilder.appendString(s, sb);
        StringBuilder.appendString(postfix, sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns `s` with every match of the substring `src` replaced by the string `dst`.
    ///
    pub def replace(src: {src = String}, dst: {dst = String}, s: String): String =
        unsafely IO run s.replace(src#src, dst#dst)

    ///
    /// Returns `s` with every match of the character `src` replaced by the character `dst`.
    ///
    pub def replaceChar(src: {src = Char}, dst: {dst = Char}, s: String): String =
        unsafely IO run s.replace(src#src, dst#dst)

    ///
    /// Returns `s` with every match of the regular expression `regex` replaced by the string `to`.
    ///
    pub def replaceMatches(regex: {regex = String}, to: {to = String}, s: String): String =
        unsafely IO run s.replaceAll(regex#regex, to#to)

    ///
    /// Returns `s` with the first match of the regular expression `regex` replaced by the string `to`.
    ///
    pub def replaceFirstMatch(regex: {regex = String}, to: {to = String}, s: String): String =
        unsafely IO run s.replaceFirst(regex#regex, to#to)

    ///
    /// Returns `s` with the element at index `i` replaced by `a`.
    ///
    /// Returns `s` if `i < 0` or `i > length(xs)-1`.
    ///
    pub def update(i: Int32, a: Char, s: String): String =
        let f = ix -> if (ix == i) a else charAt(ix, s);
        init(f, length(s))

    ///
    /// Returns `s` with the `n` elements starting at index `i` replaced with the elements of `sub`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `s` then no patching is done at these indices.
    /// If `s` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `s`, then the element at index `j` in `sub` is used.
    ///
    pub def patch(i: Int32, n: Int32, sub: String, s: String): String =
        let sublen = length(sub);
        let f = ix ->
            if (ix >= i and ix < i + n) {
                let offset = ix - i;
                if (offset < 0 or offset >= sublen) charAt(ix,s) else charAt(offset, sub)
            } else charAt(ix, s);
        init(f, length(s))


    ///
    /// Indent every line in string `s` by `n` spaces. `n` must be greater than `0`.
    ///
    /// If the string `s` in nonempty, the returned string normalizes line
    /// termination characters and adds a line terminator to the last line
    /// of the string if it does not already end with a newline.
    ///
    /// If the string `s` is empty, then the empty string is returned.
    ///
    pub def indent(n: Int32, s: String): String =
        if (n <= 0 or length(s) == 0)
            s
        else
            indentHelper(n, s)

    ///
    /// Helper function for `indent`.
    ///
    def indentHelper(n: Int32, s: String): String = region rc {
        let sb = StringBuilder.empty(rc);
        let prefix = repeat(n, " ");
        let step = x -> {
            StringBuilder.appendString(prefix, sb);
            StringBuilder.appendLine(x, sb)
        };
        List.forEach(step, lines(s));
        StringBuilder.toString(sb)
    }

    ///
    /// Strip every indented line in string `s` by `n` spaces. `n` must be greater than `0`.
    /// Note, tabs are counted as a single space.
    ///
    /// If the string `s` in nonempty, the returned string normalizes line
    /// termination characters and adds a line terminator to the last line
    /// of the string if it does not already end with a newline.
    ///
    /// If the string `s` is empty, then the empty string is returned.
    ///
    pub def stripIndent(n: Int32, s: String): String =
        if (n <= 0 or length(s) == 0)
            s
        else
            stripIndentHelper(n, s)

    ///
    /// Helper function for `stripIndent`.
    ///
    def stripIndentHelper(n: Int32, s: String): String = region rc {
        let sb = StringBuilder.empty(rc);
        let limit = Int32.min(n, length(s));
        def loop(s1, ix) = {
            if (ix >= limit)
                sliceRight(start = ix, s1)
            else if (not Char.isWhiteSpace(charAt(ix, s1)))
                sliceRight(start = ix, s1)
            else
                loop(s1, ix + 1)
        };
        let step = s1 -> {
            let line = loop(s1, 0);
            StringBuilder.appendLine(line, sb)
        };
        List.forEach(step, lines(s));
        StringBuilder.toString(sb)
    }

    ///
    /// Split the string `s` into an array of lines, breaking on newline.
    ///
    /// Newline is recognized as any Unicode linebreak sequence including
    /// Windows (carriage return, line feed) or Unix (line feed).
    ///
    pub def lines(s: String): List[String] =
        if (isEmpty(s))
            Nil
        else
            split({regex = "\\R"}, s)

    ///
    /// Join the list of strings `a` separating each pair of strings and
    /// ending the result string with the system dependent line separator.
    ///
    pub def unlines(a: List[String]): String = region rc {
        let sb = StringBuilder.empty(rc);
        List.forEach(x -> StringBuilder.appendLine(x, sb), a);
        StringBuilder.toString(sb)
    }

    ///
    /// Split the string `s` into an list of words, dividing on one or more white space characters.
    /// Leading and trailing spaces are trimmed.
    ///
    pub def words(s: String): List[String] = match trim(s) {
        case s1 if isEmpty(s1) => Nil
        case s1                => split({regex = "\\s+"}, s1)
    }

    ///
    /// Join the array of strings `l` separating each pair of strings with a
    /// single space character.
    ///
    pub def unwords(l: List[String]): String = match l {
        case Nil     => ""
        case x :: xs => region rc {
            let sb = StringBuilder.empty(rc);
            StringBuilder.appendString(x, sb);
            let f = s -> { StringBuilder.append(' ', sb); StringBuilder.appendString(s, sb) };
            List.forEach(f, xs);
            StringBuilder.toString(sb)
        }
    }

    ///
    /// Alias for `indexOfLeft`.
    ///
    pub def indexOf(substr: {substr = String}, s: String): Option[Int32] = indexOfLeft({substr = substr#substr}, s)

    ///
    /// Return the index of the first occurrence of `substr` in `s` from the left.
    ///
    /// If `substr` is not present in `s` return None.
    ///
    /// If `substr` is the empty string return None.
    ///
    pub def indexOfLeft(substr: {substr = String}, s: String): Option[Int32] =
        if (isEmpty(substr#substr))
            None
        else {
            let i = unsafely IO run s.indexOf(substr#substr);
            if (i < 0) None else Some(i)
        }

    ///
    /// Return the index of the first occurrence of `substr` in `s` from the right.
    ///
    /// If `substr` is not present in `s` return None.
    ///
    /// If `substr` is the empty string return None.
    ///
    pub def indexOfRight(substr: {substr = String}, s: String): Option[Int32] =
        if (isEmpty(substr#substr))
            None
        else {
            let i = unsafely IO run s.lastIndexOf(substr#substr);
            if (i < 0) None else Some(i)
        }

    ///
    /// Returns `true` if and only if `substr` is an infix of `s`.
    ///
    pub def contains(substr: {substr = String}, s: String): Bool =
        if (isEmpty(substr#substr))
            true
        else {
            let a = indexOfLeft({substr = substr#substr}, s);
            not Option.isEmpty(a)
        }

    ///
    /// Return the common prefix of strings `s1` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common prefix.
    ///
    pub def commonPrefix(s1: String, s2: String): String =
        let len1 = length(s1);
        let len2 = length(s2);
        def loop(ix) = {
            if (ix >= len1 or ix >= len2)
                takeLeft(ix, s1)
            else {
                let c1 = charAt(ix, s1);
                let c2 = charAt(ix, s2);
                if (c1 != c2)
                    takeLeft(ix, s1)
                else
                    loop(ix + 1)
            }
        };
        loop(0)

    ///
    /// Return the common suffix of strings `s1` and `s2`.
    ///
    /// Returns the empty string if `s1` and `s2` do not share a common suffix.
    ///
    pub def commonSuffix(s1: String, s2: String): String =
            def loop(i, j, acc) = {
                if (i < 0)
                    s1
                else if (j < 0)
                    s2
                else {
                    let c1 = charAt(i, s1);
                    let c2 = charAt(j, s2);
                    if (c1 != c2)
                        takeRight(acc, s1)
                    else
                        loop(i - 1, j - 1, acc + 1)
                }
            };
            let start1 = length(s1) - 1;
            let start2 = length(s2) - 1;
            loop(start1, start2, 0)

    ///
    /// Abbreviate the string `s` if it exceeds the width `w`.
    ///
    /// If the length of `s` exceeds `w` and `w >= 3` then `s` is truncated and the first
    /// three characters are replaced with ellipses.
    ///
    /// If the length of `s` exceeds `w` and `w < 3` then the empty string is returned.
    ///
    pub def abbreviateLeft(w: Int32, s: String): String =
        match length(s) {
            case len if len < w => s
            case _   if w < 3   => ""
            case len            => {
                let start = len - w + 3;
                "..." + slice(start = start, end = len, s)
            }
        }

    ///
    /// Abbreviate the string `s` if it exceeds the width `w`.
    ///
    /// If the length of `s` exceeds `w` and `w >= 3` then `s` is truncated and the last
    /// three characters are replaced with ellipses.
    ///
    /// If the length of `s` exceeds `w` and `w < 3` then the empty string is returned.
    ///
    pub def abbreviateRight(w: Int32, s: String): String =
        match length(s) {
            case len if len < w => s
            case _   if w < 3   => ""
            case _              => slice(start = 0, end = w - 3, s) + "..."
        }

    ///
    /// Calculate the Levenshtein distance between the strings `s` and `t`.
    ///
    /// The answer is the number deletions, insertions or substitutions needed to turn
    /// string `s` into string `t`.
    ///
    pub def levenshteinDistance(s: String, t: String): Int32 = region rc {
        let m = length(s);
        let n = length(t);
        let v0 = Array.repeat(rc, n + 1, 0);
        let v1 = Array.repeat(rc, n + 1, 0);
        forIndex(0, n, i -> Array.put(i, i, v0));
        forIndex(0, m - 1, i -> {
            Array.put(i + 1, 0, v1);
            forIndex(0, n - 1, j -> {
                let substitutionCost = if (charAt(i, s) == charAt(j, t)) 0 else 1;
                let a = Array.get(j, v1) + 1;
                let b = Array.get(j + 1, v0) + 1;
                let c = Array.get(j, v0) + substitutionCost;
                Array.put(Int32.min(a, Int32.min(b, c)), j + 1, v1)
            });
            arraySwap(v0, v1)
        });
        Array.get(n, v0)
    }

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    /// This implements a forloop, the end point `e` is inclusive.
    ///
    def forIndex(b: Int32, e: Int32, f: Int32 -> Unit \ ef): Unit \ ef =
        if (b <= e) { f(b); forIndex(b + 1, e, f) } else ()

    ///
    /// Helper function for `levenshteinDistance`.
    ///
    def arraySwap(a: Array[Int32, r1], b : Array[Int32, r2]): Unit \ { r2, r1 } =
        forIndex(0, Array.length(a) - 1, {i ->  Array.put(Array.get(i, b), i, a) })

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zip(a: String, b: String): List[(Char, Char)] =
        let len = Int32.min(length(a), length(b));
        List.unfold(i -> if (i >= len) None else Some(((charAt(i, a), charAt(i, b)), i + 1)), 0)

    ///
    /// Returns a string where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zipWith(f: (Char, Char) -> Char \ ef, a: String, b: String): String \ ef =
        let len = Int32.min(length(a), length(b));
        init(i -> f(charAt(i, a), charAt(i, b)), len)

    ///
    /// Split the string `s` on matches of `substr`.
    ///
    pub def splitOn(substr: {substr = String}, s: String): List[String] =
        let maxlen = length(s);
        let sublen = length(substr#substr);
        let step = ix ->
            // Note - to stop we have to be greater than maxlen.
            // We want to return a final empty string if `s` terminates with `substr`.
            if (ix > maxlen)
                None
            else {
                match indexOfLeftWithOffset({substr = substr#substr}, {offset = ix}, s) {
                    case None => {
                        let s1 = sliceRight(start = ix, s);
                        Some((s1, maxlen + 1))
                    }
                    case Some(ix2) => {
                        let s1 = slice(start = ix, end = ix2, s);
                        Some((s1, ix2 + sublen))
                    }
                }
            };
        List.unfold(step, 0)

    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfLeftWithOffset(substr: {substr = String}, offset: {offset = Int32}, s: String): Option[Int32] =
        if (isEmpty(substr#substr))
            None
        else {
            let ix = unsafely IO run s.indexOf(substr#substr, offset#offset);
            if (ix < 0) None else Some(ix)
        }

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `substr` is the empty string.
    ///
    pub def indexOfRightWithOffset(substr: {substr = String}, offset: {offset = Int32}, s: String): Option[Int32] =
        if (isEmpty(substr#substr))
            None
        else {
            let ix = unsafely IO run s.lastIndexOf(substr#substr, offset#offset);
            if (ix < 0) None else Some(ix)
        }

    ///
    /// Split the string `s` into chunks of length `k`, the last chunk may be smaller than `k`.
    ///
    /// `k` should be greater than 0.
    ///
    pub def toChunks(k: Int32, s: String): List[String] =
        if (k <= 0)
            Nil
        else
            toChunksHelper(k, s)

    ///
    /// Helper function for `toChunks`.
    ///
    /// Precondition: size > 0
    ///
    def toChunksHelper(size: Int32, s: String): List[String] =
        let len = length(s);
        let step = pos -> match pos {
            case i if i >= len        => None
            case i if i + size >= len => Some((sliceRight(start = i, s), len))
            case i                    => Some((slice(start = i, end = i + size, s), i + size))
        };
        List.unfold(step, 0)

    ///
    /// Find the first instance of `substr` in string `s`, return a pair of the
    /// prefix of string `s` up to `substr` and the rest of string `s` including `substr`.
    ///
    pub def breakOnLeft(substr: {substr = String}, s: String): (String, String) =
        match indexOfLeft({substr = substr#substr}, s) {
            case None    => (s, "")
            case Some(i) => (sliceLeft(end = i, s), sliceRight(start = i, s))
        }

    ///
    /// Find the last instance of `substr` in string `s`, return a pair of the
    /// initial string including `substr` and suffix from `substr`.
    ///
    pub def breakOnRight(substr: {substr = String}, s: String): (String, String) =
        let sublen = length(substr#substr);
        match indexOfRight({substr = substr#substr}, s) {
            case None    => (s, "")
            case Some(i) => (sliceLeft(end  = i + sublen, s), sliceRight(start = i + sublen, s))
        }

    ///
    /// Count the occurrences of `substr` in string `s`.
    ///
    pub def countSubstring(substr: {substr = String}, s: String): Int32 =
        let sublen = length(substr#substr);
        def loop(offset, count) = match indexOfLeftWithOffset(substr = substr#substr, offset = offset, s) {
            case None    => count
            case Some(i) => loop(i + sublen, count + 1)
        };
        if (sublen <= 0)
            0
        else
            loop(0, 0)

    ///
    /// For every line in string `s`, strip a leading prefix consisting of
    /// blanks or control characters followed by `|` from the line, if
    /// such a prefix exists.
    ///
    pub def stripMargin(s: String): String = stripMarginWith({margin = "|"}, s)

    ///
    /// For every line in string `s`, strip a leading prefix consisting of
    /// blanks or control characters followed by `margin` from the line, if
    /// such a prefix exists.
    ///
    pub def stripMarginWith(margin: {margin = String}, s: String): String =
        let l = linesWithNewLine(s);
        intercalate("", List.map(stripMarginWithSingleLine(margin), l))

    ///
    /// Helper function for `stripMarginWith`.
    /// From a single line, strips a leading prefix consisting of blanks or
    /// control characters followed by `margin`, if such a prefix exists.
    ///
    def stripMarginWithSingleLine(margin: {margin = String}, s: String): String =
        let marginLen = length(margin#margin);
        let trimmed = dropWhile(c -> c <= ' ', s);
        if (startsWith(prefix = margin#margin, trimmed))
            sliceRight(start = marginLen, trimmed)
        else
            s

    ///
    /// Helper function for `stripMarginWith`.
    ///
    /// Split the string `s` into an array of lines, breaking on newline,
    /// while keeping the newline at the end of each line.
    ///
    /// Newline is recognized as any Unicode linebreak sequence including
    /// Windows (carriage return, line feed) or Unix (line feed).
    ///
    def linesWithNewLine(s: String): List[String] =
        if (isEmpty(s))
            Nil
        else
            split({regex = "(?<=\\R)"}, s)

    ///
    /// Returns `Some(suffix)` of string `s` if its prefix matches `substr`.
    ///
    pub def stripPrefix(substr: {substr = String}, s: String): Option[String] =
        if (not startsWith(prefix = substr#substr, s))
            None
        else
            Some(dropLeft(length(substr#substr), s))

    ///
    /// Returns `Some(prefix)` of string `s` if its suffix matches `substr`.
    ///
    pub def stripSuffix(substr: {substr = String}, s: String): Option[String] =
        if (not endsWith(suffix = substr#substr, s))
            None
        else
            Some(dropRight(length(substr#substr), s))

    ///
    /// Wrap the string `s` into lines that are each at most as long as `w`, breaking on whitespace.
    ///
    /// If a line contains a single word that exceeds the length of `w`, the word will be broken.
    ///
    pub def wrap(w: Int32, s: String): String =
        if (w <= 0) ""
        else {
            let lines = List.flatMap(l -> wrapLine(w, l),  lines(s));
            intercalate(lineSeparator(), lines)
        }

    ///
    /// Helper for wrap.
    /// Takes a single line, `s`, and returns a list consisting of the split lines, each being no longer than `w`.
    ///
    def wrapLine(w: Int32, s: String): List[String] =
        if (length(s) <= w)
            s :: Nil
        else {
            // Find the last occurrence of non-whitespace followed by whitespace
            // This is the end of the first line
            let endOpt = Regex.indexOfLastWithBounds({substr = regex"(?<!\\s)\\s"}, {start = 0}, {end = w}, s);
            let end = match endOpt {
                case Some(end) => end
                case None => w
            };
            sliceLeft({end = end}, s) :: wrapLine(w, trimLeft(sliceRight({start = end}, s)))
        }

    ///
    /// Unwrap the string, replacing every newline with a space
    ///
    pub def unwrap(s: String): String =
        Regex.replace({src = regex"[\r\n]+"}, {dst = " "}, s)

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r], s: String): Iterator[Char, r, r] \ r =
        Iterator.range(rc, 0, length(s)) |> Iterator.map(i -> charAt(i, s))

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], s: String): Iterator[(Int32, Char), r, r] \ r =
        iterator(rc, s) |> Iterator.zipWithIndex

}
