/*
 * Copyright 2019 Stephen Tetley
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

import java.lang.{StringBuilder => JStringBuilder}

///
/// Represents a StringBuilder.
///
struct StringBuilder[r] {builder: JStringBuilder}

mod StringBuilder {
    import java.lang.{StringBuilder => JStringBuilder};

    ///
    /// Returns a new mutable StringBuilder.
    ///
    pub def empty(rc: Region[r]): StringBuilder[r] \ r =
        new StringBuilder @ rc {builder = checked_ecast(unsafe new JStringBuilder())}

    ///
    /// Append `x` to the StringBuilder `sb`.
    ///
    pub def append!(x: a, sb: StringBuilder[r]): Unit \ r with ToString[a] =
        let s = ToString.toString(x);
        s `appendString!` sb;
        ()

    ///
    /// Append the String `s` to the StringBuilder `sb`.
    ///
    pub def appendString!(s: String, sb: StringBuilder[r]): Unit \ r =
        unchecked_cast((unsafe sb->builder.append(s)) as _ \ r);
        ()

    ///
    /// Append the code point `cp` to the StringBuilder `sb`.
    ///
    pub def appendCodePoint!(cp: Int32, sb: StringBuilder[r]): Unit \ r =
        unchecked_cast((unsafe sb->builder.appendCodePoint(cp)) as _ \ r);
        ()

    ///
    /// Append the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLineSeparator!(sb: StringBuilder[r]): Unit \ r =
        String.lineSeparator() `appendString!` sb

    ///
    /// Append the String `s` followed by the system line separator to the StringBuilder `sb`.
    ///
    pub def appendLine!(s: String, sb: StringBuilder[r]): Unit \ r =
        s `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends `f(x)` to the string builder `sb`.
    ///
    pub def appendLineWith!(f: a -> String \ ef, x: a, sb: StringBuilder[r]): Unit \ { ef, r } =
        f(x) `appendString!` sb;
        appendLineSeparator!(sb)

    ///
    /// Appends each string in the array `a` to the string builder `sb`.
    ///
    pub def appendLines!(a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { r1, r2 } =
        Array.forEach(x -> appendLine!(x, sb), a)

    ///
    /// Appends `f(x)` for each x in the foldable collection `t` to the string builder `sb`.
    ///
    pub def appendLinesWith(f: a -> String \ ef, t: t[a], sb: StringBuilder[r]): Unit \ (ef + r + Foldable.Aef[t]) with Foldable[t] =
        Foldable.forEach(x -> appendLineWith!(f, x, sb), t)

    ///
    /// Appends `f(x)` for each x in array `a` to the string builder `sb`.
    ///
    pub def appendLinesWith!(f: a -> String \ ef, a: Array[a, r1], sb: StringBuilder[r2]): Unit \ { ef, r1, r2 } =
        Array.forEach(x -> appendLineWith!(f, x, sb), a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate!(sep: String, a: Array[String, r1], sb: StringBuilder[r2]): Unit \ { r1, r2 } =
        let append1! = (i, s) ->
            if (i > 0) {
                appendString!(sep, sb);
                appendString!(s, sb)
            } else {
                appendString!(s, sb)
            };
       Array.forEachWithIndex(append1!, a)

    ///
    /// Returns an iterator over `sb`.
    ///
    pub def iterator(rc: Region[r1], sb: StringBuilder[r2]): Iterator[Char, r1 + r2, r1] \ { r1, r2 } =
        Iterator.range(rc, 0, length(sb)) |> Iterator.map(i -> unchecked_cast((unsafe sb->builder.charAt(i)) as _ \ r2))

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(rc1: Region[r1], sb: StringBuilder[r2]): Iterator[(Int32, Char), r1 + r2, r1] \ { r1, r2 } =
        iterator(rc1, sb) |> Iterator.zipWithIndex

    ///
    /// Return the number of characters in the StringBuilder `sb`.
    ///
    pub def length(sb: StringBuilder[r]): Int32 \ r =
        unchecked_cast((unsafe sb->builder.length()) as _ \ r)

    ///
    /// Return the number of characters in the StringBuilder `sb`.
    ///
    pub def size(sb: StringBuilder[r]): Int32 \ r = length(sb)

    ///
    /// Sets the length of the StringBuilder `sb`.
    ///
    pub def setLength!(newLength: Int32, sb: StringBuilder[r]): Unit \ r =
        unchecked_cast((unsafe sb->builder.setLength(newLength)) as _ \ r)

    ///
    /// Convert the StringBuilder `sb` to a string.
    ///
    pub def toString(sb: StringBuilder[r]): String \ r =
        unchecked_cast(unsafe sb->builder.toString() as _ \ r)

}
