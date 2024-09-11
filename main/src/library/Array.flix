/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
 * Copyright 2024 Jonathan Lindegaard Starup
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

instance Indexable[Array[a, r]] {
    type Idx = Int32
    type Elm = a
    type Aef = r + OutOfBounds

    pub def get(t: Array[a, r], i: Int32): a \ r + OutOfBounds = {
        if (0 <= i and i < Array.length(t)) Array.get(i, t)
        else do OutOfBounds.outOfBounds(
            "index ${i} is out of bounds for Array of length ${Array.length(t)}"
        )
    }
}

instance IndexableMut[Array[a, r]] {
    type Aef = r + OutOfBounds

    pub def put(t: Array[a, r], i: Int32, v: a): Unit \ r + OutOfBounds = {
        if (0 <= i and i < Array.length(t)) Array.put(v, i, t)
        else do OutOfBounds.outOfBounds(
            "index ${i} is out of bounds for Array of length ${Array.length(t)}"
        )
    }
}

instance Iterable[Array[a, r]] {
    type Elm = a
    type Aef = r

    pub def iterator(rc: Region[r1], a: Array[a, r]): Iterator[a, r + r1, r1] \ (r + r1) =
        Array.iterator(rc, a)
}

mod Array {
    import java.lang.Object
    import java.util.Arrays
    import java.util.{Random => JRandom};

    ///
    /// Compares `a` and `b` lexicographically.
    ///
    pub def compare(a: Array[v, r1], b: Array[v, r2]): Comparison \ { r1, r2 } with Order[v] =
        let len = Int32.min(Array.length(a), Array.length(b));
        def loop(i) = {
            if (i < len) {
                let cmp = get(i, a) <=> get(i, b);
                if (cmp == Comparison.EqualTo)
                    loop(i + 1)
                else
                    cmp
            } else if (i < Array.length(a)) {
                Comparison.GreaterThan
            } else if (i < Array.length(b)) {
                Comparison.LessThan
            } else {
                Comparison.EqualTo
            }
        };
        loop(0)

    ///
    /// Returns a string representation of the given array `a`.
    ///
    pub def toString(a: Array[a, r]): String \ r with ToString[a] = region rc2 {
        let sb = StringBuilder.empty(rc2);
        let first = Ref.fresh(rc2, true);

        StringBuilder.appendString!("Array#{", sb);

        let f = x -> {
            if (not Ref.get(first)) StringBuilder.appendString!(", ", sb) else Ref.put(false, first);
            StringBuilder.appendString!("${x}", sb)
        };
        forEach(f, a);

        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns a new uninitialized array of length `l` in the region `r`.
    ///
    pub def empty(rc: Region[r], l: Int32): Array[a, r] \ r =
        $ARRAY_NEW$(rc, Reflect.default(), l)

    ///
    /// Retrieves the value at position `i` in the array `a`.
    ///
    pub def get(i: Int32, a: Array[a, r]): a \ r =
        $ARRAY_LOAD$(a, i)

    ///
    /// Stores the value `x` at position `i` in the array `a`.
    ///
    pub def put(x: a, i: Int32, a: Array[a, r]): Unit \ r =
        $ARRAY_STORE$(a, i, x)

    ///
    /// Optionally returns the element at position `i` in the array `a`.
    ///
    pub def nth(i: Int32, a: Array[a, r]): Option[a] \ r =
        if (0 <= i and i < length(a))
            Some(get(i, a))
        else
            None

    ///
    /// Returns `true` if the given array `a` is empty.
    ///
    pub def isEmpty(a: Array[a, r]): Bool \ r = length(a) == 0

    ///
    /// Returns `true` if the given array `a` is non-empty.
    ///
    pub def nonEmpty(a: Array[a, r]): Bool \ r = not isEmpty(a)

    ///
    /// Returns the number of elements in the array `a`.
    ///
    pub def length(a: Array[a, r]): Int32 \ r = $ARRAY_LENGTH$(a)

    ///
    /// Returns the number of elements in the array `a`.
    ///
    pub def size(a: Array[a, r]): Int32 \ r = length(a)

    ///
    /// Returns a fresh array with the elements from the array `a` from index `start` (inclusive) until index `end` (exclusive).
    ///
    pub def slice(rc1: Region[r1], start: {start = Int32}, end: {end = Int32}, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        copyOfRange(rc1, start#start, end#end, a)

    ///
    /// Returns the array `a` as a list.
    ///
    pub def toList(a: Array[a, r]): List[a] \ r =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, a);
                loop(i - 1, x :: acc)
            }
        };
        loop(length(a), Nil)

    ///
    /// Optionally returns the array `arr` as a non-empty list.
    ///
    /// If `arr` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(arr: Array[a, r]): Option[Nel[a]] \ r =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, arr);
                loop(i - 1, Nel.cons(x, acc))
            }
        };
        if (Array.isEmpty(arr)) {
            None
        } else {
            let i = length(arr) - 1;
            Some(loop(i, Nel.singleton(get(i, arr))))
        }

    ///
    /// Returns the array `a` as a `DelayList`.
    ///
    pub def toDelayList(a: Array[a, r]): DelayList[a] \ r =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, DelayList.ECons(get(i, a), acc))
        };
        loop(length(a) - 1, DelayList.ENil)

    ///
    /// Returns the array `arr` as a chain.
    ///
    pub def toChain(arr: Array[a, r]): Chain[a] \ r =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, arr);
                loop(i - 1, Chain.cons(x, acc))
            }
        };
        loop(length(arr), Chain.empty())

    ///
    /// Optionally returns the array `arr` as a non-empty chain.
    ///
    /// If `arr` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(arr: Array[a, r]): Option[Nec[a]] \ r =
        def loop(i, acc) = {
            if (i == 0) {
                acc
            } else {
                let x = get(i - 1, arr);
                loop(i - 1, Nec.cons(x, acc))
            }
        };
        if (Array.isEmpty(arr)) {
            None
        } else {
            let i = length(arr) - 1;
            Some(loop(i, Nec.singleton(get(i, arr))))
        }

    ///
    /// Returns `a` as a MutDeque.
    ///
    pub def toMutDeque(rc1: Region[r1], a: Array[a, r2]): MutDeque[a, r1] \ { r2, r1 }  =
        let d = MutDeque.empty(rc1);
        forEach(x -> MutDeque.pushBack(x, d), a);
        d

    ///
    /// Returns `a` as a Vector.
    ///
    pub def toVector(a: Array[a, r]): Vector[a] \ r = region rc {
        let arr1 = copyOfRange(rc, 0, length(a), a);
        unchecked_cast(arr1 as Vector[a])
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def head(a: Array[a, r]): Option[a] \ r =
        if (length(a) > 0) Some(get(0, a)) else None

    ///
    /// Returns `Some(x)` if `x` is the last element of `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def last(a: Array[a, r]): Option[a] \ r =
        let len = length(a);
        if (len > 0) Some(get(len - 1, a)) else None

    ///
    /// Return a new array, appending the elements `b` to elements of `a`.
    ///
    pub def append(rc3: Region[r3], a: Array[a, r1], b: Array[a, r2]): Array[a, r3] \ { r1, r2, r3 } =
        let len1 = length(a);
        let len2 = length(b);
        if (len1 == 0)
            copyOfRange(rc3, 0, len2, b)
        else {
            let out = copyOfRange(rc3, 0, len1 + len2, a);
            updateSequence!(len1, b, out);
            out
        }

    ///
    /// Returns `true` if and only if `a` contains the element `x`.
    ///
    pub def memberOf(x: a, a: Array[a, r]): Bool \ r with Eq[a] =
        exists(y -> y == x, a)

    ///
    /// Optionally finds the smallest element of `a` according to the `Order` on `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def minimum(a: Array[a, r]): Option[a] \ r with Order[a] =
        reduceLeft(Order.min, a)

    ///
    /// Optionally finds the smallest element of `a` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, a: Array[a, r]): Option[a] \ r =
        reduceLeft(Order.minBy(cmp), a)

    ///
    /// Optionally finds the largest element of `a` according to the `Order` on `a`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def maximum(a: Array[a, r]): Option[a] \ r with Order[a] =
        reduceLeft(Order.max, a)

    ///
    /// Optionally finds the largest element of `a` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, a: Array[a, r]): Option[a] \ r =
        reduceLeft(Order.maxBy(cmp), a)

    ///
    /// Alias for `IndexOfLeft`
    ///
    pub def indexOf(x: a, a: Array[a, r]): Option[Int32] \ r with Eq[a] =
        indexOfLeft(x,a)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `arr`
    /// searching from left to right.
    ///
    pub def indexOfLeft(a: a, arr: Array[a, r]): Option[Int32] \ r with Eq[a] =
        def loop(i) = {
            if (i >= length(arr))
                -1
            else if (get(i, arr) == a)
                i
            else
                loop(i + 1)
        };
        let i = loop(0);
        if (i < 0) None else Some(i)

    ///
    /// Optionally returns the position of the first occurrence of `a` in `arr`
    /// searching from right to left.
    ///
    pub def indexOfRight(a: a, arr: Array[a, r]): Option[Int32] \ r with Eq[a] =
        def loop(i) = {
            if (i < 0)
                -1
            else if (get(i, arr) == a)
                i
            else
                loop(i - 1)
        };
        let i = loop(length(arr) - 1);
        if (i < 0) None else Some(i)

    ///
    /// Return the positions of the all the occurrences of `a` in `arr`.
    ///
    pub def indices(rc: Region[r], a: a, arr: Array[a, r]): Array[Int32, r] \ r with Eq[a] =
        findIndices(rc, b -> a == b, arr)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, arr: Array[a, r]): Option[a] \ { ef, r } =
        findLeft(f, arr)

    ///
    /// Optionally returns the first element of `a` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, arr: Array[a, r]): Option[a] \ { ef, r } =
        match findIndexOfLeft(f, arr) {
            case None    => None
            case Some(i) => Some(get(i, arr))
        }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, arr: Array[a, r]): Option[a] \ { ef, r } =
        match findIndexOfRight(x -> f(x), arr) {
            case None    => None
            case Some(i) => Some(get(i, arr))
        }

    ///
    /// Returns an array of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `[]` if `b >= e`.
    ///
    pub def range(rc: Region[r], b: Int32, e: Int32): Array[Int32, r] \ r =
        if (b >= e)
            Array#{} @ rc
        else {
            let f = x -> x + b;
            init(rc, f, e - b)
        }

    ///
    /// Returns an array with the element `x` repeated `n` times.
    ///
    /// Returns the empty array if `n <= 0`.
    ///
    pub def repeat(rc: Region[r], n: Int32, x: a): Array[a, r] \ r =
        if (n <= 0)
            Array#{} @ rc
        else
            $ARRAY_NEW$(rc, x, n)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(rc: Region[r], f: (b, a) -> b \ ef, s: b, arr: Array[a, r]): Array[b, r] \ { ef, r } =
        scanLeft(rc, f, s, arr)

    ///
    /// Accumulates the result of applying `f` to `arr` going left to right.
    ///
    /// That is, the result is of the form: `[s , f(s, x1), f(f(s, x1), x2),  ...]`.
    ///
    pub def scanLeft(rc: Region[r], f: (b, a) -> b \ ef, s: b, arr: Array[a, r]): Array[b, r] \ { ef, r } =
        let len = length(arr) + 1;
        let b = repeat(rc, len, s);
        def loop(i, acc) = {
            if (i >= len)
                ()
            else {
                let s1 = f(acc, get(i - 1, arr));
                Array.put(s1, i, b);
                loop(i + 1, s1)
            }
        };
        loop(1, s);
        b

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `[..., f(xn-1, f(xn, s)), f(xn, s), s]`.
    ///
    pub def scanRight(rc: Region[r], f: (a, b) -> b \ ef, s: b, a: Array[a, r]): Array[b, r] \ { ef, r } =
        let len = length(a);
        let b = repeat(rc, len + 1, s);
        def loop(i, acc) = {
            if (i < 0)
                ()
            else {
                let s1 = f(get(i, a), acc);
                Array.put(s1, i, b);
                loop(i - 1, s1)
            }
        };
        loop(len - 1, s);
        b

    ///
    /// Returns the result of applying `f` to every element in `a`.
    ///
    /// The result is an empty array.
    ///
    pub def map(rc1: Region[r1], f: a -> b \ ef, a: Array[a, r]): Array[b, r1] \ { ef, r, r1 } =
        let len = length(a);
        init(rc1, i -> f(get(i, a)), len)

    ///
    /// Apply `f` to every element in array `arr`. Array `arr` is mutated.
    ///
    pub def transform!(f: a -> a \ ef, arr: Array[a, r]): Unit \ { ef, r } =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                Array.put(f(get(i, arr)), i, arr);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns the result of applying `f` to every element in `a` along with that element's index.
    ///
    /// That is, the result is of the form: `[ f(a[0], 0), f(a[1], 1), ... ]`.
    ///
    pub def mapWithIndex(rc1: Region[r1], f: (Int32, a) -> b \ ef, a: Array[a, r]): Array[b, r1] \ { ef, r, r1 } =
        let len = length(a);
        init(rc1, i -> f(i, get(i, a)), len)

    ///
    /// Apply `f` to every element in array `a` along with that element's index. Array `a` is mutated.
    ///
    pub def transformWithIndex!(f: (Int32, a) -> a \ ef, arr: Array[a, r]): Unit \ { ef, r } =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                Array.put(f(i, get(i, arr)), i, arr);
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns the result of applying `f` to every element in `a` and concatenating the results.
    ///
    pub def flatMap(rc1: Region[r1], f: a -> Array[b, r1] \ ef, a: Array[a, r]): Array[b, r1] \ { ef, r, r1 } =
        let len = length(a);
        init(rc1, i -> f(get(i, a)), len) |> flatten(rc1)

    ///
    /// Returns the reverse of `a`.
    ///
    pub def reverse(rc1: Region[r1], a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        let len = length(a);
        let end = len - 1;
        init(rc1, i -> get(end - i, a), len)

    ///
    /// Reverse the array `arr`, mutating it in place.
    ///
    pub def reverse!(arr: Array[a, r]): Unit \ r =
        let len = length(arr);
        let halflen = len / 2;
        def loop(i, j) = {
            if (i >= halflen)
                ()
            else {
                let x = get(i, arr);
                let y = get(j, arr);
                put(y, i, arr);
                put(x, j, arr);
                loop(i + 1, j - 1)
            }
        };
        loop(0, len - 1)

    ///
    /// Rotate the contents of array `arr` by `n` steps to the left.
    ///
    pub def rotateLeft(rc2: Region[r2], n: Int32, arr: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        let len = length(arr);
        if (len < 1)
            Array#{} @ rc2
        else
            if (n < 0)
                rotateRightHelper(rc2, Int32.abs(n), arr)
            else
                rotateLeftHelper(rc2, n, arr)

    ///
    /// Helper function for `rotateLeft` and `rotateRight`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateLeftHelper(rc2: Region[r2], n: Int32, arr: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        let len = length(arr);
        let f = i -> { let i1 = n + i; get(i1 `Int32.remainder` len, arr) };
        init(rc2, f, len)

    ///
    /// Rotate the contents of array `arr` by `n` steps to the right.
    ///
    pub def rotateRight(rc2: Region[r2], n: Int32, arr: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        if (length(arr) < 1)
            Array#{} @ rc2
        else
            if (n < 0)
                rotateLeftHelper(rc2, Int32.abs(n), arr)
            else
                rotateRightHelper(rc2, n, arr)

    ///
    /// Helper function for `rotateRight` and `rotateLeft`.
    ///
    /// Precondition: `n` must be positive.
    ///
    /// This is an explicit helper to avoid code duplication.
    ///
    def rotateRightHelper(rc2: Region[r2], n: Int32, a: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        let len = length(a);
        let n1 = n `Int32.remainder` len;
        let start = len - n1;
        let f = i -> { let i1 = start + i; get(i1 `Int32.remainder` len, a)};
        init(rc2, f, len)

    ///
    /// Returns a copy of `a` with the element at index `i` replaced by `x`.
    ///
    /// Returns a shallow copy of `a` if `i < 0` or `i > length(xs)-1`.
    ///
    pub def update(rc1: Region[r1], i: Int32, x: a, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        let len = length(a);
        let f = ix -> if (ix == i) x else get(ix, a);
        init(rc1, f, len)

    ///
    /// Returns a copy of `a` with every occurrence of `src` replaced by `dst`.
    ///
    pub def replace(rc1: Region[r1], src: {src = a}, dst: {dst = a}, a: Array[a, r]): Array[a, r1] \ { r, r1 } with Eq[a] =
        map(rc1, e -> if (e == src#src) dst#dst else e, a)

    ///
    /// Replace every occurrence of `src` by `dst` in the array `a`, mutating it in place.
    ///
    pub def replace!(src: {src = a}, dst: {dst = a}, a: Array[a, r]): Unit \ r with Eq[a] =
        transform!(e -> if (e == src#src) dst#dst else e, a)

    ///
    /// Returns `b` with the `n` elements starting at index `i` replaced with the elements of `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    pub def patch(rc3: Region[r3], i: Int32, n: Int32, a: Array[a, r1], b: Array[a, r2]): Array[a, r3] \ { r1, r2, r3 } =
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = copyOfRange(rc3, 0, size, a);
        updateSequence(rc3, i, sub, b)

    ///
    /// Update the mutable array `b` replacing `n` elements starting at index `i` with the corresponding elements of array `a`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `b` then no patching is done at these indices.
    /// If `a` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `b`, then the element at index `j` in `a` is used.
    ///
    pub def patch!(i: Int32, n: Int32, a: Array[a, r1], b: Array[a, r2]): Unit \ { r1, r2 } = region rc3 {
        let len1 = length(a);
        let size = if (n > len1) len1 else n;
        let sub = copyOfRange(rc3, 0, size, a);
        updateSequence!(i, sub, b)
    }

    ///
    /// Returns `a` with `x` inserted between every two adjacent elements.
    ///
    pub def intersperse(rc1: Region[r1], x: a, a: Array[a, r]): Array[a, r1] \ { r, r1 }=
        let len1 = length(a);
        let len2 = len1 + len1 - 1;
        if (len2 <= 0)
            Array#{} @ rc1
        else {
            let b = repeat(rc1, len2, x);
            let f = { (i, v) -> let j = i + i; Array.put(v, j, b) };
            forEachWithIndex(f, a);
            b
        }

    ///
    /// Returns the concatenation of the elements in `arrs` with the elements of `sep` inserted between every two adjacent elements.
    ///
    pub def intercalate(rc1: Region[r1], sep: Array[a, r], arrs: Array[Array[a, r], r]): Array[a, r1] \ { r, r1 } =
        let count = length(arrs);
        let sepLength = length(sep);
        let sepCount = if (count < 2) 0 else count - 1;
        let len = sumLengths(arrs) + (sepCount * sepLength);
        match headArrays(arrs) {
            case None    => Array#{} @ rc1
            case Some(x) =>
                let out = repeat(rc1, len, x);
                let overwrite = { (st, a) ->
                    let (pos,i) = st;
                    if (i == 0) {
                        let pos1 = arrayWrites(0, a, out);
                        (pos1, 1)
                    } else {
                        let pos1 = arrayWrites(pos, sep, out);
                        let pos2 = arrayWrites(pos1, a, out);
                        (pos2, i+1)
                    }};
                discard foldLeft(overwrite, (0, 0), arrs);
                out
        }

    ///
    /// Imperatively write `sub` at position `pos` in array `out`
    ///
    /// Return the new write position which is `pos + length(sub)`.
    ///
    /// Helper function for `intercalate` which needs to do sucessive writing.
    ///
    def arrayWrites(pos: Int32, sub: Array[a, r1], out: Array[a, r2]): Int32 \ { r1, r2 } =
        updateSequence!(pos, sub, out);
        pos + length(sub)

    ///
    /// Sum the lengths of an array of arrays.
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def sumLengths(arrs: Array[Array[a, r1], r2]): Int32 \ {r1, r2} =
        foldLeft((acc, a) -> acc + length(a), 0, arrs)

    ///
    /// Find the first non-empty array of an array of arrays (left-to-right).
    ///
    /// Helper function for `intercalate` and `flatten`.
    ///
    def headArrays(arrs: Array[Array[a, r1], r2]): Option[a] \ { r1, r2 } =
        let len = length(arrs);
        def loop(i) = {
            if (i >= len)
                None
            else
                match head(get(i, arrs)) {
                    case Some(x) => Some(x)
                    case None    => loop(i + 1)
                }
        };
        loop(0)

    ///
    /// Returns the transpose of `a`.
    ///
    /// Returns `a` if the dimensions of the elements of `a` are mismatched.
    ///
    pub def transpose(rc3: Region[r3], a: Array[Array[a, r1], r2]): Array[Array[a, r3], r3] \ { r1, r2, r3 } =
        let ilen = length(a);
        if (ilen == 0)
            Array#{} @ rc3
        else {
            let jlen = length(get(0, a));
            if (jlen == 0 or uniformHelper(a, jlen))
                // Non-transposing nested copy
                init(rc3, i -> copyOfRange(rc3, 0, length(get(i, a)), get(i, a)), ilen)
            else
                init(rc3, i -> init(rc3, j -> a |> get(j) |> get(i), ilen), jlen)
        }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(a: Array[Array[a, r1], r2], l: Int32): Bool \ {r1, r2} =
        exists(x -> length(x) != l, a)

    ///
    /// Returns `true` if and only if `a1` is a prefix of `a2`.
    ///
    pub def isPrefixOf(a1: Array[a, r1], a2: Array[a, r2]): Bool \ { r1, r2 } with Eq[a] =
        let len1 = length(a1);
        if (len1 > length(a2))
            false
        else
            def loop(i) = {
                if (i >= len1)
                    true
                else if (get(i, a1) != get(i, a2))
                    false
                else
                    loop(i + 1)
            };
            loop(0)

    ///
    /// Returns `true` if and only if `a1` is a infix of `a2`.
    ///
    pub def isInfixOf(a1: Array[a, r1], a2: Array[a, r2]): Bool \ { r1, r2 } with Eq[a] =
        let len1 = length(a1);
        let len2 = length(a2);
        if (len1 > len2)
            false
        else if (len1 == 0)
            true
        else
            isInfixOfSearch(a1, a2, len1, len2, 0)

    ///
    /// Helper function for `isInfixOf` - scan a2 to find a match with first element of a1.
    ///
    /// Precondition: len1 (length of a1) > 0
    ///
    def isInfixOfSearch(a1: Array[a, r1], a2: Array[a, r2], len1: Int32, len2: Int32, j: Int32): Bool \ { r1, r2 } with Eq[a] =
        if (j >= len2)
            false
        else if (get(0, a1) == get(j, a2))
            isInfixOfCheck(a1, a2, len1, len2, 1, j+1)
        else
            isInfixOfSearch(a1, a2, len1, len2, j+1)

    ///
    /// Helper function for `isInfixOf` - a1 has started matching, scan to see if it all matches.
    ///
    def isInfixOfCheck(a1: Array[a, r1], a2: Array[a, r2], len1: Int32, len2: Int32, i: Int32, j: Int32): Bool \ { r1, r2 } with Eq[a] =
        if (i >= len1)
            // a1 exhausted, so success
            true
        else if (j >= len2)
            // a2 exhausted, a1 still trying to match, so failure
            false
        else if (get(i, a1) == get(j, a2))
            isInfixOfCheck(a1, a2, len1, len2, i+1, j+1)
        else
            isInfixOfSearch(a1, a2, len1, len2, j+1)

    ///
    /// Returns `true` if and only if `a1` is a suffix of `a2`.
    ///
    pub def isSuffixOf(a1: Array[a, r1], a2: Array[a, r2]): Bool \ { r1, r2 } with Eq[a] =
        let len1 = length(a1);
        let len2 = length(a2);
        if (len1 > len2)
            false
        else
            def loop(i, j) = {
                if (i < 0)
                    true
                else if (get(i, a1) != get(j, a2))
                    false
                else
                    loop(i - 1, j - 1)
            };
            loop(len1 - 1, len2 - 1)

    ///
    /// Returns the result of applying `combine` to all the elements in `a`, using `empty` as the initial value.
    ///
    pub def fold(arr: Array[a, r]): a \ r with Monoid[a] = foldLeft(Monoid.combine, Monoid.empty(), arr)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, a[0]), a[1])..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, arr: Array[a, r]): b \ { ef, r } =
        let len = length(arr);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, arr)))
        };
        loop(0, s)

    ///
    /// Applies `f` to a start value `s` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, arr: Array[a, r]): b \ { ef, r } =
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, arr), acc))
        };
        loop(length(arr) - 1, s)

    ///
    /// Applies `f` to a start value `z` and all elements in `a` going from right to left.
    ///
    /// That is, the result is of the form: `f(a[0], ...f(a[n-1], f(a[n], z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ {ef,  r}) -> b \ {ef, r}, z: b, arr: Array[a, r]): b \ { ef, r } =
        def loop(i) = {
            if (i == length(arr))
                z
            else
                f(get(i, arr), _ -> loop(i + 1))
        };
        loop(0)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, arr: Array[a, r]): b \ { ef, r } with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), arr)

    ///
    /// Applies `f` to all elements in `a` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `a` is empty.
    ///
    pub def reduceLeft(f: (a, a) ->  a \ ef, a: Array[a, r]): Option[a] \ { ef, r } =
        let len = length(a);
        def loop(i, acc) = {
            if (i >= len)
                acc
            else
                loop(i + 1, f(acc, get(i, a)))
        };
        if (len == 0)
            None
        else
            Some(loop(1, get(0, a)))

    ///
    /// Applies `f` to all elements in `arr` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// Returns `None` if `arr` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, arr: Array[a, r]): Option[a] \ { ef, r } =
        let len = length(arr);
        def loop(i, acc) = {
            if (i < 0)
                acc
            else
                loop(i - 1, f(get(i, arr), acc))
        };
        if (len == 0)
            None
        else
            Some(loop(len - 2, get(len - 1, arr)))

    ///
    /// Returns the number of elements in `a` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, a: Array[a, r]): Int32 \ { ef, r } =
        foldLeft((b, x) -> if (f(x)) b + 1 else b, 0, a)

    ///
    /// Returns the sum of all elements in the array `a`.
    ///
    pub def sum(a: Array[Int32, r]): Int32 \ r =
        foldLeft((acc, x) -> acc + x, 0, a)

    ///
    /// Returns the sum of all elements in the array `a` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, a: Array[a, r]): Int32 \ { ef, r } =
        foldLeft((acc, x) -> acc + f(x), 0, a)

    ///
    /// Returns the concatenation of the arrays of in the array `arrs`.
    ///
    pub def flatten(rc1: Region[r1], arrs: Array[Array[a, r], r]) : Array[a, r1] \ { r, r1 } =
        let len = sumLengths(arrs);
        match headArrays(arrs) {
            case None => Array#{} @ rc1
            case Some(x) => {
                let out = repeat(rc1, len, x);
                discard foldLeft((pos, a) -> arrayWrites(pos, a, out), 0, arrs);
                out
            }
        }

    ///
    /// Returns `true` if and only if at least one element in `arr` satisfies the predicate `f`.
    ///
    /// Returns `false` if `arr` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, arr: Array[a, r]): Bool \ { ef, r } =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                false
            else if (f(get(i, arr)))
                true
            else
                loop(i + 1)
        };
        loop(0)

    ///
    /// Returns `true` if and only if all elements in `arr` satisfy the predicate `f`.
    ///
    /// Returns `true` if `arr` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, arr: Array[a, r]): Bool \ { ef, r } =
        let len = length(arr);
        def loop(i) = {
            if (i >= len)
                true
            else if (f(get(i, arr)))
                loop(i + 1)
            else
                false
        };
        loop(0)

    ///
    /// Returns an array of every element in `arr` that satisfies the predicate `f`.
    ///
    pub def filter(rc1: Region[r1], f: a -> Bool \ ef, arr: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        let len = length(arr);
        if (len < 1) {
            Array#{} @ rc1
        } else {
            let out = empty(rc1, len);
            def loop(i, j) = {
                if (i >= len)
                    j
                else {
                    let x = get(i, arr);
                    if (f(x)) {
                        Array.put(x, j, out);
                        loop(i + 1, j + 1)
                    } else {
                        loop(i + 1, j)
                    }
                }
            };
            let endPos = loop(0, 0);
            copyOfRange(rc1, 0, endPos, out)
        }

    ///
    /// Returns a pair of arrays `(a1, a2)`.
    ///
    /// `a1` contains all elements of `a` that satisfy the predicate `f`.
    /// `a2` contains all elements of `a` that do not satisfy the predicate `f`.
    ///
    pub def partition(rc1: Region[r1], rc2: Region[r2], f: a -> Bool \ ef, a: Array[a, r]): (Array[a, r1], Array[a, r2]) \ { ef, r, r1, r2 } =
        let step = { (x, acc) ->
            let (a1, a2) = acc;
            if (f(x)) (x :: a1, a2) else (a1, x :: a2)
        };
        let (xs, ys) = foldRight(step, (Nil, Nil), a);
        (List.toArray(rc1, xs), List.toArray(rc2, ys))

    ///
    /// Returns a pair of arrays `(a1, a2)`.
    ///
    /// `a1` is the longest prefix of `a` that satisfies the predicate `f`.
    /// `a2` is the remainder of `a`.
    ///
    pub def span(rc1: Region[r1], rc2: Region[r2], f: a -> Bool \ ef, a: Array[a, r]): (Array[a, r1], Array[a, r2]) \ { ef, r, r1, r2 } =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None    => (takeLeft(rc1, length(a), a), Array#{} @ rc2)
            case Some(i) => (takeLeft(rc1, i, a), dropLeft(rc2, i, a))
        }

    ///
    /// Alias for `dropLeft`.
    ///
    pub def drop(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        dropLeft(rc1, n, a)

    ///
    /// Returns a copy of array `a`, dropping the first `n` elements.
    ///
    /// Returns `[]` if `n > length(a)`.
    ///
    pub def dropLeft(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        let len = length(a);
        if (n > len)
            Array#{} @ rc1
        else {
            let start = if (n < 0) 0 else n;
            copyOfRange(rc1, start, len, a)
        }

    ///
    /// Returns a copy of array `a`, dropping the last `n` elements.
    ///
    /// Returns `[]` if `n > length(a)`.
    ///
    pub def dropRight(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        let len = length(a);
        if (n >= len)
            Array#{} @ rc1
        else {
            let end = if (n < 0) len else len - n;
            copyOfRange(rc1, 0, end, a)
        }

    ///
    /// Alias for `dropWhileLeft`.
    ///
    pub def dropWhile(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        dropWhileLeft(rc1, f, a)

    ///
    /// Returns copy of array `a` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None    => Array#{} @ rc1
            case Some(i) => dropLeft(rc1, i, a)
        }

    ///
    /// Returns copy of array `a` without the longest suffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        match findIndexOfRight(x -> not (f(x)), a) {
            case None    => Array#{} @ rc1
            case Some(i) => copyOfRange(rc1, 0, i + 1, a)
        }

    ///
    /// Alias for `takeLeft`.
    ///
    pub def take(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        takeLeft(rc1, n, a)

    ///
    /// Returns a fresh array taking first `n` elements of `a`.
    ///
    /// Returns `a` if `n > length(xs)`.
    ///
    pub def takeLeft(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        if (n <= 0)
            Array#{} @ rc1
        else {
            let len = length(a);
            let end = if (n > len) len else n;
            copyOfRange(rc1, 0, end, a)
        }

    ///
    /// Returns a fresh array taking last `n` elements of `a`.
    ///
    /// Returns `a` if `n > length(xs)`.
    ///
    pub def takeRight(rc1: Region[r1], n: Int32, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        if (n <= 0)
            Array#{} @ rc1
        else {
            let len = length(a);
            let start = if (n > len) 0 else len - n;
            copyOfRange(rc1, start, len, a)
        }

    ///
    /// Alias for `takeWhileLeft`.
    ///
    pub def takeWhile(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        takeWhileLeft(rc1, f, a)

    ///
    /// Returns the longest prefix of `a` that satisfies the predicate `f`.
    ///
    pub def takeWhileLeft(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        match findIndexOfLeft(x -> not (f(x)), a) {
            case None    => copyOfRange(rc1, 0, length(a), a)
            case Some(i) => takeLeft(rc1, i, a)
        }

    ///
    /// Returns the longest suffix of `a` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(rc1: Region[r1], f: a -> Bool \ ef, a: Array[a, r]): Array[a, r1] \ { ef, r, r1 } =
        match findIndexOfRight(x -> not (f(x)), a) {
            case None    => copyOfRange(rc1, 0, length(a), a)
            case Some(i) => copyOfRange(rc1, i + 1, length(a), a)
        }

    ///
    /// Partitions `a` into subarrays such that for any two elements `x` and `y` in a subarray, `f(x, y)` is true.
    ///
    /// A subarray is created by iterating through the remaining elements of `a` from left to right and adding an
    /// element to the subarray if and only if doing so creates no conflicts with the elements already in the subarray.
    ///
    /// The function `f` must be pure.
    ///
    pub def groupBy(rc1: Region[r1], f: (a, a) -> Bool, a: Array[a, r2]): Array[Array[a, r1], r1] \ { r2, r1 } =
        let xs = toList(a);
        groupByHelper(rc1, f, xs, Nil) |> List.toArray(rc1)

    ///
    /// Helper function for `groupBy`.
    ///
    def groupByHelper(rc: Region[r], f: (a, a) -> Bool, xs: List[a], ac: List[Array[a, r]]): List[Array[a, r]] \ r = match xs {
        case Nil     => List.reverse(ac)
        case x :: rs =>
            let (rc1, rc2) = extractHelper(rc, f, rs, x :: Nil, Nil);
            groupByHelper(rc, f, rc2, rc1 :: ac)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def extractHelper(rc: Region[r], f: (a, a) -> Bool, xs: List[a], ps: List[a], ns: List[a]): (Array[a, r], List[a]) \ r = match xs {
        case Nil => {
            let a = List.reverse(ps);
            (List.toArray(rc, a), List.reverse(ns))
        }
        case x :: rs =>
            if (agreeHelper(f, x, ps))
                extractHelper(rc, f, rs, x :: ps, ns)
            else
                extractHelper(rc, f, rs, ps, x :: ns)
    }

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, xs: List[a]): Bool = match xs {
        case Nil => true
        case x :: rs =>
            if ((f(x, a) and f(a, x)) == false)
                false
            else
                agreeHelper(f, a, rs)
    }

    ///
    /// Returns an array where the element at index `i` is `(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zip(rc3: Region[r3], a: Array[a, r1], b: Array[b, r2]): Array[(a, b), r3] \ { r1, r2, r3 } =
        let len = Int32.min(length(a), length(b));
        init(rc3, i -> (get(i, a), get(i, b)), len)

    ///
    /// Returns an array where the element at index `i` is `f(x, y)` where
    /// `x` is the element at index `i` in `a` and `y` is the element at index `i` in `b`.
    ///
    /// If either `a` or `b` becomes depleted, then no further elements are added to the resulting array.
    ///
    pub def zipWith(rc3: Region[r3], f: (a, b) -> c \ ef, a: Array[a, r1], b: Array[b, r2]): Array[c, r3] \ { ef, r1, r2, r3 } =
        let len = Int32.min(length(a), length(b));
        init(rc3, i -> f(get(i, a), get(i, b)), len)

    ///
    /// Returns a pair of arrays, the first containing all first components in `a`
    /// and the second containing all second components in `a`.
    ///
    pub def unzip(rc1: Region[r1], rc2: Region[r2], a: Array[(a, b), r3]): (Array[a, r1], Array[b, r2]) \ { r1, r2, r3 } =
        let len = length(a);
        if (len <= 0)
            (Array#{} @ rc1, Array#{} @ rc2)
        else {
            let (x, y) = get(0, a);
            let arr = repeat(rc1, len, x);
            let brr = repeat(rc2, len, y);
            def loop(i) = {
                if (i < len) {
                    let (l, r) = get(i, a);
                    Array.put(l, i, arr);
                    Array.put(r, i, brr);
                    loop(i + 1)
                } else
                    ()
            };
            loop(1);
            (arr, brr)
        }

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, a: Array[a, r1], b: Array[b, r2]): c \ { ef, r1, r2 } =
        foldLeft2(f, c, a, b)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) ->  c \ ef, c: c, a: Array[a, r1], b: Array[b, r2]): c \ { ef, r1, r2 } =
        let lena = length(a);
        let lenb = length(b);
        def loop(i, acc) = {
            if (i >= lena or i >= lenb)
                acc
            else
                loop(i + 1, f(acc, get(i, a), get(i, b)))
        };
        loop(0, c)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `a` and `b`
    /// starting with the initial value `c` and going from right to left.
    ///
    pub def foldRight2(f: (a, b, c) -> c \ ef, c: c, a: Array[a, r1], b: Array[b, r2]): c \ { ef, r1, r2 } =
        def loop(i, j, acc) = {
            if (i < 0 or j < 0)
                acc
            else
                loop(i - 1, j - 1, f(get(i, a), get(j, b), acc))
        };
        let starta = length(a) - 1;
        let startb = length(b) - 1;
        loop(starta, startb, c)

    ///
    /// Collects the results of applying the partial function `f` to every element in `a`.
    ///
    pub def filterMap(rc1: Region[r1], f: a -> Option[b] \ ef, a: Array[a, r]): Array[b, r1] \ { ef, r, r1 } =
        foldRight((x, xs) -> match f(x) {
            case None    => xs
            case Some(b) => b :: xs }, Nil, a) |> List.toArray(rc1)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, a: Array[a, r]): Option[b] \ { ef, r } =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                None
            else
                let x = f(get(i, a));
                match x {
                    case Some(v) => Some(v)
                    case None    => loop(i + 1)
                }
        };
        loop(0)

    ///
    /// Returns the array `a` as a set.
    ///
    pub def toSet(a: Array[a, r]): Set[a] \ r with Order[a] =
        foldRight(Set.insert, Set.empty(), a)

    ///
    /// Returns the association list `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(a: Array[(a, b), r]): Map[a, b] \ r with Order[a] =
        foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), a)

    ///
    /// Returns the array `a` as a MutList.
    ///
    pub def toMutList(rc1: Region[r1], a: Array[a, r2]): MutList[a, r1] \ { r2, r1 } =
        let minCap = MutList.minCapacity();
        let len = length(a);
        let c = Order.max(len, minCap);
        let copy = copyOfRange(rc1, 0, c, a);
        new MutList @ rc1{rc = rc1, a = copy, l = len }

    ///
    /// Alias for `findIndexOfLeft`.
    ///
    pub def findIndexOf(f: a -> Bool \ ef, a: Array[a, r]): Option[Int32] \ { ef, r } =
        findIndexOfLeft(f,a)

    ///
    /// Optionally returns the position of the first element in `x` satisfying `f`.
    ///
    pub def findIndexOfLeft(f: a -> Bool \ ef, a: Array[a, r]): Option[Int32] \ { ef, r } =
        let len = length(a);
        if (len < 1)
            None
        else {
            def loop(i) = {
                if (i >= len)
                    -1
                else if (f(get(i, a)))
                    i
                else
                    loop(i + 1)
            };
            let i = loop(0);
            if (i < 0) None else Some(i)
        }

    ///
    /// Optionally returns the position of the first element in `a` satisfying `f`
    /// searching from right to left.
    ///
    pub def findIndexOfRight(f: a -> Bool \ ef, a: Array[a, r]): Option[Int32] \ { ef, r } =
        let len = length(a);
        def loop(i) = {
            if (i < 0)
                -1
            else if (f(get(i, a)))
                i
            else
                loop(i - 1)
        };
        let i = loop(len - 1);
        if (i < 0) None else Some(i)

    ///
    /// Returns the positions of the all the elements in `a` satisfying `f`.
    ///
    pub def findIndices(rc2: Region[r2], f: a -> Bool \ ef, a: Array[a, r1]): Array[Int32, r2] \ { ef, r1, r2 } =
        let len = length(a);
        let l = MutList.empty(rc2);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                if (f(get(i, a))) MutList.push!(i, l) else ();
                loop(i + 1)
            }
        };
        loop(0);
        MutList.toArray(rc2, l)

    ///
    /// Build an array of length `len` by applying `f` to the successive indices.
    ///
    pub def init(rc: Region[r], f: Int32 -> a \ ef, len: Int32): Array[a, r] \ { ef, r } =
        if (len <= 0)
            Array#{} @ rc
        else {
            let x = f(0);
            let a = Array.repeat(rc, len, x);
            def loop(i) = {
                if (i < len) {
                    Array.put(f(i), i, a);
                    loop(i + 1)
                } else
                    ()
            };
            loop(1);
            a
        }

    ///
    /// Returns `true` if arrays `a` and `b` have the same elements in the same order, i.e. are structurally equal.
    ///
    pub def sameElements(a: Array[a, r1], b: Array[a, r2]): Bool \ { r1, r2 } with Eq[a] =
        let alen = length(a);
        let blen = length(b);
        def loop(i) = {
            if (i >= alen)
                true
            else if (get(i, a) != get(i, b))
                false
            else
                loop(i + 1)
        };
        if (alen == blen)
            loop(0)
        else
            false

    ///
    /// Returns an iterator over `a`
    ///
    /// Modifying `a` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def iterator(rc: Region[r1], a: Array[a, r2]): Iterator[a, r1 + r2, r1] \ {r1, r2} =
        Iterator.range(rc, 0, length(a)) |> Iterator.map(i -> Array.get(i, a))

    ///
    /// Returns an iterator over `a` zipped with the indices of the elements.
    ///
    /// Modifying `a` while using an iterator has undefined behavior and is dangerous.
    ///
    pub def enumerator(rc: Region[r1], a: Array[a, r2]): Iterator[(Int32, a), r1 + r2, r1] \ {r1, r2} =
        iterator(rc, a) |> Iterator.zipWithIndex

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    pub def forEach(f: a -> Unit \ ef, a: Array[a, r]): Unit \ { ef, r } =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(get(i, a));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Apply the effectful function `f` to all the elements in the array `a`.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, a: Array[a, r]): Unit \ { ef, r } =
        let len = length(a);
        def loop(i) = {
            if (i >= len)
                ()
            else {
                f(i, get(i, a));
                loop(i + 1)
            }
        };
        loop(0)

    ///
    /// Returns a copy of `a` with the elements starting at index `i` replaced by `sub`.
    ///
    pub def updateSequence(rc3: Region[r3], i: Int32, sub: Array[a, r1], a: Array[a, r2]): Array[a, r3] \ { r1, r2, r3 } =
        let end = i + length(sub);
        let f = ix -> if (ix >= i and ix < end) get(ix - i, sub) else get(ix, a);
        let len = length(a);
        init(rc3, f, len)

    ///
    /// Update the mutable array `a` with the elements starting at index `i` replaced by `sub`.
    ///
    pub def updateSequence!(i: Int32, sub: Array[a, r1], a: Array[a, r2]): Unit \ { r1, r2 } =
        let end = i + length(sub);
        let f = { (ix, _) ->
            if (ix >= i and ix < end)
                Array.put(get(ix - i, sub), ix, a)
            else
                ()
        };
        forEachWithIndex(f, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` with `sep` inserted between each element.
    ///
    pub def join(sep: String, a: Array[a, r]): String \ r with ToString[a] =
        joinWith(ToString.toString, sep, a)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `a` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, a: Array[a, r]): String \ { ef, r } = region rc1 {
        use StringBuilder.append!;
        let lastSep = String.length(sep);
        let sb = StringBuilder.empty(rc1);
        forEach(x -> { append!(f(x), sb); append!(sep, sb) }, a);
        StringBuilder.toString(sb) |> String.dropRight(lastSep)
    }

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(rc1: Region[r1], a: Array[a, r]): Array[a, r1] \ { r, r1 } with Order[a] =
        sortWith(rc1, Order.compare, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the `Order` instance for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(rc1: Region[r1], f: a -> b, a: Array[a, r]): Array[a, r1] \ { r, r1 } with Order[b] =
        sortWith(rc1, Order.compare `on` f, a)

    ///
    /// Returns a sorted copy of array `a`, where the elements are ordered from low to high according to
    /// the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(rc1: Region[r1], cmp: (a,a) -> Comparison, a: Array[a, r]): Array[a, r1] \ { r, r1 } =
        let len = length(a);
        let b = copyOfRange(rc1, 0, len, a);
        sortWith!(cmp, b);
        b

    ///
    /// Sort array `a` so that elements are ordered from low to high according to their `Order` instance.
    /// The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort!(a: Array[a, r]): Unit \ r with Order[a] =
        sortWith!(Order.compare, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the `Order` instance for
    /// the values obtained by applying `f` to each element. The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy!(f: a -> b, a: Array[a, r]): Unit \ r with Order[b] =
        sortWith!(Order.compare `on` f, a)

    ///
    /// Sort array `a` so that elements are ordered from low to high according to the comparison function `cmp`.
    /// The array is mutated in-place.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith!(cmp: (a,a) -> Comparison, a: Array[a, r]): Unit \ r =
        sortWithin!(cmp, 0, length(a) - 1, a)

    ///
    /// Sort array `a` between the indices `lo` and `hi` (both inclusive) so that elements in that range are ordered
    /// from low to high according to the comparison function `cmp`. The array is mutated in-place where elements
    /// outside the specified range are not changed. If `lo >= hi`, this does nothing.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `a`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWithin!(cmp: (a,a) -> Comparison, lo: Int32, hi: Int32, a: Array[a, r]): Unit \ r=
        if (lo >= hi)
            ()
        else {
            let p = quicksortPartition(cmp, a, lo, hi);
            sortWithin!(cmp, lo, p - 1, a);
            sortWithin!(cmp, p + 1, hi, a)
        }

    ///
    /// Swap the elements at `i` and `j` (helper for sorting).
    ///
    /// Precondition: `i` and `j` are within bounds.
    ///
    pub def swap!(i: Int32, j: Int32, a: Array[a, r]): Unit \ r =
        let x = get(i, a);
        let y = get(j, a);
        Array.put(y, i, a);
        Array.put(x, j, a)

    ///
    /// Partition step of the Quicksort algorithm.
    ///
    def quicksortPartition(cmp: (a,a) -> Comparison, a: Array[a, r], lo: Int32, hi: Int32): Int32 \ r =
        let pivot = get(hi, a);
        let i = forWithAccum(lo, hi, lo, (j,ix) ->
            if (cmp(get(j, a), pivot) == Comparison.LessThan) {
                swap!(ix, j, a);
                ix+1
            } else
                ix
            );
        swap!(i, hi, a);
        i

    ///
    /// A for loop with an accumulator (helper for sorting).
    ///
    /// precondition: lo <= hi
    ///
    def forWithAccum(lo: Int32, hi: Int32, ac: a, f: (Int32, a) -> a \ ef): a \ ef =
        if (lo == hi)
            f(lo, ac)
        else {
            let ac1 = f(lo, ac);
            forWithAccum(lo+1, hi, ac1, f)
        }

    ///
    /// Copies the range between `b` (inclusive) and `e` (exclusive) of a into an empty array.
    ///
    /// A valid range has the following properties:
    ///   - 0 <= `b` <= `length(a)`.
    ///   - `e` >= `b`.
    /// If the range is valid and greater than the length of `a`, the resulting array with have the length of the range
    /// and include the specified range of `a`.
    ///
    pub def copyOfRange(_: Region[r2], b: Int32, e: Int32, a: Array[a, r1]): Array[a, r2] \ { r1, r2 } =
        // All of the below casts are required to cast the type to `Array[a, ...]`.
        typematch a {
            // The casts here are used to both modify the java method type and to simulate GADT typing
            case arr: Array[Bool, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Char, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Float32, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Float64, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Int8, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Int16, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Int32, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[Int64, r1] =>
                unchecked_cast(Arrays.copyOfRange(arr, b, e) as Array[a, r2] \ r1 + r2)
            case arr: Array[a, r1] =>
                // `a` must be non-primitive now
                let arrObj = unchecked_cast(arr as Array[Object, r1]);
                unchecked_cast(Arrays.copyOfRange(arrObj, b, e) as Array[a, r2] \ r1 + r2)
            case _: _ => unreachable!()
        }

    ///
    /// Shuffles `a` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(a: Array[a, r]): Unit \ {r, NonDet} = unchecked_cast({
        let rnd = new JRandom();
        let len = Array.length(a);
        def loop(i) = {
            if (i > 0) {
                let j = rnd.nextInt(i + 1);
                Array.swap!(i, j, a);
                loop(i - 1)
            }
            else
                ()
        };
        loop(len - 1)
    } as _ \ {r, NonDet})

}
