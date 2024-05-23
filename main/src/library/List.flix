/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] with Sendable {
    case Nil,
    case Cons(t, List[t])
}

instance ToString[List[a]] with ToString[a] {
    pub def toString(l: List[a]): String = List.toString(l)
}

instance Hash[List[a]] with Hash[a] {
    pub def hash(l: List[a]): Int32 =
        List.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), l)
}

instance Eq[List[a]] with Eq[a] {
    pub def eq(l1: List[a], l2: List[a]): Bool = match (l1, l2) {
        case (Nil, Nil) => true
        case (x :: rs, y :: qs) => if (x != y) false else rs == qs
        case _ => false
    }
}

instance Order[List[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: List[a], l2: List[a]): Comparison = match (l1, l2) {
        case (_ :: _, Nil) => Comparison.GreaterThan
        case (Nil, Nil) => Comparison.EqualTo
        case (Nil, _ :: _) => Comparison.LessThan
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == Comparison.EqualTo) zs <=> ws else cmp
    }

}

instance Functor[List] {
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef = List.map(f, l)
}

instance Applicative[List] {
    pub def point(a: a) : List[a] = List.point(a)
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef = List.ap(f, x)
}

instance Monad[List] {
    pub def flatMap(f: a -> List[b] \ ef, x : List[a]) : List[b] \ ef = List.flatMap(f, x)
}

instance MonadZero[List] {
    pub def empty(): List[a] = Nil
}

instance MonadZip[List] {
    pub def zipWith(f: (a, b) -> c \ ef, xs: List[a], ys: List[b]): List[c] \ ef = List.zipWith(f, xs, ys)
    pub def zipWithA(f: (a, b) -> f[c] \ ef, xs: List[a], ys: List[b]): f[List[c]] \ ef with Applicative[f] = List.zipWithA(f, xs, ys)
    pub override def zip(xs: List[a], ys: List[b]): List[(a, b)] = List.zip(xs, ys)
    pub override def unzip(xs: List[(a, b)]): (List[a], List[b]) = List.unzip(xs)
}

instance Foldable[List] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = List.foldLeft(f, s, l)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef = List.foldRight(f, s, l)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, l: List[a]): b \ ef = List.foldRightWithCont(f, s, l)
}

instance UnorderedFoldable[List] {
    pub def foldMap(f: a -> b \ ef, l: List[a]): b \ ef with CommutativeMonoid[b] = List.foldMap(f, l)
    override pub def isEmpty(l: List[a]): Bool = List.isEmpty(l)
    override pub def exists(f: a -> Bool \ ef, l: List[a]): Bool \ ef = List.exists(f, l)
    override pub def forAll(f: a -> Bool \ ef, l: List[a]): Bool \ ef = List.forAll(f, l)
    override pub def memberOf(x: a, l: List[a]): Bool with Eq[a] = List.memberOf(x, l)
}

instance Traversable[List] {
    pub def traverse(f: a -> m[b] \ ef, t: List[a]): m[List[b]] \ ef with Applicative[m] = List.traverse(f, t)
    pub override def sequence(t: List[m[a]]): m[List[a]] with Applicative[m] = List.sequence(t)
}

instance Filterable[List] {
    pub def filterMap(f: a -> Option[b] \ ef, x: List[a]): List[b] \ ef = List.filterMap(f, x)
    pub override def filter(f: a -> Bool \ ef, x: List[a]): List[a] \ ef = List.filter(f, x)
}

instance Witherable[List]

instance SemiGroup[List[a]] {
    pub def combine(x: List[a], y: List[a]): List[a] = x ::: y
}

instance Monoid[List[a]] {
    pub def empty(): List[a] = Nil
}

instance Collectable[List[a]] {
    type Elm = a
    pub def collect(iter: Iterator[a, ef, r]): List[a] \ { r, ef } = Iterator.toList(iter)
}

instance Iterable[List[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], l: List[a]): Iterator[a, r, r] \ r = List.iterator(rc, l)
}

instance ToJava[List[a]] {
    type Out = ##java.util.List
    type Aef = IO
    pub def toJava(l: List[a]): ##java.util.List \ IO = Adaptor.toList(l)
}

instance FromJava[List[a]] {
    type In = ##java.util.List
    pub def fromJava(l: ##java.util.List): List[a]  = Adaptor.fromList(l)
}

mod List {

    ///
    /// Renders the list `l` to a String.
    ///
    pub def toString(l: List[a]): String with ToString[a] = region rc {
        let sb = StringBuilder.empty(rc);
        foreach (x <- l)
            StringBuilder.appendString!("${x} :: ", sb);
        StringBuilder.appendString!("Nil", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the empty list `Nil`.
    ///
    pub def empty(): List[a] = Nil

    ///
    /// Returns true if and only if `l` is the empty list, i.e. `Nil`.
    ///
    pub def isEmpty(l: List[a]): Bool = match l {
        case Nil => true
        case _   => false
    }

    ///
    /// Returns true if and only if `l` is a non-empty list.
    ///
    pub def nonEmpty(l: List[a]): Bool = not isEmpty(l)

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def head(l: List[a]): Option[a] = match l {
        case Nil    => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def last(l: List[a]): Option[a] = match l {
        case Nil      => None
        case x :: Nil => Some(x)
        case _ :: rs  => last(rs)
    }

    ///
    /// Returns the length of `l`.
    ///
    pub def length(l: List[a]): Int32 =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case _ :: xs => loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// The infix operator `:::` is an alias for `append` (`l1 ::: l2 = append(l1, l2)`).
    ///
    pub def append(l1: List[a], l2: List[a]): List[a] =
        foldRight((x, acc) -> x :: acc, l2, l1)

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    pub def memberOf(a: a, l: List[a]): Bool with Eq[a] = match l {
        case Nil     => false
        case x :: xs => if (a == x) true else memberOf(a, xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Optionally returns the position of `x` in `l`.
    ///
    pub def indexOf(a: a, l: List[a]): Option[Int32] with Eq[a] =
        def loop(ll, acc) = match ll {
            case Nil     => None
            case x :: xs => if (a == x) Some(acc) else loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => if (f(x)) Some(x) else findLeft(f, xs)
    }

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k()
            case x :: xs => loop(xs, () -> if (f(x)) Some(x) else k())
        };
        loop(l, constant(None))

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): List[Int32] =
        //decAndCheck();
        def loop(i, acc) =
            if (i < b)
                acc
            else
                //loop(Sub.sub(i, 1), i :: acc);
                //loop({decAndCheck();Sub.sub(i, 1)}, i :: acc);
                loop(i - 1, i :: acc);
        loop(e - 1, Nil)

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(n: Int32, a: a): List[a] =
        def loop(i, acc) =
            if (i >= n)
                acc
            else
                loop(i + 1, a :: acc);
        loop(0, Nil)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef =
        def loop(ll, k, acc) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(acc, x);
                loop(xs, ks -> k(y :: ks), y)
        };
        loop(l, ks -> s :: ks, s)

    ///
    /// Accumulates the result of applying `f` to `l` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b \ ef, s: b, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s :: Nil)
            case x :: xs => loop(xs, ks -> match ks {
                case ss :: _ => k(f(x, ss) :: ks)
                case _       => unreachable!()})
        };
        loop(l, eidentity)

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => {
                let y = f(x);
                loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : List[a] = a :: Nil

    ///
    /// Apply every function from `f` to every argument from `x` and return a list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef =
        map(g -> map(g, x), f) |> flatten

    ///
    /// Lift a binary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...` and `l2 = y1, y2, ...` the results appear in the order
    /// `f(x1,y1), f(x1,y2), ..., f(x2,y1), f(x2,y2), ...`.
    ///
    pub def map2(f: t1 -> t2 -> r \ ef, l1: List[t1], l2: List[t2]): List[r] \ ef = Applicative.map2(f, l1, l2)

    ///
    /// Lift a ternary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...`, `l2 = y1, y2, ...` and `l3 = z1, z2, ...` the results appear
    /// in the following order:
    ///
    /// ```
    /// f(x1,y1,z1), f(x1,y1,z2), ..., f(x1,y2,z1), f(x1,y2,z2), ...,
    /// f(x2,y1,z1), f(x2,y1,z2), ..., f(x2,y2,z1), f(x2,y2,z2), ...`
    /// ...
    /// ```
    ///
    pub def map3(f: t1 -> t2 -> t3 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3]): List[r] \ ef = Applicative.map3(f, l1, l2, l3)

    /// Lift a 4-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `map3`.
    pub def map4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4]): List[r] \ ef = Applicative.map4(f, l1, l2, l3, l4)

    /// Lift a 5-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `map3`.
    pub def map5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4], l5: List[t5]): List[r] \ ef = Applicative.map5(f, l1, l2, l3, l4, l5)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(i, x);
                loop(xs, i + 1, ys -> k(y :: ys))
        };
        loop(l, 0, identity)

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    pub def flatMap(f: a -> List[b] \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case x :: xs => loop(xs, acc ::: f(x))
        };
        loop(l, Nil)

    ///
    /// Returns the reverse of `l`.
    ///
    pub def reverse(l: List[a]): List[a] =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case x :: xs => loop(xs, x :: acc)
        };
        loop(l, Nil)

    ///
    /// Returns `l` with its elements rotated `n` positions to the left.
    ///
    /// That is, returns a new list where the first `n mod length(l)` elements in `l`
    /// are the last `n mod length(l)` elements of the new list.
    ///
    pub def rotateLeft(n: Int32, l: List[a]): List[a] =
        let len = length(l);
        if (len == 0)
            l
        else {
            let rem1 = n `Int32.remainder` len;
            let rotate = if (rem1 < 0) rem1 + len else rem1;
            drop(rotate, l) ::: take(rotate, l)
        }

    ///
    /// Returns `l` with its elements rotated `n` positions to the right.
    ///
    /// That is, returns a new list where the last `n mod length(l)` elements in `l`
    /// are the first `n mod length(l)` elements of the new list.
    ///
    pub def rotateRight(n: Int32, l: List[a]): List[a] = rotateLeft(-n, l)

    ///
    /// Returns `l` with the element at index `i` replaced by `x`.
    ///
    /// Returns `l` if `i < 0` or `i > length(l)-1`.
    ///
    pub def update(i: Int32, a: a, l: List[a]): List[a] =
        def loop(ll, j, k) = match (j, ll) {
            case (_, Nil)     => k(Nil)
            case (0, _ :: xs) => k(a :: xs)
            case (_, x :: xs) => loop(xs, j - 1, ks -> k(x :: ks))
        };
        loop(l, i, identity)

    ///
    /// Returns `l` with every occurrence of `src` replaced by `dst`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, l: List[a]): List[a] with Eq[a] =
        map(e -> if (e == src.src) dst.dst else e, l)

    ///
    /// Returns `l2` with the `n` elements starting at index `i` replaced with the elements of `l1`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `l2` then no patching is done at these indices.
    /// If `l1` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `l2`, then the element at index `j` in `l1` is used.
    ///
    pub def patch(i: Int32, n: Int32, l1: List[a], l2: List[a]): List[a] =
        def loop(ll1, ll2, c, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => {
                if (c >= i and c < i + n)
                    loop(xs, ys, c + 1, ks -> k(x :: ks))
                else
                    loop(l1, ys, c + 1, ks -> k(y :: ks))
            }
            case _ => k(ll2)
        };
        loop(drop(-i, l1), l2, 0, identity)

    ///
    /// Returns all permutations of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first permutation and `reverse(l)` is the last permutation.
    ///
    pub def permutations(l: List[a]): List[List[a]] = match l {
        case Nil => Nil :: Nil
        case _   => permutationHelper(0, l)
    }

    ///
    /// Helper function for `permutations`.
    /// Returns all permutations of `l` starting with an element at or after index `i`.
    ///
    def permutationHelper(i: Int32, l: List[a]): List[List[a]] =
        if (i == length(l))
            Nil
        else
            applyHelper(at(i, l), permutations(removeIndex(i, l))) ::: permutationHelper(i + 1, l)

    ///
    /// Helper function for `permutations`.
    ///
    def at(i: Int32, l: List[a]): a = match (i, l) {
        case (0, x :: _)  => x
        case (p, _ :: xs) => at(p - 1, xs)
        case _ => unreachable!()
    }

    ///
    /// Helper function for `permutations`.
    ///
    def removeIndex(i: Int32, l: List[a]): List[a] = match (i, l) {
        case (_, Nil) => l
        case (0, _ :: xs) => xs
        case (p, x :: xs) => x :: removeIndex(p - 1, xs)
    }

    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    pub def subsequences(l: List[a]): List[List[a]] = match l {
        case Nil     => Nil :: Nil
        case x :: xs =>
            let r = subsequences(xs);
            applyHelper(x, r) ::: r
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `l` with `x` added to the beginning of each element in `l`.
    ///
    def applyHelper(a: a, l: List[List[a]]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k((a :: x) :: ks))
        };
        loop(l, identity)

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, l: List[a]): List[a] =
        def loop(ll, k) = match ll {
            case x1 :: x2 :: xs => loop(x2 :: xs, ks -> k(x1 :: a :: ks))
            case _              => k(ll)
        };
        loop(l, identity)

    ///
    /// Returns the concatenation of the elements in `l2` with the elements of `l1` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
    pub def intercalate(l1: List[a], l2: List[List[a]]): List[a] =
        def loop(ll, k) = match ll {
            case Nil            => k(Nil)
            case y :: Nil       => k(y)
            case y1 :: y2 :: ys => loop(y2 :: ys, ks -> k(y1 ::: l1 ::: ks))
        };
        loop(l2, identity)

    ///
    /// Returns the transpose of `l`.
    ///
    /// Returns `l` if the dimensions of the elements of `l` are mismatched.
    ///
    pub def transpose(l: List[List[a]]): List[List[a]] = match l {
        case Nil    => Nil
        case x :: _ =>
            let len = length(x);
            if (not uniformHelper(l, len) or len == 0) l else transposeHelper(l, len)
    }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(l: List[List[a]], len: Int32): Bool = match l {
        case Nil     => true
        case x :: xs => if (length(x) == len) uniformHelper(xs, len) else false
    }

    ///
    /// Helper function for `transpose`.
    ///
    def transposeHelper(l: List[List[a]], len: Int32): List[List[a]] = match l {
        case Nil     => repeat(len, Nil)
        case x :: xs => applyListHelper(x, transposeHelper(xs, len))
    }

    ///
    /// Helper function for `transpose`.
    ///
    def applyListHelper(l1: List[a], l2: List[List[a]]): List[List[a]] = match (l1, l2) {
        case (Nil, Nil)         => Nil
        case (x :: xs, y :: ys) => (x :: y) :: applyListHelper(xs, ys)
        case _                  => unreachable!()
    }

    ///
    /// Returns `true` if and only if `l1` is a prefix of `l2`.
    ///
    pub def isPrefixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)           => true
        case (_, Nil)           => false
        case (x :: xs, y :: ys) => if (x == y) isPrefixOf(xs, ys) else false
    }

    ///
    /// Returns `true` if and only if `l1` is an infix of `l2`.
    ///
    pub def isInfixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)     => true
        case (_, Nil)     => false
        case (_, _ :: ys) => if (isPrefixOf(l1, l2)) true else isInfixOf(l1, ys)
    }

    ///
    /// Returns `true` if and only if `l1` is a suffix of `l2`.
    ///
    pub def isSuffixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = isPrefixOf(reverse(l1), reverse(l2))

    ///
    /// Returns the result of applying `combine` to all the elements in `l`, using `empty` as the initial value.
    ///
    pub def fold(l: List[a]): a with Monoid[a] = Foldable.fold(l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = match l {
        case Nil     => s
        case x :: xs => foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, eidentity)

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: List[a]): b \ ef = match l {
        case Nil     => z
        case x :: xs => f(x, _ -> foldRightWithCont(f, z, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(f, x, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, l: List[a]): Option[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(None)
            case x :: xs => loop(xs, ks -> k(match ks {
                case None    => Some(x)
                case Some(v) => Some(f(x, v))
            }))
        };
        loop(l, eidentity)

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, l: List[a]): Int32 \ ef =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    pub def flatten(l: List[List[a]]): List[a] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k(x ::: ks))
        };
        loop(l, identity)

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => false
        case x :: xs => if (f(x)) true else exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    pub def forAll(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => true
        case x :: xs => if (f(x)) forAll(f, xs) else false
    }

    ///
    /// Returns a list of every element in `l` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, l: List[a]): List[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => if (f(x)) loop(xs, ks -> k(x :: ks)) else loop(xs, k)
        };
        loop(l, identity)

    ///
    /// Returns the sublist of `l` without the last element.
    /// Returns `None` if the list `l` is `Nil`.
    ///
    pub def init(l: List[a]): Option[List[a]] =
        def loop(ll, k) = match ll {
            case Nil      => None
            case _ :: Nil => Some(k(Nil))
            case x :: xs  => loop(xs, ks -> k(x :: ks))
        };
        loop(l, identity)

    ///
    /// Returns the sublist of `l` from index `start` (inclusive) to index `end` (exclusive).
    ///
    /// That is, an element at index `i` in `l` is part of the returned sublist if and only if `i >= start` and `i < end`.
    /// Note: Indices that are out of bounds in `l` are not considered (i.e. slice(start, end, l) = slice(max(0, start), min(length(l), end), l)).
    ///
    pub def slice(start: {start = Int32}, end: {end = Int32}, l: List[a]): List[a] =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                if (i < start.start)
                    loop(xs, i + 1, k)
                else if (i >= end.end)
                    k(Nil)
                else
                    loop(xs, i + 1, ks -> k(x :: ks))
        };
        if (start.start < end.end) loop(l, 0, identity) else Nil

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// `l2` contains all elements of `l` that do not satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool \ ef, l: List[a]): (List[a], List[a]) \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    loop(xs, match (ks, ls) -> k((ks, x :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// `l2` is the remainder of `l`.
    ///
    /// The function `f` must be pure.
    ///
    pub def span(f: a -> Bool, l: List[a]): (List[a], List[a]) =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    k((Nil, ll))
        };
        loop(l, identity)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    pub def drop(n: Int32, l: List[a]): List[a] = match l {
        case _ if n <= 0 => l
        case Nil         => Nil
        case _ :: xs     => drop(n - 1, xs)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef = match l {
        case Nil     => Nil
        case x :: xs => if (f(x)) dropWhile(f, xs) else l
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Returns `l` if `n > length(l)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def take(n: Int32, l: List[a]): List[a] =
        def loop(ll, i, k) =
            if (i <= 0)
                k(Nil)
            else
                match ll {
                    case Nil     => k(Nil)
                    case x :: xs => loop(xs, i - 1, ks -> k(x :: ks))
                };
        loop(l, n, identity)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => if (f(x)) loop(xs, ks -> k(x :: ks)) else k(Nil)
        };
        loop(l, identity)

    ///
    /// Split the list `xs` at the position `n` returning the left and right parts.
    /// Position `n` is included in the right part.
    ///
    /// Example: `splitAt(2, 1::2::3::4::Nil)` returns `(1::2::Nil, 3::4::Nil)`
    ///
    /// Returns `(xs, Nil)` if `n > length(xs)`.
    /// Returns `(Nil, xs)` if `n < 0`.
    ///
    pub def splitAt(n: Int32, xs: List[a]): (List[a], List[a]) =
        (List.take(n, xs), List.drop(n, xs))

    ///
    /// Partitions `l` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    ///
    /// A sublist is created by iterating through the remaining elements of `l` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    /// The function `f` must be pure.
    ///
    pub def groupBy(f: (a, a) -> Bool, l: List[a]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let (r1, r2) = extractHelper(f, xs, x :: Nil);
                loop(r2, ks -> k(r1 :: ks))
        };
        loop(l, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    /// The list `l2` is needed by `agreeHelper` so it must be materialized (rather than a continuation).
    /// We have no choice but to reverse it.
    ///
    def extractHelper(f: (a, a) -> Bool, l1: List[a], l2: List[a]): (List[a], List[a]) =
        def loop(ll, ll2, k) = match ll {
            case Nil     => (reverse(ll2), k(Nil))
            case x :: xs => if (agreeHelper(f, x, ll2)) loop(xs, x :: ll2, k) else loop(xs, ll2, ks -> k(x :: ks))
        };
        loop(l1, l2, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, l: List[a]): Bool = match l {
        case Nil     => true
        case x :: xs => if (f(x, a) and f(a, x)) agreeHelper(f, a, xs) else false
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zip(l1: List[a], l2: List[b]): List[(a, b)] =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => loop(xs, ys, ks -> k((x, y) :: ks))
            case _                  => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, l1: List[a], l2: List[b]): List[c] \ ef =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) =>
                let z = f(x, y);
                loop(xs, ys, ks -> k(z :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where each element `e` is mapped to `(i, e)` where `i`
    /// is the index of `e`.
    ///
    pub def zipWithIndex(l: List[a]): List[(Int32, a)] =
        def loop(ll, i, k) = match ll {
            case Nil       => k(Nil)
            case (x :: xs) => loop(xs, i + 1, ks -> k((i, x) :: ks))
        };
        loop(l, 0, identity)

    ///
    /// Generalize `zipWith` to an applicative functor `f`.
    ///
    pub def zipWithA(f: (a, b) -> f[c] \ ef, xs: List[a], ys: List[b]): f[List[c]] \ ef with Applicative[f] =
        use Functor.{<$>};
        use Applicative.{<*>};
        def loop(l1, l2, k) = match (l1, l2) {
            case (x :: rs1, y :: rs2) => loop(rs1, rs2, ks -> k(((z, zs) -> z :: zs) <$> f(x, y) <*> ks))
            case (_, _)               => k(Applicative.point(Nil))
        };
        loop(xs, ys, eidentity)



    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
        def loop(ll, k) = match ll {
            case Nil            => k((Nil, Nil))
            case (x1, x2) :: xs => loop(xs, match (ks, ls) -> k((x1 :: ks, x2 :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a list where the element at index `i` is `(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zip3(l1: List[a], l2: List[b], l3: List[c]): List[(a, b, c)] =
        zipWith3((x, y, z) -> (x, y, z), l1, l2, l3)

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d \ ef, l1: List[a], l2: List[b], l3: List[c]): List[d] \ ef =
        def loop(ll1, ll2, ll3, k) = match (ll1, ll2, ll3) {
            case (x :: xs, y :: ys, z :: zs) =>
                let r = f(x, y, z);
                loop(xs, ys, zs, ks -> k(r :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, l3, identity)

    ///
    /// Returns a triple of lists, the first containing all first components in `l`
    /// the second containing all second components in `l` and the third containing all
    /// third components in `l`.
    ///
    pub def unzip3(l: List[(a, b, c)]): (List[a], List[b], List[c]) =
        def loop(ll, k) = match ll {
            case Nil             => k((Nil, Nil, Nil))
            case (x, y, z) :: xs => loop(xs, match (ks, ls, ms) -> k((x :: ks, y :: ls, z :: ms)))
        };
        loop(l, identity)

    ///
    /// Alias for `foldLeft2`.
    ///
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = foldLeft2(f, c, l1, l2)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from left to right.
    ///
    pub def foldLeft2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = match (l1, l2) {
        case (x :: xs, y :: ys) => foldLeft2(f, f(c, x, y), xs, ys)
        case _                  => c
    }

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from right to left.
    ///
    pub def foldRight2(f: (a, b, c) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => loop(xs, ys, ks -> k(f(x, y, ks)))
            case _                  => k(c)
        };
        let len1 = length(l1);
        let len2 = length(l2);
        loop(drop(len1 - len2, l1), drop(len2 - len1, l2), eidentity)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: List[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case None    => loop(xs, k)
                case Some(v) => loop(xs, ks -> k(v :: ks))
            }
        };
        loop(l, identity)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element of `l` is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, l: List[a]): Option[b] \ ef = match l {
        case Nil     => None
        case x :: xs => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
    }

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` with `sep` inserted between each element.
    ///
    pub def join(sep: String, l: List[a]): String with ToString[a] =
        Foldable.join(sep, l)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `l` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, l: List[a]): String \ ef =
        Foldable.joinWith(f, sep, l)

    ///
    /// Returns the elements of `l` as a `DelayList`.
    ///
    @Lazy
    pub def toDelayList(l: List[a]): DelayList[a] = match l {
        case Nil     => DelayList.ENil
        case x :: xs => DelayList.LCons(x, lazy toDelayList(xs))
    }

    ///
    /// Returns the list `l` as a chain.
    ///
    pub def toChain(l: List[a]): Chain[a] =
        List.foldLeft(Chain.snoc, Chain.empty(), l)

    ///
    /// Returns `l` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], l: List[a]): MutDeque[a, r] \ r =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), l);
        d

    ///
    /// Returns `l` as a mutable list.
    ///
    pub def toMutList(rc: Region[r], l: List[a]): MutList[a, r] \ r = region rc2 {
        Array.toMutList(rc, toArray(rc2, l)) // `Array.toMutList` respects the invariant of `MutList`
    }

    ///
    /// Returns the list `l` as a set.
    ///
    pub def toSet(l: List[a]): Set[a] with Order[a] =
        foldRight((x, acc) -> Set.insert(x, acc), Set.empty(), l)

    ///
    /// Returns the association list `l` as a map.
    ///
    /// If `l` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(l: List[(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> Map.insert(fst(x), snd(x), acc), Map.empty(), l)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    /// If `s` contains multiple mappings with the same key, `toMapWith` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMapWith(f: a -> b, l: List[a]): Map[a, b] with Order[a] =
        List.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    pub def forEach(f: a -> Unit \ ef, l: List[a]): Unit \ ef = match l {
        case Nil     => ()
        case x :: xs => f(x); forEach(f, xs)
    }

    ///
    /// Applies `f` to every element of `l` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, l: List[a]): Unit \ ef =
        def loop(ll, i) = match ll {
            case Nil     => ()
            case x :: xs => f(i, x); loop(xs, i+1)
        };
        loop(l, 0)

    ///
    /// Returns the list `l` as an array.
    ///
    pub def toArray(rc: Region[r], l: List[a]): Array[a, r] \ r = match head(l) {
        case None    => Array#{} @ rc
        case Some(_) =>
            let a = Array.empty(rc, length(l));
            forEach(match (i, b) -> Array.put(b, i, a), zipWithIndex(l));
            a
        }

    ///
    /// Returns the list `l` as a vector.
    ///
    pub def toVector(l: List[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, length(l));
        forEachWithIndex((i, x) -> Array.put(x, i, arr), l);
        Array.toVector(arr)
    }

    ///
    /// Returns the list `l` as `Option[Nel[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(l: List[a]): Option[Nel[a]] = match l {
        case Nil     => None
        case x :: xs => Some(Nel.Nel(x, xs))
    }

    ///
    /// Returns the list `l` as `Option[Nec[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(l: List[a]): Option[Nec[a]] = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(Nec.snoc, Nec.singleton(x), xs))
    }

    ///
    /// Sort list `l` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(l: List[a]): List[a] with Order[a] = region rc {
        toArray(rc, l) !> Array.sort! |> Array.toList
    }

    /// Sort list `l` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, l: List[a]): List[a] with Order[b] = region rc {
        toArray(rc, l) !> Array.sortBy!(f) |> Array.toList
    }

    ///
    /// Sort list `l` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `l`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, l: List[a]): List[a] = region rc {
        toArray(rc, l) !> Array.sortWith!(cmp) |> Array.toList
    }

    ///
    /// Build a list by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new list element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: s -> Option[(a, s)] \ ef, st: s): List[a] \ ef =
        def loop(sst, k) = match f(sst) {
            case None         => k(Nil)
            case Some(a, st1) => loop(st1, ks -> k(a :: ks))
        };
        loop(st, identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new list element `a`.
    ///
    /// `next` should return `None` to signal the end of building the list.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] \ ef): List[a] \ ef =
        def loop(k) = match next() {
            case None    => k(Nil)
            case Some(x) => loop(ks -> k(x :: ks))
        };
        loop(identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Ok(Some(a)` to signal a new list element `Ok(a)`.
    ///
    /// `next` should return `Ok(None)` to signal the end of building the list.
    ///
    /// `next` should return `Err(e)` to signal that an error occurred. The function returns `Err(e)`.
    ///
    pub def unfoldWithOkIter(next: Unit -> Result[e, Option[a]] \ ef): Result[e, List[a]] \ ef =
        def loop(k) = match next() {
            case Ok(None)    => k(Ok(Nil))
            case Err(e)      => k(Err(e))
            case Ok(Some(x)) => loop(Result.flatMap(ks -> k(Ok(x :: ks))))
        };
        loop(identity)

    ///
    /// Returns the list `l` with duplicates removed. The first occurence of
    /// an element is kept and except for the removal of subsequent duplicates
    /// the order of `l` is preserved.
    ///
    /// `distinct` uses the Flix's builtin equality test. Use `distinctWith` if you
    /// need a custom equality test.
    ///
    pub def distinct(l: List[a]): List[a] with Eq[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (memberOf(x, ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Returns the list `l` with duplicates removed using the supplied function
    /// `f` for comparison. The first occurrence of an element is kept and except
    /// for the removal of subsequent duplicates the order of `l` is preserved.
    ///
    pub def distinctWith(f: (a, a) -> Bool, l: List[a]): List[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (exists(f(x), ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Returns the sum of all elements in the list `l`.
    ///
    pub def sum(l: List[Int32]): Int32 =
        Foldable.sum(l)

    ///
    /// Returns the sum of all elements in the list `l` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, l: List[a]): Int32 \ ef =
        Foldable.sumWith(f, l)

    ///
    /// Returns an iterator over `l`.
    ///
    pub def iterator(rc: Region[r], xs: List[a]): Iterator[a, r, r] \ r =
        let ls = ref xs @ rc;
        let next = () -> {
            match (deref ls) {
                case Nil     => None
                case x :: rs => Ref.put(rs, ls); Some(x)
            }
        };
        Iterator.iterate(rc, next)

    ///
    /// Returns an iterator over `l` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], l: List[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, l) |> Iterator.zipWithIndex

    ///
    /// Returns the association list `l` as a `DelayMap`.
    ///
    /// If `l` contains multiple mappings with the same key, `toDelayMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toDelayMap(l: List[(a, b)]): DelayMap[a, b] with Order[a] =
        List.foldRight((x, acc) -> DelayMap.insert(fst(x), snd(x), acc), DelayMap.empty(), l)

    ///
    /// Helper function for `sequence` and `traverse`.
    ///
    /// Builds an "applicative list" from a head of one applicative action and an
    /// applicative list of the tail.
    ///
    def consA(mx: f[a], ml: f[List[a]]): f[List[a]] with Applicative[f] =
        use Functor.{<$>};
        use Applicative.{<*>};
        (((x, xs) -> x :: xs) <$> mx) <*> ml

    ///
    /// Returns the result of running all the actions in the list `l` going from left
    /// to right.
    ///
    pub def sequence(l: List[m[a]]): m[List[a]] with Applicative[m] =
        def loop(ll, k) = match ll {
            case Nil      => k(Applicative.point(Nil))
            case mx :: rs => loop(rs, ks -> k(consA(mx, ks)))
        };
        loop(l, identity)

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// list `l` going from left to right.
    ///
    pub def traverse(f: a -> m[b] \ ef, l: List[a]): m[List[b]] \ ef with Applicative[m] =
        def loop(ll, k) = match ll {
            case Nil     => k(Applicative.point(Nil))
            case x :: xs => {let ans = f(x); loop(xs, ks -> k(consA(ans, ks)))}
        };
        loop(l, identity)

    ///
    /// Merges the two lists `l1` and `l2`. Assuming they are both sorted.
    /// If two elements compare `EqualTo`, then the element of `l1` is first in the result.
    ///
    pub def merge(l1: List[a], l2: List[a]): List[a] with Order[a] =
        def loop(acc1, acc2, k) = match (acc1, acc2) {
            case (x :: xs, y :: ys) => {
                let cmp = x <=> y;
                if (cmp == Comparison.LessThan or cmp == Comparison.EqualTo) loop(xs, acc2, ks -> k(x :: ks))
                else                                   loop(acc1, ys, ks -> k(y :: ks))
            }
            case (Nil    , y :: ys) => k(y :: ys)
            case (x :: xs, Nil    ) => k(x :: xs)
            case (Nil    , Nil    ) => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Shuffles `l` using the Fisher–Yates shuffle.
    ///
    pub def shuffle(rnd: Random, l: List[a]): List[a] \ IO = region rc {
        toArray(rc, l) !> Array.shuffle(rnd) |> Array.toList
    }

    ///
    /// Returns the frequency for each element in list `l`
    ///
    pub def frequency(l: List[t]): Map[t, Int32] with Order[t] =
        def freq(ll, m) = match ll {
            case Nil   => m
            case x::xs    =>
                match Map.get(x,m) {
                    case None       => freq(xs, Map.insert(x, 1, m))
                    case Some(_)    => freq(xs, Map.update(v -> Some(v+1), x, m))
                }
        };
        freq(l, Map#{})

}
