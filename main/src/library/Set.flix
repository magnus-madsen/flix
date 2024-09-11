/*
 * Copyright 2017 Liam Palmer, Esben Bjerre
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

import java.util.{Set => JSet}

///
/// The Set type.
///
/// A set is currently represented internally as a red-black tree.
///
pub enum Set[t] with Sendable {
    case Set(RedBlackTree[t, Unit])
}

instance Eq[Set[a]] with Eq[a] {
    pub def eq(s1: Set[a], s2: Set[a]): Bool =
        Set.toList(s1) == Set.toList(s2)
}

instance Order[Set[a]] with Order[a] {
    pub def compare(s1: Set[a], s2: Set[a]): Comparison =
        Set.toList(s1) <=> Set.toList(s2)

}

instance Hash[Set[a]] with Hash[a] {
    pub def hash(s: Set[a]): Int32 =
        Set.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), s)
}

instance ToString[Set[a]] with ToString[a] {
    pub def toString(s: Set[a]): String = Set.toString(s)
}

instance LowerBound[Set[a]] {
    pub def minValue(): Set[a] = Set.empty()
}

instance PartialOrder[Set[a]] with Order[a] {
    pub def lessEqual(s1: Set[a], s2: Set[a]): Bool = s1 `Set.isSubsetOf` s2
}

instance JoinLattice[Set[a]] with Order[a] {
    pub def leastUpperBound(s1: Set[a], s2: Set[a]): Set[a] = s1 `Set.union` s2
}

instance MeetLattice[Set[a]] with Order[a] {
    pub def greatestLowerBound(s1: Set[a], s2: Set[a]): Set[a] = s1 `Set.intersection` s2
}

instance Foldable[Set] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, s1: Set[a]): b \ ef = Set.foldLeft(f, s, s1)
    pub def foldRight(f: (a, b) -> b \ ef, s: b, s1: Set[a]): b \ ef = Set.foldRight(f, s, s1)
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, s1: Set[a]): b \ ef = Set.foldRightWithCont(f, s, s1)
}

instance UnorderedFoldable[Set] {
    pub def foldMap(f: a -> b \ ef, s: Set[a]): b \ ef with CommutativeMonoid[b] = Set.foldMap(f, s)
    redef isEmpty(s: Set[a]): Bool = Set.isEmpty(s)
    redef exists(f: a -> Bool \ ef, s: Set[a]): Bool \ ef = Set.exists(f, s)
    redef forAll(f: a -> Bool \ ef, s: Set[a]): Bool \ ef = Set.forAll(f, s)

    @ParallelWhenPure
    redef count(f: a -> Bool \ ef, s: Set[a]): Int32 \ ef = Set.count(f, s)
}

instance SemiGroup[Set[a]] with Order[a] {
    pub def combine(x: Set[a], y: Set[a]): Set[a] = Set.union(x,y)
}

instance CommutativeSemiGroup[Set[a]] with Order[a]

instance Monoid[Set[a]] with Order[a] {
    pub def empty(): Set[a] = Set.empty()
}

instance CommutativeMonoid[Set[a]] with Order[a]

instance Collectable[Set[a]] with Order[a] {
    type Elm = a
    pub def collect(iter: Iterator[a, ef, r]): Set[a] \ { ef, r } = Iterator.toSet(iter)
}

instance Iterable[Set[a]] {
    type Elm = a
    pub def iterator(rc: Region[r], s: Set[a]): Iterator[a, r, r] \ r = Set.iterator(rc, s)
}

instance ToJava[Set[a]] with Order[a] {
    type Out = JSet
    type Aef = IO
    pub def toJava(s: Set[a]): JSet \ IO = Adaptor.toSet(s)
}

instance FromJava[Set[a]] with Order[a] {
    type In = JSet
    pub def fromJava(s: JSet): Set[a] = Adaptor.fromSet(s)
}

mod Set {

    ///
    /// Returns a string representation of the given set `s`.
    ///
    pub def toString(s: Set[a]): String with ToString[a] = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString!("Set#{", sb);
        forEachWithIndex((i, x) -> {
            if (i == 0)
                StringBuilder.appendString!("${x}", sb)
            else
                StringBuilder.appendString!(", ${x}", sb)
        }, s);
        StringBuilder.appendString!("}", sb);
        StringBuilder.toString(sb)
    }

    ///
    /// Returns the number of threads to use for parallel evaluation.
    ///
    def threads(): Int32 =
        // Note: We use a multiple of the number of physical cores for better performance.
        let multiplier = 4;
        multiplier * Environment.getVirtualProcessors()

    ///
    /// Determines whether to use parallel evaluation.
    ///
    /// By default we only enable parallel evaluation if the set has a certain size.
    ///
    def useParallelEvaluation(s: Set[a]): Bool =
        let Set(t) = s;
        let minSize = Int32.pow(2, RedBlackTree.blackHeight(t));
        minSize >= 1024

    ///
    /// Returns the number of elements in `s`.
    ///
    pub def size(s: Set[a]): Int32 =
        let Set(t) = s;
        RedBlackTree.size(t)

    ///
    /// Returns the empty set.
    ///
    /// `Set#{}` is syntactic sugar for `empty` i.e. `Set#{} == empty()`.
    ///
    pub def empty(): Set[a] = Set(RedBlackTree.empty())

    ///
    /// Adds `x` to `s`.
    ///
    pub def insert(x: a, s: Set[a]): Set[a] with Order[a] =
        let Set(t) = s;
        Set(RedBlackTree.insert(x, (), t))

    ///
    /// Removes `x` from `s`.
    ///
    pub def remove(x: a, s: Set[a]): Set[a] with Order[a] =
        let Set(t) = s;
        Set(RedBlackTree.remove(x, t))

    ///
    /// Returns true if and only if `s` is the empty set.
    ///
    pub def isEmpty(s: Set[a]): Bool =
        let Set(t) = s;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns true if and only if `s` is a non-empty set.
    ///
    pub def nonEmpty(s: Set[a]): Bool = not isEmpty(s)

    ///
    /// Returns the singleton set containing `x`.
    ///
    /// `Set#{x}` is syntactic sugar for `singleton` i.e. `Set#{x} == singleton(x)`.
    ///
    pub def singleton(x: a): Set[a] with Order[a] = insert(x, empty())

    ///
    /// Returns a set of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `empty()` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Set[Int32] =
        def loop(i, s) = {
            if (i >= e)
                s
            else
                loop(i + 1, insert(i, s))
        };
        loop(b, empty())

    ///
    /// Returns true if and only if `x` is a member of `s`.
    ///
    pub def memberOf(x: a, s: Set[a]): Bool with Order[a] =
        let Set(t) = s;
        RedBlackTree.memberOf(x, t)

    ///
    /// Optionally finds the smallest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def minimum(s: Set[a]): Option[a] =
        let Set(t) = s;
        RedBlackTree.minimumKey(t) |> Option.map(fst)

    ///
    /// Optionally finds the smallest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def minimumBy(cmp: (a, a) -> Comparison \ ef, s: Set[a]): Option[a] \ ef =
        def min() = reduceLeft((x, y) -> if (cmp(x, y) == Comparison.LessThan) x else y, s);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(s))
                    let h = (kl, _, kr, _) -> g(kl, kr);
                    let Set(t) = s;
                    let o = RedBlackTree.parMinimumBy(threads() - 1, h, t);
                    Option.map(fst, o)
                else
                    min()
            case Purity2.Impure(_) => min()
        }

    ///
    /// Optionally finds the largest element of `s` according to the `Order` on `a`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    pub def maximum(s: Set[a]): Option[a] =
        let Set(t) = s;
        RedBlackTree.maximumKey(t) |> Option.map(fst)

    ///
    /// Optionally finds the largest element of `s` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `s` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def maximumBy(cmp: (a, a) -> Comparison \ ef, s: Set[a]): Option[a] \ ef =
        def max() = reduceLeft((x, y) -> if (cmp(x, y) == Comparison.GreaterThan) x else y, s);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(s))
                    let h = (kl, _, kr, _) -> g(kl, kr);
                    let Set(t) = s;
                    let o = RedBlackTree.parMaximumBy(threads() - 1, h, t);
                    Option.map(fst, o)
                else
                    max()
            case Purity2.Impure(_) => max()
        }

    ///
    /// Returns true if and only if every element in `s1` appears in `s2`.
    ///
    pub def isSubsetOf(s1: Set[a], s2: Set[a]): Bool with Order[a] = forAll(x -> memberOf(x, s2), s1)

    ///
    /// Returns true if and only if every element in `s1` appears in `s2` and `s != s2`.
    ///
    pub def isProperSubsetOf(s1: Set[a], s2: Set[a]): Bool with Order[a] = size(s1) != size(s2) and isSubsetOf(s1, s2)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, s: Set[a]): Option[a] \ ef = findLeft(f, s)

    ///
    /// Optionally returns the first element of `s` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, s: Set[a]): Option[a] \ ef =
        let Set(t) = s;
        RedBlackTree.findLeft((x, _) -> f(x), t) |> Option.map(fst)

    ///
    /// Optionally returns the first element of `s` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, s: Set[a]): Option[a] \ ef =
        let Set(t) = s;
        RedBlackTree.findRight((x, _) -> f(x), t) |> Option.map(fst)

    ///
    /// Returns the result of applying `combine` to all the elements in `s`, using `empty` as the initial value.
    ///
    pub def fold(s: Set[a]): a with Monoid[a] = Foldable.fold(s)

    ///
    /// Applies `f` to a start value `s` and all elements in `s` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, s1: Set[a]): b \ ef =
        let Set(t) = s1;
        RedBlackTree.foldLeft((acc, k, _) -> f(acc, k), s, t)

    ///
    /// Applies `f` to a start value `s` and all elements in `s1` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, s1: Set[a]): b \ ef =
        let Set(t) = s1;
        RedBlackTree.foldRight((k, _, c) -> f(k, c), s, t)

    ///
    /// Applies `f` to a start value `z` and all elements in `s` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, s: Set[a]): b \ ef =
        let Set(t) = s;
        RedBlackTree.foldRightWithCont((k, _, c) -> f(k, c), z, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, s: Set[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), s)

    ///
    /// Applies `f` to all elements in `s` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, s: Set[a]): Option[a] \ ef =
        let Set(t) = s;
        RedBlackTree.reduceLeft((x, _, y, _) -> (f(x, y), ()), t) |> Option.map(fst)

    ///
    /// Applies `f` to all elements in `s` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    /// Returns `None` if `s` is the empty set.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, s: Set[a]): Option[a] \ ef =
        let Set(t) = s;
        RedBlackTree.reduceRight((x, _, y, _) -> (f(x, y), ()), t) |> Option.map(fst)

    ///
    /// Returns the number of elements in `s` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def count(f: a -> Bool \ ef, s: Set[a]): Int32 \ ef =
        match purityOf(f) {
            case Purity.Pure(g) =>
                if (useParallelEvaluation(s))
                    let h = (k, _) -> g(k);
                    let Set(t) = s;
                    RedBlackTree.parCount(threads() - 1, h, t)
                else
                    foldLeft((b, k) -> if (f(k)) b + 1 else b, 0, s)
            case Purity.Impure(g) => foldLeft((b, k) -> if (g(k)) b + 1 else b, 0, s)
        }

    ///
    /// Returns the sum of all elements in the set `s`.
    ///
    pub def sum(s: Set[Int32]): Int32 =
        Foldable.sum(s)

    ///
    /// Returns the sum of all elements in the set `s` according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure
    pub def sumWith(f: a -> Int32 \ ef, s: Set[a]): Int32 \ ef =
        let Set(t) = s;
        def sw() = RedBlackTree.sumWith((k, _) -> f(k), t);
        match purityOf(f) {
            case Purity.Pure(g) =>
                if (useParallelEvaluation(s))
                    RedBlackTree.parSumWith(threads() - 1, (k, _) -> g(k), t)
                else
                    sw()
            case Purity.Impure(_) => sw()
        }

    ///
    /// Returns the union of the elements in `s`.
    ///
    pub def flatten(s: Set[Set[a]]): Set[a] with Order[a] =
        foldLeft((acc, x) -> union(acc, x), empty(), s)

    ///
    /// Returns `true` if and only if at least one element in `s` satisfies the predicate `f`.
    ///
    /// Returns `false` if `s` is the empty set.
    ///
    pub def exists(f: a -> Bool \ ef, s: Set[a]): Bool \ ef =
        let Set(t) = s;
        RedBlackTree.exists((x, _) -> f(x), t)

    ///
    /// Returns `true` if and only if all elements in `s` satisfy the predicate `f`.
    ///
    /// Returns `true` if `s` is the empty set.
    ///
    pub def forAll(f: a -> Bool \ ef, s: Set[a]): Bool \ ef =
        let Set(t) = s;
        RedBlackTree.forAll((x, _) -> f(x), t)

    ///
    /// Returns the union of `s1` and `s2`.
    ///
    pub def union(s1: Set[a], s2: Set[a]): Set[a] with Order[a] =
        use RedBlackTree.{blackHeight, foldLeft, insert};
        let Set(t1) = s1;
        let Set(t2) = s2;
        if (blackHeight(t1) < blackHeight(t2))
            Set(foldLeft((acc, x, _) -> insert(x, (), acc), t2, t1))
        else
            Set(foldLeft((acc, y, _) -> insert(y, (), acc), t1, t2))

    ///
    /// Returns the intersection of `s1` and `s2`.
    ///
    pub def intersection(s1: Set[a], s2: Set[a]): Set[a] with Order[a] =
        foldLeft((acc, x) -> if (memberOf(x, s1)) insert(x, acc) else acc, empty(), s2)

    ///
    /// Returns the difference of `s1` and `s2`, i.e. `s1 - s2`.
    ///
    pub def difference(s1: Set[a], s2: Set[a]): Set[a] with Order[a] =
        foldLeft((acc, x) -> if (not memberOf(x, s2)) insert(x, acc) else acc, empty(), s1)

    ///
    /// Returns all subsets of `s`.
    ///
    pub def subsets(s: Set[a]): Set[Set[a]] with Order[a] =
        foldLeft((acc, x) -> union(map(y -> insert(x, y), acc), acc), insert(empty(), empty()), s)

    ///
    /// Returns the set of all elements of `s` that satisfy the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, s: Set[a]): Set[a] \ ef with Order[a] =
        foldLeft((acc, x) -> if (f(x)) insert(x, acc) else acc, empty(), s)

    ///
    /// Returns the result of applying `f` to every element in `s`.
    ///
    /// Note: The returned set may be smaller than `s`.
    ///
    pub def map(f: a -> b \ ef, s: Set[a]): Set[b] \ ef with Order[b] =
        foldLeft((acc, x) -> insert(f(x), acc), empty(), s)

    ///
    /// Returns the result of applying `f` to every element in `s` and taking the union.
    ///
    pub def flatMap(f: a -> Set[b] \ ef, s: Set[a]): Set[b] \ ef with Order[b] =
        foldLeft((acc, x) -> union(acc, f(x)), empty(), s)

    ///
    /// Collects the results of applying the partial function `f` to every element in `s`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, s: Set[a]): Set[b] \ ef with Order[b] =
        let step = (acc, x) -> match f(x) {
            case None    => acc
            case Some(v) => insert(v, acc)
        };
        foldLeft(step, empty(), s)

    ///
    /// Replaces the element `src` with `dst` if `src` is in `s`. Otherwise, returns `s`.
    ///
    /// Note: The returned set may be smaller than `s`.
    ///
    pub def replace(src: {src = a}, dst: {dst = a}, s: Set[a]): Set[a] with Order[a] =
        if (memberOf(src#src, s)) insert(dst#dst, remove(src#src, s)) else s

    ///
    /// Returns a pair of sets `(s1, s2)`.
    ///
    /// `s1` contains all elements of `s` that satisfy the predicate `f`.
    /// `s2` contains all elements of `s` that do not satisfy the predicate `f`.
    ///
    pub def partition(f: a -> Bool \ ef, s: Set[a]): (Set[a], Set[a]) \ ef with Order[a] =
        foldLeft((acc, x) ->
            let (a, b) = acc;
            if (f(x))
                (insert(x, a), b)
            else
                (a, insert(x, b)), (empty(), empty()),
            s)

    ///
    /// Returns the set `s` as a list.
    ///
    pub def toList(s: Set[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, s)

    ///
    /// Returns the set `s` as an array.
    ///
    pub def toArray(rc: Region[r], s: Set[a]): Array[a, r] \ r = match size(s) {
        case 0  => Array#{} @ rc
        case sz =>
            let a = Array.empty(rc, sz);
            Iterator.forEachWithIndex((i, b) -> Array.put(b, i, a), iterator(rc, s));
            a
        }

    ///
    /// Returns the set `s` as a vector.
    ///
    pub def toVector(s: Set[a]): Vector[a] = region rc {
        let arr = Array.empty(rc, size(s));
        Iterator.forEachWithIndex((i, x) -> Array.put(x, i, arr), iterator(rc, s));
        Array.toVector(arr)
    }

    ///
    /// Returns the set `s` as a chain.
    ///
    pub def toChain(s: Set[a]): Chain[a] =
        foldLeft((acc, x) -> Chain.snoc(acc, x), Chain.empty(), s)

    ///
    /// Returns the association set `s` as a map.
    ///
    /// If `s` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(s: Set[(a, b)]): Map[a, b] with Order[a] =
        foldRight((x, acc) -> let (k, v) = x; Map.insert(k, v, acc), Map#{}, s)

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: Set[a]): Map[a, b] with Order[a] =
        Set.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `s` as a MutDeque.
    ///
    pub def toMutDeque(rc: Region[r], s: Set[a]): MutDeque[a, r] \ r =
        let d = MutDeque.empty(rc);
        forEach(x -> MutDeque.pushBack(x, d), s);
        d

    ///
    /// Returns the set `s` as a `MutSet`.
    ///
    pub def toMutSet(rc: Region[r], s: Set[a]): MutSet[a, r] \ r =
        new MutSet @ rc  {rc = rc, ms = s }

    ///
    /// Applies `f` to every element of `s`.
    ///
    pub def forEach(f: a -> Unit \ ef, s: Set[a]): Unit \ ef =
        let Set(t) = s;
        RedBlackTree.forEach((x, _) -> f(x), t)

    ///
    /// Applies `f` to every element of `s` along with that element's index.
    ///
    pub def forEachWithIndex(f: (Int32, a) -> Unit \ ef, s: Set[a]): Unit \ ef =
        let Set(t) = s;
        RedBlackTree.forEachWithIndex((i, x, _) -> f(i, x), t)

    ///
    /// Build a set by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new set element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the set.
    ///
    pub def unfold(f: s -> Option[(a, s)] \ ef, st: s): Set[a] \ ef with Order[a] =
        def loop(sst, s) = match f(sst) {
            case None         => s
            case Some(a, st1) => loop(st1, insert(a, s))
        };
        loop(st, empty())

    ///
    /// Build a set by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a value pair `a`.
    ///
    /// `next` should return `None` to signal the end of building the set.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] \ ef): Set[a] \ ef with Order[a] =
        def loop(s) = match next() {
            case None    => s
            case Some(a) => loop(insert(a, s))
        };
        loop(empty())

    ///
    /// Returns the set `s` as a `DelayList`.
    ///
    pub def toDelayList(s: Set[a]): DelayList[a] =
        foldRight((x, acc) -> DelayList.ECons(x, acc), DelayList.ENil, s)

    ///
    /// Returns an iterator over `s`.
    ///
    pub def iterator(rc: Region[r], s: Set[a]): Iterator[a, r, r] \ r =
        let Set(t) = s;
        RedBlackTree.iterator(rc, t) |> Iterator.map(fst)

    ///
    /// Returns an iterator over `s` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], s: Set[a]): Iterator[(Int32, a), r, r] \ r =
        iterator(rc, s) |> Iterator.zipWithIndex

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` with `sep` inserted between each element.
    ///
    pub def join(sep: String, s: Set[a]): String with ToString[a] =
        Foldable.join(sep, s)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `s` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String \ ef, sep: String, s: Set[a]): String \ ef =
        Foldable.joinWith(f, sep, s)

}
