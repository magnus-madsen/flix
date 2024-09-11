/*
 * Copyright 2021 Jakob Schneider Villumsen
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

pub enum DelayMap[k, v] with Sendable {
    case DMap(RedBlackTree[k, Lazy[v]])
}

instance Eq[DelayMap[k, v]] with Eq[k], Eq[v] {
    pub def eq(m1: DelayMap[k, v], m2: DelayMap[k, v]): Bool =
        DelayMap.toList(m1) == DelayMap.toList(m2)
}

instance Order[DelayMap[k, v]] with Order[k], Order[v] {
    pub def compare(x: DelayMap[k, v], y: DelayMap[k, v]): Comparison =
        DelayMap.toList(x) <=> DelayMap.toList(y)
}

instance ToString[DelayMap[k, v]] with ToString[k], ToString[v] {
    pub def toString(m: DelayMap[k, v]): String = DelayMap.toString(m)
}

instance Indexable[DelayMap[k, v]] with Order[k] {
    type Idx = k
    type Elm = v
    type Aef = KeyNotFound
    pub def get(t: DelayMap[k, v], i: k): v \ KeyNotFound = match DelayMap.get(i, t) {
        case Some(v) => v
        case None    => do KeyNotFound.keyNotFound("key not found")
    }
}

instance Functor[DelayMap[k]] {
    pub def map(f: v1 -> v2 \ ef, m: DelayMap[k, v1]): DelayMap[k, v2] \ ef = DelayMap.map(f, m)
}

instance Foldable[DelayMap[k]] {
    pub def foldLeft(f: (b, v) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef = DelayMap.foldLeft(f, s, m)
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef = DelayMap.foldRight(f, s, m)
    pub def foldRightWithCont(f: (v, Unit -> b \ ef) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef = DelayMap.foldRightWithCont(f, s, m)

}

instance Iterable[DelayMap[k, v]] {
    type Elm = (k, v)
    pub def iterator(rc: Region[r], m: DelayMap[k, v]): Iterator[(k, v), r, r] \ r =
        DelayMap.iterator(rc, m)
}

mod DelayMap {

    ///
    /// Returns a string representation of the given `DelayMap` `m`.
    ///
    @Experimental
    pub def toString(m: DelayMap[k, v]): String with ToString[k], ToString[v] = region rc {
        let sb = StringBuilder.empty(rc);
        let _ = parallelForce(m);
        StringBuilder.appendString!("DelayMap#{", sb);
        forEachWithIndex((i, k, v) -> {
            if (i < 1)
                StringBuilder.appendString!("${k} => ${v}", sb)
            else
                StringBuilder.appendString!(", ${k} => ${v}", sb)
            }, m);
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
    /// By default we only enable parallel evaluation if the map has a certain size.
    ///
    def useParallelEvaluation(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        let minSize = Int32.pow(2, RedBlackTree.blackHeight(t));
        minSize >= 1024

    ///
    /// Returns the empty map.
    ///
    @Experimental
    pub def empty(): DelayMap[k, v] =
        DMap(RedBlackTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    @Experimental
    pub def singleton(k: k, v: v): DelayMap[k, v] with Order[k] =
        insert(k, v, empty())

    ///
    /// Returns the number of keys in `m`.
    ///
    @Experimental
    pub def size(m: DelayMap[k, v]): Int32 =
        let DMap(t) = m;
        RedBlackTree.size(t)

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    @Experimental
    pub def isEmpty(m: DelayMap[k, v]): Bool =
        let DMap(t) = m;
        RedBlackTree.isEmpty(t)

    ///
    /// Returns `true` if and only if `m` is a non-empty map.
    ///
    @Experimental
    pub def nonEmpty(m: DelayMap[k, v]): Bool = not isEmpty(m)

    ///
    /// Returns `m` with `k => v`.
    ///
    @Experimental
    pub def insert(k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.insert(k, lazy v, t))

    ///
    /// Returns `Some(v)` if `k => v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    @Experimental
    pub def get(k: k, m: DelayMap[k, v]): Option[v] with Order[k] =
        let DMap(t) = m;
        match RedBlackTree.get(k, t) {
            case None    => None
            case Some(x) => Some(force x)
        }

    ///
    /// Returns `v` if `k => v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    @Experimental
    pub def getWithDefault(k: k, d: v, m: DelayMap[k, v]): v with Order[k] =
        Option.getWithDefault(d, get(k, m))

    ///
    /// Returns the number of mappings `k => v` in `m` that satisfy the predicate `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Experimental @ParallelWhenPure
    pub def count(f: (k, v) -> Bool \ ef, m: DelayMap[k, v]): Int32 \ ef =
        def c() = foldLeftWithKey((b, k, v) -> if (f(k, v)) b + 1 else b, 0, m);
        match purityOf2(f) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g(k, force v);
                    let DMap(t) = m;
                    RedBlackTree.parCount(threads() - 1, h, t)
                else
                    c()
            case Purity2.Impure(_) => c()
        }

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    @Experimental
    pub def memberOf(k: k, m: DelayMap[k, v]): Bool with Order[k] =
        let DMap(t) = m;
        RedBlackTree.memberOf(k, t)

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental
    pub def minimumKey(m: DelayMap[k, v]): Option[(k, v)] =
        let DMap(t) = m;
        match RedBlackTree.minimumKey(t) {
            case None       => None
            case Some(k, v) => Some(k, force v)
        }

    ///
    /// Optionally finds `k => v` where `k` is the smallest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def minimumKeyBy(cmp: (k, k) -> Comparison \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == Comparison.LessThan) (kl, vl) else (kr, vr), m);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g(kl, kr);
                    let DMap(t) = m;
                    let res = RedBlackTree.parMinimumBy(threads() - 1, h, t);
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    min()
            case Purity2.Impure(_) => min()
        }
    ///
    /// Optionally finds `k => v` where `v` is the smallest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental @Parallel
    pub def minimumValue(m: DelayMap[k, v]): Option[(k, v)] with Order[v] =
        minimumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `k` is the smallest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def minimumValueBy(cmp: (v, v) -> Comparison \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        def min() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == Comparison.LessThan) (kl, vl) else (kr, vr), m);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g(force vl, force vr);
                    let DMap(t) = m;
                    let res = RedBlackTree.parMinimumBy(threads() - 1, h, t);
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    min()
            case Purity2.Impure(_) => min()
        }

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the `Order` instance of `k`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental
    pub def maximumKey(m: DelayMap[k, v]): Option[(k, v)] =
        let DMap(t) = m;
        match RedBlackTree.maximumKey(t) {
            case None       => None
            case Some(k, v) => Some(k, force v)
        }

    ///
    /// Optionally finds `k => v` where `k` is the largest key according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def maximumKeyBy(cmp: (k, k) -> Comparison \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(kl, kr) == Comparison.GreaterThan) (kl, vl) else (kr, vr), m);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (kl, _, kr, _) -> g(kl, kr);
                    let DMap(t) = m;
                    let res = RedBlackTree.parMaximumBy(threads() - 1, h, t);
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    max()
            case Purity2.Impure(_) => max()
        }

    ///
    /// Optionally finds `k => v` where `v` is the largest value.
    ///
    /// Returns `None` if `m` is empty.
    ///
    @Experimental @Parallel
    pub def maximumValue(m: DelayMap[k, v]): Option[(k, v)] with Order[v] =
        maximumValueBy((x, y) -> x <=> y, m)

    ///
    /// Optionally finds `k => v` where `k` is the largest value according to the given comparator `cmp`.
    ///
    /// Returns `None` if `m` is empty.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `cmp`.
    ///
    @Experimental @ParallelWhenPure
    pub def maximumValueBy(cmp: (v, v) -> Comparison \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        def max() = reduceLeftWithKey((kl, vl, kr, vr) -> if (cmp(vl, vr) == Comparison.GreaterThan) (kl, vl) else (kr, vr), m);
        match purityOf2(cmp) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (_, vl, _, vr) -> g(force vl, force vr);
                    let DMap(t) = m;
                    let res = RedBlackTree.parMaximumBy(threads() - 1, h, t);
                    match res {
                        case None       => None
                        case Some(k, v) => Some(k, force v)
                    }
                else
                    max()
            case Purity2.Impure(_) => max()
        }

    ///
    /// Returns the keys of `m`.
    ///
    @Experimental
    pub def keysOf(m: DelayMap[k, v]): Set[k] with Order[k] =
        foldLeftWithKey((acc, k, _) -> Set.insert(k, acc), Set.empty(), m)

    ///
    /// Returns the values of `m`.
    ///
    @Experimental
    pub def valuesOf(m: DelayMap[k, v]): List[v] =
        foldRight((v, acc) -> v :: acc, Nil, m)

    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    @Experimental
    pub def remove(k: k, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        DMap(RedBlackTree.remove(k, t))

    ///
    /// Updates `m` with `k => f(v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Experimental @LazyWhenPure
    pub def insertWith(f: (v, v) -> v \ ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k => f(k, v, v1)` if `k => v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k => v`.
    ///
    @Experimental @LazyWhenPure
    pub def insertWithKey(f: (k, v, v) -> v \ ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        match purityOf3(f) {
            case Purity3.Pure(g)   => insertWithKeyL(g, k, v, m)
            case Purity3.Impure(g) => insertWithKeyE(g, k, v, m)
        }

    ///
    /// Helper function for `insertWithKey`. Applies `f` lazily.
    ///
    @Lazy
    def insertWithKeyL(f: (k, v, v) -> v, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1, v2) -> lazy f(k1, force v1, force v2);
        DMap(RedBlackTree.insertWith(f1, k, lazy v, t))

    ///
    /// Helper function for `insertWithKey`. Applies `f` eagerly.
    ///
    def insertWithKeyE(f: (k, v, v) -> v \ ef, k: k, v: v, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1, v2) -> {
                let x = f(k1, force v1, force v2);
                lazy x
            };
        DMap(RedBlackTree.insertWith(f1, k, lazy v, t))

    ///
    /// Returns a map with mappings `k => f(v)` for every `k => v` in `m`.
    ///
    /// Purity reflective:
    ///  - Runs in parallel when given a pure function `f`.
    ///  - Applies `f` lazily if `f` is pure.
    ///
    @Experimental @ParallelWhenPure @LazyWhenPure
    pub def map(f: v1 -> v2 \ ef, m: DelayMap[k, v1]): DelayMap[k, v2] \ ef =
        mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k => f(k, v)` for every `k => v` in `m`.
    ///
    /// Purity reflective:
    ///  - Runs in parallel when given a pure function `f`.
    ///  - Applies `f` lazily if `f` is pure.
    ///
    @Experimental @ParallelWhenPure @LazyWhenPure
    pub def mapWithKey(f: (k, v1) -> v2 \ ef, m: DelayMap[k, v1]): DelayMap[k, v2] \ ef =
        match purityOf2(f) {
            case Purity2.Pure(g)   => mapWithKeyL(g, m)
            case Purity2.Impure(g) => mapWithKeyE(g, m)
        }

    ///
    /// Helper function for `mapWithKey`. Applies `f` lazily.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @ParallelWhenPure @Lazy
    def mapWithKeyL(f: (k, v1) -> v2, m: DelayMap[k, v1]): DelayMap[k, v2] =
        let DMap(t) = m;
        let g = (k, v) -> lazy f(k, force v);
        DMap(RedBlackTree.mapWithKey(g, t))

    ///
    /// Helper function for `mapWithKey`. Applies `f` eagerly.
    ///
    def mapWithKeyE(f: (k, v1) -> v2 \ ef, m: DelayMap[k, v1]): DelayMap[k, v2] \ ef =
        let g = (k, v) -> {
                let x1 = f(k, force v);
                lazy x1
            };
        let _ = parallelForce(m);
        let DMap(t) = m;
        DMap(RedBlackTree.mapWithKey(g, t))

    ///
    /// Forces `m` in parallel if it is big, otherwise returns `m`.
    ///
    @Parallel
    def parallelForce(m: DelayMap[k, v]): Unit =
        if (useParallelEvaluation(m))
            forceAll(m)
        else
            ()

    ///
    /// Forces **all values** in `m`.
    ///
    @Experimental @Parallel
    pub def forceAll(m: DelayMap[k, v]): Unit =
        use RedBlackTree.Node;
        def seqLoop(tt) = match tt {
            case Node(_, a, _, v, b) =>
                let _ = seqLoop(a);
                let _ = force v;
                seqLoop(b)
            case _ => ()
        };
        def parLoop(n, tt) = {
            if (n <= 1)
                seqLoop(tt)
            else
                match tt {
                    case Node(_, a, _, v, b) =>
                        par (
                            _ <- parLoop((n - 2) / 2, a); // We divide the rest of the threads as follows:
                            _ <- parLoop((n - 2) / 2, b); // We spawn two new threads leaving us with n - 2
                            _ <- force v                  // that we distribute over the two spanned threads.
                        ) yield ()
                    case _ => ()
                }
        };
        let DMap(t) = m;
        if (useParallelEvaluation(m))
            parLoop(threads() - 1, t)
        else
            seqLoop(t)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `v` satisfies the predicate `f`.
    ///
    @Experimental
    pub def filter(f: v -> Bool \ ef, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k => v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    @Experimental
    pub def filterWithKey(f: (k, v) -> Bool \ ef, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        foldLeftWithKey((acc, k, v) -> if (f(k, v)) insert(k, v, acc) else acc, empty(), m)

    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    @Experimental @Lazy
    pub def union(m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def unionWith(f: (v, v) -> v \ ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def unionWithKey(f: (k, v, v) -> v \ ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        match purityOf3(f) {
            case Purity3.Pure(g)   => unionWithKeyL(g, m1, m2)
            case Purity3.Impure(g) => unionWithKeyE(g, m1, m2)
        }

    ///
    /// Helper function for `unionWithKey`. Applies `f` lazily.
    ///
    @Lazy
    def unionWithKeyL(f: (k, v, v) -> v, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let DMap(xs) = m1;
        let DMap(ys) = m2;
        let f1 = (k, v1, v2) -> lazy (f(k, force v1, force v2));
        if (blackHeight(xs) < blackHeight(ys))
            DMap(foldRight((k, v, acc) -> insertWith(f1, k, v, acc), ys, xs))
        else
            DMap(foldRight((k, v, acc) -> insertWith((_, v1, v2) -> f1(k, v2, v1), k, v, acc), xs, ys))

    ///
    /// Helper function for `unionWithKey`. Applies `f` eagerly.
    ///
    def unionWithKeyE(f: (k, v, v) -> v \ ef, m1: DelayMap[k, v], m2: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        use RedBlackTree.{blackHeight, foldRight, insertWith};
        let DMap(xs) = m1;
        let DMap(ys) = m2;
        let f1 = (k, v1, v2) -> {
                let x = f(k, force v1, force v2);
                lazy x
            };
        if (blackHeight(xs) < blackHeight(ys))
            let _ = parallelForce(m1);
            DMap(foldRight((k, v, acc) -> insertWith(f1, k, v, acc), ys, xs))
        else
            let _ = parallelForce(m2);
            DMap(foldRight((k, v, acc) -> insertWith((_, v1, v2) -> f1(k, v2, v1), k, v, acc), xs, ys))

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    @Experimental
    pub def foldLeft(f: (b, v) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef =
        foldLeftWithKey((acc, _, v) -> f(acc, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, k1, v1), k2, v2)..., vn)`.
    ///
    @Experimental
    pub def foldLeftWithKey(f: (b, k, v) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (b, k, v) -> f(b, k, force v);
        RedBlackTree.foldLeft(f1, s, t)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    @Experimental
    pub def foldRight(f: (v, b) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef =
        foldRightWithKey((_, v, acc) -> f(v, acc), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    @Experimental
    pub def foldRightWithKey(f: (k, v, b) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, b1) -> f(k1, force v1, b1);
        RedBlackTree.foldRight(f1, s, t)

    ///
    /// Applies `f` to a start value `z` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, z)))`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    @Experimental
    pub def foldRightWithCont(f: (v, Unit -> b \ ef) -> b \ ef, z: b, m: DelayMap[k, v]): b \ ef =
        foldRightWithKeyCont((_, v, c) -> f(v, c), z, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    /// A `foldRightWithKeyCont` allows early termination by not calling the continuation.
    ///
    @Experimental
    pub def foldRightWithKeyCont(f: (k, v, Unit -> b \ ef) -> b \ ef, s: b, m: DelayMap[k, v]): b \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, c) -> f(k1, force v1, c);
        RedBlackTree.foldRightWithCont(f1, s, t)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    @Experimental
    pub def reduceLeft(f: (v, v) -> v \ ef, m: DelayMap[k, v]): Option[v] \ ef =
        reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty map.
    ///
    @Experimental
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, k2, v2) -> {
            let (k, v) = f(k1, force v1, k2, force v2);
            (k, lazy v)
        };
        match RedBlackTree.reduceLeft(f1, t) {
            case Some((k, v)) => Some((k, force v))
            case None         => None
        }

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty DelayMap.
    ///
    @Experimental
    pub def reduceRight(f: (v, v) -> v \ ef, m: DelayMap[k, v]): Option[v] \ ef =
        reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) |> Option.map(snd)

    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty DelayMap.
    ///
    @Experimental
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) \ ef, m: DelayMap[k, v]): Option[(k, v)] \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k1, v1, k2, v2) -> {
            let (k, v) = f(k1, force v1, k2, force v2);
            (k, lazy v)
        };
        match RedBlackTree.reduceRight(f1, t) {
            case Some((k, v)) => Some((k, force v))
            case None         => None
        }

    ///
    /// Updates `m` with `k => f(v)` if `k => v` is in `m`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def adjust(f: v -> v \ ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => f(k, v)` if `k => v` is in `m`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def adjustWithKey(f: (k, v) -> v \ ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        updateWithKey((k1, v) -> Some(f(k1, v)), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def update(f: v -> Option[v] \ ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k => v1` if `k => v` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    /// Purity reflective: Applies `f` lazily if `f` is pure.
    ///
    @Experimental @LazyWhenPure
    pub def updateWithKey(f: (k, v) -> Option[v] \ ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        match purityOf2(f) {
            case Purity2.Pure(g)   => updateWithKeyL(g, k, m)
            case Purity2.Impure(g) => updateWithKeyE(g, k, m)
        }

    ///
    /// Helper function for `updateWithKey`. Does not force the `v`.
    ///
    @Lazy
    def updateWithKeyL(f: (k, v) -> Option[v], k: k, m: DelayMap[k, v]): DelayMap[k, v] with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1) -> {
            let res = lazy match f(k1, force v1) {
                case Some(v2) => v2
                case None     => force v1
            };
            Some(res)
        };
        DMap(RedBlackTree.updateWith(f1, k, t))

    ///
    /// Helper function for `updateWithKey`. Forces `v`.
    ///
    def updateWithKeyE(f: (k, v) -> Option[v] \ ef, k: k, m: DelayMap[k, v]): DelayMap[k, v] \ ef with Order[k] =
        let DMap(t) = m;
        let f1 = (k1, v1) -> {
            let res = f(k1, force v1);
            match res {
                case Some(v2) => Some(lazy v2)
                case None     => None
            }
        };
        DMap(RedBlackTree.updateWith(f1, k, t))

    ///
    /// Returns the map `m` as a list of key-value pairs.
    ///
    @Experimental
    pub def toList(m: DelayMap[k, v]): List[(k, v)] =
        foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, m)

    ///
    /// Returns `m` as a Map, i.e. every value is forced.
    ///
    @Experimental @Parallel
    pub def toMap(m: DelayMap[k, v]): Map[k, v] =
        let _ = parallelForce(m);
        let DMap(t) = m;
        Map.Map(RedBlackTree.mapWithKey((_, v) -> force v, t))

    ///
    /// Returns `m` as a mutable map.
    ///
    @Experimental @Parallel
    pub def toMutMap(rc: Region[r], m: DelayMap[k, v]): MutMap[k, v, r] \ r =
        new MutMap @ rc {rc = rc, m = toMap(m)}

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    @Experimental
    pub def toSet(m: DelayMap[k, v]): Set[(k, v)] with Order[k], Order[v] =
        foldLeftWithKey((acc, k, v) -> Set.insert((k, v), acc), Set.empty(), m)

    ///
    /// Returns the map `m` as a `DelayList` of key-value pairs.
    ///
    @Experimental
    pub def toDelayList(m: DelayMap[k, v]): DelayList[(k, v)] =
        foldRightWithKey((k, v, acc) -> DelayList.ECons((k, v), acc), DelayList.ENil, m)

    ///
    /// Returns an iterator over all key-value pairs in `m`.
    ///
    @Experimental
    pub def iterator(rc: Region[r], m: DelayMap[a, b]): Iterator[(a, b), r, r] \ r =
        let DMap(t) = m;
        RedBlackTree.iterator(rc, t) |> Iterator.map(match (k, v) -> (k, force v))

    ///
    /// Returns the map `m` as a MutDeque of key-value pairs.
    ///
    @Experimental
    pub def toMutDeque(rc: Region[r], m: DelayMap[k, v]): MutDeque[(k, v), r] \ r =
        let m1 = DelayMap.toMap(m);
        Map.toMutDeque(rc, m1)

    ///
    /// Applies `f` to every `(key, value)` of `m`.
    ///
    @Experimental
    pub def forEach(f: (k, v) -> Unit \ ef, m: DelayMap[k, v]): Unit \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        let f1 = (k, v) -> f(k, force v);
        RedBlackTree.forEach(f1, t)

    ///
    /// Applies `f` to tuple `(index, key, value)` formed of the keys and values of
    /// DelayMap `m` and the index of the traversal.
    ///
    @Experimental
    pub def forEachWithIndex(f: (Int32, k, v) -> Unit \ ef, m: DelayMap[k, v]): Unit \ ef = region rc {
       let ix = Ref.fresh(rc, 0);
       let f1 = (k, v) -> { let i =  Ref.get(ix); f(i, k, v); Ref.put(i + 1, ix) };
       forEach(f1, m)
    }

    ///
    /// Returns the sum of all values in `m`.
    ///
    @Experimental @Parallel
    pub def sumKeys(m: DelayMap[Int32, v]): Int32 =
        sumWith((k, _) -> k, m)

    ///
    /// Returns the sum of all values in `m`.
    ///
    @Experimental @Parallel
    pub def sumValues(m: DelayMap[k, Int32]): Int32 =
        sumWith((_, v) -> v, m)

    ///
    /// Returns the sum of all key-value pairs `k => v` in `m`
    /// according to the function `f`.
    ///
    /// Purity reflective: Runs in parallel when given a pure function `f`.
    ///
    @Experimental @ParallelWhenPure
    pub def sumWith(f: (k, v) -> Int32 \ ef, m: DelayMap[k, v]): Int32 \ ef =
        let DMap(t) = m;
        def sw() = {
            let _ = parallelForce(m);
            RedBlackTree.sumWith((k, v) -> f(k, force v), t)
        };
        match purityOf2(f) {
            case Purity2.Pure(g) =>
                if (useParallelEvaluation(m))
                    let h = (k, v) -> g(k, force v);
                    RedBlackTree.parSumWith(threads() - 1, h, t)
                else
                    sw()
            case Purity2.Impure(_) => sw()
        }

    ///
    /// Returns the concatenation of the string representation of each key `k`
    /// in `m` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinKeys(sep: String, m: DelayMap[k, v]): String with ToString[k] =
        let DMap(t) = m;
        RedBlackTree.joinKeys(sep, t)

    ///
    /// Returns the concatenation of the string representation of each value `v`
    /// in `m` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinValues(sep: String, m: DelayMap[k, v]): String with ToString[v] =
        joinWith((_, v) -> ToString.toString(v), sep, m)

    ///
    /// Returns the concatenation of the string representation of each key-value pair
    /// `k => v` in `m` according to `f` with `sep` inserted between each element.
    ///
    @Experimental
    pub def joinWith(f: (k, v) -> String \ ef, sep: String, m: DelayMap[k, v]): String \ ef =
        let _ = parallelForce(m);
        let DMap(t) = m;
        RedBlackTree.joinWith((k, v) -> f(k, force v), sep, t)

}
