/*
 * Copyright 2020 Esben Bjerre
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

use Bool.{==>, <==>}

///
/// A trait for types with a total order.
///
pub lawful trait Order[a] with Eq[a] {

    ///
    /// Returns `Comparison.LessThan` if `x` < `y`, `Equal` if `x` == `y` or `Comparison.GreaterThan` `if `x` > `y`.
    ///
    pub def compare(x: a, y: a): Comparison

    ///
    /// Returns `true` if and only if `x < y`.
    ///
    pub def less(x: a, y: a): Bool = match Order.compare(x, y) {
        case Comparison.LessThan   => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x <= y`.
    ///
    pub def lessEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case Comparison.LessThan   => true
        case Comparison.EqualTo    => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x > y`.
    ///
    pub def greater(x: a, y: a): Bool = match Order.compare(x, y) {
        case Comparison.GreaterThan    => true
        case _              => false
    }

    ///
    /// Returns `true` if and only if `x >= y`.
    ///
    pub def greaterEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case Comparison.GreaterThan    => true
        case Comparison.EqualTo        => true
        case _              => false
    }

    ///
    /// Returns the minimum of `x` and `y`.
    ///
    pub def min(x: a, y: a): a = match (x <=> y) {
        case Comparison.GreaterThan => y
        case _ => x
    }

    ///
    /// Returns the maximum of `x` and `y`.
    ///
    pub def max(x: a, y: a): a = match (x <=> y) {
        case Comparison.LessThan => y
        case _ => x
    }

    ///
    /// Reflexivity: An element `x` is lower or equal to itself.
    ///
    law reflexivity: forall(x: a) x <= x

    ///
    /// Antisymmetry: If `x` is lower or equal to `y` and `y` is lower or equal to `x` then `x` must be equal to `y`.
    ///
    law symmetry: forall(x: a, y: a) ((x <= y) and (y <= x)) ==> (x == y)

    ///
    /// Transitivity: If `x` is lower or equal to `y` and `y` is lower equal to `z` then `x` must be lower or equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a) ((x <= y) and (y <= z)) ==> (x <= z)

    ///
    /// Totality: For each two elements `x` and `y` either `x` is lower or equal to `y` or the other way round.
    ///
    law totality: forall(x: a, y: a) ((x <= y) or (y <= x))

    ///
    /// Definition of the minimum function.
    ///
    law min: forall(x: a, y: a) ((x <= y) ==> (Order.min(x, y) == x)) and ((y <= x) ==> (Order.min(x, y) == y))

    ///
    /// Definition of the maximum function.
    ///
    law max: forall(x: a, y: a) ((y <= x) ==> (Order.max(x, y) == x)) and ((x <= y) ==> (Order.max(x, y) == y))

    ///
    /// x < y is logically equivalent to not (y <= x).
    /// This law defines the associated strict total order "<" associated with "<=".
    ///
    law strictTotalOrder: forall(x: a, y: a) (x < y) <==> (not (y <= x))

    ///
    /// This law defines ">" based on "<".
    ///
    law inverseOrder1: forall(x: a, y: a) (x > y) <==> (y < x)

    ///
    /// This law defines ">=" based on "<=".
    ///
    law inverseOrder2: forall(x: a, y: a) (x >= y) <==> (y <= x)

    ///
    /// Definition of `compare` based on the defined total order.
    ///
    law compare: forall(x: a, y: a) ((x < y) ==> (Order.compare(x, y) == Comparison.LessThan)) and ((x == y) ==> (Order.compare(x, y) == Comparison.EqualTo)) and ((x > y) ==> (Order.compare(x, y) == Comparison.GreaterThan))
}

mod Order {

    ///
    /// Returns the minimum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def minBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == Comparison.GreaterThan) y else x

    ///
    /// Returns the maximum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def maxBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == Comparison.LessThan) y else x

    ///
    /// Lazily combines two comparisons.
    ///
    /// If `c1` is either `Comparison.LessThan` or `Comparison.GreaterThan` then `c2` is never evaluated.
    ///
    pub def thenCompare(c1: Comparison, c2: Lazy[Comparison]): Comparison =
        if (c1 != Comparison.EqualTo) c1 else force c2

}

instance Order[Unit] {

    pub def compare(_: Unit, _: Unit): Comparison = Comparison.EqualTo

}

instance Order[Bool] {

    pub def compare(x: Bool, y: Bool): Comparison =
        if (x) {
            if (y)
                Comparison.EqualTo
            else
                Comparison.GreaterThan
        } else {
            if (y)
                Comparison.LessThan
            else
                Comparison.EqualTo
        }

}

instance Order[Char] {

    redef less(x: Char, y: Char): Bool = $CHAR_LT$(x, y)

    redef lessEqual(x: Char, y: Char): Bool = $CHAR_LE$(x, y)

    redef greater(x: Char, y: Char): Bool = $CHAR_GT$(x, y)

    redef greaterEqual(x: Char, y: Char): Bool = $CHAR_GE$(x, y)

    pub def compare(x: Char, y: Char): Comparison =
        if ($CHAR_LT$(x, y))
            Comparison.LessThan
        else if ($CHAR_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[Float32] {

    redef less(x: Float32, y: Float32): Bool = $FLOAT32_LT$(x, y)

    redef lessEqual(x: Float32, y: Float32): Bool = $FLOAT32_LE$(x, y)

    redef greater(x: Float32, y: Float32): Bool = $FLOAT32_GT$(x, y)

    redef greaterEqual(x: Float32, y: Float32): Bool = $FLOAT32_GE$(x, y)

    pub def compare(x: Float32, y: Float32): Comparison =
        if ($FLOAT32_LT$(x, y))
            Comparison.LessThan
        else if ($FLOAT32_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[Float64] {

    redef less(x: Float64, y: Float64): Bool = $FLOAT64_LT$(x, y)

    redef lessEqual(x: Float64, y: Float64): Bool = $FLOAT64_LE$(x, y)

    redef greater(x: Float64, y: Float64): Bool = $FLOAT64_GT$(x, y)

    redef greaterEqual(x: Float64, y: Float64): Bool = $FLOAT64_GE$(x, y)

    pub def compare(x: Float64, y: Float64): Comparison =
        if ($FLOAT64_LT$(x, y))
            Comparison.LessThan
        else if ($FLOAT64_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[BigDecimal] {

    redef less(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) < 0

    redef lessEqual(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) <= 0

    redef greater(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) > 0

    redef greaterEqual(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) >= 0

    pub def compare(x: BigDecimal, y: BigDecimal): Comparison =
        match (unsafely IO run x.compareTo(y)) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}

instance Order[Int8] {

    redef less(x: Int8, y: Int8): Bool = $INT8_LT$(x, y)

    redef lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)

    redef greater(x: Int8, y: Int8): Bool = $INT8_GT$(x, y)

    redef greaterEqual(x: Int8, y: Int8): Bool = $INT8_GE$(x, y)

    pub def compare(x: Int8, y: Int8): Comparison =
        if ($INT8_LT$(x, y))
            Comparison.LessThan
        else if ($INT8_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[Int16] {

    redef less(x: Int16, y: Int16): Bool = $INT16_LT$(x, y)

    redef lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)

    redef greater(x: Int16, y: Int16): Bool = $INT16_GT$(x, y)

    redef greaterEqual(x: Int16, y: Int16): Bool = $INT16_GE$(x, y)

    pub def compare(x: Int16, y: Int16): Comparison =
        if ($INT16_LT$(x, y))
            Comparison.LessThan
        else if ($INT16_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[Int32] {

    redef less(x: Int32, y: Int32): Bool = $INT32_LT$(x, y)

    redef lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)

    redef greater(x: Int32, y: Int32): Bool = $INT32_GT$(x, y)

    redef greaterEqual(x: Int32, y: Int32): Bool = $INT32_GE$(x, y)

    pub def compare(x: Int32, y: Int32): Comparison =
        if ($INT32_LT$(x, y))
            Comparison.LessThan
        else if ($INT32_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[Int64] {

    redef less(x: Int64, y: Int64): Bool = $INT64_LT$(x, y)

    redef lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)

    redef greater(x: Int64, y: Int64): Bool = $INT64_GT$(x, y)

    redef greaterEqual(x: Int64, y: Int64): Bool = $INT64_GE$(x, y)

    pub def compare(x: Int64, y: Int64): Comparison =
        if ($INT64_LT$(x, y))
            Comparison.LessThan
        else if ($INT64_GT$(x, y))
            Comparison.GreaterThan
        else
            Comparison.EqualTo

}

instance Order[BigInt] {

    redef less(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) < 0

    redef lessEqual(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) <= 0

    redef greater(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) > 0

    redef greaterEqual(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) >= 0

    pub def compare(x: BigInt, y: BigInt): Comparison =
        match (unsafely IO run x.compareTo(y)) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}

instance Order[String] {

    pub def compare(x: String, y: String): Comparison =
        Comparison.fromInt32((unsafely IO run x.compareTo(y)))

}

instance Order[(a1, a2)] with Order[a1], Order[a2] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2), t2: (a1, a2)): Comparison =
        use Order.thenCompare;
        let (x1, x2) = t1;
        let (y1, y2) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2)

}

instance Order[(a1, a2, a3)] with Order[a1], Order[a2], Order[a3] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3), t2: (a1, a2, a3)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3)

}

instance Order[(a1, a2, a3, a4)] with Order[a1], Order[a2], Order[a3], Order[a4] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4)

}

instance Order[(a1, a2, a3, a4, a5)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5) = t1;
        let (y1, y2, y3, y4, y5) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5)

}

instance Order[(a1, a2, a3, a4, a5, a6)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6) = t1;
        let (y1, y2, y3, y4, y5, y6) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7) = t1;
        let (y1, y2, y3, y4, y5, y6, y7) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13], Order[a14] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13) `thenCompare`
        lazy (x14 <=> y14)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13], Order[a14], Order[a15] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13) `thenCompare`
        lazy (x14 <=> y14) `thenCompare`
        lazy (x15 <=> y15)

}
