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
/// A trait for equality and inequality.
///
pub lawful trait Eq[a] {

    ///
    /// Returns `true` if and only if `x` is equal to `y`.
    ///
    pub def eq(x: a, y: a): Bool

    ///
    /// Returns `true` if and only if `x` is not equal to `y`.
    ///
    pub def neq(x: a, y: a): Bool = not Eq.eq(x, y)

    ///
    /// Reflexivity: An element `x` is equal to itself.
    ///
    law reflexivity: forall(x: a) x == x

    ///
    /// Symmetry: If `x` is equal to `y` then `y` must also be equal to `x`.
    ///
    law symmetry: forall(x: a, y: a) (x == y) ==> (y == x)

    ///
    /// Transitivity: If `x` is equal to `y` and `y` is equal to `z` then `x` must be equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a) ((x == y) and (y == z)) ==> (x == z)

    ///
    /// x != y is logically equivalent to not (x == y).
    ///
    law inverseNeq: forall(x: a, y: a) (x != y) <==> (not (x == y))
}

instance Eq[Unit] {
    pub def eq(_x: Unit, _y: Unit): Bool = true
}

instance Eq[Bool] {
    pub def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
    redef neq(x: Bool, y: Bool): Bool = $BOOL_NEQ$(x, y)
}

instance Eq[Char] {
    pub def eq(x: Char, y: Char): Bool = $CHAR_EQ$(x, y)
    redef neq(x: Char, y: Char): Bool = $CHAR_NEQ$(x, y)
}

instance Eq[Float32] {
    pub def eq(x: Float32, y: Float32): Bool = $FLOAT32_EQ$(x, y)
    redef neq(x: Float32, y: Float32): Bool = $FLOAT32_NEQ$(x, y)
}

instance Eq[Float64] {
    pub def eq(x: Float64, y: Float64): Bool = $FLOAT64_EQ$(x, y)
    redef neq(x: Float64, y: Float64): Bool = $FLOAT64_NEQ$(x, y)
}

instance Eq[BigDecimal] {
    pub def eq(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) == 0

    redef neq(x: BigDecimal, y: BigDecimal): Bool =
        (unsafely IO run x.compareTo(y)) != 0
}

instance Eq[Int8] {
    pub def eq(x: Int8, y: Int8): Bool = $INT8_EQ$(x, y)
    redef neq(x: Int8, y: Int8): Bool = $INT8_NEQ$(x, y)
}

instance Eq[Int16] {
    pub def eq(x: Int16, y: Int16): Bool = $INT16_EQ$(x, y)
    redef neq(x: Int16, y: Int16): Bool = $INT16_NEQ$(x, y)
}

instance Eq[Int32] {
    pub def eq(x: Int32, y: Int32): Bool = $INT32_EQ$(x, y)
    redef neq(x: Int32, y: Int32): Bool = $INT32_NEQ$(x, y)
}

instance Eq[Int64] {
    pub def eq(x: Int64, y: Int64): Bool = $INT64_EQ$(x, y)
    redef neq(x: Int64, y: Int64): Bool = $INT64_NEQ$(x, y)
}

instance Eq[BigInt] {
    pub def eq(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) == 0

    redef neq(x: BigInt, y: BigInt): Bool =
        (unsafely IO run x.compareTo(y)) != 0
}

instance Eq[String] {
    pub def eq(x: String, y: String): Bool = unsafely IO run x.equals(y)
}

instance Eq[(a1, a2)] with Eq[a1], Eq[a2] {

    pub def eq(t1: (a1, a2), t2: (a1, a2)): Bool =
        let (x1, x2) = t1;
        let (y1, y2) = t2;
            x1 == y1 and x2 == y2

}

instance Eq[(a1, a2, a3)] with Eq[a1], Eq[a2], Eq[a3] {

    pub def eq(t1: (a1, a2, a3), t2: (a1, a2, a3)): Bool =
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
            x1 == y1 and x2 == y2 and x3 == y3

}

instance Eq[(a1, a2, a3, a4)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4] {

    pub def eq(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Bool =
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4

}

instance Eq[(a1, a2, a3, a4, a5)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5] {

    pub def eq(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Bool =
        let (x1, x2, x3, x4, x5) = t1;
        let (y1, y2, y3, y4, y5) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5

}

instance Eq[(a1, a2, a3, a4, a5, a6)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Bool =
        let (x1, x2, x3, x4, x5, x6) = t1;
        let (y1, y2, y3, y4, y5, y6) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7) = t1;
        let (y1, y2, y3, y4, y5, y6, y7) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8
}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13], Eq[a14] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13 and x14 == y14

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13], Eq[a14], Eq[a15] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13 and x14 == y14 and x15 == y15

}
