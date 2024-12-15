/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

use Bool.{==>}

///
/// A Partial Order is a function ⊑ which satisfies three properties: reflexivity, anti-symmetry, and transitivity.
///
pub lawful trait PartialOrder[a] {

    ///
    /// Returns `true` if `x` is smaller or equal to `y`.
    ///
    pub def lessEqual(x: a, y: a): Bool
    // TODO This should not return a Boolean, but instead perform a three-way comparison.

    ///
    /// Reflexivity: An element `x` is lower or equal to itself.
    ///
    law reflexivity: forall(x: a) PartialOrder.lessEqual(x, x)

    ///
    /// Transitivity: If `x` is lower or equal to `y` and `y` is lower equal to `z` then `x` must be lower or equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a) (PartialOrder.lessEqual(x, y) and PartialOrder.lessEqual(y, z)) ==> PartialOrder.lessEqual(x, z)
}

instance PartialOrder[Int8] {
    pub def lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)
}

instance PartialOrder[Int16] {
    pub def lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)
}

instance PartialOrder[Int32] {
    pub def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)
}

instance PartialOrder[Int64] {
    pub def lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)
}

instance PartialOrder[BigInt] {
    pub def lessEqual(x: BigInt, y: BigInt): Bool =
        unsafely IO run (x.compareTo(y)) <= 0
}

instance PartialOrder[(a1, a2)] with PartialOrder[a1], PartialOrder[a2] {
    pub def lessEqual(x: (a1, a2),
                           y: (a1, a2)): Bool = match (x, y) {
        case ((x1, x2), (y1, y2)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2)
    }
}

instance PartialOrder[(a1, a2, a3)] with PartialOrder[a1], PartialOrder[a2],
                                         PartialOrder[a3] {
    pub def lessEqual(x: (a1, a2, a3),
                           y: (a1, a2, a3)): Bool = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3)
    }
}

instance PartialOrder[(a1, a2, a3, a4)] with PartialOrder[a1], PartialOrder[a2],
                                             PartialOrder[a3], PartialOrder[a4] {
    pub def lessEqual(x: (a1, a2, a3, a4),
                           y: (a1, a2, a3, a4)): Bool = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5)] with PartialOrder[a1], PartialOrder[a2],
                                                 PartialOrder[a3], PartialOrder[a4],
                                                 PartialOrder[a5] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5),
                           y: (a1, a2, a3, a4, a5)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6)] with PartialOrder[a1], PartialOrder[a2],
                                                     PartialOrder[a3], PartialOrder[a4],
                                                     PartialOrder[a5], PartialOrder[a6] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6),
                           y: (a1, a2, a3, a4, a5, a6)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7)] with PartialOrder[a1], PartialOrder[a2],
                                                         PartialOrder[a3], PartialOrder[a4],
                                                         PartialOrder[a5], PartialOrder[a6],
                                                         PartialOrder[a7] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7),
                           y: (a1, a2, a3, a4, a5, a6, a7)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8)] with PartialOrder[a1], PartialOrder[a2],
                                                             PartialOrder[a3], PartialOrder[a4],
                                                             PartialOrder[a5], PartialOrder[a6],
                                                             PartialOrder[a7], PartialOrder[a8] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with PartialOrder[a1], PartialOrder[a2],
                                                                 PartialOrder[a3], PartialOrder[a4],
                                                                 PartialOrder[a5], PartialOrder[a6],
                                                                 PartialOrder[a7], PartialOrder[a8],
                                                                 PartialOrder[a9] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8) and
            PartialOrder.lessEqual(x9, y9)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with PartialOrder[a1], PartialOrder[a2],
                                                                      PartialOrder[a3], PartialOrder[a4],
                                                                      PartialOrder[a5], PartialOrder[a6],
                                                                      PartialOrder[a7], PartialOrder[a8],
                                                                      PartialOrder[a9], PartialOrder[a10] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8) and
            PartialOrder.lessEqual(x9, y9) and
            PartialOrder.lessEqual(x10, y10)
    }
}
