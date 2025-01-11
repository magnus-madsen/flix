/*
 *  Copyright 2020 Matthew Lutze
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

import java.lang.Byte
import java.lang.Character
import java.lang.Double
import java.lang.Float
import java.lang.Integer
import java.lang.Long
import java.lang.Short

///
/// A trait for types that can be converted to strings.
///
pub trait ToString[a] {
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String
}

instance ToString[Unit] {
    pub def toString(_x: Unit): String = "()"
}

instance ToString[Bool] {
    pub def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString[Char] {
    pub def toString(x: Char): String = Character.toString(x)
}

instance ToString[Float32] {
    pub def toString(x: Float32): String = unsafe Float.toString(x)
}

instance ToString[Float64] {
    pub def toString(x: Float64): String = Double.toString(x)
}

instance ToString[Int8] {
    pub def toString(x: Int8): String = unsafe Byte.toString(x)
}

instance ToString[Int16] {
    pub def toString(x: Int16): String = unsafe Short.toString(x)
}

instance ToString[Int32] {
    pub def toString(x: Int32): String = unsafe Integer.toString(x)
}

instance ToString[Int64] {
    pub def toString(x: Int64): String = unsafe Long.toString(x)
}

instance ToString[String] {
    pub def toString(x: String): String = x
}

instance ToString[BigInt] {
    pub def toString(x: BigInt): String = unsafe x.toString()
}

instance ToString[BigDecimal] {
    pub def toString(x: BigDecimal): String = unsafe x.toString()
}

instance ToString[Regex] {
    pub def toString(x: Regex): String = unsafe x.toString()
}

instance ToString[(a1, a2)] with ToString[a1], ToString[a2] {
    pub def toString(t: (a1, a2)): String = match t {
        case (x1, x2) =>
            "(${x1}, ${x2})"
    }
}

instance ToString[(a1, a2, a3)] with ToString[a1], ToString[a2], ToString[a3] {
    pub def toString(t: (a1, a2, a3)): String = match t {
        case (x1, x2, x3) =>
            "(${x1}, ${x2}, ${x3})"
    }
}

instance ToString[(a1, a2, a3, a4)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4] {
    pub def toString(t: (a1, a2, a3, a4)): String = match t {
        case (x1, x2, x3, x4) =>
            "(${x1}, ${x2}, ${x3}, ${x4})"
    }
}

instance ToString[(a1, a2, a3, a4, a5)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5] {
    pub def toString(t: (a1, a2, a3, a4, a5)): String = match t {
        case (x1, x2, x3, x4, x5) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6)): String = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14], ToString[a15] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14}, ${x15})"
    }
}
