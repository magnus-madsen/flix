/*
 *  Copyright 2020 Matthew Lutze, Esben Bjerre
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
/// A trait for types that can be hashed.
///
pub trait Hash[a] {
    ///
    /// Returns a hash value for the given x.
    ///
    pub def hash(x: a): Int32

    // TODO: Add a law that hash is consistent with equals.
}

instance Hash[Unit] {
    pub def hash(_x: Unit): Int32 = 0
}

instance Hash[Bool] {
    pub def hash(x: Bool): Int32 = match x {
        case true => 1231
        case false => 1237
    }
}

instance Hash[Char] {
    pub def hash(x: Char): Int32 = unsafely IO run Character.hashCode(x)
}

instance Hash[Float32] {
    pub def hash(x: Float32): Int32 = unsafely IO run Float.hashCode(x)
}

instance Hash[Float64] {
    pub def hash(x: Float64): Int32 = unsafely IO run Double.hashCode(x)
}

instance Hash[BigDecimal] {
    pub def hash(x: BigDecimal): Int32 = unsafely IO run x.hashCode()
}

instance Hash[Int8] {
    pub def hash(x: Int8): Int32 = unsafely IO run Byte.hashCode(x)
}

instance Hash[Int16] {
    pub def hash(x: Int16): Int32 = unsafely IO run Short.hashCode(x)
}

instance Hash[Int32] {
    pub def hash(x: Int32): Int32 = unsafely IO run Integer.hashCode(x)
}

instance Hash[Int64] {
    pub def hash(x: Int64): Int32 = unsafely IO run Long.hashCode(x)
}

instance Hash[String] {
    pub def hash(x: String): Int32 = unsafely IO run x.hashCode()
}

instance Hash[BigInt] {
    pub def hash(x: BigInt): Int32 = unsafely IO run x.hashCode()
}

instance Hash[(a1, a2)] with Hash[a1], Hash[a2] {
    pub def hash(t: (a1, a2)): Int32 = match t {
        case (x1, x2) => Hash.hash(x1) `Hash.combine` Hash.hash(x2)
    }
}

instance Hash[(a1, a2, a3)] with Hash[a1], Hash[a2], Hash[a3] {
    pub def hash(t: (a1, a2, a3)): Int32 = match t {
        case (x1, x2, x3) => Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3)
    }
}

instance Hash[(a1, a2, a3, a4)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4] {
    pub def hash(t: (a1, a2, a3, a4)): Int32 = match t {
        case (x1, x2, x3, x4) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
    }
}

instance Hash[(a1, a2, a3, a4, a5)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5] {
    pub def hash(t: (a1, a2, a3, a4, a5)): Int32 = match t {
        case (x1, x2, x3, x4, x5) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
                `Hash.combine` Hash.hash(x9)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9], Hash[a10] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
                `Hash.combine` Hash.hash(x9) `Hash.combine` Hash.hash(x10)
    }
}

mod Hash {

    ///
    /// Combines the two given hash values.
    ///
    pub def combine(h1: Int32, h2: Int32): Int32 = {
        h1 * magic() + h2
    }

    ///
    /// Returns a number used for combining hash values.
    ///
    pub def magic(): Int32 = -1640531535

}
