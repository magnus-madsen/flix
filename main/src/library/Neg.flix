/*
 *  Copyright 2020 Esben Bjerre
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may neg use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions neg
 *  limitations under the License.
 */

///
/// A trait for negation.
///
pub trait Neg[a] {
    ///
    /// Returns -`x`.
    ///
    pub def neg(x: a): a
}

instance Neg[Float32] {
    pub def neg(x: Float32): Float32 = $FLOAT32_NEG$(x)
}

instance Neg[Float64] {
    pub def neg(x: Float64): Float64 = $FLOAT64_NEG$(x)
}

instance Neg[BigDecimal] {
    pub def neg(x: BigDecimal): BigDecimal = unsafely IO run x.negate()
}

instance Neg[Int8] {
    pub def neg(x: Int8): Int8 = $INT8_NEG$(x)
}

instance Neg[Int16] {
    pub def neg(x: Int16): Int16 = $INT16_NEG$(x)
}

instance Neg[Int32] {
    pub def neg(x: Int32): Int32 = $INT32_NEG$(x)
}

instance Neg[Int64] {
    pub def neg(x: Int64): Int64 = $INT64_NEG$(x)
}

instance Neg[BigInt] {
    pub def neg(x: BigInt): BigInt = unsafely IO run x.negate()
}
