/*
 *  Copyright 2020 Esben Bjerre
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

///
/// A trait for subtraction.
///
pub trait Sub[a] {
    ///
    /// Returns the difference of `x` and `y`.
    ///
    pub def sub(x: a, y: a): a
}

instance Sub[Float32] {
    pub def sub(x: Float32, y: Float32): Float32 = $FLOAT32_SUB$(x, y)
}

instance Sub[Float64] {
    pub def sub(x: Float64, y: Float64): Float64 = $FLOAT64_SUB$(x, y)
}

instance Sub[BigDecimal] {
    pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal = unsafely IO run x.subtract(y)
}

instance Sub[Int8] {
    pub def sub(x: Int8, y: Int8): Int8 = $INT8_SUB$(x, y)
}

instance Sub[Int16] {
    pub def sub(x: Int16, y: Int16): Int16 = $INT16_SUB$(x, y)
}

instance Sub[Int32] {
    pub def sub(x: Int32, y: Int32): Int32 = $INT32_SUB$(x, y)
}

instance Sub[Int64] {
    pub def sub(x: Int64, y: Int64): Int64 = $INT64_SUB$(x, y)
}

instance Sub[BigInt] {
    pub def sub(x: BigInt, y: BigInt): BigInt = unsafely IO run x.subtract(y)
}
