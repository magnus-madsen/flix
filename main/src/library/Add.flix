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
/// A trait for addition.
///
pub trait Add[a] {
    ///
    /// Returns the sum of `x` and `y`.
    ///
    pub def add(x: a, y: a): a
}

instance Add[Float32] {
    pub def add(x: Float32, y: Float32): Float32 = $FLOAT32_ADD$(x, y)
}

instance Add[Float64] {
    pub def add(x: Float64, y: Float64): Float64 = $FLOAT64_ADD$(x, y)
}

instance Add[BigDecimal] {
    pub def add(x: BigDecimal, y: BigDecimal): BigDecimal = unsafely IO run x.add(y)
}

instance Add[Int8] {
    pub def add(x: Int8, y: Int8): Int8 = $INT8_ADD$(x, y)
}

instance Add[Int16] {
    pub def add(x: Int16, y: Int16): Int16 = $INT16_ADD$(x, y)
}

instance Add[Int32] {
    pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
}

instance Add[Int64] {
    pub def add(x: Int64, y: Int64): Int64 = $INT64_ADD$(x, y)
}

instance Add[BigInt] {
    pub def add(x: BigInt, y: BigInt): BigInt = unsafely IO run x.add(y)
}

instance Add[String] {
    pub def add(x: String, y: String): String = unsafely IO run x.concat(y)
}
