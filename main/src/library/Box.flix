/*
 * Copyright 2024 Joseph Tan
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

import java.lang.Boolean
import java.lang.Character
import java.lang.Float
import java.lang.Double
import java.lang.Byte
import java.lang.Short
import java.lang.Integer
import java.lang.Long

///
/// A trait for primitive types that can be boxed and unboxed.
///
trait Box[a] {
    type Boxed

    ///
    /// Returns the boxed representation of the given `x`.
    ///
    pub def box(x: a): Box.Boxed[a]

    ///
    /// Returns the unboxed representation of the given `x`.
    ///
    pub def unbox(x: Box.Boxed[a]): a
}

instance Box[Bool] {
    type Boxed = Boolean
    pub def box(x: Bool): Boolean = Boolean.valueOf(x)
    pub def unbox(x: Boolean): Bool = x.booleanValue()
}

instance Box[Char] {
    type Boxed = Character
    pub def box(x: Char): Character = Character.valueOf(x)
    pub def unbox(x: Character): Char = x.charValue()
}

instance Box[Float32] {
    type Boxed = Float
    pub def box(x: Float32): Float = unsafe Float.valueOf(x)
    pub def unbox(x: Float): Float32 = unsafe x.floatValue()
}

instance Box[Float64] {
    type Boxed = Double
    pub def box(x: Float64): Double = Double.valueOf(x)
    pub def unbox(x: Double): Float64 = x.doubleValue()
}

instance Box[Int8] {
    type Boxed = Byte
    pub def box(x: Int8): Byte = unsafe Byte.valueOf(x)
    pub def unbox(x: Byte): Int8 = unsafe x.byteValue()
}

instance Box[Int16] {
    type Boxed = Short
    pub def box(x: Int16): Short = unsafe Short.valueOf(x)
    pub def unbox(x: Short): Int16 = unsafe x.shortValue()
}

instance Box[Int32] {
    type Boxed = Integer
    pub def box(x: Int32): Integer = unsafe Integer.valueOf(x)
    pub def unbox(x: Integer): Int32 = unsafe x.intValue()
}

instance Box[Int64] {
    type Boxed = Long
    pub def box(x: Int64): Long = unsafe Long.valueOf(x)
    pub def unbox(x: Long): Int64 = unsafe x.longValue()
}
