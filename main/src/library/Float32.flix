/*
 * Copyright 2016 Liam Palmer
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

instance LowerBound[Float32] {
    pub def minValue(): Float32 = Float32.minValue()
}

instance UpperBound[Float32] {
    pub def maxValue(): Float32 = Float32.maxValue()
}

mod Float32 {

    import java.lang.Double
    import java.lang.Float
    import java.lang.Math
    import java.lang.NumberFormatException

    ///
    /// Returns the number of bits used to represent a `Float32`.
    ///
    pub def size(): Int32 = 32

    ///
    /// Returns the maximum exponent that a `Float32` may have.
    ///
    pub def maxExponent(): Int32 = 127

    ///
    /// Returns the minimum exponent that a `Float32` may have.
    ///
    pub def minExponent(): Int32 = -126

    ///
    /// Returns the maximum number representable by a `Float32`.
    ///
    pub def maxValue(): Float32 = (2.0f32 - pow(2.0f32, -23.0f32)) * pow(2.0f32, 127.0f32)

    ///
    /// Returns the minimum number representable by a `Float32`.
    ///
    pub def minValue(): Float32 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float32`.
    ///
    pub def minPositiveValue(): Float32 = pow(2.0f32, -149.0f32)

    ///
    /// Returns the NaN (not a number) value of type `Float32`.
    ///
    pub def nan(): Float32 = 0.0f32 / 0.0f32

    ///
    /// Returns the positive infinity value of type `Float32`.
    ///
    pub def positiveInfinity(): Float32 = 1.0f32 / 0.0f32

    ///
    /// Returns the negative infinity value of type `Float32`.
    ///
    pub def negativeInfinity(): Float32 = -1.0f32 / 0.0f32

    ///
    /// Returns true if and only if `x` is a non-infinite and non-Nan `Float32` value.
    ///
    pub def isFinite(x: Float32): Bool = x >= minValue() and x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float32` value.
    ///
    pub def isInfinite(x: Float32): Bool = x == positiveInfinity() or x == negativeInfinity()

    ///
    /// Returns true if and only if `x` is the NaN value of type `Float32`.
    ///
    pub def isNan(x: Float32): Bool = x != x

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Float32, y: Float32): Float32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Float32, y: Float32): Float32 = if (x >= y) x else y

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Float32): String = ToString.toString(x)

    ///
    /// Parse the string `s` as a Float32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Float32] = try {
        Some(unsafe Float.parseFloat(s.strip()))
     } catch {
         case _: NumberFormatException => None
     }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` is within the range of Int8,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127), or it is NaN or infinity.
    ///
    pub def tryToInt8(x: Float32): Option[Int8] =
        if (x < Int8.toFloat32(Int8.minValue()) or x > Int8.toFloat32(Int8.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(unsafe Float.valueOf(x).byteValue())

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` is within the range of Int16,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767), or it is NaN or infinity.
    ///
    pub def tryToInt16(x: Float32): Option[Int16] =
        if (x < Int16.toFloat32(Int16.minValue()) or x > Int16.toFloat32(Int16.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(unsafe Float.valueOf(x).shortValue())

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` is within the range of Int32,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647), or it is NaN or infinity.
    ///
    /// Note: while the range of an Int32 is precisely defined using Int32 values, converting this range to
    /// Float32 values is imprecise.
    ///
    pub def tryToInt32(x: Float32): Option[Int32] =
        if (x < Int32.toFloat32(Int32.minValue()) or x > Int32.toFloat32(Int32.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(unsafe Float.valueOf(x).intValue())

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` is within the range of Int64,
    /// rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807), or it is NaN or infinity.
    ///
    /// Note: while the range of an Int64 is precisely defined using Int64 values, converting
    /// this range to Float32 values is imprecise.
    ///
    pub def tryToInt64(x: Float32): Option[Int64] =
        if (x < Int64.toFloat32(Int64.minValue()) or x > Int64.toFloat32(Int64.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(unsafe Float.valueOf(x).longValue())

    ///
    /// Convert `x` to an `Option[BigInt]`.
    ///
    /// Returns `Some(x as BigInt)` if the numeric value of `x` is representable as a BigInt.
    ///
    /// Returns `None` if the value of `x` is NaN or infinity.
    ///
    pub def tryToBigInt(x: Float32): Option[BigInt] =
        Float64.tryToBigInt(toFloat64(x))

    ///
    /// Convert `x` to an Float64.
    ///
    pub def toFloat64(x: Float32): Float64 =
        unsafe Float.valueOf(x).doubleValue()

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `Some(x as BigDecimal)` if the numeric value of `x` is representable
    /// as a BigDecimal value.
    ///
    /// If `x` is NaN or infinity return `None`.
    ///
    pub def tryToBigDecimal(x: Float32): Option[BigDecimal] =
        toFloat64(x) |> Float64.tryToBigDecimal

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(x: Float32, minimum: Float32, maximum: Float32): Float32 =
        if (x < minimum)
             minimum
        else
            if (x > maximum)
                maximum
            else
                x

    ///
    /// Convert `x` to an `Int8`.
    ///
    /// Returns `x` clamped within the Int8 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int8 risks masking it.
    ///
    pub def clampToInt8(x: Float32, minimum: Int8, maximum: Int8, nanValue: Int8): Int8 =
        let minf32 = Int8.toFloat32(minimum);
        let maxf32 = Int8.toFloat32(maximum);
        if (isNan(x))
            nanValue
        else
            unsafe Float.valueOf(clamp(x, minf32, maxf32)).byteValue()

    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int16 risks masking it.
    ///
    pub def clampToInt16(x: Float32, minimum: Int16, maximum: Int16, nanValue: Int16): Int16 =
        let minf32 = Int16.toFloat32(minimum);
        let maxf32 = Int16.toFloat32(maximum);
        if (isNan(x))
            nanValue
        else
            unsafe Float.valueOf(clamp(x, minf32, maxf32)).shortValue()

    ///
    /// Convert `x` to an `Int32`.
    ///
    /// Returns `x` clamped within the Int32 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int32 risks masking it.
    ///
    pub def clampToInt32(x: Float32, minimum: Int32, maximum: Int32, nanValue: Int32): Int32 =
        let minf32 = Int32.toFloat32(minimum);
        let maxf32 = Int32.toFloat32(maximum);
        if (isNan(x))
            nanValue
        else
            unsafe Float.valueOf(clamp(x, minf32, maxf32)).intValue()

    ///
    /// Convert `x` to an `Int64`.
    ///
    /// Returns `x` clamped within the Int64 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int64 risks masking it.
    ///
    pub def clampToInt64(x: Float32, minimum: Int64, maximum: Int64, nanValue: Int64): Int64 =
        let minf32 = Int64.toFloat32(minimum);
        let maxf32 = Int64.toFloat32(maximum);
        if (isNan(x))
            nanValue
        else
            unsafe Float.valueOf(clamp(x, minf32, maxf32)).longValue()

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: Float32): Float32 =
        unsafe Math.abs(x)

    ///
    /// Returns `x` rounded up to a Float32 representing the nearest larger integer value.
    ///
    pub def ceil(x: Float32): Float32 =
        let x1 = unsafe Math.ceil(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Returns `x` rounded down to a Float32 representing the nearest smaller integer value.
    ///
    pub def floor(x: Float32): Float32 =
        let x1 = unsafe Math.floor(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Returns `x` rounded to a Float32 representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5f32) == 0.0f32`).
    ///
    pub def round(x: Float32): Float32 =
        let x1 = unsafe Math.rint(Float.valueOf(x).doubleValue());
        Double.valueOf(x1).floatValue()

    ///
    /// Returns `b` raised to the power of `n`.
    ///
    pub def pow(b: Float32, n: Float32): Float32 = $FLOAT32_EXP$(b, n)

    ///
    /// Get the primitive Float32 value from its object representation (i.e. java.lang.Float).
    ///
    /// This function is expected to be used when marshaling Float32s from Java. Generally in Flix
    /// code you should not need to use `java.lang.Float`.
    ///
    pub def floatValue(d: Float): Float32 =
        unsafe d.floatValue()

    ///
    /// Convert an Float32 value to its object representation (i.e. java.lang.Float).
    ///
    /// This function is expected to be used when marshaling Float32s to Java. Generally in Flix
    /// code you should not need to use `java.lang.Float`.
    ///
    pub def valueOf(d: Float32): Float =
        unsafe Float.valueOf(d)

}
