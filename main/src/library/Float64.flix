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

instance LowerBound[Float64] {
    pub def minValue(): Float64 = Float64.minValue()
}

instance UpperBound[Float64] {
    pub def maxValue(): Float64 = Float64.maxValue()
}

mod Float64 {

    import java.lang.Double
    import java.lang.Math
    import java.math.BigDecimal
    import java.lang.NumberFormatException

    ///
    /// Returns the number of bits used to represent a `Float64`.
    ///
    pub def size(): Int32 = 64

    ///
    /// Returns the maximum exponent that a `Float64` may have.
    ///
    pub def maxExponent(): Int32 = 1023

    ///
    /// Returns the minimum exponent that a `Float64` may have.
    ///
    pub def minExponent(): Int32 = -1022

    ///
    /// Returns the maximum number representable by a `Float64`.
    ///
    pub def maxValue(): Float64 = (2.0f64 - pow(2.0f64, -52.0f64)) * pow(2.0f64, 1023.0f64)

    ///
    /// Returns the minimum number representable by a `Float64`.
    ///
    pub def minValue(): Float64 = -maxValue()

    ///
    /// Returns the minimum positive number representable by a `Float64`.
    ///
    pub def minPositiveValue(): Float64 = pow(2.0f64, -1074.0f64)

    ///
    /// Returns the NaN (not a number) value of type `Float64`.
    ///
    pub def nan(): Float64 = 0.0f64 / 0.0f64

    ///
    /// Returns the positive infinity value of type `Float64`.
    ///
    pub def positiveInfinity(): Float64 = 1.0f64 / 0.0f64

    ///
    /// Returns the negative infinity value of type `Float64`.
    ///
    pub def negativeInfinity(): Float64 = -1.0f64 / 0.0f64

    ///
    /// Returns true if and only if `x` is a non-infinite and non-Nan `Float64` value.
    ///
    pub def isFinite(x: Float64): Bool = x >= minValue() and x <= maxValue()

    ///
    /// Returns true if and only if `x` is an infinite and non-Nan `Float64` value.
    ///
    pub def isInfinite(x: Float64): Bool = x == positiveInfinity() or x == negativeInfinity()

    ///
    /// Returns true if and only if `x` is the NaN value of type `Float64`.
    ///
    pub def isNan(x: Float64): Bool = x != x

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Float64, y: Float64): Float64 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Float64, y: Float64): Float64 = if (x >= y) x else y

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Float64): String = ToString.toString(x)

    ///
    /// Parse the string `s` as a Float64, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Float64] = try {
        Some(Double.parseDouble(unsafe s.strip()))
    } catch {
        case _: NumberFormatException => None
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` is within the range
    /// of Int8, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127), or it is NaN or infinity.
    ///
    pub def tryToInt8(x: Float64): Option[Int8] =
        if (x < Int8.toFloat64(Int8.minValue()) or x > Int8.toFloat64(Int8.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(Double.valueOf(x).byteValue())

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` is within the range
    /// of Int16, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767), or it is NaN or infinity.
    ///
    pub def tryToInt16(x: Float64): Option[Int16] =
        if (x < Int16.toFloat64(Int16.minValue()) or x > Int16.toFloat64(Int16.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(Double.valueOf(x).shortValue())

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` is within the range
    /// of Int32, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647), or it is NaN or infinity.
    ///
    pub def tryToInt32(x: Float64): Option[Int32] =
        if (x < Int32.toFloat64(Int32.minValue()) or x > Int32.toFloat64(Int32.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(Double.valueOf(x).intValue())

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` is within the range
    /// of Int64, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807), or it is NaN or infinity.
    ///
    /// Note: while the range of an Int64 is precisely defined using Int64 values,
    /// converting this range to Float64 values is imprecise.
    ///
    pub def tryToInt64(x: Float64): Option[Int64] =
        if (x < Int64.toFloat64(Int64.minValue()) or x > Int64.toFloat64(Int64.maxValue()) or isNan(x) or isInfinite(x))
            None
        else
            Some(Double.valueOf(x).longValue())

    ///
    /// Convert `x` to an `Option[BigInt]`.
    ///
    /// Returns `Some(x as BigInt)` if the numeric value of `x` is representable as a BigInt.
    ///
    /// Returns `None` if the value of `x` is NaN or infinity.
    ///
    pub def tryToBigInt(x: Float64): Option[BigInt] =
        try {
            Some(unsafe BigDecimal.valueOf(x).toBigInteger())
        } catch {
            // Catch NaN, isInfinite.
            case _: NumberFormatException => None
        }

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    /// Returns `Some(x as Float32)` if the numeric value of `x` is within the range
    /// of Float32, loss of precision may occur.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float32
    /// (i.e. 1.4E-45 to 3.4028235E38).
    ///
    /// If `x` is NaN return `Some(Float32.NaN)``, if `x` is positive or negative infinity return
    /// `Some` wrapping the corresponding Float32 infinity.
    ///
    pub def tryToFloat32(x: Float64): Option[Float32] =
        if (x < Float32.toFloat64(Float32.minValue()) or x > Float32.toFloat64(Float32.maxValue()))
            None
        else
            Some(Double.valueOf(x).floatValue())

    ///
    /// Convert `x` to an `Option[BigDecimal]`.
    ///
    /// Returns `Some(x as BigDecimal)` if the numeric value of `x` is representable
    /// as a BigDecimal value.
    ///
    /// If `x` is NaN or infinity return `None`.
    ///
    pub def tryToBigDecimal(x: Float64): Option[BigDecimal] =
        try {
            Some(unsafe new BigDecimal(x))
        } catch {
            case _: NumberFormatException => None
        }

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(x: Float64, minimum: Float64, maximum: Float64): Float64 =
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
    pub def clampToInt8(x: Float64, minimum: Int8, maximum: Int8, nanValue: Int8): Int8 =
        let minf64 = Int8.toFloat64(minimum);
        let maxf64 = Int8.toFloat64(maximum);
        if (isNan(x))
            nanValue
        else
            Double.valueOf(clamp(x, minf64, maxf64)).byteValue()

    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int16 risks masking it.
    ///
    pub def clampToInt16(x: Float64, minimum: Int16, maximum: Int16, nanValue: Int16): Int16 =
        let minf64 = Int16.toFloat64(minimum);
        let maxf64 = Int16.toFloat64(maximum);
        if (isNan(x))
            nanValue
        else
            Double.valueOf(clamp(x, minf64, maxf64)).shortValue()

    ///
    /// Convert `x` to an `Int32`.
    ///
    /// Returns `x` clamped within the Int32 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int32 risks masking it.
    ///
    pub def clampToInt32(x: Float64, minimum: Int32, maximum: Int32, nanValue: Int32): Int32 =
        let minf64 = Int32.toFloat64(minimum);
        let maxf64 = Int32.toFloat64(maximum);
        if (isNan(x))
            nanValue
        else
            Double.valueOf(clamp(x, minf64, maxf64)).intValue()

    ///
    /// Convert `x` to an `Int64`.
    ///
    /// Returns `x` clamped within the Int64 range `minimum` to `maximum`.
    ///
    /// Warning: it is recommended to test `x` for NaN (not-a-number) before calling this
    /// function. Relying on `nanValue` to convert NaN to a permissable Int64 risks masking it.
    ///
    pub def clampToInt64(x: Float64, minimum: Int64, maximum: Int64, nanValue: Int64): Int64 =
        let minf64 = Int64.toFloat64(minimum);
        let maxf64 = Int64.toFloat64(maximum);
        if (isNan(x))
            nanValue
        else
            Double.valueOf(clamp(x, minf64, maxf64)).longValue()

    ///
    /// Convert `x` to a `Float32`.
    ///
    /// Returns `x` clamped within the Float32 range `minimum` to `maximum`.
    ///
    pub def clampToFloat32(x: Float64, minimum: Float32, maximum: Float32): Float32 =
        let minf64 = Float32.toFloat64(minimum);
        let maxf64 = Float32.toFloat64(maximum);
        if (isNan(x))
            Float32.nan()
        else
            Double.valueOf(clamp(x, minf64, maxf64)).floatValue()

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: Float64): Float64 =
        unsafe Math.abs(x)

    ///
    /// Returns `x` rounded up to a Float64 representing the nearest larger integer value.
    ///
    pub def ceil(x: Float64): Float64 =
        unsafe Math.ceil(x)

    ///
    /// Returns `x` rounded down to a Float64 representing the nearest smaller integer value.
    ///
    pub def floor(x: Float64): Float64 =
        unsafe Math.floor(x)

    ///
    /// Returns `x` rounded to a Float64 representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5f64) == 0.0f64`).
    ///
    pub def round(x: Float64): Float64 =
        unsafe Math.rint(x)

    ///
    /// Returns `x` raised to the power of `n`.
    ///
    pub def pow(x: Float64, n: Float64): Float64 = $FLOAT64_EXP$(x, n)

    ///
    /// Returns the exponential of `x`.
    ///
    pub def exp(x: Float64): Float64 =
        unsafe Math.exp(x)

    ///
    /// Get the primitive Float64 value from its object representation (i.e. java.lang.Double).
    ///
    /// This function is expected to be used when marshaling Float64s from Java. Generally in Flix
    /// code you should not need to use `java.lang.Double`.
    ///
    pub def doubleValue(i: Double): Float64 =
        i.doubleValue()

    ///
    /// Convert an Float64 value to its object representation (i.e. java.lang.Double).
    ///
    /// This function is expected to be used when marshaling Float64s to Java. Generally in Flix
    /// code you should not need to use `java.lang.Double`.
    ///
    pub def valueOf(i: Float64): Double =
        Double.valueOf(i)

}
