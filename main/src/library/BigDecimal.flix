/*
 * Copyright 2022 Stephen Tetley
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

/// Represents arbitrary-precision signed decimal numbers.
/// The representation consists of both a "value" and a "scale". Standard
/// equality (`==` and the `Eq` trait) consider both value and scale,
/// numerical equality (are the values the same after the scales have been
/// normalized) is provided by the function `numericEquals`.
mod BigDecimal {

    import java.math.BigDecimal
    import java.math.RoundingMode
    import java.lang.NumberFormatException

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigDecimal, y: BigDecimal): BigDecimal = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigDecimal, y: BigDecimal): BigDecimal = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigDecimal): BigDecimal =
        unsafely IO run x.abs()

    ///
    /// Returns `x` raised to the power of `n`.
    ///
    pub def pow(x: BigDecimal, n: Int32): BigDecimal =
        unsafely IO run x.pow(n)

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigDecimal, y: BigDecimal): BigDecimal = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigDecimal, y: BigDecimal): Int32 =
        unsafely IO run x.compareTo(y)

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigDecimal): Int32 =
        unsafely IO run x.signum()

    ///
    /// Parse the string `s` as an BigDecimal, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigDecimal] = try {
        Some(unsafely IO run new BigDecimal(s.strip()))
    } catch {
        case _: NumberFormatException => None
    }

    ///
    /// Returns `x` as a "plain string" without an exponent field.
    ///
    pub def toPlainString(x: BigDecimal): String =
        unsafely IO run x.toPlainString()

    ///
    /// Compare `x` and `y` for equality normalizing any difference in scale.
    ///
    pub def numericEquals(x: BigDecimal, y: BigDecimal): Bool =
        unsafely IO run x.compareTo(y) == 0

    ///
    /// Returns the scale of `x`.
    ///
    pub def scale(x: BigDecimal): Int32 =
        unsafely IO run x.scale()

    ///
    /// Returns the precision of `x`.
    ///
    pub def precision(x: BigDecimal): Int32 =
        unsafely IO run x.precision()

    ///
    /// Returns `x` rounded up to a BigDecimal representing the nearest larger integer value.
    ///
    pub def ceil(x: BigDecimal): BigDecimal = unsafely IO run x.setScale(0, RoundingMode.CEILING)

    ///
    /// Returns `x` rounded down to a BigDecimal representing the nearest smaller integer value.
    ///
    pub def floor(x: BigDecimal): BigDecimal = unsafely IO run x.setScale(0, RoundingMode.FLOOR)

    ///
    /// Returns `x` rounded to a BigDecimal representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5ff64) == 0.0ff64`).
    ///
    pub def round(x: BigDecimal): BigDecimal = unsafely IO run x.setScale(0, RoundingMode.HALF_EVEN)

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` is within the range
    /// of Int8, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    pub def tryToInt8(x: BigDecimal): Option[Int8] =
        tryToInt32(x) |> Option.flatMap(Int32.tryToInt8)

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` is within the range
    /// of Int16, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    pub def tryToInt16(x: BigDecimal): Option[Int16] =
        tryToInt32(x) |> Option.flatMap(Int32.tryToInt16)

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` is within the range
    /// of Int32, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647).
    ///
    pub def tryToInt32(x: BigDecimal): Option[Int32] =
        if (x < Int32.toBigDecimal(Int32.minValue()) or x > Int32.toBigDecimal(Int32.maxValue()))
            None
        else
            Some(unsafely IO run x.intValue())

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` is within the range
    /// of Int64, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807).
    ///
    pub def tryToInt64(x: BigDecimal): Option[Int64] =
        if (x < Int64.toBigDecimal(Int64.minValue()) or x > Int64.toBigDecimal(Int64.maxValue()))
            None
        else
            Some(unsafely IO run x.longValue())

    ///
    /// Convert `x` to a `BigInt`.
    ///
    pub def toBigInt(x: BigDecimal): BigInt =
        unsafely IO run x.toBigInteger()

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    /// Returns `Some(x as Float32)` if the numeric value of `x` is within the range
    /// of Float32, loss of precision may occur.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float32
    /// (i.e. 1.4E-45 to 3.4028235E38).
    ///
    pub def tryToFloat32(x: BigDecimal): Option[Float32] =
        let d = unsafely IO run x.floatValue();
        if (Float32.isFinite(d))
            Some(d)
        else
            None

    ///
    /// Convert `x` to an `Option[Float64]`.
    ///
    /// Returns `Some(x as Float64)` if the numeric value of `x` is within the range
    /// of Float64, loss of precision may occur.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float64
    /// (i.e 4.9E-324 to 1.7976931348623157E308).
    ///
    pub def tryToFloat64(x: BigDecimal): Option[Float64] =
        let d = unsafely IO run x.doubleValue();
        if (Float64.isFinite(d))
            Some(d)
        else
            None


}

