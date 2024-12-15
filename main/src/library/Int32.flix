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

instance LowerBound[Int32] {
    pub def minValue(): Int32 = Int32.minValue()
}

instance UpperBound[Int32] {
    pub def maxValue(): Int32 = Int32.maxValue()
}

mod Int32 {

    import java.lang.Integer
    import java.lang.NumberFormatException
    import java.math.BigDecimal
    import java.math.BigInteger

    ///
    /// Returns the number of bits used to represent an `Int32`.
    ///
    pub def size(): Int32 = 32

    ///
    /// Returns the minimum number representable by an `Int32`.
    ///
    pub def minValue(): Int32 = leftShift(1, size() - 1)

    ///
    /// Returns the maximum number representable by an `Int32`.
    ///
    pub def maxValue(): Int32 = bitwiseNot(minValue())

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Int32, y: Int32): Int32 = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    pub def abs(x: Int32): Int32 = {
        if      (x >= 0)            x
        else if (x == minValue())   -1
        else                        -x
    }

    ///
    /// Returns the Euclidean modulo of `x` and `n`.
    /// The result is always non-negative.
    ///
    pub def modulo(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$($INT32_REM$(x, n) + n, n)

    ///
    /// Returns the remainder of `x / n`.
    /// The result can be negative.
    ///
    /// See also `Int32.modulo`.
    ///
    pub def remainder(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$(x, n)

    ///
    /// Returns the distance between `x` and `y`.
    /// If this distance exceeds maxValue(), -1 is returned.
    ///
    pub def dist(x: Int32, y: Int32): Int32 = {
        if      (x >= 0 and y >= 0)                      abs(x - y)
        else if (x < 0 and y < 0)                        abs(x - y)
        else if (x == minValue() or y == minValue())    -1
        else if (minValue() + abs(x) + abs(y) >= 0)     -1
        else                                            abs(x - y)
    }

    ///
    /// Returns 1 if x > y, -1 if x < y, and 0 if x = y.
    /// The sign of x - y.
    ///
    pub def compare(x: Int32, y: Int32): Int32 = {
        if      (x == y)    0
        else if (x < y)     -1
        else                1
    }

    ///
    /// Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
    /// The sign of x.
    ///
    pub def signum(x: Int32): Int32 = compare(x, 0)

    ///
    /// Returns `b` raised to the power of `n`.
    ///
    pub def pow(b: Int32, n: Int32): Int32 = $INT32_EXP$(b, n)

    ///
    /// Returns the logical right shift of `x` by `dist`.
    /// Only the rightmost 5 bits of `dist` are considered (ie. `dist rem 32`).
    /// A zero is shifted into the leftmost position regardless of sign extension.
    ///
    pub def logicalRightShift(dist: {dist = Int32}, x: Int32): Int32 =
        if (x < 0 and remainder(dist#dist, size()) != 0)
            rightShift(bitwiseAnd(rightShift(x, 1), maxValue()), dist#dist - 1)
        else
            rightShift(x, dist#dist)

    ///
    /// Returns the number of one-bits in the two's complement binary
    /// representation of `x`.
    ///
    pub def bitCount(x: Int32): Int32 = {
        if      (x == 0)         0
        else if (x `remainder` 2 != 0) bitCount(logicalRightShift(dist = 1, x)) + 1
        else                           bitCount(logicalRightShift(dist = 1, x))
    }

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` right by `dist` bits.
    ///
    pub def rotateRight(dist: {dist = Int32}, x: Int32): Int32 =
        bitwiseOr(logicalRightShift(dist = dist#dist, x), leftShift(x, -dist#dist))

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` left by `dist` bits.
    ///
    pub def rotateLeft(dist: {dist = Int32}, x: Int32): Int32 =
        bitwiseOr(logicalRightShift(dist = -dist#dist, x), leftShift(x, dist#dist))

    ///
    /// Returns the value obtained by reversing the bits in the
    /// two's complement binary representation of `x`.
    ///
    pub def reverse(x: Int32): Int32 = reverseHelper(x, 0, size()-1)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(x: Int32, l: Int32, r: Int32): Int32 =
        if (l >= r) x else reverseHelper(swap(x, l, r), l+1, r-1)

    ///
    /// Helper function for `reverse`.
    ///
    def swap(x: Int32, l: Int32, r: Int32): Int32 = match (getBit(pos = l, x), getBit(pos = r, x)) {
        case (1, 0) => clearBit(pos = l, setBit(pos = r, x))
        case (0, 1) => clearBit(pos = r, setBit(pos = l, x))
        case _ => x
    }

    ///
    /// Returns the position of the highest-order/leftmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def highestOneBitPosition(x: Int32): Int32 =
        // Start at bit 31 and scan right
        oneBitPositionHelper(x, size() - 1, -1)

    ///
    /// Returns the position of the lowest-order/rightmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def lowestOneBitPosition(x: Int32): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    ///
    /// Helper function for highestOneBitPosition and lowestOneBitPosition.
    /// If `x` has a one-bit at `position`, return that position.
    /// Otherwise recursively check the next bit in the same way.
    ///
    def oneBitPositionHelper(x: Int32, position: Int32, delta: Int32): Int32 = {
        if      (position < 0)                   -1
        else if (position > size() - 1)          -1
        else if (getBit(pos = position, x) == 1) position
        else if (delta == 0)                     -1
        else                                     oneBitPositionHelper(x, position + delta, delta)
    }

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def highestOneBit(x: Int32): Int32 =
        bitPositionToInt32(highestOneBitPosition(x))

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def lowestOneBit(x: Int32): Int32 =
        bitPositionToInt32(lowestOneBitPosition(x))

    ///
    /// Helper function for highestOneBit and lowestOneBit.
    /// Returns a value with a single one-bit at bit number `position`.
    /// Returns 0 if `position` is outside the range 0-31 inclusive.
    ///
    def bitPositionToInt32(position: Int32): Int32 =
        if (position < 0 or position > size() - 1) 0 else leftShift(1, position)

    ///
    /// Returns the number of zero bits preceding the
    /// highest-order/leftmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    pub def numberOfLeadingZeros(x: Int32): Int32 =
        size() - 1 - highestOneBitPosition(x)

    ///
    /// Returns the number of zero bits following the
    /// lowest-order/rightmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    pub def numberOfTrailingZeros(x: Int32): Int32 =
        if (x == 0) 32 else lowestOneBitPosition(x)

    ///
    /// Returns the bit of `x` at position `pos` (either 0 or 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def getBit(pos: {pos = Int32}, x: Int32): Int32 =
        if (rightShift(x, pos#pos) `remainder` 2 == 0) 0 else 1

    ///
    /// Returns `x` with the bit at position `pos` set (to 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def setBit(pos: {pos = Int32}, x: Int32): Int32 = bitwiseOr(x, leftShift(1, pos#pos))

    ///
    /// Returns `x` with the bit at position `pos` cleared (to 0).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def clearBit(pos: {pos = Int32}, x: Int32): Int32 = bitwiseAnd(x, bitwiseNot(leftShift(1, pos#pos)))

    ///
    /// Returns `x` with the bit at position `pos` flipped.
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    pub def flipBit(pos: {pos = Int32}, x: Int32): Int32 = bitwiseXor(x, leftShift(1, pos#pos))

    ///
    /// Returns the integer binary logarithm of `x`.
    /// If the given value is 0 or negative, 0 is returned.
    ///
    pub def log2(x: Int32): Int32 =
        if (x <= 0) {
            0
        } else {
            highestOneBitPosition(x)
        }

    ///
    /// Returns `x` with the bits shifted left by `y` places
    ///
    pub def leftShift(x: Int32, y: Int32): Int32 = $INT32_SHL$(x, y)

    ///
    /// Returns `x` with the bits shifted right by `y` places
    ///
    pub def rightShift(x: Int32, y: Int32): Int32 = $INT32_SHR$(x, y)

    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def bitwiseAnd(x: Int32, y: Int32): Int32 = $INT32_AND$(x, y)

    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def bitwiseNot(x: Int32): Int32 = $INT32_NOT$(x)

    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def bitwiseOr(x: Int32, y: Int32): Int32 = $INT32_OR$(x, y)

    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def bitwiseXor(x: Int32, y: Int32): Int32 = $INT32_XOR$(x, y)

    ///
    /// Returns the factorial of `x`.
    /// If the given value is negative, 0 is returned.
    ///
    pub def factorial(x: Int32): Int32 =
        if (x < 0) {
            0
        } else {
            def loop(y, acc) = match y {
                case 0 => acc
                case _ => loop(y - 1, y * acc)
            };
            loop(x, 1)
        }

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Int32): String = ToString.toString(x)

    ///
    /// Parse the string `s` as an Int32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Int32] = try {
        Some(unsafely IO run Integer.parseInt(s.strip()))
    } catch {
        case _: NumberFormatException => None
    }

    ///
    /// Parse the string `s` as an Int32, where the `radix` is used while parsing.
    /// Leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Ok(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def parse(radix: Int32, s: String): Result[String, Int32] =
        try {
            Ok(unsafely IO run Integer.parseInt(s.strip(), radix))
        } catch {
            case _: NumberFormatException => Err("Int32.parse")
        }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    pub def tryToInt8(x: Int32): Option[Int8] =
       if (x < Int8.toInt32(Int8.minValue()) or x > Int8.toInt32(Int8.maxValue()))
            None
        else
            Some(unsafely IO run Integer.valueOf(x).byteValue())

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    pub def tryToInt16(x: Int32): Option[Int16] =
        if (x < Int16.toInt32(Int16.minValue()) or x > Int16.toInt32(Int16.maxValue()))
            None
        else
            Some(unsafely IO run Integer.valueOf(x).shortValue())

    ///
    /// Convert `x` to a Int64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toInt64(x: Int32): Int64 =
        unsafely IO run Integer.valueOf(x).longValue()

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigInt(x: Int32): BigInt =
        unsafely IO run BigInteger.valueOf(Integer.valueOf(x).longValue())

    ///
    /// Convert `x` to an Float32.
    ///
    /// The numeric value of `x` may lose precision.
    ///
    pub def toFloat32(x: Int32): Float32 =
        unsafely IO run Integer.valueOf(x).floatValue()

    ///
    /// Convert `x` to a Float64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toFloat64(x: Int32): Float64 =
        unsafely IO run Integer.valueOf(x).doubleValue()

    ///
    /// Convert `x` to a BigDecimal.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int32): BigDecimal =
        unsafely IO run new BigDecimal(x)

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(min: {min = Int32}, max: {max = Int32}, x: Int32): Int32 =
        if (x < min#min)
             min#min
        else
            if (x > max#max)
                max#max
            else
                x

    ///
    /// Convert `x` to an `Int8`.
    ///
    /// Returns `x` clamped within the Int8 range `min` to `max`.
    ///
    pub def clampToInt8(min: {min = Int8}, max: {max = Int8}, x: Int32): Int8 =
        let mini32 = Int8.toInt32(min#min);
        let maxi32 = Int8.toInt32(max#max);
        unsafely IO run Integer.valueOf(clamp(min = mini32, max = maxi32, x)).byteValue()


    ///
    /// Convert `x` to an `Int16`.
    ///
    /// Returns `x` clamped within the Int16 range `min` to `max`.
    ///
    pub def clampToInt16(min: {min = Int16}, max: {max = Int16}, x: Int32): Int16 =
        let mini32 = Int16.toInt32(min#min);
        let maxi32 = Int16.toInt32(max#max);
        unsafely IO run Integer.valueOf(clamp(min = mini32, max = maxi32, x)).shortValue()

    ///
    /// Get the primitive Int32 value from its object representation (i.e. Integer).
    ///
    /// This function is expected to be used when marshaling Int32s from Java. Generally in Flix
    /// code you should not need to use `Integer`.
    ///
    pub def intValue(i: Integer): Int32 =
        unsafely IO run i.intValue()

    ///
    /// Convert an Int32 value to its object representation (i.e. Integer).
    ///
    /// This function is expected to be used when marshaling Int32s to Java. Generally in Flix
    /// code you should not need to use `Integer`.
    ///
    pub def valueOf(i: Int32): Integer =
        unsafely IO run Integer.valueOf(i)

}
