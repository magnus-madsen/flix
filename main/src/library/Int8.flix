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

instance LowerBound[Int8] {
    pub def minValue(): Int8 = Int8.minValue()
}

instance UpperBound[Int8] {
    pub def maxValue(): Int8 = Int8.maxValue()
}

mod Int8 {

    import java.lang.Byte
    import java.lang.NumberFormatException
    import java.math.BigDecimal
    import java.math.BigInteger

    ///
    /// Returns the number of bits used to represent an `Int8`.
    ///
    pub def size(): Int32 = 8

    ///
    /// Returns the minimum number representable by an `Int8`.
    ///
    pub def minValue(): Int8 = leftShift(1i8, size() - 1)

    ///
    /// Returns the maximum number representable by an `Int8`.
    ///
    pub def maxValue(): Int8 = bitwiseNot(minValue())

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: Int8, y: Int8): Int8 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: Int8, y: Int8): Int8 = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    pub def abs(x: Int8): Int8 = {
        if      (x >= 0i8)          x
        else if (x == minValue())   -1i8
        else                        -x
    }

    ///
    /// Returns the Euclidean modulo of `x` and `n`.
    /// The result is always non-negative.
    ///
    pub def modulo(x: Int8, n: Int8): Int8 =
        if (n == 0i8)
            0i8
        else
            $INT8_REM$($INT8_REM$(x, n) + n, n)

    ///
    /// Returns the remainder of `x / n`.
    /// The result can be negative.
    ///
    /// See also `Int8.modulo`.
    ///
    pub def remainder(x: Int8, n: Int8): Int8 =
        if (n == 0i8)
            0i8
        else
            $INT8_REM$(x, n)

    ///
    /// Returns the distance between `x` and `y`.
    /// If this distance exceeds maxValue(), -1 is returned.
    ///
    pub def dist(x: Int8, y: Int8): Int8 = {
        if      (x >= 0i8 and y >= 0i8)                  abs(x - y)
        else if (x < 0i8 and y < 0i8)                    abs(x - y)
        else if (x == minValue() or y == minValue())    -1i8
        else if (minValue() + abs(x) + abs(y) >= 0i8)   -1i8
        else                                            abs(x - y)
    }

    ///
    /// Returns 1 if x > y, -1 if x < y, and 0 if x = y.
    /// The sign of x - y.
    ///
    pub def compare(x: Int8, y: Int8): Int32 = {
        if      (x == y)    0
        else if (x < y)     -1
        else                1
    }

    ///
    /// Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
    /// The sign of x.
    ///
    pub def signum(x: Int8): Int32 = compare(x, 0i8)

    ///
    /// Returns `b` raised to the power of `n`.
    ///
    pub def pow(b: Int8, n: Int8): Int8 = $INT8_EXP$(b, n)


    ///
    /// Returns the logical right shift of `x` by `distance`.
    /// Only the rightmost 5 bits of `distance` are considered (ie. `distance rem 32`).
    /// A zero is shifted into the leftmost position regardless of sign extension.
    ///
    pub def logicalRightShift(dist: {dist = Int32}, x: Int8): Int8 =
        if (x < 0i8 and Int32.remainder(dist#dist, 32) != 0)
            rightShift(bitwiseAnd(rightShift(x, 1), maxValue()), dist#dist - 1)
        else
            rightShift(x, dist#dist)

    ///
    /// Returns the number of one-bits in the two's complement binary
    /// representation of `x`.
    ///
    pub def bitCount(x: Int8): Int32 = {
        if      (x == 0i8)           0
        else if (x `remainder` 2i8 != 0i8) bitCount(logicalRightShift(dist = 1, x)) + 1
        else                         bitCount(logicalRightShift(dist = 1, x))
    }

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` right by `distance` bits.
    ///
    pub def rotateRight(dist: {dist = Int32}, x: Int8): Int8 =
        let rem1 = dist#dist `Int32.remainder` size();
        let rot = if (rem1 >= 0) rem1 else rem1 + size();
        bitwiseOr(logicalRightShift(dist = rot, x), leftShift(x, size() - rot))

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` left by `distance` bits.
    ///
    pub def rotateLeft(dist: {dist = Int32}, x: Int8): Int8 =
        let rem1 = dist#dist `Int32.remainder` size();
        let rot = if (rem1 >= 0) rem1 else rem1 + size();
        bitwiseOr(logicalRightShift(dist = size() - rot, x), leftShift(x, rot))

    ///
    /// Returns the value obtained by reversing the bits in the
    /// two's complement binary representation of `x`.
    ///
    pub def reverse(x: Int8): Int8 = reverseHelper(x, 0, size()-1)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(x: Int8, l: Int32, r: Int32): Int8 =
        if (l >= r) x else reverseHelper(swap(x, l, r), l+1, r-1)

    ///
    /// Helper function for `reverse`.
    ///
    def swap(x: Int8, l: Int32, r: Int32): Int8 = match (getBit(pos = l, x), getBit(pos = r, x)) {
        case (1, 0) => clearBit(pos = l, setBit(pos = r, x))
        case (0, 1) => clearBit(pos = r, setBit(pos = l, x))
        case _ => x
    }

    ///
    /// Returns the position of the highest-order/leftmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 7 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def highestOneBitPosition(x: Int8): Int32 =
        // Start at bit 7 and scan right
        oneBitPositionHelper(x, size() - 1, -1)

    ///
    /// Returns the position of the lowest-order/rightmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 7 (leftmost bit)
    ///                         -1 if x = 0
    ///
    pub def lowestOneBitPosition(x: Int8): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    ///
    /// Helper function for highestOneBitPosition and lowestOneBitPosition.
    /// If `x` has a one-bit at `position`, return that position.
    /// Otherwise recursively check the next bit in the same way.
    ///
    def oneBitPositionHelper(x: Int8, position: Int32, delta: Int32): Int32 = {
        if      (position < 0)                   -1
        else if (position > (size() - 1))        -1
        else if (getBit(pos = position, x) == 1) position
        else if (delta == 0)                     -1
        else                                     oneBitPositionHelper(x, position + delta, delta)
    }

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def highestOneBit(x: Int8): Int8 =
        bitPositionToInt8(highestOneBitPosition(x))

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    pub def lowestOneBit(x: Int8): Int8 =
        bitPositionToInt8(lowestOneBitPosition(x))

    ///
    /// Helper function for highestOneBit and lowestOneBit.
    /// Returns a value with a single one-bit at bit number `position`.
    /// Returns 0 if `position` is outside the range 0-7 inclusive.
    ///
    def bitPositionToInt8(position: Int32): Int8 =
        if (position < 0 or position > size() - 1) 0i8 else leftShift(1i8, position)

    ///
    /// Returns the number of zero bits preceding the
    /// highest-order/leftmost one-bit in `x`.
    /// Returns 8 if x=0.
    ///
    pub def numberOfLeadingZeros(x: Int8): Int32 =
        if (x == 0i8) size() else size() - 1 - highestOneBitPosition(x)

    ///
    /// Returns the number of zero bits following the
    /// lowest-order/rightmost one-bit in `x`.
    /// Returns 8 if x=0.
    ///
    pub def numberOfTrailingZeros(x: Int8): Int32 =
        if (x == 0i8) size() else lowestOneBitPosition(x)

    ///
    /// Returns the bit of `x` at `pos` (either 0 or 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    pub def getBit(pos: {pos = Int32}, x: Int8): Int32 =
        if (rightShift(x, pos#pos) `remainder` 2i8 == 0i8) 0 else 1

    ///
    /// Returns `x` with the bit at position `pos` set (to 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    pub def setBit(pos: {pos = Int32}, x: Int8): Int8 = bitwiseOr(x, leftShift(1i8, pos#pos))

    ///
    /// Returns `x` with the bit at position `pos` cleared (to 0).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    pub def clearBit(pos: {pos = Int32}, x: Int8): Int8 = bitwiseAnd(x, bitwiseNot(leftShift(1i8, pos#pos)))

    ///
    /// Returns `x` with the bit at position `pos` flipped.
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 7 (leftmost bit)
    ///
    pub def flipBit(pos: {pos = Int32}, x: Int8): Int8 = bitwiseXor(x, leftShift(1i8, pos#pos))

    ///
    /// Returns the integer binary logarithm of `x`.
    /// If the given value is 0 or negative, 0 is returned.
    ///
    pub def log2(x: Int8): Int8 =
        if (x <= 0i8) {
            0i8
        } else {
            let position = highestOneBitPosition(x);
            Int32.clampToInt8(min = 0i8, max = Int8.maxValue(), position)
        }

    ///
    /// Returns `x` with the bits shifted left by `y` places
    ///
    pub def leftShift(x: Int8, y: Int32): Int8 = $INT8_SHL$(x, y)

    ///
    /// Returns `x` with the bits shifted right by `y` places
    ///
    pub def rightShift(x: Int8, y: Int32): Int8 = $INT8_SHR$(x, y)

    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def bitwiseAnd(x: Int8, y: Int8): Int8 = $INT8_AND$(x, y)

    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def bitwiseNot(x: Int8): Int8 = $INT8_NOT$(x)

    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def bitwiseOr(x: Int8, y: Int8): Int8 = $INT8_OR$(x, y)

    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def bitwiseXor(x: Int8, y: Int8): Int8 = $INT8_XOR$(x, y)

    ///
    /// Returns the factorial of `x`.
    ///
    pub def factorial(x: Int8): Int32 = toInt32(x) |> Int32.factorial

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: Int8): String = ToString.toString(x)

    ///
    /// Parse the string `s` as an Int8, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[Int8] = try {
        Some(unsafely IO run Byte.parseByte(s.strip()))
    } catch {
        case _: NumberFormatException => None
    }

    ///
    /// Convert `x` to an Int16.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toInt16(x: Int8): Int16 =
        unsafely IO run Byte.valueOf(x).shortValue()

    ///
    /// Convert `x` to an Int32.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toInt32(x: Int8): Int32 =
        unsafely IO run Byte.valueOf(x).intValue()

    ///
    /// Convert `x` to an Int64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toInt64(x: Int8): Int64 =
        unsafely IO run Byte.valueOf(x).longValue()

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigInt(x: Int8): BigInt =
        unsafely IO run BigInteger.valueOf(Byte.valueOf(x).longValue())

    ///
    /// Convert `x` to a Float32.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toFloat32(x: Int8): Float32 =
        unsafely IO run Byte.valueOf(x).floatValue()

    ///
    /// Convert `x` to a Float64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toFloat64(x: Int8): Float64 =
        unsafely IO run Byte.valueOf(x).doubleValue()

    ///
    /// Convert `x` to a BigDecimal.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    pub def toBigDecimal(x: Int8): BigDecimal =
        unsafely IO run new BigDecimal(Int8.toInt32(x))

    ///
    /// Get the primitive Int8 value from its object representation (i.e. Byte).
    ///
    /// This function is expected to be used when marshaling Int8s from Java. Generally in Flix
    /// code you should not need to use `Byte`.
    ///
    pub def byteValue(i: Byte): Int8 =
        unsafely IO run i.byteValue()

    ///
    /// Convert an Int8 value to its object representation (i.e. Byte).
    ///
    /// This function is expected to be used when marshaling Int8s to Java. Generally in Flix
    /// code you should not need to use `Byte`.
    ///
    pub def valueOf(i: Int8): Byte =
        unsafely IO run Byte.valueOf(i)

}
