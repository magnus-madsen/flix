/*
 * Copyright 2017 Liam Palmer
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

mod TestInt64 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mul01(): Bool = mul(1i64, 0i64) == 0i64

    @test
    def mul02(): Bool = mul(0i64, 1i64) == 0i64

    @test
    def mul03(): Bool = mul(0i64, 0i64) == 0i64

    @test
    def mul04(): Bool = mul(-32i64, 288230376151711744i64) == -9223372036854775808i64

    @test
    def mul05(): Bool = mul(6223i64, 1482142380982609i64) == 9223372036854775807i64

    @test
    def mul06(): Bool = mul(5i64, 11i64) == 55i64

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Int64.maxValue() == Int64.bitwiseNot(Int64.leftShift(1i64, 63))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Int64.minValue() == Int64.leftShift(1i64, 63)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Int64.size() == 64

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Int64.min(1i64, 2i64) == 1i64

    @test
    def min02(): Bool = Int64.min(2i64, -1i64) == -1i64

    @test
    def min03(): Bool = Int64.min(-33i64, -66i64) == -66i64

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Int64.max(48i64, 49i64) == 49i64

    @test
    def max02(): Bool = Int64.max(4i64, -16i64) == 4i64

    @test
    def max03(): Bool = Int64.max(-34i64, -16i64) == -16i64

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def abs01(): Bool = Int64.abs(63i64) == 63i64

    @test
    def abs02(): Bool = Int64.abs(-63i64) == 63i64

    @test
    def abs03(): Bool = Int64.abs(9223372036854775807i64) == 9223372036854775807i64

    @test
    def abs04(): Bool = Int64.abs(-9223372036854775807i64) == 9223372036854775807i64

    @test
    def abs05(): Bool = Int64.abs(0i64) == 0i64

    @test
    def abs06(): Bool = Int64.abs(-9223372036854775808i64) == -1i64

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def modulo01(): Bool = (120000000000i64 `Int64.modulo` 20000000000i64) == 0i64

    @test
    def modulo02(): Bool = (120000000000i64 `Int64.modulo` 50000000000i64) == 20000000000i64

    @test
    def modulo03(): Bool = (-120000000000i64 `Int64.modulo` 50000000000i64) == 30000000000i64

    @test
    def modulo04(): Bool = (120000000000i64 `Int64.modulo` -50000000000i64) == -30000000000i64

    @test
    def modulo05(): Bool = (-120000000000i64 `Int64.modulo` -50000000000i64) == -20000000000i64

    @test
    def modulo06(): Bool = (1i64 `Int64.modulo` 0i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def remainder01(): Bool = (120000000000i64 `Int64.remainder` 20000000000i64) == 0i64

    @test
    def remainder02(): Bool = (120000000000i64 `Int64.remainder` 50000000000i64) == 20000000000i64

    @test
    def remainder03(): Bool = (-120000000000i64 `Int64.remainder` 50000000000i64) == -20000000000i64

    @test
    def remainder04(): Bool = (120000000000i64 `Int64.remainder` -50000000000i64) == 20000000000i64

    @test
    def remainder05(): Bool = (-120000000000i64 `Int64.remainder` -50000000000i64) == -20000000000i64

    @test
    def remainder06(): Bool = (1i64 `Int64.remainder` 0i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def dist01(): Bool = Int64.dist(31i64, -7i64) == 38i64

    @test
    def dist02(): Bool = Int64.dist(-44i64, -1i64) == 43i64

    @test
    def dist03(): Bool = Int64.dist(-2i64, -2i64) == 0i64

    @test
    def dist04(): Bool = Int64.dist(-9223372036854775808i64, -1i64) == 9223372036854775807i64

    @test
    def dist05(): Bool = Int64.dist(-9223372036854775808i64, 0i64) == -1i64

    @test
    def dist06(): Bool = Int64.dist(-123i64, 4i64) == 127i64

    @test
    def dist07(): Bool = Int64.dist(-9223372036854775803i64, 5i64) == -1i64

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = Int64.compare(-1i64, 44i64) == -1

    @test
    def compare02(): Bool = Int64.compare(-1i64, -44i64) == 1

    @test
    def compare03(): Bool = Int64.compare(88i64, 88i64) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = Int64.signum(-22i64) == -1

    @test
    def signum02(): Bool = Int64.signum(22i64) == 1

    @test
    def signum03(): Bool = Int64.signum(0i64) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def pow01(): Bool = Int64.pow(0i64, 0i64) == 1i64

    @test
    def pow02(): Bool = Int64.pow(0i64, 1i64) == 0i64

    @test
    def pow03(): Bool = Int64.pow(1i64, 0i64) == 1i64

    @test
    def pow04(): Bool = Int64.pow(1i64, 1i64) == 1i64

    @test
    def pow05(): Bool = Int64.pow(5i64, 2i64) == 25i64

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseLeftShift01(): Bool = Int64.leftShift(8i64, 0) == 8i64

    @test
    def testBitwiseLeftShift02(): Bool = Int64.leftShift(8i64, 32) == 34359738368i64

    @test
    def testBitwiseLeftShift03(): Bool = Int64.leftShift(8i64, 60) == -9223372036854775808i64

    @test
    def testBitwiseLeftShift04(): Bool = Int64.leftShift(8i64, 61) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseRightShift01(): Bool = Int64.rightShift(12000000000i64, 0) == 12000000000i64

    @test
    def testBitwiseRightShift02(): Bool = Int64.rightShift(12000000000i64, 2) == 3000000000i64

    @test
    def testBitwiseRightShift03(): Bool = Int64.rightShift(12000000000i64, 63) == 0i64

    @test
    def testBitwiseRightShift04(): Bool = Int64.rightShift(-12000000000i64, 2) == -3000000000i64

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def logicalRightShift01(): Bool = Int64.logicalRightShift(dist = 4, 2233i64) == Int64.rightShift(2233i64, 4)

    @test
    def logicalRightShift02(): Bool = Int64.logicalRightShift(dist = 33, 22336688888i64) == Int64.rightShift(22336688888i64, 33)

    @test
    def logicalRightShift03(): Bool = Int64.logicalRightShift(dist = -8, 2233i64) == Int64.rightShift(2233i64, -8)

    @test
    def logicalRightShift04(): Bool = Int64.logicalRightShift(dist = 4, 2233i64) == Int64.rightShift(2233i64, 4)

    @test
    def logicalRightShift05(): Bool = Int64.logicalRightShift(dist = 1, -1i64) == 9223372036854775807i64

    @test
    def logicalRightShift06(): Bool = Int64.logicalRightShift(dist = 4, -35i64) == 1152921504606846973i64

    @test
    def logicalRightShift07(): Bool = Int64.logicalRightShift(dist = -2777, -2777i64) == 33554431i64

    @test
    def logicalRightShift08(): Bool = Int64.logicalRightShift(dist = 64, -23328384858i64) == -23328384858i64

    @test
    def logicalRightShift09(): Bool = Int64.logicalRightShift(dist = 0, -27000000000000i64) == -27000000000000i64

    @test
    def logicalRightShift10(): Bool = Int64.logicalRightShift(dist = 1, 9223372036854775807i64) == 4611686018427387903i64

    @test
    def logicalRightShift11(): Bool = Int64.logicalRightShift(dist = 1, -9223372036854775808i64) == 4611686018427387904i64

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def bitCount01(): Bool = Int64.bitCount(1990911889900134i64) == 25

    @test
    def bitCount02(): Bool = Int64.bitCount(-1i64) == 64

    @test
    def bitCount03(): Bool = Int64.bitCount(-999888888114322199i64) == 35

    @test
    def bitCount04(): Bool = Int64.bitCount(0i64) == 0

    @test
    def bitCount05(): Bool = Int64.bitCount(-9223372036854775808i64) == 1

    @test
    def bitCount06(): Bool = Int64.bitCount(-9223372036854775805i64) == 3

    @test
    def bitCount07(): Bool = Int64.bitCount(9223372036854775807i64) == 63

    @test
    def bitCount08(): Bool = Int64.bitCount(9223372036854775806i64) == 62

    @test
    def bitCount09(): Bool = Int64.bitCount(9223372036854775791i64) == 62

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateLeft01(): Bool = Int64.rotateLeft(dist = 7, -99991001265258663i64) == 5647895911756442879i64

    @test
    def rotateLeft02(): Bool = Int64.rotateLeft(dist = 24, -983667729064i64) == 1943538110990123007i64

    @test
    def rotateLeft03(): Bool = Int64.rotateLeft(dist = -104, -983667729064i64) == 1943538110990123007i64

    @test
    def rotateLeft04(): Bool = Int64.rotateLeft(dist = 23, 1478293001i64) == 12400820494532608i64

    @test
    def rotateLeft05(): Bool = Int64.rotateLeft(dist = -41, 1478293001i64) == 12400820494532608i64

    @test
    def rotateLeft06(): Bool = Int64.rotateLeft(dist = 87, 1478293001i64) == 12400820494532608i64

    @test
    def rotateLeft07(): Bool = Int64.rotateLeft(dist = 87, 0i64) == 0i64

    @test
    def rotateLeft08(): Bool = Int64.rotateLeft(dist = 87, -1i64) == -1i64

    @test
    def rotateLeft09(): Bool = Int64.rotateLeft(dist = 3, -9223372036854775808i64) == 4i64

    @test
    def rotateLeft10(): Bool = Int64.rotateLeft(dist = 2, 9223372036854775807i64) == -3i64

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateRight01(): Bool = Int64.rotateRight(dist = 57, -99991001265258663i64) == 5647895911756442879i64

    @test
    def rotateRight02(): Bool = Int64.rotateRight(dist = 40, -983667729064i64) == 1943538110990123007i64

    @test
    def rotateRight03(): Bool = Int64.rotateRight(dist = -88, -983667729064i64) == 1943538110990123007i64

    @test
    def rotateRight04(): Bool = Int64.rotateRight(dist = 41, 1478293001i64) == 12400820494532608i64

    @test
    def rotateRight05(): Bool = Int64.rotateRight(dist = -23, 1478293001i64) == 12400820494532608i64

    @test
    def rotateRight06(): Bool = Int64.rotateRight(dist = 105, 1478293001i64) == 12400820494532608i64

    @test
    def rotateRight07(): Bool = Int64.rotateRight(dist = 87, 0i64) == 0i64

    @test
    def rotateRight08(): Bool = Int64.rotateRight(dist = 87, -1i64) == -1i64

    @test
    def rotateRight09(): Bool = Int64.rotateRight(dist = 3, -9223372036854775808i64) == 1152921504606846976i64

    @test
    def rotateRight10(): Bool = Int64.rotateRight(dist = 2, 9223372036854775807i64) == -2305843009213693953i64

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def reverse01(): Bool = Int64.reverse(5093163785157044906i64) == 6149336169419208034i64

    @test
    def reverse02(): Bool = Int64.reverse(6149336169419208034i64) == 5093163785157044906i64

    @test
    def reverse03(): Bool = Int64.reverse(8455181377513485674i64) == 6245943384048626350i64

    @test
    def reverse04(): Bool = Int64.reverse(6245943384048626350i64) == 8455181377513485674i64

    @test
    def reverse05(): Bool = Int64.reverse(-9223372036854775808i64) == 1i64

    @test
    def reverse06(): Bool = Int64.reverse(1i64) == -9223372036854775808i64

    @test
    def reverse07(): Bool = Int64.reverse(4i64) == 2305843009213693952i64

    @test
    def reverse08(): Bool = Int64.reverse(2305843009213693952i64) == 4i64

    @test
    def reverse09(): Bool = Int64.reverse(-1i64) == -1i64

    @test
    def reverse10(): Bool = Int64.reverse(0i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBitPosition01(): Bool = Int64.highestOneBitPosition(-1i64) == 63

    @test
    def highestOneBitPosition02(): Bool = Int64.highestOneBitPosition(-9223372036854775808i64) == 63

    @test
    def highestOneBitPosition03(): Bool = Int64.highestOneBitPosition(1i64) == 0

    @test
    def highestOneBitPosition04(): Bool = Int64.highestOneBitPosition(2i64) == 1

    @test
    def highestOneBitPosition05(): Bool = Int64.highestOneBitPosition(3i64) == 1

    @test
    def highestOneBitPosition06(): Bool = Int64.highestOneBitPosition(20i64) == 4

    @test
    def highestOneBitPosition07(): Bool = Int64.highestOneBitPosition(Int64.pow(2i64, 30i64)) == 30

    @test
    def highestOneBitPosition08(): Bool = Int64.highestOneBitPosition(9223372036854775807i64) == 62

    @test
    def highestOneBitPosition09(): Bool = Int64.highestOneBitPosition(0i64) == -1

    @test
    def highestOneBitPosition10(): Bool = Int64.highestOneBitPosition(Int64.pow(2i64, 23i64) + Int64.pow(2i64, 60i64)) == 60

    @test
    def highestOneBitPosition11(): Bool = Int64.highestOneBitPosition(Int64.pow(2i64, 20i64) + Int64.pow(2i64, 59i64)) == 59

    @test
    def highestOneBitPosition12(): Bool = Int64.highestOneBitPosition(-44556677i64) == 63

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBit01(): Bool = Int64.highestOneBit(-1i64) == Int64.leftShift(1i64, 63)

    @test
    def highestOneBit02(): Bool = Int64.highestOneBit(-9223372036854775808i64) == Int64.leftShift(1i64, 63)

    @test
    def highestOneBit03(): Bool = Int64.highestOneBit(-778899i64) == Int64.leftShift(1i64, 63)

    @test
    def highestOneBit04(): Bool = Int64.highestOneBit(2i64) == 2i64

    @test
    def highestOneBit05(): Bool = Int64.highestOneBit(3i64) == 2i64

    @test
    def highestOneBit06(): Bool = Int64.highestOneBit(20i64) == 16i64

    @test
    def highestOneBit07(): Bool = Int64.highestOneBit(Int64.pow(2i64, 57i64) + Int64.pow(2i64, 32i64)) == 144115188075855872i64

    @test
    def highestOneBit08(): Bool = Int64.highestOneBit(9223372036854775807i64) == 4611686018427387904i64

    @test
    def highestOneBit09(): Bool = Int64.highestOneBit(0i64) == 0i64

    @test
    def highestOneBit10(): Bool = Int64.highestOneBit(Int64.pow(2i64, 44i64) + Int64.pow(2i64, 18i64)) == 17592186044416i64

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfLeadingZeros01(): Bool = Int64.numberOfLeadingZeros(0i64) == 64

    @test
    def numberOfLeadingZeros02(): Bool = Int64.numberOfLeadingZeros(-9223372036854775808i64) == 0

    @test
    def numberOfLeadingZeros03(): Bool = Int64.numberOfLeadingZeros(-1i64) == 0

    @test
    def numberOfLeadingZeros04(): Bool = Int64.numberOfLeadingZeros(Int64.bitwiseNot(Int64.leftShift(1i64, 63))) == 1

    @test
    def numberOfLeadingZeros05(): Bool = Int64.numberOfLeadingZeros(Int64.pow(2i64, 62i64)) == 1

    @test
    def numberOfLeadingZeros06(): Bool = Int64.numberOfLeadingZeros(Int64.pow(2i64, 61i64) + Int64.pow(2i64, 18i64)) == 2

    @test
    def numberOfLeadingZeros07(): Bool = Int64.numberOfLeadingZeros(Int64.pow(2i64, 20i64) + Int64.pow(2i64, 18i64)) == 43

    @test
    def numberOfLeadingZeros08(): Bool = Int64.numberOfLeadingZeros(20i64) == 59

    @test
    def numberOfLeadingZeros09(): Bool = Int64.numberOfLeadingZeros(1i64) == 63

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBitPosition01(): Bool = Int64.lowestOneBitPosition(-1i64) == 0

    @test
    def lowestOneBitPosition02(): Bool = Int64.lowestOneBitPosition(-9223372036854775808i64) == 63

    @test
    def lowestOneBitPosition03(): Bool = Int64.lowestOneBitPosition(1i64) == 0

    @test
    def lowestOneBitPosition04(): Bool = Int64.lowestOneBitPosition(2i64) == 1

    @test
    def lowestOneBitPosition05(): Bool = Int64.lowestOneBitPosition(3i64) == 0

    @test
    def lowestOneBitPosition06(): Bool = Int64.lowestOneBitPosition(4i64) == 2

    @test
    def lowestOneBitPosition07(): Bool = Int64.lowestOneBitPosition(12i64) == 2

    @test
    def lowestOneBitPosition08(): Bool = Int64.lowestOneBitPosition(Int64.pow(2i64, 30i64)) == 30

    @test
    def lowestOneBitPosition09(): Bool = Int64.lowestOneBitPosition(Int64.pow(2i64, 57i64) + Int64.pow(2i64, 18i64)) == 18

    @test
    def lowestOneBitPosition10(): Bool = Int64.lowestOneBitPosition(9223372036854775807i64) == 0

    @test
    def lowestOneBitPosition11(): Bool = Int64.lowestOneBitPosition(0i64) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBit01(): Bool = Int64.lowestOneBit(-1i64) == 1i64

    @test
    def lowestOneBit02(): Bool = Int64.lowestOneBit(-9223372036854775808i64) == Int64.leftShift(1i64, 63)

    @test
    def lowestOneBit03(): Bool = Int64.lowestOneBit(1i64) == 1i64

    @test
    def lowestOneBit04(): Bool = Int64.lowestOneBit(2i64) == 2i64

    @test
    def lowestOneBit05(): Bool = Int64.lowestOneBit(6i64) == 2i64

    @test
    def lowestOneBit06(): Bool = Int64.lowestOneBit(12i64) == 4i64

    @test
    def lowestOneBit07(): Bool = Int64.lowestOneBit(Int64.pow(2i64, 6i64) + Int64.pow(2i64, 18i64) + Int64.pow(2i64, 27i64)) == 64i64

    @test
    def lowestOneBit08(): Bool = Int64.lowestOneBit(Int64.pow(2i64, 54i64)) == 18014398509481984i64

    @test
    def lowestOneBit09(): Bool = Int64.lowestOneBit(Int64.pow(2i64, 62i64) + Int64.pow(2i64, 18i64)) == 262144i64

    @test
    def lowestOneBit10(): Bool = Int64.lowestOneBit(9223372036854775807i64) == 1i64

    @test
    def lowestOneBit11(): Bool = Int64.lowestOneBit(0i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfTrailingZeros01(): Bool = Int64.numberOfTrailingZeros(0i64) == 64

    @test
    def numberOfTrailingZeros02(): Bool = Int64.numberOfTrailingZeros(3i64) == 0

    @test
    def numberOfTrailingZeros03(): Bool = Int64.numberOfTrailingZeros(42i64) == 1

    @test
    def numberOfTrailingZeros04(): Bool = Int64.numberOfTrailingZeros(Int64.pow(2i64, 16i64) + Int64.pow(2i64, 22i64)) == 16

    @test
    def numberOfTrailingZeros05(): Bool = Int64.numberOfTrailingZeros(Int64.pow(2i64, 57i64)) == 57

    @test
    def numberOfTrailingZeros06(): Bool = Int64.numberOfTrailingZeros(-1i64) == 0

    @test
    def numberOfTrailingZeros07(): Bool = Int64.numberOfTrailingZeros(-9223372036854775808i64) == 63

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getBit01(): Bool = Int64.getBit(pos = 5, -1i64) == 1

    @test
    def getBit02(): Bool = Int64.getBit(pos = 63, -1000000i64) == 1

    @test
    def getBit03(): Bool = Int64.getBit(pos = 59, Int64.pow(2i64, 59i64)) == 1

    @test
    def getBit04(): Bool = Int64.getBit(pos = 60, Int64.pow(2i64, 59i64)) == 0

    @test
    def getBit05(): Bool = Int64.getBit(pos = 58, Int64.pow(2i64, 59i64)) == 0

    @test
    def getBit06(): Bool = Int64.getBit(pos = 61, -1i64 - Int64.pow(2i64, 61i64)) == 0

    @test
    def getBit07(): Bool = Int64.getBit(pos = 60, -1i64 - Int64.pow(2i64, 61i64)) == 1

    @test
    def getBit08(): Bool = Int64.getBit(pos = 55, 0i64) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def setBit01(): Bool = Int64.setBit(pos = 0, 0i64) == 1i64

    @test
    def setBit02(): Bool = Int64.setBit(pos = 1, 0i64) == 2i64

    @test
    def setBit03(): Bool = Int64.setBit(pos = 31, 0i64) == 2147483648i64

    @test
    def setBit04(): Bool = Int64.setBit(pos = 32, 0i64) == 4294967296i64

    @test
    def setBit05(): Bool = Int64.setBit(pos = 31, -1418638422666i64) == -1418638422666i64

    @test
    def setBit07(): Bool = Int64.setBit(pos = 4, -17i64) == -1i64

    @test
    def setBit08(): Bool = Int64.setBit(pos = 10, -1025i64) == -1i64

    @test
    def setBit09(): Bool = Int64.setBit(pos = 63, -17i64) == -17i64

    @test
    def setBit10(): Bool = Int64.setBit(pos = 30, -17i64) == -17i64

    @test
    def setBit11(): Bool = Int64.setBit(pos = 63, 0i64) == Int64.leftShift(1i64, 63)

    @test
    def setBit12(): Bool = Int64.setBit(pos = 64, 0i64) == 1i64

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clearBit01(): Bool = Int64.clearBit(pos = 0, 1i64) == 0i64

    @test
    def clearBit02(): Bool = Int64.clearBit(pos = 1, 1i64) == 1i64

    @test
    def clearBit03(): Bool = Int64.clearBit(pos = 1, 2i64) == 0i64

    @test
    def clearBit04(): Bool = Int64.clearBit(pos = 2, 2i64) == 2i64

    @test
    def clearBit05(): Bool = Int64.clearBit(pos = 31, 0i64) == 0i64

    @test
    def clearBit06(): Bool = Int64.clearBit(pos = 30, 0i64) == 0i64

    @test
    def clearBit07(): Bool = Int64.clearBit(pos = 63, Int64.leftShift(1i64, 63)) == 0i64

    @test
    def clearBit08(): Bool = Int64.clearBit(pos = 27, 442931848i64) == 308714120i64

    @test
    def clearBit09(): Bool = Int64.clearBit(pos = 0, -1i64) == -2i64

    @test
    def clearBit10(): Bool = Int64.clearBit(pos = 1, -1i64) == -3i64

    @test
    def clearBit11(): Bool = Int64.clearBit(pos = 5, -1i64) == -33i64

    @test
    def clearBit12(): Bool = Int64.clearBit(pos = 63, -1i64) == Int64.bitwiseNot(Int64.leftShift(1i64, 63))

    @test
    def clearBit13(): Bool = Int64.clearBit(pos = 64, -1i64) == -2i64

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flipBit01(): Bool = Int64.flipBit(pos = 0, 1i64) == 0i64

    @test
    def flipBit02(): Bool = Int64.flipBit(pos = 1, 1i64) == 3i64

    @test
    def flipBit03(): Bool = Int64.flipBit(pos = 1, 2i64) == 0i64

    @test
    def flipBit04(): Bool = Int64.flipBit(pos = 2, 2i64) == 6i64

    @test
    def flipBit05(): Bool = Int64.flipBit(pos = 31, 0i64) == 2147483648i64

    @test
    def flipBit06(): Bool = Int64.flipBit(pos = 30, 0i64) == 1073741824i64

    @test
    def flipBit07(): Bool = Int64.flipBit(pos = 63, Int64.leftShift(1i64, 63)) == 0i64

    @test
    def flipBit08(): Bool = Int64.flipBit(pos = 0, -1i64) == -2i64

    @test
    def flipBit9(): Bool = Int64.flipBit(pos = 1, -1i64) == -3i64

    @test
    def flipBit10(): Bool = Int64.flipBit(pos = 5, -1i64) == -33i64

    @test
    def flipBit11(): Bool = Int64.flipBit(pos = 31, -1i64) == (-1i64-Int64.leftShift(1i64, 31))

    @test
    def flipBit12(): Bool = Int64.flipBit(pos = 64, -1i64) == -2i64

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def log201(): Bool = Int64.log2(10i64) == 3i64

    @test
    def log202(): Bool = Int64.log2(10000i64) == 13i64

    @test
    def log203(): Bool = Int64.log2(8i64) == 3i64

    @test
    def log204(): Bool = Int64.log2(256i64) == 8i64

    @test
    def log205(): Bool = Int64.log2(1i64) == 0i64

    @test
    def log206(): Bool = Int64.log2(12345678i64) == 23i64

    @test
    def log207(): Bool = Int64.log2(20i64) == 4i64

    @test
    def log208(): Bool = Int64.log2(9_223_372_036_854_775_807i64) == 62i64

    @test
    def log209(): Bool = Int64.log2(0i64) == 0i64

    @test
    def log210(): Bool = Int64.log2(-1i64) == 0i64


    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseAnd01(): Bool = Int64.bitwiseAnd(0i64, 0i64) == 0i64

    @test
    def bitwiseAnd02(): Bool = Int64.bitwiseAnd(127i64, 0i64) == 0i64

    @test
    def bitwiseAnd03(): Bool = Int64.bitwiseAnd(-128i64, 0i64) == 0i64

    @test
    def bitwiseAnd04(): Bool = Int64.bitwiseAnd(0i64, 1i64) == 0i64

    @test
    def bitwiseAnd05(): Bool = Int64.bitwiseAnd(127i64, 1i64) == 1i64

    @test
    def bitwiseAnd06(): Bool = Int64.bitwiseAnd(-128i64, 1i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseNot01(): Bool = Int64.bitwiseNot(0i64) == -1i64

    @test
    def bitwiseNot02(): Bool = Int64.bitwiseNot(1i64) == -2i64

    @test
    def bitwiseNot03(): Bool = Int64.bitwiseNot(-1i64) == -0i64

    @test
    def bitwiseNot04(): Bool = Int64.bitwiseNot(127i64) == -128i64

    @test
    def bitwiseNot05(): Bool = Int64.bitwiseNot(-128i64) == 127i64

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseOr01(): Bool = Int64.bitwiseOr(0i64, 0i64) == 0i64

    @test
    def bitwiseOr02(): Bool = Int64.bitwiseOr(127i64, 0i64) == 127i64

    @test
    def bitwiseOr03(): Bool = Int64.bitwiseOr(-128i64, 0i64) == -128i64

    @test
    def bitwiseOr04(): Bool = Int64.bitwiseOr(0i64, 1i64) == 1i64

    @test
    def bitwiseOr05(): Bool = Int64.bitwiseOr(127i64, 1i64) == 127i64

    @test
    def bitwiseOr06(): Bool = Int64.bitwiseOr(-128i64, 1i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseXor01(): Bool = Int64.bitwiseXor(0i64, 0i64) == 0i64

    @test
    def bitwiseXor02(): Bool = Int64.bitwiseXor(127i64, 0i64) == 127i64

    @test
    def bitwiseXor03(): Bool = Int64.bitwiseXor(-128i64, 0i64) == -128i64

    @test
    def bitwiseXor04(): Bool = Int64.bitwiseXor(0i64, 1i64) == 1i64

    @test
    def bitwiseXor05(): Bool = Int64.bitwiseXor(127i64, 1i64) == 126i64

    @test
    def bitwiseXor06(): Bool = Int64.bitwiseXor(-128i64, 1i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def factorial01(): Bool = Int64.factorial(0i64) == 1i64

    @test
    def factorial02(): Bool = Int64.factorial(1i64) == 1i64

    @test
    def factorial03(): Bool = Int64.factorial(2i64) == 2i64

    @test
    def factorial04(): Bool = Int64.factorial(3i64) == 6i64

    @test
    def factorial05(): Bool = Int64.factorial(4i64) == 24i64

    @test
    def factorial06(): Bool = Int64.factorial(5i64) == 120i64

    @test
    def factorial07(): Bool = Int64.factorial(10i64) == 3628800i64

    @test
    def factorial08(): Bool = Int64.factorial(-1i64) == 0i64

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Int64.toString(0i64) == "0"

    @test
    def toString02(): Bool = Int64.toString(1i64) == "1"

    @test
    def toString03(): Bool = Int64.toString(-1i64) == "-1"

    @test
    def toString04(): Bool = Int64.toString(9223372036854775807i64) == "9223372036854775807"

    @test
    def toString05(): Bool = Int64.toString(-9223372036854775808i64) == "-9223372036854775808"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Int64.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = Int64.fromString(" 1") == Some(1i64)

    @test
    def fromString03(): Bool = Int64.fromString("1 ") == Some(1i64)

    @test
    def fromString04(): Bool = Int64.fromString("0") == Some(0i64)

    @test
    def fromString05(): Bool = Int64.fromString("1") == Some(1i64)

    @test
    def fromString06(): Bool = Int64.fromString("-1") == Some(-1i64)

    @test
    def fromString07(): Bool = Int64.fromString("9223372036854775807") == Some(9223372036854775807i64)

    @test
    def fromString08(): Bool = Int64.fromString("-9223372036854775808") == Some(-9223372036854775808i64)

    /////////////////////////////////////////////////////////////////////////////
    // parse                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def parse01(): Bool = ("0" |> Int64.parse(10)) == Ok(0i64)

    @test
    def parse02(): Bool = ("     0      " |> Int64.parse(10)) == Ok(0i64)

    @test
    def parse03(): Bool = ("-FF" |> Int64.parse(16)) == Ok(-255i64)

    @test
    def parse04(): Bool = ("1100110" |> Int64.parse(2)) == Ok(102i64)

    @test
    def parse05(): Bool = ("Kona" |> Int64.parse(27)) == Ok(411787i64)

    @test
    def parse06(): Bool = ("Kona" |> Int64.parse(10)) |> Result.isErr

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Int64.tryToInt8(0i64) == Some(0i8)

    @test
    def tryToInt802(): Bool = Int64.tryToInt8(1i64) == Some(1i8)

    @test
    def tryToInt803(): Bool = Int64.tryToInt8(-1i64) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Int64.tryToInt8(127i64) == Some(127i8)

    @test
    def tryToInt805(): Bool = Int64.tryToInt8(128i64) == None

    @test
    def tryToInt806(): Bool = Int64.tryToInt8(-128i64) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Int64.tryToInt8(-129i64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = Int64.tryToInt16(0i64) == Some(0i16)

    @test
    def tryToInt1602(): Bool = Int64.tryToInt16(1i64) == Some(1i16)

    @test
    def tryToInt1603(): Bool = Int64.tryToInt16(-1i64) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = Int64.tryToInt16(32767i64) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = Int64.tryToInt16(32768i64) == None

    @test
    def tryToInt1606(): Bool = Int64.tryToInt16(-32768i64) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = Int64.tryToInt16(-32769i64) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt3201(): Bool = Int64.tryToInt32(0i64) == Some(0)

    @test
    def tryToInt3202(): Bool = Int64.tryToInt32(1i64) == Some(1)

    @test
    def tryToInt3203(): Bool = Int64.tryToInt32(-1i64) == Some(-1)

    @test
    def tryToInt3204(): Bool = Int64.tryToInt32(2147483647i64) == Some(2147483647)

    @test
    def tryToInt3205(): Bool = Int64.tryToInt32(2147483648i64) == None

    @test
    def tryToInt3206(): Bool = Int64.tryToInt32(-2147483648i64) == Some(-2147483648)

    @test
    def tryToInt3207(): Bool = Int64.tryToInt32(-2147483649i64) == None

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = Int64.toBigInt(0i64) == 0ii

    @test
    def toBigInt02(): Bool = Int64.toBigInt(1i64) == 1ii

    @test
    def toBigInt03(): Bool = Int64.toBigInt(-1i64) == -1ii

    @test
    def toBigInt04(): Bool = Int64.toBigInt(9223372036854775807i64) == 9223372036854775807ii

    @test
    def toBigInt05(): Bool = Int64.toBigInt(-9223372036854775808i64) == -9223372036854775808ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat3201(): Bool = Int64.toFloat32(0i64) == 0.0f32

    @test
    def toFloat3202(): Bool = Int64.toFloat32(1i64) == 1.0f32

    @test
    def toFloat3203(): Bool = Int64.toFloat32(-1i64) == -1.0f32

    @test
    def toFloat3204(): Bool = Int64.toFloat32(9223372036854775807i64) == 9223372036854775807.0f32

    @test
    def toFloat3205(): Bool = Int64.toFloat32(-9223372036854775808i64) == -9223372036854775808.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Int64.toFloat64(0i64) == 0.0f64

    @test
    def toFloat6402(): Bool = Int64.toFloat64(1i64) == 1.0f64

    @test
    def toFloat6403(): Bool = Int64.toFloat64(-1i64) == -1.0f64

    @test
    def toFloat6404(): Bool = Int64.toFloat64(9223372036854775807i64) == 9223372036854775807.0f64

    @test
    def toFloat6405(): Bool = Int64.toFloat64(-9223372036854775808i64) == -9223372036854775808.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigDecimal01(): Bool = Int64.toBigDecimal(0i64) == 0.0ff

    @test
    def toBigDecimal02(): Bool = Int64.toBigDecimal(1i64) == 1.0ff

    @test
    def toBigDecimal03(): Bool = Int64.toBigDecimal(-1i64) == -1.0ff

    @test
    def toBigDecimal04(): Bool = Int64.toBigDecimal(9223372036854775807i64) == 9223372036854775807.0ff

    @test
    def toBigDecimal05(): Bool = Int64.toBigDecimal(-9223372036854775808i64) == -9223372036854775808.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, 0i64) == 0i8

    @test
    def clampToInt802(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, 100i64) == 100i8

    @test
    def clampToInt803(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, 101i64) == 100i8

    @test
    def clampToInt804(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, -100i64) == -100i8

    @test
    def clampToInt805(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, -101i64) == -100i8

    @test
    def clampToInt806(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, Int64.maxValue()) == 100i8

    @test
    def clampToInt807(): Bool = Int64.clampToInt8(min = -100i8, max = 100i8, Int64.minValue()) == -100i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, 0i64) == 0i16

    @test
    def clampToInt1602(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, 100i64) == 100i16

    @test
    def clampToInt1603(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, 101i64) == 100i16

    @test
    def clampToInt1604(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, -100i64) == -100i16

    @test
    def clampToInt1605(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, -101i64) == -100i16

    @test
    def clampToInt1606(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, Int64.maxValue()) == 100i16

    @test
    def clampToInt1607(): Bool = Int64.clampToInt16(min = -100i16, max = 100i16, Int64.minValue()) == -100i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt3201(): Bool = Int64.clampToInt32(min = -100, max = 100, 0i64) == 0

    @test
    def clampToInt3202(): Bool = Int64.clampToInt32(min = -100, max = 100, 100i64) == 100

    @test
    def clampToInt3203(): Bool = Int64.clampToInt32(min = -100, max = 100, 101i64) == 100

    @test
    def clampToInt3204(): Bool = Int64.clampToInt32(min = -100, max = 100, -100i64) == -100

    @test
    def clampToInt3205(): Bool = Int64.clampToInt32(min = -100, max = 100, -101i64) == -100

    @test
    def clampToInt3206(): Bool = Int64.clampToInt32(min = -100, max = 100, Int64.maxValue()) == 100

    @test
    def clampToInt3207(): Bool = Int64.clampToInt32(min = -100, max = 100, Int64.minValue()) == -100

    /////////////////////////////////////////////////////////////////////////////
    // longValue                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def longValue01(): Bool =
        let i = Int64.valueOf(0i64);
        Int64.longValue(i) == 0i64

    @test
    def longValue02(): Bool =
        let i = Int64.valueOf(1i64);
        Int64.longValue(i) == 1i64

    @test
    def shortValue03(): Bool =
        let i = Int64.valueOf(-1i64);
        Int64.longValue(i) == -1i64

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valueOf01(): Bool =
        let i = Int64.valueOf(0i64);
        unsafely IO run i.equals(Int64.valueOf(0i64))

    @test
    def valueOf02(): Bool =
        let i = Int64.valueOf(1i64);
        unsafely IO run i.equals(Int64.valueOf(1i64))

    @test
    def valueOf03(): Bool =
        let i = Int64.valueOf(-1i64);
        unsafely IO run i.equals(Int64.valueOf(-1i64))

}
