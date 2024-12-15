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

mod TestInt16 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mul01(): Bool = mul(1i16, 0i16) == 0i16

    @test
    def mul02(): Bool = mul(0i16, 1i16) == 0i16

    @test
    def mul03(): Bool = mul(0i16, 0i16) == 0i16

    @test
    def mul04(): Bool = mul(4i16, -8192i16) == -32768i16

    @test
    def mul05(): Bool = mul(7i16, 4681i16) == 32767i16

    @test
    def mul06(): Bool = mul(5i16, 11i16) == 55i16

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Int16.maxValue() == Int16.bitwiseNot(Int16.leftShift(1i16, 15))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Int16.minValue() == Int16.leftShift(1i16, 15)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Int16.size() == 16

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Int16.min(1i16, 2i16) == 1i16

    @test
    def min02(): Bool = Int16.min(2i16, -1i16) == -1i16

    @test
    def min03(): Bool = Int16.min(-33i16, -66i16) == -66i16

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Int16.max(48i16, 49i16) == 49i16

    @test
    def max02(): Bool = Int16.max(4i16, -16i16) == 4i16

    @test
    def max03(): Bool = Int16.max(-34i16, -16i16) == -16i16

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def abs01(): Bool = Int16.abs(31i16) == 31i16

    @test
    def abs02(): Bool = Int16.abs(-31i16) == 31i16

    @test
    def abs03(): Bool = Int16.abs(32767i16) == 32767i16

    @test
    def abs04(): Bool = Int16.abs(-32767i16) == 32767i16

    @test
    def abs05(): Bool = Int16.abs(0i16) == 0i16

    @test
    def abs06(): Bool = Int16.abs(-32768i16) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def modulo01(): Bool = (12000i16 `Int16.modulo` 2000i16) == 0i16

    @test
    def modulo02(): Bool = (12000i16 `Int16.modulo` 5000i16) == 2000i16

    @test
    def modulo03(): Bool = (-12000i16 `Int16.modulo` 5000i16) == 3000i16

    @test
    def modulo04(): Bool = (12000i16 `Int16.modulo` -5000i16) == -3000i16

    @test
    def modulo05(): Bool = (-12000i16 `Int16.modulo` -5000i16) == -2000i16

    @test
    def modulo06(): Bool = (1i16 `Int16.modulo` 0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def remainder01(): Bool = (12000i16 `Int16.remainder` 2000i16) == 0i16

    @test
    def remainder02(): Bool = (12000i16 `Int16.remainder` 5000i16) == 2000i16

    @test
    def remainder03(): Bool = (-12000i16 `Int16.remainder` 5000i16) == -2000i16

    @test
    def remainder04(): Bool = (12000i16 `Int16.remainder` -5000i16) == 2000i16

    @test
    def remainder05(): Bool = (-12000i16 `Int16.remainder` -5000i16) == -2000i16

    @test
    def remainder06(): Bool = (1i16 `Int16.remainder` 0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def dist01(): Bool = Int16.dist(31i16, -7i16) == 38i16

    @test
    def dist02(): Bool = Int16.dist(-44i16, -1i16) == 43i16

    @test
    def dist03(): Bool = Int16.dist(-2i16, -2i16) == 0i16

    @test
    def dist04(): Bool = Int16.dist(-32768i16, -1i16) == 32767i16

    @test
    def dist05(): Bool = Int16.dist(-32768i16, 0i16) == -1i16

    @test
    def dist06(): Bool = Int16.dist(-123i16, 4i16) == 127i16

    @test
    def dist07(): Bool = Int16.dist(-32763i16, 5i16) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = Int16.compare(-1i16, 44i16) == -1

    @test
    def compare02(): Bool = Int16.compare(-1i16, -44i16) == 1

    @test
    def compare03(): Bool = Int16.compare(88i16, 88i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = Int16.signum(-22i16) == -1

    @test
    def signum02(): Bool = Int16.signum(22i16) == 1

    @test
    def signum03(): Bool = Int16.signum(0i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def pow01(): Bool = Int16.pow(0i16, 0i16) == 1i16

    @test
    def pow02(): Bool = Int16.pow(0i16, 1i16) == 0i16

    @test
    def pow03(): Bool = Int16.pow(1i16, 0i16) == 1i16

    @test
    def pow04(): Bool = Int16.pow(1i16, 1i16) == 1i16

    @test
    def pow05(): Bool = Int16.pow(5i16, 2i16) == 25i16

    /////////////////////////////////////////////////////////////////////////////
    // leftShift                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseLeftShift01(): Bool = Int16.leftShift(8i16, 0) == 8i16

    @test
    def testBitwiseLeftShift02(): Bool = Int16.leftShift(8i16, 8) == 2048i16

    @test
    def testBitwiseLeftShift03(): Bool = Int16.leftShift(8i16, 12) == -32768i16

    @test
    def testBitwiseLeftShift04(): Bool = Int16.leftShift(8i16, 13) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseRightShift01(): Bool = Int16.rightShift(12000i16, 0) == 12000i16

    @test
    def testBitwiseRightShift02(): Bool = Int16.rightShift(12000i16, 2) == 3000i16

    @test
    def testBitwiseRightShift03(): Bool = Int16.rightShift(12000i16, 15) == 0i16

    @test
    def testBitwiseRightShift04(): Bool = Int16.rightShift(-12000i16, 2) == -3000i16


    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def logicalRightShift01(): Bool = Int16.logicalRightShift(dist = 4, 2233i16) == Int16.rightShift(2233i16, 4)

    @test
    def logicalRightShift02(): Bool = Int16.logicalRightShift(dist = 33, 32767i16) == Int16.rightShift(32767i16, 33)

    @test
    def logicalRightShift03(): Bool = Int16.logicalRightShift(dist = -8, 2233i16) == Int16.rightShift(2233i16, -8)

    @test
    def logicalRightShift04(): Bool = Int16.logicalRightShift(dist = 4, 2233i16) == Int16.rightShift(2233i16, 4)

    @test
    def logicalRightShift05(): Bool = Int16.logicalRightShift(dist = 1, -1i16) == 32767i16

    @test
    def logicalRightShift06(): Bool = Int16.logicalRightShift(dist = 4, -35i16) == 4093i16

    @test
    def logicalRightShift07(): Bool = Int16.logicalRightShift(dist = -2777, -2777i16) == 490i16

    @test
    def logicalRightShift08(): Bool = Int16.logicalRightShift(dist = 64, -233i16) == -233i16

    @test
    def logicalRightShift09(): Bool = Int16.logicalRightShift(dist = 63, -233i16) == 0i16

    @test
    def logicalRightShift10(): Bool = Int16.logicalRightShift(dist = 0, -27i16) == -27i16

    @test
    def logicalRightShift11(): Bool = Int16.logicalRightShift(dist = 1, 32767i16) == 16383i16

    @test
    def logicalRightShift12(): Bool = Int16.logicalRightShift(dist = 17, 32767i16) == 0i16

    @test
    def logicalRightShift13(): Bool = Int16.logicalRightShift(dist = 16, 32767i16) == 0i16

    @test
    def logicalRightShift14(): Bool = Int16.logicalRightShift(dist = 17, -32768i16) == 0i16

    @test
    def logicalRightShift15(): Bool = Int16.logicalRightShift(dist = 16, -32768i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def bitCount01(): Bool = Int16.bitCount(19909i16) == 8

    @test
    def bitCount02(): Bool = Int16.bitCount(-1i16) == 16

    @test
    def bitCount03(): Bool = Int16.bitCount(-451i16) == 12

    @test
    def bitCount04(): Bool = Int16.bitCount(0i16) == 0

    @test
    def bitCount05(): Bool = Int16.bitCount(32767i16) == 15

    @test
    def bitCount06(): Bool = Int16.bitCount(-32768i16) == 1

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateRight01(): Bool = Int16.rotateRight(dist = 3, 0i16) == 0i16

    @test
    def rotateRight02(): Bool = Int16.rotateRight(dist = 19, 0i16) == 0i16

    @test
    def rotateRight03(): Bool = Int16.rotateRight(dist = -3, 0i16) == 0i16

    @test
    def rotateRight04(): Bool = Int16.rotateRight(dist = -3, -1i16) == -1i16

    @test
    def rotateRight05(): Bool = Int16.rotateRight(dist = 19, -1i16) == -1i16

    @test
    def rotateRight06(): Bool = Int16.rotateRight(dist = -3, -1i16) == -1i16

    @test
    def rotateRight07(): Bool = Int16.rotateRight(dist = 3, 8410i16) == 17435i16

    @test
    def rotateRight08(): Bool = Int16.rotateRight(dist = 19, 8410i16) == 17435i16

    @test
    def rotateRight09(): Bool = Int16.rotateRight(dist = -13, 8410i16) == 17435i16

    @test
    def rotateRight10(): Bool = Int16.rotateRight(dist = 10, -14949i16) == 26353i16

    @test
    def rotateRight11(): Bool = Int16.rotateRight(dist = 26, -14949i16) == 26353i16

    @test
    def rotateRight12(): Bool = Int16.rotateRight(dist = -6, -14949i16) == 26353i16

    @test
    def rotateRight13(): Bool = Int16.rotateRight(dist = 6, -6415i16) == -14437i16

    @test
    def rotateRight14(): Bool = Int16.rotateRight(dist = 22, -6415i16) == -14437i16

    @test
    def rotateRight15(): Bool = Int16.rotateRight(dist = -10, -6415i16) == -14437i16

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateLeft01(): Bool = Int16.rotateLeft(dist = 3, 0i16) == 0i16

    @test
    def rotateLeft02(): Bool = Int16.rotateLeft(dist = 19, 0i16) == 0i16

    @test
    def rotateLeft03(): Bool = Int16.rotateLeft(dist = -3, 0i16) == 0i16

    @test
    def rotateLeft04(): Bool = Int16.rotateLeft(dist = 3, -1i16) == -1i16

    @test
    def rotateLeft05(): Bool = Int16.rotateLeft(dist = 19, -1i16) == -1i16

    @test
    def rotateLeft06(): Bool = Int16.rotateLeft(dist = -3, -1i16) == -1i16

    @test
    def rotateLeft07(): Bool = Int16.rotateLeft(dist = 13, 8410i16) == 17435i16

    @test
    def rotateLeft08(): Bool = Int16.rotateLeft(dist = 29, 8410i16) == 17435i16

    @test
    def rotateLeft09(): Bool = Int16.rotateLeft(dist = -3, 8410i16) == 17435i16

    @test
    def rotateLeft10(): Bool = Int16.rotateLeft(dist = 6, -14949i16) == 26353i16

    @test
    def rotateLeft11(): Bool = Int16.rotateLeft(dist = 22, -14949i16) == 26353i16

    @test
    def rotateLeft12(): Bool = Int16.rotateLeft(dist = -10, -14949i16) == 26353i16

    @test
    def rotateLeft13(): Bool = Int16.rotateLeft(dist = 10, -6415i16) == -14437i16

    @test
    def rotateLeft14(): Bool = Int16.rotateLeft(dist = 26, -6415i16) == -14437i16

    @test
    def rotateLeft15(): Bool = Int16.rotateLeft(dist = -6, -6415i16) == -14437i16

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def reverse01(): Bool = Int16.reverse(18140i16) == 15202i16

    @test
    def reverse02(): Bool = Int16.reverse(15202i16) == 18140i16

    @test
    def reverse03(): Bool = Int16.reverse(-17565i16) == -14627i16

    @test
    def reverse04(): Bool = Int16.reverse(-14627i16) == -17565i16

    @test
    def reverse05(): Bool = Int16.reverse(-26552i16) == 4633i16

    @test
    def reverse06(): Bool = Int16.reverse(4633i16) == -26552i16

    @test
    def reverse07(): Bool = Int16.reverse(4i16) == 8192i16

    @test
    def reverse08(): Bool = Int16.reverse(8192i16) == 4i16

    @test
    def reverse09(): Bool = Int16.reverse(-1i16) == -1i16

    @test
    def reverse10(): Bool = Int16.reverse(0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBitPosition01(): Bool = Int16.highestOneBitPosition(-1i16) == 15

    @test
    def highestOneBitPosition02(): Bool = Int16.highestOneBitPosition(-32768i16) == 15

    @test
    def highestOneBitPosition03(): Bool = Int16.highestOneBitPosition(1i16) == 0

    @test
    def highestOneBitPosition04(): Bool = Int16.highestOneBitPosition(2i16) == 1

    @test
    def highestOneBitPosition05(): Bool = Int16.highestOneBitPosition(3i16) == 1

    @test
    def highestOneBitPosition06(): Bool = Int16.highestOneBitPosition(20i16) == 4

    @test
    def highestOneBitPosition07(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 14i16)) == 14

    @test
    def highestOneBitPosition08(): Bool = Int16.highestOneBitPosition(32767i16) == 14

    @test
    def highestOneBitPosition09(): Bool = Int16.highestOneBitPosition(0i16) == -1

    @test
    def highestOneBitPosition10(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 8i16) + Int16.pow(2i16, 11i16)) == 11

    @test
    def highestOneBitPosition11(): Bool = Int16.highestOneBitPosition(Int16.pow(2i16, 3i16) + Int16.pow(2i16, 7i16)) == 7

    @test
    def highestOneBitPosition12(): Bool = Int16.highestOneBitPosition(-32767i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBit01(): Bool = Int16.highestOneBit(-1i16) == Int16.leftShift(1i16, 15)

    @test
    def highestOneBit02(): Bool = Int16.highestOneBit(-32768i16) == Int16.leftShift(1i16, 15)

    @test
    def highestOneBit03(): Bool = Int16.highestOneBit(-77i16) == Int16.leftShift(1i16, 15)

    @test
    def highestOneBit04(): Bool = Int16.highestOneBit(2i16) == 2i16

    @test
    def highestOneBit05(): Bool = Int16.highestOneBit(3i16) == 2i16

    @test
    def highestOneBit06(): Bool = Int16.highestOneBit(20i16) == 16i16

    @test
    def highestOneBit07(): Bool = Int16.highestOneBit(Int16.pow(2i16, 11i16) + Int16.pow(2i16, 13i16)) == 8192i16

    @test
    def highestOneBit08(): Bool = Int16.highestOneBit(32767i16) == 16384i16

    @test
    def highestOneBit09(): Bool = Int16.highestOneBit(0i16) == 0i16

    @test
    def highestOneBit10(): Bool = Int16.highestOneBit(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 9i16)) == 512i16

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfLeadingZeros01(): Bool = Int16.numberOfLeadingZeros(0i16) == 16

    @test
    def numberOfLeadingZeros02(): Bool = Int16.numberOfLeadingZeros(-32768i16) == 0

    @test
    def numberOfLeadingZeros03(): Bool = Int16.numberOfLeadingZeros(-1i16) == 0

    @test
    def numberOfLeadingZeros04(): Bool = Int16.numberOfLeadingZeros(32767i16) == 1

    @test
    def numberOfLeadingZeros05(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 14i16)) == 1

    @test
    def numberOfLeadingZeros06(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 11i16)) == 4

    @test
    def numberOfLeadingZeros07(): Bool = Int16.numberOfLeadingZeros(Int16.pow(2i16, 7i16) + Int16.pow(2i16, 6i16)) == 8

    @test
    def numberOfLeadingZeros08(): Bool = Int16.numberOfLeadingZeros(20i16) == 11

    @test
    def numberOfLeadingZeros09(): Bool = Int16.numberOfLeadingZeros(1i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBitPosition01(): Bool = Int16.lowestOneBitPosition(-1i16) == 0

    @test
    def lowestOneBitPosition02(): Bool = Int16.lowestOneBitPosition(-32768i16) == 15

    @test
    def lowestOneBitPosition03(): Bool = Int16.lowestOneBitPosition(1i16) == 0

    @test
    def lowestOneBitPosition04(): Bool = Int16.lowestOneBitPosition(2i16) == 1

    @test
    def lowestOneBitPosition05(): Bool = Int16.lowestOneBitPosition(3i16) == 0

    @test
    def lowestOneBitPosition06(): Bool = Int16.lowestOneBitPosition(4i16) == 2

    @test
    def lowestOneBitPosition07(): Bool = Int16.lowestOneBitPosition(12i16) == 2

    @test
    def lowestOneBitPosition08(): Bool = Int16.lowestOneBitPosition(Int16.pow(2i16, 11i16)) == 11

    @test
    def lowestOneBitPosition09(): Bool = Int16.lowestOneBitPosition(Int16.pow(2i16, 11i16) + Int16.pow(2i16, 6i16)) == 6

    @test
    def lowestOneBitPosition10(): Bool = Int16.lowestOneBitPosition(32767i16) == 0

    @test
    def lowestOneBitPosition11(): Bool = Int16.lowestOneBitPosition(0i16) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBit01(): Bool = Int16.lowestOneBit(-1i16) == 1i16

    @test
    def lowestOneBit02(): Bool = Int16.lowestOneBit(-32768i16) == Int16.leftShift(1i16, 15)

    @test
    def lowestOneBit03(): Bool = Int16.lowestOneBit(1i16) == 1i16

    @test
    def lowestOneBit04(): Bool = Int16.lowestOneBit(2i16) == 2i16

    @test
    def lowestOneBit05(): Bool = Int16.lowestOneBit(6i16) == 2i16

    @test
    def lowestOneBit06(): Bool = Int16.lowestOneBit(12i16) == 4i16

    @test
    def lowestOneBit07(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 6i16) + Int16.pow(2i16, 9i16) + Int16.pow(2i16, 14i16)) == 64i16

    @test
    def lowestOneBit08(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 13i16)) == 8192i16

    @test
    def lowestOneBit09(): Bool = Int16.lowestOneBit(Int16.pow(2i16, 3i16) + Int16.pow(2i16, 5i16)) == 8i16

    @test
    def lowestOneBit10(): Bool = Int16.lowestOneBit(32767i16) == 1i16

    @test
    def lowestOneBit11(): Bool = Int16.lowestOneBit(0i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfTrailingZeros01(): Bool = Int16.numberOfTrailingZeros(0i16) == 16

    @test
    def numberOfTrailingZeros02(): Bool = Int16.numberOfTrailingZeros(3i16) == 0

    @test
    def numberOfTrailingZeros03(): Bool = Int16.numberOfTrailingZeros(42i16) == 1

    @test
    def numberOfTrailingZeros04(): Bool = Int16.numberOfTrailingZeros(Int16.pow(2i16, 10i16) + Int16.pow(2i16, 14i16)) == 10

    @test
    def numberOfTrailingZeros05(): Bool = Int16.numberOfTrailingZeros(Int16.pow(2i16, 8i16)) == 8

    @test
    def numberOfTrailingZeros06(): Bool = Int16.numberOfTrailingZeros(-1i16) == 0

    @test
    def numberOfTrailingZeros07(): Bool = Int16.numberOfTrailingZeros(-32768i16) == 15

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getBit01(): Bool = Int16.getBit(pos = 5, -1i16) == 1

    @test
    def getBit02(): Bool = Int16.getBit(pos = 15, -10000i16) == 1

    @test
    def getBit03(): Bool = Int16.getBit(pos = 12, Int16.pow(2i16, 12i16)) == 1

    @test
    def getBit04(): Bool = Int16.getBit(pos = 13, Int16.pow(2i16, 12i16)) == 0

    @test
    def getBit05(): Bool = Int16.getBit(pos = 11, Int16.pow(2i16, 12i16)) == 0

    @test
    def getBit06(): Bool = Int16.getBit(pos = 9, -1i16 - Int16.pow(2i16, 9i16)) == 0

    @test
    def getBit07(): Bool = Int16.getBit(pos = 10, -1i16 - Int16.pow(2i16, 9i16)) == 1

    @test
    def getBit08(): Bool = Int16.getBit(pos = 15, 0i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def setBit01(): Bool = Int16.setBit(pos = 0, 0i16) == 1i16

    @test
    def setBit02(): Bool = Int16.setBit(pos = 1, 0i16) == 2i16

    @test
    def setBit03(): Bool = Int16.setBit(pos = 15, 0i16) == -32768i16

    @test
    def setBit04(): Bool = Int16.setBit(pos = 16, 0i16) == 0i16

    @test
    def setBit05(): Bool = Int16.setBit(pos = 31, -1418i16) == -1418i16

    @test
    def setBit07(): Bool = Int16.setBit(pos = 4, -17i16) == -1i16

    @test
    def setBit08(): Bool = Int16.setBit(pos = 10, -1025i16) == -1i16

    @test
    def setBit09(): Bool = Int16.setBit(pos = 63, -17i16) == -17i16

    @test
    def setBit10(): Bool = Int16.setBit(pos = 30, -17i16) == -17i16

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clearBit01(): Bool = Int16.clearBit(pos = 0, 1i16) == 0i16

    @test
    def clearBit02(): Bool = Int16.clearBit(pos = 1, 1i16) == 1i16

    @test
    def clearBit03(): Bool = Int16.clearBit(pos = 1, 2i16) == 0i16

    @test
    def clearBit04(): Bool = Int16.clearBit(pos = 2, 2i16) == 2i16

    @test
    def clearBit05(): Bool = Int16.clearBit(pos = 31, 0i16) == 0i16

    @test
    def clearBit06(): Bool = Int16.clearBit(pos = 30, 0i16) == 0i16

    @test
    def clearBit07(): Bool = Int16.clearBit(pos = 15, Int16.leftShift(1i16, 15)) == 0i16

    @test
    def clearBit08(): Bool = Int16.clearBit(pos = 11, 27957i16) == 25909i16

    @test
    def clearBit09(): Bool = Int16.clearBit(pos = 0, -1i16) == -2i16

    @test
    def clearBit10(): Bool = Int16.clearBit(pos = 1, -1i16) == -3i16

    @test
    def clearBit11(): Bool = Int16.clearBit(pos = 5, -1i16) == -33i16

    @test
    def clearBit12(): Bool = Int16.clearBit(pos = 15, -1i16) == Int16.bitwiseNot(Int16.leftShift(1i16, 15))

    @test
    def clearBit13(): Bool = Int16.clearBit(pos = 32, -1i16) == -2i16

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flipBit01(): Bool = Int16.flipBit(pos = 0, 1i16) == 0i16

    @test
    def flipBit02(): Bool = Int16.flipBit(pos = 1, 1i16) == 3i16

    @test
    def flipBit03(): Bool = Int16.flipBit(pos = 1, 2i16) == 0i16

    @test
    def flipBit04(): Bool = Int16.flipBit(pos = 2, 2i16) == 6i16

    @test
    def flipBit05(): Bool = Int16.flipBit(pos = 15, 0i16) == -32768i16

    @test
    def flipBit06(): Bool = Int16.flipBit(pos = 16, 0i16) == 0i16

    @test
    def flipBit07(): Bool = Int16.flipBit(pos = 15, Int16.leftShift(1i16, 15)) == 0i16

    @test
    def flipBit08(): Bool = Int16.flipBit(pos = 0, -1i16) == -2i16

    @test
    def flipBit9(): Bool = Int16.flipBit(pos = 1, -1i16) == -3i16

    @test
    def flipBit10(): Bool = Int16.flipBit(pos = 5, -1i16) == -33i16

    @test
    def flipBit11(): Bool = Int16.flipBit(pos = 13, -1i16) == (-1i16-Int16.leftShift(1i16, 13))

    @test
    def flipBit12(): Bool = Int16.flipBit(pos = 32, -1i16) == -2i16

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def log201(): Bool = Int16.log2(10i16) == 3i16

    @test
    def log202(): Bool = Int16.log2(10000i16) == 13i16

    @test
    def log203(): Bool = Int16.log2(8i16) == 3i16

    @test
    def log204(): Bool = Int16.log2(256i16) == 8i16

    @test
    def log205(): Bool = Int16.log2(1i16) == 0i16

    @test
    def log206(): Bool = Int16.log2(32767i16) == 14i16

    @test
    def log207(): Bool = Int16.log2(20i16) == 4i16

    @test
    def log208(): Bool = Int16.log2(0i16) == 0i16

    @test
    def log209(): Bool = Int16.log2(-1i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseAnd01(): Bool = Int16.bitwiseAnd(0i16, 0i16) == 0i16

    @test
    def bitwiseAnd02(): Bool = Int16.bitwiseAnd(127i16, 0i16) == 0i16

    @test
    def bitwiseAnd03(): Bool = Int16.bitwiseAnd(-128i16, 0i16) == 0i16

    @test
    def bitwiseAnd04(): Bool = Int16.bitwiseAnd(0i16, 1i16) == 0i16

    @test
    def bitwiseAnd05(): Bool = Int16.bitwiseAnd(127i16, 1i16) == 1i16

    @test
    def bitwiseAnd06(): Bool = Int16.bitwiseAnd(-128i16, 1i16) == 0i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseNot01(): Bool = Int16.bitwiseNot(0i16) == -1i16

    @test
    def bitwiseNot02(): Bool = Int16.bitwiseNot(1i16) == -2i16

    @test
    def bitwiseNot03(): Bool = Int16.bitwiseNot(-1i16) == -0i16

    @test
    def bitwiseNot04(): Bool = Int16.bitwiseNot(127i16) == -128i16

    @test
    def bitwiseNot05(): Bool = Int16.bitwiseNot(-128i16) == 127i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseOr01(): Bool = Int16.bitwiseOr(0i16, 0i16) == 0i16

    @test
    def bitwiseOr02(): Bool = Int16.bitwiseOr(127i16, 0i16) == 127i16

    @test
    def bitwiseOr03(): Bool = Int16.bitwiseOr(-128i16, 0i16) == -128i16

    @test
    def bitwiseOr04(): Bool = Int16.bitwiseOr(0i16, 1i16) == 1i16

    @test
    def bitwiseOr05(): Bool = Int16.bitwiseOr(127i16, 1i16) == 127i16

    @test
    def bitwiseOr06(): Bool = Int16.bitwiseOr(-128i16, 1i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseXor01(): Bool = Int16.bitwiseXor(0i16, 0i16) == 0i16

    @test
    def bitwiseXor02(): Bool = Int16.bitwiseXor(127i16, 0i16) == 127i16

    @test
    def bitwiseXor03(): Bool = Int16.bitwiseXor(-128i16, 0i16) == -128i16

    @test
    def bitwiseXor04(): Bool = Int16.bitwiseXor(0i16, 1i16) == 1i16

    @test
    def bitwiseXor05(): Bool = Int16.bitwiseXor(127i16, 1i16) == 126i16

    @test
    def bitwiseXor06(): Bool = Int16.bitwiseXor(-128i16, 1i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def factorial01(): Bool = Int16.factorial(0i16) == 1

    @test
    def factorial02(): Bool = Int16.factorial(1i16) == 1

    @test
    def factorial03(): Bool = Int16.factorial(2i16) == 2

    @test
    def factorial04(): Bool = Int16.factorial(3i16) == 6

    @test
    def factorial05(): Bool = Int16.factorial(4i16) == 24

    @test
    def factorial06(): Bool = Int16.factorial(5i16) == 120

    @test
    def factorial07(): Bool = Int16.factorial(10i16) == 3628800

    @test
    def factorial08(): Bool = Int16.factorial(-1i16) == 0

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Int16.toString(0i16) == "0"

    @test
    def toString02(): Bool = Int16.toString(1i16) == "1"

    @test
    def toString03(): Bool = Int16.toString(-1i16) == "-1"

    @test
    def toString04(): Bool = Int16.toString(32767i16) == "32767"

    @test
    def toString05(): Bool = Int16.toString(-32768i16) == "-32768"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Int16.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = Int16.fromString(" 1") == Some(1i16)

    @test
    def fromString03(): Bool = Int16.fromString("1 ") == Some(1i16)

    @test
    def fromString04(): Bool = Int16.fromString("0") == Some(0i16)

    @test
    def fromString05(): Bool = Int16.fromString("1") == Some(1i16)

    @test
    def fromString06(): Bool = Int16.fromString("-1") == Some(-1i16)

    @test
    def fromString07(): Bool = Int16.fromString("32767") == Some(32767i16)

    @test
    def fromString08(): Bool = Int16.fromString("-32768") == Some(-32768i16)

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Int16.tryToInt8(0i16) == Some(0i8)

    @test
    def tryToInt802(): Bool = Int16.tryToInt8(1i16) == Some(1i8)

    @test
    def tryToInt803(): Bool = Int16.tryToInt8(-1i16) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Int16.tryToInt8(127i16) == Some(127i8)

    @test
    def tryToInt805(): Bool = Int16.tryToInt8(128i16) == None

    @test
    def tryToInt806(): Bool = Int16.tryToInt8(-128i16) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Int16.tryToInt8(-129i16) == None

    /////////////////////////////////////////////////////////////////////////////
    // toInt32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt3201(): Bool = Int16.toInt32(0i16) == 0

    @test
    def toInt3202(): Bool = Int16.toInt32(1i16) == 1

    @test
    def toInt3203(): Bool = Int16.toInt32(-1i16) == -1

    @test
    def toInt3204(): Bool = Int16.toInt32(32767i16) == 32767

    @test
    def toInt3205(): Bool = Int16.toInt32(-32768i16) == -32768

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt6401(): Bool = Int16.toInt64(0i16) == 0i64

    @test
    def toInt6402(): Bool = Int16.toInt64(1i16) == 1i64

    @test
    def toInt6403(): Bool = Int16.toInt64(-1i16) == -1i64

    @test
    def toInt6404(): Bool = Int16.toInt64(32767i16) == 32767i64

    @test
    def toInt6405(): Bool = Int16.toInt64(-32768i16) == -32768i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = Int16.toBigInt(0i16) == 0ii

    @test
    def toBigInt02(): Bool = Int16.toBigInt(1i16) == 1ii

    @test
    def toBigInt03(): Bool = Int16.toBigInt(-1i16) == -1ii

    @test
    def toBigInt04(): Bool = Int16.toBigInt(32767i16) == 32767ii

    @test
    def toBigInt05(): Bool = Int16.toBigInt(-32768i16) == -32768ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat3201(): Bool = Int16.toFloat32(0i16) == 0.0f32

    @test
    def toFloat3202(): Bool = Int16.toFloat32(1i16) == 1.0f32

    @test
    def toFloat3203(): Bool = Int16.toFloat32(-1i16) == -1.0f32

    @test
    def toFloat3204(): Bool = Int16.toFloat32(32767i16) == 32767.0f32

    @test
    def toFloat3205(): Bool = Int16.toFloat32(-32768i16) == -32768.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Int16.toFloat64(0i16) == 0.0f64

    @test
    def toFloat6402(): Bool = Int16.toFloat64(1i16) == 1.0f64

    @test
    def toFloat6403(): Bool = Int16.toFloat64(-1i16) == -1.0f64

    @test
    def toFloat6404(): Bool = Int16.toFloat64(32767i16) == 32767.0f64

    @test
    def toFloat6405(): Bool = Int16.toFloat64(-32768i16) == -32768.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigDecimal01(): Bool = Int16.toBigDecimal(0i16) == 0.0ff

    @test
    def toBigDecimal02(): Bool = Int16.toBigDecimal(1i16) == 1.0ff

    @test
    def toBigDecimal03(): Bool = Int16.toBigDecimal(-1i16) == -1.0ff

    @test
    def toBigDecimal04(): Bool = Int16.toBigDecimal(32767i16) == 32767.0ff

    @test
    def toBigDecimal05(): Bool = Int16.toBigDecimal(-32768i16) == -32768.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 0i16) == 0i8

    @test
    def clampToInt802(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 100i16) == 100i8

    @test
    def clampToInt803(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, 101i16) == 100i8

    @test
    def clampToInt804(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, -100i16) == -100i8

    @test
    def clampToInt805(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, -101i16) == -100i8

    @test
    def clampToInt806(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, Int16.maxValue()) == 100i8

    @test
    def clampToInt807(): Bool = Int16.clampToInt8(min = -100i8, max = 100i8, Int16.minValue()) == -100i8

    @test
    def clampToInt808(): Bool = Int16.clampToInt8(min = 10i8, max = 0i8, 5i16) == 10i8      // Bad range

    /////////////////////////////////////////////////////////////////////////////
    // shortValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shortValue01(): Bool =
        let i = Int16.valueOf(0i16);
        Int16.shortValue(i) == 0i16

    @test
    def shortValue02(): Bool =
        let i = Int16.valueOf(1i16);
        Int16.shortValue(i) == 1i16

    @test
    def shortValue03(): Bool =
        let i = Int16.valueOf(-1i16);
        Int16.shortValue(i) == -1i16

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valueOf01(): Bool =
        let i = Int16.valueOf(0i16);
        unsafely IO run i.equals(Int16.valueOf(0i16))

    @test
    def valueOf02(): Bool =
        let i = Int16.valueOf(1i16);
        unsafely IO run i.equals(Int16.valueOf(1i16))

    @test
    def valueOf03(): Bool =
        let i = Int16.valueOf(-1i16);
        unsafely IO run i.equals(Int16.valueOf(-1i16))

}
