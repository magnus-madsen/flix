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

mod TestInt32 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mul01(): Bool = mul(1, 0) == 0

    @test
    def mul02(): Bool = mul(0, 1) == 0

    @test
    def mul03(): Bool = mul(0, 0) == 0

    @test
    def mul04(): Bool = mul(-4, 536870912) == -2147483648

    @test
    def mul05(): Bool = mul(-1, -2147483647) == 2147483647

    @test
    def mul06(): Bool = mul(5, 11) == 55

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Int32.maxValue() == Int32.bitwiseNot(Int32.leftShift(1, 31))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Int32.minValue() == Int32.leftShift(1, 31)

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Int32.size() == 32

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Int32.min(1, 2) == 1

    @test
    def min02(): Bool = Int32.min(2, -1) == -1

    @test
    def min03(): Bool = Int32.min(-33, -66) == -66

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Int32.max(48, 49) == 49

    @test
    def max02(): Bool = Int32.max(4, -16) == 4

    @test
    def max03(): Bool = Int32.max(-34, -16) == -16

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def abs01(): Bool = Int32.abs(31) == 31

    @test
    def abs02(): Bool = Int32.abs(-31) == 31

    @test
    def abs03(): Bool = Int32.abs(2147483647) == 2147483647

    @test
    def abs04(): Bool = Int32.abs(-2147483647) == 2147483647

    @test
    def abs05(): Bool = Int32.abs(0) == 0

    @test
    def abs06(): Bool = Int32.abs(-2147483648) == -1

    /////////////////////////////////////////////////////////////////////////////
    // mod                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def modulo01(): Bool = (1200000 `Int32.modulo` 200000) == 0

    @test
    def modulo02(): Bool = (1200000 `Int32.modulo` 500000) == 200000

    @test
    def modulo03(): Bool = (-1200000 `Int32.modulo` 500000) == 300000

    @test
    def modulo04(): Bool = (1200000 `Int32.modulo` -500000) == -300000

    @test
    def modulo05(): Bool = (-1200000 `Int32.modulo` -500000) == -200000

    @test
    def modulo06(): Bool = (1200000i32 `Int32.modulo` 200000i32) == 0i32

    @test
    def modulo07(): Bool = (1200000i32 `Int32.modulo` 500000i32) == 200000i32

    @test
    def modulo08(): Bool = (-1200000i32 `Int32.modulo` 500000i32) == 300000i32

    @test
    def modulo09(): Bool = (1200000i32 `Int32.modulo` -500000i32) == -300000i32

    @test
    def modulo10(): Bool = (-1200000i32 `Int32.modulo` -500000i32) == -200000i32

    @test
    def modulo11(): Bool = (1 `Int32.modulo` 0) == 0

    @test
    def modulo12(): Bool = (1i32 `Int32.modulo` 0i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // rem                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def remainder01(): Bool = (1200000 `Int32.remainder` 200000) == 0

    @test
    def remainder02(): Bool = (1200000 `Int32.remainder` 500000) == 200000

    @test
    def remainder03(): Bool = (-1200000 `Int32.remainder` 500000) == -200000

    @test
    def remainder04(): Bool = (1200000 `Int32.remainder` -500000) == 200000

    @test
    def remainder05(): Bool = (-1200000 `Int32.remainder` -500000) == -200000

    @test
    def remainder06(): Bool = (1200000i32 `Int32.remainder` 200000i32) == 0i32

    @test
    def remainder07(): Bool = (1200000i32 `Int32.remainder` 500000i32) == 200000i32

    @test
    def remainder08(): Bool = (-1200000i32 `Int32.remainder` 500000i32) == -200000i32

    @test
    def remainder09(): Bool = (1200000i32 `Int32.remainder` -500000i32) == 200000i32

    @test
    def remainder10(): Bool = (-1200000i32 `Int32.remainder` -500000i32) == -200000i32

    @test
    def remainder11(): Bool = (1 `Int32.remainder` 0) == 0

    @test
    def remainder12(): Bool = (1i32 `Int32.remainder` 0i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // dist                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def dist01(): Bool = Int32.dist(31, -7) == 38

    @test
    def dist02(): Bool = Int32.dist(-44, -1) == 43

    @test
    def dist03(): Bool = Int32.dist(-2, -2) == 0

    @test
    def dist04(): Bool = Int32.dist(-2147483648, -1) == 2147483647

    @test
    def dist05(): Bool = Int32.dist(-2147483648, 0) == -1

    @test
    def dist06(): Bool = Int32.dist(-123, 4) == 127

    @test
    def dist07(): Bool = Int32.dist(-2147483643, 5) == -1

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def compare01(): Bool = Int32.compare(-1, 44) == -1

    @test
    def compare02(): Bool = Int32.compare(-1, -44) == 1

    @test
    def compare03(): Bool = Int32.compare(88, 88) == 0

    /////////////////////////////////////////////////////////////////////////////
    // signum                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def signum01(): Bool = Int32.signum(-22) == -1

    @test
    def signum02(): Bool = Int32.signum(22) == 1

    @test
    def signum03(): Bool = Int32.signum(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def pow01(): Bool = Int32.pow(0i32, 0i32) == 1i32

    @test
    def pow02(): Bool = Int32.pow(0i32, 1i32) == 0i32

    @test
    def pow03(): Bool = Int32.pow(1i32, 0i32) == 1i32

    @test
    def pow04(): Bool = Int32.pow(1i32, 1i32) == 1i32

    @test
    def pow05(): Bool = Int32.pow(5i32, 2i32) == 25i32

    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseLeftShift01(): Bool = Int32.leftShift(8, 0) == 8

    @test
    def testBitwiseLeftShift02(): Bool = Int32.leftShift(8, 16) == 524288

    @test
    def testBitwiseLeftShift03(): Bool = Int32.leftShift(8, 28) == -2147483648

    @test
    def testBitwiseLeftShift04(): Bool = Int32.leftShift(8, 29) == 0

    @test
    def testBitwiseLeftShift05(): Bool = Int32.leftShift(8i32, 0) == 8i32

    @test
    def testBitwiseLeftShift06(): Bool = Int32.leftShift(8i32, 16) == 524288i32

    @test
    def testBitwiseLeftShift07(): Bool = Int32.leftShift(8i32, 28) == -2147483648i32

    @test
    def testBitwiseLeftShift08(): Bool = Int32.leftShift(8i32, 29) == 0i32


    /////////////////////////////////////////////////////////////////////////////
    // rightShift                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testBitwiseRightShift01(): Bool = Int32.rightShift(120000, 0) == 120000

    @test
    def testBitwiseRightShift02(): Bool = Int32.rightShift(120000, 2) == 30000

    @test
    def testBitwiseRightShift03(): Bool = Int32.rightShift(120000, 31) == 0

    @test
    def testBitwiseRightShift04(): Bool = Int32.rightShift(-120000, 2) == -30000

    @test
    def testBitwiseRightShift05(): Bool = Int32.rightShift(120000i32, 0) == 120000i32

    @test
    def testBitwiseRightShift06(): Bool = Int32.rightShift(120000i32, 2) == 30000i32

    @test
    def testBitwiseRightShift07(): Bool = Int32.rightShift(120000i32, 31) == 0i32

    @test
    def testBitwiseRightShift08(): Bool = Int32.rightShift(-120000i32, 2) == -30000i32

    /////////////////////////////////////////////////////////////////////////////
    // logicalRightShift                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def logicalRightShift01(): Bool = Int32.logicalRightShift(dist = 4, 2233) == Int32.rightShift(2233, 4)

    @test
    def logicalRightShift02(): Bool = Int32.logicalRightShift(dist = 33, 223366) == Int32.rightShift(223366, 33)

    @test
    def logicalRightShift03(): Bool = Int32.logicalRightShift(dist = -8, 2233) == Int32.rightShift(2233, -8)

    @test
    def logicalRightShift04(): Bool = Int32.logicalRightShift(dist = 4, 2233) == Int32.rightShift(2233, 4)

    @test
    def logicalRightShift05(): Bool = Int32.logicalRightShift(dist = 1, -1) == 2147483647

    @test
    def logicalRightShift06(): Bool = Int32.logicalRightShift(dist = 4, -35) == 268435453

    @test
    def logicalRightShift07(): Bool = Int32.logicalRightShift(dist = -2777, -2777) == 33554410

    @test
    def logicalRightShift08(): Bool = Int32.logicalRightShift(dist = 64, -233) == -233

    @test
    def logicalRightShift09(): Bool = Int32.logicalRightShift(dist = 0, -27) == -27

    @test
    def logicalRightShift10(): Bool = Int32.logicalRightShift(dist = 1, 2147483647) == 1073741823

    /////////////////////////////////////////////////////////////////////////////
    // bitCount                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def bitCount01(): Bool = Int32.bitCount(19909) == 8

    @test
    def bitCount02(): Bool = Int32.bitCount(-1) == 32

    @test
    def bitCount03(): Bool = Int32.bitCount(-999888888) == 12

    @test
    def bitCount04(): Bool = Int32.bitCount(0) == 0

    @test
    def bitCount05(): Bool = Int32.bitCount(2147483647) == 31

    @test
    def bitCount06(): Bool = Int32.bitCount(-2147483648) == 1

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateLeft01(): Bool = Int32.rotateLeft(dist = 4, -1698265258) == -1402440343

    @test
    def rotateLeft02(): Bool = Int32.rotateLeft(dist = 5, -1836677290) == 1355868882

    @test
    def rotateLeft03(): Bool = Int32.rotateLeft(dist = 24, 1478293001) == 156769526

    @test
    def rotateLeft04(): Bool = Int32.rotateLeft(dist = -36, -12267711) == 536104180

    @test
    def rotateLeft05(): Bool = Int32.rotateLeft(dist = 87, 0) == 0

    @test
    def rotateLeft06(): Bool = Int32.rotateLeft(dist = 87, -1) == -1

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def rotateRight01(): Bool = Int32.rotateRight(dist = 4, -1698265258) == 1772906613

    @test
    def rotateRight02(): Bool = Int32.rotateRight(dist = 5, -1836677290) == -1265355718

    @test
    def rotateRight03(): Bool = Int32.rotateRight(dist = 24, 1478293001) == 485886296

    @test
    def rotateRight04(): Bool = Int32.rotateRight(dist = -36, -12267711) == -196283361

    @test
    def rotateRight05(): Bool = Int32.rotateRight(dist = 87, 0) == 0

    @test
    def rotateRight06(): Bool = Int32.rotateRight(dist = 87, -1) == -1

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def reverse01(): Bool = Int32.reverse(1789570000) == 198530390

    @test
    def reverse02(): Bool = Int32.reverse(198530390) == 1789570000

    @test
    def reverse03(): Bool = Int32.reverse(-1036473682) == 1968512067

    @test
    def reverse04(): Bool = Int32.reverse(1968512067) == -1036473682

    @test
    def reverse05(): Bool = Int32.reverse(137) == -1862270976

    @test
    def reverse06(): Bool = Int32.reverse(-1862270976) == 137

    @test
    def reverse07(): Bool = Int32.reverse(4) == 536870912

    @test
    def reverse08(): Bool = Int32.reverse(536870912) == 4

    @test
    def reverse09(): Bool = Int32.reverse(-1) == -1

    @test
    def reverse10(): Bool = Int32.reverse(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBitPosition                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBitPosition01(): Bool = Int32.highestOneBitPosition(-1) == 31

    @test
    def highestOneBitPosition02(): Bool = Int32.highestOneBitPosition(-2147483648) == 31

    @test
    def highestOneBitPosition03(): Bool = Int32.highestOneBitPosition(1) == 0

    @test
    def highestOneBitPosition04(): Bool = Int32.highestOneBitPosition(2) == 1

    @test
    def highestOneBitPosition05(): Bool = Int32.highestOneBitPosition(3) == 1

    @test
    def highestOneBitPosition06(): Bool = Int32.highestOneBitPosition(20) == 4

    @test
    def highestOneBitPosition07(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 30)) == 30

    @test
    def highestOneBitPosition08(): Bool = Int32.highestOneBitPosition(2147483647) == 30

    @test
    def highestOneBitPosition09(): Bool = Int32.highestOneBitPosition(0) == -1

    @test
    def highestOneBitPosition10(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 18) + Int32.pow(2, 23)) == 23

    @test
    def highestOneBitPosition11(): Bool = Int32.highestOneBitPosition(Int32.pow(2, 3) + Int32.pow(2, 7)) == 7

    @test
    def highestOneBitPosition12(): Bool = Int32.highestOneBitPosition(-32767) == 31

    /////////////////////////////////////////////////////////////////////////////
    // highestOneBit                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def highestOneBit01(): Bool = Int32.highestOneBit(-1) == Int32.leftShift(1, 31)

    @test
    def highestOneBit02(): Bool = Int32.highestOneBit(-2147483000) == Int32.leftShift(1, 31)

    @test
    def highestOneBit03(): Bool = Int32.highestOneBit(-77) == Int32.leftShift(1, 31)

    @test
    def highestOneBit04(): Bool = Int32.highestOneBit(2) == 2

    @test
    def highestOneBit05(): Bool = Int32.highestOneBit(3) == 2

    @test
    def highestOneBit06(): Bool = Int32.highestOneBit(20) == 16

    @test
    def highestOneBit07(): Bool = Int32.highestOneBit(Int32.pow(2, 30)) == 1073741824

    @test
    def highestOneBit08(): Bool = Int32.highestOneBit(2147483647) == 1073741824

    @test
    def highestOneBit09(): Bool = Int32.highestOneBit(0) == 0

    @test
    def highestOneBit10(): Bool = Int32.highestOneBit(Int32.pow(2, 18) + Int32.pow(2, 24)) == 16777216

    /////////////////////////////////////////////////////////////////////////////
    // numberOfLeadingZeros                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfLeadingZeros01(): Bool = Int32.numberOfLeadingZeros(0) == 32

    @test
    def numberOfLeadingZeros02(): Bool = Int32.numberOfLeadingZeros(Int32.leftShift(1, 31)) == 0

    @test
    def numberOfLeadingZeros03(): Bool = Int32.numberOfLeadingZeros(-1) == 0

    @test
    def numberOfLeadingZeros04(): Bool = Int32.numberOfLeadingZeros(Int32.bitwiseNot(Int32.leftShift(1, 31))) == 1

    @test
    def numberOfLeadingZeros05(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 29)) == 2

    @test
    def numberOfLeadingZeros06(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 24) + Int32.pow(2, 18)) == 7

    @test
    def numberOfLeadingZeros07(): Bool = Int32.numberOfLeadingZeros(Int32.pow(2, 7) + Int32.pow(2, 6)) == 24

    @test
    def numberOfLeadingZeros08(): Bool = Int32.numberOfLeadingZeros(20) == 27

    @test
    def numberOfLeadingZeros09(): Bool = Int32.numberOfLeadingZeros(1) == 31

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBitPosition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBitPosition01(): Bool = Int32.lowestOneBitPosition(-1) == 0

    @test
    def lowestOneBitPosition02(): Bool = Int32.lowestOneBitPosition(-2147483648) == 31

    @test
    def lowestOneBitPosition03(): Bool = Int32.lowestOneBitPosition(1) == 0

    @test
    def lowestOneBitPosition04(): Bool = Int32.lowestOneBitPosition(2) == 1

    @test
    def lowestOneBitPosition05(): Bool = Int32.lowestOneBitPosition(3) == 0

    @test
    def lowestOneBitPosition06(): Bool = Int32.lowestOneBitPosition(4) == 2

    @test
    def lowestOneBitPosition07(): Bool = Int32.lowestOneBitPosition(12) == 2

    @test
    def lowestOneBitPosition08(): Bool = Int32.lowestOneBitPosition(Int32.pow(2, 30)) == 30

    @test
    def lowestOneBitPosition09(): Bool = Int32.lowestOneBitPosition(Int32.pow(2, 23) + Int32.pow(2, 18)) == 18

    @test
    def lowestOneBitPosition10(): Bool = Int32.lowestOneBitPosition(2147483647) == 0

    @test
    def lowestOneBitPosition11(): Bool = Int32.lowestOneBitPosition(0) == -1

    /////////////////////////////////////////////////////////////////////////////
    // lowestOneBit                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def lowestOneBit01(): Bool = Int32.lowestOneBit(-1) == 1

    @test
    def lowestOneBit02(): Bool = Int32.lowestOneBit(-2147483648) == Int32.leftShift(1, 31)

    @test
    def lowestOneBit03(): Bool = Int32.lowestOneBit(1) == 1

    @test
    def lowestOneBit04(): Bool = Int32.lowestOneBit(2) == 2

    @test
    def lowestOneBit05(): Bool = Int32.lowestOneBit(6) == 2

    @test
    def lowestOneBit06(): Bool = Int32.lowestOneBit(12) == 4

    @test
    def lowestOneBit07(): Bool = Int32.lowestOneBit(Int32.pow(2, 6) + Int32.pow(2, 18) + Int32.pow(2, 27)) == 64

    @test
    def lowestOneBit08(): Bool = Int32.lowestOneBit(Int32.pow(2, 30)) == 1073741824

    @test
    def lowestOneBit09(): Bool = Int32.lowestOneBit(Int32.pow(2, 18) + Int32.pow(2, 23)) == 262144

    @test
    def lowestOneBit10(): Bool = Int32.lowestOneBit(2147483647) == 1

    @test
    def lowestOneBit11(): Bool = Int32.lowestOneBit(0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // numberOfTrailingZeros                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def numberOfTrailingZeros01(): Bool = Int32.numberOfTrailingZeros(0) == 32

    @test
    def numberOfTrailingZeros02(): Bool = Int32.numberOfTrailingZeros(3) == 0

    @test
    def numberOfTrailingZeros03(): Bool = Int32.numberOfTrailingZeros(42) == 1

    @test
    def numberOfTrailingZeros04(): Bool = Int32.numberOfTrailingZeros(Int32.pow(2, 16) + Int32.pow(2, 22)) == 16

    @test
    def numberOfTrailingZeros05(): Bool = Int32.numberOfTrailingZeros(Int32.pow(2, 30)) == 30

    @test
    def numberOfTrailingZeros06(): Bool = Int32.numberOfTrailingZeros(-1) == 0

    @test
    def numberOfTrailingZeros07(): Bool = Int32.numberOfTrailingZeros(-2147483648) == 31

    /////////////////////////////////////////////////////////////////////////////
    // getBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getBit01(): Bool = Int32.getBit(pos = 5, -1) == 1

    @test
    def getBit02(): Bool = Int32.getBit(pos = 31, -1000000) == 1

    @test
    def getBit03(): Bool = Int32.getBit(pos = 23, Int32.pow(2, 23)) == 1

    @test
    def getBit04(): Bool = Int32.getBit(pos = 24, Int32.pow(2, 23)) == 0

    @test
    def getBit05(): Bool = Int32.getBit(pos = 22, Int32.pow(2, 23)) == 0

    @test
    def getBit06(): Bool = Int32.getBit(pos = 25, -1 - Int32.pow(2, 25)) == 0

    @test
    def getBit07(): Bool = Int32.getBit(pos = 26, -1 - Int32.pow(2, 25)) == 1

    @test
    def getBit08(): Bool = Int32.getBit(pos = 15, 0) == 0

    /////////////////////////////////////////////////////////////////////////////
    // setBit                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def setBit01(): Bool = Int32.setBit(pos = 0, 0) == 1

    @test
    def setBit02(): Bool = Int32.setBit(pos = 1, 0) == 2

    @test
    def setBit03(): Bool = Int32.setBit(pos = 31, 0) == -2147483648

    @test
    def setBit04(): Bool = Int32.setBit(pos = 30, 0) == 1073741824

    @test
    def setBit05(): Bool = Int32.setBit(pos = 31, -1418638422) == -1418638422

    @test
    def setBit07(): Bool = Int32.setBit(pos = 4, -17) == -1

    @test
    def setBit08(): Bool = Int32.setBit(pos = 10, -1025) == -1

    @test
    def setBit09(): Bool = Int32.setBit(pos = 31, -17) == -17

    @test
    def setBit10(): Bool = Int32.setBit(pos = 30, -17) == -17

    @test
    def setBit11(): Bool = Int32.setBit(pos = 32, 0) == 1

    /////////////////////////////////////////////////////////////////////////////
    // clearBit                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clearBit01(): Bool = Int32.clearBit(pos = 0, 1) == 0

    @test
    def clearBit02(): Bool = Int32.clearBit(pos = 1, 1) == 1

    @test
    def clearBit03(): Bool = Int32.clearBit(pos = 1, 2) == 0

    @test
    def clearBit04(): Bool = Int32.clearBit(pos = 2, 2) == 2

    @test
    def clearBit05(): Bool = Int32.clearBit(pos = 31, 0) == 0

    @test
    def clearBit06(): Bool = Int32.clearBit(pos = 30, 0) == 0

    @test
    def clearBit07(): Bool = Int32.clearBit(pos = 31, -2147483648) == 0

    @test
    def clearBit08(): Bool = Int32.clearBit(pos = 27, 442931848) == 308714120

    @test
    def clearBit09(): Bool = Int32.clearBit(pos = 0, -1) == -2

    @test
    def clearBit10(): Bool = Int32.clearBit(pos = 1, -1) == -3

    @test
    def clearBit11(): Bool = Int32.clearBit(pos = 5, -1) == -33

    @test
    def clearBit12(): Bool = Int32.clearBit(pos = 31, -1) == 2147483647

    @test
    def clearBit13(): Bool = Int32.clearBit(pos = 32, -1) == -2

    /////////////////////////////////////////////////////////////////////////////
    // flipBit                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flipBit01(): Bool = Int32.flipBit(pos = 0, 1) == 0

    @test
    def flipBit02(): Bool = Int32.flipBit(pos = 1, 1) == 3

    @test
    def flipBit03(): Bool = Int32.flipBit(pos = 1, 2) == 0

    @test
    def flipBit04(): Bool = Int32.flipBit(pos = 2, 2) == 6

    @test
    def flipBit05(): Bool = Int32.flipBit(pos = 31, 0) == -2147483648

    @test
    def flipBit06(): Bool = Int32.flipBit(pos = 30, 0) == 1073741824

    @test
    def flipBit07(): Bool = Int32.flipBit(pos = 31, -2147483648) == 0

    @test
    def flipBit08(): Bool = Int32.flipBit(pos = 0, -1) == -2

    @test
    def flipBit9(): Bool = Int32.flipBit(pos = 1, -1) == -3

    @test
    def flipBit10(): Bool = Int32.flipBit(pos = 5, -1) == -33

    @test
    def flipBit11(): Bool = Int32.flipBit(pos = 31, -1) == 2147483647

    @test
    def flipBit12(): Bool = Int32.flipBit(pos = 32, -1) == -2

    /////////////////////////////////////////////////////////////////////////////
    // log2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def log201(): Bool = Int32.log2(10) == 3

    @test
    def log202(): Bool = Int32.log2(10000) == 13

    @test
    def log203(): Bool = Int32.log2(8) == 3

    @test
    def log204(): Bool = Int32.log2(256) == 8

    @test
    def log205(): Bool = Int32.log2(1) == 0

    @test
    def log206(): Bool = Int32.log2(12345678) == 23

    @test
    def log207(): Bool = Int32.log2(20) == 4

    @test
    def log208(): Bool = Int32.log2(0) == 0

    @test
    def log209(): Bool = Int32.log2(-1) == 0

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseAnd                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseAnd01(): Bool = Int32.bitwiseAnd(0i32, 0i32) == 0i32

    @test
    def bitwiseAnd02(): Bool = Int32.bitwiseAnd(127i32, 0i32) == 0i32

    @test
    def bitwiseAnd03(): Bool = Int32.bitwiseAnd(-128i32, 0i32) == 0i32

    @test
    def bitwiseAnd04(): Bool = Int32.bitwiseAnd(0i32, 1i32) == 0i32

    @test
    def bitwiseAnd05(): Bool = Int32.bitwiseAnd(127i32, 1i32) == 1i32

    @test
    def bitwiseAnd06(): Bool = Int32.bitwiseAnd(-128i32, 1i32) == 0i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseNot                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseNot01(): Bool = Int32.bitwiseNot(0i32) == -1i32

    @test
    def bitwiseNot02(): Bool = Int32.bitwiseNot(1i32) == -2i32

    @test
    def bitwiseNot03(): Bool = Int32.bitwiseNot(-1i32) == -0i32

    @test
    def bitwiseNot04(): Bool = Int32.bitwiseNot(127i32) == -128i32

    @test
    def bitwiseNot05(): Bool = Int32.bitwiseNot(-128i32) == 127i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseOr                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseOr01(): Bool = Int32.bitwiseOr(0i32, 0i32) == 0i32

    @test
    def bitwiseOr02(): Bool = Int32.bitwiseOr(127i32, 0i32) == 127i32

    @test
    def bitwiseOr03(): Bool = Int32.bitwiseOr(-128i32, 0i32) == -128i32

    @test
    def bitwiseOr04(): Bool = Int32.bitwiseOr(0i32, 1i32) == 1i32

    @test
    def bitwiseOr05(): Bool = Int32.bitwiseOr(127i32, 1i32) == 127i32

    @test
    def bitwiseOr06(): Bool = Int32.bitwiseOr(-128i32, 1i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // bitwiseXor                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bitwiseXor01(): Bool = Int32.bitwiseXor(0i32, 0i32) == 0i32

    @test
    def bitwiseXor02(): Bool = Int32.bitwiseXor(127i32, 0i32) == 127i32

    @test
    def bitwiseXor03(): Bool = Int32.bitwiseXor(-128i32, 0i32) == -128i32

    @test
    def bitwiseXor04(): Bool = Int32.bitwiseXor(0i32, 1i32) == 1i32

    @test
    def bitwiseXor05(): Bool = Int32.bitwiseXor(127i32, 1i32) == 126i32

    @test
    def bitwiseXor06(): Bool = Int32.bitwiseXor(-128i32, 1i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // factorial                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def factorial01(): Bool = Int32.factorial(0) == 1

    @test
    def factorial02(): Bool = Int32.factorial(1) == 1

    @test
    def factorial03(): Bool = Int32.factorial(2) == 2

    @test
    def factorial04(): Bool = Int32.factorial(3) == 6

    @test
    def factorial05(): Bool = Int32.factorial(4) == 24

    @test
    def factorial06(): Bool = Int32.factorial(5) == 120

    @test
    def factorial07(): Bool = Int32.factorial(10) == 3628800

    @test
    def factorial08(): Bool = Int32.factorial(-1) == 0

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Int32.toString(0) == "0"

    @test
    def toString02(): Bool = Int32.toString(1) == "1"

    @test
    def toString03(): Bool = Int32.toString(-1) == "-1"

    @test
    def toString04(): Bool = Int32.toString(2147483647) == "2147483647"

    @test
    def toString05(): Bool = Int32.toString(-2147483648) == "-2147483648"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Int32.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = Int32.fromString(" 1") == Some(1)

    @test
    def fromString03(): Bool = Int32.fromString("1 ") == Some(1)

    @test
    def fromString04(): Bool = Int32.fromString("0") == Some(0)

    @test
    def fromString05(): Bool = Int32.fromString("1") == Some(1)

    @test
    def fromString06(): Bool = Int32.fromString("-1") == Some(-1)

    @test
    def fromString07(): Bool = Int32.fromString("2147483647") == Some(2147483647)

    @test
    def fromString08(): Bool = Int32.fromString("-2147483648") == Some(-2147483648)

    /////////////////////////////////////////////////////////////////////////////
    // parse                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def parse01(): Bool = ("0" |> Int32.parse(10)) == Ok(0)

    @test
    def parse02(): Bool = ("     0      " |> Int32.parse(10)) == Ok(0)

    @test
    def parse03(): Bool = ("-FF" |> Int32.parse(16)) == Ok(-255)

    @test
    def parse04(): Bool = ("1100110" |> Int32.parse(2)) == Ok(102)

    @test
    def parse05(): Bool = ("Kona" |> Int32.parse(27)) == Ok(411787)

    @test
    def parse06(): Bool = ("2147483648" |> Int32.parse(10)) |> Result.isErr

    @test
    def parse07(): Bool = ("Kona" |> Int32.parse(10)) |> Result.isErr

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Int32.tryToInt8(0) == Some(0i8)

    @test
    def tryToInt802(): Bool = Int32.tryToInt8(1) == Some(1i8)

    @test
    def tryToInt803(): Bool = Int32.tryToInt8(-1) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Int32.tryToInt8(127) == Some(127i8)

    @test
    def tryToInt805(): Bool = Int32.tryToInt8(128) == None

    @test
    def tryToInt806(): Bool = Int32.tryToInt8(-128) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Int32.tryToInt8(-129) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = Int32.tryToInt16(0) == Some(0i16)

    @test
    def tryToInt1602(): Bool = Int32.tryToInt16(1) == Some(1i16)

    @test
    def tryToInt1603(): Bool = Int32.tryToInt16(-1) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = Int32.tryToInt16(32767) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = Int32.tryToInt16(32768) == None

    @test
    def tryToInt1606(): Bool = Int32.tryToInt16(-32768) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = Int32.tryToInt16(-32769) == None

    /////////////////////////////////////////////////////////////////////////////
    // toInt64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toInt6401(): Bool = Int32.toInt64(0) == 0i64

    @test
    def toInt6402(): Bool = Int32.toInt64(1) == 1i64

    @test
    def toInt6403(): Bool = Int32.toInt64(-1) == -1i64

    @test
    def toInt6404(): Bool = Int32.toInt64(2147483647) == 2147483647i64

    @test
    def toInt6405(): Bool = Int32.toInt64(-2147483648) == -2147483648i64

    /////////////////////////////////////////////////////////////////////////////
    // toBigInt                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigInt01(): Bool = Int32.toBigInt(0) == 0ii

    @test
    def toBigInt02(): Bool = Int32.toBigInt(1) == 1ii

    @test
    def toBigInt03(): Bool = Int32.toBigInt(-1) == -1ii

    @test
    def toBigInt04(): Bool = Int32.toBigInt(2147483647) == 2147483647ii

    @test
    def toBigInt05(): Bool = Int32.toBigInt(-2147483648) == -2147483648ii

    /////////////////////////////////////////////////////////////////////////////
    // toFloat32                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat3201(): Bool = Int32.toFloat32(0) == 0.0f32

    @test
    def toFloat3202(): Bool = Int32.toFloat32(1) == 1.0f32

    @test
    def toFloat3203(): Bool = Int32.toFloat32(-1) == -1.0f32

    @test
    def toFloat3204(): Bool = Int32.toFloat32(2147483647) == 2147483647.0f32

    @test
    def toFloat3205(): Bool = Int32.toFloat32(-2147483648) == -2147483648.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Int32.toFloat64(0) == 0.0f64

    @test
    def toFloat6402(): Bool = Int32.toFloat64(1) == 1.0f64

    @test
    def toFloat6403(): Bool = Int32.toFloat64(-1) == -1.0f64

    @test
    def toFloat6404(): Bool = Int32.toFloat64(2147483647) == 2147483647.0f64

    @test
    def toFloat6405(): Bool = Int32.toFloat64(-2147483648) == -2147483648.0f64

    /////////////////////////////////////////////////////////////////////////////
    // toBigDecimal                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toBigDecimal01(): Bool = Int32.toBigDecimal(0) == 0.0ff

    @test
    def toBigDecimal02(): Bool = Int32.toBigDecimal(1) == 1.0ff

    @test
    def toBigDecimal03(): Bool = Int32.toBigDecimal(-1) == -1.0ff

    @test
    def toBigDecimal04(): Bool = Int32.toBigDecimal(2147483647) == 2147483647.0ff

    @test
    def toBigDecimal05(): Bool = Int32.toBigDecimal(-2147483648) == -2147483648.0ff

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 0) == 0i8

    @test
    def clampToInt802(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 100) == 100i8

    @test
    def clampToInt803(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, 101) == 100i8

    @test
    def clampToInt804(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, -100) == -100i8

    @test
    def clampToInt805(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, -101) == -100i8

    @test
    def clampToInt806(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, Int32.maxValue()) == 100i8

    @test
    def clampToInt807(): Bool = Int32.clampToInt8(min = -100i8, max = 100i8, Int32.minValue()) == -100i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 0) == 0i16

    @test
    def clampToInt1602(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 100) == 100i16

    @test
    def clampToInt1603(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, 101) == 100i16

    @test
    def clampToInt1604(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, -100) == -100i16

    @test
    def clampToInt1605(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, -101) == -100i16

    @test
    def clampToInt1606(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, Int32.maxValue()) == 100i16

    @test
    def clampToInt1607(): Bool = Int32.clampToInt16(min = -100i16, max = 100i16, Int32.minValue()) == -100i16

    /////////////////////////////////////////////////////////////////////////////
    // intValue                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intValue01(): Bool =
        let i = Int32.valueOf(0);
        Int32.intValue(i) == 0

    @test
    def intValue02(): Bool =
        let i = Int32.valueOf(1);
        Int32.intValue(i) == 1

    @test
    def intValue03(): Bool =
        let i = Int32.valueOf(-1);
        Int32.intValue(i) == -1

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valueOf01(): Bool =
        let i = Int32.valueOf(0);
        unsafely IO run i.equals(Int32.valueOf(0))

    @test
    def valueOf02(): Bool =
        let i = Int32.valueOf(1);
        unsafely IO run i.equals(Int32.valueOf(1))

    @test
    def valueOf03(): Bool =
        let i = Int32.valueOf(-1);
        unsafely IO run i.equals(Int32.valueOf(-1))

}
