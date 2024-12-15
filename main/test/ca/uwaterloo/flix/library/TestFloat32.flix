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

mod TestFloat32 {

    use Mul.mul

    /////////////////////////////////////////////////////////////////////////////
    // mul                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mul01(): Bool = mul(1.0f32, 1.0f32) == 1.0f32

    @test
    def mul02(): Bool = mul(17.0f32, 0.0f32) == 0.0f32

    @test
    def mul03(): Bool = mul(0.0f32, 32.0f32) == 0.0f32

    @test
    def mul04(): Bool = mul(123.123f32, 2.0f32) == 246.246f32

    @test
    def mul05(): Bool = mul(5.0f32, 223.0f32) == 1115.0f32

    @test
    def mul06(): Bool = mul(0.0f32, 0.0f32) == 0.0f32

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Float32.size() == 32

    /////////////////////////////////////////////////////////////////////////////
    // minExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minExponent01(): Bool = Float32.minExponent() == -126

    /////////////////////////////////////////////////////////////////////////////
    // maxExponent                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxExponent01(): Bool = Float32.maxExponent() == 127

    /////////////////////////////////////////////////////////////////////////////
    // maxValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def maxValue01(): Bool = Float32.maxValue() == ((2.0f32 - Float32.pow(2.0f32, -23.0f32)) * Float32.pow(2.0f32, 127.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // minValue                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minValue01(): Bool = Float32.minValue() == (-(2.0f32 - Float32.pow(2.0f32, -23.0f32)) * Float32.pow(2.0f32, 127.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // minPositiveValue                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def minPositiveValue01(): Bool = Float32.minPositiveValue() == Float32.pow(2.0f32, -149.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // nan                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def nan01(): Bool = Float32.isNan(Float32.nan()) == true

    /////////////////////////////////////////////////////////////////////////////
    // positiveInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def positiveInfinity01(): Bool = Float32.positiveInfinity() == (1.0f32 / 0.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // negativeInfinity                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def negativeInfinity01(): Bool = Float32.negativeInfinity() == (-1.0f32 / 0.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // isFinite                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isFinite01(): Bool = Float32.isFinite(1.0f32) == true

    @test
    def isFinite02(): Bool = Float32.isFinite(-1.0f32) == true

    @test
    def isFinite03(): Bool = Float32.isFinite(Float32.minValue()) == true

    @test
    def isFinite04(): Bool = Float32.isFinite(Float32.maxValue()) == true

    @test
    def isFinite05(): Bool = Float32.isFinite(Float32.negativeInfinity()) == false

    @test
    def isFinite06(): Bool = Float32.isFinite(Float32.positiveInfinity()) == false

    @test
    def isFinite07(): Bool = Float32.isFinite(Float32.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isInfinite                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isInfinite01(): Bool = Float32.isInfinite(1.0f32) == false

    @test
    def isInfinite02(): Bool = Float32.isInfinite(-1.0f32) == false

    @test
    def isInfinite03(): Bool = Float32.isInfinite(Float32.minValue()) == false

    @test
    def isInfinite04(): Bool = Float32.isInfinite(Float32.maxValue()) == false

    @test
    def isInfinite05(): Bool = Float32.isInfinite(Float32.negativeInfinity()) == true

    @test
    def isInfinite06(): Bool = Float32.isInfinite(Float32.positiveInfinity()) == true

    @test
    def isInfinite07(): Bool = Float32.isInfinite(Float32.nan()) == false

    /////////////////////////////////////////////////////////////////////////////
    // isNan                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isNan01(): Bool = Float32.isNan(Float32.nan()) == true

    @test
    def isNan02(): Bool = Float32.isNan(0.0f32 / 0.0f32) == true

    @test
    def isNan03(): Bool = Float32.isNan(Float32.positiveInfinity()) == false

    @test
    def isNan04(): Bool = Float32.isNan(Float32.negativeInfinity()) == false

    @test
    def isNan05(): Bool = Float32.isNan(1.0f32) == false

    @test
    def isNan06(): Bool = Float32.isNan(-1.0f32) == false

    @test
    def isNan07(): Bool = Float32.isNan(Float32.minValue()) == false

    @test
    def isNan08(): Bool = Float32.isNan(Float32.maxValue()) == false

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def min01(): Bool = Float32.min(1.0f32, 2.0f32) == 1.0f32

    @test
    def min02(): Bool = Float32.min(2.0f32, -1.0f32) == -1.0f32

    @test
    def min03(): Bool = Float32.min(-33.0f32, -66.0f32) == -66.0f32

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def max01(): Bool = Float32.max(48.0f32, 49.0f32) == 49.0f32

    @test
    def max02(): Bool = Float32.max(4.0f32, -16.0f32) == 4.0f32

    @test
    def max03(): Bool = Float32.max(-34.0f32, -16.0f32) == -16.0f32

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toString01(): Bool = Float32.toString(0.0f32) == "0.0"

    @test
    def toString02(): Bool = Float32.toString(1.0f32) == "1.0"

    @test
    def toString03(): Bool = Float32.toString(-1.0f32) == "-1.0"

    @test
    def toString04(): Bool = Float32.toString(3.14f32) == "3.14"

    @test
    def toString05(): Bool = Float32.toString(-3.14f32) == "-3.14"

    @test
    def toString06(): Bool = Float32.toString(Float32.nan()) == "NaN"

    @test
    def toString07(): Bool = Float32.toString(Float32.positiveInfinity()) == "Infinity"

    @test
    def toString08(): Bool = Float32.toString(Float32.negativeInfinity()) == "-Infinity"

    /////////////////////////////////////////////////////////////////////////////
    // fromString                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def fromString01(): Bool = Float32.fromString("Bad") |> Option.isEmpty

    @test
    def fromString02(): Bool = Float32.fromString(" 1.0") == Some(1.0f32)

    @test
    def fromString03(): Bool = Float32.fromString("1.0 ") == Some(1.0f32)

    @test
    def fromString04(): Bool = Float32.fromString("0.0") == Some(0.0f32)

    @test
    def fromString05(): Bool = Float32.fromString("1.0") == Some(1.0f32)

    @test
    def fromString06(): Bool = Float32.fromString("-1.0") == Some(-1.0f32)

    @test
    def fromString07(): Bool = Float32.fromString("3.14") == Some(3.14f32)

    @test
    def fromString08(): Bool = Float32.fromString("-3.14") == Some(-3.14f32)

    @test
    def fromString09(): Bool = match Float32.fromString("NaN") {
        case Some(x)  => Float32.isNan(x)
        case _ => false
    }

    @test
    def fromString10(): Bool = Float32.fromString("Infinity") == Some(Float32.positiveInfinity())

    @test
    def fromString11(): Bool = Float32.fromString("-Infinity") == Some(Float32.negativeInfinity())

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt8                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt801(): Bool = Float32.tryToInt8(0.0f32) == Some(0i8)

    @test
    def tryToInt802(): Bool = Float32.tryToInt8(1.0f32) == Some(1i8)

    @test
    def tryToInt803(): Bool = Float32.tryToInt8(-1.0f32) == Some(-1i8)

    @test
    def tryToInt804(): Bool = Float32.tryToInt8(127.0f32) == Some(127i8)

    @test
    def tryToInt805(): Bool = Float32.tryToInt8(128.0f32) == None

    @test
    def tryToInt806(): Bool = Float32.tryToInt8(-128.0f32) == Some(-128i8)

    @test
    def tryToInt807(): Bool = Float32.tryToInt8(-129.0f32) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt16                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt1601(): Bool = Float32.tryToInt16(0.0f32) == Some(0i16)

    @test
    def tryToInt1602(): Bool = Float32.tryToInt16(1.0f32) == Some(1i16)

    @test
    def tryToInt1603(): Bool = Float32.tryToInt16(-1.0f32) == Some(-1i16)

    @test
    def tryToInt1604(): Bool = Float32.tryToInt16(32767.0f32) == Some(32767i16)

    @test
    def tryToInt1605(): Bool = Float32.tryToInt16(32768.0f32) == None

    @test
    def tryToInt1606(): Bool = Float32.tryToInt16(-32768.0f32) == Some(-32768i16)

    @test
    def tryToInt1607(): Bool = Float32.tryToInt16(-32769.0f32) == None

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt32                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt3201(): Bool = Float32.tryToInt32(0.0f32) == Some(0)

    @test
    def tryToInt3202(): Bool = Float32.tryToInt32(1.0f32) == Some(1)

    @test
    def tryToInt3203(): Bool = Float32.tryToInt32(-1.0f32) == Some(-1)

    @test
    def tryToInt3204(): Bool = Float32.tryToInt32(2147483647.0f32) != None

    @test
    def tryToInt3205(): Bool = Float32.tryToInt32(2147484000.0f32) == None        // Allow imprecision

    @test
    def tryToInt3206(): Bool = Float32.tryToInt32(-2147483648.0f32) != None

    @test
    def tryToInt3207(): Bool = Float32.tryToInt32(-2147484000.0f32) == None       // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToInt64                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToInt6401(): Bool = Float32.tryToInt64(0.0f32) == Some(0i64)

    @test
    def tryToInt6402(): Bool = Float32.tryToInt64(1.0f32) == Some(1i64)

    @test
    def tryToInt6403(): Bool = Float32.tryToInt64(-1.0f32) == Some(-1i64)

    @test
    def tryToInt6404(): Bool = Float32.tryToInt64(9223372036854775807.0f32) != None

    @test
    def tryToInt6405(): Bool = Float32.tryToInt64(9223373000000000000.0f32) == None        // Allow large imprecision

    @test
    def tryToInt6406(): Bool = Float32.tryToInt64(-9223372036854775808.0f32) != None

    @test
    def tryToInt6407(): Bool = Float32.tryToInt64(-9223373000000000000.0f32) == None       // Allow large imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigInt                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToBigInt01(): Bool = Float32.tryToBigInt(0.0f32) == Some(0ii)

    @test
    def tryToBigInt02(): Bool = Float32.tryToBigInt(1.0f32) == Some(1ii)

    @test
    def tryToBigInt03(): Bool = Float32.tryToBigInt(-1.0f32) == Some(-1ii)

    @test
    def tryToBigInt04(): Bool =
        let step = () -> Float32.maxValue() |> Float32.tryToBigInt;
        step() != None

    @test
    def tryToBigInt05(): Bool =
        let step = () -> Float32.minValue() |> Float32.tryToBigInt;
        step() != None

    /////////////////////////////////////////////////////////////////////////////
    // toFloat64                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toFloat6401(): Bool = Float32.toFloat64(0.0f32) == 0.0f64

    @test
    def toFloat6402(): Bool = Float32.toFloat64(1.0f32) == 1.0f64

    @test
    def toFloat6403(): Bool = Float32.toFloat64(-1.0f32) == -1.0f64

    @test
    def toFloat6404(): Bool =
        let d = Float32.toFloat64(123456789.10f32);
        (d >= 123456700.0f64 and d <= 123456800.0f64)       // Allow imprecision

    @test
    def toFloat6405(): Bool =
        let d = Float32.toFloat64(-123456789.10f32);
        ( d <= -123456700.0f64 and d >= -123456800.0f64)     // Allow imprecision

    /////////////////////////////////////////////////////////////////////////////
    // tryToBigDecimal                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def tryToBigDecimal01(): Bool = Float32.tryToBigDecimal(0.0f32) == Some(0.0ff)

    @test
    def tryToBigDecimal02(): Bool = Float32.tryToBigDecimal(1.0f32) == Some(1.0ff)

    @test
    def tryToBigDecimal03(): Bool = Float32.tryToBigDecimal(-1.0f32) == Some(-1.0ff)

    @test
    def tryToBigDecimal04(): Bool =
        match Float32.tryToBigDecimal(123456789.10f32) {
            case Some(d) => (d >= 123456700.0ff and d <= 123456800.0ff)       // Allow imprecision
            case None    => false
        }

    @test
    def tryToBigDecimal05(): Bool =
        match Float32.tryToBigDecimal(-123456789.10f32) {
            case Some(d) => ( d <= -123456700.0ff and d >= -123456800.0ff)     // Allow imprecision
            case None    => false
        }

    @test
    def tryToBigDecimal06(): Bool =
        match Float32.tryToBigDecimal(Float32.nan()) {
            case Some(_) => false
            case None    => true
        }

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt8                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt801(): Bool = Float32.clampToInt8(0.0f32, -100i8, 100i8, -127i8) == 0i8

    @test
    def clampToInt802(): Bool = Float32.clampToInt8(100.0f32, -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt803(): Bool = Float32.clampToInt8(101.0f32, -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt804(): Bool = Float32.clampToInt8(-100.0f32, -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt805(): Bool = Float32.clampToInt8(-101.0f32, -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt806(): Bool = Float32.clampToInt8(Float32.maxValue(), -100i8, 100i8, -127i8) == 100i8

    @test
    def clampToInt807(): Bool = Float32.clampToInt8(Float32.minValue(), -100i8, 100i8, -127i8) == -100i8

    @test
    def clampToInt808(): Bool = Float32.clampToInt8(Float32.nan(), -100i8, 100i8, -127i8) == -127i8

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt16                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt1601(): Bool = Float32.clampToInt16(0.0f32, -100i16, 100i16, -127i16) == 0i16

    @test
    def clampToInt1602(): Bool = Float32.clampToInt16(100.0f32, -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1603(): Bool = Float32.clampToInt16(101.0f32, -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1604(): Bool = Float32.clampToInt16(-100.0f32, -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1605(): Bool = Float32.clampToInt16(-101.0f32, -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1606(): Bool = Float32.clampToInt16(Float32.maxValue(), -100i16, 100i16, -127i16) == 100i16

    @test
    def clampToInt1607(): Bool = Float32.clampToInt16(Float32.minValue(), -100i16, 100i16, -127i16) == -100i16

    @test
    def clampToInt1608(): Bool = Float32.clampToInt16(Float32.nan(), -100i16, 100i16, -127i16) == -127i16

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt32                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt3201(): Bool = Float32.clampToInt32(0.0f32, -100i32, 100i32, -127i32) == 0i32

    @test
    def clampToInt3202(): Bool = Float32.clampToInt32(100.0f32, -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3203(): Bool = Float32.clampToInt32(101.0f32, -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3204(): Bool = Float32.clampToInt32(-100.0f32, -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3205(): Bool = Float32.clampToInt32(-101.0f32, -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3206(): Bool = Float32.clampToInt32(Float32.maxValue(), -100i32, 100i32, -127i32) == 100i32

    @test
    def clampToInt3207(): Bool = Float32.clampToInt32(Float32.minValue(), -100i32, 100i32, -127i32) == -100i32

    @test
    def clampToInt3208(): Bool = Float32.clampToInt32(Float32.nan(), -100i32, 100i32, -127i32) == -127i32

    /////////////////////////////////////////////////////////////////////////////
    // clampToInt64                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def clampToInt6401(): Bool = Float32.clampToInt64(0.0f32, -100i64, 100i64, -127i64) == 0i64

    @test
    def clampToInt6402(): Bool = Float32.clampToInt64(100.0f32, -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6403(): Bool = Float32.clampToInt64(101.0f32, -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6404(): Bool = Float32.clampToInt64(-100.0f32, -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6405(): Bool = Float32.clampToInt64(-101.0f32, -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6406(): Bool = Float32.clampToInt64(Float32.maxValue(), -100i64, 100i64, -127i64) == 100i64

    @test
    def clampToInt6407(): Bool = Float32.clampToInt64(Float32.minValue(), -100i64, 100i64, -127i64) == -100i64

    @test
    def clampToInt6408(): Bool = Float32.clampToInt64(Float32.nan(), -100i64, 100i64, -127i64) == -127i64

    /////////////////////////////////////////////////////////////////////////////
    // abs                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    def equalsEps(x: Float32, y: Float32, eps: Float32): Bool =
        let diff = Float32.abs(x - y);
        let x1 = Float32.abs(x);
        let y1 = Float32.abs(y);
        let largest = if (y1 > x1) y1 else x1;
        if (diff <= largest * eps) true else false

    @test
    def abs01(): Bool =
        equalsEps(Float32.abs(0.0f32), 0.0f32, 0.001f32)

    @test
    def abs02(): Bool =
        equalsEps(Float32.abs(10.1f32), 10.1f32, 0.001f32)

    @test
    def abs03(): Bool =
        equalsEps(Float32.abs(-10.1f32), 10.1f32, 0.001f32)

    @test
    def abs04(): Bool =
        Float32.abs(Float32.nan()) |> Float32.isNan

    @test
    def abs05(): Bool =
        Float32.abs(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @test
    def abs06(): Bool =
        Float32.abs(Float32.negativeInfinity()) == Float32.positiveInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // ceil                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def ceil01(): Bool =
        equalsEps(Float32.ceil(0.5f32), 1.0f32, 0.001f32)

    @test
    def ceil02(): Bool =
        equalsEps(Float32.ceil(0.1f32), 1.0f32, 0.001f32)

    @test
    def ceil03(): Bool =
        equalsEps(Float32.ceil(1.0f32), 1.0f32, 0.001f32)

    @test
    def ceil04(): Bool =
        equalsEps(Float32.ceil(0.0f32), 0.0f32, 0.001f32)

    @test
    def ceil05(): Bool =
        equalsEps(Float32.ceil(-0.1f32), 0.0f32, 0.001f32)

    @test
    def ceil06(): Bool =
        equalsEps(Float32.ceil(-0.9f32), 0.0f32, 0.001f32)

    @test
    def ceil07(): Bool =
        Float32.ceil(Float32.nan()) |> Float32.isNan

    @test
    def ceil08(): Bool =
        Float32.ceil(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @test
    def ceil09(): Bool =
        Float32.ceil(Float32.negativeInfinity()) == Float32.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // floor                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def floor01(): Bool =
        equalsEps(Float32.floor(0.5f32), 0.0f32, 0.001f32)

    @test
    def floor02(): Bool =
        equalsEps(Float32.floor(0.1f32), 0.0f32, 0.001f32)

    @test
    def floor03(): Bool =
        equalsEps(Float32.floor(1.0f32), 1.0f32, 0.001f32)

    @test
    def floor04(): Bool =
        equalsEps(Float32.floor(0.0f32), 0.0f32, 0.001f32)

    @test
    def floor05(): Bool =
        equalsEps(Float32.floor(-0.1f32), -1.0f32, 0.001f32)

    @test
    def floor06(): Bool =
        equalsEps(Float32.floor(-0.9f32), -1.0f32, 0.001f32)

    @test
    def floor07(): Bool =
        Float32.floor(Float32.nan()) |> Float32.isNan

    @test
    def floor08(): Bool =
        Float32.floor(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @test
    def floor09(): Bool =
        Float32.floor(Float32.negativeInfinity()) == Float32.negativeInfinity()

    /////////////////////////////////////////////////////////////////////////////
    // round                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def round01(): Bool =
        equalsEps(Float32.round(0.5f32), 0.0f32, 0.001f32)

    @test
    def round02(): Bool =
        equalsEps(Float32.round(0.1f32), 0.0f32, 0.001f32)

    @test
    def round03(): Bool =
        equalsEps(Float32.round(1.0f32), 1.0f32, 0.001f32)

    @test
    def round04(): Bool =
        equalsEps(Float32.round(0.0f32), 0.0f32, 0.001f32)

    @test
    def round05(): Bool =
        equalsEps(Float32.round(-0.1f32), 0.0f32, 0.001f32)

    @test
    def round06(): Bool =
        equalsEps(Float32.round(-0.9f32), -1.0f32, 0.001f32)

    @test
    def round07(): Bool =
        Float32.round(Float32.nan()) |> Float32.isNan

    @test
    def round08(): Bool =
        Float32.round(Float32.positiveInfinity()) == Float32.positiveInfinity()

    @test
    def round09(): Bool =
        Float32.round(Float32.negativeInfinity()) == Float32.negativeInfinity()

    @test
    def round10(): Bool =
        equalsEps(Float32.round(1.5f32), 2.0f32, 0.001f32)

    @test
    def round11(): Bool =
        equalsEps(Float32.round(2.5f32), 2.0f32, 0.001f32)

    /////////////////////////////////////////////////////////////////////////////
    // pow                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def pow01(): Bool = equalsEps(Float32.pow(0.0f32, 0.0f32), 1.0f32, 0.001f32)

    @test
    def pow02(): Bool = equalsEps(Float32.pow(0.0f32, 1.0f32), 0.0f32, 0.001f32)

    @test
    def pow03(): Bool = equalsEps(Float32.pow(1.0f32, 0.0f32), 1.0f32, 0.001f32)

    @test
    def pow04(): Bool = equalsEps(Float32.pow(1.0f32, 1.0f32), 1.0f32, 0.001f32)

    @test
    def pow05(): Bool = equalsEps(Float32.pow(5.0f32, 2.0f32), 25.0f32, 0.001f32)

    /////////////////////////////////////////////////////////////////////////////
    // floatValue                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def floatValue01(): Bool =
        let d = Float32.valueOf(0.0f32);
        Float32.floatValue(d) == 0.0f32

    @test
    def floatValue02(): Bool =
        let d = Float32.valueOf(1.0f32);
        Float32.floatValue(d) == 1.0f32

    @test
    def floatValue03(): Bool =
        let d = Float32.valueOf(-1.0f32);
        Float32.floatValue(d) == -1.0f32

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valueOf01(): Bool =
        let d = Float32.valueOf(0.0f32);
        unsafely IO run d.equals(Float32.valueOf(0.0f32))

    @test
    def valueOf02(): Bool =
        let d = Float32.valueOf(1.0f32);
        unsafely IO run d.equals(Float32.valueOf(1.0f32))

    @test
    def valueOf03(): Bool =
        let d = Float32.valueOf(-1.0f32);
        unsafely IO run d.equals(Float32.valueOf(-1.0f32))

}
