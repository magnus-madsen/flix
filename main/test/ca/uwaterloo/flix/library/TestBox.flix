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

mod TestBox {

    use Box.{box, unbox}
    import java.lang.Boolean
    import java.lang.Character
    import java.lang.Float
    import java.lang.Double
    import java.lang.Byte
    import java.lang.Short
    import java.lang.Integer
    import java.lang.Long

    /////////////////////////////////////////////////////////////////////////////
    // Bool                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def boolUnbox01(): Bool = unbox(Boolean.valueOf(true)) == true

    @test
    def boolUnbox02(): Bool = unbox(Boolean.valueOf(false)) == false

    @test
    def boolBox01(): Bool = box(true).equals(Boolean.valueOf(true))

    @test
    def boolBox02(): Bool = box(false).equals(Boolean.valueOf(false))

    @test
    def boolRoundTrip01(): Bool = unbox(box(true)) == true

    @test
    def boolRoundTrip02(): Bool = unbox(box(false)) == false

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charUnbox01(): Bool = unbox(Character.valueOf('a')) == 'a'

    @test
    def charUnbox02(): Bool = unbox(Character.valueOf('Z')) == 'Z'

    @test
    def charBox01(): Bool = box('a').equals(Character.valueOf('a'))

    @test
    def charBox02(): Bool = box('Z').equals(Character.valueOf('Z'))

    @test
    def charRoundTrip01(): Bool = unbox(box('a')) == 'a'

    @test
    def charRoundTrip02(): Bool = unbox(box('Z')) == 'Z'

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32Unbox01(): Bool = unsafe unbox(Float.valueOf(1.23f32)) == 1.23f32

    @test
    def float32Unbox02(): Bool = unsafe unbox(Float.valueOf(-4.56f32)) == -4.56f32

    @test
    def float32Box01(): Bool = unsafe box(1.23f32).equals(Float.valueOf(1.23f32))

    @test
    def float32Box02(): Bool = unsafe box(-4.56f32).equals(Float.valueOf(-4.56f32))

    @test
    def float32RoundTrip01(): Bool = unbox(box(1.23f32)) == 1.23f32

    @test
    def float32RoundTrip02(): Bool = unbox(box(-4.56f32)) == -4.56f32

    @test
    def float32MaxValue(): Bool = unsafe unbox(box(Float.MAX_VALUE)) == Float.MAX_VALUE

    @test
    def float32MinValue(): Bool = unsafe unbox(box(Float.MIN_VALUE)) == Float.MIN_VALUE

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64Unbox01(): Bool = unbox(Double.valueOf(1.23)) == 1.23

    @test
    def float64Unbox02(): Bool = unbox(Double.valueOf(-4.56)) == -4.56

    @test
    def float64Box01(): Bool = box(1.23).equals(Double.valueOf(1.23))

    @test
    def float64Box02(): Bool = box(-4.56).equals(Double.valueOf(-4.56))

    @test
    def float64RoundTrip01(): Bool = unbox(box(1.23)) == 1.23

    @test
    def float64RoundTrip02(): Bool = unbox(box(-4.56)) == -4.56

    @test
    def float64MaxValue(): Bool = unsafe unbox(box(Double.MAX_VALUE)) == Double.MAX_VALUE

    @test
    def float64MinValue(): Bool = unsafe unbox(box(Double.MIN_VALUE)) == Double.MIN_VALUE

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8Unbox01(): Bool = unsafe unbox(Byte.valueOf(42i8)) == 42i8

    @test
    def int8Unbox02(): Bool = unsafe unbox(Byte.valueOf(-128i8)) == -128i8

    @test
    def int8Box01(): Bool = unsafe box(42i8).equals(Byte.valueOf(42i8))

    @test
    def int8Box02(): Bool = unsafe box(-128i8).equals(Byte.valueOf(-128i8))

    @test
    def int8RoundTrip01(): Bool = unbox(box(42i8)) == 42i8

    @test
    def int8RoundTrip02(): Bool = unbox(box(-128i8)) == -128i8

    @test
    def int8MaxValue(): Bool = unsafe unbox(box(Byte.MAX_VALUE)) == Byte.MAX_VALUE

    @test
    def int8MinValue(): Bool = unsafe unbox(box(Byte.MIN_VALUE)) == Byte.MIN_VALUE

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16Unbox01(): Bool = unsafe unbox(Short.valueOf(12345i16)) == 12345i16

    @test
    def int16Unbox02(): Bool = unsafe unbox(Short.valueOf(-32768i16)) == -32768i16

    @test
    def int16Box01(): Bool = unsafe box(12345i16).equals(Short.valueOf(12345i16))

    @test
    def int16Box02(): Bool = unsafe box(-32768i16).equals(Short.valueOf(-32768i16))

    @test
    def int16RoundTrip01(): Bool = unbox(box(12345i16)) == 12345i16

    @test
    def int16RoundTrip02(): Bool = unbox(box(-32768i16)) == -32768i16

    @test
    def int16MaxValue(): Bool = unsafe unbox(box(Short.MAX_VALUE)) == Short.MAX_VALUE

    @test
    def int16MinValue(): Bool = unsafe unbox(box(Short.MIN_VALUE)) == Short.MIN_VALUE

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32Unbox01(): Bool = unsafe unbox(Integer.valueOf(1234567890i32)) == 1234567890i32

    @test
    def int32Unbox02(): Bool = unsafe unbox(Integer.valueOf(-2147483648i32)) == -2147483648i32

    @test
    def int32Box01(): Bool = unsafe box(1234567890i32).equals(Integer.valueOf(1234567890i32))

    @test
    def int32Box02(): Bool = unsafe box(-2147483648i32).equals(Integer.valueOf(-2147483648i32))

    @test
    def int32RoundTrip01(): Bool = unbox(box(1234567890i32)) == 1234567890i32

    @test
    def int32RoundTrip02(): Bool = unbox(box(-2147483648i32)) == -2147483648i32

    @test
    def int32MaxValue(): Bool = unsafe unbox(box(Integer.MAX_VALUE)) == Integer.MAX_VALUE

    @test
    def int32MinValue(): Bool = unsafe unbox(box(Integer.MIN_VALUE)) == Integer.MIN_VALUE

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64Unbox01(): Bool = unsafe unbox(Long.valueOf(1234567890123456789i64)) == 1234567890123456789i64

    @test
    def int64Unbox02(): Bool = unsafe unbox(Long.valueOf(-9223372036854775808i64)) == -9223372036854775808i64

    @test
    def int64Box01(): Bool = unsafe box(1234567890123456789i64).equals(Long.valueOf(1234567890123456789i64))

    @test
    def int64Box02(): Bool = unsafe box(-9223372036854775808i64).equals(Long.valueOf(-9223372036854775808i64))

    @test
    def int64RoundTrip01(): Bool = unbox(box(1234567890123456789i64)) == 1234567890123456789i64

    @test
    def int64RoundTrip02(): Bool = unbox(box(-9223372036854775808i64)) == -9223372036854775808i64

    @test
    def int64MaxValue(): Bool = unsafe unbox(box(Long.MAX_VALUE)) == Long.MAX_VALUE

    @test
    def int64MinValue(): Bool = unsafe unbox(box(Long.MIN_VALUE)) == Long.MIN_VALUE

}
