/*
 *  Copyright 2024 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod TestFromJava {

    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{List => JList}
    import java.lang.{String => JString}
    import java.math.BigInteger
    import java.math.BigDecimal

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int8FromJava01(): Bool =
        let i = Int8.valueOf(0i8);
        FromJava.fromJava(i) == 0i8

    @test
    def int8FromJava02(): Bool =
        let i = Int8.valueOf(100i8);
        FromJava.fromJava(i) == 100i8

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int16FromJava01(): Bool =
        let i = Int16.valueOf(0i16);
        FromJava.fromJava(i) == 0i16

    @test
    def int16FromJava02(): Bool =
        let i = Int16.valueOf(100i16);
        FromJava.fromJava(i) == 100i16

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int32FromJava01(): Bool =
        let i = Int32.valueOf(0);
        FromJava.fromJava(i) == 0

    @test
    def int32FromJava02(): Bool =
        let i = Int32.valueOf(100);
        FromJava.fromJava(i) == 100

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def int64FromJava01(): Bool =
        let i = Int64.valueOf(0i64);
        FromJava.fromJava(i) == 0i64

    @test
    def int64FromJava02(): Bool =
        let i = Int64.valueOf(100i64);
        FromJava.fromJava(i) == 100i64

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float32FromJava01(): Bool =
        let d = Float32.valueOf(0.0f32);
        FromJava.fromJava(d) == 0.0f32

    @test
    def float32FromJava02(): Bool =
        let d = Float32.valueOf(100.0f32);
        FromJava.fromJava(d) == 100.0f32

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def float64FromJava01(): Bool =
        let d = Float64.valueOf(0.0f64);
        FromJava.fromJava(d) == 0.0f64

    @test
    def float64FromJava02(): Bool =
        let d = Float64.valueOf(100.0f64);
        FromJava.fromJava(d) == 100.0f64

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigIntFromJava01(): Bool =
        let i = unsafely IO run BigInteger.valueOf(0i64);
        FromJava.fromJava(i) == 0ii

    @test
    def bigIntFromJava02(): Bool =
        let i = unsafely IO run BigInteger.valueOf(100i64);
        FromJava.fromJava(i) == 100ii

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def bigDecimalFromJava01(): Bool =
        let d = unsafely IO run BigDecimal.valueOf(0i64);
        FromJava.fromJava(d) == 0.0ff

    @test
    def bigDecimalFromJava02(): Bool =
        let d = unsafely IO run BigDecimal.valueOf(100i64);
        FromJava.fromJava(d) == 100.0ff

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def charFromJava01(): Bool =
        let c = Char.valueOf('0');
        FromJava.fromJava(c) == '0'

    @test
    def charFromJava02(): Bool =
        let c = Char.valueOf('a');
        FromJava.fromJava(c) == 'a'

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stringFromJava01(): Bool =
        let s = unsafely IO run JString.valueOf(true);
        FromJava.fromJava(s) == "true"

    @test
    def stringFromJava02(): Bool =
        let s = unsafely IO run JString.valueOf(false);
        FromJava.fromJava(s) == "false"

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def chainFromJava01(): Bool =
        let l = unsafely IO run JList.of();
        (FromJava.fromJava(l): Chain[String]) == (Chain.empty(): Chain[String])

    @test
    def chainFromJava02(): Bool =
        let l = unsafely IO run JList.of("one");
        (FromJava.fromJava(l): Chain[String]) == Chain.singleton("one")

    @test
    def chainFromJava03(): Bool =
        let l = unsafely IO run JList.of("one", "two");
        (FromJava.fromJava(l): Chain[String]) == List.toChain(List#{"one", "two"})

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def listFromJava01(): Bool =
        let l = unsafely IO run JList.of();
        (FromJava.fromJava(l): List[String]) == (Nil: List[String])

    @test
    def listFromJava02(): Bool =
        let l = unsafely IO run JList.of("one");
        (FromJava.fromJava(l): List[String]) == List#{"one"}

    @test
    def listFromJava03(): Bool =
        let l = unsafely IO run JList.of("one", "two");
        (FromJava.fromJava(l): List[String]) == List#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFromJava01(): Bool =
        let m = unsafely IO run JMap.of();
        (FromJava.fromJava(m): Map[String, String]) == Map#{}

    @test
    def mapFromJava02(): Bool =
        let m = unsafely IO run JMap.of("a", "one");
        (FromJava.fromJava(m): Map[String, String]) == Map#{"a" => "one"}

    @test
    def mapFromJava03(): Bool =
        let m = unsafely IO run JMap.of("a", "one", "b", "two");
        (FromJava.fromJava(m): Map[String, String]) == Map#{"a" => "one", "b" => "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def setFromJava01(): Bool=
        let s = unsafely IO run JSet.of();
        (FromJava.fromJava(s): Set[String]) == Set#{}

    @test
    def setFromJava02(): Bool =
        let s = unsafely IO run JSet.of("one");
        (FromJava.fromJava(s): Set[String]) == Set#{"one"}

    @test
    def setFromJava03(): Bool =
        let s = unsafely IO run JSet.of("one", "two");
        (FromJava.fromJava(s): Set[String]) == Set#{"one", "two"}

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def vectorFromJava01(): Bool =
        let v = unsafely IO run JList.of();
        (FromJava.fromJava(v): Vector[String]) == Vector#{}

    @test
    def vectorFromJava02(): Bool =
        let v = unsafely IO run JList.of("one");
        (FromJava.fromJava(v): Vector[String]) == Vector#{"one"}

    @test
    def vectorFromJava03(): Bool =
        let v = unsafely IO run JList.of("one", "two");
        (FromJava.fromJava(v): Vector[String]) == Vector#{"one", "two"}

}
