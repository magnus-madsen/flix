/*
 * Copyright 2020 Esben Bjerre
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

mod TestMutList {
    use ToString.toString
    use MutList.sameElements

    ///
    /// Helper function to test capacity of internal array.
    ///
    def capacity(v: MutList[a, r]): Int32 \ r =
        Array.length(v->a)

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        MutList.toString(l) == "MutList#{}"
    }

    @test
    def toString02(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        MutList.push!(1, l);
        MutList.toString(l) == "MutList#{1}"
    }

    @test
    def toString03(): Bool = region rc {
        let l: MutList[Int32, rc] = MutList.empty(rc);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.push!(4, l);
        MutList.push!(5, l);
        MutList.toString(l) == "MutList#{1, 2, 3, 4, 5}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region rc {
        let u: MutList[Int32, _] = MutList.empty(rc);
        let v: MutList[Int32, _] = MutList.empty(rc);
        u `sameElements` v
    }

    @test
    def sameElements02(): Bool = region rc {
        let u = MutList.empty(rc);
        MutList.push!(0, u);
        let v = MutList.empty(rc);
        (u `sameElements` v) == false
    }

    @test
    def sameElements03(): Bool = region rc {
        let u = MutList.empty(rc);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.empty(rc);
        MutList.push!(2, v);
        MutList.push!(3, v);
        u `sameElements` v
    }

    @test
    def sameElements04(): Bool = region rc {
        let u = MutList.empty(rc);
        MutList.push!(2, u);
        MutList.push!(3, u);
        let v = MutList.empty(rc);
        MutList.push!(2, v);
        MutList.push!(2, v);
        (u `sameElements` v) == false
    }

    @test
    def sameElements05(): Bool = region rc {
        let u = MutList.empty(rc);
        MutList.push!(1, u);
        let v = MutList.empty(rc);
        (u `sameElements` v) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // Order.compare                                                           //
    /////////////////////////////////////////////////////////////////////////////

    // TODO


    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = region rc {
        let r = try MutList.get(0, MutList.empty(rc)) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "index 0 is out of bounds for MutList of length 0"
    }

    @test
    def get02(): Bool = region rc {
        let l = MutList.empty(rc);
        MutList.push!("item", l);
        let r = try MutList.get(0, l) with OutOfBounds {
            def outOfBounds(_, _) = "error"
        };
        r == "item"
    }

    @test
    def get03(): Bool = region rc {
        let l = MutList.empty(rc);
        MutList.push!("item", l);
        let r = try MutList.get(1, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "index 1 is out of bounds for MutList of length 1"
    }

    @test
    def get04(): Bool = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 2));

        let r = try MutList.get(1, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "item 1"
    }

    @test
    def get05(): Bool = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 4));

        let r = try MutList.get(3, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "item 3"
    }

    @test
    def get06(): Bool = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = try MutList.get(30, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "item 30"
    }

    @test
    def get07(): Bool = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = try MutList.get(-30, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "index -30 is out of bounds for MutList of length 42"
    }

    @test
    def get08(): Bool = region rc {
        let l = MutList.map(rc, i -> "item ${i}", MutList.range(rc, 0, 42));

        let r = try MutList.get(50, l) with OutOfBounds {
            def outOfBounds(msg, _) = msg
        };
        r == "index 50 is out of bounds for MutList of length 42"
    }


    /////////////////////////////////////////////////////////////////////////////
    // put                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def put01(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);

        try MutList.put("new item", 0, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == Some("index 0 is out of bounds for MutList of length 0")
    }

    @test
    def put02(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);
        MutList.push!("item", l);

        try MutList.put("new item", 0, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == None and MutList.nth(0, l) == Some("new item")
    }

    @test
    def put03(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.empty(rc);
        MutList.push!("item", l);

        try MutList.put("new item", 1, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == Some("index 1 is out of bounds for MutList of length 1")
    }

    @test
    def put04(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 2);

        try MutList.put(1234, 1, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == None and MutList.nth(1, l) == Some(1234)
    }

    @test
    def put05(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 4);

        try MutList.put(1234, 3, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == None and MutList.nth(3, l) == Some(1234)
    }

    @test
    def put06(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        try MutList.put(1234, 30, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == None and MutList.nth(30, l) == Some(1234)
    }

    @test
    def put07(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        try MutList.put(1234, -30, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == Some("index -30 is out of bounds for MutList of length 42")
    }

    @test
    def put08(): Bool = region rc {
        let error = Ref.fresh(rc, None);
        let l = MutList.range(rc, 0, 42);

        try MutList.put(1234, 50, l) with OutOfBounds {
            def outOfBounds(msg, _) = Ref.put(Some(msg), error)
        };
        Ref.get(error) == Some("index 50 is out of bounds for MutList of length 42")
    }


    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nth01(): Bool = region rc {
        MutList.nth(0, (MutList.empty(rc): MutList[Int32, _])) == None
    }

    @test
    def nth02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.nth(0, v) == Some(42)
    }

    @test
    def nth03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.nth(1, v) == None
    }

    @test
    def nth04(): Bool = region rc {
        MutList.nth(1, MutList.range(rc, 0, 2)) == Some(1)
    }

    @test
    def nth05(): Bool = region rc {
        MutList.nth(3, MutList.range(rc, 0, 4)) == Some(3)
    }

    @test
    def nth06(): Bool = region rc {
        MutList.nth(30, MutList.range(rc, 0, 42)) == Some(30)
    }

    @test
    def nth07(): Bool = region rc {
        MutList.nth(-30, MutList.range(rc, 0, 42)) == None
    }

    @test
    def nth08(): Bool = region rc {
        MutList.nth(50, MutList.range(rc, 0, 42)) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = region rc {
        MutList.length(MutList.empty(rc)) == 0
    }

    @test
    def length02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.length(v) == 1
    }

    @test
    def length03(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 2)) == 2
    }

    @test
    def length04(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 4)) == 4
    }

    @test
    def length05(): Bool = region rc {
        MutList.length(MutList.range(rc, 0, 42)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool = region rc {
        MutList.size(MutList.empty(rc)) == 0
    }

    @test
    def size02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.size(v) == 1
    }

    @test
    def size03(): Bool = region rc {
        MutList.size(MutList.range(rc, 0, 2)) == 2
    }

    @test
    def size04(): Bool = region rc {
        MutList.size(MutList.range(rc, 0, 4)) == 4
    }

    @test
    def size05(): Bool = region rc {
        MutList.size(MutList.range(rc, 0, 42)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region rc {
        MutList.isEmpty(MutList.empty(rc))
    }

    @test
    def isEmpty02(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 0))
    }

    @test
    def isEmpty03(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 1)) == false
    }

    @test
    def isEmpty04(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 0, 2)) == false
    }

    @test
    def isEmpty05(): Bool = region rc {
        MutList.isEmpty(MutList.range(rc, 3, 4)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nonEmpty01(): Bool = region rc {
        MutList.nonEmpty(MutList.empty(rc)) == false
    }

    @test
    def nonEmpty02(): Bool = region rc {
        MutList.nonEmpty(MutList.range(rc, 0, 0)) == false
    }

    @test
    def nonEmpty03(): Bool = region rc {
        MutList.nonEmpty(MutList.range(rc, 0, 1)) == true
    }

    @test
    def nonEmpty04(): Bool = region rc {
        MutList.nonEmpty(MutList.range(rc, 0, 2)) == true
    }

    @test
    def nonEmpty05(): Bool = region rc {
        MutList.nonEmpty(MutList.range(rc, 3, 4)) == true
    }


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = region rc {
        MutList.memberOf(42, MutList.empty(rc)) == false
    }

    @test
    def memberOf02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(2, v);
        MutList.memberOf(2, v)
    }

    @test
    def memberOf03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.memberOf(2, v) == false
    }

    @test
    def memberOf04(): Bool = region rc {
        MutList.memberOf(4, MutList.range(rc, 0, 4)) == false
    }

    @test
    def memberOf05(): Bool = region rc {
        MutList.memberOf(0, MutList.range(rc, 0, 3))
    }

    @test
    def memberOf06(): Bool = region rc {
        MutList.memberOf(2, MutList.range(rc, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = region rc {
        MutList.count(x -> x > 2, MutList.range(rc, 0, 4)) == 1
    }

    @test
    def count02(): Bool = region rc {
        MutList.count(x -> x >= 2, MutList.range(rc, 0, 4)) == 2
    }

    @test
    def count03(): Bool = region rc {
        MutList.count(x -> x `Int32.remainder` 2 == 0, MutList.range(rc, 0, 10)) == 5
    }

    @test
    def count04(): Bool = region rc {
        MutList.count(x -> x == 11, MutList.range(rc, 0, 21)) == 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = region rc {
        MutList.exists(x -> x > 9, MutList.range(rc, 0, 9)) == false
    }

    @test
    def exists02(): Bool = region rc {
        MutList.exists(x -> x < 0, MutList.range(rc, 0, 9)) == false
    }

    @test
    def exists03(): Bool = region rc {
        MutList.exists(x -> x == 8, MutList.range(rc, 0, 9))
    }

    @test
    def exists04(): Bool = region rc {
        MutList.exists(x -> x > 0, MutList.range(rc, 0, 9))
    }

    @test
    def exists05(): Bool = region rc {
        MutList.exists(x -> x == 0, MutList.empty(rc)) == false
    }


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = region rc {
        MutList.forAll(x -> x > 0, MutList.range(rc, 0, 9)) == false
    }

    @test
    def forAll02(): Bool = region rc {
        MutList.forAll(x -> x >= 0, MutList.range(rc, 0, 9))
    }

    @test
    def forAll03(): Bool = region rc {
        MutList.forAll(x -> x >= 0 and x < 9, MutList.range(rc, 0, 9))
    }

    @test
    def forAll05(): Bool = region rc {
        MutList.forAll(x -> x > 0, MutList.empty(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.head(v) == Some(42)
    }

    @test
    def head02(): Bool = region rc {
        MutList.head((MutList.empty(rc): MutList[Unit, _])) == None
    }

    @test
    def head03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(101, v);
        MutList.push!(102, v);
        MutList.head(v) == Some(101)
    }


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = region rc {
        MutList.last((MutList.empty(rc): MutList[Unit, _])) == None
    }

    @test
    def last02(): Bool = region rc {
        MutList.last(MutList.range(rc, 0, 8)) == Some(7)
    }

    @test
    def last03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.last(v) == Some('B')
    }


    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLeft01(): Bool = region rc {
        MutList.indexOfLeft(2, MutList.range(rc, 2, 3)) == Some(0)
    }

    @test
    def indexOfLeft02(): Bool = region rc {
        MutList.indexOfLeft(10, MutList.range(rc, 0, 10)) == None
    }

    @test
    def indexOfLeft03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('C', v);
        MutList.indexOfLeft('B', v) == Some(1)
    }

    @test
    def indexOfLeft04(): Bool = region rc {
        MutList.indexOfLeft(99, MutList.range(rc, 0, 100)) == Some(99)
    }

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfRight01(): Bool = region rc {
        MutList.indexOfRight(2, MutList.range(rc, 2, 3)) == Some(0)
    }

    @test
    def indexOfRight02(): Bool = region rc {
        MutList.indexOfRight(10, MutList.range(rc, 0, 10)) == None
    }

    @test
    def indexOfRight03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!('A', v);
        MutList.push!('B', v);
        MutList.push!('A', v);
        MutList.indexOfRight('A', v) == Some(2)
    }

    @test
    def indexOfRight04(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 4);
        let v2 = MutList.range(rc, 0, 4);
        MutList.append!(MutList.toList(v2), v1);
        MutList.indexOfRight(2, v1) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = region rc {
        MutList.findLeft(x -> x > 3, MutList.range(rc, 3, 11)) == Some(4)
    }

    @test
    def findLeft02(): Bool = region rc {
        MutList.findLeft(x -> x > 10, MutList.range(rc, 3, 11)) == None
    }

    @test
    def findLeft03(): Bool = region rc {
        MutList.findLeft(x -> x < 3, MutList.range(rc, 3, 11)) == None
    }

    @test
    def findLeft04(): Bool = region rc {
        MutList.findLeft(x -> x `Int32.remainder` 2 == 1, MutList.range(rc, 3, 11)) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = region rc {
        MutList.findRight(x -> x > 0, MutList.range(rc, 2, 5)) == Some(4)
    }

    @test
    def findRight02(): Bool = region rc {
        MutList.findRight(x -> x == 10, MutList.range(rc, 9, 10)) == None
    }

    @test
    def findRight03(): Bool = region rc {
        MutList.findRight(x -> x `Int32.remainder` 2 == 0, MutList.range(rc, 3, 4)) == None
    }

    @test
    def findRight04(): Bool = region rc {
        MutList.findRight(x -> x < 5, MutList.range(rc, 3, 11)) == Some(4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 0, MutList.empty(rc));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @test
    def scanLeft02(): Bool = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.empty(rc);
        MutList.push!(0, v2);
        MutList.push!(1, v2);
        MutList.push!(3, v2);
        MutList.push!(6, v2);
        MutList.push!(10, v2);
        MutList.push!(15, v2);
        let v3 = MutList.scanLeft(rc, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @test
    def scanLeft03(): Bool = region rc {
        let v = MutList.scanLeft(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        MutList.last(v) == Some(75)
    }


    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 0, MutList.empty(rc));
        MutList.length(v) == 1 and MutList.head(v) == Some(0)
    }

    @test
    def scanRight02(): Bool = region rc {
        let v1 = MutList.range(rc, 1, 6);
        let v2 = MutList.empty(rc);
        MutList.push!(15, v2);
        MutList.push!(14, v2);
        MutList.push!(12, v2);
        MutList.push!(9, v2);
        MutList.push!(5, v2);
        MutList.push!(0, v2);
        let v3 = MutList.scanRight(rc, (x, y) -> x + y, 0, v1);
        MutList.sameElements(v2, v3)
    }

    @test
    def scanRight03(): Bool = region rc {
        let v = MutList.scanRight(rc, (x, y) -> x + y, 7, MutList.range(rc, 5, 13));
        MutList.last(v) == Some(7)
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region rc {
        let v = MutList.map(rc, x -> x * 2, MutList.range(rc, 0, 6));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @test
    def map02(): Bool = region rc {
        let v = MutList.map(rc, identity, MutList.range(rc, 0, 10));
        MutList.sameElements(v, MutList.range(rc, 0, 10))
    }

    @test
    def map03(): Bool = region rc {
        let v = MutList.map(rc, identity, (MutList.empty(rc): MutList[Unit, _]));
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def map04(): Bool = region rc {
        let v = MutList.map(rc, x -> x + 1, MutList.range(rc, 0, 100));
        MutList.sameElements(v, MutList.range(rc, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = region rc {
        MutList.flatten(rc, MutList.empty(rc)) |> MutList.isEmpty
    }

    @test
    def flatten02(): Bool = region rc {
        let empties = List.repeat(10, MutList.empty(rc)) |> List.toMutList(rc);
        MutList.flatten(rc, empties) |> MutList.isEmpty
    }

    @test
    def flatten03(): Bool = region rc {
        let lists = List#{
            List#{1, 2} |> List.toMutList(rc),
            MutList.empty(rc),
            List#{3, 4} |> List.toMutList(rc)
        } |> List.toMutList(rc);
        MutList.sameElements(MutList.flatten(rc, lists), List#{1, 2, 3, 4} |> List.toMutList(rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = region rc {
        MutList.flatMap(rc, x -> x, MutList.empty(rc)) |> MutList.isEmpty
    }

    @test
    def flatMap02(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.flatMap(rc, _ -> MutList.empty(rc), v) |> MutList.isEmpty
    }

    @test
    def flatMap03(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        def f(n) = List#{n} |> List.toMutList(rc);
        let w = MutList.flatMap(rc, f, v);
        MutList.sameElements(v, w)
    }

    @test
    pub def flatMap04(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        def f(n) = List.repeat(n, Int32.toString(n)) |> List.toMutList(rc);
        let w = MutList.flatMap(rc, f, v);
        let expected = List#{
            "1", "2", "2", "3", "3", "3", "4", "4", "4", "4"
        } |> List.toMutList(rc);
        MutList.sameElements(expected, w)
    }

    /////////////////////////////////////////////////////////////////////////////
    // transform!                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transform!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transform!(x -> x * 2, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 2, 4, 6, 8, 10} @ rc))
    }

    @test
    def transform!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.range(rc, 0, 10))
    }

    @test
    def transform!03(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.transform!(identity, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def transform!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.transform!(x -> x + 1, v);
        MutList.sameElements(v, MutList.range(rc, 1, 101))
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i < 3) 0 else 1, MutList.range(rc, 0, 6));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 0, 0, 1, 1, 1} @ rc))
    }

    @test
    def mapWithIndex02(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, x) -> if (x == 8 and i == 1) 42 else 0, MutList.range(rc, 7, 10));
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 42, 0} @ rc))
    }

    @test
    def mapWithIndex03(): Bool = region rc {
        let v = MutList.mapWithIndex(rc, (i, _) -> if (i == 0) 42 else 0, MutList.empty(rc));
        MutList.sameElements(v, MutList.empty(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // transformWithIndex!                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def transformWithIndex!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.transformWithIndex!((i, _) -> if (i < 3) 0 else 1, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 0, 0, 1, 1, 1} @ rc))
    }

    @test
    def transformWithIndex!02(): Bool = region rc {
        let v = MutList.range(rc, 7, 10);
        MutList.transformWithIndex!((i, x) -> if (x == 8 and i == 1) 42 else 0, v);
        MutList.sameElements(v, Array.toMutList(rc, Array#{0, 42, 0} @ rc))
    }

    @test
    def transformWithIndex!03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.transformWithIndex!((i, _) -> if (i == 0) 42 else 0, v);
        MutList.sameElements(v, MutList.empty(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @test
    def foldLeft02(): Bool = region rc {
        let v1 = MutList.empty(rc);
        let v2 = MutList.range(rc, 0, 10);
        MutList.foldLeft((_, x) -> MutList.push!(x, v1), (), v2);
        MutList.sameElements(v1, v2)
    }

    @test
    def foldLeft03(): Bool = region rc {
        MutList.foldLeft((acc, x) -> x :: acc, Nil, MutList.range(rc, 0, 6)) == 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil
    }

    @test
    def foldLeft04(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc - x, 0, MutList.range(rc, 0, 100)) == -4950
    }

    @test
    def foldLeft05(): Bool = region rc {
        MutList.foldLeft((acc, x) -> acc + x, 42, MutList.empty(rc)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region rc {
        MutList.foldRight((x, acc) -> acc + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @test
    def foldRight02(): Bool = region rc {
        MutList.foldRight((x, acc) -> Int32.toString(x) + acc, "", MutList.range(rc, 3, 8)) == "34567"
    }

    @test
    def foldRight03(): Bool = region rc {
        MutList.foldRight((x, acc) -> x :: acc, Nil, MutList.range(rc, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRight04(): Bool = region rc {
        MutList.foldRight((x, acc) -> x - acc, 0, MutList.range(rc, 0, 100)) == -50
    }

    @test
    def foldRight05(): Bool = region rc {
        MutList.foldRight((x, acc) -> x + acc, 42, MutList.empty(rc)) == 42
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> k() + x, 0, MutList.range(rc, 1, 5)) == 10
    }

    @test
    def foldRightWithCont02(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> Int32.toString(x) + k(), "", MutList.range(rc, 3, 8)) == "34567"
    }

    @test
    def foldRightWithCont03(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x :: k(), Nil, MutList.range(rc, 0, 6)) == 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    @test
    def foldRightWithCont04(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x - k(), 0, MutList.range(rc, 0, 100)) == -50
    }

    @test
    def foldRightWithCont05(): Bool = region rc {
        MutList.foldRightWithCont((x, k) -> x + k(), 42, MutList.empty(rc)) == 42
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region rc {
        MutList.foldMap(x -> 2 * x, List.toMutList(rc, (Nil: List[Int32]))) == 0
    }

    @test
    def foldMap02(): Bool = region rc {
        MutList.foldMap(x -> 2 * x, List.toMutList(rc, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region rc {
        MutList.foldMap(x -> if (x == "a") "b" else x, List.toMutList(rc, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region rc {
        MutList.foldMap(x -> if (x == "c") "b" else x, List.toMutList(rc, "a" :: "b" :: "c":: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region rc {
        MutList.foldMap(Int32.toString, List.toMutList(rc, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x + y, (MutList.empty(rc): MutList[Int32, _])) == None
    }

    @test
    def reduceLeft02(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x + y, MutList.range(rc, 0, 5)) == Some(10)
    }

    @test
    def reduceLeft03(): Bool = region rc {
        MutList.reduceLeft((x, y) -> x - y, MutList.range(rc, 0, 101)) == Some(-5050)
    }

    @test
    def reduceLeft04(): Bool = region rc {
        MutList.reduceLeft((_, y) -> if (y > 4) 1 else 0, MutList.range(rc, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = region rc {
        MutList.reduceRight((x, y) -> x + y, (MutList.empty(rc): MutList[Int32, _])) == None
    }

    @test
    def reduceRight02(): Bool = region rc {
        MutList.reduceRight((x, y) -> x + y, MutList.range(rc, 0, 5)) == Some(10)
    }

    @test
    def reduceRight03(): Bool = region rc {
        MutList.reduceRight((x, y) -> x - y, MutList.range(rc, 0, 6)) == Some(-3)
    }

    @test
    def reduceRight04(): Bool = region rc {
        MutList.reduceRight((x, _) -> if (x < 2) 1 else 0, MutList.range(rc, 0, 6)) == Some(1)
    }


    /////////////////////////////////////////////////////////////////////////////
    // clear!                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def clear!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 1);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == 1 and MutList.isEmpty(v)
    }

    @test
    def clear!02(): Bool = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        MutList.clear!(v);
        len == MutList.length(v)
    }

    @test
    def clear!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.clear!(v);
        MutList.isEmpty(v)
    }


    /////////////////////////////////////////////////////////////////////////////
    // copy                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def copy01(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        v1 `sameElements` v2
    }

    @test
    def copy02(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.copy(rc, v1);
        MutList.transform!(x -> x + 1, v2);
        MutList.last(v1) == Some(99) and MutList.last(v2) == Some(100)
    }

    @test
    def copy03(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 6);
        let v2 = MutList.copy(rc, v1);
        MutList.clear!(v1);
        MutList.length(v1) == 0 and MutList.length(v2) == 6
    }

    @test
    def copy04(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 1000);
        let v2 = MutList.copy(rc, v1);
        (v1->l == v2->l) and (v1->a `Array.sameElements` v2->a)
    }

    @test
    def copy05(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 3);
        let v2 = MutList.copy(rc, v1);
        (v1->l == v2->l) and (v1->a `Array.sameElements` v2->a) and
            (Array.length(v2->a) == 8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // pop                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pop01(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        let len = MutList.length(v);
        let last = MutList.pop!(v);
        MutList.length(v) == len - 1 and last == Some(5)
    }

    @test
    def pop02(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.pop!(v) == None
    }

    @test
    def pop03(): Bool = region rc {
        let v = MutList.range(rc, 1, 4);
        let three = MutList.pop!(v);
        let two = MutList.pop!(v);
        let one = MutList.pop!(v);
        let none = MutList.pop!(v);
        three == Some(3) and two == Some(2) and one == Some(1) and none == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // push                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def push01(): Bool = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        MutList.push!(1, v);
        MutList.last(v) == Some(1) and MutList.length(v) == len + 1
    }

    @test
    def push02(): Bool = region rc {
        let v = MutList.empty(rc);
        let len = MutList.length(v);
        List.forEach(x -> MutList.push!(x, v), List.range(0, 97));
        MutList.head(v) == Some(0) and MutList.last(v) == Some(96) and MutList.length(v) == len + 97
    }

    @test
    def push03(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(42, v);
        MutList.push!(31, v);
        MutList.push!(1019, v);
        MutList.length(v) == 3 and MutList.nth(0, v) == Some(42) and MutList.nth(1, v) == Some(31) and MutList.nth(2, v) == Some(1019)
    }

    @test
    def push04(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.push!(0, v);
        MutList.length(v) == 4 and MutList.nth(0, v) == Some(0) and MutList.nth(1, v) == Some(0) and MutList.nth(2, v) == Some(0) and MutList.nth(3, v) == Some(0)
    }


    /////////////////////////////////////////////////////////////////////////////
    // insert!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert!01(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.insert!(42, 0, v);
        MutList.length(v) == 1 and MutList.head(v) == Some(42)
    }

    @test
    def insert!02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.insert!(107, 0, v);
        MutList.insert!(7, 0, v);
        MutList.length(v) == 2 and MutList.head(v) == Some(7)
    }

    @test
    def insert!03(): Bool = region rc {
        let v = MutList.empty(rc);
        List.forEach(x -> MutList.insert!(x, 0, v), List.range(0, 107));
        MutList.length(v) == 107 and MutList.head(v) == Some(106)
    }

    @test
    def insert!04(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.insert!(42, 0, v);
        MutList.insert!(3, 1, v);
        MutList.insert!(5, 2, v);
        MutList.insert!(6, 0, v);
        MutList.length(v) == 4 and MutList.head(v) == Some(6) and MutList.last(v) == Some(5)
    }

    @test
    def insert!05(): Bool = region rc {
        let v = MutList.empty(rc);
        List.forEach(x -> MutList.push!(x, v), List.range(97, 147));
        MutList.insert!(42, 0, v);
        MutList.insert!(49, 0, v);
        MutList.length(v) == 52 and MutList.head(v) == Some(49) and MutList.last(v) == Some(146)
    }

    @test
    def insert!06(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 0, v);
        MutList.insert!(42, 1, v);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 2, v);
        MutList.length(v) == 6 and MutList.head(v) == Some(97) and MutList.last(v) == Some(42)
    }

    @test
    def insert!07(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.insert!(97, 0, v);
        MutList.insert!(98, 1, v);
        MutList.insert!(99, 2, v);
        MutList.insert!(107, 1, v);
        MutList.length(v) == 4 and MutList.nth(1, v) == Some(107)
    }

    @test
    def insert!08(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 4);
        MutList.insert!(42, 3, l1);
        MutList.insert!(41, 2, l1);
        MutList.insert!(40, 1, l1);
        MutList.insert!(55, 0, l1);
        MutList.insert!(33, 1, l1);
        let l2 = MutList.empty(rc);
        let xs = 55 :: 33 :: 0 :: 40 :: 1 :: 41 :: 2 :: 42 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // remove!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove!01(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def remove!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(rc, 1, 4))
    }

    @test
    def remove!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        MutList.remove!(3, v);
        MutList.sameElements(v, MutList.range(rc, 0, 3))
    }

    @test
    def remove!04(): Bool = region rc {
        let v = MutList.range(rc, 3, 7);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.range(rc, 6, 7))
    }

    @test
    def remove!05(): Bool = region rc {
        let v = MutList.range(rc, 9, 12);
        MutList.remove!(2, v);
        MutList.remove!(1, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def remove!06(): Bool = region rc {
        let v = MutList.range(rc, 7, 13);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.remove!(0, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def remove!07(): Bool = region rc {
        let v = MutList.range(rc, 0, 3);
        MutList.remove!(3, v);
        MutList.remove!(7, v);
        MutList.remove!(99, v);
        MutList.sameElements(v, MutList.range(rc, 0, 3))
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushAll!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushAll!01(): Bool = region rc {
        let v = MutList.range(rc, 0, 4);
        let l = List.range(4, 8);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 0, 8))
    }

    @test
    def pushAll!02(): Bool = region rc {
        let v = MutList.empty(rc);
        let l = List.range(1, 4);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 1, 4))
    }

    @test
    def pushAll!03(): Bool = region rc {
        let v = MutList.range(rc, 9, 24);
        let l = (Nil: List[Int32]);
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.range(rc, 9, 24))
    }

    @test
    def pushAll!04(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        let l: List[Unit] = Nil;
        MutList.pushAll!(l, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def pushAll!05(): Bool = region rc {
        let v = MutList.range(rc, 1, 3);
        let l1 = List.range(3, 5);
        let l2 = List.range(5, 99);
        MutList.pushAll!(l1, v);
        MutList.pushAll!(l2, v);
        MutList.sameElements(v, MutList.range(rc, 1, 99))
    }


    /////////////////////////////////////////////////////////////////////////////
    // retain!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def retain!01(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x `Int32.remainder` 2 == 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def retain!02(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x `Int32.remainder` 2 == 1, l1);
        let l2 = MutList.empty(rc);
        let xs = 1 :: 3 :: 5 :: 7 :: 9 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def retain!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x > 5, v);
        MutList.sameElements(v, MutList.range(rc, 6, 10))
    }

    @test
    def retain!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.retain!(x -> x > 10, v);
        MutList.sameElements(v, MutList.empty(rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace!01(): Bool = region rc {
        let l1 = MutList.empty(rc);
        MutList.push!(1, l1);
        MutList.push!(2, l1);
        MutList.push!(2, l1);
        MutList.push!(1, l1);
        MutList.push!(1, l1);
        MutList.replace!(src = 1, dst = 42, l1);
        let l2 = MutList.empty(rc);
        let xs = 42 :: 2 :: 2 :: 42 :: 42 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!02(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(src = 4, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 0 :: 5 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!03(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(src = 5, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 0 :: 6 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!04(): Bool = region rc {
        let l1 = MutList.range(rc, 4, 7);
        MutList.replace!(src = 6, dst = 0, l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 5 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def replace!05(): Bool = region rc {
        let l1 = MutList.empty(rc);
        let xs1 = List.repeat(97, 'a');
        List.forEach(x -> MutList.push!(x, l1), xs1);
        MutList.replace!(src = 'a', dst = 'z', l1);
        let l2 = MutList.empty(rc);
        let xs2 = List.repeat(97, 'z');
        List.forEach(x -> MutList.push!(x, l2), xs2);
        MutList.sameElements(l1, l2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // reverse!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01!(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def reverse02!(): Bool = region rc {
        let v = MutList.range(rc, 0, 1);
        MutList.reverse!(v);
        MutList.sameElements(v, MutList.range(rc, 0, 1))
    }

    @test
    def reverse03!(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 5);
        MutList.reverse!(l1);
        let l2 = MutList.empty(rc);
        let xs = 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def reverse04!(): Bool = region rc {
        let l1 = MutList.range(rc, 0, 6);
        MutList.reverse!(l1);
        let l2 = MutList.empty(rc);
        let xs = 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: Nil;
        List.forEach(x -> MutList.push!(x, l2), xs);
        MutList.sameElements(l1, l2)
    }

    @test
    def reverse05!(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 7 :: 6 :: 5 :: 4 :: 3 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.reverse!(l);
        MutList.sameElements(l, MutList.range(rc, 2, 8))
    }


    /////////////////////////////////////////////////////////////////////////////
    // shrink!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shrink!01(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(0, v);
        MutList.push!(1, v);
        MutList.push!(2, v);
        MutList.shrink!(v);
        MutList.length(v) == 3
    }

    @test
    def shrink!02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.shrink!(v);
        MutList.length(v) == 0
    }

    @test
    def shrink!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 9);
        MutList.push!(9, v);
        MutList.shrink!(v);
        MutList.length(v) == 10
    }

    @test
    def shrink!04(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.push!(4, v);
        MutList.append!(MutList.toList(MutList.range(rc, 0, 4)), v);
        MutList.push!(5, v);
        MutList.shrink!(v);
        MutList.length(v) == 6
    }


    /////////////////////////////////////////////////////////////////////////////
    // truncate!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def truncate!01(): Bool = region rc {
        let v: MutList[Unit, _] = MutList.empty(rc);
        MutList.truncate!(0, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def truncate!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 10);
        MutList.truncate!(5, v);
        MutList.sameElements(v, MutList.range(rc, 0, 5))
    }

    @test
    def truncate!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 99);
        MutList.truncate!(-1, v);
        MutList.sameElements(v, MutList.empty(rc))
    }

    @test
    def truncate!04(): Bool = region rc {
        let v = MutList.range(rc, 7, 8);
        MutList.truncate!(2, v);
        MutList.sameElements(v, MutList.range(rc, 7, 8))
    }

    @test
    def truncate!05(): Bool = region rc {
        let v = MutList.range(rc, 99, 102);
        MutList.truncate!(1, v);
        MutList.sameElements(v, MutList.range(rc, 99, 100))
    }


    /////////////////////////////////////////////////////////////////////////////
    // reserve!                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reserve!01(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.reserve!(4, v);
        capacity(v) - MutList.length(v) >= 4
    }

    @test
    def reserve!02(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.reserve!(5, v);
        capacity(v) - MutList.length(v) >= 5
    }

    @test
    def reserve!03(): Bool = region rc {
        let v = MutList.range(rc, 0, 99);
        MutList.reserve!(1, v);
        capacity(v) - MutList.length(v) >= 1
    }

    @test
    def reserve!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 100);
        MutList.reserve!(100, v);
        capacity(v) - MutList.length(v) >= 100
    }

    @test
    def reserve!05(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.reserve!(MutList.length(v), v);
        capacity(v) - MutList.length(v) >= MutList.length(v)
    }

    @test
    def reserve!06(): Bool = region rc {
        let n = 197;
        let v = MutList.range(rc, 0, n);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        MutList.reserve!(1, v);
        capacity(v) - MutList.length(v) >= 1
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        let l = (MutList.empty(rc): MutList[Int32, rc]);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @test
    def iterator02(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case Some(_) => true
            case _       => false
        }
    }

    @test
    def iterator03(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        discard MutList.pop!(l);
        let iter = MutList.iterator(rc, l);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @test
    def iterator04(): Bool = region rc {
        let l = List.toMutList(rc, 1 :: 2 :: 3 :: Nil);
        MutList.iterator(rc, l) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def iterator05(): Bool = region rc {
        let l = MutList.range(rc, 0, 100);
        MutList.iterator(rc, l) |> Iterator.toList == List.range(0, 100) and
        MutList.toList(l) == List.range(0, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let v = MutList.empty(rc);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x `Int32.remainder` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }

    @test
    def forEach03(): Bool = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FT"
    }

    @test
    def forEach04(): Bool = region rc {
        let v = MutList.range(rc, 0, 2);
        let sb = StringBuilder.empty(rc);
        let f = x -> if (x < 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEach(f, v);
        StringBuilder.toString(sb) == "FF"
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let v1 = MutList.range(rc, 0, 100);
        let v2 = MutList.empty(rc);
        MutList.forEachWithIndex((i, x) -> if (x `Int32.remainder` 2 == 0 and i < 10) MutList.push!(x, v2) else (), v1);
        let l = MutList.empty(rc);
        let xs = 0 :: 2 :: 4 :: 6 :: 8 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sameElements(v2, l)
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let v = MutList.range(rc, 1, 4);
        let sb = StringBuilder.empty(rc);
        let f = (i, x) -> if (x `Int32.remainder` 2 == 1 and i == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFF"
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        let sb = StringBuilder.empty(rc);
        let f = (_, x) -> if (x `Int32.remainder` 2 == 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutList.forEachWithIndex(f, v);
        StringBuilder.toString(sb) == "TFTFT"
    }


    /////////////////////////////////////////////////////////////////////////////
    // compress!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def compress!01(): Bool = region rc {
        // Pop 76 element such that the load factor is strictly lower than 1 / 4.
        let v = MutList.range(rc, 0, 100);
        let c = capacity(v);
        let i = Ref.fresh(rc, 0);
        let f = _ -> {
            if (Ref.get(i) < 76) {
                discard MutList.pop!(v);
                Ref.put(Ref.get(i) + 1, i)
            }
            else ()
        };
        MutList.forEach(f, v);
        capacity(v) == c / 2 and
            MutList.length(v) == 100 - 75 - 1
    }

    @test
    def compress!02(): Bool = region rc {
        let v = MutList.empty(rc);
        MutList.compress!(v);
        capacity(v) == capacity(MutList.empty(rc))
    }

    @test
    def compress!03(): Bool = region rc {
        let v = MutList.empty(rc);
        let c = capacity(v);
        MutList.push!(42, v);
        MutList.compress!(v);
        capacity(v) <= c and
        MutList.length(v) == 1
    }

    @test
    def compress!04(): Bool = region rc {
        let v = MutList.range(rc, 0, 5);
        MutList.compress!(v);
        capacity(v) <= capacity(MutList.range(rc, 0, 5))
    }


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, (MutList.empty(rc): MutList[Int32, _])) == None
    }

    @test
    def minimumBy02(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)) == Some(0)
    }

    @test
    def minimumBy03(): Bool = region rc {
        MutList.minimumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)) == Some(5)
    }

    @test
    def minimumBy04(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push!(-2, v);
        MutList.minimumBy((x, y) -> x <=> y, v) == Some(-2)
    }

    @test
    def minimumBy05(): Bool = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x < y and x `Int32.remainder` 2 == 0) Comparison.LessThan else Comparison.GreaterThan;
        MutList.minimumBy(cmp, v) == Some(10)
    }


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, (MutList.empty(rc): MutList[Int32, _])) == None
    }

    @test
    def maximumBy02(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 0, 5)) == Some(4)
    }

    @test
    def maximumBy03(): Bool = region rc {
        MutList.maximumBy((x, y) -> x <=> y, MutList.range(rc, 5, 10)) == Some(9)
    }

    @test
    def maximumBy04(): Bool = region rc {
        let v = MutList.range(rc, 0, 6);
        MutList.push!(11, v);
        MutList.maximumBy((x, y) -> x <=> y, v) == Some(11)
    }

    @test
    def maximumBy05(): Bool = region rc {
        let v = MutList.range(rc, 9, 19);
        let cmp = (x, y) -> if (x > y and x `Int32.remainder` 2 == 0) Comparison.GreaterThan else Comparison.LessThan;
        MutList.maximumBy(cmp, v) == Some(18)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        Array.sameElements(MutList.toArray(rc, Array.toMutList(rc, Array#{1, 2, 3} @ rc)), Array#{1, 2, 3} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        let a = MutList.toArray(rc, v);
        Array.put(42, 1, a);
        MutList.nth(1, v) == Some(2)
    }

    @test
    def toArray03(): Bool = region rc {
        let v = Array.toMutList(rc, Array#{1, 2, 3} @ rc);
        discard MutList.pop!(v);
        discard MutList.pop!(v);
        Array.length(MutList.toArray(rc, v)) == 1
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        MutList.toVector((MutList.empty(rc) : MutList[Int32, _])) `Vector.equals` Vector.empty()
    }

    @test
    def toVector02(): Bool = region rc {
        let l = MutList.empty(rc);
        MutList.push!(0, l);
        MutList.toVector(l) `Vector.equals` Vector.singleton(0)
    }

    @test
    def toVector03(): Bool = region rc {
        let l = MutList.empty(rc);
        MutList.push!(1, l);
        MutList.push!(2, l);
        MutList.push!(3, l);
        MutList.toVector(l) `Vector.equals` List.toVector(1 :: 2 :: 3 :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region rc {
        (MutList.empty(rc): MutList[Int32, _]) |> MutList.toList == (Nil: List[Int32])
    }

    @test
    def toList02(): Bool = region rc {
        let ml = MutList.range(rc, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
            not (MutList.isEmpty(ml))
    }

    @test
    def toList03(): Bool = region rc {
        let ml = MutList.range(rc, 0, 1000);
        ml |> MutList.toList == List.range(0, 1000) and
           ml `sameElements` MutList.range(rc, 0, 1000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs: List[Int32] = Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.empty()
    }

    @test
    def toChain02(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == Chain.singleton(1)
    }

    @test
    def toChain03(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: Nil)
    }

    @test
    def toChain04(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.toChain(l) == List.toChain(1 :: 2 :: 3 :: Nil)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let l = (MutList.empty(rc): MutList[Int32, _]);
        let d = (MutDeque.empty(rc): MutDeque[Int32, _]);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let xs = 1 :: Nil;

        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let xs = 1 :: 2 :: Nil;

        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let xs = 1 :: 2 :: 3 :: Nil;

        let l = MutList.empty(rc);
        List.forEach(x -> MutList.push!(x, l), xs);

        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), xs);

        MutList.toMutDeque(rc, l) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // The following tests for sort functions correspond to those in TestArray //
    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @test
    def sortWith01(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])));
        a `sameElements` Array.toMutList(rc, Array#{} @ rc)
    }

    @test
    def sortWith02(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0} @ rc)
    }

    @test
    def sortWith03(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0, 1} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @test
    def sortWith04(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @test
    def sortWith05(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{1,1} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{1,1} @ rc)
    }

    @test
    def sortWith06(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith07(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith08(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith09(): Bool = region rc {
        let a = MutList.sortWith(rc, cmp, Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,2,3,4} @ rc)
    }

    @test
    def sortWith10(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortWith11(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortWith12(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }


    @test
    def sortWith13(): Bool = region rc {
        let a = MutList.sortWith(rc, flip(cmp), Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc));
        a `sameElements` Array.toMutList(rc, Array#{4,3,2,2,1,0} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(a: MutList[Int32, rc]): Bool \ rc = region rc1 {
        MutList.sort(rc1, a) `sameElements` MutList.sortWith(rc1, cmp, a)
    }

    @test
    def sort01(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @test
    def sort02(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0} @ rc))
    }

    @test
    def sort03(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @test
    def sort04(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @test
    def sort05(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @test
    def sort06(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @test
    def sort07(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @test
    def sort08(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @test
    def sort09(): Bool = region rc {
        testSortVsSortWith(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sortWith!                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sortWith01!(): Bool = region rc {
        let a = Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc]));
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc]))
    }

    @test
    def sortWith02!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0} @ rc)
    }

    @test
    def sortWith03!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }

    @test
    def sortWith04!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{1,0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1} @ rc)
    }
    @test
    def sortWith05!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{1,1} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{1,1} @ rc)
    }

    @test
    def sortWith06!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith07!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith08!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc)
    }

    @test
    def sortWith09!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith!(cmp, a);
        a `sameElements` Array.toMutList(rc, Array#{0,1,2,2,3,4} @ rc)
    }

    @test
    def sortWith10!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortWith11!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortWith12!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc)
    }

    @test
    def sortWith13!(): Bool = region rc {
        let a = Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc);
        MutList.sortWith!(flip(cmp), a);
        a `sameElements` Array.toMutList(rc, Array#{4,3,2,2,1,0} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // sort!                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    def testSort!VsSortWith!(a: MutList[Int32, r]): Bool \ r = region rc1 {
        let b = MutList.copy(rc1, a);
        let c = MutList.copy(rc1, a);
        MutList.sort!(b);
        MutList.sortWith!(cmp, c);
        b `sameElements` c
    }

    @test
    def sort!01(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @test
    def sort!02(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0} @ rc))
    }

    @test
    def sort!03(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @test
    def sort!04(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @test
    def sort!05(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @test
    def sort!06(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @test
    def sort!07(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @test
    def sort!08(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @test
    def sort!09(): Bool = region rc {
        testSort!VsSortWith!(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: MutList[Int32, r]): Bool \ r = region rc1 {
        (MutList.sortBy(rc1, identity, a) `sameElements` MutList.sort(rc1, a)) and
        (MutList.sortBy(rc1, x -> 4 * x + 7, a) `sameElements` MutList.sort(rc1, a)) and
        (MutList.sortBy(rc1, x -> -x, a) `sameElements` MutList.sortWith(rc1, flip(cmp),a))
    }

    @test
    def sortBy01(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @test
    def sortBy02(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0} @ rc))
    }

    @test
    def sortBy03(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @test
    def sortBy04(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @test
    def sortBy05(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @test
    def sortBy06(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @test
    def sortBy07(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @test
    def sortBy08(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @test
    def sortBy09(): Bool = region rc {
        testSortByVsSort(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }

    enum R {
        case R({i = Int32, s = String})
    }

    instance Eq[R] {
        pub def eq(a: R, b: R): Bool =
            let R.R(x) = a;
            let R.R(y) = b;
            x#i == y#i and x#s == y#s
    }

    @test
    def sortBy10(): Bool = region rc {
        MutList.sortBy(rc, r -> let R.R(x) = r; x#i, Array.toMutList(rc, Array#{R.R({i = 2, s = "A"}), R.R({i = 1, s = "B"}), R.R({i = 3, s = "C"})} @ rc))
        `sameElements` Array.toMutList(rc, Array#{R.R({i = 1, s = "B"}), R.R({i = 2, s = "A"}), R.R({i = 3, s = "C"})} @ rc)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sortBy!                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    def testSortBy!VsSortBy(a: MutList[Int32, rc]): Bool \ rc = region rc1 {
        let b = MutList.copy(rc1, a);
        let c = MutList.copy(rc1, a);
        MutList.sortBy!(identity, b);
        MutList.sortBy!(x -> 4 * x + 7, c);
        (b `sameElements` MutList.sortBy(rc1, x -> 4 * x + 7, a)) and
        (c `sameElements` MutList.sortBy(rc1, identity, a))
    }

    @test
    def sortBy!01(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, ((Array#{} @ rc): Array[Int32, rc])))
    }

    @test
    def sortBy!02(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0} @ rc))
    }

    @test
    def sortBy!03(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0,1} @ rc))
    }

    @test
    def sortBy!04(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{1,0} @ rc))
    }

    @test
    def sortBy!05(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{1,1} @ rc))
    }

    @test
    def sortBy!06(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{0,1,2,3,4,5} @ rc))
    }

    @test
    def sortBy!07(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{5,4,3,2,1,0} @ rc))
    }

    @test
    def sortBy!08(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{5,3,0,4,1,2} @ rc))
    }

    @test
    def sortBy!09(): Bool = region rc {
        testSortBy!VsSortBy(Array.toMutList(rc, Array#{2,3,0,4,1,2} @ rc))
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region rc {
        MutList.empty(rc) |> MutList.sum == 0
    }

    @test
    def sum02(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 1
    }

    @test
    def sum03(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 6
    }

    @test
    def sum04(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 3
    }

    @test
    def sum05(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == -10
    }

    @test
    def sum06(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 0
    }

    @test
    def sum07(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = List.range(1, 101);
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sum(l) == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region rc {
        MutList.empty(rc) |> MutList.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }

    @test
    def sumWith03(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 9
    }

    @test
    def sumWith04(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: -3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 7
    }

    @test
    def sumWith05(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = -1 :: -2 :: -3 :: -4 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == -6
    }

    @test
    def sumWith06(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 10 :: -10 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.sumWith(x -> x + 1, l) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region rc {
        ((MutList.empty(rc): MutList[Int32, _])) |> MutList.join(",") == ""
    }

    @test
    def join02(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1"
    }

    @test
    def join03(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }

    @test
    def join04(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.join(",", l) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region rc {
        ((MutList.empty(rc): MutList[Int32, _])) |> MutList.joinWith(x -> "${x + 1}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2"
    }

    @test
    def joinWith03(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = 1 :: 2 :: 3 :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> "${x + 1}", ",", l) == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region rc {
        let l = MutList.empty(rc);
        let xs = "1" :: "2" :: "3" :: Nil;
        List.forEach(x -> MutList.push!(x, l), xs);
        MutList.joinWith(x -> x + x, ",", l) == "11,22,33"
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet = region rc {
        let l: List[Int32] = Nil;
        let v = MutList.shuffle(rc, List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 0 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @test
    def shuffle02(): Bool \ NonDet = region rc {
        let l = 1 :: Nil;
        let v = MutList.shuffle(rc, List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 1 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @test
    def shuffle03(): Bool \ NonDet = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let v = MutList.shuffle(rc, List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 3 and v `MutList.sameElements` List.toMutList(rc, l)
    }

    @test
    def shuffle04(): Bool \ NonDet = region rc {
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let v = MutList.shuffle(rc, List.toMutList(rc, l));
        MutList.sort!(v);

        MutList.length(v) == 10 and v `MutList.sameElements` List.toMutList(rc, l)
    }

}
