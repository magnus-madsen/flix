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
mod TestMutMap {

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        MutMap.toString(m) == "MutMap#{}"
    }

    @test
    def toString02(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, 101, m);
        MutMap.toString(m) == "MutMap#{1 => 101}"
    }

    @test
    def toString03(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, 101, m);
        MutMap.put!(2, 102, m);
        MutMap.put!(3, 103, m);
        MutMap.put!(4, 104, m);
        MutMap.put!(5, 105, m);
        MutMap.toString(m) == "MutMap#{1 => 101, 2 => 102, 3 => 103, 4 => 104, 5 => 105}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region rc {
        let m1 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        let m2 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        MutMap.sameElements(m1, m2)
    }

    @test
    def sameElements02(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put!(1, "a", m2);

        MutMap.sameElements(m, m2)
    }

    @test
    def sameElements03(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, "a", m);
        MutMap.put!(1, "b", m);

        let m2 = MutMap.empty(rc);
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements04(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, "a", m);
        MutMap.put!(2, "b", m);

        let m2 = MutMap.empty(rc);
        MutMap.put!(1, "a", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements05(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);

        not MutMap.sameElements(m, m2)
    }

    @test
    def sameElements06(): Bool = region rc {
        let m = MutMap.empty(rc);
        MutMap.put!(2, "b", m);
        MutMap.put!(3, "c", m);
        MutMap.put!(1, "a", m);

        let m2 = MutMap.empty(rc);
        MutMap.put!(1, "a", m2);
        MutMap.put!(2, "b", m2);
        MutMap.put!(3, "c", m2);

        MutMap.sameElements(m, m2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // mergeWithKey                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def merge01(): Bool = region rc {
        let m1 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        let m2 = (MutMap.empty(rc): MutMap[Int32, Int32, _]);
        MutMap.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap.isEmpty(m2)
    }

    @test
    def merge02(): Bool = region rc {
        let m1 = MutMap.empty(rc);
        let m2 = MutMap.empty(rc);
        MutMap.put!(1, 0, m1);
        MutMap.put!(2, 0, m2);
        MutMap.mergeWithKey!((_, v, _) -> v, m1, m2);
        MutMap.memberOf(1, m2) and MutMap.memberOf(2, m2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = region rc {
        MutMap.empty(rc) |> MutMap.sumValues == 0
    }

    @test
    def sumValues02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |>
            MutMap.sumValues == 1
    }

    @test
    def sumValues03(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumValues == 6
    }

    @test
    def sumValues04(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumValues == 3
    }

    @test
    def sumValues05(): Bool = region rc {
        Map.toMutMap(rc, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap.sumValues == -10
    }

    @test
    def sumValues06(): Bool = region rc {
        Map.toMutMap(rc, Map#{10 => 10, -10 => -10}) |>
            MutMap.sumValues == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = region rc {
        MutMap.empty(rc) |> MutMap.sumKeys == 0
    }

    @test
    def sumKeys02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |>
            MutMap.sumKeys == 1
    }

    @test
    def sumKeys03(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumKeys == 6
    }

    @test
    def sumKeys04(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumKeys == 3
    }

    @test
    def sumKeys05(): Bool = region rc {
        Map.toMutMap(rc, Map#{-1 => -1, -2 => -2, -5 => -3, -4 => -4}) |>
            MutMap.sumKeys == -12
    }

    @test
    def sumKeys06(): Bool = region rc {
        Map.toMutMap(rc, Map#{10 => 10, -10 => -10}) |>
            MutMap.sumKeys == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region rc {
        MutMap.empty(rc) |> MutMap.sumWith((k, v) -> k + v) == 0
    }

    @test
    def sumWith02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |>
            MutMap.sumWith((k, v) -> k + v) == 2
    }

    @test
    def sumWith03(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3}) |>
            MutMap.sumWith((k, v) -> k + v) == 12
    }

    @test
    def sumWith04(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1, 2 => 2, 3 => 3, -3 => -3}) |>
            MutMap.sumWith((k, v) -> k + v) == 6
    }

    @test
    def sumWith05(): Bool = region rc {
        Map.toMutMap(rc, Map#{-1 => -1, -2 => -2, -3 => -3, -4 => -4}) |>
            MutMap.sumWith((k, v) -> k + v) == -20
    }

    @test
    def sumWith06(): Bool = region rc {
        Map.toMutMap(rc, Map#{10 => 10, -10 => -10}) |>
            MutMap.sumWith((k, v) -> k + v) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinKeys                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinKeys01(): Bool = region rc {
        (MutMap.empty(rc): MutMap[Int32, Int32, _]) |> MutMap.joinKeys(",") == ""
    }

    @test
    def joinKeys02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |> MutMap.joinKeys(",") == "1"
    }

    @test
    def joinKeys03(): Bool = region rc {
        Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinKeys(",") == "0,1,2"
    }

    @test
    def joinKeys04(): Bool = region rc {
        Map.toMutMap(rc, Map#{"0" => 1, "1" => 2, "2" => 2}) |> MutMap.joinKeys(",") == "0,1,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinValues                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinValues01(): Bool = region rc {
        (MutMap.empty(rc): MutMap[Int32, Int32, _]) |> MutMap.joinValues(",") == ""
    }

    @test
    def joinValues02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |> MutMap.joinValues(",") == "1"
    }

    @test
    def joinValues03(): Bool = region rc {
        Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |> MutMap.joinValues(",") == "1,2,2"
    }

    @test
    def joinValues04(): Bool = region rc {
        Map.toMutMap(rc, Map#{0 => "1", 1 => "2", 2 => "2"}) |> MutMap.joinValues(",") == "1,2,2"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region rc {
        (MutMap.empty(rc): MutMap[Int32, Int32, _]) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region rc {
        Map.toMutMap(rc, Map#{1 => 1}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "1 => 1"
    }

    @test
    def joinWith03(): Bool = region rc {
        Map.toMutMap(rc, Map#{0 => 1, 1 => 2, 2 => 2}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }

    @test
    def joinWith04(): Bool = region rc {
        Map.toMutMap(rc, Map#{0 => "1", 1 => "2", 2 => "2"}) |>
            MutMap.joinWith((k, v) -> "${k} => ${v}", ", ") == "0 => 1, 1 => 2, 2 => 2"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        let d1 = MutMap.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        let d1 = MutMap.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        let d1 = MutMap.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack((3, 4), d2);
        MutDeque.pushBack((5, 6), d2);
        MutDeque.pushFront((1, 2), d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        let d1 = MutMap.toMutDeque(rc, m);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront((3, 'c'), d2);
        MutDeque.pushFront((2, 'b'), d2);
        MutDeque.pushFront((1, 'a'), d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        MutMap.toList(m) == (List#{} : List[(Int32, Int32)])
    }

    @test
    def toList02(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        MutMap.toList(m) == List#{(1, 2)}
    }

    @test
    def toList03(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        MutMap.toList(m) == List#{(1, 2), (3, 4), (5, 6)}
    }

    @test
    def toList04(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        MutMap.toList(m) == List#{(1, 'a'), (2, 'b'), (3, 'c')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        MutMap.toVector(m) == (Vector#{} : Vector[(Int32, Int32)])
    }

    @test
    def toVector02(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        MutMap.toVector(m) == Vector#{(1, 2)}
    }

    @test
    def toVector03(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        MutMap.toVector(m) == Vector#{(1, 2), (3, 4), (5, 6)}
    }

    @test
    def toVector04(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        MutMap.toVector(m) == Vector#{(1, 'a'), (2, 'b'), (3, 'c')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        Array.sameElements(MutMap.toArray(rc, m), Array#{} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 2)} @ rc)
    }

    @test
    def toArray03(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 2), (3, 4), (5, 6)} @ rc)
    }

    @test
    def toArray04(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        Array.sameElements(MutMap.toArray(rc, m), Array#{(1, 'a'), (2, 'b'), (3, 'c')} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region rc {
        let m: MutMap[Int32, Int32, _] = MutMap.empty(rc);
        MutMap.toSet(m) == (Set#{} : Set[(Int32, Int32)])
    }

    @test
    def toSet02(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2});
        MutMap.toSet(m) == Set#{(1, 2)}
    }

    @test
    def toSet03(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 2, 3 => 4, 5 => 6});
        MutMap.toSet(m) == Set#{(1, 2), (3, 4), (5, 6)}
    }

    @test
    def toSet04(): Bool = region rc {
        let m = Map.toMutMap(rc, Map#{1 => 'a', 2 => 'b', 3 => 'c'});
        MutMap.toSet(m) == Set#{(1, 'a'), (2, 'b'), (3, 'c')}
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        21 == Ref.get(ri)
    }

    @test
    def forEach02(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put!(0, 100, m);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        100 == Ref.get(ri)
    }

    @test
    def forEach03(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put!(0, 100, m);
        MutMap.put!(1, 101, m);
        MutMap.put!(2, 102, m);
        MutMap.forEach((k, v) -> Ref.put(k+v, ri), m);
        104 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        21 == Ref.get(ri)
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put!(0, 100, m);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        0 == Ref.get(ri)
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let m = MutMap.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutMap.put!(0, 100, m);
        MutMap.put!(1, 101, m);
        MutMap.put!(2, 102, m);
        MutMap.forEachWithIndex((i, _, _) -> Ref.put(i, ri), m);
        2 == Ref.get(ri)
    }

}
