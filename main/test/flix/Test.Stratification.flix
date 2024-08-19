mod Test.Stratification {

    mod Test.Stratification.Not {

        //
        //
        // Non-recursive tests
        //
        //

        @test
        def test0NonRecursiveStratification00(): #{ A000(Int32), B000(Int32), R000(Int32) } = solve #{
            A000(1). A000(2). A000(3).
            B000(3).
            R000(x) :- A000(x), not B000(x).
        }

        @test
        def test0NonRecursiveStratification01(): #{ A001(Int32), B001(Int32), R001(Int32) } = solve #{
            A001(1). A001(2). A001(3).
            B001(3).
            R001(x) :- not A001(x), B001(x).
        }

        @test
        def test0NonRecursiveStratification02(): #{ A002(Int32), B002(Int32), C002(Int32), R002(Int32) } = solve #{
            A002(1). A002(2). A002(3).
            B002(3).
            C002(3).
            R002(x) :- A002(x), not B002(x), not C002(x).
        }

        @test
        def test0NonRecursiveStratification03(): #{ A003(Int32), B003(Int32), C003(Int32), R003(Int32) } = solve #{
            A003(1). A003(2). A003(3).
            B003(3).
            C003(3).
            R003(x) :- not A003(x), B003(x), not C003(x).
        }

        @test
        def test0NonRecursiveStratification04(): #{ A004(Int32), B004(Int32), C004(Int32), R004(Int32) } = solve #{
            A004(1). A004(2).
            B004(3).
            C004(x) :- A004(x).
            C004(x) :- B004(x).
            R004(x) :- C004(x), not A004(x).
        }

        @test
        def test0NonRecursiveStratification05(): #{ A005(Int32), B005(Int32), C005(Int32), D005(Int32), R005(Int32) } = solve #{
            A005(1). A005(2).
            B005(3).
            C005(x) :- A005(x).
            C005(x) :- B005(x).
            D005(x) :- C005(x), not A005(x).
            R005(x) :- A005(x), not D005(x).
        }

        @test
        def test0NonRecursiveStratification06(): #{ A006(Int32), B006(Int32), C006(Int32), D006(Int32), E006(Int32), R006(Int32) } = solve #{
            A006(1). A006(2).
            B006(3).
            C006(x) :- A006(x).
            C006(x) :- B006(x).
            D006(x) :- C006(x), not A006(x).
            E006(x) :- B006(x), not D006(x).
            R006(x) :- E006(x).
        }

        @test
        def test0NonRecursiveStratification07(): #{ A007(Int32), N007() } = solve #{
            A007(1).
            A007(2).
            A007(3).
            N007() :- not A007(1).
            N007() :- not A007(2).
            N007() :- not A007(3).
        }

        @test
        def test0NonRecursiveStratification08(): #{ A008(Int32), B008(Int32), N008() } = solve #{
            A008(1). A008(2).
            B008(1). B008(2).
            N008() :- not A008(1), not B008(1).
            N008() :- not A008(2), not B008(2).
        }

        //
        //
        // Rule based filtering
        //
        //

        @test
        def test1RuleFilteredStratification00(): Bool =
            let p1 = #{
                B100(12). P1Diff100(12).
                A100(x) :- B100(x), P1Diff100(_).
            };
            let p2 = #{
                A100(1). DiffB100(1).  P2Diff100(2).
                B100(x) :- not A100(x), P2Diff100(x).
            };
            (query p1 select x from A100(x) |> Vector.length) +
            (query p2 select x from B100(x) |> Vector.length)
            > 0


        @test
        def test1RuleFilteredStratification01(): Bool =
            let p1 = #{
                C101(12). B101(13).
                A101(x) :- B101(x).
            };
            let p2 = #{
                A101(101).
                B101(x) :- not C101(x), A101(x), P2Diff101().
            };
            let p3 = #{
                B101(12). A101(101).
                C101(x) :- A101(x), P3Diff101().
            };
            (query p1 select x from A101(x) |> Vector.length) +
            (query p2 select x from B101(x) |> Vector.length) +
            (query p3 select x from C101(x) |> Vector.length)
            > 0

        //
        //
        // Arity based filtering
        //
        //

        @test
        def test2ArityFilteredStratification00(): Bool =
            let p1 = #{
                B700(17). C700(17).
                A700(x) :- B700(x), C700(x).
            };
            let p2 = #{
                C700(12).
                B700(x, 2) :- not A700(x), C700(x).
            };
            (query p1 select x from A700(x) |> Vector.length) +
            (query p2 select x from B700(x, _) |> Vector.length)
            > 0

        @test
        def test2ArityFilteredStratification01(): Bool =
            let p1 = #{
                B701(13). C701(13).
                A701(x) :- B701(x), C701(x).
            };
            let p2 = #{
                C701(14, 12).
                B701(x) :- not A701(x), C701(x, 12).
            };
            (query p1 select x from A701(x) |> Vector.length) +
            (query p2 select x from B701(x) |> Vector.length)
            > 0

        @test
        def test2ArityFilteredStratification02(): Bool =
            let p1 = #{
                B702("s").
                A702(x) :- B702(x), C702().
            };
            let p2 = #{
                C702("a").
                B702(x) :- not A702(x), C702(x).
            };
            (query p1 select x from A702(x) |> Vector.length) +
            (query p2 select x from B702(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test2ArityFilteredStratification99(): Bool =
            let p1 = #{
                B799(12). C799(12, 13).
                A799(x) :- B799(x), C799(x, 13).
            };
            let p2 = #{
                C799(14).
                B799(x) :- not A799(x), C799(x).
            };
            (query p1 select x from A799(x) |> Vector.length) +
            (query p2 select x from B799(x) |> Vector.length)
            > 0

        //
        //
        // Term-type based filtering
        //
        //

        @test
        def test3TypeFilteredStratification00(): Bool =
            // Int32 and String
            let p1 = #{
                B800(12).
                A800(x) :- B800(x).
            };
            let p2 = #{
                A800("hey"). C800("heyy").
                B800(x) :- not A800(x), C800(x).
            };
            (query p1 select x from A800(x) |> Vector.length) +
            (query p2 select x from B800(x) |> Vector.length)
            > 0

        @test
        def test3TypeFilteredStratification01(): Bool =
            // Option[Int32] and Option[String]
            let p1 = #{
                B801(Some(12)).
                A801(x) :- B801(x).
            };
            let p2 = #{
                A801(Some("hey")). C801(Some("heyy")).
                B801(x) :- not A801(x), C801(x).
            };
            (query p1 select x from A801(x) |> Vector.length) +
            (query p2 select x from B801(x) |> Vector.length)
            > 0

        @test
        def test3TypeFilteredStratification02(): Bool =
            // Result[Int32, Int32] and Result[Int32, Int64]
            let p1 = #{
                B802(Ok(12)). B802(Err(12)).
                A802(x) :- B802(x).
            };
            let p2 = #{
                A802(Ok(12)). C802(Err(12i64)).
                B802(x) :- not A802(x), C802(x).
            };
            (query p1 select x from A802(x) |> Vector.length) +
            (query p2 select x from B802(x) |> Vector.length)
            > 0

        @test
        def test3TypeFilteredStratification03(): Bool =
            // List[Int16] and List[Bool]
            let p1 = #{
                B803(1i16 :: Nil).
                A803(x) :- B803(x).
            };
            let p2 = #{
                A803(false :: Nil).
                B803(x) :- not A803(x), C803(x).
            };
            (query p1 select x from A803(x) |> Vector.length) +
            (query p2 select x from B803(x) |> Vector.length)
            > 0

        @test
        def test3TypeFilteredStratification04(): Bool =
            // List[Int8] and List[List[Int8]]
            let p1 = #{
                B804(1i8 :: Nil).
                A804(x) :- B804(x).
            };
            let p2 = #{
                A804((1i8 :: Nil) :: Nil).
                B804(x) :- not A804(x), C804(x).
            };
            (query p1 select x from A804(x) |> Vector.length) +
            (query p2 select x from B804(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test3TypeFilteredStratification99(): Bool =
            let p1 = #{
                B899(12). C899("string").
                A899(x) :- B899(x), C899("string").
            };
            let p2 = #{
                C899(13).
                B899(x) :- not A899(x), C899(x).
            };
            (query p1 select x from A899(x) |> Vector.length) +
            (query p2 select x from B899(x) |> Vector.length)
            > 0

        //
        //
        // Denotation based filtering
        //
        //

        @test
        def test4DenotationFilteredStratification00(): Bool =
            let p1 = #{
                B400(12i64).
                A400(x; x) :- B400(x).
            };
            let p2 = #{
                A400(12i64, 12i64). C400(12i64).
                B400(x) :- not A400(x, x), C400(x).
            };
            (query p1 select x from A400(x; _) |> Vector.length) +
            (query p2 select x from B400(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test4DenotationFilteredStratification99(): Bool =
            let p1 = #{
                B499(12). C499(;12).
                A499(x) :- B499(x), C499(;12).
            };
            let p2 = #{
                C499(13). C499(13).
                B499(x) :- not A499(x), C499(x).
            };
            (query p1 select x from A499(x) |> Vector.length) +
            (query p2 select x from B499(x) |> Vector.length)
            > 0
    }

    mod Test.Stratification.Fix {

        //
        //
        // Non-recursive tests
        //
        //

        @test
        def test5NonRecursiveStratification00(): Bool =
        let res = query #{
            A500(1). A500(2). A500(3).
            B500(3).
            R500(x) :- A500(x), fix B500(x).
        } select x from R500(x) |> Vector.toSet;
        res == Set#{3}

        @test
        def test5NonRecursiveStratification01(): Bool =
        let res = query #{
            A501(1). A501(2). A501(3).
            B501(3).
            R501(x) :- fix A501(x), B501(x).
        } select x from R501(x) |> Vector.toSet;
        res == Set#{3}

        @test
        def test5NonRecursiveStratification02(): Bool =
        let res = query #{
            A502(1; 1). A502(1; 2). A502(1; 3).
            B502(x) :- fix A502(1; x).
        } select x from B502(x) |> Vector.toSet;
        res == Set#{3}

        @test
        def test5NonRecursiveStratification03(): Bool =
        let res = query #{
            A502(1; 1). A502(1; 2). A502(1; 3).
            B502(1; 4). B502(1; -1).
            R502(x) :- fix A502(1; x).
            R502(x) :- fix B502(1; x).
        } select x from R502(x) |> Vector.toSet;
        res == Set#{3, 4}

        @test
        def test5NonRecursiveStratification04(): Bool =
        let res = query #{
            A504(1). A504(2). A504(3).
            R504(x) :- fix A504(x).
            R504(x) :- A504(x).
        } select x from R504(x) |> Vector.toSet;
        res == Set#{1, 2, 3}

        @test
        def test5NonRecursiveStratification05(): Bool =
        let res = query #{
            A505("x"; 1). A505("x"; 2).
            B505("x"; 3). B505("x"; 4).
            R505(x, xv1 + xv2) :- fix A505(x; xv1), fix B505(x; xv2).
        } select xv from R505("x", xv) |> Vector.toSet;
        res == Set#{6}

        @test
        def test5NonRecursiveStratification06(): Bool =
        let res = query #{
            A506(1, 1). A506(2, 2). A506(3, 6).
            A506(1, 5). A506(2, 1). A506(3, 9).
            B506(x; y) :- A506(x, y).
            C506(;x) :- B506(_; x).
            R506(x) :- fix C506(;x).
        } select x from R506(x) |> Vector.toSet;
        res == Set#{9}

        @test
        def test5NonRecursiveStratification07(): Bool =
        let res = query #{
            A507(1). A507(2). A507(3).
            B507(x+1) :- fix A507(x).
            C507(x+1) :- fix B507(x).
            D507(x+1) :- fix C507(x).
            E507(x+1) :- fix D507(x).
            R507(x) :- fix E507(x).
        } select x from R507(x) |> Vector.toSet;
        res == Set#{5, 6, 7}

        //
        //
        // Rule based filtering
        //
        //

        @test
        def test6RuleFilteredStratification00(): Bool =
            let p1 = #{
                B600(12). P1Diff600(12).
                A600(x) :- B600(x), P1Diff600(_).
            };
            let p2 = #{
                A600(1). DiffB600(1).  P2Diff600(2).
                B600(x) :- fix A600(x), P2Diff600(x).
            };
            (query p1 select x from A600(x) |> Vector.length) +
            (query p2 select x from B600(x) |> Vector.length)
            > 0

        @test
        def test6RuleFilteredStratification01(): Bool =
            let p1 = #{
                C601(12). B601(13).
                A601(x) :- B601(x).
            };
            let p2 = #{
                A601(601).
                B601(x) :- fix C601(x), A601(x), P2Diff601().
            };
            let p3 = #{
                B601(12). A601(601).
                C601(x) :- A601(x), P3Diff601().
            };
            (query p1 select x from A601(x) |> Vector.length) +
            (query p2 select x from B601(x) |> Vector.length) +
            (query p3 select x from C601(x) |> Vector.length)
            > 0

        //
        //
        // Arity based filtering
        //
        //

        @test
        def test7ArityFilteredStratification00(): Bool =
            let p1 = #{
                B700(17). C700(17).
                A700(x) :- B700(x), C700(x).
            };
            let p2 = #{
                C700(12).
                B700(x, 2) :- fix A700(x), C700(x).
            };
            (query p1 select x from A700(x) |> Vector.length) +
            (query p2 select x from B700(x, _) |> Vector.length)
            > 0

        @test
        def test7ArityFilteredStratification01(): Bool =
            let p1 = #{
                B701(13). C701(13).
                A701(x) :- B701(x), C701(x).
            };
            let p2 = #{
                C701(14, 12).
                B701(x) :- fix A701(x), C701(x, 12).
            };
            (query p1 select x from A701(x) |> Vector.length) +
            (query p2 select x from B701(x) |> Vector.length)
            > 0

        @test
        def test7ArityFilteredStratification02(): Bool =
            let p1 = #{
                B702("s").
                A702(x) :- B702(x), C702().
            };
            let p2 = #{
                A702("a"). C702("a").
                B702(x) :- fix A702(x), C702(x).
            };
            (query p1 select x from A702(x) |> Vector.length) +
            (query p2 select x from B702(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test7ArityFilteredStratification99(): Bool =
            let p1 = #{
                B799(12). C799(12, 13).
                A799(x) :- B799(x), C799(x, 13).
            };
            let p2 = #{
                C799(13). C799(14).
                B799(x) :- fix A799(x), C799(x).
            };
            (query p1 select x from A799(x) |> Vector.length) +
            (query p2 select x from B799(x) |> Vector.length)
            > 0

        //
        //
        // Term-type based filtering
        //
        //

        @test
        def test8TypeFilteredStratification00(): Bool =
            // Int32 and String
            let p1 = #{
                B800(12).
                A800(x) :- B800(x).
            };
            let p2 = #{
                A800("hey"). C800("heyy").
                B800(x) :- fix A800(x), C800(x).
            };
            (query p1 select x from A800(x) |> Vector.length) +
            (query p2 select x from B800(x) |> Vector.length)
            > 0

        @test
        def test8TypeFilteredStratification01(): Bool =
            // Option[Int32] and Option[String]
            let p1 = #{
                B801(Some(12)).
                A801(x) :- B801(x).
            };
            let p2 = #{
                A801(Some("hey")). C801(Some("heyy")).
                B801(x) :- fix A801(x), C801(x).
            };
            (query p1 select x from A801(x) |> Vector.length) +
            (query p2 select x from B801(x) |> Vector.length)
            > 0

        @test
        def test8TypeFilteredStratification02(): Bool =
            // Result[Int32, Int32] and Result[Int32, Int64]
            let p1 = #{
                B802(Ok(12)). B802(Err(12)).
                A802(x) :- B802(x).
            };
            let p2 = #{
                A802(Ok(12)). C802(Err(12i64)).
                B802(x) :- fix A802(x), C802(x).
            };
            (query p1 select x from A802(x) |> Vector.length) +
            (query p2 select x from B802(x) |> Vector.length)
            > 0

        @test
        def test8TypeFilteredStratification03(): Bool =
            // List[Int16] and List[Bool]
            let p1 = #{
                B803(1i16 :: Nil).
                A803(x) :- B803(x).
            };
            let p2 = #{
                A803(false :: Nil).
                B803(x) :- fix A803(x), C803(x).
            };
            (query p1 select x from A803(x) |> Vector.length) +
            (query p2 select x from B803(x) |> Vector.length)
            > 0

        @test
        def test8TypeFilteredStratification04(): Bool =
            // List[Int8] and List[List[Int8]]
            let p1 = #{
                B804(1i8 :: Nil).
                A804(x) :- B804(x).
            };
            let p2 = #{
                A804((1i8 :: Nil) :: Nil).
                B804(x) :- fix A804(x), C804(x).
            };
            (query p1 select x from A804(x) |> Vector.length) +
            (query p2 select x from B804(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test8TypeFilteredStratification99(): Bool =
            let p1 = #{
                B899(12). C899("string").
                A899(x) :- B899(x), C899("string").
            };
            let p2 = #{
                C899(13). C899(13).
                B899(x) :- fix A899(x), C899(x).
            };
            (query p1 select x from A899(x) |> Vector.length) +
            (query p2 select x from B899(x) |> Vector.length)
            > 0

        //
        //
        // Denotation based filtering
        //
        //

        @test
        def test9DenotationFilteredStratification00(): Bool =
            let p1 = #{
                B900(12i64).
                A900(x; x) :- B900(x).
            };
            let p2 = #{
                A900(12i64, 12i64). C900(12i64).
                B900(x) :- fix A900(x, x), C900(x).
            };
            (query p1 select x from A900(x; _) |> Vector.length) +
            (query p2 select x from B900(x) |> Vector.length)
            > 0

        // combined with rule based filtering
        @test
        def test9DenotationFilteredStratification99(): Bool =
            let p1 = #{
                B999(12). C999(;12).
                A999(x) :- B999(x), C999(;12).
            };
            let p2 = #{
                C999(13).
                B999(x) :- fix A999(x), C999(x).
            };
            (query p1 select x from A999(x) |> Vector.length) +
            (query p2 select x from B999(x) |> Vector.length)
            > 0
    }
}
