mod Test.Java.Function {

    import java.lang.Object
    import java.util.Optional
    import java.util.function.Function
    import java.util.function.Consumer
    import java.util.function.Predicate
    import java.util.function.IntFunction
    import java.util.function.IntConsumer
    import java.util.function.IntPredicate
    import java.util.function.IntUnaryOperator
    import java.util.stream.IntStream
    import java.util.function.LongFunction
    import java.util.function.LongConsumer
    import java.util.function.LongPredicate
    import java.util.function.LongUnaryOperator
    import java.util.stream.LongStream
    import java.util.function.DoubleFunction
    import java.util.function.DoubleConsumer
    import java.util.function.DoublePredicate
    import java.util.function.DoubleUnaryOperator
    import java.util.stream.DoubleStream
    import java.util.stream.Stream

    @Test
    def testFunction(): Bool \ {NonDet, IO} = {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream = Stream.of(mkObject(42));
        stream.findFirst().get().toString() == "42"
    }

    @Test
    def testConsumer(): Bool \ {NonDet, IO} = {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let st = Ref.fresh(Static, "<none>");
        let stream = Stream.of(mkObject(8));
        stream.forEach((obj: Object) -> let s1 = obj.toString(); Ref.put(s1, st));
        Ref.get(st) == "8"
    }

    @Test
    def testPredicate(): Bool \ IO = region rc {
        let mkObject = i -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream0 = Stream.of(Array#{mkObject(1), mkObject(2), mkObject(3), mkObject(4), mkObject(5)} @ rc);
        let stream1 = stream0.filter((obj: Object) -> obj.toString() == "5");
        stream1.findFirst().get().toString() == "5"
    }

    @Test
    def testIntFunction(): Bool \ IO = {
        let stream0 = IntStream.of(42);
        let f = (i: Int32) -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = stream0.mapToObj(f);
        stream1.findFirst().get().toString() == "42"
    }

    @Test
    def testIntConsumer(): Bool \ IO = {
        let st = Ref.fresh(Static, 0);
        let stream = IntStream.range(0, 9);
        stream.forEach(i -> Ref.put(i, st));
        Ref.get(st) == 8
    }

    @Test
    def testIntPredicate(): Bool \ IO = {
        let stream0 = IntStream.range(0, 9);
        let stream1 = stream0.filter(i -> i `Int32.remainder` 2 == 0);
        stream1.sum() == 20
    }

    @Test
    def testIntUnaryOperator(): Bool \ IO = {
        let stream0 = IntStream.of(5);
        let stream1 = stream0.map(i -> checked_ecast(i+7));
        stream1.sum() == 12
    }

    @Test
    def testLongFunction(): Bool \ {NonDet, IO} = {
        let stream0 = LongStream.of(42i64);
        let f = (i: Int64) -> new Object {
            def toString(_this: Object): String = "${i}"
        };
        let stream1 = stream0.mapToObj(f);
        stream1.findFirst().get().toString() == "42"
    }

    @Test
    def testLongConsumer(): Bool \ IO = {
        let st = Ref.fresh(Static, 0i64);
        let stream = LongStream.range(0i64, 9i64);
        stream.forEach(i -> Ref.put(i, st));
        Ref.get(st) == 8i64
    }

    @Test
    def testLongPredicate(): Bool \ IO = {
        let stream0 = LongStream.range(0i64, 9i64);
        let stream1 = stream0.filter(i -> checked_ecast(i `Int64.remainder` 2i64 == 0i64));
        stream1.sum() == 20i64
    }

    @Test
    def testLongUnaryOperator(): Bool \ IO = {
        let stream0 = LongStream.of(5i64);
        let stream1 = stream0.map(i -> checked_ecast(i+7i64));
        stream1.sum() == 12i64
    }

    @Test
    def testDoubleFunction(): Bool \ IO = {
        let stream0 = DoubleStream.of(42.0f64);
        let f = d -> new Object {
            def toString(_this: Object): String = match Float64.tryToInt32(d) {
                case Some(i) => "${i}"
                case None    => ""
           }
        };
        let stream1 = stream0.mapToObj(f);
        stream1.findFirst().get().toString() == "42"
    }

    @Test
    def testDoubleConsumer(): Bool \ IO = {
        let st = Ref.fresh(Static, 0.0f64);
        let stream = DoubleStream.of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ Static);
        stream.forEach(i -> Ref.put(i, st));
        let last = Ref.get(st);
        Float64.tryToInt32(last) == Some(8)
    }

    @Test
    def testDoublePredicate(): Bool \ IO = region rc {
        let stream0 = DoubleStream.of(Array#{0.0f64, 1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64, 7.0f64, 8.0f64} @ rc);
        let stream1 = stream0.filter(d -> checked_ecast(match Float64.tryToInt32(d) {
            case Some(i) => i `Int32.remainder` 2 == 0
            case None    => false
            }));
        let tot = stream1.sum();
        Float64.tryToInt32(tot) == Some(20)
    }

    @Test
    def testDoubleUnaryOperator(): Bool \ IO = {
        let stream0 = DoubleStream.of(5.0f64);
        let stream1 = stream0.map(d -> checked_ecast(d + 7.0f64));
        let tot = stream1.sum();
        Float64.tryToInt32(tot) == Some(12)
    }

}
