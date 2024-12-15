mod Test.Handler.Gen.AtomicInteger {

    import java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        unsafely IO run new AtomicInteger(n)

    def toInt(n: AtomicInteger): Int32 =
        unsafely IO run n.intValue()

    eff Gen {
        def gen(): AtomicInteger
    }

    def generator(): Unit \ Gen =
        Gen.gen(); generator()

    def sample(limit: Int32): List[AtomicInteger] =
        region rc {
            let counter = Ref.fresh(rc, newAtomicInteger(0));
            run {
                generator(); Nil
            } with Gen {
                def gen(k) =
                    let i = getAndInc(counter);
                    if (toInt(i) == limit) Nil else i :: k(i)
            }
        }

    def getAndInc(r: Ref[AtomicInteger, r]): AtomicInteger \ r =
        let i = toInt(Ref.get(r));
        Ref.put(newAtomicInteger(i + 1), r);
        newAtomicInteger(i)

    def range(a: AtomicInteger, b: AtomicInteger): List[AtomicInteger] =
        List.unfold(i -> if (toInt(i) >= toInt(b)) None else Some((i, newAtomicInteger(toInt(i) + 1))), a)

    @Test
    def testSample01(): Bool =
        Assert.eq(Nil, sample(0) |> List.map(toInt))

    @Test
    def testSample02(): Bool =
        Assert.eq(0 :: Nil, sample(1) |> List.map(toInt))

    @Test
    def testSample03(): Bool =
        Assert.eq(0 :: 1 :: Nil, sample(2) |> List.map(toInt))

    @Test
    def testSample04(): Bool =
        Assert.eq(0 :: 1 :: 2 :: Nil, sample(3) |> List.map(toInt))

    @Test
    def testSample05(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: Nil, sample(4) |> List.map(toInt))

    @Test
    def testSample06(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: Nil, sample(5) |> List.map(toInt))

    @Test
    def testSample07(): Bool =
        Assert.eq(0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil, sample(6) |> List.map(toInt))

    @Test
    def testSample08(): Bool =
        Assert.eq(range(newAtomicInteger(0), newAtomicInteger(10)) |> List.map(toInt), sample(10) |> List.map(toInt))

    @Test
    def testSample09(): Bool =
        Assert.eq(range(newAtomicInteger(0), newAtomicInteger(100)) |> List.map(toInt), sample(100) |> List.map(toInt))

}
