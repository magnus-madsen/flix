mod Test.Exp.Unsafe {

    import java.lang.System

    @Test
    def test01(): Unit = unsafely IO run println(42)

    @Test
    def test02(): Unit = unsafely IO run println(42)

    @Test
    def test03(): Int64 = unsafely IO run System.currentTimeMillis()

    @Test
    def test04(): Bool = unsafely OutOfBounds run Vector#{1, 2, 3}[1] == 2

    def unsafeCompose(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ ef2 =
        x -> g(unsafely ef1 run f(x)) // note that `unsafely ef1 run (g(f(x))` would give ef2 - ef1

    @Test
    def test05(): Bool = {
        def f() = {println("hello"); 42};
        def g(i) = i + 1;
        unsafeCompose(f, g)() == 43
    }

    def unsafeAbortKeyNotFoundHandler(f: a -> b \ ef): a -> b \ ef - KeyNotFound =
        x -> unsafely KeyNotFound run f(x)

    @Test
    def test06(): Bool = {
        unsafeAbortKeyNotFoundHandler(() -> Map#{'a' => 42}['a'])() == 42
    }

    @Test
    def test07(): Bool \ IO = region local {
        let r = Ref.fresh(local, 42);
        def unsafeDebug(): Unit \ IO = {
            let v = unsafely local run Ref.get(r);
            println(v)
        };
        spawn unsafeDebug() @ local;
        Ref.get(r) == 42
    }

}
