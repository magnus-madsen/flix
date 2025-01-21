mod Test.Exp.Jvm.NewObject {

    import dev.flix.test.TestBoolArrayInterface
    import dev.flix.test.TestBoolInterface
    import dev.flix.test.TestCharArrayInterface
    import dev.flix.test.TestCharInterface
    import dev.flix.test.TestClass
    import dev.flix.test.TestClassWithDefaultConstructor
    import dev.flix.test.TestClassWithNonDefaultConstructor
    import dev.flix.test.TestDefaultMethods
    import dev.flix.test.TestFloat32ArrayInterface
    import dev.flix.test.TestFloat32Interface
    import dev.flix.test.TestFloat64ArrayInterface
    import dev.flix.test.TestFloat64Interface
    import dev.flix.test.TestFunctionalInterface
    import dev.flix.test.TestGenericInterface
    import dev.flix.test.TestGenericMethod
    import dev.flix.test.TestInt8ArrayInterface
    import dev.flix.test.TestInt16ArrayInterface
    import dev.flix.test.TestInt16Interface
    import dev.flix.test.TestInt32ArrayInterface
    import dev.flix.test.TestInt32Interface
    import dev.flix.test.TestInt64ArrayInterface
    import dev.flix.test.TestInt64Interface
    import dev.flix.test.TestNonPublicInterface
    import dev.flix.test.TestOverloadedMethods
    import dev.flix.test.TestStackOffsets
    import dev.flix.test.TestThrowingInterface
    import dev.flix.test.TestVarargsInterface
    import dev.flix.test.TestVoidInterface
    import dev.flix.test.TestClassWithProtectedConstructor
    import dev.flix.test.{TestClassWithStaticNestedClass$NestedClass => NestedClass}
    import java.io.Serializable
    import java.lang.Object
    import java.lang.Class
    import java.util.Comparator

    def implementSerializable(): Serializable \ IO =
        new Serializable { }

    def implementSerializableAgain(): Serializable \ IO =
        new Serializable { }

    @test
    def testImplementInterface01(): Bool \ {NonDet, IO} =
        implementSerializable().toString().startsWith("Anon")

    @test
    def testImplementInterface02(): Bool \ IO =
        implementSerializable().hashCode() != implementSerializable().hashCode()

    @test
    def testImplementInterface03(): Bool \ IO =
        not implementSerializable().equals(implementSerializable())

    @test
    def testImplementInterface04(): Bool \ IO =
        implementSerializable().getClass().equals(
            implementSerializable().getClass()
        )

    @test
    def testImplementInterface05(): Bool \ IO =
        not implementSerializable().getClass().equals(
            implementSerializableAgain().getClass()
        )

    @test
    def testVoidInterface01(): Bool \ IO =
        let anon = new TestVoidInterface {
            def testMethod(_this: TestVoidInterface): Unit = ()
        };
        TestVoidInterface.runTest(anon)

    @test
    def testBoolInterface01(): Bool \ IO =
        let anon = new TestBoolInterface {
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        TestBoolInterface.runTest(anon)

    @test
    def testCharInterface01(): Bool \ IO =
        let anon = new TestCharInterface {
            def testMethod(_this: TestCharInterface, x: Char): Char = Char.toUpperCase(x)
        };
        TestCharInterface.runTest(anon)

    @test
    def testInt16Interface01(): Bool \ IO =
        let anon = new TestInt16Interface {
            def testMethod(_this: TestInt16Interface, x: Int16): Int16 = x + 1i16
        };
        TestInt16Interface.runTest(anon)

    @test
    def testInt32Interface01(): Bool \ IO =
        let anon = new TestInt32Interface {
            def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
        };
        TestInt32Interface.runTest(anon)

    @test
    def testInt64Interface01(): Bool \ IO =
        let anon = new TestInt64Interface {
            def testMethod(_this: TestInt64Interface, x: Int64): Int64 = x + 1i64
        };
        TestInt64Interface.runTest(anon)

    @test
    def testFloat32Interface01(): Bool \ IO =
        let anon = new TestFloat32Interface {
            def testMethod(_this: TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
        };
        TestFloat32Interface.runTest(anon)

    @test
    def testFloat64Interface01(): Bool \ IO =
        let anon = new TestFloat64Interface {
            def testMethod(_this: TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
        };
        TestFloat64Interface.runTest(anon)

    @test
    def testBoolArrayInterface01(): Bool \ IO =
        let anon = new TestBoolArrayInterface {
            def testMethod(_this: TestBoolArrayInterface, xs: Array[Bool, Static]): Array[Bool, Static] \ IO =
                xs |> Array.map(Static, x -> not x)
        };
        TestBoolArrayInterface.runTest(anon)

    @test
    def testCharArrayInterface01(): Bool \ IO =
        let anon = new TestCharArrayInterface {
            def testMethod(_this: TestCharArrayInterface, xs: Array[Char, Static]): Array[Char, Static] \ IO =
                xs |> Array.map(Static, Char.toUpperCase)
        };
        TestCharArrayInterface.runTest(anon)

    @test
    def testInt8ArrayInterface01(): Bool \ IO =
        let anon = new TestInt8ArrayInterface {
            def testMethod(_this: TestInt8ArrayInterface, xs: Array[Int8, Static]): Array[Int8, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i8)
        };
        TestInt8ArrayInterface.runTest(anon)

    @test
    def testInt16ArrayInterface01(): Bool \ IO =
        let anon = new TestInt16ArrayInterface {
            def testMethod(_this: TestInt16ArrayInterface, xs: Array[Int16, Static]): Array[Int16, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i16)
        };
        TestInt16ArrayInterface.runTest(anon)

    @test
    def testInt32ArrayInterface01(): Bool \ IO =
        let anon = new TestInt32ArrayInterface {
            def testMethod(_this: TestInt32ArrayInterface, xs: Array[Int32, Static]): Array[Int32, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1)
        };
        TestInt32ArrayInterface.runTest(anon)

    @test
    def testInt64ArrayInterface01(): Bool \ IO =
        let anon = new TestInt64ArrayInterface {
            def testMethod(_this: TestInt64ArrayInterface, xs: Array[Int64, Static]): Array[Int64, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i64)
        };
        TestInt64ArrayInterface.runTest(anon)

    @test
    def testFloat32ArrayInterface01(): Bool \ IO =
        let anon = new TestFloat32ArrayInterface {
            def testMethod(_this: TestFloat32ArrayInterface, xs: Array[Float32, Static]): Array[Float32, Static] \ IO =
                xs |> Array.map(Static, x -> x + 0.5f32)
        };
        TestFloat32ArrayInterface.runTest(anon)

    @test
    def testFloat64ArrayInterface01(): Bool \ IO =
        let anon = new TestFloat64ArrayInterface {
            def testMethod(_this: TestFloat64ArrayInterface, xs: Array[Float64, Static]): Array[Float64, Static] \ IO =
                xs |> Array.map(Static, x -> x + 0.5f64)
        };
        TestFloat64ArrayInterface.runTest(anon)

    @test
    def testStackOffsets01(): Bool \ IO =
        let anon = new TestStackOffsets {
            def testMethod(_this: TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String =
                "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
        };
        TestStackOffsets.runTest(anon)

    @test
    def testOverloadedMethods01(): Bool \ IO =
        let anon = new TestOverloadedMethods {
            def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
            def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
            def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
        };
        TestOverloadedMethods.runTest(anon)

    @test
    def testDefaultMethods01(): Bool \ IO =
        let anon = new TestDefaultMethods {
            def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
        };
        anon.methodWithNoImplementation(1) == 2 and anon.methodWithDefaultImplementation(1) == 43

    @test
    def testDefaultMethods02(): Bool \ IO =
        let anon = new TestDefaultMethods {
            def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
            def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
        };
        anon.methodWithNoImplementation(1) == 2 and anon.methodWithDefaultImplementation(1) == 3

    @test
    def testGenericInterface01(): Bool \ IO =
        let anon = new TestGenericInterface {
            def testMethod(_this: TestGenericInterface, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        TestGenericInterface.runTest(anon)

    @test
    def testGenericMethod01(): Bool \ IO =
        let anon = new TestGenericMethod {
            def testMethod(_this: TestGenericMethod, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        TestGenericMethod.runTest(anon)

    @test
    def testAliasesInObject01(): Bool \ IO =
        let anon = new TestGenericInterface {
            def testMethod(_this: TestGenericInterface, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        TestGenericInterface.runTest(anon)

    @test
    def testVarargsInterface01(): Bool \ IO =
        let anon = new TestVarargsInterface {
            def testMethod(_this: TestVarargsInterface, xs: Array[Int32, Static]): Int32 \ IO =
                xs |> Array.sum
        };
        TestVarargsInterface.runTest(anon)

    @test
    def testThrowingInterface01(): Bool \ IO =
        let anon = new TestThrowingInterface {
            def testMethod(_this: TestThrowingInterface): Unit = ()
        };
        TestThrowingInterface.runTest(anon)

    @test
    def testFunctionalInterface01(): Bool \ IO =
        let anon = new TestFunctionalInterface {
            def testMethod(_this: TestFunctionalInterface, x: Int32): Int32 = x + 1
        };
        TestFunctionalInterface.runTest(anon)

    @test
    def testFunctionalInterface02(): Bool \ IO =
        let anon = new Comparator {
            def compare(_this: Comparator, _t: Object, _u: Object): Int32 = 0
        };
        anon.compare("foo", "bar") == 0

    @test
    def testClassWithDefaultConstructor01(): Bool \ IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
        };
        anon.abstractMethod(1) == 2 and
            anon.concreteMethod("bar") == "foobar"

    @test
    def testClassWithDefaultConstructor02(): Bool \ IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
        };
        anon.abstractMethod(1) == 2 and
            anon.concreteMethod("bar") == "flix: bar"

    @test
    def testMethodClosure01(): Bool \ {NonDet, IO} =
        let x = Array#{1, 2, 3} @ Static;
        let y = Array#{4, 5, 6} @ Static;
        let anon = new Object {
            def toString(_this: Object): String \ IO = Array.toString(Array.append(Static, x, y))
        };
        anon.toString() == "Array#{1, 2, 3, 4, 5, 6}"

    @test
    def testStaticNestedClass01(): NestedClass \ IO =
        new NestedClass {}

    @test
    def testClassWithProtectedConstructor01(): TestClassWithProtectedConstructor \ IO =
        new TestClassWithProtectedConstructor {}
}
