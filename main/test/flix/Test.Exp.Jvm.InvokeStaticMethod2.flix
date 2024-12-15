mod Test.Exp.Jvm.InvokeStaticMethod {

    import dev.flix.test.TestClassWithInheritedMethod
    import java.lang.{String => JString}
    import java.nio.file.InvalidPathException
    import java.nio.file.Path
    import java.util.Arrays

    @test
    def testInvokeStaticMethod_01(): Bool \ IO =
        JString.valueOf(true) == "true"

    @test
    def testInvokeStaticMethod_02(): Bool \ IO =
        JString.valueOf('A') == "A"

    @test
    def testInvokeStaticMethod_03(): Bool \ IO =
        JString.valueOf(21.42f32) == "21.42"

    @test
    def testInvokeStaticMethod_04(): Bool \ IO =
        JString.valueOf(21.42f64) == "21.42"

    @test
    def testInvokeStaticMethod_05(): Bool \ IO =
        JString.valueOf(42i32) == "42"

    @test
    def testInvokeStaticMethod_06(): Bool \ IO =
        JString.valueOf(42i64) == "42"

    @test
    def testInvokeObjectStaticMethod01(): JString \ IO =
        JString.valueOf(true)

    @test
    def testInvokeInterfaceStaticMethod02(): Path \ IO = region rc {
        try {
            Path.of("end", Array#{"p1", "p2", "p3"} @ rc)
        } catch {
            case _: InvalidPathException => (checked_cast(null) : Path)
        }
    }

    @test
    def testInvokeInheritedStaticMethod01(): Bool =
        unsafely IO run TestClassWithInheritedMethod.staticMethod(1) == 2

    @test
    def testInvokeStaticMethod_WithArrayParam(): Bool \ IO = region rc {
        Arrays.fill(Array#{"this", "that"} @ rc, "foo");
        true
    }
}
