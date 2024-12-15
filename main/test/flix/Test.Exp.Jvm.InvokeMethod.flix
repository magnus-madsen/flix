mod Test.Exp.Jvm.InvokeMethod {

    import dev.flix.test.TestClassWithInheritedMethod
    import java.lang.{String => JString}

    @test
    def testInvokeMethod01(): Bool \ IO =
        "HELLO WORLD".toLowerCase() == "hello world"

    @test
    def testInvokeMethod02(): Bool \ IO =
        "hello world".toUpperCase() == "HELLO WORLD"

    @test
    def testInvokeMethod03(): Bool \ IO =
        "hello world".length() == 11

    @test
    def testInvokeMethod04(): Bool \ IO =
        "hello world".indexOf("world") == 6

    @test
    def testInvokeMethod05(): Bool \ IO =
        "hello world".replaceAll("hello", "goodbye") == "goodbye world"

    @test
    def testInvokePureMethod01(): Bool =
        unsafely IO run "HELLO WORLD".toLowerCase() == "hello world"

    @test
    def testInvokeObjectMethod01(): JString \ IO =
        "HELLO WORLD".toLowerCase()

    @test
    def testInvokeInheritedMethod01(): Bool \ IO =
        let obj = new TestClassWithInheritedMethod();
        obj.concreteMethod("bar") == "foobar"
}
