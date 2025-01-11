mod Test.Exp.Jvm.InvokeConstructor {

    import java.lang.Boolean
    import java.lang.Byte
    import java.lang.Character
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.{String => JString}
    import java.util.{Locale$Builder => Builder}

    @test
    def testInvokeConstructor_01(): Boolean =
        new Boolean(true)

    @test
    def testInvokeConstructor_02(): Character =
        new Character('a')

    @test
    def testInvokeConstructor_03(): Float \ IO =
        new Float(123.456f32)

    @test
    def testInvokeConstructor_04(): Double =
        new Double(123.456f64)

    @test
    def testInvokeConstructor_05(): Byte \ IO =
        new Byte(123i8)

    @test
    def testInvokeConstructor_06(): Short \ IO =
        new Short(123i16)

    @test
    def testInvokeConstructor_07(): Integer \ IO =
        new Integer(123i32)

    @test
    def testInvokeConstructor_08(): Long \ IO =
        new Long(123i64)

    @test
    def testInvokeConstructor_09(): JString \ IO =
        new JString("Hello World")

    @test
    def testInvokeObjectConstructor2_01(): Object \ IO =
        let obj = new JString("Hello World");
        (checked_cast(obj) : Object)

    @test
    def testInvokeStaticNestedConstructor2_01(): Builder \ IO =
        new Builder()
}
