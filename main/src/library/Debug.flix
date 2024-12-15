mod Debug {

    import java.lang.Byte
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.Short
    import java.lang.System
    import java.util.Arrays
    import java.util.Objects

    ///
    /// Prints the stringified representation of `x` with the given `prefix`.
    ///
    /// Note: For internal use only. The actual `dbg(exp)` expression is parsed and ultimately rewritten to call this function.
    ///
    @Internal
    pub def debugWithPrefix(prefix: String, x: a): a \ IO = {
        System.out.println("${prefix}%{x}");
        x
    }

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
    @Internal
    def escape(s: String): String = {
        def replace(src: String, dst: String, subject: String): String = unsafely IO run subject.replace(src, dst);
        s
            // NB: \\ must come first to avoid clobbering other cases
            |> replace("\\", "\\\\")
            |> replace("\n", "\\n")
            |> replace("\r", "\\r")
            |> replace("\"", "\\\"")
            |> replace("\'", "\\\'")
            |> replace("\t", "\\t")
    }

    ///
    /// Returns an automatic string representation of `x`.
    ///
    @Internal
    pub def stringify(x: a): String = {
        typematch x {
            case _: Unit => "()"
            case b: Bool =>  if (b) "true" else "false"
            case c: Char => "\'" + escape("${(c: Char)}") + "\'"
            case y: Float32 =>
                unsafely IO run Float.toString(y) + "f32"
            case y: Float64 =>
                unsafely IO run Double.toString(y)
            case y: BigDecimal =>
                if (unsafely IO run Objects.isNull(y)) {
                    "null"
                } else {
                    unsafely IO run y.toString() + "ff"
                }
            case y: Int8 =>
                unsafely IO run Byte.toString(y) + "i8"
            case y: Int16 =>
                unsafely IO run Short.toString(y) + "i16"
            case y: Int32 =>
                unsafely IO run Integer.toString(y)
            case y: Int64 =>
                unsafely IO run Long.toString(y) + "i64"
            case y: BigInt =>
                if (unsafely IO run Objects.isNull(y)) {
                    "null"
                } else {
                    unsafely IO run y.toString() + "ii"
                }
            case s: String =>
                if (unsafely IO run Objects.isNull(s)) {
                    "null"
                } else {
                    "\"" + escape(s) + "\""
                }
            case y: Array[Bool, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Bool, Static]))
            case y: Array[Char, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Char, Static]))
            case y: Array[Float32, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Float32, Static]))
            case y: Array[Float64, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Float64, Static]))
            case y: Array[Int8, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Int8, Static]))
            case y: Array[Int16, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Int16, Static]))
            case y: Array[Int32, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Int32, Static]))
            case y: Array[Int64, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Int64, Static]))
            case y: Array[Unit, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Object, Static]))
            case y: Array[BigInt, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Object, Static]))
            case y: Array[BigDecimal, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Object, Static]))
            case y: Array[String, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Object, Static]))
            case y: Array[Array[_, _], _] =>
                unsafely IO run Arrays.deepToString(unchecked_cast(y as Array[Object, Static]))
            case y: Array[_, _] =>
                unsafely IO run Arrays.toString(unchecked_cast(y as Array[Object, Static]))
            case y: _ =>
                unsafely IO run Objects.toString(unchecked_cast(y as Object))
        }
    }

}
