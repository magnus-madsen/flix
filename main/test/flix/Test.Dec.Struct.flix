struct S[r] {
    foo: Int32
}

@test
def foo(): Unit = ()

mod Test.Dec.Struct {
    pub struct Empty[r] {}

    pub struct Singleton[r] {
        field: Int32
    }

    pub struct Poly[a, r] {
        zero: Int32,
        one: a
    }

    pub struct PolyMulti[a, b, r] {
        zero: Int32,
        oneA: a,
        oneB: b,
        two: Map[a, b]
    }

    pub struct Recursive[r] {
        base: Int32,
        recurse: Recursive[r]
    }

    pub struct PolyRecursive[a, r] {
        base: a,
        recurse: PolyRecursive[a, r]
    }

    pub struct PolyRecursiveNonRegular[a, r] {
        base: a,
        recurse: PolyRecursiveNonRegular[Poly[a, r], r]
    }

    pub struct BinaryTree[t, r] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        value: t
    }

    pub struct S[r] {
        x: Int32
    }

    def _testStructTrait01(p: a): t \ DotGet_x.Aef[a] with DotGet_x[a] where DotGet_x.FieldType[a] ~ t =
        (p->x)

    def _testStructTrait02(p: a): Unit \ DotPut_x.Aef[a] with DotPut_x[a] where DotPut_x.FieldType[a] ~ Int32 =
        (p->x = 3)
}
