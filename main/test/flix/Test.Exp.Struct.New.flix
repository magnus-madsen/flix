mod Mod1 {
    pub struct S[r] {}
    struct S2[r] {}

    mod Mod2 {
        @test
        def testOuterMod01(): Unit =
            region rc {
                new Mod1.S2 @ rc {};
                ()
            }
    }
}

mod Test.Exp.Struct.New {

    struct Empty[r] { }

    struct OneEmpty[r] {
        a: Empty[r]
    }

    struct TwoEmpty[r] {
        a: Empty[r],
        b: Empty[r]
    }

    struct EmptyC[r] {
        a: Empty[r]
    }

    struct EmptyB[r] {
        a: EmptyC[r]
    }

    struct EmptyA[r] {
        a: EmptyB[r]
    }

    struct A[r] {
        a: B[r]
    }

    struct B[r] {
        a: C[r]
    }

    struct C[r] {
        a: D[r]
    }

    struct D[r] {
        a: Int32
    }

    struct Name[r] {
        name: String
    }

    struct FullName[r] {
        fstName: String,
        lstName: String
    }

    struct Person[r] {
        fstName: String,
        lstName: String,
        age: Int32
    }

    struct Cowboy[r] {
        fstName: String,
        lstName: String,
        age: Int32,
        cowboy: Bool
    }

    struct TwoInts[r] {
        x: Int32,
        y: Int32
    }

    struct ThreeInts[r] {
        x: Int32,
        y: Int32,
        z: Int32
    }

    struct Outer[r] {
        a: TwoInts[r],
        b: TwoInts[r]
    }

    struct TwoIntsHolder[r] {
        a: TwoInts[r]
    }

    struct A3[r] {
        a: A2[r]
    }

    struct A2[r] {
        a: A1[r]
    }

    struct A1[r] {
        a: Empty[r]
    }

    struct BinaryTree[t, r] {
        left: Option[BinaryTree[t, r]],
        right: Option[BinaryTree[t, r]],
        value: t
    }

    @test
    def testEmptyStruct01(): Unit =
        region rc {
            new Empty @ rc {};
            ()
        }

    @test
    def testEmptyStruct02(): Unit =
        region rc {
            new OneEmpty @ rc {a = new Empty @ rc { }};
            ()
        }

    @test
    def testEmptyStruct03(): Unit =
        region rc {
            new TwoEmpty @ rc  {a = new Empty @ rc  {}, b = new Empty @ rc  {}};
            ()
        }

    @test
    def testEmptyStruct04(): Unit =
        region rc {
            new EmptyA @ rc { a = new EmptyB @ rc { a = new EmptyC @ rc { a = new Empty @ rc {} } } };
            ()
        }

    @test
    def testStructLiteral01(): Unit =
        region rc {
            new Name @ rc { name = "Lucky Luke" };
            ()
        }

    @test
    def testStructLiteral02(): Unit =
        region rc {
            new FullName @ rc { fstName = "Lucky", lstName = "Luke" };
            ()
        }

    @test
    def testStructLiteral03(): Unit =
        region rc {
            new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42 };
            ()
        }

    @test
    def testStructLiteral04(): Unit =
        region rc {
            new Cowboy @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            ()
        }

    @test
    def testStructLiteral05(): Unit =
        region rc {
            new TwoInts @ rc { x = 0, y = 0 };
            ()
        }

    @test
    def testStructLiteral06(): Unit =
        region rc {
            new ThreeInts @ rc { x = 0, y = 0, z = 0 };
            ()
        }

    @test
    def testNestedStructLiteral01(): Unit =
        region rc {
            new TwoIntsHolder @ rc { a = new TwoInts @ rc { x = 1, y = 2 }};
            ()
        }

    @test
    def testNestedStructLiteral02(): Unit =
        region rc {
            new Outer @ rc { a = new TwoInts @ rc { x = 1, y = 2 }, b = new TwoInts @ rc { x = 3, y = 4 }};
            ()
        }

    @test
    def testNestedStructLiteral03(): Unit =
        region rc {
            new A @ rc {a = new B @ rc {a = new C @ rc {a = new D @ rc {a = 42}}}};
            ()
        }

    @test
    def testNestedLabels01(): Unit =
        region rc {
            new A3 @ rc {a = new A2 @ rc {a = new A1 @ rc {a = new Empty @ rc {}}}};
            ()
        }

    @test
    def testOtherMod01(): Unit =
        region rc {
            new Mod1.S @ rc {};
            ()
        }

    @test
    def binaryTree01(): Unit =
        region rc {
            new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            ()
        }

    @test
    def binaryTree02(): Unit =
        region rc {
            let leaf = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            new BinaryTree @ rc {
                left = Some(leaf),
                right = Some(leaf),
                value = 3
            };
            ()
        }

    @test
    def binaryTree03(): Unit =
        region rc {
            let leaf = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 3
            };
            let innernode = new BinaryTree @ rc {
                left = Some(leaf),
                right = Some(leaf),
                value = 3
            };
            new BinaryTree @ rc {
                left = Some(innernode),
                right = Some(innernode),
                value = 3
            };
            ()
        }

    @test
    def testVariableNameSameAsFieldName01(): Unit =
        let left = ();
        left
}
