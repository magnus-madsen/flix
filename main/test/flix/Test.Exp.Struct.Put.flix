mod Test.Exp.Struct.Put {

    struct Person[r] {
        mut fstName: String,
        mut lstName: String,
        mut age: Int32,
        mut cowboy: Bool
    }

    @test
    def testStructPut01(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            s->fstName == "Unlucky"
        }

    @test
    def testStructPut02(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->lstName = "Uriel";
            s->lstName == "Uriel"
        }

    @test
    def testStructPut03(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->age = 23;
            s->age == 23
        }

    @test
    def testStructPut04(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->cowboy = false;
            not s->cowboy and s->fstName == "Lucky" and s->lstName == "Luke" and s->age == 42
        }

    @test
    def testStructMultiPut01(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            s->lstName = "Uriel";
            s->age = 23;
            s->cowboy = false;
            s->fstName == "Unlucky" and s->lstName == "Uriel" and s->age == 23 and s->cowboy == false
        }

    @test
    def testStructMultiPut02(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->cowboy = false;
            s->age = 23;
            s->lstName = "Uriel";
            s->fstName = "Unlucky";
            s->fstName == "Unlucky" and s->lstName == "Uriel" and s->age == 23 and s->cowboy == false
        }

    @test
    def testStructMultiPut03(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->fstName = "Unlucky";
            s->fstName = "Lucky";
            s->lstName = "Uriel";
            s->lstName = "Luke";
            s->lstName == "Luke" and s->fstName == "Lucky" and s->age == 42 and s->cowboy == true
        }

    @test
    def testStructMultiPut04(): Bool =
        region rc {
            let s = new Person @ rc { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
            s->lstName = "Uriel";
            s->fstName = "Unlucky";
            s->lstName = "Luke";
            s->fstName = "Lucky";
            s->fstName == "Lucky" and s->lstName == "Luke" and s->age == 42 and s->cowboy == true
        }

    struct BinaryTree[t, r] {
        mut left: Option[BinaryTree[t, r]],
        mut right: Option[BinaryTree[t, r]],
        mut value: t
    }

    def binaryTreeSum(tree: Option[BinaryTree[Int32, r]]): Int32 \ r =
        match tree {
            case None => 0
            case Some(t) => t->value + binaryTreeSum(t->left) + binaryTreeSum(t->right)
        }

    def doubleTree(tree: Option[BinaryTree[Int32, r]]): Unit \ {r, IO} =
        match tree {
            case None => ()
            case Some(t) =>
                t->value = t->value + t->value;
                doubleTree(t->left);
                doubleTree(t->right)
        }

    @test
    def binaryTree01(): Bool \ IO =
        region rc {
            let leaf1 = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 10
            };
            let leaf2 = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 9
            };
            let leaf3 = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 8
            };
            let leaf4 = new BinaryTree @ rc {
                left = None,
                right = None,
                value = 7
            };
            let innernode1 = new BinaryTree @ rc {
                left = Some(leaf1),
                right = Some(leaf2),
                value = 4
            };
            let innernode2 = new BinaryTree @ rc {
                left = Some(leaf3),
                right = Some(leaf4),
                value = 4
            };
            let tree = new BinaryTree @ rc {
                left = Some(innernode1),
                right = Some(innernode2),
                value = 5
            };
            doubleTree(Some(tree));
            binaryTreeSum(Some(tree)) == 94
        }

    struct Nested[t, r] {
        mut value : t
    }

    @test
    def nestedPut01(): Bool =
        region rc {
            let nested = new Nested @ rc { value = new Nested @ rc  { value = new Nested @ rc  {value = new Nested @ rc  {value = 15}}}};
            nested->value->value->value->value == 15;
            nested->value->value->value->value = 37;
            nested->value->value->value->value == 37
        }

    struct List[t, r] {
        mut value: t,
        mut next: Option[List[t, r]]
    }

    def listAppend(list: List[t, r], rc: Region[r], value: t): Unit \ r = match list->next {
        case Some(l) => listAppend(l, rc, value)
        case None => list->next = Some(new List @ rc {value = value, next = None})
    }

    def listSum(list: List[Int32, r]): Int32 \ r =
        list->value + match list->next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listDifference(list: List[Int32, r]): Int32 \ r =
        list->value - match list->next {
            case Some(l) => listSum(l)
            case None => 0
        }

    def listReverse(list: List[Int32, r], rc: Region[r]): List[Int32, r] \ r =
        match list->next {
            case None => list
            case Some(l) =>
                let reversed = listReverse(l, rc);
                listAppend(reversed, rc, list->value);
                reversed
        }

    @test
    def testList01(): Bool =
        region rc {
            let list = new List @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55
        }

    @test
    def testList02(): Bool =
        region rc {
            let list = new List @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55
        }

    @test
    def testList03(): Bool =
        region rc {
            let list = new List @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listSum(list) == 55 and listSum(listReverse(list, rc)) == 55
        }

    @test
    def testList04(): Bool =
        region rc {
            let list = new List @ rc { value = 1, next = None };
            listAppend(list, rc, 2);
            listAppend(list, rc, 3);
            listAppend(list, rc, 4);
            listAppend(list, rc, 5);
            listAppend(list, rc, 6);
            listAppend(list, rc, 7);
            listAppend(list, rc, 8);
            listAppend(list, rc, 9);
            listAppend(list, rc, 10);
            listDifference(list) == -53 and listDifference(listReverse(list, rc)) == -35
        }
}
