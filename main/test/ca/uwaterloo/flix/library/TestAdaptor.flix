/*
 * Copyright 2023 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestAdaptor {

    import java.util.Optional
    import java.util.stream.Stream
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.util.{List => JList}
    import java.util.TreeMap
    import java.util.TreeSet
    import java.util.ArrayList
    import java.util.LinkedList

    /////////////////////////////////////////////////////////////////////////////
    // Comparator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def comparator01(): Bool \ IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("aaaa", "aaaa") == 0

    @test
    def comparator02(): Bool \ IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("aaaa", "bbbb") == -1

    @test
    def comparator03(): Bool \ IO =
        let c1 = Adaptor.comparator((Proxy.Proxy: Proxy[String]));
        c1.compare("bbbb", "aaaa") == 1

    /////////////////////////////////////////////////////////////////////////////
    // fromOptional                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromOptional01(): Bool \ {} =
        let o = unsafely IO run Optional.empty();
        Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o) == (None : Option[String])

    @test
    def fromOptional02(): Bool \ {} =
        let o = unsafely IO run Optional.of("Hello");
        Adaptor.fromOptional((Proxy.Proxy: Proxy[String]), o) == Some("Hello")

    /////////////////////////////////////////////////////////////////////////////
    // fromMapEntry                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMapEntry01(): Bool \ {} =
        let e = unsafely IO run JMap.entry("hello", "world");
        Adaptor.fromMapEntry((Proxy.Proxy: Proxy[String]), (Proxy.Proxy: Proxy[String]), e) == ("hello", "world")

    /////////////////////////////////////////////////////////////////////////////
    // fromList                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromList01(): Bool \ {} =
        let l = unsafely IO run JList.of();
        Adaptor.fromList(l) == (List#{} : List[String])

    @test
    def fromList02(): Bool \ {} =
        let l = unsafely IO run JList.of("hello");
        Adaptor.fromList(l) == List#{"hello"}

    @test
    def fromList03(): Bool \ {} =
        let l = unsafely IO run JList.of("hello", "world");
        Adaptor.fromList(l) == List#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromListToIterator                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromListToIterator01(): Bool \ {} = region rc {
        let l = unsafely IO run JList.of();
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == (List#{} : List[String])
    }

    @test
    def fromListToIterator02(): Bool \ {} = region rc {
        let l = unsafely IO run JList.of("hello");
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == List#{"hello"}
    }

    @test
    def fromListToIterator03(): Bool \ {} = region rc {
        let l = unsafely IO run JList.of("hello", "world");
        Iterator.toList(Adaptor.fromListToIterator(rc, (Proxy.Proxy: Proxy[String]), l)) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromSet                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromSet01(): Bool \ {} =
        let s = unsafely IO run JSet.of();
        Adaptor.fromSet(s) == (Set#{} : Set[String])

    @test
    def fromSet02(): Bool \ {} =
        let s = unsafely IO run JSet.of("hello");
        Adaptor.fromSet(s) == Set#{"hello"}

    @test
    def fromSet03(): Bool \ {} =
        let s = unsafely IO run JSet.of("hello", "world");
        Adaptor.fromSet(s) == Set#{"hello", "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromSetToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromSetToIterator01(): Bool \ {} = region rc {
        let s = unsafely IO run JSet.of();
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == (Set#{} : Set[String])
    }

    @test
    def fromSetToIterator02(): Bool \ {} = region rc {
        let s = unsafely IO run JSet.of("hello");
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == Set#{"hello"}
    }

    @test
    def fromSetToIterator03(): Bool \ {} = region rc {
        let s = unsafely IO run JSet.of("hello", "world");
        Iterator.toSet(Adaptor.fromSetToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == Set#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool \ {} =
        let m = unsafely IO run JMap.of();
        Adaptor.fromMap(m) == (Map#{} : Map[String, String])

    @test
    def fromMap02(): Bool \ {} =
        let m = unsafely IO run JMap.of("a", "hello");
        Adaptor.fromMap(m) == Map#{"a" => "hello"}

    @test
    def fromMap03(): Bool \ {} =
        let m = unsafely IO run JMap.of("a", "hello", "b", "world");
        Adaptor.fromMap(m) == Map#{"a" => "hello", "b" => "world"}

    /////////////////////////////////////////////////////////////////////////////
    // fromMapToIterator                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMapToIterator01(): Bool \ {} = region rc {
        let m = unsafely IO run JMap.of();
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == (Map#{} : Map[String, String])
    }

    @test
    def fromMapToIterator02(): Bool \ {} = region rc {
        let m = unsafely IO run JMap.of("a", "hello");
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == Map#{"a" => "hello"}
    }

    @test
    def fromMapToIterator03(): Bool \ {} = region rc {
        let m = unsafely IO run JMap.of("a", "hello", "b", "world");
        Iterator.toMap(Adaptor.fromMapToIterator(rc, m)) == Map#{"a" => "hello", "b" => "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromIterator                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromIterator01(): Bool \ {} = region rc {
        let iter = unsafely IO run JList.of().iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == (Nil : List[String])
    }

    @test
    def fromIterator02(): Bool \ {} = region rc {
        let iter = unsafely IO run JList.of("hello").iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == List#{"hello"}
    }


    @test
    def fromIterator03(): Bool \ {} = region rc {
        let iter = unsafely IO run JList.of("hello", "world").iterator();
        let flixIter = Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[String]), iter);
        Iterator.toList(flixIter) == List#{"hello", "world"}
    }

    /////////////////////////////////////////////////////////////////////////////
    // fromStreamToIterator                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromStreamToIterator01(): Bool \ {} = region rc {
        let s = unsafely IO run Stream.empty();
        Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == (Nil : List[String])
    }

    @test
    def fromStreamToIterator02(): Bool \ {} = region rc {
        let s = unsafely IO run Stream.of("hello");
        Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == "hello" :: Nil
    }

    // TODO: Broken
    // @test
    // def fromStreamToIterator03(): Bool \ {} = region rc {
    //    let arr = Array#{"hello", "world"} @ rc;
    //    unsafe let s = Stream.of(arr);
    //    Iterator.toList(Adaptor.fromStreamToIterator(rc, (Proxy.Proxy: Proxy[String]), s)) == "hello" :: "world" :: Nil
    // }

    /////////////////////////////////////////////////////////////////////////////
    // fromCollectionToIterator                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromCollectionToIterator01(): Bool \ {} = region rc {
        let col = checked_cast (unsafely IO run JList.of());
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == (Nil : List[String])
    }

    @test
    def fromCollectionToIterator02(): Bool \ {} = region rc {
        let col = checked_cast (unsafely IO run JList.of("hello"));
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == "hello" :: Nil
    }

    @test
    def fromCollectionToIterator03(): Bool \ {} = region rc {
        let col = checked_cast (unsafely IO run JList.of("hello", "world"));
        Iterator.toList(Adaptor.fromCollectionToIterator(rc, (Proxy.Proxy: Proxy[String]), col)) == "hello" :: "world" :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toOptional                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toOptional01(): Bool \ IO =
        let o = Adaptor.toOptional(None);
        o.equals(Optional.empty())

    @test
    def toOptional02(): Bool \ IO =
        let o = Adaptor.toOptional(Some("hello"));
        o.equals(Optional.of("hello"))

    /////////////////////////////////////////////////////////////////////////////
    // toMapEntry                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapEntry01(): Bool \ IO =
        let e = Adaptor.toMapEntry(("hello", "world"));
        e.equals(JMap.entry("hello", "world"))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool \ IO =
        let l = Adaptor.toList(Nil);
        JList.of().equals(l)

    @test
    def toList02(): Bool \ IO =
        let l = Adaptor.toList(List#{"hello"});
        JList.of("hello").equals(l)

    @test
    def toList03(): Bool \ IO =
        let l = Adaptor.toList(List#{"hello", "world"});
        JList.of("hello", "world").equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toArrayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArrayList01(): Bool \ IO =
        let l = Adaptor.toArrayList(Nil);
        new ArrayList(JList.of()).equals(l)

    @test
    def toArrayList02(): Bool \ IO =
        let l = Adaptor.toArrayList(List#{"hello"});
        new ArrayList(JList.of("hello")).equals(l)

    @test
    def toArrayList03(): Bool \ IO =
        let l = Adaptor.toArrayList(List#{"hello", "world"});
        new ArrayList(JList.of("hello", "world")).equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toLinkedList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toLinkedList01(): Bool \ IO =
        let l = Adaptor.toLinkedList(Nil);
        new LinkedList(JList.of()).equals(l)

    @test
    def toLinkedList02(): Bool \ IO =
        let l = Adaptor.toLinkedList(List#{"hello"});
        new LinkedList(JList.of("hello")).equals(l)

    @test
    def toLinkedList03(): Bool \ IO =
        let l = Adaptor.toLinkedList(List#{"hello", "world"});
        new LinkedList(JList.of("hello", "world")).equals(l)

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool \ IO =
        let s = Adaptor.toSet((Set#{} : Set[String]));
        JSet.of().equals(s)

    @test
    def toSet02(): Bool \ IO =
        let s = Adaptor.toSet(Set#{"hello"});
        JSet.of("hello").equals(s)

    @test
    def toSet03(): Bool \ IO =
        let s = Adaptor.toSet(Set#{"hello", "world"});
        JSet.of("hello", "world").equals(s)

    /////////////////////////////////////////////////////////////////////////////
    // toTreeSet                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeSet01(): Bool \ IO =
        let s = Adaptor.toTreeSet((Set#{} : Set[String]));
        new TreeSet(JList.of()).equals(s)

    @test
    def toTreeSet02(): Bool \ IO =
        let s = Adaptor.toTreeSet(Set#{"hello"});
        new TreeSet(JList.of("hello")).equals(s)

    @test
    def toTreeSet03(): Bool \ IO =
        let s = Adaptor.toTreeSet(Set#{"hello", "world"});
        new TreeSet(JList.of("hello", "world")).equals(s)

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool \ IO =
        let m = Adaptor.toMap((Map#{} : Map[String, String]));
        JMap.of().equals(m)

    @test
    def toMap02(): Bool \ IO =
        let m = Adaptor.toMap(Map#{"a" => "hello"});
        JMap.of("a", "hello").equals(m)

    @test
    def toMap03(): Bool \ IO =
        let m = Adaptor.toMap(Map#{"a" => "hello", "b" => "world"});
        JMap.of("a", "hello", "b", "world").equals(m)

    /////////////////////////////////////////////////////////////////////////////
    // toTreeMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toTreeMap01(): Bool \ IO =
        let m = Adaptor.toTreeMap((Map#{} : Map[String, String]));
        new TreeMap(JMap.of()).equals(m)

    @test
    def toTreeMap02(): Bool \ IO =
        let m = Adaptor.toTreeMap(Map#{"a" => "hello"});
        new TreeMap(JMap.of("a", "hello")).equals(m)

    @test
    def toTreeMap03(): Bool \ IO =
        let m = Adaptor.toTreeMap(Map#{"a" => "hello", "b" => "world"});
        new TreeMap(JMap.of("a", "hello", "b", "world")).equals(m)


}
