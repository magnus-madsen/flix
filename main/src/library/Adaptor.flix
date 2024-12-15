/*
 *  Copyright 2023 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod Adaptor {

    import java.lang.Object
    import java.util.ArrayList
    import java.util.Collection
    import java.util.Comparator
    import java.util.{Iterator => JIterator}
    import java.util.LinkedList
    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.{Map$Entry => Entry}
    import java.util.Optional
    import java.util.{Set => JSet}
    import java.util.TreeMap
    import java.util.TreeSet
    import java.util.stream.BaseStream
    import java.util.stream.Stream

    ///
    /// Returns a comparator for elements of type `a`.
    ///
    /// The comparator is backed by the ordering on `a`.
    ///
    /// Note: Currently requires a Proxy (until Java types support generics).
    ///
    pub def comparator(_: Proxy[a]): Comparator \ IO with Order[a] =
        let f1 = (o1, o2) -> {
            let a = unchecked_cast(o1 as a);
            let b = unchecked_cast(o2 as a);
            Order.compare(a, b) |> Comparison.toInt32
        };
        new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                f1(t, u)
        }

    ///
    /// Returns the given Java Optional as a Flix Option.
    ///
    pub def fromOptional(_: Proxy[a], o: Optional): Option[a] =
        if (unsafely IO run o.isPresent()) {
            let o1 = unsafely IO run o.get();
            Some(unchecked_cast(o1 as a))
        } else
            None

    ///
    /// Returns the given Java `Map.Entry` as a Flix tuple.
    ///
    pub def fromMapEntry(_: Proxy[k], _: Proxy[v], e: Entry): (k, v) =
        let k1 = unsafely IO run e.getKey();
        let v1 = unsafely IO run e.getValue();
        (unchecked_cast(k1 as k), unchecked_cast(v1 as v))


    ///
    /// Returns all elements in the given Java List as a Flix List.
    ///
    pub def fromList(l: JList): List[a] = region rc {
        def iterator(x) = unchecked_cast(x.iterator() as _ \ rc);
        iterator(l) |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toList
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java List.
    ///
    pub def fromListToIterator(rc: Region[r], prx: Proxy[a], l: JList): Iterator[a, r, r] \ r =
        def iterator(x) = unchecked_cast(x.iterator() as _ \ r);
        iterator(l) |> fromIterator(rc, prx)

    ///
    /// Returns all elements in the given Java Set as a Flix Set.
    ///
    /// WARNING: The Flix Set will use the ordering defined on `a`.
    ///
    pub def fromSet(l: JSet): Set[a] with Order[a] = region rc {
        def iterator(x) = unchecked_cast(x.iterator() as _ \ rc);
        iterator(l) |> fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toSet
    }

    ///
    /// Returns a Flix Iterator of the elements in the given Java Set.
    ///
    pub def fromSetToIterator(rc: Region[r], prx: Proxy[a], s: JSet): Iterator[a, r, r] \ r =
        def iterator(x) = unchecked_cast(x.iterator() as _ \ r);
        iterator(s) |> fromIterator(rc, prx)

    ///
    /// Returns all key-value pairs in the given Java Map as a Flix Map.
    ///
    /// WARNING: The Flix Map will use the ordering defined on `k`.
    ///
    pub def fromMap(m: JMap): Map[k, v] with Order[k] = region rc {
        def step(acc, entry) = {
            let (k, v) = entry;
            Map.insert(k, v, acc)
        };
        fromMapToIterator(rc, m) |> Iterator.foldLeft(step, Map.empty())
    }

    ///
    /// Returns a Flix Iterator of the key-value pairs in the given Java Map.
    ///
    pub def fromMapToIterator(rc: Region[r], m: JMap): Iterator[(k, v), r, r] \ r =
        def entrySet(x) = unchecked_cast(x.entrySet() as _ \ r);
        def iterator(x) = unchecked_cast(x.iterator() as _ \ r);
        def hasNext(x) = unchecked_cast(x.hasNext() as _ \ r);
        def next(x) = unchecked_cast(x.next() as _ \ r);
        let entries = entrySet(m);
        let iter = iterator(entries);
        let getNext = () -> match hasNext(iter) {
            case true  => {
                let entry = {let obj = next(iter); unchecked_cast(obj as Entry)};
                let (k, v) = fromMapEntry((Proxy.Proxy: Proxy[k]), (Proxy.Proxy: Proxy[v]), entry);
                Some((k, v))
            }
            case false => None
        };
        Iterator.iterate(rc, getNext)

    ///
    /// Returns a fresh Flix `Iterator` from the Java iterator `iter`.
    ///
    pub def fromIterator(rc: Region[r], _: Proxy[a], iter: JIterator): Iterator[a, r, r] =
        def hasNext(x) = unchecked_cast(x.hasNext() as _ \ r);
        def next(x) = unchecked_cast(x.next() as _ \ r);
        let step = () -> {
            match hasNext(iter) {
                case true  => next(iter) |> (o -> unchecked_cast(o as a)) |> Some
                case false => None
            }
        };
        let iterF = () -> (step());
        Iterator.iterate(rc, iterF)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Stream.
    ///
    pub def fromStreamToIterator(rc: Region[r], proxy: Proxy[a], strm: Stream): Iterator[a, r, r] \ r =
        let baseStream: BaseStream = checked_cast(strm);
        let iter = unchecked_cast ((baseStream.iterator(): _ \ IO) as _ \ r);
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns a Flix Iterator of the elements in the given Java Collection.
    ///
    pub def fromCollectionToIterator(rc: Region[r], proxy: Proxy[a], col: Collection): Iterator[a, r, r] \ r =
        let iter = unchecked_cast ((col.iterator(): _ \ IO) as _ \ r);
        Adaptor.fromIterator(rc, proxy, iter)

    ///
    /// Returns the `o` as a new Java `Optional`.
    ///
    pub def toOptional(o: Option[a]): Optional \ IO =
        match o {
            case None    => Optional.empty()
            case Some(a) => Optional.of(unchecked_cast(a as Object))
        }

    ///
    /// Returns the key-value pair `p` as a new Java `Map.Entry`.
    ///
    pub def toMapEntry(p: (k, v)): Entry \ IO =
        let (k, v) = p;
        JMap.entry(unchecked_cast(k as Object), unchecked_cast(v as Object))

    ///
    /// Alias for `toArrayList`.
    ///
    pub def toList(ma: m[a]): JList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        checked_cast(toArrayList(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `ArrayList`.
    ///
    /// Creates a fresh `ArrayList` and copies all elements in `ma` into it.
    ///
    pub def toArrayList(ma: m[a]): ArrayList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        let alist = new ArrayList();
        Foldable.forEach(x -> discard alist.add(unchecked_cast(x as Object)), ma);
        alist

    ///
    /// Returns the elements of the given foldable `ma` as a new Java `LinkedList`.
    ///
    /// Creates a fresh `LinkedList` and copies all elements in `ma` into it.
    ///
    pub def toLinkedList(ma: m[a]): LinkedList \ (IO + Foldable.Aef[m]) with Foldable[m] =
        let llist = new LinkedList();
        Foldable.forEach(x -> discard llist.add(unchecked_cast(x as Object)), ma);
        llist

    ///
    /// Alias for `toTreeSet`.
    ///
    pub def toSet(ma: m[a]): JSet \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] = checked_cast(toTreeSet(ma))

    ///
    /// Returns the elements of the given foldable `ma` as a new `TreeSet`.
    ///
    pub def toTreeSet(ma: m[a]): TreeSet \ (IO + Foldable.Aef[m]) with Order[a], Foldable[m] =
        let tset = new TreeSet();
        Foldable.forEach(x -> discard tset.add(unchecked_cast(x as Object)), ma);
        tset

    ///
    /// Alias for `toTreeMap`.
    ///
    pub def toMap(m: Map[k, v]): JMap \ IO with Order[k] = checked_cast(toTreeMap(m))

    ///
    /// Returns all key-value pairs of the given map `m` as a new `TreeMap`.
    ///
    /// The `TreeMap` uses a `Comparator` constructed from the `Order` on `k`.
    ///
    pub def toTreeMap(m: Map[k, v]): TreeMap \ IO with Order[k] =
        let tmap = new TreeMap();
        Map.forEach((k, v) -> discard tmap.put(unchecked_cast(k as Object), unchecked_cast(v as Object)), m);
        tmap


}
