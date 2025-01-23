/*
 * Copyright 2024 Lionel Mendes
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

//
// Acknowledgement: this is an (free) adaptation from https://typelevel.org/cats-collections/range.html
//

pub enum Range[t] {
    case Range(t, t)
    case Empty
}

instance ToString[Range[t]] with ToString[t] {
    pub def toString(r: Range[t]) : String = Data.Range.toString(r)
}

instance Iterable[Range[t]] with Discrete[t] {
    type Elm = t
    pub def iterator(rc: Region[r], r: Range[t]): Iterator[t, r, r] \ r = Data.Range.iteratorBuilder(rc, r)
}

instance Discrete[Int32] {
    pub def succ(x: Int32): Int32 = x + 1
    pub def prev(x: Int32): Int32 = x - 1
}

mod Data.Range {

    ///
    /// Renders the range `r` to a String.
    ///
    pub def toString(r: Range[t]): String with ToString[t] = match r {
        case Range.Range(start,end)     => "[${start} , ${end}]"
        case Range.Empty                => "Empty"
    }

    ///
    /// Returns an iterator from range `r`
    ///
    pub def iteratorBuilder(rc: Region[r], r: Range[t]): Iterator[t, r, r] \ r with Discrete[t] =
        match r {
            case Range.Range(start,end) =>  def next(x:t): (Bool, t) = if (start <= end) (x <= end, Discrete.succ(x) ) else (x >= end, Discrete.prev(x));

                                            let curr = Ref.fresh(rc, Some(start));
                                            let iterF = () -> match (Ref.get(curr)) {
                                                                case Some(i) if fst(next(i))    =>  Ref.put(Some(snd(next(i))), curr);
                                                                                                    Some(i)
                                                                case _                          =>  None
                                                        };
                                            Iterator.iterate(rc, iterF)
            case _                      =>  Iterator.empty(rc)
        }

    ///
    /// Returns a `Range` reversed. Means lower bound becomes upper one and vice versa
    ///
    pub def reverse(r: Range[t]): Range[t] =
        match r {
            case Range.Range(start,end) => Range.Range(end, start)
            case _    => Range.Empty
        }

    ///
    /// Returns a true is range `r` is empty and false is not
    ///
    pub def isEmpty(r: Range[t]): Bool =
        match r {
            case Range.Range(_ , _) => false
            case Range.Empty        => true
        }

    ///
    /// Returns a true is `x` value is included into the range `r` and false is not
    ///
    pub def contains(x:t, r: Range[t]): Bool with Order[t] =
        match r {
            case Range.Range(start,end) if (start <= end) => if ((x >= start) and (x <= end)) true else false
            case Range.Range(start,end) if (start >= end) => if ((x <= start) and (x >= end)) true else false
            case _    => false
         }

}
