/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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

///
/// The Result type.
///
/// A result represents a successful value or an error value.
/// The constructor `Ok(v)` represents the successful value `v`,
/// whereas the constructor `Err(v)` represents the error value `v`.
///
@MustUse
pub enum Result[e, t] with Eq, Order, ToString, Sendable {
    case Ok(t),
    case Err(e)
}

instance Hash[Result[e, t]] with Hash[e], Hash[t] {
    pub def hash(r: Result[e, t]): Int32 = match r {
        case Ok(v)  => 5381 + 113 * Hash.hash(v)
        case Err(v) => 5351 + 97 * Hash.hash(v)
    }
}

instance Functor[Result[e]] {
    pub def map(f: a -> b \ ef, x: Result[e, a]): Result[e, b] \ ef = Result.map(f, x)
}

instance Applicative[Result[e]] {
    pub def point(a: a) : Result[e, a] = Ok(a)
    pub def ap(f: Result[e, a -> b \ ef], x: Result[e, a]): Result[e, b] \ ef = Result.ap(f, x)
}

instance Monad[Result[e]] {
    pub def flatMap(f: a -> Result[e, b] \ ef, x : Result[e, a]): Result[e, b] \ ef = Result.flatMap(f, x)
}

mod Result {

    import java.lang.Exception

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    pub def isOk(r: Result[e, t]): Bool = match r {
        case Ok(_)  => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    pub def isErr(r: Result[e, t]): Bool = match r {
        case Ok(_)  => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    pub def getWithDefault(d: t, r: Result[e, t]): t = match r {
        case Ok(v)  => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r` is `Ok(v)`. Otherwise returns `default`.
    ///
    pub def withDefault(default: {default = Result[e2, t]}, r: Result[e1, t]): Result[e2, t] = match r {
        case Ok(v)  => Ok(v)
        case Err(_) => default#default
    }

    ///
    /// Returns `Ok(dst)` if `r` is `Ok(src)`. Otherwise returns `r`.
    ///
    pub def replace(src: {src = t}, dst: {dst = t}, r: Result[e, t]): Result[e, t] with Eq[t] = match r {
        case Ok(v)  => Ok(if (v == src#src) dst#dst else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    pub def exists(f: t -> Bool \ ef, r: Result[e, t]): Bool \ ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => false
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    pub def forAll(f: t -> Bool \ ef, r: Result[e, t]): Bool \ ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => true
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def map(f: t1 -> t2 \ ef, r: Result[e, t1]): Result[e, t2] \ ef = match r {
        case Ok(v)  => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `Err(f(e))` if `r` is `Err(e)`. Returns `Ok(v)` if `r` is `Ok(v)`.
    ///
    pub def mapErr(f: e1 -> e2 \ ef, r: Result[e1, t]): Result[e2, t] \ ef = match r {
        case Ok(v)  => Ok(v)
        case Err(w) => Err(f(w))
    }


    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def flatMap(f: t1 -> Result[e, t2] \ ef, r: Result[e, t1]): Result[e, t2] \ ef = match r {
        case Ok(v)  => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    pub def flatten(r: Result[e, Result[e, t]]): Result[e, t] = match r {
        case Ok(v)  => v
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    pub def count(f: t -> Bool \ ef, r: Result[e, t]): Int32 \ ef = match r {
        case Ok(v)  => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def sum(r: Result[e, Int32]): Int32 =
        foldLeft((acc, x) -> acc + x, 0, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def sumWith(f: t -> Int32 \ ef, r: Result[e, t]): Int32 \ ef =
        foldLeft((acc, x) -> acc + f(x), 0, r)

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: t -> Bool, r: Result[e, t]): Option[t] = match r {
        case Ok(v)  => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft(f: (a, t) -> a \ ef, z: a, r: Result[e, t]): a \ ef = match r {
        case Ok(v)  => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    pub def foldRight(f: (t, a) -> a \ ef, z: a, r: Result[e, t]): a \ ef = match r {
        case Ok(v)  => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (t, Unit -> a \ ef) -> a \ ef, z: a, r: Result[e, t]): a \ ef = match r {
        case Ok(v)  => f(v, _ -> checked_ecast(z))
        case Err(_) => z
    }

    ///
    /// Returns `Ok(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def sequence(l: List[Result[e, a]]): Result[e, List[a]] =
        def loop(ll, k) = match ll {
            case Nil         => k(Nil)
            case Err(e) :: _ => Err(e)
            case Ok(x) :: xs => loop(xs, ks -> k(x :: ks))
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def traverse(f: a -> Result[e, b] \ ef, l: List[a]): Result[e, List[b]] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Ok(y)  => loop(xs, ks -> k(y :: ks))
                case Err(e) => Err(e)
            }
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Ok()` if each of `f(l_i)` is `Ok(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Result[e, b] \ ef, l: List[a]): Result[e, Unit] \ ef = match l {
        case Nil     => Ok()
        case x :: xs => match f(x) {
            case Ok(_)  => traverseX(f, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `l` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `l` the result is of the form:
    /// `Ok(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Result[e, b] \ ef, s: b, l: List[a]): Result[e, b] \ ef = match l {
        case Nil     => Ok(s)
        case x :: xs => match f(s, x) {
            case Ok(s1) => foldLeftM(f, s1, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from right to left.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `l` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `l` the result is of the form:
    /// `Ok(f(x1, ...f(xn-1, f(xn, s))...))`.
    ///
    pub def foldRightM(f: (a, b) -> Result[e, b] \ ef, s: b, l: List[a]): Result[e, b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, s1 -> match f(x, s1) {
                case Ok(s2) => k(s2)
                case Err(e) => Err(e)
            })
        };
        loop(l, s1 -> checked_ecast(Ok(s1)))

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    pub def toList(r: Result[e, t]): List[t] = match r {
        case Ok(v)  => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty set.
    ///
    pub def toSet(r: Result[e, t]): Set[t] with Order[t] = match r {
        case Ok(v)  => Set.singleton(v)
        case Err(_) => Set.empty()
    }

    ///
    /// Returns a singleton map with the mapping `k -> v` if `o` is `Ok((k, v))`. Otherwise returns the empty map.
    ///
    pub def toMap(r: Result[e, (k, v)]): Map[k, v] with Order[k] = match r {
        case Ok((k, v)) => Map.singleton(k, v)
        case Err(_)     => Map.empty()
    }

    ///
    /// Returns a map with elements of `s` as keys and `f` applied as values.
    ///
    pub def toMapWith(f: a -> b, s: Result[a, a]): Map[a, b] with Order[a] =
        Result.foldRight((x, acc) -> Map.insert(x, f(x), acc), Map.empty(), s)

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    pub def toOption(r: Result[e, t]): Option[t] = match r {
        case Ok(v)  => Some(v)
        case Err(_) => None
    }

    ///
    /// Returns a `Success(v)` if `r` is `Ok(v)`. Otherwise returns a
    /// `Failure(Nec.singleton(t))` if `r` is `Err(t)`.
    ///
    pub def toValidation(r: Result[e, t]): Validation[e, t] = match r {
        case Ok(v) => Validation.Success(v)
        case Err(t) => Validation.Failure(Nec.singleton(t))
    }

    ///
    /// Applies `f` to `v` if `r` is `Ok(v)`. Otherwise does nothing.
    ///
    pub def forEach(f: t -> Unit \ ef, r: Result[e, t]): Unit \ ef = match r {
        case Ok(v)  => f(v)
        case Err(_) => ()
    }

    ///
    /// Applies the function in `r1` to the value in `r2`.
    ///
    pub def ap(r1: Result[e, t -> u \ ef], r2: Result[e, t]): Result[e, u] \ ef = match r1 {
        case Err(e) => Err(e)
        case Ok(f)  => match r2 {
            case Ok(a)  => Ok(f(a))
            case Err(e) => Err(e)
        }
    }

    ///
    /// Applies the binary function `f` to the values in `r1` and `r2`.
    ///
    /// Returns the first `Err(e)` value if either of `r1` and `r2` are `Err(e)`.
    ///
    pub def map2(f: (t1, t2) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2]): Result[e, u] \ ef =
        ap(map(f, r1), r2)

    ///
    /// Applies the ternary function `f` to the values in `r1`, `r2` and `r3`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2` and `r3` are `Err(e)`.
    ///
    pub def map3(f: (t1, t2, t3) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3]): Result[e, u] \ ef =
        ap(map2(f, r1, r2), r3)

    ///
    /// Applies the 4-ary function `f` to the values in `r1`, `r2`, `r3` and `r4`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, `r3` and `r4` are `Err(e)`.
    ///
    pub def map4(f: (t1, t2, t3, t4) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4]): Result[e, u] \ ef =
        ap(map3(f, r1, r2, r3), r4)

    ///
    /// Applies the 5-ary function `f` to the values in `r1`, `r2`, ... `r5`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r5` are `Err(e)`.
    ///
    pub def map5(f: (t1, t2, t3, t4, t5) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5]): Result[e, u] \ ef =
        ap(map4(f, r1, r2, r3, r4), r5)

    ///
    /// Applies the 6-ary function `f` to the values in `r1`, `r2`, ... `r6`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r6` are `Err(e)`.
    ///
    pub def map6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5], r6: Result[e, t6]): Result[e, u] \ ef =
        ap(map5(f, r1, r2, r3, r4, r5), r6)

    ///
    /// Applies the 7-ary function `f` to the values in `r1`, `r2`, ... `r7`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r7` are `Err(e)`.
    ///
    pub def map7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5], r6: Result[e, t6], r7: Result[e, t7]): Result[e, u] \ ef=
        ap(map6(f, r1, r2, r3, r4, r5, r6), r7)

    ///
    /// Applies the 8-ary function `f` to the values in `r1`, `r2`, ... `r8`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r8` are `Err(e)`.
    ///
    pub def map8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5], r6: Result[e, t6], r7: Result[e, t7], r8: Result[e, t8]): Result[e, u] \ ef =
        ap(map7(f, r1, r2, r3, r4, r5, r6, r7), r8)

    ///
    /// Applies the 9-ary function `f` to the values in `r1`, `r2`, ... `r9`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r9` are `Err(e)`.
    ///
    pub def map9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5], r6: Result[e, t6], r7: Result[e, t7], r8: Result[e, t8], r9: Result[e, t9]): Result[e, u] \ ef =
        ap(map8(f, r1, r2, r3, r4, r5, r6, r7, r8), r9)

    ///
    /// Applies the 10-ary function `f` to the values in `r1`, `r2`, ... `r10`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r10` are `Err(e)`.
    ///
    pub def map10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, r1: Result[e, t1], r2: Result[e, t2], r3: Result[e, t3], r4: Result[e, t4], r5: Result[e, t5], r6: Result[e, t6], r7: Result[e, t7], r8: Result[e, t8], r9: Result[e, t9], r10: Result[e, t10]): Result[e, u] \ ef =
        ap(map9(f, r1, r2, r3, r4, r5, r6, r7, r8, r9), r10)

    ///
    /// Returns an iterator over `r` with 1 element or an empty iterator if `r` is `Err`.
    ///
    pub def iterator(rc: Region[r], r: Result[e, t]): Iterator[t, r, r] \ r = match r {
        case Err(_) => Iterator.empty(rc)
        case Ok(x)  => Iterator.singleton(rc, x)
    }

    ///
    /// Returns an iterator over `r` zipped with the indices of the elements.
    ///
    pub def enumerator(rc: Region[r], r: Result[e, t]): Iterator[(Int32, t), r, r] \ r =
        iterator(rc, r) |> Iterator.zipWithIndex

    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    ///
    /// If `f` throws a Java `Exception`, `Err(e)` is returned
    /// where `e` is the error message.
    ///
    pub def tryCatch(f: Unit -> a \ ef): Result[String, a] \ ef =
        try {
            Ok(f())
        } catch {
            case e: Exception => Err(unsafely IO run e.getMessage())
        }

}
