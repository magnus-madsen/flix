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

mod Test.Data.Range {
    use Data.Range.{reverse, contains, isEmpty}

    @Test
    def range01(): Bool = region rc {
        let expected = List#{-2,-1,0,1,2,3,4,5,6,7,8,9};
        let r = Range.Range(-2, 9);
        let v = MutList.empty(rc);
        foreach (x <- r) {  MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range02(): Bool = region rc {
        let expected = List#{9,8,7,6,5,4,3,2,1,0,-1};
        let r = Range.Range(9, -1);
        let v = MutList.empty(rc);
        foreach (x <- r) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def range03(): Bool = region rc {
        let expected = List#{};
        let r:Range[Int32] = Range.Empty;
        let v = MutList.empty(rc);
        foreach (x <- r) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def reverseRange01(): Bool = region rc {
        let expected = List#{9,8,7,6,5,4,3,2,1,0,-1};
        let r = reverse(Range.Range(-1, 9));
        let v = MutList.empty(rc);
        foreach (x <- r) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def reverseRange02(): Bool = region rc {
        let expected = List#{};
        let r:Range[Int32] = reverse(Range.Empty);
        let v = MutList.empty(rc);
        foreach (x <- r) { MutList.push!(x, v) };
        let actual = MutList.toList(v);
        Assert.eq(expected, actual)
    }

    @Test
    def contains01(): Bool =
        let expected = false;
        let r:Range[Int32] = Range.Empty;
        let actual = contains(0,r);
        Assert.eq(expected, actual)

    @Test
    def contains02(): Bool =
        let expected = true;
        let r = Range.Range(0,5);
        let actual = contains(0,r);
        Assert.eq(expected, actual)

    @Test
    def contains03(): Bool =
        let expected = true;
        let r = Range.Range(0,5);
        let actual = contains(5,r);
        Assert.eq(expected, actual)

    @Test
    def contains04(): Bool =
        let expected = false;
        let r = Range.Range(0,5);
        let actual = contains(-1,r);
        Assert.eq(expected, actual)

    @Test
    def contains05(): Bool =
        let expected = true;
        let r = Range.Range(5,0);
        let actual = contains(0,r);
        Assert.eq(expected, actual)

    @Test
    def contains06(): Bool =
        let expected = true;
        let r = Range.Range(5,0);
        let actual = contains(5,r);
        Assert.eq(expected, actual)

    @Test
    def contains07(): Bool =
        let expected = false;
        let r = Range.Range(5,0);
        let actual = contains(6,r);
        Assert.eq(expected, actual)

    @Test
    def contains08(): Bool =
        let expected = false;
        let r = Range.Range(5,0);
        let actual = contains(-1,r);
        Assert.eq(expected, actual)

    @Test
    def toString01(): Bool =
        let expected = "[0 , 5]";
        let r = Range.Range(0,5);
        let actual = "${r}";
        Assert.eq(expected, actual)

    @Test
    def toString02(): Bool =
        let expected = "Empty";
        let r:Range[Int32] = Range.Empty;
        let actual = "${r}";
        Assert.eq(expected, actual)

}
