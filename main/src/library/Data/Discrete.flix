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
// Acknowledgement: this is an adaptation from https://typelevel.org/cats-collections/discrete.html
//

///
/// Discrete represents discrete operations that can be performed on a type `t`
/// These operations are presented by the following functions.
/// def succ(x): Returns the successor value of x
/// def pred(x): Returns the predecessor value of x
///
pub trait Discrete[t] with Order[t] {
        pub def succ(x: t): t
        pub def prev(x: t): t
    }
