/*
 * Copyright 2021 Magnus Madsen
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
import {ClassSym} from "./ClassSym";
import {Instance} from "./Instance";
import {Modifier} from "./Modifier";
import {TypeParam} from "./TypeParam";
import {TypeConstraint} from "./TypeConstraint";

export interface Class {
    sym: ClassSym
    doc: [String]
    mod: [Modifier]
    tparam: TypeParam
    superClasses: [TypeConstraint]
    // TODO
    //   signatures: List[TypedAst.Sig], laws: List[TypedAst.Def], loc: SourceLocation
    instances: [Instance]
}
