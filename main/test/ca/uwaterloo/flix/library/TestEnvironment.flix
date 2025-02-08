/*
 * Copyright 2021 Ramiro Calle
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
mod TestEnvironment {

    @test
    def testGetEnv01(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Map.isEmpty(Environment.getEnv())
    )()

    @test
    def testGetVar01(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getVar("PATH"))
    )()

    @test
    def testGetVar02(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        Option.isEmpty(Environment.getVar("UNKNOWN"))
    )()

    @test
    def testGetProp02(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        Option.isEmpty(Environment.getProp("UNKNOWN"))
    )()

    @test
    def testGetOsName(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getOsName())
    )()

    @test
    def testGetOsArch(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getOsArch())
    )()

    @test
    def testGetOsVersion(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getOsVersion())
    )()

    @test
    def testGetFileSeparator(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not String.isEmpty(Environment.getFileSeparator())
    )()

    @test
    def testGetPathSeparator(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not String.isEmpty(Environment.getPathSeparator())
    )()

    @test
    def testGetLineSeparator(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not String.isEmpty(Environment.getLineSeparator())
    )()

    @test
    def testGetCurrentWorkingDirectory(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getCurrentWorkingDirectory())
    )()

    @test
    def testGetTemporaryDirectory(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getTemporaryDirectory())
    )()

    @test
    def testGetUserName(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getUserName())
    )()

    @test
    def testGetUserHomeDirectory(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        not Option.isEmpty(Environment.getUserHomeDirectory())
    )()

    @test
    def testGetVirtualProcessors(): Bool \ {Env, Sys, IO} = Environment.handle(() ->
        Environment.getVirtualProcessors() > 0
    )()

    @test
    def testGetArgs(): List[String] \ {Env, Sys, IO} = Environment.handle(() ->
        Environment.getArgs()
    )()

}
