/*
 * Copyright 2019 Magnus Madsen
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

mod Time {

    import java.time.{Instant => JInstant}

    ///
    /// A wrapper around a Java instant.
    ///
    enum Instant(JInstant)

    instance ToString[Instant] {
        pub def toString(x: Time.Instant): String = Instant.toString(x)
    }

    mod Instant {

        import java.time.{Instant => JInstant}

        ///
        /// Returns the current time from the system clock.
        ///
        @Experimental
        pub def now(): Time.Instant \ IO =
            Time.Instant.Instant(JInstant.now())

        ///
        /// Returns `true` if `x` is before `y`.
        ///
        @Experimental
        pub def before(x: Time.Instant, y: Time.Instant): Bool =
            let Time.Instant.Instant(t1) = x;
            let Time.Instant.Instant(t2) = y;
            unsafely IO run t1.isBefore(t2)

        ///
        /// Returns `true` if `x` is after `y`.
        ///
        @Experimental
        pub def after(x: Time.Instant, y: Time.Instant): Bool =
            let Time.Instant.Instant(t1) = x;
            let Time.Instant.Instant(t2) = y;
            unsafely IO run t1.isAfter(t2)

        ///
        /// Returns the given instant `x` plus `d` number of nano seconds.
        ///
        @Experimental
        pub def plus(x: Time.Instant, d: Int64): Time.Instant =
            let Time.Instant.Instant(t) = x;
            Time.Instant.Instant(unsafely IO run t.plusNanos(d))

        ///
        /// Returns the given instant `x` minus `d` number of nano seconds.
        ///
        @Experimental
        pub def minus(x: Time.Instant, d: Int64): Time.Instant =
            let Time.Instant.Instant(t) = x;
            Time.Instant.Instant(unsafely IO run t.minusNanos(d))

        ///
        /// Returns `x` as a string using the ISO-8601 representation.
        ///
        @Experimental
        pub def toString(x: Time.Instant): String =
            let Time.Instant.Instant(t) = x;
            unsafely IO run t.toString()

    }

}
