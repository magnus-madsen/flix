/*
 *  Copyright 2024 Holger Dal Mogensen
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

///
/// The effect used to interact with the HTTP protocol.
///
eff Http {
    def request(method: String, url: String, headers: Map[String, List[String]], body: Option[String]): Http.Response
}

mod Http {

    use IoError.ErrorKind
    use IoError.IoError

    import java.io.IOException
    import java.io.UncheckedIOException
    import java.lang.IllegalArgumentException
    import java.lang.InterruptedException
    import java.net.ConnectException
    import java.net.http.HttpClient
    import java.net.http.{HttpResponse$BodyHandlers => BodyHandlers}
    import java.net.http.HttpRequest
    import java.net.http.{HttpRequest$BodyPublishers => BodyPublishers}
    import java.net.URI
    import java.util.{List => JList}

    ///
    /// A response from an HTTP request.
    ///
    pub enum Response({status = Int32, headers = Map[String, List[String]], body = String})

    mod Response {
        pub def status(r: Response): Int32 = match r {
            case Response(x) => x#status
        }
        pub def headers(r: Response): Map[String, List[String]] = match r {
            case Response(x) => x#headers
        }
        pub def body(r: Response): String = match r {
            case Response(x) => x#body
        }
    }

    ///
    /// Send a `GET` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def get(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("GET", url, headers, None)

    ///
    /// Send a `HEAD` request to the given `url` with the given `headers` and wait for the response.
    ///
    /// A `HEAD` request is identical to a `GET` request except that the server should not return a message-body in the response.
    ///
    pub def head(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("HEAD", url, headers, None)

    ///
    /// Send a `DELETE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def delete(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("DELETE", url, headers, None)

    ///
    /// Send an `OPTIONS` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def options(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("OPTIONS", url, headers, None)

    ///
    /// Send a `TRACE` request to the given `url` with the given `headers` and wait for the response.
    ///
    pub def trace(url: String, headers: Map[String, List[String]]): Http.Response \ Http =
        Http.request("TRACE", url, headers, None)

    ///
    /// Send a `POST` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def post(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("POST", url, headers, Some(body))

    ///
    /// Send a `PUT` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def put(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("PUT", url, headers, Some(body))

    ///
    /// Send a `PATCH` request to the given `url` with the given `headers` and `body` and wait for the response.
    ///
    pub def patch(url: String, headers: Map[String, List[String]], body: String): Http.Response \ Http =
        Http.request("PATCH", url, headers, Some(body))

    ///
    /// Handles the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `Net`, `NonDet`, and `IO` effects.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - Http) + {Net, NonDet, IO} = x ->
        run {
            Ok(f(x))
        } with handler Http {
            def request(method, url, headers, body, k) =
                try {
                    let bodyPublisher = match body {
                       case Some(b) => BodyPublishers.ofString(b)
                       case None    => BodyPublishers.noBody()
                    };
                    let builder =
                       HttpRequest.newBuilder(URI.create(url)).method(method, bodyPublisher)
                           |> builder -> Map.foldLeftWithKey(
                               (mb, k, l) -> List.foldLeft((lb, v) -> lb.header(k, v), mb, l),
                               builder,
                               headers
                           );
                    let request = builder.build();

                    let client = HttpClient.newHttpClient();
                    let response = client.send(request, BodyHandlers.ofString());

                    let responseHeadersJlist: Map[String, JList] = ToFlix.toFlix(response.headers().map());
                    let responseHeaders: Map[String, List[String]] = Map.map(ToFlix.toFlix, responseHeadersJlist);

                    k(Http.Response.Response({
                       status = response.statusCode(),
                       headers = responseHeaders,
                       body = response.body().toString() // Body already is a string but has type Object
                    }))
                } catch {
                    case ex: IllegalArgumentException => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: ConnectException          => Err(IoError(ErrorKind.ConnectionFailed, ex.getMessage()))
                    case ex: InterruptedException     => Err(IoError(ErrorKind.Interrupted, ex.getMessage()))
                    case ex: UncheckedIOException     => Err(IoError(ErrorKind.Other, ex.getMessage()))
                    case ex: IOException              => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
        }

    ///
    /// Runs the `Http` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Http` effect using the `Net`, `NonDet`, and `IO` effects.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ (ef - Http) + {Net, NonDet, IO} = handle(f)()

}
