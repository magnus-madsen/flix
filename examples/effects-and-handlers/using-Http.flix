def main(): Unit \ {Net, NonDet, Sys, IO} =
    let result = App.runAllWithResult(() ->
        let response = Http.get("http://example.com/", Map.empty());
        let body = Http.Response.body(response);
        println(body)
    );
    println(result)
