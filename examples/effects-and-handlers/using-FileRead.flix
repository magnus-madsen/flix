def main(): Unit \ {Env, Net, Sys, IO} =
    let result = App.runAllWithResult(() ->
        let lines = FileRead.readLines("Main.flix");
        List.forEach(println, lines)
    );
    println(result)
