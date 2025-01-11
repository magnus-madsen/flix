def main(): Unit \ {Net, NonDet, Sys, IO} =
    let result = App.runAllWithResult(() ->
        let lines = FileRead.readLines("Main.flix");
        List.forEach(println, lines)
    );
    println(result)
