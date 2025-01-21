def main(): Unit \ {Net, NonDet, Sys, IO} =
    run {
        Console.println("Please enter your name: ");
        let name = Console.readln();
        Console.println("Hello ${name}")
    } with Console.runWithIO
