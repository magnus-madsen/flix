def main(): Unit \ {Env, Net, Sys, IO} =
    App.runAll(() -> {
        let m = "${Chalk.blue("Hello")} ${Chalk.green("World!")}!";
        Console.println(m)
    })
