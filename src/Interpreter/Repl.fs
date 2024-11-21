module Repl

open System

let run (debug: bool) =
    printfn "Interpreter (type 'exit' to quit)"
    let mutable running = true

    while running do
        printf "> "
        let input = Console.ReadLine()

        if input.ToLower() = "exit" then
            running <- false
        else
            let result = Evaluate.run (input, debug)
            printfn "%s" (Utils.formatResult result)
