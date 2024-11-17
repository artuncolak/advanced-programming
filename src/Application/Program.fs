open Interpreter

[<EntryPoint>]
let main argv =
    let hasDebug = Array.contains "--debug" argv
    let argsWithoutDebug = Array.filter ((<>) "--debug") argv

    match argsWithoutDebug with
    | [| "--repl" |] ->
        Interpreter.run (Repl, hasDebug, None)
        0
    | [| "--input"; filePath |] ->
        Interpreter.run (Script, hasDebug, Some filePath)
        0
    | _ ->
        printfn "Usage: Program [mode] [arguments]"
        printfn "Modes:"
        printfn "  --repl              Start interactive REPL mode"
        printfn "  --input <filepath>  Execute script from file"
        printfn "Options:"
        printfn "  --debug             Enable debug output"
        1
