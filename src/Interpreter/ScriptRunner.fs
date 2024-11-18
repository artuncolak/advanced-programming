module ScriptRunner

open System

let run (filePath: string, debug: bool) =
    try
        let lines = IO.File.ReadAllLines(filePath)

        for lineNumber, line in Array.indexed lines do
            if not (String.IsNullOrWhiteSpace(line)) then
                try
                    let _ = Evaluate.run (line, debug)
                    ()
                with
                | ex when ex.Message.StartsWith("Parser error") ->
                    printfn "Error on line %d: %s" (lineNumber + 1) ex.Message
                    ()
                | ex ->
                    printfn "Error on line %d: %s" (lineNumber + 1) ex.Message
                    ()
    with
    | :? IO.FileNotFoundException -> printfn "Error: Source file not found at path: %s" filePath
    | ex -> printfn "Error reading file: %s" ex.Message
