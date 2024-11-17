module ScriptRunner

open System

let run (filePath: string, debug: bool) =
    try
        let lines = IO.File.ReadAllLines(filePath)

        for line in lines do
            if not (String.IsNullOrWhiteSpace(line)) then
                let _ = Evaluate.run (line, debug)
                ()
    with
    | :? IO.FileNotFoundException -> printfn "Error: Source file not found at path: %s" filePath
    | ex -> printfn "Error reading/evaluating file: %s" ex.Message
