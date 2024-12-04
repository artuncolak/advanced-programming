module ScriptRunner

open System
open SharedTypes

let run (filePath: string, debug: bool) =
    try
        let lines = IO.File.ReadAllLines(filePath)

        for lineNumber, line in Array.indexed lines do
            if not (String.IsNullOrWhiteSpace(line)) then
                let outputtype = Evaluate.run (line, debug)
                match outputtype with
                  | ValidOutput (value) ->
                      Console.WriteLine(value)
                  | DivideByZeroError (value) ->
                      raise (Exception($"{value} at line number {lineNumber + 1}"))
                  | LexerError (value) ->
                      raise (Exception($"{value} at line number {lineNumber + 1}"))
                  | ParserError (value) ->
                      raise (Exception($"{value} at line number {lineNumber + 1}"))
                  | GeneralError (value) ->
                      raise (Exception($"{value} at line number {lineNumber + 1}"))
//                try
//                    let _ = Evaluate.resultToString (Evaluate.run (line, debug))
//                    ()
//                with
//                | ex when ex.Message.StartsWith("Parser") ->
//                    printfn "Error on line %d: %s" (lineNumber + 1) ex.Message
//                    ()
//                | ex when ex.Message.StartsWith("Lexer") ->
//                    printfn "Error on line %d: %s" (lineNumber + 1) ex.Message
//                    ()
//                | ex ->
//                    printfn "Error on line %d: %s" (lineNumber + 1) ex.Message
//                    ()
    with
    | :? IO.FileNotFoundException -> printfn "Error: Source file not found at path: %s" filePath
    | ex -> printfn "Error reading file: %s" ex.Message
