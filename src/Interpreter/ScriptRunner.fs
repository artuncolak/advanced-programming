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
                  | DivideByZeroError (value) ->
                      raise (Exception($"{value} on line number {lineNumber + 1}"))
                  | LexerError (value) ->
                      raise (Exception($"{value} on line number {lineNumber + 1}"))
                  | ParserError (value) ->
                      raise (Exception($"{value} on line number {lineNumber + 1}"))
                  | ArgumentError (value) ->
                      raise (Exception($"{value} on line number {lineNumber + 1}"))
                  | ValidOutput (value) ->
                      ()
    with
    | :? IO.FileNotFoundException -> printfn "Error: Source file not found at path: %s" filePath
    | ex -> printfn "Error reading file: %s" ex.Message
