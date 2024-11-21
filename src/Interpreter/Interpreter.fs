namespace Interpreter

open Types

module Interpreter =
    let run (mode: Mode, debug: bool, filePath: string option) =
        match mode with
        | Repl -> Repl.run (debug)
        | Script ->
            match filePath with
            | Some path -> ScriptRunner.run (path, debug)
            | None -> failwith "Script mode requires a file path"

    let evaluate (input: string) =
        let result = Evaluate.run (input, false)
        result
