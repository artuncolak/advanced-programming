module Evaluate

open System
open SharedTypes
open Lexer
open SymbolTable
open Parser

let run (input: string, debug: bool) =
    try
        if debug then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine("\n================OUTPUT================\n")
            Console.ResetColor()

        let tokens = Lexer.run input
        let (remaining, result) = Parser.run tokens

        if debug then
            Console.ForegroundColor <- ConsoleColor.Blue
            Console.WriteLine("\n================DEBUG=================\n")
            Console.WriteLine("Tokens:")
            tokens |> List.iter (fun t -> printf "%A " t.Token)
            printfn ""
            SymbolTable.print ()
            Console.ResetColor()

        match remaining with
        | [] -> Utils.formatResult result
        | { Token = t; Position = p } :: _ ->
            let offset = remaining.Length
            raise (Exception($"Parser error: Unexpected token '{t}' at position {p + 1}"))

    with
    | :? DivideByZeroException -> "Error: Division by zero"
    | ex when ex.Message.StartsWith("Lexer error") -> ex.Message
    | ex when ex.Message.StartsWith("Parser error") -> ex.Message
    | ex -> sprintf "Error: %s" ex.Message
