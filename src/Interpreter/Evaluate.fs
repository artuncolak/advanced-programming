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
        | [] -> result
        | { Token = t; Position = p } :: _ -> raise (Exception($"Parser error: Unexpected token '{t}' at position {p}"))

    with
    | :? System.DivideByZeroException ->
        printfn "Error: Division by zero"
        Int 0
    | ex when ex.Message.StartsWith("Lexer error") ->
        printfn "%s" ex.Message
        Int 0
    | ex when ex.Message.StartsWith("Parser error") ->
        printfn "%s" ex.Message
        Int 0
    | ex ->
        printfn "Error: %s" ex.Message
        Int 0
