module Evaluate

open System
open SharedTypes
open Lexer
open SymbolTable
open Parser

let resultToString x =
    match x with
    | ValidOutput (value) -> string value
    | DivideByZeroError (value) -> string value
    | LexerError (value) -> string value
    | ParserError (value) -> string value
    | ArgumentError (value) -> string value

let run (input: string, debug: bool) : ExpressionResult =
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
        | [] -> ValidOutput (Utils.formatResult result)
        | { Token = t; Position = p } :: _ ->
            raise (Exception($"Parser error: Unexpected token '{t}' at position {p + 1}"))

    with
    | :? DivideByZeroException -> DivideByZeroError "Cannot divide by 0"
    | ex when ex.Message.StartsWith("Lexer error") -> LexerError ex.Message
    | ex when ex.Message.StartsWith("Parser error") -> ParserError ex.Message
    | ex when ex.Message.StartsWith("Argument error") -> ArgumentError ex.Message
