// Simple Interpreter in F#
// Author: R.J. Lapeer
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type Variable = char

type RealNum =
    | Float of float
    | Int of int

type terminal =
    | Add
    | Sub
    | Mul
    | Div
    | Lpar
    | Rpar
    | Num of RealNum
    | Pow
    | Mod
    | Assign
    | Var of Variable
    | Print

type PositionedToken = { Token: terminal; Position: int }

type SymbolTable = Map<Variable, RealNum>

let mutable symbolTable: SymbolTable = Map.empty
let mutable debugMode = false

let str2lst s = [ for c in s -> c ]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let intVal (c: char) = (int) ((int) c - (int) '0')

let parseError = System.Exception("Parser error")

let formatResult (result: RealNum) : string =
    match result with
    | Float f -> sprintf "%.6g" f
    | Int i -> string i

let setVariable (var: Variable) (value: RealNum) : RealNum =
    symbolTable <- symbolTable.Add(var, value)
    value

let getVariable (var: Variable) : RealNum =
    match symbolTable.TryFind(var) with
    | Some value -> value
    | None -> raise (System.Exception($"Variable {var} not found"))

let toFloat (r: RealNum) =
    match r with
    | Float f -> f
    | Int i -> float i

let toInt (r: RealNum) =
    match r with
    | Float f -> int f
    | Int i -> i

let toIntOrFloat (r: RealNum) =
    match r with
    | Float f -> f
    | Int i -> i

let rec scNum (iStr, iVal: RealNum, isDecimal, multiplier) =
    match iStr with
    | c :: tail when isdigit c ->
        if isDecimal then
            let decimalVal = toFloat iVal + float (intVal c) * multiplier
            scNum (tail, Float decimalVal, isDecimal, multiplier / 10.0)
        else
            scNum (tail, Int(10 * toInt iVal + intVal c), isDecimal, multiplier)
    | '.' :: tail when not isDecimal -> scNum (tail, iVal, true, 0.1)
    | _ -> (iStr, iVal)

let lexer input : PositionedToken list =
    let rec scan (input: char list) (pos: int) =
        match input with
        | [] -> []
        | '+' :: tail -> { Token = Add; Position = pos } :: scan tail (pos + 1)
        | '-' :: tail -> { Token = Sub; Position = pos } :: scan tail (pos + 1)
        | '*' :: tail -> { Token = Mul; Position = pos } :: scan tail (pos + 1)
        | '/' :: tail -> { Token = Div; Position = pos } :: scan tail (pos + 1)
        | '%' :: tail -> { Token = Mod; Position = pos } :: scan tail (pos + 1)
        | '^' :: tail -> { Token = Pow; Position = pos } :: scan tail (pos + 1)
        | '(' :: tail -> { Token = Lpar; Position = pos } :: scan tail (pos + 1)
        | ')' :: tail -> { Token = Rpar; Position = pos } :: scan tail (pos + 1)
        | '=' :: tail -> { Token = Assign; Position = pos } :: scan tail (pos + 1)
        | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tail -> { Token = Print; Position = pos } :: scan tail (pos + 5)
        | c :: tail when System.Char.IsLetter(c) -> { Token = Var c; Position = pos } :: scan tail (pos + 1)
        | c :: tail when isblank c -> scan tail (pos + 1)
        | c :: tail when isdigit c ->
            let (iStr, realNum) = scNum (tail, Int(intVal c), false, 1)

            { Token = Num realNum; Position = pos }
            :: scan iStr (pos + (List.length input - List.length iStr))
        | c :: _ -> raise (System.Exception($"Lexer error: Unrecognized character '{c}'"))

    scan (str2lst input) 0

let getInputString () : string =
    // Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt> | <NR>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt> | <empty>
// <P>        ::= <NR> <Popt> | <NR>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <F>        ::= "-" <NR> | "^" <NR> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"

let add (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 + f2)
    | Float f, Int i -> Float(f + float i)
    | Int i, Float f -> Float(float i + f)
    | Int i1, Int i2 -> Int(i1 + i2)

let sub (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 - f2)
    | Float f, Int i -> Float(f - float i)
    | Int i, Float f -> Float(float i - f)
    | Int i1, Int i2 -> Int(i1 - i2)

let mul (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 * f2)
    | Float f, Int i -> Float(f * float i)
    | Int i, Float f -> Float(float i * f)
    | Int i1, Int i2 -> Int(i1 * i2)

let div (x: RealNum) (y: RealNum) : RealNum =
    if toIntOrFloat y = 0 || toIntOrFloat y = 0.0 then
        raise (DivideByZeroException "Divide by zero not allowed")
    else
        match x, y with
        | Float f1, Float f2 -> Float(f1 / f2)
        | Float f, Int i -> Float(f / float i)
        | Int i, Float f -> Float(float i / f)
        | Int i1, Int i2 -> Int(i1 / i2)

let modulo (x: RealNum) (y: RealNum) : RealNum =
    let baseResult =
        match x, y with
        | Float f1, Float f2 -> Float(f1 % f2)
        | Float f, Int i -> Float(f % float i)
        | Int i, Float f -> Float(float i % f)
        | Int i1, Int i2 -> Int(i1 % i2)

    if toIntOrFloat x % toIntOrFloat y < 0 then
        add baseResult y
    else
        baseResult

let pow (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(float f1 ** float f2)
    | Float f, Int i -> Float(f ** float i)
    | Int i, Float f -> Float(float i ** f)
    // negative integer powers return a Float
    | Int i1, Int i2 ->
        if (i1 >= 0) && (i2 >= 0) then
            Int(int (float i1 ** float i2))
        else
            Float(float i1 ** float i2)

let neg (x: RealNum) : RealNum =
    match x with
    | Float f -> Float -f
    | Int i -> Int -i

let parseNeval tList =
    let rec E tList =
        match tList with
        | { Token = Print; Position = _ } :: { Token = Lpar; Position = _ } :: tail ->
            let (remaining, value) = E tail

            match remaining with
            | { Token = Rpar; Position = _ } :: rest ->
                printfn "%s" (formatResult value)
                (rest, value)
            | _ -> raise parseError
        | { Token = Var v; Position = _ } :: { Token = Assign; Position = _ } :: tail ->
            let (remaining, value) = E tail
            (remaining, setVariable v value)
        | _ -> (T >> Eopt) tList

    and Eopt (tList, value: RealNum) =
        match tList with
        | { Token = Add; Position = pos } :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, add value tval)
        | { Token = Sub; Position = pos } :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, sub value tval)
        | _ -> (tList, value)

    and T tList = (P >> Topt) tList

    and Topt (tList, value: RealNum) =
        match tList with
        | { Token = Mul; Position = pos } :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, mul value tval)
        | { Token = Div; Position = pos } :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, div value tval)
        | { Token = Mod; Position = pos } :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, modulo value tval)
        | _ -> (tList, value)

    and P tList = (F >> Popt) tList

    and Popt (tList, value: RealNum) =
        match tList with
        | { Token = Pow; Position = pos } :: tail ->
            let (tLst, tval) = F tail
            Popt(tLst, pow value tval)
        | _ -> (tList, value)

    and F tList =
        match tList with
        | { Token = Sub; Position = pos } :: tail ->
            let (tLst, tval) = NR tail
            (tLst, neg tval)
        | _ -> NR tList

    and NR tList =
        match tList with
        | { Token = Num value; Position = _ } :: tail -> (tail, value)
        | { Token = Var v; Position = _ } :: tail -> (tail, getVariable v)
        | { Token = Lpar; Position = pos } :: tail ->
            let t = E tail

            match fst t with
            | { Token = Rpar; Position = _ } :: tail -> tail, snd t
            | _ -> raise (System.Exception($"Parser error: Missing closing parenthesis at position {pos}"))
        | { Token = token; Position = pos } :: _ ->
            raise (System.Exception($"Parser error: Unexpected '{token}' token at position {pos}"))
        | [] -> raise (System.Exception("Parser error: Unexpected end of input"))

    E tList

let rec printTList (lst: list<terminal>) : list<string> =
    match lst with
    | head :: tail ->
        Console.Write("{0} ", head.ToString())
        printTList tail

    | [] ->
        Console.Write("EOL\n")
        []

let printSymbolTable () =
    printfn "\nSymbol Table:"

    if Map.isEmpty symbolTable then
        printfn "  <empty>"
    else
        symbolTable |> Map.iter (fun k v -> printfn "  %c = %s" k (formatResult v))

    printfn ""

let evaluate (input: string) =
    try
        if debugMode then
            Console.ForegroundColor <- ConsoleColor.Green
            Console.WriteLine("\n================OUTPUT================\n")
            Console.ResetColor()

        let oList = lexer input
        let Out = parseNeval oList

        if debugMode then
            Console.ForegroundColor <- ConsoleColor.Blue
            Console.WriteLine("\n================DEBUG=================\n")
            Console.WriteLine("Tokens:")
            let _ = printTList (List.map (fun pt -> pt.Token) oList)
            printSymbolTable ()
            Console.ResetColor()
    with
    | :? System.DivideByZeroException -> printfn "Divide by zero not allowed"
    | ex when ex.Message.StartsWith("Lexer error") -> printfn "%s" ex.Message
    | ex when ex.Message.StartsWith("Parser error") -> printfn "%s" ex.Message
    | ex -> printfn "Error: %s" ex.Message

let readAndEvaluateFile (filePath: string) =
    try
        let lines = System.IO.File.ReadAllLines(filePath)

        for line in lines do
            if not (String.IsNullOrWhiteSpace(line)) then
                evaluate line
    with
    | :? System.IO.FileNotFoundException -> printfn "Error: Source file not found at path: %s" filePath
    | ex -> printfn "Error reading/evaluating file: %s" ex.Message

let repl () =
    printfn "Interpreter (type 'exit' to quit)"
    let mutable running = true

    while running do
        printf "> "
        let input = Console.ReadLine()

        if input.ToLower() = "exit" then
            running <- false
        else
            evaluate input

[<EntryPoint>]
let main argv =
    let hasDebug = Array.contains "--debug" argv
    let argsWithoutDebug = Array.filter ((<>) "--debug") argv

    debugMode <- hasDebug

    match argsWithoutDebug with
    | [| "--repl" |] ->
        repl ()
        0
    | [| "--input"; filePath |] ->
        readAndEvaluateFile filePath
        0
    | _ ->
        printfn "Usage: Program [mode] [arguments]"
        printfn "Modes:"
        printfn "  --repl              Start interactive REPL mode"
        printfn "  --input <filepath>  Execute script from file"
        printfn "Options:"
        printfn "  --debug             Enable debug output"
        1
