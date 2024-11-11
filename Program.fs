// Simple Interpreter in F#
// Author: R.J. Lapeer
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type RealNum =
    | Float of float
    | Int of int

type Variable = char

type terminal =
    | Add
    | Sub
    | Mul
    | Div
    | IntDiv
    | Lpar
    | Rpar
    | Num of RealNum
    | Pow
    | Mod
    | Assign
    | Var of Variable

type SymbolTable = Map<Variable, RealNum>

let str2lst s = [ for c in s -> c ]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let lexError = System.Exception("Lexer error")
let intVal (c: char) = (int) ((int) c - (int) '0')
let parseError = System.Exception("Parser error")

let mutable symbolTable: SymbolTable = Map.empty

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

let isLetter c = System.Char.IsLetter c

let rec scVar input acc =
    match input with
    | c :: tail when isLetter c || isdigit c -> scVar tail (acc + c)
    | _ -> (input, acc)


let lexer input =
    let rec scan input =
        match input with
        | [] -> []
        | '=' :: tail -> Assign :: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isLetter c ->
            let (remaining, varName) = scVar tail (c)
            Var varName :: scan remaining
        | '/' :: '/' :: tail -> IntDiv :: scan tail
        | '+' :: tail -> Add :: scan tail
        | '-' :: tail -> Sub :: scan tail
        | '*' :: tail -> Mul :: scan tail
        | '/' :: tail -> Div :: scan tail
        | '%' :: tail -> Mod :: scan tail
        | '^' :: tail -> Pow :: scan tail
        | '(' :: tail -> Lpar :: scan tail
        | ')' :: tail -> Rpar :: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c ->
            let (iStr, realNum) = scNum (tail, Int(intVal c), false, 1)
            Num(realNum) :: scan iStr
        | _ -> raise lexError

    scan (str2lst input)

// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt> | <NR>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt> | <empty>
// <P>        ::= <NR> <Popt> | <NR>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <F>        ::= "-" <NR> | "^" <NR> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"

let parser tList =
    let rec E tList =
        match tList with
        | Var v :: Assign :: tail ->
            match E tail with
            | remaining -> remaining
        | _ -> (T >> Eopt) tList

    and Eopt tList =
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList

    and T tList = (P >> Topt) tList

    and Topt tList =
        match tList with
        | Mul :: tail -> (P >> Topt) tail
        | Div :: tail -> (P >> Topt) tail
        | IntDiv :: tail -> (P >> Topt) tail
        | Mod :: tail -> (P >> Topt) tail
        | _ -> tList

    and P tList = (F >> Popt) tList

    and Popt tList =
        match tList with
        | Pow :: tail -> (F >> Popt) tail
        | _ -> tList

    and F tList =
        match tList with
        | Sub :: tail -> NR tail
        | _ -> NR tList

    and NR tList =
        match tList with
        | Num(Int value) :: tail -> tail
        | Num(Float value) :: tail -> tail
        | Lpar :: tail ->
            match E tail with
            | Rpar :: tail -> tail
            | _ -> raise parseError
        | _ -> raise parseError

    E tList

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
        | Int i1, Int i2 ->
            if i1 % i2 = 0 then
                Int(i1 / i2)
            else
                Float(float i1 / float i2)

let intdiv (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Int(int (f1 / f2))
    | Float f, Int i -> Int(int (f / float i))
    | Int i, Float f -> Int(int (float i / f))
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
        | Var v :: Assign :: tail ->
            let (remaining, value) = E tail
            (remaining, setVariable v value)
        | _ -> (T >> Eopt) tList

    and Eopt (tList, value: RealNum) =
        match tList with
        | Add :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, add value tval)
        | Sub :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, sub value tval)
        | _ -> (tList, value)

    and T tList = (P >> Topt) tList

    and Topt (tList, value: RealNum) =
        match tList with
        | Mul :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, mul value tval)
        | Div :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, div value tval)
        | IntDiv :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, intdiv value tval)
        | Mod :: tail ->
            let (tLst, tval) = P tail
            Topt(tLst, modulo value tval)
        | _ -> (tList, value)

    and P tList = (F >> Popt) tList

    and Popt (tList, value: RealNum) =
        match tList with
        | Pow :: tail ->
            let (tLst, tval) = F tail
            Popt(tLst, pow value tval)
        | _ -> (tList, value)

    and F tList =
        match tList with
        | Sub :: tail ->
            let (tLst, tval) = NR tail
            (tLst, neg tval)
        | _ -> NR tList

    and NR tList =
        match tList with
        | Num value :: tail -> (tail, value)
        | Var v :: tail -> (tail, getVariable v)
        | Lpar :: tail ->
            let (tLst, tval) = E tail
            match tLst with
            | Rpar :: tail -> (tail, tval)
            | _ -> raise parseError
        | _ -> raise parseError

    E tList

let rec printTList (lst: list<terminal>) : list<string> =
    match lst with
    | head :: tail ->
        Console.Write("{0} ", head.ToString())
        printTList tail

    | [] ->
        Console.Write("EOL\n")
        []


let evaluate (input: string) =
    try
        let oList = lexer input
        let Out = parseNeval oList
        snd Out
    with
    | :? System.DivideByZeroException ->
        printfn "Divide by zero not allowed"
        Int 0
    | ex ->
        printfn "Error: %s" ex.Message
        Int 0

[<EntryPoint>]
let main argv =
    printfn "Interpreter (type 'exit' to quit)"
    let mutable running = true

    while running do
        printf "> "
        let input = Console.ReadLine()

        if input.ToLower() = "exit" then
            running <- false
        else
            let result = evaluate input
            printfn "%A" result

    0
