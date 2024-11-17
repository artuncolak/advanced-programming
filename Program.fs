// Simple Interpreter in F#
// Author: R.J. Lapeer
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

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

type PositionedToken = { Token: terminal; Position: int }

let str2lst s = [ for c in s -> c ]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let intVal (c: char) = (int) ((int) c - (int) '0')
//let lexError = System.Exception("Lexer error")
//let parseError = System.Exception("Parser error")

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

let rec scNum (iStr, iVal:RealNum, isDecimal, multiplier) =
    match iStr with 
    | c :: tail when isdigit c ->
        if isDecimal then
            let decimalVal = toFloat iVal + float (intVal c) * multiplier
            scNum (tail, Float decimalVal, isDecimal, multiplier / 10.0)
        else
            scNum (tail, Int (10 * toInt iVal + intVal c), isDecimal, multiplier)
    | '.' :: tail when not isDecimal ->
        scNum (tail, iVal, true, 0.1)
    | _ -> (iStr, iVal) 

let lexer input : PositionedToken list =
    let rec scan (input: char list) (pos: int) =
        match input with
        | [] -> []
        | '+' :: tail -> { Token = Add; Position = pos } :: scan tail  (pos + 1)
        | '-' :: tail -> { Token = Sub; Position = pos } :: scan tail (pos + 1)
        | '*' :: tail -> { Token = Mul; Position = pos } :: scan tail (pos + 1)
        | '/' :: tail -> { Token = Div; Position = pos } :: scan tail (pos + 1)
        | '%' :: tail -> { Token = Mod; Position = pos } :: scan tail (pos + 1)
        | '^' :: tail -> { Token = Pow; Position = pos } :: scan tail (pos + 1)
        | '(' :: tail -> { Token = Lpar; Position = pos } :: scan tail (pos + 1)
        | ')' :: tail -> { Token = Rpar; Position = pos } :: scan tail (pos + 1)
        | c :: tail when isblank c -> scan tail (pos + 1)
        | c :: tail when isdigit c ->
            let (iStr, realNum) = scNum (tail, Int (intVal c), false, 1)
            { Token = Num realNum; Position = pos } :: scan iStr (pos + (List.length input - List.length iStr))
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

let parser ( tList: list<PositionedToken> ) =
    let rec E tList = (T >> Eopt) tList // >> is forward function composition operator: let inline (>>) f g x = g(f x)

    and Eopt tList =
        match tList with
        | { Token = Add; Position = pos } :: tail -> (T >> Eopt) tail
        | { Token = Sub; Position = pos } :: tail -> (T >> Eopt) tail
        | _ -> tList

    and T tList = (P >> Topt) tList

    and Topt tList =
        match tList with
        | { Token = Mul; Position = pos } :: tail -> (P >> Topt) tail
        | { Token = Div; Position = pos } :: tail -> (P >> Topt) tail
        | { Token = Mod; Position = pos } :: tail -> (P >> Topt) tail
        | _ -> tList

    and P tList = (F >> Popt) tList

    and Popt tList =
        match tList with
        | { Token = Pow; Position = pos } :: tail -> (F >> Popt) tail
        | _ -> tList

    and F tList =
        match tList with
        | { Token = Sub; Position = pos } :: tail -> NR tail
        | _ -> NR tList

    and NR tList =
        match tList with
        | { Token = Num _; Position = _ } :: tail -> tail
        | { Token = Lpar; Position = pos } :: tail ->
            match E tail with
            | { Token = Rpar; Position = _ } :: tail -> tail
            | _ -> raise (System.Exception($"Parser error: Missing closing parenthesis at position {pos}"))
        | { Token = token; Position = pos } :: _ ->
            raise (System.Exception($"Parser error: Unexpected '{token}' token at position {pos}"))
        | [] -> raise (System.Exception("Parser error: Unexpected end of input"))

    E tList

let add (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 + f2)
    | Float f, Int i -> Float (f + float i)
    | Int i, Float f -> Float (float i + f)
    | Int i1, Int i2 -> Int (i1 + i2)

let sub (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 - f2)
    | Float f, Int i -> Float (f - float i)
    | Int i, Float f -> Float (float i - f)
    | Int i1, Int i2 -> Int (i1 - i2)

let mul (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 * f2)
    | Float f, Int i -> Float (f * float i)
    | Int i, Float f -> Float (float i * f)
    | Int i1, Int i2 -> Int (i1 * i2)

let div (x: RealNum) (y: RealNum) : RealNum =
    if toIntOrFloat y = 0 || toIntOrFloat y = 0.0 then
        raise (DivideByZeroException "Divide by zero not allowed")
    else
        match x, y with
        | Float f1, Float f2 -> Float(f1 / f2)
        | Float f, Int i -> Float (f / float i)
        | Int i, Float f -> Float (float i / f)
        | Int i1, Int i2 -> Int (i1 / i2)

let modulo (x: RealNum) (y: RealNum) : RealNum =
    let baseResult =
        match x, y with
        | Float f1, Float f2 -> Float(f1 % f2)
        | Float f, Int i -> Float (f % float i)
        | Int i, Float f -> Float (float i % f)
        | Int i1, Int i2 -> Int (i1 % i2)
    if toIntOrFloat x % toIntOrFloat y < 0 then
        add baseResult y
    else
        baseResult

let pow (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float (float f1 ** float f2)
    | Float f, Int i -> Float (f ** float i)
    | Int i, Float f -> Float (float i ** f)
    // negative integer powers return a Float
    | Int i1, Int i2 -> 
        if (i1 >= 0) && (i2 >= 0) then
            Int (int (float i1 ** float i2))
        else
            Float (float i1 ** float i2)

let neg (x: RealNum): RealNum =
    match x with
    | Float f -> Float -f
    | Int i -> Int -i

let parseNeval tList =
    let rec E tList = (T >> Eopt) tList

    and Eopt (tList, value:RealNum) =
        match tList with
        | { Token = Add; Position = pos } :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, add value tval)
        | { Token = Sub; Position = pos } :: tail ->
            let (tLst, tval) = T tail
            Eopt(tLst, sub value tval)
        | _ -> (tList, value)

    and T tList = (P >> Topt) tList

    and Topt (tList, value:RealNum) =
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

    and Popt (tList, value:RealNum) =
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


[<EntryPoint>]
let main argv =
    try
        // Console.WriteLine("Simple Interpreter")
        let input: string = getInputString ()
        let oList = lexer input
        // let sList = printTList oList
        // let pList = printTList (parser oList)
        // Console.WriteLine(pList)
        // Console.WriteLine(sList)
        let Out =
            parseNeval oList
//        Console.WriteLine(System.Math.Round(snd Out, 3))                                                                                                                                          
        Console.WriteLine(snd Out)
    with
    | :? System.DivideByZeroException -> Console.WriteLine("Divide by zero not allowed")
    | ex when ex.Message.StartsWith("Lexer error") ->
        Console.WriteLine($"{ex.Message}")
    | ex when ex.Message.StartsWith("Parser error") ->
        Console.WriteLine($"{ex.Message}")
    |  ex ->
        Console.WriteLine($"{ex.Message}")
        reraise ()

    0
