namespace Lexer

open SharedTypes
open Utils

module Lexer =
    let rec scNum (iStr: char list, iVal: RealNum, isDecimal: bool, multiplier: float) =
        match iStr with
        | c :: tail when isdigit c ->
            if isDecimal then
                let decimalVal =
                    match iVal with
                    | Float f -> f
                    | Int i -> float i

                let newVal = decimalVal + float (intVal c) * multiplier
                scNum (tail, Float newVal, isDecimal, multiplier / 10.0)
            else
                let newVal =
                    match iVal with
                    | Float f -> int f
                    | Int i -> i

                scNum (tail, Int(10 * newVal + intVal c), isDecimal, multiplier)
        | '.' :: tail when not isDecimal -> scNum (tail, iVal, true, 0.1)
        | _ -> (iStr, iVal)

    let run (input: string) : PositionedToken list =
        let rec scan (input: char list) (pos: int) =
            match input with
            | [] -> []
            | 'i' :: 'f' :: tail when isblank (List.head tail) ->
                { Token = If; Position = pos } :: scan (List.skipWhile isblank tail) (pos + 2)
            | 't' :: 'h' :: 'e' :: 'n' :: tail when isblank (List.head tail) ->
                { Token = Then; Position = pos } :: scan (List.skipWhile isblank tail) (pos + 4)
            | 'e' :: 'l' :: 's' :: 'e' :: tail when isblank (List.head tail) ->
                { Token = Else; Position = pos } :: scan (List.skipWhile isblank tail) (pos + 4)
            | '=' :: '=' :: tail -> { Token = Eq; Position = pos } :: scan tail (pos + 2)
            | '!' :: '=' :: tail -> { Token = Ne; Position = pos } :: scan tail (pos + 2)
            | '>' :: '=' :: tail -> { Token = Ge; Position = pos } :: scan tail (pos + 2)
            | '<' :: '=' :: tail -> { Token = Le; Position = pos } :: scan tail (pos + 2)
            | '>' :: tail -> { Token = Gt; Position = pos } :: scan tail (pos + 1)
            | '<' :: tail -> { Token = Lt; Position = pos } :: scan tail (pos + 1)
            | 'v' :: 'a' :: 'r' :: tail when isblank (List.head tail) ->
                { Token = VarKeyword; Position = pos }
                :: scan (List.skipWhile isblank tail) (pos + 3)
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
                let (remainingStr, num) = scNum (tail, Int(intVal c), false, 1.0)

                { Token = Num num; Position = pos }
                :: scan remainingStr (pos + (input.Length - remainingStr.Length))
            | c :: _ -> raise (System.Exception($"Lexer error: Unrecognized character '{c}' at position {pos}"))

        scan (str2lst input) 0
