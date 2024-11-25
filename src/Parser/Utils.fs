module Utils

open SharedTypes

let formatResult (result: RealNum) : string =
    match result with
    | Float f -> sprintf "%.6g" f
    | Int i -> string i
