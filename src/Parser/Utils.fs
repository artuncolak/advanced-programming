module Utils

open SharedTypes

let formatResult (result: RealNum) : string =
    match result with
    | String s -> sprintf "\"%s\"" s
    | Float f -> sprintf "%.6g" f
    | Int i -> string i
