module Utils

open SharedTypes

let formatResult (result: RealNum) : string =
    match result with
    | String s -> sprintf "\"%s\"" s
    | Float f -> sprintf "%.15f" f
    | Int i -> string i
