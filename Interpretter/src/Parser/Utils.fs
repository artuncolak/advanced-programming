module Utils

open SharedTypes

let formatFloat (f: float) : string =
    sprintf "%.3f" f

let formatResult (result: RealNum) : string =
    match result with
    | String s -> sprintf "\"%s\"" s
    | Float f -> formatFloat f
    | Int i -> string i
