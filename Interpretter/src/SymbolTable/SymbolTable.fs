namespace SymbolTable

open Types
open SharedTypes

module SymbolTable =
    let mutable symbolTable: SymbolTable = Map.empty

    let set (var: Variable) (value: RealNum) : RealNum =
        symbolTable <- symbolTable.Add(var, value)
        value

    let get (var: Variable) : RealNum =
        match symbolTable.TryFind(var) with
        | Some value -> value
        | None -> raise (System.Exception($"Variable {var} not found"))

    let clear () = symbolTable <- Map.empty

    let print () =
        printfn "\nSymbol Table:"

        if Map.isEmpty symbolTable then
            printfn "  <empty>"
        else
            symbolTable
            |> Map.iter (fun k v ->
                match v with
                | String s -> printfn "  %c = String \"%s\"" k s
                | Float f -> printfn "  %c = Float %.1f" k f
                | Int i -> printfn "  %c = Int %d" k i)

        printfn ""
