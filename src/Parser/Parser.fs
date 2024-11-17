namespace Parser

open SharedTypes
open SymbolTable
open Errors

module Parser =
    let run (tList: PositionedToken list) =
        let rec E tList =
            match tList with
            | { Token = Print; Position = _ } :: { Token = Lpar; Position = _ } :: tail ->
                let (remaining, value) = E tail

                match remaining with
                | { Token = Rpar; Position = _ } :: rest ->
                    printfn "%s" (Utils.formatResult value)
                    (rest, value)
                | _ -> raise parseError
            | { Token = VarKeyword; Position = _ } :: { Token = Var v; Position = _ } :: { Token = Assign; Position = _ } :: tail ->
                let (remaining, value) = E tail
                (remaining, SymbolTable.set v value)
            | _ -> (T >> Eopt) tList

        and Eopt (tList, value: RealNum) =
            match tList with
            | { Token = Add; Position = _ } :: tail ->
                let (tLst, tval) = T tail
                Eopt(tLst, Operations.add value tval)
            | { Token = Sub; Position = _ } :: tail ->
                let (tLst, tval) = T tail
                Eopt(tLst, Operations.sub value tval)
            | _ -> (tList, value)

        and T tList = (P >> Topt) tList

        and Topt (tList, value: RealNum) =
            match tList with
            | { Token = Mul; Position = _ } :: tail ->
                let (tLst, tval) = P tail
                Topt(tLst, Operations.mul value tval)
            | { Token = Div; Position = _ } :: tail ->
                let (tLst, tval) = P tail
                Topt(tLst, Operations.div value tval)
            | { Token = Mod; Position = _ } :: tail ->
                let (tLst, tval) = P tail
                Topt(tLst, Operations.modulo value tval)
            | _ -> (tList, value)

        and P tList = (F >> Popt) tList

        and Popt (tList, value: RealNum) =
            match tList with
            | { Token = Pow; Position = _ } :: tail ->
                let (tLst, tval) = F tail
                Popt(tLst, Operations.pow value tval)
            | _ -> (tList, value)

        and F tList =
            match tList with
            | { Token = Sub; Position = _ } :: tail ->
                let (tLst, tval) = NR tail
                (tLst, Operations.neg tval)
            | _ -> NR tList

        and NR tList =
            match tList with
            | { Token = Num value; Position = _ } :: tail -> (tail, value)
            | { Token = Var v; Position = _ } :: tail -> (tail, SymbolTable.get v)
            | { Token = Lpar; Position = pos } :: tail ->
                let (remaining, value) = E tail

                match remaining with
                | { Token = Rpar; Position = _ } :: rest -> (rest, value)
                | _ -> raise (System.Exception($"Parser error: Missing closing parenthesis at position {pos}"))
            | { Token = token; Position = pos } :: _ ->
                raise (System.Exception($"Parser error: Unexpected token '{token}' at position {pos}"))
            | [] -> raise (System.Exception("Parser error: Unexpected end of input"))

        E tList
