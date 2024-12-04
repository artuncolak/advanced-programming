namespace Parser

open SharedTypes
open SymbolTable

module Parser =
    let run (tList: PositionedToken list) =
        let rec E tList =
            match tList with
            | { Token = If; Position = _ } :: tail ->
                let (remaining1, condition) = E tail

                match remaining1 with
                | { Token = Then; Position = _ } :: [] ->
                    raise (System.Exception("Parser error: Expected statement after 'then'"))
                | { Token = Then; Position = _ } :: tail2 ->
                    match condition with
                    | Int 0
                    | Float 0.0 ->
                        // Condition is false, skip the then part and look for else
                        System.Console.WriteLine("Condition is false")
                        let rec skipThenPart tokens depth =
                            match tokens with
                            | { Token = If; Position = _ } :: rest -> skipThenPart rest (depth + 1)
                            | { Token = Else; Position = _ } :: [] ->
                                raise (System.Exception("Parser error: Expected statement after 'else'"))
                            | { Token = Else; Position = _ } :: rest when depth = 0 ->
                                let (remaining2, result) = E rest
                                (remaining2, result)
                            | _ :: rest -> skipThenPart rest depth
                            | [] -> ([], Int 0)

                        skipThenPart tail2 0
                    | _ ->
                        let (remaining2, result) = E tail2

                        let rec skipElsePart tokens depth =
                            match tokens with
                            | { Token = If; Position = _ } :: rest -> skipElsePart rest (depth + 1)
                            | { Token = Else; Position = _ } :: rest when depth = 0 ->
                                let rec skipToNextToken tokens currentDepth =
                                    match tokens with
                                    | { Token = If; Position = _ } :: rest -> skipToNextToken rest (currentDepth + 1)
                                    | { Token = Else; Position = _ } :: rest when currentDepth = 0 -> rest
                                    | _ :: rest -> skipToNextToken rest currentDepth
                                    | [] -> []
                                let remainingTokens = skipToNextToken rest 0
                                (remainingTokens, result) 
                            | _ :: rest -> skipElsePart rest depth
                            | [] -> (tokens, result)

                        skipElsePart remaining2 0
                | _ -> raise (System.Exception("Parser error: Expected 'then' after if condition"))
            | { Token = Print; Position = _ } :: { Token = Lpar; Position = _ } :: tail ->
                let (remaining, value) = E tail

                match remaining with
                | { Token = Rpar; Position = _ } :: rest ->
                    (rest, value)
                | _ -> raise (System.Exception("Parser error: Expected closing parenthesis ')' after print expression"))
            | { Token = VarKeyword; Position = _ } :: { Token = Var v; Position = _ } :: tail ->
                match tail with
                | { Token = Assign; Position = _ } :: rest ->
                    let (remaining, value) = E rest
                    (remaining, SymbolTable.set v value)
                | _ -> raise (System.Exception("Parser error: Expected '=' after variable declaration"))
            | _ -> (T >> Eopt) tList

        and Eopt (tList, value: RealNum) =
            match tList with
            | { Token = Eq; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 = i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 = f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i = f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f = float i then Int 1 else Int 0)
            | { Token = Ne; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 <> i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 <> f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i <> f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f <> float i then Int 1 else Int 0)
            | { Token = Gt; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 > i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 > f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i > f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f > float i then Int 1 else Int 0)
            | { Token = Ge; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 >= i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 >= f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i >= f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f >= float i then Int 1 else Int 0)
            | { Token = Lt; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 < i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 < f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i < f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f < float i then Int 1 else Int 0)
            | { Token = Le; Position = _ } :: tail ->
                let (tLst, tval) = T tail

                match (value, tval) with
                | (Int i1, Int i2) -> (tLst, if i1 <= i2 then Int 1 else Int 0)
                | (Float f1, Float f2) -> (tLst, if f1 <= f2 then Int 1 else Int 0)
                | (Int i, Float f) -> (tLst, if float i <= f then Int 1 else Int 0)
                | (Float f, Int i) -> (tLst, if f <= float i then Int 1 else Int 0)
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
                | _ ->
                let offset = tail.Length
                raise (System.Exception($"Parser error: Missing closing parenthesis"))
            | { Token = token; Position = pos } :: _ ->
                raise (System.Exception($"Parser error: Unexpected token '{token}' at position {pos + 1}"))
            | [] -> raise (System.Exception("Parser error: Unexpected end of input"))

        E tList
