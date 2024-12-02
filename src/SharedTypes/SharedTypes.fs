namespace SharedTypes

type Variable = char

type RealNum =
    | Float of float
    | Int of int
    | String of string

type Terminal =
    | Add
    | Sub
    | Mul
    | Div
    | Lpar
    | Rpar
    | Num of RealNum
    | Pow
    | Mod
    | Assign
    | Var of Variable
    | Print
    | VarKeyword
    | If
    | Then
    | Else
    | Eq
    | Ne
    | Gt
    | Ge
    | Lt
    | Le
    | StringLiteral of string

type PositionedToken = { Token: Terminal; Position: int }
