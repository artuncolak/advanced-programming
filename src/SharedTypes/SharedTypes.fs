namespace SharedTypes

type Variable = char

type RealNum =
    | Float of float
    | Int of int

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

type PositionedToken = { Token: Terminal; Position: int }
