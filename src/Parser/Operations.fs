module Operations

open System
open SharedTypes

let add (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 + f2)
    | Float f, Int i -> Float(f + float i)
    | Int i, Float f -> Float(float i + f)
    | Int i1, Int i2 -> Int(i1 + i2)

let sub (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 - f2)
    | Float f, Int i -> Float(f - float i)
    | Int i, Float f -> Float(float i - f)
    | Int i1, Int i2 -> Int(i1 - i2)

let mul (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 * f2)
    | Float f, Int i -> Float(f * float i)
    | Int i, Float f -> Float(float i * f)
    | Int i1, Int i2 -> Int(i1 * i2)

let div (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | _, Int 0 -> raise (DivideByZeroException "Division by zero")
    | _, Float f when f = 0.0 -> raise (DivideByZeroException "Division by zero")
    | Float f1, Float f2 -> Float(f1 / f2)
    | Float f, Int i -> Float(f / float i)
    | Int i, Float f -> Float(float i / f)
    | Int i1, Int i2 -> Int(i1 / i2)

let modulo (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | _, Int 0 -> raise (DivideByZeroException "Modulo by zero")
    | _, Float f when f = 0.0 -> raise (DivideByZeroException "Modulo by zero")
    | Float f1, Float f2 -> Float(f1 % f2)
    | Float f, Int i -> Float(f % float i)
    | Int i, Float f -> Float(float i % f)
    | Int i1, Int i2 -> Int(i1 % i2)

let pow (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | Float f1, Float f2 -> Float(f1 ** f2)
    | Float f, Int i -> Float(f ** float i)
    | Int i, Float f -> Float(float i ** f)
    | Int i1, Int i2 when i2 >= 0 -> Int(pown i1 i2)
    | Int i1, Int i2 -> Float(float i1 ** float i2)

let neg (x: RealNum) : RealNum =
    match x with
    | Float f -> Float(-f)
    | Int i -> Int(-i)
