module Operations

open System
open SharedTypes

let add (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | String s1, String s2 -> String(s1 + s2)
    | String s, Int i -> String(s + string i)
    | Int i, String s -> String(string i + s)
    | String s, Float f -> String(s + string (Utils.formatFloat f))
    | Float f, String s -> String(string (Utils.formatFloat f) + s)
    | Float f1, Float f2 -> Float(f1 + f2)
    | Float f, Int i -> Float(f + float i)
    | Int i, Float f -> Float(float i + f)
    | Int i1, Int i2 -> Int(i1 + i2)

let sub (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | (_, String _) -> raise (Exception("Argument error: Invalid string operation"))
    | (String _, _) -> raise (Exception("Argument error: Invalid string operation"))
    | Float f1, Float f2 -> Float(f1 - f2)
    | Float f, Int i -> Float(f - float i)
    | Int i, Float f -> Float(float i - f)
    | Int i1, Int i2 -> Int(i1 - i2)

let mul (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | (_, String _) -> raise (Exception("Argument error: Invalid string operation"))
    | (String _, _) -> raise (Exception("Argument error: Invalid string operation"))
    | Float f1, Float f2 -> Float(f1 * f2)
    | Float f, Int i -> Float(f * float i)
    | Int i, Float f -> Float(float i * f)
    | Int i1, Int i2 -> Int(i1 * i2)

let div (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | (_, String _) -> raise (Exception("Argument error: Invalid string operation"))
    | (String _, _) -> raise (Exception("Argument error: Invalid string operation"))
    | _, Int 0 -> raise (DivideByZeroException "Division by zero")
    | _, Float f when f = 0.0 -> raise (DivideByZeroException "Division by zero")
    | Float f1, Float f2 -> Float(f1 / f2)
    | Float f, Int i -> Float(f / float i)
    | Int i, Float f -> Float(float i / f)
    | Int i1, Int i2 -> Int(i1 / i2)

let modulo (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | (_, String _) -> raise (Exception("Argument error: Invalid string operation"))
    | (String _, _) -> raise (Exception("Argument error: Invalid string operation"))
    | _, Int 0 -> raise (DivideByZeroException "Modulo by zero")
    | _, Float f when f = 0.0 -> raise (DivideByZeroException "Modulo by zero")
    | Float f1, Float f2 ->
      if Float(f1 % f2) < Float(0) then
        Float(f1 % f2 + f2)
      else
        Float(f1 % f2)
    | Float f, Int i ->
      if Float(f % float i) < Float(0) then
        Float(f % float i + float i)
      else
        Float(f % float i)
    | Int i, Float f ->
      if Float(float i % f) < Float(0) then
        Float(f % float i + float i)
      else
        Float(float i % f)
    | Int i1, Int i2 ->
      if Int(i1 % i2) < Int(0) then
        Int(i1 % i2 + i2)
      else
        Int(i1 % i2)

let pow (x: RealNum) (y: RealNum) : RealNum =
    match x, y with
    | (_, String _) -> raise (Exception("Argument error: Invalid string operation"))
    | (String _, _) -> raise (Exception("Argument error: Invalid string operation"))
    | Float f1, Float f2 -> Float(f1 ** f2)
    | Float f, Int i -> Float(f ** float i)
    | Int i, Float f -> Float(float i ** f)
    | Int i1, Int i2 when i2 >= 0 -> Int(pown i1 i2)
    | Int i1, Int i2 -> Float(float i1 ** float i2)

let neg (x: RealNum) : RealNum =
    match x with
    | (String _) -> raise (Exception("Argument error: Invalid string operation"))
    | Float f -> Float(-f)
    | Int i -> Int(-i)

let log (x: RealNum) : RealNum =
    match x with
    | Int 0 -> raise (Exception("Argument error: Log of 0"))
    | Float 0.0 -> raise (Exception("Argument error: Log of 0"))
    | Float f -> Float(Math.Log(f))
    | Int i -> Float(Math.Log(i))
