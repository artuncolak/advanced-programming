module Utils

let str2lst (s: string) = s.ToCharArray() |> Array.toList

let isblank (c: char) = System.Char.IsWhiteSpace(c)

let isdigit (c: char) = System.Char.IsDigit(c)

let intVal (c: char) = int c - int '0'
