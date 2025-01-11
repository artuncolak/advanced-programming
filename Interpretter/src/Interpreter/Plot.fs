module Plot

// input is in form <y expr> = <x expr>
// start with y = x + 5 in range -10 to 10 with step 1

let run (debug: bool, input: string, min: float, max: float, primaryStep: float, splineStep: float) =
    let rec step inp minimum primStep maximum curr debugParam =
        if curr <= maximum then
            ignore (Evaluate.run(sprintf "var x = %s" (string(minimum)), debugParam))
            ignore (Evaluate.resultToString (Evaluate.run (sprintf "var %s" inp, debugParam)))
            let result = Evaluate.resultToString (Evaluate.run ("y", debugParam))
            step inp minimum primStep maximum curr debugParam
            printfn "%s" (result)
    step input min primaryStep max min debug
