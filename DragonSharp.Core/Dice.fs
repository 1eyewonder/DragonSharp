namespace DragonSharp.Core

open System
open Microsoft.FSharp.Reflection

type Die =
    D4 | D6 | D8 | D10 | D12 | D20
    member this.Sides =
        match this with
        | D4 -> 4
        | D6 -> 6
        | D8 -> 8
        | D10 -> 10
        | D12 -> 12
        | D20 -> 20

module Dice =
    let getDiceOptions =
        FSharpType.GetUnionCases typeof<Die>
        |> Array.map (fun case -> FSharpValue.MakeUnion(case,[||]) :?> Die)
    
    let rec Roll (count:int) (die:Die)  =
        if count = 0 then 0
        else 
            printfn $"Rolling a %A{die}..."
            let result = Random().Next(1, die.Sides + 1)
            printfn $"Roll result is %A{result}"
            Roll(count - 1)(die) + result        