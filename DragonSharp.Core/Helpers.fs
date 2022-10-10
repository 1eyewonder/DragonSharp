module DragonSharp.Core.Helpers

open System
open Microsoft.FSharp.Reflection

let getDuFromString<'a> (s:string) =
    try
        let caseInfos =
            FSharpType.GetUnionCases typeof<'a>
            |> Array.filter (fun case -> case.Name = s)
        match caseInfos with
        |[|case|] -> Some (FSharpValue.MakeUnion(case,[||]) :?> 'a)
        |_ -> None        
    with _ -> None

let tryParseInt (s:string) =
    try 
        Some (int s)
    with :? FormatException -> 
        None