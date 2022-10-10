open System
open FSharpPlus
open DragonSharp.Commands

[<EntryPoint>]
let main argv =
    
    let debug =
        match argv with
        | [|"-debug"|] -> true
        | _ -> false
        
    let rec loop () =
        try 
            Console.WriteLine "Enter command:"
            let command =
                Console.ReadLine()
                |> String.split [" "]
                |> Seq.toArray
            
            let exit = AppCommands.exitCommand
            let roll = AppCommands.rollCommand
            let help = AppCommands.helpCommand
            
            match command with
            // help
            | [|x|] when exit.Name = x || exit.ShortName = x -> ()
            
            // exit
            | [|x|] when help.Name = x || help.ShortName = x ->
                CommandActions.printAvailableCommands ()
                loop ()
                
            // roll
            | [|x|] when roll.Name = x || roll.ShortName = x ->
                CommandActions.rollDice "1" "d20"
                loop () 
            | [|x; die|] when roll.Name = x || roll.ShortName = x ->
                CommandActions.rollDice "1" die
                loop ()    
            | [|x; qty; die|] when roll.Name = x || roll.ShortName = x -> 
                CommandActions.rollDice qty die
                loop ()
                
            // default
            | _ -> loop()
            
        with e ->
            Console.WriteLine e.Message
            if debug then Console.WriteLine e.StackTrace
            loop()
            
    loop()       
    0