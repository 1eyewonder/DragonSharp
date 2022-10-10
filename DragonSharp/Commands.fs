namespace DragonSharp.Commands

open System
open DragonSharp.Core
open DragonSharp.Core.Helpers
open FSharpPlus

type Command = {
    Name: string
    Description: string
    ShortName: string
}

module AppCommands =
    
    let exitCommand = {
        Name = "exit"
        Description = "Exit the application"
        ShortName = "e"
    }
    
    let helpCommand = {
        Name = "help"
        Description = "Show help options"
        ShortName = "h"
    }
    
    let rollCommand = {
        Name = "roll"
        Description = "Rolls a dice"
        ShortName = "r"
    }
    
    let getAllCommands = [
        exitCommand
        helpCommand
        rollCommand
    ]
    
module CommandActions =
    
    let private nl = Environment.NewLine
    
    let printAvailableCommands () =
        AppCommands.getAllCommands
            |> Seq.map (fun command -> sprintf "%A%s" command nl)
            |> Seq.iter Console.WriteLine
            
    let rollDice qty die =
        let diceQty =
                match tryParseInt qty with
                | Some x -> x
                | None ->
                    failwithf "Invalid dice quantity: %s. Please enter a valid number" qty
                
        let die =
            let potentialDie = 
                die
                |> String.toUpper
                |> getDuFromString
                
            match potentialDie with
            | Some die -> die
            | None ->
                failwithf "Invalid die type provided. Valid die types are %A. Any casing is valid" Dice.getDiceOptions
        
        let total = Dice.Roll diceQty die
        Console.WriteLine (sprintf "You total roll is %A" total)