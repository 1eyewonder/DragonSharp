namespace DragonSharp.Core.Character

[<Measure>] type xp
    
module Currency =
    
    type CurrencyType =
        | Platinum
        | Gold
        | Electrum
        | Silver
        | Copper
        
    [<Measure>]
    type pp =
        static member create(value: int) = LanguagePrimitives.Int32WithMeasure<pp> value
        static member toGp(value: int<pp>) = gp.create(int value * 10)
        static member toEp(value: int<pp>) = ep.create(int value * 20)
        static member toSp(value: int<pp>) = sp.create(int value * 100)
        static member toCp(value: int<pp>) = cp.create(int value * 1000)
    and       
        [<Measure>] gp =
        static member create(value: int) = LanguagePrimitives.Int32WithMeasure<gp> value
        static member toPp(value: int<gp>) = pp.create(int value / 10), gp.create(int value % 10)
        static member toEp(value: int<gp>) = ep.create(int value * 2)
        static member toSp(value: int<gp>) = sp.create(int value * 10)
        static member toCp(value: int<gp>) = cp.create(int value * 100)
    and
        [<Measure>] ep =
        static member create(value: int) = LanguagePrimitives.Int32WithMeasure<ep> value
        static member toPp(value: int<ep>) = pp.create(int value / 20), ep.create(int value % 20)
        static member toGp(value: int<ep>) = gp.create(int value / 2), ep.create(int value % 2)
        static member toSp(value: int<ep>) = sp.create(int value * 5)
        static member toCp(value: int<ep>) = cp.create(int value * 50)
    and
        [<Measure>] sp =
        static member create(value: int) = LanguagePrimitives.Int32WithMeasure<sp> value
        static member toPp(value: int<sp>) = pp.create(int value / 100), sp.create(int value % 100)
        static member toGp(value: int<sp>) = gp.create(int value / 10), sp.create(int value % 10)
        static member toEp(value: int<sp>) = ep.create(int value / 5), sp.create(int value % 5) 
        static member toCp(value: int<sp>) = cp.create(int value * 10)
    and
        [<Measure>] cp =
        static member create(value: int) = LanguagePrimitives.Int32WithMeasure<cp> value
        static member toPp(value: int<cp>) = pp.create(int value / 1000), cp.create(int value % 1000)
        static member toGp(value: int<cp>) = gp.create(int value / 100), cp.create(int value % 100)
        static member toEp(value: int<cp>) = ep.create(int value / 50), cp.create(int value % 50)
        static member toSp(value: int<cp>) = sp.create(int value / 10), cp.create(int value % 10)
        
    type CoinBag = {
        Platinum: int<pp>
        Gold: int<gp>
        Electrum: int<ep>
        Silver: int<sp>
        Copper: int<cp>
    }
    
    [<CompiledName("GetCoinBag")>]
    let getCoinBag (wealth: int<cp>) =
        let platinum, cRemain = wealth |> cp.toPp
        let gold, cRemain = cRemain |> cp.toGp
        let electrum, cRemain = cRemain |> cp.toEp
        let silver, cRemain = cRemain |> cp.toSp
        
        {
            Platinum = platinum
            Gold = gold
            Electrum = electrum
            Silver = silver
            Copper = cRemain
        }
        
    let getCopper (bag: CoinBag) =
        let copper = bag.Platinum |> pp.toCp
        let copper = copper + (bag.Gold |> gp.toCp)
        let copper = copper + (bag.Electrum |> ep.toCp)
        let copper = copper + (bag.Silver |> sp.toCp)
        copper + bag.Copper

open Currency

module Lore =
    type Origin = {
        ruleSystem: string
        nationalOrigin: string
        startingLevel: int
        statRollMethod: string
    }
    
open Lore

module Character =  

    type Sex =
        | Male
        | Female
        | NonBinary
        | Neither

    type Name = string

    type CharacterClass =
        | Artificer
        | Bard
        | Barbarian
        | Cleric
        | Druid
        | Fighter
        | Monk
        | Paladin
        | Ranger
        | Rogue
        | Sorcerer
        | Warlock
        | Wizard

        static member All =
            [
                Artificer
                Bard
                Barbarian
                Cleric
                Druid
                Fighter
                Monk
                Paladin
                Ranger
                Rogue
                Sorcerer
                Warlock
                Wizard
            ]

    type AbilityScoreMethod =
        | Rolled
        | StandardArray
        | PointBuy
        
    type Stats = {
    Str: int
    Dex: int
    Con: int
    Int: int
    Wis: int
    Cha: int
    }

    type CharacterSheet =
        {
            Id: int option
            Name: Name
            Origin: Origin
            Sex: Sex
            Stats: Stats
            Health: (int * int) array
            AC: int
            Experience: int<xp>
            Levels: (CharacterClass * int) array
            Wealth: int<cp>
        }

module Inventory =
    
    type ItemType =
    | Armor
    | Potion
    | Ring
    | Rod
    | Scroll
    | Staff
    | Wand
    | Weapon
    | Wondrous
    | Other
    
    type Item = {
        Name: string
        Description: string
        Weight: float
    }
    
module Leveling =
    let xpChart =
        [
            1, 0
            2, 300
            3, 900
            4, 2700
            5, 6500
            6, 14000
            7, 23000
            8, 34000
            9, 48000
            10, 64000
            11, 85000
            12, 100000
            13, 120000
            14, 140000
            15, 165000
            16, 195000
            17, 225000
            18, 265000
            19, 305000
            20, 355000
        ]
        |> Map.ofList

    let pointCost (score: int) =
        match score with
        | 8 -> Some 0
        | 9 -> Some 1
        | 10 -> Some 2
        | 11 -> Some 3
        | 12 -> Some 4
        | 13 -> Some 5
        | 14 -> Some 7
        | 15 -> Some 9
        | _ -> None