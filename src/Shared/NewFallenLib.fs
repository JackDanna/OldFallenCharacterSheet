namespace FallenLib

module Neg1To4 =
    type Neg1To4 = 
    | NegOne
    | Zero
    | One
    | Two
    | Three
    | Four

module Dice =

    type DicePool = {
        d4  : uint
        d6  : uint
        d8  : uint
        d10 : uint
        d12 : uint
        d20 : uint
    }

    type DicePoolPenalty = uint    // always should deduct the dice with the fewest faces first (i.e. d4, then d6, then d8...)

    type DicePoolModification =
    | AddDice    of DicePool
    | RemoveDice of DicePoolPenalty

    let emptyDicePool    = { d4=0u; d6=0u; d8=0u; d10=0u; d12=0u; d20=0u}
    let baseDicePool     = { d4=0u; d6=3u; d8=0u; d10=0u; d12=0u; d20=0u}

    let createD6DicePoolMod (numDice:uint) = AddDice { d4=0u; d6=numDice; d8=0u; d10=0u; d12=0u; d20=0u }

    let diceToString numDice diceTypeString =
        if numDice <> 0u then string numDice + diceTypeString else ""

    let checkIfEmptyDicePoolString dicePoolString =
        if dicePoolString = "" then "0d6" else dicePoolString

    let dicePoolToString (dicePool: DicePool) =
        [|
            diceToString dicePool.d4 "d4"
            diceToString dicePool.d6 "d6"
            diceToString dicePool.d8 "d8"
            diceToString dicePool.d10 "d10"
            diceToString dicePool.d12 "d12"
            diceToString dicePool.d20 "d20"
        |]
        |> Array.filter ( fun diceString -> diceString <> "")
        |> String.concat ", "
        |> checkIfEmptyDicePoolString

    let combineDicePools dicePools =
        Array.fold (fun acc pool ->
            {
                d4  = acc.d4  + pool.d4
                d6  = acc.d6  + pool.d6
                d8  = acc.d8  + pool.d8
                d10 = acc.d10 + pool.d10
                d12 = acc.d12 + pool.d12
                d20 = acc.d20 + pool.d20
            }
        ) { d4 = 0u; d6 = 0u; d8 = 0u; d10 = 0u; d12 = 0u; d20 = 0u } dicePools

    let removeDice (dice:uint) (neg:uint) : uint * uint =
        let result = int dice - int neg
        // If the result is negative, their are still dice to lose, but they are of a higher face value
        if result < 0 then
            (0u, uint (abs result))
        // Else the result is 0 or positive, there are no more dice to lose
        else
            (uint result, 0u)

    let removeDiceFromDicePool (dicePool:DicePool) (numDiceToRemove:DicePoolPenalty) =
        let d4,  d6Neg  = removeDice dicePool.d4 numDiceToRemove
        let d6,  d8Neg  = removeDice dicePool.d6 d6Neg
        let d8,  d10Neg = removeDice dicePool.d8 d8Neg
        let d10, d12Neg = removeDice dicePool.d10 d10Neg
        let d12, d20Neg = removeDice dicePool.d12 d12Neg
        let d20, _      = removeDice dicePool.d20 d20Neg
        { d4=d4; d6=d6; d8=d8; d10=d10; d12=d12; d20=d20 }

    let modifyDicePool (dicePool:DicePool) (dicePoolModification:DicePoolModification)  : DicePool =
        match dicePoolModification with
        | AddDice diceToAdd ->
            combineDicePools [|dicePool; diceToAdd|]
        | RemoveDice diceToRemove ->
            removeDiceFromDicePool dicePool diceToRemove
    
    let sumDicePool (dicePool: DicePool) =
        let { d4 = d4; d6 = d6; d8 = d8; d10 = d10; d12 = d12; d20 = d20 } = dicePool
        d4 + d6 + d8 + d10 + d12 + d20

    let modifyDicePoolByModList (dicePool:DicePool) (dicePoolModifications:DicePoolModification array) : DicePool =

        let acc : DicePoolPenalty = 0u
        let combinedDicePoolPenalty = 
            Array.fold (fun acc diceMod ->
                match diceMod with
                | RemoveDice dicePoolPenalty -> acc + dicePoolPenalty
                | _ -> acc
            ) acc dicePoolModifications |> RemoveDice

        let combinedPositiveDicePool = 
            Array.fold (fun acc diceMod ->
                match diceMod with
                | AddDice dicePool -> combineDicePools [| acc; dicePool |] 
                | _ -> acc
            ) dicePool dicePoolModifications

        // Does the subtractions only at the end after combining
        modifyDicePool combinedPositiveDicePool combinedDicePoolPenalty

    let dicePoolModToInt (dicePoolMod: DicePoolModification) : int =
        match dicePoolMod with
        | AddDice data -> sumDicePool data |> int
        | RemoveDice data -> (int data) * -1

    let intToDicePoolModification (num : int) =
        if num < 0 then RemoveDice (uint (abs num)) else AddDice { d4=0u; d6=(uint num); d8=0u; d10=0u; d12=0u; d20=0u }

    let createDicePoolModification (numDiceStr:string) (diceType:string) =
        let numDice = uint numDiceStr
        match diceType with
        | "4"  -> {d4=numDice;d6=0u;d8=0u;d10=0u;d12=0u;d20=0u}
        | "6"  -> {d4=0u;d6=numDice;d8=0u;d10=0u;d12=0u;d20=0u}
        | "8"  -> {d4=0u;d6=0u;d8=numDice;d10=0u;d12=0u;d20=0u}
        | "10" -> {d4=0u;d6=0u;d8=0u;d10=numDice;d12=0u;d20=0u}
        | "12" -> {d4=0u;d6=0u;d8=0u;d10=0u;d12=numDice;d20=0u}
        | "20" -> {d4=0u;d6=0u;d8=0u;d10=0u;d12=0u;d20=numDice}
        | _    -> {d4=0u;d6=0u;d8=0u;d10=0u;d12=0u;d20=0u}
    
    let stringToDicePool (str:string) =
        str.Split ", "
        |> Array.map( fun (diceStr) ->
            let diceNumAndDiceType = diceStr.Split "d"
            createDicePoolModification diceNumAndDiceType[0] diceNumAndDiceType[1]
        )
        |> combineDicePools

    let stringToDicePoolModification (dicePoolJSONString:string) : DicePoolModification =
        if dicePoolJSONString.Contains("+") then
            let str = dicePoolJSONString.Replace("+", "")
            AddDice <| stringToDicePool str
        elif dicePoolJSONString.Contains("-") then
            let removeDiceString = dicePoolJSONString.Replace("-", "")

            match System.UInt32.TryParse(removeDiceString) with
            | (true, result) ->
                RemoveDice result
            | _ ->
                RemoveDice 0u
        else
            RemoveDice 0u

    let stringToDicePoolModificationOption (dicePoolJSONString:string) : DicePoolModification option=
        match dicePoolJSONString with
        | "None" -> None
        | modString -> Some <| stringToDicePoolModification modString



module SkillUtils =

    open Neg1To4
    open Dice

    type Skill = {
        name                 : string
        skillLevel           : Neg1To4
        attributeDiceMod     : DicePoolModification
        baseDice             : DicePool
    }

    let neg1To4_To_d6_DiceMod neg1To4 =
        match neg1To4 with
        | NegOne -> RemoveDice 1u
        | Zero -> AddDice emptyDicePool
        | One -> AddDice { d4=0u; d6=1u; d8=0u; d10=0u; d12=0u; d20=0u }
        | Two -> AddDice { d4=0u; d6=2u; d8=0u; d10=0u; d12=0u; d20=0u }
        | Three-> AddDice { d4=0u; d6=3u; d8=0u; d10=0u; d12=0u; d20=0u }
        | Four -> AddDice { d4=0u; d6=4u; d8=0u; d10=0u; d12=0u; d20=0u }

    let skillToDicePoolString baseDice skillLevel attributeDiceMod =
        modifyDicePoolByModList
            baseDice 
            [|
                skillLevel |> neg1To4_To_d6_DiceMod
                attributeDiceMod
            |]
        |> dicePoolToString