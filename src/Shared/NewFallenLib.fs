namespace FallenLib

module Neg1To4 =
    type Neg1To4 = 
    | NegOne
    | Zero
    | One
    | Two
    | Three
    | Four

    let neg1To4ToInt neg1To4 =
        match neg1To4 with
        | NegOne -> -1
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4

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

    let emptyDicePool    = { d4=0u; d6=0u; d8=0u; d10=0u; d12=0u; d20=0u}
    let baseDicePool     = { d4=0u; d6=3u; d8=0u; d10=0u; d12=0u; d20=0u}

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

    let sumDicePool (dicePool: DicePool) =
        let { d4 = d4; d6 = d6; d8 = d8; d10 = d10; d12 = d12; d20 = d20 } = dicePool
        d4 + d6 + d8 + d10 + d12 + d20

    let createDicePool (numDiceStr:string) (diceType:string) =
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
            createDicePool diceNumAndDiceType[0] diceNumAndDiceType[1]
        )
        |> combineDicePools

    type DicePoolCalculation = {
        dicePool        : DicePool
        dicePoolPenalty : DicePoolPenalty
    }

    let emptyDicePoolCalculation = {
        dicePool = emptyDicePool
        dicePoolPenalty = 0u
    }

    let baseDicePoolCalculation = {
        dicePool = baseDicePool
        dicePoolPenalty = 0u
    }

    let createD6DicePoolCalc numDice = 
        {
            dicePool = {d4=0u;d6=numDice;d8=0u;d10=0u;d12=0u;d20=0u}
            dicePoolPenalty = 0u
        }

    let createPenaltyDicePoolCalc num = 
        {
            dicePool = {d4=0u;d6=0u;d8=0u;d10=0u;d12=0u;d20=0u}
            dicePoolPenalty = num
        }

    let combineDicePoolCalculations dicePoolCalculations =
        List.fold (fun acc next ->
            {
                dicePool = {
                    d4  = acc.dicePool.d4  + next.dicePool.d4
                    d6  = acc.dicePool.d6  + next.dicePool.d6
                    d8  = acc.dicePool.d8  + next.dicePool.d8
                    d10 = acc.dicePool.d10 + next.dicePool.d10
                    d12 = acc.dicePool.d12 + next.dicePool.d12
                    d20 = acc.dicePool.d20 + next.dicePool.d20
                }
                dicePoolPenalty = acc.dicePoolPenalty + next.dicePoolPenalty
            }
        ) emptyDicePoolCalculation dicePoolCalculations

    let calcDicePoolCalculation dicePoolCalculation =
        removeDiceFromDicePool dicePoolCalculation.dicePool dicePoolCalculation.dicePoolPenalty

module Attribute =
    open Neg1To4
    
    type Attribute = {
        name : string
        neg1To4Stat : Neg1To4
    }

module SkillUtils =

    open Neg1To4
    open Dice
    open Attribute

    type Skill = {
        name  : string
        level : Neg1To4
    }

    let skillInit() = { name = ""; level = Zero }

    let neg1To4_To_d6_DicePoolCalc neg1To4 =
        match neg1To4 with
        | NegOne -> createPenaltyDicePoolCalc 1u
        | Zero -> createD6DicePoolCalc 0u
        | One -> createD6DicePoolCalc 1u
        | Two -> createD6DicePoolCalc 2u
        | Three-> createD6DicePoolCalc 3u
        | Four -> createD6DicePoolCalc 4u

    let coreSkillToDicePoolString baseDice skillLevel attributelevel =
        combineDicePoolCalculations
            [
                baseDice
                skillLevel |> neg1To4_To_d6_DicePoolCalc
                attributelevel |> neg1To4_To_d6_DicePoolCalc
            ]
        |> calcDicePoolCalculation
        |> dicePoolToString

    let skillToDicePoolString baseDice skillLevel attributeDiceMod =
        combineDicePoolCalculations
            [
                baseDice
                skillLevel |> neg1To4_To_d6_DicePoolCalc
                attributeDiceMod
            ]
        |> calcDicePoolCalculation
        |> dicePoolToString

module Vocation =

    open SkillUtils
    open Dice
    open Attribute

    type ZeroToFour =
    | Zero
    | One
    | Two
    | Three
    | Four

    type GoverningAttribute = {
        isGoverning : bool
        attributeStat : Attribute
    }

    let zeroToFourToUint zeroToFour =
        match zeroToFour with
        | Zero -> 0u
        | One -> 1u
        | Two -> 2u
        | Three -> 3u
        | Four -> 4u
    
    let zeroToFourToDiceCalc zeroToFour =
        zeroToFour
        |> zeroToFourToUint
        |> createD6DicePoolCalc
    

    let governingAttributesToDiceCalc governingAttributes =
        governingAttributes
        |> List.filter ( fun governingAttribute -> governingAttribute.isGoverning )
        |> List.map ( fun governingAttribute ->
            neg1To4_To_d6_DicePoolCalc governingAttribute.attributeStat.neg1To4Stat
        )
        |> combineDicePoolCalculations

    let vocationToDicePoolString baseDice governingAttributes (vocationLevel:ZeroToFour) =
            governingAttributes
            |> governingAttributesToDiceCalc
            |> List.singleton
            |> List.append [
                baseDice
                vocationLevel |> zeroToFourToDiceCalc
            ]
            |> combineDicePoolCalculations
            |> calcDicePoolCalculation
            |> dicePoolToString

module VocationalSkill =
    open SkillUtils
    open Neg1To4
    open Dice
    open Vocation

    let zeroToFourToNegOneToFour zeroToFour =
        match zeroToFour with
        | Zero -> Neg1To4.Zero
        | One -> Neg1To4.One
        | Two -> Neg1To4.Two
        | Three -> Neg1To4.Three
        | Four -> Neg1To4.Four

    let vocationalToDicePoolString baseDice governingAttributes (vocationalSkillLevel:Neg1To4) =
            governingAttributes
            |> governingAttributesToDiceCalc
            |> List.singleton
            |> List.append [
                baseDice
                vocationalSkillLevel |> neg1To4_To_d6_DicePoolCalc

            ]
            |> combineDicePoolCalculations
            |> calcDicePoolCalculation
            |> dicePoolToString