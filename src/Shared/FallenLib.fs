namespace FallenLib

// Util
module StringUtils =
    open System.Text.RegularExpressions

    let isNumeric (number: string) =
        let regex = Regex(@"^[0-9]+$")
        regex.IsMatch(number)

module MathUtils =
    open System

    let divideUintByUintThenRound numerator divisor roundDown =
        let floatCalculation = float numerator / float divisor

        match roundDown with
        | true -> Math.Floor floatCalculation |> uint
        | false -> Math.Ceiling floatCalculation |> uint

    let divideUintsThenCompareToMaxThenRound
        (numerator: uint)
        (divisor: uint)
        (maxAllowableValue: uint option)
        roundDown
        =
        let result = divideUintByUintThenRound numerator divisor roundDown

        match maxAllowableValue with
        | Some max -> if (max < result) then max else result
        | None -> result

module TypeUtils =

    let stringListToTypeMap (stringTypeArray: string list) =
        List.zip stringTypeArray stringTypeArray
        |> Map.ofList

module Penetration =

    type Penetration = uint

module DamageType =

    type DamageType = string

    let damageTypesToString (damageTypes: DamageType list) = String.concat ", " damageTypes

    let stringToDamageTypeList (damageTypeMap: Map<string, DamageType>) (damageTypesString: string) =
        if damageTypesString.Length = 0 then
            []
        else
            damageTypesString.Split ", "
            |> List.ofArray
            |> List.map (fun (damageTypeString) -> damageTypeMap.Item damageTypeString)

module EngageableOpponents =

    open MathUtils
    open StringUtils

    type EngageableOpponentsCalculation =
        { name: string
          combatRollDivisor: uint
          maxEO: uint option }

    let eoCalculationListToMap eoCalculationList =
        eoCalculationList
        |> List.map (fun (eoCalculation) -> eoCalculation.name, eoCalculation)
        |> Map.ofList

    type CalculatedEngageableOpponents = uint

    type EngageableOpponents =
        | Calculation of EngageableOpponentsCalculation
        | Calculated of CalculatedEngageableOpponents

    let determineEngageableOpponents numDice engageableOpponents =
        match engageableOpponents with
        | Calculated calculatedEngageableOpponents -> calculatedEngageableOpponents
        | Calculation eoCalculation ->
            divideUintsThenCompareToMaxThenRound numDice eoCalculation.combatRollDivisor eoCalculation.maxEO true


    //Todo: this needs to only parse uints
    let parseMaxEngageableOpponentsString input =
        if isNumeric input then
            uint input |> Some
        else
            None

    let parseEngaeableOpponentsString eoCalculationMap input =
        if isNumeric input then
            uint input |> Calculated
        elif Map.containsKey input eoCalculationMap then
            eoCalculationMap.Item input |> Calculation
        else
            Calculated 0u

module Neg1To4 =
    type Neg1To4 =
        | NegOne
        | Zero
        | One
        | Two
        | Three
        | Four

    let createNeg1To4Option num : Neg1To4 option =
        match num with
        | -1 -> Some NegOne
        | 0 -> Some Zero
        | 1 -> Some One
        | 2 -> Some Two
        | 3 -> Some Three
        | 4 -> Some Four
        | _ -> None

    let neg1To4ToInt neg1To4 =
        match neg1To4 with
        | NegOne -> -1
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4

    let intToNeg1To4 num =
        defaultArg (createNeg1To4Option num) NegOne

module ZeroToFour =

    type ZeroToFour =
        | Zero
        | One
        | Two
        | Three
        | Four

    let zeroToFourToUint zeroToFour =
        match zeroToFour with
        | Zero -> 0u
        | One -> 1u
        | Two -> 2u
        | Three -> 3u
        | Four -> 4u

module ZeroToThree =
    type ZeroToThree =
        | Zero
        | One
        | Two
        | Three

    let zeroToThreeToUint zeroToThree =
        match zeroToThree with
        | Zero -> 0u
        | One -> 1u
        | Two -> 2u
        | Three -> 3u

module Dice =

    open Neg1To4
    open ZeroToFour

    type DicePool =
        { d4: uint
          d6: uint
          d8: uint
          d10: uint
          d12: uint
          d20: uint }

    type DicePoolPenalty = uint // always should deduct the dice with the fewest faces first (i.e. d4, then d6, then d8...)

    type DicePoolMod =
        | AddDice of DicePool
        | RemoveDice of DicePoolPenalty

    let emptyDicePool =
        { d4 = 0u
          d6 = 0u
          d8 = 0u
          d10 = 0u
          d12 = 0u
          d20 = 0u }

    let baseDicePool = { emptyDicePool with d6 = 3u }

    let createD6DicePoolMod (numDice: uint) =
        AddDice
            { d4 = 0u
              d6 = numDice
              d8 = 0u
              d10 = 0u
              d12 = 0u
              d20 = 0u }

    let diceToString numDice diceTypeString =
        if numDice <> 0u then
            string numDice + diceTypeString
        else
            ""

    let checkIfEmptyDicePoolString dicePoolString =
        if dicePoolString = "" then
            "0d6"
        else
            dicePoolString

    let dicePoolToString (dicePool: DicePool) =
        [ diceToString dicePool.d4 "d4"
          diceToString dicePool.d6 "d6"
          diceToString dicePool.d8 "d8"
          diceToString dicePool.d10 "d10"
          diceToString dicePool.d12 "d12"
          diceToString dicePool.d20 "d20" ]
        |> List.filter (fun diceString -> diceString <> "")
        |> String.concat ", "
        |> checkIfEmptyDicePoolString

    let combineDicePools dicePools =
        List.fold
            (fun acc pool ->
                { d4 = acc.d4 + pool.d4
                  d6 = acc.d6 + pool.d6
                  d8 = acc.d8 + pool.d8
                  d10 = acc.d10 + pool.d10
                  d12 = acc.d12 + pool.d12
                  d20 = acc.d20 + pool.d20 })
            emptyDicePool
            dicePools

    let removeDice (dice: uint) (neg: uint) : uint * uint =
        let result = int dice - int neg
        // If the result is negative, their are still dice to lose, but they are of a higher face value
        if result < 0 then
            (0u, uint (abs result))
        // Else the result is 0 or positive, there are no more dice to lose
        else
            (uint result, 0u)

    let removeDiceFromDicePool (dicePool: DicePool) (numDiceToRemove: DicePoolPenalty) =
        let d4, d6Neg = removeDice dicePool.d4 numDiceToRemove
        let d6, d8Neg = removeDice dicePool.d6 d6Neg
        let d8, d10Neg = removeDice dicePool.d8 d8Neg
        let d10, d12Neg = removeDice dicePool.d10 d10Neg
        let d12, d20Neg = removeDice dicePool.d12 d12Neg
        let d20, _ = removeDice dicePool.d20 d20Neg

        { d4 = d4
          d6 = d6
          d8 = d8
          d10 = d10
          d12 = d12
          d20 = d20 }

    let modifyDicePool (dicePool: DicePool) (dicePoolMod: DicePoolMod) : DicePool =
        match dicePoolMod with
        | AddDice diceToAdd -> combineDicePools [ dicePool; diceToAdd ]
        | RemoveDice diceToRemove -> removeDiceFromDicePool dicePool diceToRemove

    let dicePoolToNumDice (dicePool: DicePool) =
        let { d4 = d4
              d6 = d6
              d8 = d8
              d10 = d10
              d12 = d12
              d20 = d20 } =
            dicePool

        d4 + d6 + d8 + d10 + d12 + d20

    let modifyDicePoolByDicePoolModList dicePool dicePoolMods =

        let combinedDicePoolPenalty =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | RemoveDice dicePoolPenalty -> acc + dicePoolPenalty
                    | _ -> acc)
                0u
                dicePoolMods
            |> RemoveDice

        let combinedPositiveDicePool =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | AddDice dicePool -> combineDicePools [ acc; dicePool ]
                    | _ -> acc)
                dicePool
                dicePoolMods

        // Does the subtractions only at the end after combining
        modifyDicePool combinedPositiveDicePool combinedDicePoolPenalty

    let intToD6DicePoolMod (num: int) =
        if num < 0 then
            RemoveDice(uint (abs num))
        else
            createD6DicePoolMod (uint num)

    let neg1To4ToD6DicePoolMod neg1To4 =
        neg1To4 |> neg1To4ToInt |> intToD6DicePoolMod

    let zeroToFourToDicePoolMod zeroToFour =
        zeroToFour
        |> zeroToFourToUint
        |> createD6DicePoolMod

    let createDicePoolMod (numDiceStr: string) (diceType: string) =
        let numDice = uint numDiceStr

        match diceType with
        | "4" -> { emptyDicePool with d4 = numDice }
        | "6" -> { emptyDicePool with d6 = numDice }
        | "8" -> { emptyDicePool with d8 = numDice }
        | "10" -> { emptyDicePool with d10 = numDice }
        | "12" -> { emptyDicePool with d12 = numDice }
        | "20" -> { emptyDicePool with d20 = numDice }
        | _ -> emptyDicePool

    let parseDicePoolString (dicePoolString: string) =
        dicePoolString.Split ", "
        |> List.ofArray
        |> List.map (fun (diceStr) ->
            let diceNumAndDiceType = diceStr.Split "d"
            createDicePoolMod diceNumAndDiceType[0] diceNumAndDiceType[1])
        |> combineDicePools

    let parseDicePoolModString (dicePoolModString: string) : DicePoolMod =
        if dicePoolModString.Contains("+") then
            let str = dicePoolModString.Replace("+", "")
            AddDice <| parseDicePoolString str
        elif dicePoolModString.Contains("-") then
            let removeDiceString = dicePoolModString.Replace("-", "")

            match System.UInt32.TryParse(removeDiceString) with
            | (true, result) -> RemoveDice result
            | _ -> RemoveDice 0u
        else
            RemoveDice 0u

    let parseDicePoolModOptionString (dicePoolJSONString: string) : DicePoolMod option =
        match dicePoolJSONString with
        | "None" -> None
        | modString -> Some <| parseDicePoolModString modString

    let dicePoolModToString dicePoolMod =
        match dicePoolMod with
        | RemoveDice removeDice -> $"-{uint removeDice}d"
        | AddDice addDice -> dicePoolToString addDice

module Attribute =
    open Neg1To4
    open Dice

    type Attribute = string

    type AttributeStat = { attribute: Attribute; lvl: Neg1To4 }

    let sumAttributesLevels attributeList attributeStatList =
        attributeStatList
        |> List.map (fun attributeStat ->
            if List.contains attributeStat.attribute attributeList then
                neg1To4ToInt attributeStat.lvl
            else
                0)
        |> List.sum

    let sumAttributesD6DiceMods attributeList attributeStatList =
        sumAttributesLevels attributeList attributeStatList
        |> intToD6DicePoolMod

module BattleMapUOM =
    let feetPerBattleMapUOM = 5u

module Range =

    open System

    type CalculatedRange =
        { name: string
          effectiveRange: uint
          maxRange: uint }

    type RangeCalculation =
        { name: string // Description of the range
          numDicePerEffectiveRangeUnit: uint // The amount of dice required to gain an effective Range Unit
          ftPerEffectiveRangeUnit: uint // The amount of ft per effective range unit
          roundEffectiveRangeUp: bool // If true, round up the effective range, otherwise round down
          maxRange: uint } // Max Range is the absolute limit of how far a attack can go

    type Range =
        | CalculatedRange of CalculatedRange
        | RangeCalculation of RangeCalculation

    type RangeAdjustment = int

    let calculatedRangeToString calculatedRange =
        sprintf "%u/%u" calculatedRange.effectiveRange calculatedRange.maxRange

    let calculateRange numDice rangeCalculation =
        { name = rangeCalculation.name
          effectiveRange =
            if rangeCalculation.roundEffectiveRangeUp then
                uint (
                    Math.Ceiling(
                        float numDice
                        / float rangeCalculation.numDicePerEffectiveRangeUnit
                    )
                )
                * rangeCalculation.ftPerEffectiveRangeUnit
            else
                uint (
                    Math.Floor(
                        float numDice
                        / float rangeCalculation.numDicePerEffectiveRangeUnit
                    )
                )
                * rangeCalculation.ftPerEffectiveRangeUnit
          maxRange = rangeCalculation.maxRange }

    let rangeToCalculatedRange (numDice: uint) (range: Range) : CalculatedRange =
        match range with
        | CalculatedRange calculatedRange -> calculatedRange
        | RangeCalculation rangeCalculation -> calculateRange numDice rangeCalculation

    let determineGreatestRange numDice (primaryRange: Range) (optionalRange: Range option) =
        let calculatedPrimaryRange = rangeToCalculatedRange numDice primaryRange

        match optionalRange with
        | Some secondaryRange ->
            let calculatedSecondaryRange = rangeToCalculatedRange numDice secondaryRange

            if calculatedPrimaryRange.effectiveRange
               >= calculatedSecondaryRange.effectiveRange then
                calculatedPrimaryRange
            else
                calculatedSecondaryRange
        | None -> calculatedPrimaryRange

    let calculatedRangeListToRangeMap calculatedRangeList =
        List.map
            (fun (calculatedRange: CalculatedRange) -> calculatedRange.name, CalculatedRange calculatedRange)
            calculatedRangeList
        |> Map.ofList

    let rangeCalculationListToRangeMap rangeCalculationList =
        List.map
            (fun (rangeCalculation: RangeCalculation) -> rangeCalculation.name, RangeCalculation rangeCalculation)
            rangeCalculationList
        |> Map.ofList

    let createRangeMap calculatedRanges rangeCalculations : Map<string, Range> =
        Map.fold
            (fun acc key value -> Map.add key value acc)
            (calculatedRangeListToRangeMap calculatedRanges)
            (rangeCalculationListToRangeMap rangeCalculations)

    let isMeleeOrReachRange range =
        match range with
        | CalculatedRange calculatedRange ->
            match calculatedRange.name with
            | "Melee"
            | "Reach" -> true
            | _ -> false
        | _ -> false

module AreaOfEffect =
    type AreaOfEffect =
        | Cone
        | Sphere

    let AreaOfEffectOptionMap =
        Map [ ("Cone", Some Cone)
              ("Sphere", Some Sphere)
              ("None", None) ]

module CalculatedAOE =
    open System
    open AreaOfEffect
    open BattleMapUOM

    type CalculatedCone =
        { area: float
          distance: uint
          angle: float }

    type CalculatedSphere = { area: float; radius: float }

    type CalculatedAOE =
        | ConeToCalculatedCone of CalculatedCone
        | SphereToCalculatedSphere of CalculatedSphere

    let calculatedConeToString decimalPlaces (calculatedCone: CalculatedCone) =
        let decimalLimitedArea =
            calculatedCone.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedAngle =
            calculatedCone.angle.ToString("F" + decimalPlaces.ToString())

        sprintf
            "area: %s ft^2, distance: %u ft, angle: %s θ"
            decimalLimitedArea
            calculatedCone.distance
            decimalLimitedAngle

    let calculatedSphereToString decimalPlaces calculatedSphere =
        let decimalLimitedArea =
            calculatedSphere.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedRadius =
            calculatedSphere.radius.ToString("F" + decimalPlaces.ToString())

        sprintf "area: %s ft^2, radius: %s ft" decimalLimitedArea decimalLimitedRadius

    let calculatedAOEToString calculatedAOE =
        let decimalPlaces = 1

        match calculatedAOE with
        | ConeToCalculatedCone calculatedCone -> calculatedConeToString decimalPlaces calculatedCone
        | SphereToCalculatedSphere sphereShape -> calculatedSphereToString decimalPlaces sphereShape


    let calculatedAOEOptionToString shapeOption =
        match shapeOption with
        | Some shape -> calculatedAOEToString shape
        | None -> ""

    let calcConeArea (distance: uint) (angle: float) : float =
        float (distance * distance)
        * Math.Tan(angle / 2.0)

    let calcConeDistance (area: uint) (angle: float) =
        uint (Math.Sqrt(float area / Math.Tan(angle / 2.)))

    let calcConeAngle (area: uint) (distance: uint) =
        2.
        * Math.Atan(Math.Sqrt(float area / float (distance * distance)))

    let calcCone (numDice: uint) : CalculatedCone =
        let distance = numDice * feetPerBattleMapUOM
        let angle = 53.0

        { area = calcConeArea distance angle
          distance = distance
          angle = angle }

    let calcCircle (numDice: uint) : CalculatedSphere =
        let radius: float = 2.5 * float numDice

        { area = 2.0 * Math.PI * (radius ** 2)
          radius = radius }

    let calcShape (numDice: uint) (aoe: AreaOfEffect) : CalculatedAOE =
        match aoe with
        | Cone -> ConeToCalculatedCone(calcCone numDice)
        | Sphere -> SphereToCalculatedSphere(calcCircle numDice)

    let determineAOEShapeOption numDice aoe =
        match aoe with
        | Some aoe -> Some(calcShape numDice aoe)
        | None -> None

    let compareAndDetermineAOEShapeOption
        (numDice: uint)
        (aoe: AreaOfEffect option)
        (resourceAOE: AreaOfEffect option)
        : CalculatedAOE option =
        match resourceAOE with
        | Some resourceAOE -> Some(calcShape numDice resourceAOE)
        | None -> determineAOEShapeOption numDice aoe

module ResourceClass =
    type ResourceClass = string

// Magic

module ResourcePool =
    open ResourceClass

    type ResourcePool =
        { name: ResourceClass
          remainingResources: uint
          poolMax: uint }

module MagicResourceForCombatCalculation =

    open Dice
    open ResourceClass

    type MagicResourceForCombatCalculation =
        { magicResouceClass: ResourceClass
          numMagicResourceDice: uint }

    let magicResourceForCombatToResourceDesc resource =
        $"( {resource.numMagicResourceDice} {resource.magicResouceClass})"

    let magicResourceForCombatToResourceDescAndD6DicePoolMod (resource: MagicResourceForCombatCalculation) =
        magicResourceForCombatToResourceDesc resource, createD6DicePoolMod resource.numMagicResourceDice

module MagicSkill =
    open DamageType
    open ResourceClass
    open Range

    type MagicSkill =
        { name: string
          damageTypes: DamageType list
          rangeAdjustment: RangeAdjustment
          isMeleeCapable: bool
          resourceClass: ResourceClass }

module MagicCombat =
    open Range
    open EngageableOpponents
    open AreaOfEffect
    open Neg1To4
    open Penetration
    open Dice

    type MagicCombat =
        { name: string
          lvlRequirment: Neg1To4
          dicePoolMod: DicePoolMod
          penetration: Penetration
          range: Range
          engageableOpponents: EngageableOpponents
          minResourceRequirment: uint
          areaOfEffect: AreaOfEffect option }

    let determineMagicCombatTypes (meleeCapable: bool) (lvl: Neg1To4) (magicCombatList: MagicCombat list) =
        // Logic for filtering by Lvl requirement
        List.filter
            (fun magicCombat ->
                neg1To4ToInt lvl
                >= neg1To4ToInt magicCombat.lvlRequirment)
            magicCombatList
        // Logic for filtering by if it can melee
        |> List.filter (fun magicCombat ->
            not (
                isMeleeOrReachRange magicCombat.range
                && not meleeCapable
            ))

// Item

module ConduitClass =
    open MagicSkill
    open Dice
    open Range
    open DamageType
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceClass

    type ConduitClass =
        { name: string
          oneHandedDice: DicePoolMod option
          twoHandedDice: DicePoolMod
          penetration: Penetration
          rangeAdjustment: RangeAdjustment
          damageTypes: DamageType list
          engageableOpponents: EngageableOpponents option
          dualWieldableBonus: DicePoolMod option
          areaOfEffect: AreaOfEffect option
          resourceClass: ResourceClass option
          effectedMagicSkills: MagicSkill list }

module WeaponResourceClass =

    open Dice
    open Range
    open DamageType
    open AreaOfEffect
    open ResourceClass
    open Penetration

    type WeaponResourceClass =
        { name: string
          resourceClass: ResourceClass
          resourceDice: DicePoolMod
          penetration: Penetration
          range: Range option
          damageTypes: DamageType list
          areaOfEffect: AreaOfEffect option }

    let weaponResourceClassOptionToWeaponResourceClass (resource: WeaponResourceClass option) =
        match resource with
        | Some resource ->
            (" (" + resource.name + ")",
             resource.resourceDice,
             resource.penetration,
             resource.range,
             resource.damageTypes,
             resource.areaOfEffect)
        | None -> ("", createD6DicePoolMod (0u), 0u, None, [], None)

module WeaponClass =
    open Dice
    open Range
    open DamageType
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceClass

    type WeaponClass =
        { name: string
          oneHandedWeaponDice: DicePoolMod option
          twoHandedWeaponDice: DicePoolMod
          penetration: Penetration
          range: Range
          damageTypes: DamageType list
          engageableOpponents: EngageableOpponents
          dualWieldableBonus: DicePoolMod option
          areaOfEffect: AreaOfEffect option
          resourceClass: ResourceClass option }

module ContainerClass =
    type ContainerClass = { name: string; weightCapacity: float }

module ItemTier =
    open Dice

    type ItemTier =
        { name: string
          level: int
          runeSlots: uint
          baseDice: DicePool
          durabilityMax: uint }

module EffectForDisplay =

    type DurationAndSource = { duration: string; source: string }

    type EffectForDisplay =
        { name: string
          effect: string
          durationAndSource: DurationAndSource }

    let indefiniteStringForDuration = "Indefinite"

module AttributeDeterminedDiceModEffect =
    open Attribute
    open Dice

    type AttributeDeterminedDiceModEffect =
        { name: string
          attributesToEffect: Attribute list
          dicePoolMod: DicePoolMod }

    let attributeDeterminedDiceModEffectToEffectString attributeDeterminedDiceModEffect =
        let attributesString =
            String.concat "," attributeDeterminedDiceModEffect.attributesToEffect

        let dicePoolModString =
            dicePoolModToString attributeDeterminedDiceModEffect.dicePoolMod

        $"{dicePoolModString} {attributesString} ({attributeDeterminedDiceModEffect.name})"

    let collectAttributeDeterminedDiceMod governingAttributesOfSkill attributeDeterminedDiceModList =
        attributeDeterminedDiceModList
        |> List.filter (fun attributeDeterminedDiceMod ->
            attributeDeterminedDiceMod.attributesToEffect
            |> List.exists (fun attribute -> List.contains attribute governingAttributesOfSkill))
        |> List.map (fun attributeDeterminedDiceMod -> attributeDeterminedDiceMod.dicePoolMod)

module AttributeDeterminedDiceModEffectForDisplay =
    open EffectForDisplay
    open AttributeDeterminedDiceModEffect

    type AttributeDeterminedDiceModEffectForDisplay =
        { attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect
          durationAndSource: DurationAndSource }

    let attributeDeterminedDiceModEffectToForDisplay
        (addme: AttributeDeterminedDiceModEffect)
        : AttributeDeterminedDiceModEffectForDisplay =
        { attributeDeterminedDiceModEffect = addme
          durationAndSource = { duration = "?"; source = "?" } }

    let attributeDeterminedDiceModEffectToEffectForDisplay (addmefd: AttributeDeterminedDiceModEffectForDisplay) =
        { name = addmefd.attributeDeterminedDiceModEffect.name
          effect = attributeDeterminedDiceModEffectToEffectString addmefd.attributeDeterminedDiceModEffect
          durationAndSource = addmefd.durationAndSource }

module PhysicalDefenseEffect =

    type PhysicalDefenseEffect =
        { name: string
          physicalDefense: float }

    let physicalDefenseEffectToEffectString defenseClass =
        $"{defenseClass.physicalDefense} Physical Defense"

module SkillDiceModEffect =
    open Dice

    type SkillDiceModEffect =
        { name: string
          skillToEffect: string
          diceMod: DicePoolMod }

    let skillDiceModEffectToEffectString skillDiceModEffect =
        $"{dicePoolModToString skillDiceModEffect.diceMod} {skillDiceModEffect.skillToEffect}"

    let collectSkillAdjustmentDiceMods skillName skillAdjustmentList =
        skillAdjustmentList
        |> List.filter (fun skillAdjustment -> skillAdjustment.skillToEffect = skillName)
        |> List.map (fun skillAdjustment -> skillAdjustment.diceMod)

module SkillDiceModEffectForDisplay =
    open SkillDiceModEffect
    open EffectForDisplay

    type SkillDiceModEffectForDisplay =
        { skillDiceModEffect: SkillDiceModEffect
          durationAndSource: DurationAndSource }

    let skillDiceModEffectToForDisplay (sdme: SkillDiceModEffect) : SkillDiceModEffectForDisplay =
        { skillDiceModEffect = sdme
          durationAndSource = { duration = "?"; source = "?" } }

    let skillDiceModEffectToEffectForDisplay (sdmefd: SkillDiceModEffectForDisplay) =
        { name = sdmefd.skillDiceModEffect.name
          effect = skillDiceModEffectToEffectString sdmefd.skillDiceModEffect
          durationAndSource = sdmefd.durationAndSource }

module AttributeStatAdjustmentEffect =

    open Attribute

    type AttributeStatAdjustmentEffect =
        { name: string
          attribute: Attribute
          adjustment: int }

    let attributeStatAdjustmentToEffectString attributeStatAdjustment =
        $"{attributeStatAdjustment.adjustment} {attributeStatAdjustment.attribute}"

module ItemEffect =

    open SkillDiceModEffectForDisplay

    open SkillDiceModEffect
    open AttributeStatAdjustmentEffect
    open PhysicalDefenseEffect
    open EffectForDisplay
    open AttributeDeterminedDiceModEffect

    type ItemEffect =
        | SkillDiceModEffect of SkillDiceModEffect
        | AttributeStatAdjustmentEffect of AttributeStatAdjustmentEffect
        | DefenseClass of PhysicalDefenseEffect
        | AttributeDeterminedDiceModEffect of AttributeDeterminedDiceModEffect

    let itemEffectToString itemEffect =
        match itemEffect with
        | SkillDiceModEffect skillDiceModEffect -> skillDiceModEffect.name
        | AttributeStatAdjustmentEffect attributeStatAdjustment -> attributeStatAdjustment.name
        | DefenseClass defenseClass -> defenseClass.name
        | AttributeDeterminedDiceModEffect addme -> addme.name

    // _ToEffectForDisplay

    let attributeStatAdjustmentToEffectForDisplay
        (attributeStatAdjustment: AttributeStatAdjustmentEffect)
        duration
        source
        =
        { name = attributeStatAdjustment.name
          effect = attributeStatAdjustmentToEffectString attributeStatAdjustment
          durationAndSource = { duration = duration; source = source } }

    let defenseClassToEffectForDisplay (defenseClass: PhysicalDefenseEffect) duration source =
        { name = defenseClass.name
          effect = physicalDefenseEffectToEffectString defenseClass
          durationAndSource = { duration = duration; source = source } }

    let attributeDeterminedDiceModEffect (temp) duration source =
        { name = temp.name
          effect = attributeDeterminedDiceModEffectToEffectString temp
          durationAndSource = { duration = duration; source = source } }

    let itemEffectToEffectForDisplay itemEffect source =

        let duration = "While equiped"

        match itemEffect with
        | SkillDiceModEffect sdme -> skillDiceModEffectToEffectForDisplay (skillDiceModEffectToForDisplay sdme)
        | AttributeStatAdjustmentEffect asa -> attributeStatAdjustmentToEffectForDisplay asa duration source
        | DefenseClass dc -> defenseClassToEffectForDisplay dc duration source
        | AttributeDeterminedDiceModEffect addme -> attributeDeterminedDiceModEffect addme duration source

    // Collects
    let itemEffectToSkillDiceModEffects (itemEffect: ItemEffect) =
        match itemEffect with
        | SkillDiceModEffect skillAdjustment -> [ skillAdjustment ]
        | _ -> []

    let itemEffectToAttributeDeterminedDiceModEffects (itemEffect: ItemEffect) =
        match itemEffect with
        | AttributeDeterminedDiceModEffect addme -> [ addme ]
        | _ -> []

module Item =
    open ItemTier
    open WeaponClass
    open WeaponResourceClass
    open ConduitClass
    open ContainerClass
    open ItemEffect

    type ItemClass =
        | WeaponClass of WeaponClass
        | ConduitClass of ConduitClass
        | WeaponResourceClass of WeaponResourceClass
        | ContainerClass of ContainerClass
        | ItemEffect of ItemEffect

    type Item =
        { name: string
          itemClasses: ItemClass list
          itemTier: ItemTier
          value: string
          weight: float }

    let sumItemListWeight itemList =
        if List.isEmpty itemList then
            0.0
        else
            List.sumBy (fun item -> item.weight) itemList

    let itemClassesToString itemClasses =
        List.map
            (fun itemClass ->
                match itemClass with
                | WeaponClass weaponClass -> weaponClass.name
                | ConduitClass conduitClass -> conduitClass.name
                | WeaponResourceClass weaponResourceClass -> weaponResourceClass.name
                | ContainerClass containerClass -> containerClass.name
                | ItemEffect itemEffect -> itemEffectToString itemEffect)
            itemClasses
        |> String.concat ", "

    let itemToWeaponClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | WeaponClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let itemToConduitClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ConduitClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let itemToWeaponResourceClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | WeaponResourceClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let itemToItemNameAndContainerClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ContainerClass specifiedItemClass -> [ (item.name, specifiedItemClass) ]
            | _ -> [])

    let itemToContainerClassNames item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ContainerClass _ -> [ item.name ]
            | _ -> [])

    let itemToItemEffectSubTypes (itemEffectToItemEffectSubType) (item: Item) =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ItemEffect itemEffect -> itemEffectToItemEffectSubType itemEffect
            | _ -> [])

    let itemToItemNameAndItemEffectList (item: Item) =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ItemEffect itemEffect -> [ (item.name, itemEffect) ]
            | _ -> [])

    let itemToSkillDiceModEffects =
        itemToItemEffectSubTypes itemEffectToSkillDiceModEffects

    let itemToAttributeDeterminedDiceModEffects =
        itemToItemEffectSubTypes itemEffectToAttributeDeterminedDiceModEffects

module Container =
    open ContainerClass
    open Item

    type Container =
        { name: string
          containerClass: ContainerClass
          isEquipped: bool
          itemList: Item list }

    let itemNameAndContainerClassToContainer (itemName, containerClass: ContainerClass) =
        { name = itemName
          containerClass = containerClass
          isEquipped = false
          itemList = [] }

    let sumContainerListWeight (containerList: Container list) =
        containerList
        |> List.map (fun container -> sumItemListWeight container.itemList)
        |> List.sum

module Equipment =
    open Item
    open ConduitClass

    type Equipment =
        { isEquipped: bool
          item: Item
          quantity: uint }

    let calculateEquipmentListWeight equipmentList =
        equipmentList
        |> List.fold
            (fun acc (equipmentItem: Equipment) ->
                equipmentItem.item.weight
                * float equipmentItem.quantity
                + acc)
            0.0

    let getEquipedItems equipmentList =
        equipmentList
        |> List.filter (fun equipmentItem ->
            equipmentItem.isEquipped
            && equipmentItem.quantity > 0u)
        |> List.map (fun equipmentItem -> equipmentItem.item)

    let doesConduitEffectMagicSkill conduitClass skillName =
        conduitClass.effectedMagicSkills
        |> List.exists (fun magicSkill -> magicSkill.name = skillName)

    let getEquipedConduitItemsWithSkillName equipmentList skillName =
        equipmentList
        |> getEquipedItems
        |> List.filter (fun (item) ->
            item
            |> itemToConduitClasses
            |> List.filter (fun conduitClass -> doesConduitEffectMagicSkill conduitClass skillName)
            |> List.isEmpty
            |> not)

    let equipmentListToSkillDiceModEffects equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect itemToSkillDiceModEffects

    let equipmentToEquipedAttributeDeterminedDiceModEffects equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect itemToAttributeDeterminedDiceModEffects

    let equipmentToEquipedEffectItems equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect itemToItemNameAndItemEffectList

// Character

module SkillStat =
    open Neg1To4
    open Dice

    type SkillStat =
        { name: string
          lvl: Neg1To4
          dicePool: DicePool }

module CoreSkillGroup =

    open Attribute
    open SkillStat
    open Dice
    open AttributeDeterminedDiceModEffect

    type CoreSkillGroup =
        { attributeStat: AttributeStat
          coreSkillList: SkillStat list }

    let coreSkillToDicePool
        baseDice
        lvl
        (attributeStat: AttributeStat)
        skillAdjustmentDiceModList
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        =

        let dicePoolMods =
            skillAdjustmentDiceModList
            @ [ neg1To4ToD6DicePoolMod lvl
                neg1To4ToD6DicePoolMod attributeStat.lvl ]
              @ collectAttributeDeterminedDiceMod [ attributeStat.attribute ] attributeDeterminedDiceModEffectList

        modifyDicePoolByDicePoolModList baseDice dicePoolMods

    let coreSkillGroupListToAttributeStats (coreSkillGroups: CoreSkillGroup list) =
        List.map (fun coreSkillGroup -> coreSkillGroup.attributeStat) coreSkillGroups

    let coreSkillGroupListToAttributes coreSkillGroupData =
        coreSkillGroupListToAttributeStats coreSkillGroupData
        |> List.map (fun attributeStat -> attributeStat.attribute)

    let coreSkillGroupListToSkillStats (coreSkillGroupList: CoreSkillGroup list) =
        coreSkillGroupList
        |> List.collect (fun coreSkillGroup -> coreSkillGroup.coreSkillList)

    let coreSkillGroupListToAttributeStatsAndSkillStats (coreSkillGroupList: CoreSkillGroup list) =
        coreSkillGroupListToAttributeStats coreSkillGroupList, coreSkillGroupListToSkillStats coreSkillGroupList

module Vocation =
    open ZeroToFour
    open Attribute
    open Dice

    type GoverningAttribute =
        { isGoverning: bool
          attributeStat: AttributeStat }

    let collectGovernedAttributes governingAttributes =
        List.collect
            (fun governingAttribute ->
                if governingAttribute.isGoverning then
                    [ governingAttribute.attributeStat.attribute ]
                else
                    [])
            governingAttributes

    type Vocation =
        { name: string
          level: ZeroToFour
          governingAttributes: GoverningAttribute list
          dicePool: DicePool }

    let governingAttributesToDicePoolMod governingAttributes =
        governingAttributes
        |> List.filter (fun governingAttribute -> governingAttribute.isGoverning)
        |> List.map (fun governingAttribute -> neg1To4ToD6DicePoolMod governingAttribute.attributeStat.lvl)

    let vocationToDicePool baseDice level governingAttributes skillAdjustmentDiceModList =
        let diceModList =
            (governingAttributesToDicePoolMod governingAttributes)
            @ [ zeroToFourToDicePoolMod level ]
              @ skillAdjustmentDiceModList

        modifyDicePoolByDicePoolModList baseDice diceModList

    let attributesToGoverningAttributesInit attributes =
        List.map
            (fun (attributeStat: AttributeStat) ->
                { attributeStat = attributeStat
                  isGoverning = false })
            attributes

    let attributesToGoverningAttributes attributeStats oldGoverningAttributes =

        attributeStats
        |> List.map (fun attributeStat ->
            { attributeStat = attributeStat
              isGoverning =
                List.collect
                    (fun (oldGoverningAttribute: GoverningAttribute) ->
                        if (oldGoverningAttribute.attributeStat.attribute = attributeStat.attribute)
                           && oldGoverningAttribute.isGoverning then
                            [ oldGoverningAttribute.isGoverning ]
                        else
                            [])
                    oldGoverningAttributes
                |> List.isEmpty
                |> not })

module VocationGroup =

    open Neg1To4
    open ZeroToFour
    open SkillStat
    open Vocation
    open Dice
    open AttributeDeterminedDiceModEffect

    type VocationGroup =
        { vocation: Vocation
          vocationalSkills: SkillStat list }

    let findVocationalSkillLvlWithDefault skillName (defaultLvl: Neg1To4) (skillStatList: SkillStat list) =
        skillStatList
        |> List.filter (fun skill -> skill.name = skillName)
        |> (fun (list: SkillStat list) ->
            if list.Length = 0 then
                defaultLvl
            else
                List.maxBy (fun (skill: SkillStat) -> skill.lvl) list
                |> (fun skillList -> skillList.lvl))

    let findVocationalSkillLvl
        (vocationGroupList: VocationGroup list)
        (vocationalSkillName: string)
        (defaultLvl: Neg1To4)
        : Neg1To4 =

        vocationGroupList
        |> List.collect (fun vocation -> vocation.vocationalSkills)
        |> findVocationalSkillLvlWithDefault vocationalSkillName defaultLvl

    let zeroToFourToNegOneToFour zeroToFour =
        match zeroToFour with
        | Zero -> Neg1To4.Zero
        | One -> Neg1To4.One
        | Two -> Neg1To4.Two
        | Three -> Neg1To4.Three
        | Four -> Neg1To4.Four

    let vocationalSkillToDicePool
        baseDice
        level
        governingAttributes
        skillAdjustmentDiceModList
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        =

        let diceModList =
            (governingAttributesToDicePoolMod governingAttributes)
            @ [ neg1To4ToD6DicePoolMod level ]
              @ skillAdjustmentDiceModList
                @ collectAttributeDeterminedDiceMod
                    (collectGovernedAttributes governingAttributes)
                    attributeDeterminedDiceModEffectList

        modifyDicePoolByDicePoolModList baseDice diceModList

module CombatRoll =
    open Dice
    open DamageType
    open CalculatedAOE
    open Range
    open EngageableOpponents
    open Penetration
    open Attribute
    open Neg1To4
    open Equipment
    open VocationGroup
    open WeaponResourceClass
    open ItemTier
    open Item
    open WeaponClass

    open SkillStat
    open MagicResourceForCombatCalculation
    open MagicSkill
    open MagicCombat
    open ConduitClass
    open AttributeDeterminedDiceModEffect

    type CombatRoll =
        { name: string
          dicePool: DicePool
          calculatedRange: CalculatedRange
          penetration: Penetration
          damageTypes: DamageType list
          areaOfEffectShape: CalculatedAOE option
          engageableOpponents: CalculatedEngageableOpponents }

    // Weapon CombatRoll
    let createWeaponCombatRoll
        name
        (weaponClass: WeaponClass)
        weaponTierBaseDice
        attributeDeterminedDiceModArray
        attributeStats
        skillStatLvl
        (combatRollGoverningAttributes: Attribute list)
        resource
        descSuffix
        wieldingDiceMods
        : CombatRoll =

        let (resourceDesc, resourceDice, resourcePenetration, resourceRange, resourceDamageTypes, resourceAreaOfEffect) =
            weaponResourceClassOptionToWeaponResourceClass resource

        let dicePool =
            collectAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray // These are injuries, weight penalties, ect...
            |> List.append wieldingDiceMods
            |> List.append [ attributeStats
                             |> sumAttributesD6DiceMods combatRollGoverningAttributes
                             skillStatLvl |> neg1To4ToInt |> intToD6DicePoolMod
                             resourceDice ]
            |> modifyDicePoolByDicePoolModList weaponTierBaseDice

        let numDice = dicePoolToNumDice dicePool

        { name = name + resourceDesc + descSuffix
          dicePool = dicePool
          calculatedRange = determineGreatestRange numDice weaponClass.range resourceRange
          penetration = weaponClass.penetration + resourcePenetration
          damageTypes = List.append weaponClass.damageTypes resourceDamageTypes
          areaOfEffectShape = compareAndDetermineAOEShapeOption numDice weaponClass.areaOfEffect resourceAreaOfEffect
          engageableOpponents = determineEngageableOpponents numDice weaponClass.engageableOpponents }

    let createHandedVariationsWeaponCombatRolls
        (twoHandedWeaponDice: DicePoolMod)
        (oneHandedWeaponDiceOption: DicePoolMod Option)
        (dualWieldableBonusOption: DicePoolMod Option)
        preloadedCreateCombatRoll
        : CombatRoll list =

        let twoHandedCombat =
            preloadedCreateCombatRoll " (Two-handed)" [ twoHandedWeaponDice ]

        match oneHandedWeaponDiceOption with
        | None -> [ twoHandedCombat ]
        | Some oneHandedWeaponDice ->

            let oneHandedCombat =
                preloadedCreateCombatRoll " (One-handed)" [ oneHandedWeaponDice ]

            // If two and one handed have the same dice roll, then just return one handed to not clutter UI
            let handedVariations =
                if oneHandedWeaponDice = twoHandedWeaponDice then
                    [ oneHandedCombat ]
                else
                    [ twoHandedCombat; oneHandedCombat ]

            match dualWieldableBonusOption with
            | None -> handedVariations
            | Some dualWieldableBonus ->
                preloadedCreateCombatRoll
                    " (Dual-wielded)"
                    [ oneHandedWeaponDice
                      dualWieldableBonus ]
                |> List.singleton
                |> List.append handedVariations

    let createWeaponCombatRolls
        equipmentList
        (attributeStats: AttributeStat list)
        (vocationGroupList: VocationGroup list)
        attributeDeterminedDiceModArray
        (combatRollGoverningAttributes: Attribute list)
        : list<CombatRoll> =

        equipmentList
        |> getEquipedItems
        |> List.collect (fun weaponItem ->
            weaponItem
            |> itemToWeaponClasses
            |> List.collect (fun weaponClass ->

                let preloadedCreateCombatRoll =
                    createWeaponCombatRoll
                        weaponItem.name
                        weaponClass
                        weaponItem.itemTier.baseDice
                        attributeDeterminedDiceModArray
                        attributeStats
                        (findVocationalSkillLvl vocationGroupList weaponClass.name Zero)
                        combatRollGoverningAttributes

                let preloadedCreateHandVariationsCombatRolls =
                    createHandedVariationsWeaponCombatRolls
                        weaponClass.twoHandedWeaponDice
                        weaponClass.oneHandedWeaponDice
                        weaponClass.dualWieldableBonus

                match weaponClass.resourceClass with
                | Some resourceClass ->
                    equipmentList
                    |> getEquipedItems
                    |> List.collect (fun item ->
                        itemToWeaponResourceClasses item
                        |> List.filter (fun weaponResourceItem -> weaponResourceItem.resourceClass = resourceClass)
                        |> List.collect (fun weaponResourceClass ->
                            Some weaponResourceClass
                            |> preloadedCreateCombatRoll
                            |> preloadedCreateHandVariationsCombatRolls))
                | None ->
                    None
                    |> preloadedCreateCombatRoll
                    |> preloadedCreateHandVariationsCombatRolls))

    // Magic CombatRoll
    let determineMagicRangedClass (rangeMap: Map<string, Range>) (lvl: int) : Range =
        match lvl with
        | n when n = 0 -> rangeMap.Item "Short"
        | n when n = 1 -> rangeMap.Item "Medium"
        | n when n = 2 -> rangeMap.Item "Extended"
        | n when n = 3 -> rangeMap.Item "Long"
        | n when n = 4 -> rangeMap.Item "Sharpshooter"
        | n when n >= 5 -> rangeMap.Item "Extreme"
        | _ -> rangeMap.Item "Close"

    let determineMagicRange (rangeMap: Map<string, Range>) (magicCombatName: string) (lvl: int) : Range =
        match magicCombatName with
        | "Melee" -> rangeMap.Item "Reach"
        | "Melee Trick" -> rangeMap.Item "Melee"
        | "Ranged Trick" -> rangeMap.Item "Short"
        | _ -> determineMagicRangedClass rangeMap lvl

    let createMagicCombatRoll
        (magicResource: MagicResourceForCombatCalculation)
        (attributeStats: AttributeStat list)
        (skillStat: SkillStat)
        (magicSkill: MagicSkill)
        (magicCombatType: MagicCombat)
        (rangeMap: Map<string, Range>)
        (attributeDeterminedDiceModArray: list<AttributeDeterminedDiceModEffect>)
        (combatRollGoverningAttributes: Attribute list)
        : CombatRoll =

        let (resourceName, resourceDice) =
            magicResourceForCombatToResourceDescAndD6DicePoolMod magicResource

        let range =
            determineMagicRange rangeMap magicCombatType.name (neg1To4ToInt skillStat.lvl)

        let diceMods =
            collectAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray
            |> List.append [ sumAttributesD6DiceMods combatRollGoverningAttributes attributeStats
                             neg1To4ToInt skillStat.lvl |> intToD6DicePoolMod
                             magicCombatType.dicePoolMod
                             resourceDice ]

        let combatRoll = modifyDicePoolByDicePoolModList baseDicePool diceMods

        let numDice = dicePoolToNumDice combatRoll

        { name = sprintf "%s %s %s" magicSkill.name magicCombatType.name resourceName
          dicePool = combatRoll
          calculatedRange = rangeToCalculatedRange numDice range
          penetration = magicCombatType.penetration
          damageTypes = magicSkill.damageTypes
          areaOfEffectShape = determineAOEShapeOption numDice magicCombatType.areaOfEffect
          engageableOpponents = determineEngageableOpponents numDice magicCombatType.engageableOpponents }

    let createMagicCombatRollWithConduit
        rangeMap
        magicResource
        attributeStats
        skillStatLvl
        (magicSkill: MagicSkill)
        (magicCombatType: MagicCombat)
        (conduit: ConduitClass)
        conduitItemDesc
        conduitTierBaseDice
        attributeDeterminedDiceModArray
        (combatRollGoverningAttributes: Attribute list)
        descSuffix
        wieldingDiceMods
        : CombatRoll =

        let (resourceDesc, resourceDice) =
            magicResourceForCombatToResourceDescAndD6DicePoolMod magicResource

        let skillStatLvlAsInt = neg1To4ToInt skillStatLvl

        let range =
            determineMagicRange rangeMap magicCombatType.name (skillStatLvlAsInt + conduit.rangeAdjustment)

        let damageTypes = List.append magicSkill.damageTypes conduit.damageTypes

        let engageableOpponents =
            match conduit.engageableOpponents with
            | Some EO -> EO
            | None -> magicCombatType.engageableOpponents

        let dicePool =
            collectAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray
            |> List.append wieldingDiceMods
            |> List.append [ attributeStats
                             |> sumAttributesD6DiceMods combatRollGoverningAttributes
                             skillStatLvlAsInt |> intToD6DicePoolMod
                             magicCombatType.dicePoolMod
                             resourceDice ]
            |> modifyDicePoolByDicePoolModList conduitTierBaseDice

        let numDice = dicePoolToNumDice dicePool

        { name =
            sprintf "%s %s with %s %s %s" magicSkill.name magicCombatType.name conduitItemDesc resourceDesc descSuffix
          dicePool = dicePool
          calculatedRange = rangeToCalculatedRange numDice range
          penetration = magicCombatType.penetration + conduit.penetration
          damageTypes = damageTypes
          areaOfEffectShape =
            compareAndDetermineAOEShapeOption numDice magicCombatType.areaOfEffect conduit.areaOfEffect
          engageableOpponents = determineEngageableOpponents numDice engageableOpponents }


    let createMagicCombatRollWithConduitHandVariations
        (conduit: ConduitClass)
        preloadedCreatMagicCombatRollWithConduit
        : CombatRoll list =

        let twoHandedCombatRoll =
            preloadedCreatMagicCombatRollWithConduit " (Two-handed)" [ conduit.twoHandedDice ]

        match conduit.oneHandedDice with
        | None -> [ twoHandedCombatRoll ]
        | Some oneHandedDice ->

            let oneHandedCombatRoll =
                preloadedCreatMagicCombatRollWithConduit " (One-handed)" [ oneHandedDice ]

            let handedVariations =
                if conduit.twoHandedDice = oneHandedDice then
                    [ oneHandedCombatRoll ]
                else
                    [ twoHandedCombatRoll
                      oneHandedCombatRoll ]


            match conduit.dualWieldableBonus with
            | None -> handedVariations
            | Some dualWieldableBonus ->

                preloadedCreatMagicCombatRollWithConduit " (Dual-wielded)" [ oneHandedDice; dualWieldableBonus ]
                |> List.singleton
                |> List.append handedVariations

    let createMagicCombatRolls
        (attributeStats: AttributeStat list)
        (vocationGroups: VocationGroup list)
        (magicSkillMap: Map<string, MagicSkill>)
        (magicCombatMap: Map<string, MagicCombat>)
        (equipment: Equipment list)
        rangeMap
        attributeDeterminedDiceModArray
        (combatRollGoverningAttributes: Attribute list)
        : list<CombatRoll> =

        let magicMapKeys = magicSkillMap.Keys |> List.ofSeq

        vocationGroups
        |> List.collect (fun vocationGroup ->
            vocationGroup.vocationalSkills
            |> List.filter (fun skillStat -> List.contains skillStat.name magicMapKeys)
            |> List.collect (fun skillStat ->
                // Indexes into tho the magicMap for the type of Magic
                let magicSkill = magicSkillMap.Item skillStat.name

                // Maps across the magicCombat types that the magic is capable of
                determineMagicCombatTypes magicSkill.isMeleeCapable skillStat.lvl (Seq.toList magicCombatMap.Values)
                |> List.collect (fun magicCombatType ->

                    let equipedConduits = getEquipedConduitItemsWithSkillName equipment skillStat.name

                    let magicResource: MagicResourceForCombatCalculation =
                        { magicResouceClass = magicSkill.resourceClass
                          numMagicResourceDice = magicCombatType.minResourceRequirment }

                    if equipedConduits.Length > 0 then
                        List.collect
                            (fun (conduitItem: Item) ->
                                itemToConduitClasses conduitItem
                                |> List.collect (fun conduitClass ->
                                    let preloadedCreatMagicCombatRollWithConduit =
                                        createMagicCombatRollWithConduit
                                            rangeMap
                                            magicResource
                                            attributeStats
                                            skillStat.lvl
                                            magicSkill
                                            magicCombatType
                                            conduitClass
                                            conduitItem.name
                                            conduitItem.itemTier.baseDice
                                            attributeDeterminedDiceModArray
                                            combatRollGoverningAttributes

                                    createMagicCombatRollWithConduitHandVariations
                                        conduitClass
                                        preloadedCreatMagicCombatRollWithConduit))
                            equipedConduits
                    else
                        createMagicCombatRoll
                            magicResource
                            attributeStats
                            skillStat
                            magicSkill
                            magicCombatType
                            rangeMap
                            attributeDeterminedDiceModArray
                            combatRollGoverningAttributes
                        |> List.singleton)))

    let createCombatRolls
        (magicSkillMap: Map<string, MagicSkill>)
        (magicCombatMap: Map<string, MagicCombat>)
        (rangeMap: Map<string, Range>)
        (combatRollGoverningAttributeList: Attribute list)
        (attributeDeterminedDiceModList: AttributeDeterminedDiceModEffect list)
        (equipmentList: Equipment list)
        (attributeStatList: AttributeStat list)
        (vocationGroupList: VocationGroup list)
        : CombatRoll list =

        List.append
            (createWeaponCombatRolls
                equipmentList
                attributeStatList
                vocationGroupList
                attributeDeterminedDiceModList
                combatRollGoverningAttributeList)
            (createMagicCombatRolls
                attributeStatList
                vocationGroupList
                magicSkillMap
                magicCombatMap
                equipmentList
                rangeMap
                attributeDeterminedDiceModList
                combatRollGoverningAttributeList)

module CarryWeightEffect =
    open Attribute
    open VocationGroup
    open Neg1To4
    open EffectForDisplay
    open CoreSkillGroup
    open AttributeDeterminedDiceModEffect

    type CarryWeightCalculation =
        { name: string
          baseWeight: uint
          governingAttribute: Attribute
          weightIncreasePerAttribute: uint
          governingSkill: string
          weightIncreasePerSkill: uint }

    type WeightClass =
        { name: string
          bottomPercent: float
          topPercent: float
          percentOfMovementSpeed: float
          attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect }

    type CarryWeightEffectForDisplay =
        { carryWeightCalculation: CarryWeightCalculation
          weightClass: WeightClass
          durationAndSource: DurationAndSource }

    let calculateCarryWeight (maxCarryWeightCalculation: CarryWeightCalculation) coreSkillGroupList =

        let (attributeStatList, coreSkillList) =
            coreSkillGroupListToAttributeStatsAndSkillStats coreSkillGroupList

        let attributeLevel =
            sumAttributesLevels [ maxCarryWeightCalculation.governingAttribute ] attributeStatList

        let skillLevel =
            findVocationalSkillLvlWithDefault maxCarryWeightCalculation.governingSkill Zero coreSkillList
            |> neg1To4ToInt

        int maxCarryWeightCalculation.baseWeight
        + (attributeLevel
           * int maxCarryWeightCalculation.weightIncreasePerAttribute)
        + (skillLevel
           * int maxCarryWeightCalculation.weightIncreasePerSkill)
        |> float

    let determineWeightClass (maxCarryWeight: float) (inventoryWeight: float) (weightClassList: WeightClass list) =

        let percentOfMaxCarryWeight = inventoryWeight / maxCarryWeight

        List.find
            (fun weightClass ->
                (weightClass.topPercent >= percentOfMaxCarryWeight)
                && (percentOfMaxCarryWeight
                    >= weightClass.bottomPercent))
            weightClassList

    let determineCarryWeightCalculationForDisplay
        (coreSkillGroupList: CoreSkillGroup list)
        (inventoryWeight: float)
        (weightClassList: WeightClass list)
        (carryWeightCalculation: CarryWeightCalculation)
        : CarryWeightEffectForDisplay =

        let maxCarryWeight = calculateCarryWeight carryWeightCalculation coreSkillGroupList

        { carryWeightCalculation = carryWeightCalculation
          weightClass = determineWeightClass maxCarryWeight inventoryWeight weightClassList
          durationAndSource =
            { duration = indefiniteStringForDuration
              source = $"{inventoryWeight}/{maxCarryWeight} lb" } }

    let carryWeightEffectForDisplayToEffectForDisplay (cwefd: CarryWeightEffectForDisplay) =
        { name = cwefd.carryWeightCalculation.name
          effect = attributeDeterminedDiceModEffectToEffectString cwefd.weightClass.attributeDeterminedDiceModEffect
          durationAndSource = cwefd.durationAndSource }

module MovementSpeedCalculation =

    open Attribute
    open Neg1To4
    open CoreSkillGroup
    open EffectForDisplay

    type MovementSpeedCalculation =
        { name: string
          baseMovementSpeed: uint
          governingAttribute: Attribute
          feetPerAttributeLvl: uint
          governingSkill: string
          feetPerSkillLvl: uint }

    type MovementSpeedEffectForDisplay =
        { movementSpeedCalculation: MovementSpeedCalculation
          movementSpeed: float
          durationAndSource: DurationAndSource }

    let calculateMovementSpeed
        percentOfMovementSpeed
        movementSpeedCalculation
        (attributeLvl: Neg1To4)
        (skillLvl: Neg1To4)
        =
        let attributeMod =
            neg1To4ToInt attributeLvl
            * int movementSpeedCalculation.feetPerAttributeLvl

        let skillMod =
            neg1To4ToInt skillLvl
            * int movementSpeedCalculation.feetPerSkillLvl

        let movementSpeed =
            (float movementSpeedCalculation.baseMovementSpeed
             + float attributeMod
             + float skillMod)
            * percentOfMovementSpeed

        if movementSpeed >= 0.0 then
            movementSpeed
        else
            0.0

    let createMovementSpeedString movementSpeedCalculation reflexLvl athleticsLvl percentOfMovementSpeed =
        let decimalPlaces = 0

        let movementSpeed =
            calculateMovementSpeed percentOfMovementSpeed movementSpeedCalculation reflexLvl athleticsLvl

        let scaledMovementSpeed = float movementSpeed * percentOfMovementSpeed
        sprintf "%s ft" (scaledMovementSpeed.ToString("F" + decimalPlaces.ToString()))

    let movementSpeedCalculationToSourceForDisplay movementSpeedCalculation =
        $"{movementSpeedCalculation.baseMovementSpeed} ft (base), +{movementSpeedCalculation.feetPerAttributeLvl} ft (per {movementSpeedCalculation.governingAttribute}), +{movementSpeedCalculation.feetPerSkillLvl} ft (per {movementSpeedCalculation.governingSkill})"

    let determineMovementSpeedEffectForDisplay
        (coreSkillGroupList: CoreSkillGroup list)
        (percentOfMovementSpeed: float)
        (movementSpeedCalculation: MovementSpeedCalculation)
        : MovementSpeedEffectForDisplay =

        let attributeLevel =
            coreSkillGroupList
            |> List.tryFind (fun coreSkillGroup ->
                coreSkillGroup.attributeStat.attribute = movementSpeedCalculation.governingAttribute)
            |> (fun attributeLevelOption ->
                match attributeLevelOption with
                | Some coreSkillGroup -> coreSkillGroup.attributeStat.lvl
                | None -> Neg1To4.Zero)


        let skillLevel =
            coreSkillGroupList
            |> List.collect (fun coreSkillGroupList -> coreSkillGroupList.coreSkillList)
            |> List.tryFind (fun skillStat -> skillStat.name = movementSpeedCalculation.governingSkill)
            |> (fun skillStatOption ->
                match skillStatOption with
                | Some skillStat -> skillStat.lvl
                | None -> Neg1To4.Zero)

        { movementSpeedCalculation = movementSpeedCalculation
          movementSpeed =
            calculateMovementSpeed percentOfMovementSpeed movementSpeedCalculation attributeLevel skillLevel
          durationAndSource =
            { duration = indefiniteStringForDuration
              source = movementSpeedCalculationToSourceForDisplay movementSpeedCalculation } }

    let movementSpeedEffectForDisplayToEffectForDisplay (msefd: MovementSpeedEffectForDisplay) =

        { name = msefd.movementSpeedCalculation.name
          effect = $"{msefd.movementSpeed} ft"
          durationAndSource = msefd.durationAndSource }


module CharacterEffect =

    open EffectForDisplay
    open SkillDiceModEffectForDisplay
    open AttributeDeterminedDiceModEffectForDisplay
    open CarryWeightEffect
    open Equipment
    open MovementSpeedCalculation

    type CharacterEffect =
        | EffectForDisplay of EffectForDisplay
        | SkillDiceModEffectForDisplay of SkillDiceModEffectForDisplay
        | AttributeDeterminedDiceModEffectForDisplay of AttributeDeterminedDiceModEffectForDisplay
        | CarryWeightEffectForDisplay of CarryWeightEffectForDisplay
        | MovementSpeedEffectForDisplay of MovementSpeedEffectForDisplay

    let findPercentageOfMovementSpeed characterEffectList =
        let fullMovementSpeedPercent = 1.00

        characterEffectList
        |> List.tryFind (fun characterEffect ->
            match characterEffect with
            | CarryWeightEffectForDisplay _ -> true
            | _ -> false)
        |> (fun carryWeightCharacterEffectForDisplayOption ->
            match carryWeightCharacterEffectForDisplayOption with
            | Some carryWeightEffectForDisplay ->
                match carryWeightEffectForDisplay with
                | CarryWeightEffectForDisplay cwefd -> cwefd.weightClass.percentOfMovementSpeed
                | _ -> fullMovementSpeedPercent
            | None -> fullMovementSpeedPercent)

    let characterEffectsToSkillDiceModEffects (characterEffectList: CharacterEffect list) =
        characterEffectList
        |> List.collect (fun characterEffect ->
            match characterEffect with
            | SkillDiceModEffectForDisplay sdmefd -> [ sdmefd.skillDiceModEffect ]
            | _ -> [])

    let collectSkillAdjustments equipmentList characterEffectList =
        equipmentListToSkillDiceModEffects equipmentList
        @ characterEffectsToSkillDiceModEffects characterEffectList

    let collectCharacterAttributeDeterminedDiceModEffects (characterEffectList: CharacterEffect list) =
        characterEffectList
        |> List.collect (fun characterEffect ->
            match characterEffect with
            | CarryWeightEffectForDisplay carryWeightEffectForDisplay ->
                [ carryWeightEffectForDisplay.weightClass.attributeDeterminedDiceModEffect ]
            | AttributeDeterminedDiceModEffectForDisplay attributeDeterminedDiceModEffectToForDisplay ->
                [ attributeDeterminedDiceModEffectToForDisplay.attributeDeterminedDiceModEffect ]
            | _ -> [])

    let collectAttributeDeterminedDiceModEffects equipmentList characterEffectList =
        equipmentToEquipedAttributeDeterminedDiceModEffects equipmentList
        @ collectCharacterAttributeDeterminedDiceModEffects characterEffectList

module Character =

    open CoreSkillGroup
    open CombatRoll
    open Equipment
    open VocationGroup
    open Container
    open ZeroToThree
    open CharacterEffect

    let calculateCharacterWeight equipmentList (containerList: Container list) =
        containerList
        |> List.filter (fun container -> container.isEquipped)
        |> sumContainerListWeight
        |> (+) (calculateEquipmentListWeight equipmentList)

    type CharacterInformation =
        { backstory: string
          notes: string
          beliefsAndMorality: string
          goalsAndAspirations: string
          disposition: string }

    type Character =
        { name: string
          characterInformation: CharacterInformation
          coreSkillGroupList: CoreSkillGroup list
          vocationGroupList: VocationGroup list
          equipmentList: Equipment list
          combatRollList: CombatRoll list
          containerList: Container list
          destinyPoints: ZeroToThree
          characterEffectList: CharacterEffect list }

    let collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects equipmentList characterEffectList =
        (collectSkillAdjustments equipmentList characterEffectList,
         collectAttributeDeterminedDiceModEffects equipmentList characterEffectList)