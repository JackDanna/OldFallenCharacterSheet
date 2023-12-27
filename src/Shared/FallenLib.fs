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

    let stringAndMapToDamageTypeList (damageTypeMap: Map<string, DamageType>) (damageTypesString: string) =
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

    let dicePoolModToInt dicePoolMod =
        match dicePoolMod with
        | AddDice dicePool -> dicePoolToNumDice dicePool |> int
        | RemoveDice dicePoolPenalty -> int dicePoolPenalty * -1

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

    type AttributeName = string

    type Attribute = { name: AttributeName; level: Neg1To4 }

    let attributesToAttributeNames attributes =
        List.map (fun (attribute: Attribute) -> attribute.name) attributes

    let sumAttributesLevels attributeNameList attributeList =
        attributeList
        |> List.map (fun attributeStat ->
            if List.contains attributeStat.name attributeNameList then
                neg1To4ToInt attributeStat.level
            else
                0)
        |> List.sum

    let sumAttributesD6DiceMods attributeList attributeStatList =
        sumAttributesLevels attributeList attributeStatList
        |> intToD6DicePoolMod

    let governingAttributesToDicePoolMod (attributes: Attribute list) (governingAttributes: AttributeName list) =
        attributes
        |> List.filter (fun attribute -> (List.contains attribute.name governingAttributes))
        |> List.map (fun governingAttribute -> neg1To4ToD6DicePoolMod governingAttribute.level)

module Skill =
    open Neg1To4

    type SkillName = string

    type Skill = { name: SkillName; level: Neg1To4 }

    let findSkillLvlWithDefault skillName (defaultLvl: Neg1To4) (skillList: Skill list) =
        skillList
        |> List.filter (fun skill -> skill.name = skillName)
        |> (fun (list: Skill list) ->
            if list.Length = 0 then
                defaultLvl
            else
                List.maxBy (fun (skill: Skill) -> skill.level) list
                |> (fun skillList -> skillList.level))

module CoreSkill =
    open Skill
    open Attribute

    type CoreSkill =
        { skill: Skill
          governingAttribute: AttributeName }

module VocationalSkill =
    open Skill
    open Attribute

    type VocationalSkill =
        { skill: Skill
          governingAttributes: AttributeName list }

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

// Item Stats

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

// Effects

module AttributeDeterminedDiceModEffect =
    open Attribute
    open Dice

    type AttributeDeterminedDiceModEffect =
        { name: string
          attributesToEffect: AttributeName list
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

module AttributeStatAdjustmentEffect =

    open Attribute

    type AttributeStatAdjustmentEffect =
        { name: string
          attribute: AttributeName
          adjustment: int }

    let attributeStatAdjustmentToEffectString attributeStatAdjustment =
        $"{attributeStatAdjustment.adjustment} {attributeStatAdjustment.attribute}"

module MovementSpeedEffect =

    open Attribute
    open Neg1To4
    open CoreSkill
    open SkillDiceModEffect
    open AttributeDeterminedDiceModEffect
    open Dice

    type MovementSpeedCalculation =
        { name: string
          baseMovementSpeed: uint
          governingAttribute: AttributeName
          feetPerAttributeLvl: uint
          governingSkill: string
          feetPerSkillLvl: uint }

    type CoreSkillAndAttributeData =
        { coreSkillList: CoreSkill list
          attributeList: Attribute list
          skillDiceModEffectList: SkillDiceModEffect list
          attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list }

    let calculateMovementSpeed (coreSkillAndAttributeData: CoreSkillAndAttributeData) movementSpeedCalculation =

        let attributeMod =
            coreSkillAndAttributeData.attributeList
            |> List.tryFind (fun attribute -> attribute.name = movementSpeedCalculation.governingAttribute)
            |> (fun attributeLevelOption ->
                match attributeLevelOption with
                | Some attribute ->
                    neg1To4ToInt attribute.level
                    * int movementSpeedCalculation.feetPerAttributeLvl
                | None -> 0)


        let coreSkillMod =
            coreSkillAndAttributeData.coreSkillList
            |> List.tryFind (fun coreSkill -> coreSkill.skill.name = movementSpeedCalculation.governingSkill)
            |> (fun coreSkillOption ->
                match coreSkillOption with
                | Some coreSkill ->
                    neg1To4ToInt coreSkill.skill.level
                    * int movementSpeedCalculation.feetPerSkillLvl
                | None -> 0)

        let skillDiceModInt =
            coreSkillAndAttributeData.skillDiceModEffectList
            |> collectSkillAdjustmentDiceMods movementSpeedCalculation.governingSkill
            |> List.map dicePoolModToInt
            |> List.sum

        let attributeDeterminedDiceModInt =
            coreSkillAndAttributeData.attributeDeterminedDiceModEffectList
            |> collectAttributeDeterminedDiceMod [ movementSpeedCalculation.governingAttribute ]
            |> List.map dicePoolModToInt
            |> List.sum

        let movementSpeed =
            List.sum [ int movementSpeedCalculation.baseMovementSpeed
                       attributeMod
                       coreSkillMod
                       skillDiceModInt
                       attributeDeterminedDiceModInt ]

        if movementSpeed >= 0 then
            movementSpeed
        else
            0

    let movementSpeedCalculationToSourceForDisplay movementSpeedCalculation =
        $"{movementSpeedCalculation.baseMovementSpeed} ft (base), +{movementSpeedCalculation.feetPerAttributeLvl} ft (per {movementSpeedCalculation.governingAttribute}), +{movementSpeedCalculation.feetPerSkillLvl} ft (per {movementSpeedCalculation.governingSkill})"

module Effect =
    open SkillDiceModEffect
    open AttributeStatAdjustmentEffect
    open PhysicalDefenseEffect
    open AttributeDeterminedDiceModEffect
    open MovementSpeedEffect

    type Effect =
        | SkillDiceModEffect of SkillDiceModEffect
        | AttributeStatAdjustmentEffect of AttributeStatAdjustmentEffect
        | PhysicalDefenseEffect of PhysicalDefenseEffect
        | AttributeDeterminedDiceModEffect of AttributeDeterminedDiceModEffect
        | MovementSpeedCalculation of MovementSpeedCalculation

    let effectToEffectName effect =
        match effect with
        | SkillDiceModEffect skillDiceModEffect -> skillDiceModEffect.name
        | AttributeStatAdjustmentEffect attributeStatAdjustment -> attributeStatAdjustment.name
        | PhysicalDefenseEffect defenseClass -> defenseClass.name
        | AttributeDeterminedDiceModEffect addme -> addme.name
        | MovementSpeedCalculation msc -> msc.name

    let effectToSkillDiceModEffectList (effect: Effect) =
        match effect with
        | SkillDiceModEffect skillAdjustment -> [ skillAdjustment ]
        | _ -> []

    let effectToAttributeDeterminedDiceModEffectList (effect: Effect) =
        match effect with
        | AttributeDeterminedDiceModEffect addme -> [ addme ]
        | _ -> []

// Character Stats

module CoreSkillDicePool =
    open AttributeDeterminedDiceModEffect
    open SkillDiceModEffect
    open Attribute
    open CoreSkill
    open Dice

    let calculateCoreSkillDicePool
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (coreSkill: CoreSkill)
        : DicePool =

        let attribute =
            attributeList
            |> List.find (fun (attribute: Attribute) -> attribute.name = coreSkill.governingAttribute)

        modifyDicePoolByDicePoolModList
            baseDicePool
            ([ coreSkill.skill.level |> neg1To4ToD6DicePoolMod
               attribute.level |> neg1To4ToD6DicePoolMod ]
             @ (skillDiceModEffectList
                |> collectSkillAdjustmentDiceMods coreSkill.skill.name)
               @ (attributeDeterminedDiceModEffectList
                  |> collectAttributeDeterminedDiceMod [ attribute.name ]))

    let calculateCoreSkillDicePoolList
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (coreSkillList: CoreSkill list)
        : DicePool List =

        coreSkillList
        |> List.map (fun coreSkill ->
            calculateCoreSkillDicePool
                skillDiceModEffectList
                attributeDeterminedDiceModEffectList
                attributeList
                coreSkill)

module VocationalSkillDicePool =
    open SkillDiceModEffect
    open AttributeDeterminedDiceModEffect
    open Attribute
    open Dice
    open VocationalSkill

    let calculateVocationalSkillDicePool
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocationalSkill: VocationalSkill)
        : DicePool =

        modifyDicePoolByDicePoolModList
            baseDicePool
            ([ vocationalSkill.skill.level
               |> neg1To4ToD6DicePoolMod ]
             @ (vocationalSkill.governingAttributes
                |> governingAttributesToDicePoolMod attributeList)
               @ (skillDiceModEffectList
                  |> collectSkillAdjustmentDiceMods vocationalSkill.skill.name)
                 @ (attributeDeterminedDiceModEffectList
                    |> collectAttributeDeterminedDiceMod vocationalSkill.governingAttributes))

module Vocation =
    open ZeroToFour
    open Attribute
    open Dice
    open VocationalSkill
    open VocationalSkillDicePool

    open SkillDiceModEffect
    open AttributeDeterminedDiceModEffect

    type Vocation =
        { name: string
          level: ZeroToFour
          governingAttributes: AttributeName list
          vocationalSkills: VocationalSkill list }

    type VocationDicePool =
        { dicePool: DicePool
          vocationalSkillDicePools: DicePool list }

    type StringifiedVocationDicePool =
        { dicePoolString: string
          vocationalSkillDicePoolStrings: string list }

    let vocationToDicePool
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocation: Vocation)
        =

        modifyDicePoolByDicePoolModList
            baseDicePool
            ((governingAttributesToDicePoolMod attributeList vocation.governingAttributes)
             @ (vocation.level
                |> zeroToFourToDicePoolMod
                |> List.singleton)
               @ (skillDiceModEffectList
                  |> collectSkillAdjustmentDiceMods vocation.name)
                 @ (attributeDeterminedDiceModEffectList
                    |> collectAttributeDeterminedDiceMod vocation.governingAttributes))

    let vocationToVocationDicePool
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocation: Vocation)
        =

        { dicePool =
            vocationToDicePool skillDiceModEffectList attributeDeterminedDiceModEffectList attributeList vocation
          vocationalSkillDicePools =
            List.map
                (fun vocationalSkill ->
                    calculateVocationalSkillDicePool
                        skillDiceModEffectList
                        attributeDeterminedDiceModEffectList
                        attributeList
                        vocationalSkill)
                vocation.vocationalSkills }

    let vocationListToVocationDicePoolList
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocationList: Vocation List)
        =

        List.map
            (vocationToVocationDicePool skillDiceModEffectList attributeDeterminedDiceModEffectList attributeList)
            vocationList

    let vocationDicePoolToStringifiedVocationDicePool
        (vocationDicePool: VocationDicePool)
        : StringifiedVocationDicePool =
        { dicePoolString = dicePoolToString vocationDicePool.dicePool
          vocationalSkillDicePoolStrings = List.map dicePoolToString vocationDicePool.vocationalSkillDicePools }

    let vocationDicePoolListToStringifiedVocationDicePoolList vocationDicePoolList =
        List.map vocationDicePoolToStringifiedVocationDicePool vocationDicePoolList

    let vocationToStringifiedVocationDicePool
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocation: Vocation)
        =

        vocation
        |> vocationToVocationDicePool skillDiceModEffectList attributeDeterminedDiceModEffectList attributeList
        |> vocationDicePoolToStringifiedVocationDicePool

    let vocationListToStringifiedVocationDicePoolList
        (skillDiceModEffectList: SkillDiceModEffect list)
        (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
        (attributeList: Attribute list)
        (vocationList: Vocation list)
        =
        List.map
            (vocationToStringifiedVocationDicePool
                skillDiceModEffectList
                attributeDeterminedDiceModEffectList
                attributeList)
            vocationList


module VocationGroup =

    open Neg1To4
    open ZeroToFour
    open Vocation
    open VocationalSkill

    let findVocationalSkillLvlGivenVocationalSkills
        skillName
        (defaultLvl: Neg1To4)
        (vocationalSkillList: VocationalSkill list)
        =

        vocationalSkillList
        |> List.filter (fun vocationalSkill -> vocationalSkill.skill.name = skillName)
        |> (fun vocationalSkillList ->
            if vocationalSkillList.Length = 0 then
                defaultLvl
            else
                List.maxBy (fun vocationalSkill -> vocationalSkill.skill.level) vocationalSkillList
                |> (fun vocationalSkill -> vocationalSkill.skill.level))

    let findVocationalSkillLvlGivenVocations
        (vocationList: Vocation list)
        (vocationalSkillName: string)
        (defaultLvl: Neg1To4)
        : Neg1To4 =

        vocationList
        |> List.collect (fun vocation -> vocation.vocationalSkills)
        |> findVocationalSkillLvlGivenVocationalSkills vocationalSkillName defaultLvl

    let zeroToFourToNegOneToFour zeroToFour =
        match zeroToFour with
        | Zero -> Neg1To4.Zero
        | One -> Neg1To4.One
        | Two -> Neg1To4.Two
        | Three -> Neg1To4.Three
        | Four -> Neg1To4.Four



module CarryWeightCalculation =
    open Attribute
    open Neg1To4
    open Skill
    open CoreSkill

    type CarryWeightCalculation =
        { name: string
          baseWeight: uint
          governingAttribute: AttributeName
          weightIncreasePerAttribute: uint
          governingSkill: string
          weightIncreasePerSkill: uint }

    let calculateCarryWeight
        (carryWeightCalculation: CarryWeightCalculation)
        attributeList
        (coreSkillList: CoreSkill list)
        =

        let attributeLevel =
            sumAttributesLevels [ carryWeightCalculation.governingAttribute ] attributeList

        let skillLevel =
            findSkillLvlWithDefault
                carryWeightCalculation.governingSkill
                Zero
                (List.map (fun coreSkill -> coreSkill.skill) coreSkillList)
            |> neg1To4ToInt

        int carryWeightCalculation.baseWeight
        + (attributeLevel
           * int carryWeightCalculation.weightIncreasePerAttribute)
        + (skillLevel
           * int carryWeightCalculation.weightIncreasePerSkill)
        |> float

module WeightClass =
    open AttributeDeterminedDiceModEffect

    type WeightClass =
        { name: string
          bottomPercent: float
          topPercent: float
          attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect }

    let determineWeightClass (maxCarryWeight: float) (inventoryWeight: float) (weightClassList: WeightClass list) =

        let percentOfMaxCarryWeight = inventoryWeight / maxCarryWeight

        List.find
            (fun weightClass ->
                (weightClass.topPercent >= percentOfMaxCarryWeight)
                && (percentOfMaxCarryWeight
                    >= weightClass.bottomPercent))
            weightClassList

// EffectForDisplay

module TextEffectForDisplay =

    type DurationAndSource = { duration: string; source: string }

    type TextEffectForDisplay =
        { name: string
          effect: string
          durationAndSource: DurationAndSource }

    let indefiniteStringForDuration = "Indefinite"

module AttributeStatAdjustmentEffectForDisplay =
    open AttributeStatAdjustmentEffect
    open TextEffectForDisplay

    type AttributeStatAdjustmentEffectForDisplay =
        { attributeStatAdjustmentEffect: AttributeStatAdjustmentEffect
          durationAndSource: DurationAndSource }

    let attributeStatAdjustmentEffectToEffectForDisplayForItem
        (attributeStatAdjustment: AttributeStatAdjustmentEffect)
        duration
        source
        =
        { name = attributeStatAdjustment.name
          effect = attributeStatAdjustmentToEffectString attributeStatAdjustment
          durationAndSource = { duration = duration; source = source } }

    let attributeStatAdjustmentEffectForDisplayToTextEffectForDisplay asdefd =
        { name = asdefd.attributeStatAdjustmentEffect.name
          effect = attributeStatAdjustmentToEffectString asdefd.attributeStatAdjustmentEffect
          durationAndSource = asdefd.durationAndSource }

module PhysicalDefenseEffectForDisplay =
    open PhysicalDefenseEffect
    open TextEffectForDisplay

    type PhysicalDefenseEffectForDisplay =
        { physicalDefenseEffect: PhysicalDefenseEffect
          durationAndSource: DurationAndSource }

    let physicalDefenseEffectToEffectForDisplay (physicalDefenseEffect: PhysicalDefenseEffect) duration source =
        { name = physicalDefenseEffect.name
          effect = physicalDefenseEffectToEffectString physicalDefenseEffect
          durationAndSource = { duration = duration; source = source } }

    let physicalDefenseEffectForDisplayToTextEffectForDisplay pdefd =
        { name = pdefd.physicalDefenseEffect.name
          effect = physicalDefenseEffectToEffectString pdefd.physicalDefenseEffect
          durationAndSource = pdefd.durationAndSource }

module AttributeDeterminedDiceModEffectForDisplay =
    open TextEffectForDisplay
    open AttributeDeterminedDiceModEffect

    type AttributeDeterminedDiceModEffectForDisplay =
        { attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect
          durationAndSource: DurationAndSource }

    let attributeDeterminedDiceModEffectToForDisplay
        (addme: AttributeDeterminedDiceModEffect)
        : AttributeDeterminedDiceModEffectForDisplay =
        { attributeDeterminedDiceModEffect = addme
          durationAndSource = { duration = "?"; source = "?" } }

    let attributeDeterminedDiceModEffectToTextEffectForDisplay (addmefd: AttributeDeterminedDiceModEffectForDisplay) =
        { name = addmefd.attributeDeterminedDiceModEffect.name
          effect = attributeDeterminedDiceModEffectToEffectString addmefd.attributeDeterminedDiceModEffect
          durationAndSource = addmefd.durationAndSource }

    let attributeDeterminedDiceModEffectToItemEffectForDisplay addme duration source =
        { name = addme.name
          effect = attributeDeterminedDiceModEffectToEffectString addme
          durationAndSource = { duration = duration; source = source } }

module SkillDiceModEffectForDisplay =
    open SkillDiceModEffect
    open TextEffectForDisplay

    type SkillDiceModEffectForDisplay =
        { skillDiceModEffect: SkillDiceModEffect
          durationAndSource: DurationAndSource }

    let skillDiceModEffectToSkillDiceModEffectForDisplay (sdme: SkillDiceModEffect) : SkillDiceModEffectForDisplay =
        { skillDiceModEffect = sdme
          durationAndSource = { duration = "?"; source = "?" } }

    let skillDiceModEffectForDisplayToTextEffectForDisplay (sdmefd: SkillDiceModEffectForDisplay) =
        { name = sdmefd.skillDiceModEffect.name
          effect = skillDiceModEffectToEffectString sdmefd.skillDiceModEffect
          durationAndSource = sdmefd.durationAndSource }

module MovementSpeedEffectForDisplay =

    open MovementSpeedEffect
    open TextEffectForDisplay
    open Attribute
    open CoreSkill
    open SkillDiceModEffect
    open AttributeDeterminedDiceModEffect

    type MovementSpeedEffectForDisplay =
        { movementSpeedCalculation: MovementSpeedCalculation
          movementSpeed: float
          durationAndSource: DurationAndSource }

    let determineMovementSpeedEffectForDisplay
        (coreSkillAndAttributeData: CoreSkillAndAttributeData)
        (movementSpeedCalculation: MovementSpeedCalculation)
        : MovementSpeedEffectForDisplay =

        { movementSpeedCalculation = movementSpeedCalculation
          movementSpeed = calculateMovementSpeed coreSkillAndAttributeData movementSpeedCalculation
          durationAndSource =
            { duration = indefiniteStringForDuration
              source = movementSpeedCalculationToSourceForDisplay movementSpeedCalculation } }

    let movementSpeedEffectForDisplayToEffectForDisplay (msefd: MovementSpeedEffectForDisplay) =

        { name = msefd.movementSpeedCalculation.name
          effect = $"{msefd.movementSpeed} ft"
          durationAndSource = msefd.durationAndSource }

module EffectForDisplay =

    open Effect

    open TextEffectForDisplay
    open SkillDiceModEffectForDisplay
    open AttributeDeterminedDiceModEffectForDisplay
    open AttributeStatAdjustmentEffectForDisplay
    open PhysicalDefenseEffectForDisplay
    open MovementSpeedEffectForDisplay
    open MovementSpeedEffect

    type EffectForDisplay =
        | TextEffectForDisplay of TextEffectForDisplay
        | AttributeStatAdjustmentEffectForDisplay of AttributeStatAdjustmentEffectForDisplay
        | SkillDiceModEffectForDisplay of SkillDiceModEffectForDisplay
        | AttributeDeterminedDiceModEffectForDisplay of AttributeDeterminedDiceModEffectForDisplay
        | PhysicalDefenseEffectForDisplay of PhysicalDefenseEffectForDisplay
        | MovementSpeedEffectForDisplay of MovementSpeedEffectForDisplay

    let effectForDiplayToName (effectForDisplay: EffectForDisplay) =
        match effectForDisplay with
        | TextEffectForDisplay tefd -> tefd.name
        | AttributeStatAdjustmentEffectForDisplay asaefd -> asaefd.attributeStatAdjustmentEffect.name
        | SkillDiceModEffectForDisplay sdmefd -> sdmefd.skillDiceModEffect.name
        | AttributeDeterminedDiceModEffectForDisplay addmefd -> addmefd.attributeDeterminedDiceModEffect.name
        | PhysicalDefenseEffectForDisplay pdefd -> pdefd.physicalDefenseEffect.name
        | MovementSpeedEffectForDisplay msefd -> msefd.movementSpeedCalculation.name

    let effectForDisplayListToSkillDiceModEffectList (characterEffectList: EffectForDisplay list) =
        characterEffectList
        |> List.collect (fun characterEffect ->
            match characterEffect with
            | SkillDiceModEffectForDisplay sdmefd -> [ sdmefd.skillDiceModEffect ]
            | _ -> [])

    let effectForDisplayListToAttributeDeterminedDiceModEffectList (characterEffectList: EffectForDisplay list) =
        characterEffectList
        |> List.collect (fun effectForDisplay ->
            match effectForDisplay with
            | AttributeDeterminedDiceModEffectForDisplay attributeDeterminedDiceModEffectToForDisplay ->
                [ attributeDeterminedDiceModEffectToForDisplay.attributeDeterminedDiceModEffect ]
            | _ -> [])

    let itemEffectToEffectForDisplay coreSkillAndAttributeData effect source =

        let durationAndSource =
            { duration = "While equiped"
              source = source }

        match effect with
        | SkillDiceModEffect sdme ->
            { skillDiceModEffect = sdme
              durationAndSource = durationAndSource }
            |> SkillDiceModEffectForDisplay
        | AttributeStatAdjustmentEffect asae ->
            { attributeStatAdjustmentEffect = asae
              durationAndSource = durationAndSource }
            |> AttributeStatAdjustmentEffectForDisplay
        | PhysicalDefenseEffect pde ->
            { physicalDefenseEffect = pde
              durationAndSource = durationAndSource }
            |> PhysicalDefenseEffectForDisplay
        | AttributeDeterminedDiceModEffect addme ->
            { attributeDeterminedDiceModEffect = addme
              durationAndSource = durationAndSource }
            |> AttributeDeterminedDiceModEffectForDisplay
        | MovementSpeedCalculation msc ->
            determineMovementSpeedEffectForDisplay coreSkillAndAttributeData msc
            |> MovementSpeedEffectForDisplay

    let effectForDisplayToTextEffectForDisplay effectForDisplay =
        match effectForDisplay with
        | TextEffectForDisplay tefd -> tefd
        | SkillDiceModEffectForDisplay sdme ->
            sdme
            |> skillDiceModEffectForDisplayToTextEffectForDisplay
        | AttributeStatAdjustmentEffectForDisplay asae ->
            asae
            |> attributeStatAdjustmentEffectForDisplayToTextEffectForDisplay
        | PhysicalDefenseEffectForDisplay pde ->
            pde
            |> physicalDefenseEffectForDisplayToTextEffectForDisplay
        | AttributeDeterminedDiceModEffectForDisplay addme ->
            addme
            |> attributeDeterminedDiceModEffectToTextEffectForDisplay
        | MovementSpeedEffectForDisplay msc -> movementSpeedEffectForDisplayToEffectForDisplay msc

// Item stuff

module Item =
    open ItemTier
    open WeaponClass
    open WeaponResourceClass
    open ConduitClass
    open ContainerClass
    open Effect

    type ItemClass =
        | WeaponClass of WeaponClass
        | ConduitClass of ConduitClass
        | WeaponResourceClass of WeaponResourceClass
        | ContainerClass of ContainerClass
        | ItemEffect of Effect

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
                | ItemEffect itemEffect -> effectToEffectName itemEffect)
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
        itemToItemEffectSubTypes effectToSkillDiceModEffectList

    let itemToAttributeDeterminedDiceModEffects =
        itemToItemEffectSubTypes effectToAttributeDeterminedDiceModEffectList

module ItemStack =
    open Item

    type ItemStack = { item: Item; quantity: uint }

    let itemStackListToItemList itemStackList =
        List.map (fun itemStack -> itemStack.item) itemStackList

    let sumItemStackListWeight (itemStackList: ItemStack list) =
        itemStackList
        |> List.map (fun itemStack -> itemStack.item.weight * (float itemStack.quantity))
        |> List.sum

module Container =
    open ContainerClass
    open ItemStack

    type Container =
        { name: string
          containerClass: ContainerClass
          isEquipped: bool
          itemStackList: ItemStack list }

    let itemNameAndContainerClassToContainer (itemName, containerClass: ContainerClass) =
        { name = itemName
          containerClass = containerClass
          isEquipped = false
          itemStackList = [] }

    let sumContainerListWeight (containerList: Container list) =
        containerList
        |> List.map (fun container -> sumItemStackListWeight container.itemStackList)
        |> List.sum

module Equipment =
    open Item
    open ItemStack
    open ConduitClass

    type Equipment =
        { isEquipped: bool
          itemStack: ItemStack }

    let calculateEquipmentListWeight equipmentList =
        equipmentList
        |> List.fold
            (fun acc (equipment: Equipment) ->
                equipment.itemStack.item.weight
                * float equipment.itemStack.quantity
                + acc)
            0.0

    let getEquipedItems equipmentList =
        equipmentList
        |> List.filter (fun equipmentItem ->
            equipmentItem.isEquipped
            && equipmentItem.itemStack.quantity > 0u)
        |> List.map (fun equipmentItem -> equipmentItem.itemStack.item)

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

    let equipmentListToEquipedEquipmentEffects equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect itemToItemNameAndItemEffectList

module EquipmentEffectForDisplay =
    open Equipment
    open EffectForDisplay

    let collectSkillAdjustments equipmentList characterEffectList =
        equipmentListToSkillDiceModEffects equipmentList
        @ effectForDisplayListToSkillDiceModEffectList characterEffectList

    let collectAttributeDeterminedDiceModEffects equipmentList characterEffectList =
        equipmentToEquipedAttributeDeterminedDiceModEffects equipmentList
        @ effectForDisplayListToAttributeDeterminedDiceModEffectList characterEffectList

    let equipmentListToEffectForDisplay coreSkillAndAttributeData equipmentList =
        equipmentList
        |> equipmentListToEquipedEquipmentEffects
        |> List.map (fun (itemName, itemEffect) ->
            itemEffectToEffectForDisplay coreSkillAndAttributeData itemEffect itemName)

// Character Stuff

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

    open MagicResourceForCombatCalculation
    open MagicSkill
    open MagicCombat
    open ConduitClass
    open AttributeDeterminedDiceModEffect
    open Vocation
    open VocationalSkill

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
        (combatRollGoverningAttributes: AttributeName list)
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
        (attributeStats: Attribute list)
        (vocationList: Vocation list)
        attributeDeterminedDiceModArray
        (combatRollGoverningAttributes: AttributeName list)
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
                        (findVocationalSkillLvlGivenVocations vocationList weaponClass.name Zero)
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
        (attributeStats: Attribute list)
        (vocationalSkill: VocationalSkill)
        (magicSkill: MagicSkill)
        (magicCombatType: MagicCombat)
        (rangeMap: Map<string, Range>)
        (attributeDeterminedDiceModArray: list<AttributeDeterminedDiceModEffect>)
        (combatRollGoverningAttributes: AttributeName list)
        : CombatRoll =

        let (resourceName, resourceDice) =
            magicResourceForCombatToResourceDescAndD6DicePoolMod magicResource

        let range =
            determineMagicRange rangeMap magicCombatType.name (neg1To4ToInt vocationalSkill.skill.level)

        let diceMods =
            collectAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray
            |> List.append [ sumAttributesD6DiceMods combatRollGoverningAttributes attributeStats
                             neg1To4ToInt vocationalSkill.skill.level
                             |> intToD6DicePoolMod
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
        (combatRollGoverningAttributes: AttributeName list)
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
        (attributeStats: Attribute list)
        (vocation: Vocation list)
        (magicSkillMap: Map<string, MagicSkill>)
        (magicCombatMap: Map<string, MagicCombat>)
        (equipment: Equipment list)
        rangeMap
        attributeDeterminedDiceModArray
        (combatRollGoverningAttributes: AttributeName list)
        : list<CombatRoll> =

        let magicMapKeys = magicSkillMap.Keys |> List.ofSeq

        vocation
        |> List.collect (fun vocationGroup ->
            vocationGroup.vocationalSkills
            |> List.filter (fun vocationalSkill -> List.contains vocationalSkill.skill.name magicMapKeys)
            |> List.collect (fun vocationalSkill ->
                // Indexes into tho the magicMap for the type of Magic
                let magicSkill = magicSkillMap.Item vocationalSkill.skill.name

                // Maps across the magicCombat types that the magic is capable of
                determineMagicCombatTypes
                    magicSkill.isMeleeCapable
                    vocationalSkill.skill.level
                    (Seq.toList magicCombatMap.Values)
                |> List.collect (fun magicCombatType ->

                    let equipedConduits =
                        getEquipedConduitItemsWithSkillName equipment vocationalSkill.skill.name

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
                                            vocationalSkill.skill.level
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
                            vocationalSkill
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
        (combatRollGoverningAttributeList: AttributeName list)
        (attributeDeterminedDiceModList: AttributeDeterminedDiceModEffect list)
        (equipmentList: Equipment list)
        (attributeStatList: Attribute list)
        (vocationList: Vocation list)
        : CombatRoll list =

        List.append
            (createWeaponCombatRolls
                equipmentList
                attributeStatList
                vocationList
                attributeDeterminedDiceModList
                combatRollGoverningAttributeList)
            (createMagicCombatRolls
                attributeStatList
                vocationList
                magicSkillMap
                magicCombatMap
                equipmentList
                rangeMap
                attributeDeterminedDiceModList
                combatRollGoverningAttributeList)

module CarryWeightStat =

    open CarryWeightCalculation
    open WeightClass

    type CarryWeightStat =
        { carryWeightCalculation: CarryWeightCalculation
          currentWeight: float
          maxWeight: float
          weightClass: WeightClass }

module Character =

    open CombatRoll
    open Equipment
    open Vocation
    open Container
    open ZeroToThree
    open EffectForDisplay
    open CarryWeightStat
    open Attribute
    open CoreSkill
    open Dice

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
          attributeList: Attribute list
          coreSkillList: CoreSkill list
          coreSkillDicePoolList: DicePool list
          vocationList: Vocation list
          vocationDicePoolList: VocationDicePool list
          equipmentList: Equipment list
          combatRollList: CombatRoll list
          containerList: Container list
          destinyPoints: ZeroToThree
          characterEffectForDisplayList: EffectForDisplay list
          equipmentEffectForDisplayList: EffectForDisplay list
          characterInformation: CharacterInformation
          carryWeightStatOption: CarryWeightStat option }

    let carryWeightStatOptionToAttributeDeterminedDiceMod (carryWeightStatOption: CarryWeightStat option) =
        match carryWeightStatOption with
        | Some carryWeightStat -> [ carryWeightStat.weightClass.attributeDeterminedDiceModEffect ]
        | _ -> []

    let collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
        equipmentEffectForDisplayList
        characterEffectForDisplayList
        carryWeightStatAttributeDeterminedDiceModList
        =
        effectForDisplayListToSkillDiceModEffectList (
            equipmentEffectForDisplayList
            @ characterEffectForDisplayList
        ),
        effectForDisplayListToAttributeDeterminedDiceModEffectList (
            equipmentEffectForDisplayList
            @ characterEffectForDisplayList
        )
        @ carryWeightStatAttributeDeterminedDiceModList