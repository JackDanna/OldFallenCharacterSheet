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

    let divideUintsThenCompareToMaxThenRound (numerator: uint) (divisor: uint) (maxEO: uint option) roundDown =
        let result = divideUintByUintThenRound numerator divisor roundDown

        match maxEO with
        | Some maxEO ->
            if (maxEO < result) then
                maxEO
            else
                result
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

    let stringAndMapToDamageTypeArray (damageTypeMap: Map<string, DamageType>) (damageTypesString: string) =
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

    let eoCalculationListToMap calculatedRangeList =
        List.map (fun (eoCalculation) -> eoCalculation.name, eoCalculation) calculatedRangeList
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

    let mapMaxEO input =
        if isNumeric input then
            uint input |> Some
        else
            None

    let eoMap eoCalculationMap input =
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

    let createNeg1To4 num : Neg1To4 option =
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

    let intToNeg1To4 num = defaultArg (createNeg1To4 num) NegOne

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

    type DicePoolModification =
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

    let modifyDicePool (dicePool: DicePool) (dicePoolModification: DicePoolModification) : DicePool =
        match dicePoolModification with
        | AddDice diceToAdd -> combineDicePools [ dicePool; diceToAdd ]
        | RemoveDice diceToRemove -> removeDiceFromDicePool dicePool diceToRemove

    let sumDicePool (dicePool: DicePool) =
        let { d4 = d4
              d6 = d6
              d8 = d8
              d10 = d10
              d12 = d12
              d20 = d20 } =
            dicePool

        d4 + d6 + d8 + d10 + d12 + d20

    let modifyDicePoolByModList dicePool dicePoolModifications =

        let combinedDicePoolPenalty =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | RemoveDice dicePoolPenalty -> acc + dicePoolPenalty
                    | _ -> acc)
                0u
                dicePoolModifications
            |> RemoveDice

        let combinedPositiveDicePool =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | AddDice dicePool -> combineDicePools [ acc; dicePool ]
                    | _ -> acc)
                dicePool
                dicePoolModifications

        // Does the subtractions only at the end after combining
        modifyDicePool combinedPositiveDicePool combinedDicePoolPenalty

    let intToDicePoolModification (num: int) =
        if num < 0 then
            RemoveDice(uint (abs num))
        else
            createD6DicePoolMod (uint num)

    let neg1To4ToD6DicePoolModification neg1To4 =
        neg1To4
        |> neg1To4ToInt
        |> intToDicePoolModification

    let zeroToFourToDicePoolModification zeroToFour =
        zeroToFour
        |> zeroToFourToUint
        |> createD6DicePoolMod

    let createDicePoolModification (numDiceStr: string) (diceType: string) =
        let numDice = uint numDiceStr

        match diceType with
        | "4" -> { emptyDicePool with d4 = numDice }
        | "6" -> { emptyDicePool with d6 = numDice }
        | "8" -> { emptyDicePool with d8 = numDice }
        | "10" -> { emptyDicePool with d10 = numDice }
        | "12" -> { emptyDicePool with d12 = numDice }
        | "20" -> { emptyDicePool with d20 = numDice }
        | _ -> emptyDicePool

    let stringToDicePool (str: string) =
        str.Split ", "
        |> List.ofArray
        |> List.map (fun (diceStr) ->
            let diceNumAndDiceType = diceStr.Split "d"
            createDicePoolModification diceNumAndDiceType[0] diceNumAndDiceType[1])
        |> combineDicePools

    let stringToDicePoolModification (dicePoolJSONString: string) : DicePoolModification =
        if dicePoolJSONString.Contains("+") then
            let str = dicePoolJSONString.Replace("+", "")
            AddDice <| stringToDicePool str
        elif dicePoolJSONString.Contains("-") then
            let removeDiceString = dicePoolJSONString.Replace("-", "")

            match System.UInt32.TryParse(removeDiceString) with
            | (true, result) -> RemoveDice result
            | _ -> RemoveDice 0u
        else
            RemoveDice 0u

    let stringToDicePoolModificationOption (dicePoolJSONString: string) : DicePoolModification option =
        match dicePoolJSONString with
        | "None" -> None
        | modString -> Some <| stringToDicePoolModification modString

    let dicePoolModificationToString dicePoolModification =
        match dicePoolModification with
        | RemoveDice removeDice -> $"-{uint removeDice}d"
        | AddDice addDice -> dicePoolToString addDice

module Attribute =
    open Neg1To4
    open Dice

    type Attribute = string

    type AttributeStat = { attribute: Attribute; lvl: Neg1To4 }

    let determineAttributeLvl attributeList attributeStatList =
        List.map
            (fun attributeStat ->
                if List.contains attributeStat.attribute attributeList then
                    attributeStat.lvl
                else
                    Zero)
            attributeStatList
        |> List.map neg1To4ToInt
        |> List.sum

    let determineAttributeDiceMod attributeList attributeStatList =
        determineAttributeLvl attributeList attributeStatList
        |> intToDicePoolModification

    type AttributeDeterminedDiceModEffect =
        { name: string
          attributesToEffect: Attribute list
          dicePoolModification: DicePoolModification }

    let attributeDeterminedDiceModEffectToEffectString attributeDeterminedDiceModEffect =
        let attributesString =
            String.concat "," attributeDeterminedDiceModEffect.attributesToEffect

        let dicePoolModificationString =
            dicePoolModificationToString attributeDeterminedDiceModEffect.dicePoolModification

        $"{dicePoolModificationString} {attributesString} ({attributeDeterminedDiceModEffect.name})"


    let determineAttributeDeterminedDiceMod governingAttributeOfSkill attributeDeterminedDiceModList =
        attributeDeterminedDiceModList
        |> List.filter (fun attributeDeterminedDiceMod ->
            List.exists
                (fun attribute -> List.contains attribute governingAttributeOfSkill)
                attributeDeterminedDiceMod.attributesToEffect)
        |> List.map (fun attributeDeterminedDiceMod -> attributeDeterminedDiceMod.dicePoolModification)

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

    let createRangeMap (calculatedRanges: CalculatedRange list) rangeCalculations : Map<string, Range> =
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

module Shape =
    open System

    open AreaOfEffect

    type ConeShape =
        { area: float
          distance: uint
          angle: float }

    type SphereShape = { area: float; radius: float }

    type Shape =
        | ConeToConeShape of ConeShape
        | SphereToSphereShape of SphereShape

    let shapeToString shape =
        let decimalPlaces = 1

        match shape with
        | ConeToConeShape coneShape ->
            let decimalLimitedArea = coneShape.area.ToString("F" + decimalPlaces.ToString())
            let decimalLimitedAngle = coneShape.angle.ToString("F" + decimalPlaces.ToString())

            sprintf
                "area: %s ft^2, distance: %u ft, angle: %s θ"
                decimalLimitedArea
                coneShape.distance
                decimalLimitedAngle
        | SphereToSphereShape sphereShape ->
            let decimalLimitedArea = sphereShape.area.ToString("F" + decimalPlaces.ToString())

            let decimalLimitedRadius =
                sphereShape.radius.ToString("F" + decimalPlaces.ToString())

            sprintf "area: %s ft^2, radius: %s ft" decimalLimitedArea decimalLimitedRadius

    let shapeOptionToString shapeOption =
        match shapeOption with
        | Some shape -> shapeToString shape
        | None -> ""

    let calcConeArea (distance: uint) (angle: float) : float =
        float (distance * distance)
        * Math.Tan(angle / 2.0)

    let calcConeDistance (area: uint) (angle: float) =
        uint (Math.Sqrt(float area / Math.Tan(angle / 2.)))

    let calcConeAngle (area: uint) (distance: uint) =
        2.
        * Math.Atan(Math.Sqrt(float area / float (distance * distance)))

    let calcCone (numDice: uint) : ConeShape =
        let distance = numDice * 5u
        let angle = 53.0

        { area = calcConeArea distance angle
          distance = distance
          angle = angle }

    let calcCircle (numDice: uint) : SphereShape =
        let radius: float = 2.5 * float numDice

        { area = 2.0 * Math.PI * (radius ** 2)
          radius = radius }

    let calcShape (numDice: uint) (aoe: AreaOfEffect) : Shape =
        match aoe with
        | Cone -> ConeToConeShape(calcCone numDice)
        | Sphere -> SphereToSphereShape(calcCircle numDice)

    let determineAOE numDice aoe =
        match aoe with
        | Some aoe -> Some(calcShape numDice aoe)
        | None -> None

    let compareAndDetermineAOE
        (numDice: uint)
        (aoe: AreaOfEffect option)
        (resourceAOE: AreaOfEffect option)
        : Shape option =
        match resourceAOE with
        | Some resourceAOE -> Some(calcShape numDice resourceAOE)
        | None -> determineAOE numDice aoe

module ResourceClass =
    type ResourceClass = string

// Magic

module MagicResource =

    open Dice
    open ResourceClass

    type MagicResource =
        { magicResouceClass: ResourceClass
          numMagicResourceDice: uint }

    let determineMagicResource (resource: MagicResource) =
        sprintf "( %u %s )" resource.numMagicResourceDice resource.magicResouceClass,
        createD6DicePoolMod resource.numMagicResourceDice

    type ResourcePool =
        { name: ResourceClass
          remainingResources: uint
          poolMax: uint }

module MagicSkill =
    open DamageType
    open ResourceClass

    type MagicSkill =
        { name: string
          damageTypes: DamageType list
          rangeAdjustment: int
          isMeleeCapable: bool
          magicResourceClass: ResourceClass }

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
          diceModification: DicePoolModification
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
          oneHandedDice: DicePoolModification option
          twoHandedDice: DicePoolModification
          penetration: Penetration
          rangeAdjustment: RangeAdjustment
          damageTypes: DamageType list
          engageableOpponents: EngageableOpponents option
          dualWieldableBonus: DicePoolModification option
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
          resourceDice: DicePoolModification
          penetration: Penetration
          range: Range option
          damageTypes: DamageType list
          areaOfEffect: AreaOfEffect option }

    let prepareWeaponResourceClassOptionForUse (resource: WeaponResourceClass option) =
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
          oneHandedWeaponDice: DicePoolModification option
          twoHandedWeaponDice: DicePoolModification
          penetration: Penetration
          range: Range
          damageTypes: DamageType list
          engageableOpponents: EngageableOpponents
          dualWieldableBonus: DicePoolModification option
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

module PhysicalDefenseEffect =

    type PhysicalDefenseEffect =
        { name: string
          physicalDefense: float }

    let physicalDefenseEffectToEffectString defenseClass =
        $"{defenseClass.physicalDefense} Physical Defense"

module SkillDiceModificationEffect =

    open Dice

    type SkillDiceModificationEffect =
        { name: string
          skill: string
          diceMod: DicePoolModification }

    let skillDiceModificationEffectToEffectString skillDiceModificationEffect =
        $"{dicePoolModificationToString skillDiceModificationEffect.diceMod} {skillDiceModificationEffect.skill}"

    let collectSkillAdjustmentDiceMods skillName skillAdjustmentList =
        skillAdjustmentList
        |> List.filter (fun skillAdjustment -> skillAdjustment.skill = skillName)
        |> List.map (fun skillAdjustment -> skillAdjustment.diceMod)

module AttributeStatAdjustmentEffect =

    open Attribute

    type AttributeStatAdjustmentEffect =
        { name: string
          attribute: Attribute
          adjustment: int }

    let attributeStatAdjustmentToEffectString attributeStatAdjustment =
        $"{attributeStatAdjustment.adjustment} {attributeStatAdjustment.attribute}"

module EffectForDisplay =
    open SkillDiceModificationEffect

    type DurationAndSource = { duration: string; source: string }

    type SkillDiceModificationEffectForDisplay = SkillDiceModificationEffect * DurationAndSource

    let skillDiceModificationEffectToForDisplay
        (sdme: SkillDiceModificationEffect)
        : SkillDiceModificationEffectForDisplay =
        sdme, { duration = "?"; source = "?" }

    type EffectForDisplay =
        { name: string
          effect: string
          durationAndSource: DurationAndSource }

module ItemEffect =

    open SkillDiceModificationEffect
    open AttributeStatAdjustmentEffect
    open PhysicalDefenseEffect
    open EffectForDisplay
    open Attribute

    type ItemEffect =
        | SkillDiceModificationEffect of SkillDiceModificationEffect
        | AttributeStatAdjustmentEffect of AttributeStatAdjustmentEffect
        | DefenseClass of PhysicalDefenseEffect
        | AttributeDeterminedDiceModEffect of AttributeDeterminedDiceModEffect

    let itemEffectToString itemEffect =
        match itemEffect with
        | SkillDiceModificationEffect skillDiceModificationEffect -> skillDiceModificationEffect.name
        | AttributeStatAdjustmentEffect attributeStatAdjustment -> attributeStatAdjustment.name
        | DefenseClass defenseClass -> defenseClass.name
        | AttributeDeterminedDiceModEffect addme -> addme.name

    // _ToEffectForDisplay

    let skillDiceModificationEffectToEffectForDisplay
        (skillDiceModificationEffect: SkillDiceModificationEffect)
        duration
        source
        =
        { name = skillDiceModificationEffect.name
          effect = skillDiceModificationEffectToEffectString skillDiceModificationEffect
          durationAndSource = { duration = duration; source = source } }

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

    let itemEffectToEffectForDisplay itemEffect =

        let duration = "While equiped"

        match itemEffect with
        | SkillDiceModificationEffect sdme -> skillDiceModificationEffectToEffectForDisplay sdme duration
        | AttributeStatAdjustmentEffect asa -> attributeStatAdjustmentToEffectForDisplay asa duration
        | DefenseClass dc -> defenseClassToEffectForDisplay dc duration
        | AttributeDeterminedDiceModEffect addme -> attributeDeterminedDiceModEffect addme duration

    // Collects
    let collectSkillAdjustment (itemEffect: ItemEffect) =
        match itemEffect with
        | SkillDiceModificationEffect skillAdjustment -> [ skillAdjustment ]
        | _ -> []

    let collectAttributeDeterminedDiceModEffect (itemEffect: ItemEffect) =
        match itemEffect with
        | AttributeDeterminedDiceModEffect addme -> [ addme ]
        | _ -> []

module MovementSpeedCalculation =

    open Attribute
    open Neg1To4

    type MovementSpeedCalculation =
        { name: string
          baseMovementSpeed: uint
          governingAttributes: Attribute list
          feetPerAttributeLvl: uint
          governingSkill: string
          feetPerSkillLvl: uint }

    let calculateMovementSpeed movementSpeedCalculation (attributeLvl: Neg1To4) (skillLvl: Neg1To4) =
        let attributeMod =
            neg1To4ToInt attributeLvl
            * int movementSpeedCalculation.feetPerAttributeLvl

        let skillMod =
            neg1To4ToInt skillLvl
            * int movementSpeedCalculation.feetPerSkillLvl

        match (int movementSpeedCalculation.baseMovementSpeed
               + attributeMod
               + skillMod)
            with
        | n when n >= 0 -> uint n
        | _ -> 0u

    let createMovementSpeedString movementSpeedCalculation reflexLvl athleticsLvl percentOfMovementSpeed =
        let decimalPlaces = 0

        let movementSpeed =
            calculateMovementSpeed movementSpeedCalculation reflexLvl athleticsLvl

        let scaledMovementSpeed = float movementSpeed * percentOfMovementSpeed
        sprintf "%s ft" (scaledMovementSpeed.ToString("F" + decimalPlaces.ToString()))

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

    let collectWeaponItemClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | WeaponClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let collectConduitClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ConduitClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let collectWeaponResourceItemClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | WeaponResourceClass specifiedItemClass -> [ specifiedItemClass ]
            | _ -> [])

    let collectItemNameAndContainerClasses item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ContainerClass specifiedItemClass -> [ (item.name, specifiedItemClass) ]
            | _ -> [])

    let collectContainerClassItemName item =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ContainerClass _ -> [ item.name ]
            | _ -> [])

    let collectItemEffectSubTypes (collectItemEffectSubType) (item: Item) =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ItemEffect itemEffect -> collectItemEffectSubType itemEffect
            | _ -> [])

    let collectItemNameAndEffect (item: Item) =
        item.itemClasses
        |> List.collect (fun itemClass ->
            match itemClass with
            | ItemEffect itemEffect -> [ (item.name, itemEffect) ]
            | _ -> [])

    let collectSkillAdjustments = collectItemEffectSubTypes collectSkillAdjustment

    let collectAttributeDeterminedDiceModEffects =
        collectItemEffectSubTypes collectAttributeDeterminedDiceModEffect

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
            |> collectConduitClasses
            |> List.filter (fun conduitClass -> doesConduitEffectMagicSkill conduitClass skillName)
            |> List.isEmpty
            |> not)

    let collectEquipmentSkillAdjustments equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect collectSkillAdjustments

    let collectEquipedEquipmentAttributeDeterminedDiceModEffects equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect collectAttributeDeterminedDiceModEffects

    let getEquipedEffectItems equipmentList =
        equipmentList
        |> getEquipedItems
        |> List.collect collectItemNameAndEffect

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

    type CoreSkillGroup =
        { attributeStat: AttributeStat
          coreSkillList: SkillStat list }

    let coreSkillToDicePool baseDice lvl attributeLvl skillAdjustmentDiceModList =

        modifyDicePoolByModList
            baseDice
            (List.append
                skillAdjustmentDiceModList
                [ neg1To4ToD6DicePoolModification lvl
                  neg1To4ToD6DicePoolModification attributeLvl ])

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

    let governingAttributesToDicePoolModification governingAttributes =
        governingAttributes
        |> List.filter (fun governingAttribute -> governingAttribute.isGoverning)
        |> List.map (fun governingAttribute -> neg1To4ToD6DicePoolModification governingAttribute.attributeStat.lvl)

    let vocationToDicePool baseDice level governingAttributes skillAdjustmentDiceModList =
        let diceModList =
            (governingAttributesToDicePoolModification governingAttributes)
            @ [ zeroToFourToDicePoolModification level ]
              @ skillAdjustmentDiceModList

        modifyDicePoolByModList baseDice diceModList

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
    open Attribute

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
            (governingAttributesToDicePoolModification governingAttributes)
            @ [ neg1To4ToD6DicePoolModification level ]
              @ skillAdjustmentDiceModList
                @ determineAttributeDeterminedDiceMod
                    (collectGovernedAttributes governingAttributes)
                    attributeDeterminedDiceModEffectList

        modifyDicePoolByModList baseDice diceModList

module CombatRoll =
    open Dice
    open DamageType
    open Shape
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
    open MagicResource
    open MagicSkill
    open MagicCombat
    open ConduitClass

    type CombatRoll =
        { name: string
          dicePool: DicePool
          calculatedRange: CalculatedRange
          penetration: Penetration
          damageTypes: DamageType list
          areaOfEffectShape: Shape option
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
            prepareWeaponResourceClassOptionForUse resource

        let dicePool =
            determineAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray // These are injuries, weight penalties, ect...
            |> List.append wieldingDiceMods
            |> List.append [ attributeStats
                             |> determineAttributeDiceMod combatRollGoverningAttributes
                             skillStatLvl
                             |> neg1To4ToInt
                             |> intToDicePoolModification
                             resourceDice ]
            |> modifyDicePoolByModList weaponTierBaseDice

        let numDice = sumDicePool dicePool

        { name = name + resourceDesc + descSuffix
          dicePool = dicePool
          calculatedRange = determineGreatestRange numDice weaponClass.range resourceRange
          penetration = weaponClass.penetration + resourcePenetration
          damageTypes = List.append weaponClass.damageTypes resourceDamageTypes
          areaOfEffectShape = compareAndDetermineAOE numDice weaponClass.areaOfEffect resourceAreaOfEffect
          engageableOpponents = determineEngageableOpponents numDice weaponClass.engageableOpponents }

    let createHandedVariationsWeaponCombatRolls
        (twoHandedWeaponDice: DicePoolModification)
        (oneHandedWeaponDiceOption: DicePoolModification Option)
        (dualWieldableBonusOption: DicePoolModification Option)
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
            |> collectWeaponItemClasses
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
                        collectWeaponResourceItemClasses item
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
        (magicResource: MagicResource)
        (attributeStats: AttributeStat list)
        (skillStat: SkillStat)
        (magicSkill: MagicSkill)
        (magicCombatType: MagicCombat)
        (rangeMap: Map<string, Range>)
        (attributeDeterminedDiceModArray: list<AttributeDeterminedDiceModEffect>)
        (combatRollGoverningAttributes: Attribute list)
        : CombatRoll =

        let (resourceName, resourceDice) = determineMagicResource magicResource

        let range =
            determineMagicRange rangeMap magicCombatType.name (neg1To4ToInt skillStat.lvl)

        let diceMods =
            determineAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray
            |> List.append [ determineAttributeDiceMod combatRollGoverningAttributes attributeStats
                             neg1To4ToInt skillStat.lvl
                             |> intToDicePoolModification
                             magicCombatType.diceModification
                             resourceDice ]

        let combatRoll = modifyDicePoolByModList baseDicePool diceMods

        let numDice = sumDicePool combatRoll

        { name = sprintf "%s %s %s" magicSkill.name magicCombatType.name resourceName
          dicePool = combatRoll
          calculatedRange = rangeToCalculatedRange numDice range
          penetration = magicCombatType.penetration
          damageTypes = magicSkill.damageTypes
          areaOfEffectShape = determineAOE numDice magicCombatType.areaOfEffect
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

        let (resourceDesc, resourceDice) = determineMagicResource magicResource

        let skillStatLvlAsInt = neg1To4ToInt skillStatLvl

        let range =
            determineMagicRange rangeMap magicCombatType.name (skillStatLvlAsInt + conduit.rangeAdjustment)

        let damageTypes = List.append magicSkill.damageTypes conduit.damageTypes

        let engageableOpponents =
            match conduit.engageableOpponents with
            | Some EO -> EO
            | None -> magicCombatType.engageableOpponents

        let dicePool =
            determineAttributeDeterminedDiceMod combatRollGoverningAttributes attributeDeterminedDiceModArray
            |> List.append wieldingDiceMods
            |> List.append [ attributeStats
                             |> determineAttributeDiceMod combatRollGoverningAttributes
                             skillStatLvlAsInt |> intToDicePoolModification
                             magicCombatType.diceModification
                             resourceDice ]
            |> modifyDicePoolByModList conduitTierBaseDice

        let numDice = sumDicePool dicePool

        { name =
            sprintf "%s %s with %s %s %s" magicSkill.name magicCombatType.name conduitItemDesc resourceDesc descSuffix
          dicePool = dicePool
          calculatedRange = rangeToCalculatedRange numDice range
          penetration = magicCombatType.penetration + conduit.penetration
          damageTypes = damageTypes
          areaOfEffectShape = compareAndDetermineAOE numDice magicCombatType.areaOfEffect conduit.areaOfEffect
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

                    let magicResource: MagicResource =
                        { magicResouceClass = magicSkill.magicResourceClass
                          numMagicResourceDice = magicCombatType.minResourceRequirment }

                    if equipedConduits.Length > 0 then
                        List.collect
                            (fun (conduitItem: Item) ->
                                collectConduitClasses conduitItem
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
          attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect
          durationAndSource: DurationAndSource }

    let calculateCarryWeight (maxCarryWeightCalculation: CarryWeightCalculation) coreSkillGroupList =

        let (attributeStatList, coreSkillList) =
            coreSkillGroupListToAttributeStatsAndSkillStats coreSkillGroupList

        let attributeLevel =
            determineAttributeLvl [ maxCarryWeightCalculation.governingAttribute ] attributeStatList

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

        let weightClass =
            determineWeightClass maxCarryWeight inventoryWeight weightClassList

        let durationAndSource =
            { duration = "Indefinite"
              source = $"{inventoryWeight}/{maxCarryWeight} lb" }

        { carryWeightCalculation = carryWeightCalculation
          attributeDeterminedDiceModEffect = weightClass.attributeDeterminedDiceModEffect
          durationAndSource = durationAndSource }

module CharacterEffect =

    open EffectForDisplay
    open CarryWeightEffect
    open Equipment

    type CharacterEffect =
        | EffectForDisplay of EffectForDisplay
        | SkillDiceModificationEffectForDisplay of SkillDiceModificationEffectForDisplay
        | CarryWeightEffectForDisplay of CarryWeightEffectForDisplay

    let collectCharacterSkillDiceModifications (characterEffectList: CharacterEffect list) =
        characterEffectList
        |> List.collect (fun characterEffect ->
            match characterEffect with
            | SkillDiceModificationEffectForDisplay sdmefd ->
                let (skillDiceModificationEffect, duratoinAndSource) = sdmefd
                [ skillDiceModificationEffect ]
            | _ -> [])

    let collectSkillAdjustments equipmentList characterEffectList =
        collectEquipmentSkillAdjustments equipmentList
        @ collectCharacterSkillDiceModifications characterEffectList

    let collectCharacterAttributeDeterminedDiceModEffects (characterEffectList: CharacterEffect list) =
        characterEffectList
        |> List.collect (fun characterEffect ->
            match characterEffect with
            | CarryWeightEffectForDisplay carryWeightEffectForDisplay ->
                [ carryWeightEffectForDisplay.attributeDeterminedDiceModEffect ]
            | _ -> [])

    let collectAttributeDeterminedDiceModEffects equipmentList characterEffectList =
        collectEquipedEquipmentAttributeDeterminedDiceModEffects equipmentList
        @ collectCharacterAttributeDeterminedDiceModEffects characterEffectList

module Character =

    open CoreSkillGroup
    open CombatRoll
    open Equipment
    open VocationGroup
    open Container
    open ZeroToThree
    open CharacterEffect

    let calculateCharacterWeight equipmentList containerList =
        calculateEquipmentListWeight equipmentList
        + sumContainerListWeight containerList

    type Character =
        { name: string
          coreSkillGroupList: CoreSkillGroup list
          vocationGroupList: VocationGroup list
          equipmentList: Equipment list
          combatRollList: CombatRoll list
          containerList: Container list
          destinyPoints: ZeroToThree
          characterEffectList: CharacterEffect list }