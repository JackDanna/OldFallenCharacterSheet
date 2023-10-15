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
        | true  -> Math.Floor floatCalculation   |> uint
        | false -> Math.Ceiling floatCalculation |> uint

    let divideUintsThenCompareToMaxThenRound (numerator:uint) (divisor:uint) (maxEO:uint option) roundDown =
        let result = divideUintByUintThenRound numerator divisor  roundDown
        match maxEO with
        | Some maxEO -> if (maxEO < result) then maxEO else result
        | None -> result

module TypeUtils =

    let stringArrayToTypeMap (stringTypeArray: string array) =
        Array.map ( fun (stringType:string) ->
            stringType
        ) stringTypeArray
        |> Array.zip stringTypeArray
        |> Map.ofArray

module MapUtils =
    let createMapFromTuple2List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            Map.add (fst tuple) (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple3List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple4List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple5List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple6List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple7List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple9List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_,_,_,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

    let createMapFromTuple11List (createObjectFromTupleFunc) (list) =
        list
        |> Array.fold (fun map tuple ->
            let (desc,_,_,_,_,_,_,_,_,_,_) = tuple
            Map.add desc (createObjectFromTupleFunc tuple) map
        ) Map.empty

module Penetration =
    open MathUtils
    open StringUtils

    type PenetrationCalculation = {
        desc               : string
        penetrationDivisor : uint
    }

    let tupleToPenetrationCalculation (desc, penetrationDivisor) = { 
        desc               = desc
        penetrationDivisor = penetrationDivisor
    }

    type CalculatedPenetration = uint

    type Penetration =
    | PenetrationCalculation of PenetrationCalculation
    | CalculatedPenetration  of CalculatedPenetration

    let calculatePenetration numDice penetration = 
        match penetration with
        | PenetrationCalculation calcuation -> 
            divideUintByUintThenRound numDice calcuation.penetrationDivisor true
        | CalculatedPenetration calculated  -> 
            calculated

    let determinePenetration (numDice:uint) (primaryPenetration:Penetration) (secondaryPenetration:Penetration) =
        match secondaryPenetration with
        | PenetrationCalculation secondaryCalculation -> 
            calculatePenetration numDice (PenetrationCalculation secondaryCalculation)
        | CalculatedPenetration calculatedSecondaryPenetration ->
            calculatePenetration numDice primaryPenetration
            |> (+) calculatedSecondaryPenetration

    let createPenetrationMap (penetrationCalculationMap:Map<string,PenetrationCalculation>) (str:string) =
        if isNumeric str then
            uint str
            |> CalculatedPenetration
            
        elif Map.containsKey str penetrationCalculationMap then
            penetrationCalculationMap.Item str
            |> PenetrationCalculation
        else
            CalculatedPenetration 0u

module Damage =

    type DamageType = string

    let damageTypesToString (damageTypes:DamageType []) =
        Array.map ( fun damageType ->
           damageType.ToString()
        ) damageTypes
        |> String.concat ", "

    let stringAndMapToDamageTypeArray (damageTypeMap:Map<string,DamageType>) (damageTypesString:string) =
        if damageTypesString.Length = 0 then 
            [||]
        else 
            damageTypesString.Split ", "
            |> Array.map ( fun (damageTypeString) -> 
                damageTypeMap.Item damageTypeString
            )

module EngageableOpponents = 

    open MathUtils
    open MapUtils
    open StringUtils

    type EngageableOpponentsCalculation = {
        desc              : string
        combatRollDivisor : uint
        maxEO             : uint option
    }

    type CalculatedEngageableOpponents = uint

    type EngageableOpponents = 
    | Calculation of EngageableOpponentsCalculation
    | Calculated  of CalculatedEngageableOpponents

    let determineEngageableOpponents (numDice:uint) (engageableOpponents:EngageableOpponents) : CalculatedEngageableOpponents =
        match engageableOpponents with
        | Calculated calculatedEngageableOpponents->
            calculatedEngageableOpponents
        | Calculation eoCalculation->
            divideUintsThenCompareToMaxThenRound numDice eoCalculation.combatRollDivisor eoCalculation.maxEO true

    let mapMaxEO (input:string) =
        if isNumeric input then
            uint input
            |> Some
        else
          None

    let createEngageableOpponentsCalculation tuple = 
        match tuple with
        | (desc, combatRollDivisor, maxEO) -> { desc = desc; combatRollDivisor = combatRollDivisor; maxEO = mapMaxEO maxEO }

    let createCalculatedEngageableOpponents (tuple:string * uint) : CalculatedEngageableOpponents= 
        match tuple with
        | (_, engageableOpponents) -> engageableOpponents

    let createEOCalculationMap = createMapFromTuple3List createEngageableOpponentsCalculation
    let createCalculatedEOMap  = createMapFromTuple2List createCalculatedEngageableOpponents

    let createEOInterface (calculatedEOMap:Map<string,CalculatedEngageableOpponents>) 
                    (eoCalculationMap:Map<string,EngageableOpponentsCalculation>) 
                    (input:string) =

        if Map.containsKey input calculatedEOMap then
            calculatedEOMap.Item input
            |> Calculated
        elif Map.containsKey input eoCalculationMap then
            eoCalculationMap.Item input
            |> Calculation
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

    let convert_Neg1To4_To_Int num =
        match num with
        | NegOne -> -1
        | Zero  -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4

    let intToNeg1To4 (num:int) =
        defaultArg (createNeg1To4 num) NegOne

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

module Attribute =
    open Neg1To4
    open Dice

    type Attribute = string

    type AttributeStat = {
        attribute : Attribute
        lvl       : Neg1To4
    }

    let tupleToAttributeStat (attributeMap:Map<string,Attribute>) (attribute, lvl) = {
        attribute = attributeMap.Item attribute
        lvl       = intToNeg1To4 lvl
    }

    let determineAttributeLvl (attributeArray:Attribute array) (attributeStatArray:AttributeStat array) = 
        Array.map ( fun attributeStat -> 
            if Array.contains attributeStat.attribute attributeArray then
                attributeStat.lvl
            else
                Zero
        ) attributeStatArray
        |> Array.map convert_Neg1To4_To_Int
        |> Array.sum

    let determineAttributeDiceMod (attributeArray:Attribute array) (attributeStatArray:AttributeStat array) =
        determineAttributeLvl attributeArray attributeStatArray
        |> intToDicePoolModification

    type AttributeDeterminedDiceMod = {
        name                 : string
        attributesToEffect   : Attribute array
        dicePoolModification : DicePoolModification
    }

    let determineAttributeDeterminedDiceMod governingAttributeOfSkill (attributeDeterminedDiceModArray: AttributeDeterminedDiceMod array) =
        attributeDeterminedDiceModArray
        |> Array.filter ( fun attributeDeterminedDiceMod ->
            attributeDeterminedDiceMod.attributesToEffect
            |> Array.exists ( fun attribute -> Array.contains attribute governingAttributeOfSkill)
        )
        |> Array.map ( fun attributeDeterminedDiceMod -> attributeDeterminedDiceMod.dicePoolModification)

module Range =

    open System
    open MapUtils

    type CalculatedRange = {
        desc           : string
        effectiveRange : uint
        maxRange       : uint
    }

    type RangeCalculation = {
        desc                         : string    // Description of the range
        numDicePerEffectiveRangeUnit : uint      // The amount of dice required to gain an effective Range Unit
        ftPerEffectiveRangeUnit      : uint      // The amount of ft per effective range unit
        roundEffectiveRangeUp        : bool      // If true, round up the effective range, otherwise round down
        maxRange                     : uint      // Max Range is the absolute limit of how far a attack can go
    }

    type Range = 
    | CalculatedRange  of CalculatedRange
    | RangeCalculation of RangeCalculation

    type RangeAdjustment = int

    let calculatedRangeToString calculatedRange =
        sprintf "%u/%u" calculatedRange.effectiveRange calculatedRange.maxRange

    let calculateRange (numDice:uint) (rangeCalculation:RangeCalculation): CalculatedRange =
        { desc           = rangeCalculation.desc
          effectiveRange = uint (Math.Ceiling(float numDice/float rangeCalculation.numDicePerEffectiveRangeUnit)) * rangeCalculation.ftPerEffectiveRangeUnit
          maxRange       = rangeCalculation.maxRange
        }

    let rangeToCalculatedRange (numDice:uint) (range:Range) : CalculatedRange =
        match range with
        | CalculatedRange  calculatedRange  -> calculatedRange
        | RangeCalculation rangeCalculation -> calculateRange numDice rangeCalculation

    let determineGreatestRange numDice (primaryRange:Range) (optionalRange:Range option) =
        match optionalRange with
        | Some secondaryRange ->
            let calculatedPrimaryRange   = rangeToCalculatedRange numDice primaryRange
            let calculatedSecondaryRange = rangeToCalculatedRange numDice secondaryRange
            if calculatedPrimaryRange.effectiveRange >= calculatedSecondaryRange.effectiveRange then
                calculatedPrimaryRange
            else
                calculatedSecondaryRange
        | None -> rangeToCalculatedRange numDice primaryRange

    let createPenerationFromCalculatedRange tuple = 
        match tuple with
        | (desc, effectiveRange, maxRange) -> CalculatedRange { desc = desc; effectiveRange = effectiveRange; maxRange = maxRange }

    let createPenerationFromRangeCalculation tuple = 
        match tuple with
        | (desc,numDicePerEffectiveRangeUnit,ftPerEffectiveRangeUnit, roundEffectiveRangeUp, maxRange) -> 
        { 
            desc                         = desc
            numDicePerEffectiveRangeUnit = numDicePerEffectiveRangeUnit
            ftPerEffectiveRangeUnit      = ftPerEffectiveRangeUnit
            roundEffectiveRangeUp        = roundEffectiveRangeUp
            maxRange                     = maxRange
        } |> RangeCalculation

    let createCalculatedRangeMap   = createMapFromTuple3List createPenerationFromCalculatedRange
    let createRangeCalculationMap  = createMapFromTuple5List createPenerationFromRangeCalculation

    let createRangeMap calculatedRangeData rangeCalculationData : Map<string,Range>=

        let map1  = createCalculatedRangeMap calculatedRangeData
        let map2 = createRangeCalculationMap rangeCalculationData

        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

module AreaOfEffect =
    type AreaOfEffect =
    | Cone
    | Sphere

    let AreaOfEffectOptionMap = Map [
        ("Cone",Some Cone)
        ("Sphere",Some Sphere)
        ("None", None)
    ]

module Shape = 
    open System

    open AreaOfEffect

    type ConeShape = {
        area     : float
        distance : uint
        angle    : float
    }
    
    type SphereShape = {
        area   : float
        radius : float
    }

    type Shape =
    | ConeToConeShape     of ConeShape
    | SphereToSphereShape of SphereShape

    let shapeToString shape = 
        let decimalPlaces = 1
        match shape with
        | ConeToConeShape coneShape ->
            let decimalLimitedArea  = coneShape.area.ToString("F" + decimalPlaces.ToString())
            let decimalLimitedAngle = coneShape.angle.ToString("F" + decimalPlaces.ToString())
            sprintf "area: %s ft^2, distance: %u ft, angle: %s θ" decimalLimitedArea coneShape.distance decimalLimitedAngle
        | SphereToSphereShape sphereShape ->
            let decimalLimitedArea   = sphereShape.area.ToString("F" + decimalPlaces.ToString())
            let decimalLimitedRadius = sphereShape.radius.ToString("F" + decimalPlaces.ToString())
            sprintf "area: %s ft^2, radius: %s ft" decimalLimitedArea decimalLimitedRadius

    let shapeOptionToString shapeOption =
        match shapeOption with
        | Some shape -> shapeToString shape
        | None -> ""

    let calcConeArea (distance : uint) (angle : float) : float =
        float (distance * distance) * Math.Tan( angle/2.0)

    let calcConeDistance (area : uint) (angle : float) =
        uint (Math.Sqrt(float area / Math.Tan(angle/2.)))

    let calcConeAngle (area : uint) (distance : uint) =
        2. * Math.Atan(Math.Sqrt(float area / float (distance * distance)))

    let calcCone (numDice:uint) : ConeShape =
        let distance = numDice * 5u
        let angle = 53.0
        {
            area = calcConeArea distance angle
            distance = distance
            angle = angle
        }

    let calcCircle (numDice:uint) : SphereShape =
        let radius : float = 2.5 * float numDice
        {
            area = 2.0 * Math.PI * (radius**2)
            radius = radius
        }
        
    let calcShape (numDice : uint) (aoe:AreaOfEffect) : Shape =
        match aoe with
        | Cone   -> ConeToConeShape     (calcCone numDice)
        | Sphere -> SphereToSphereShape (calcCircle numDice)

    let determineAOE numDice aoe =
        match aoe with
            | Some aoe -> Some (calcShape numDice aoe)
            | None -> None

    let compareAndDetermineAOE (numDice:uint) (aoe:AreaOfEffect option) (resourceAOE:AreaOfEffect option) : Shape option =
        match resourceAOE with
        | Some resourceAOE -> Some (calcShape numDice resourceAOE)
        | None ->
            determineAOE numDice aoe

module ResourceClass =
    type ResourceClass = string

// Item
module WeaponResourceClass =

    open Dice
    open Range
    open Damage
    open AreaOfEffect
    open ResourceClass

    type WeaponResourceClass = {
        name                : string
        resourceClass       : ResourceClass
        resourceDice        : DicePoolModification
        penetration         : uint
        range               : Range option
        damageTypes         : DamageType []
        areaOfEffect        : AreaOfEffect option
    }

    let determineResource (resource:WeaponResourceClass option) =
        match resource with
        | Some resource ->
            (
                " (" + resource.name + ")", resource.resourceDice, resource.penetration,
                resource.range, resource.damageTypes, resource.areaOfEffect
            )
        | None ->
            ("", createD6DicePoolMod(0u), 0u, None, [||], None)

module WeaponClass =
    open Dice
    open Range
    open Damage
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceClass
    open Attribute

    type WeaponClass = {
        desc                : string
        oneHandedWeaponDice : DicePoolModification option
        twoHandedWeaponDice : DicePoolModification
        penetration         : Penetration
        range               : Range
        damageTypes         : DamageType array
        engageableOpponents : EngageableOpponents
        dualWieldableBonus  : DicePoolModification option
        areaOfEffect        : AreaOfEffect option
        resourceClass       : ResourceClass option
        governingAttributes : Attribute array
    }

module ItemTier =
    open Dice

    type ItemTier = {
        desc          : string
        level         : int
        runeSlots     : uint
        baseDice      : DicePool
        durabilityMax : uint
    }

module ConduitClass =
    open Dice
    open Range
    open Damage
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceClass
    open Attribute

    type ConduitClass = {
        desc                : string
        oneHandedDice       : DicePoolModification option
        twoHandedDice       : DicePoolModification
        penetration         : Penetration
        rangeAdjustment     : RangeAdjustment
        damageTypes         : DamageType array
        engageableOpponents : EngageableOpponents option
        dualWieldableBonus  : DicePoolModification option
        areaOfEffect        : AreaOfEffect option
        resourceClass       : ResourceClass option
        governingAttributes : Attribute array
    }

module DefenseClass =

    type DefenseClass = {
        name             : string
        physicalDefense  : float
        mentalDefense    : float
        spiritualDefense : float
    }

    let tupleToDefenseClass (name, physicalDefense, mentalDefense, spiritualDefense) =
        {
            name             = name
            physicalDefense  = physicalDefense
            mentalDefense    = mentalDefense
            spiritualDefense = spiritualDefense
        }

module Item =
    open ItemTier
    open WeaponClass
    open WeaponResourceClass
    open ConduitClass
    open DefenseClass

    type ItemClass = 
    | WeaponClass of WeaponClass
    | ConduitClass of ConduitClass
    | WeaponResourceClass of WeaponResourceClass
    | DefenseClass of DefenseClass

    type Item = {
        name        : string
        itemClasses : ItemClass array
        itemTier    : ItemTier
        value       : string
        weight      : float
    }

    let collectWeaponItemClasses item =
        item.itemClasses
        |> Array.collect( fun itemClass->
            match itemClass with
            | WeaponClass specifiedItemClass -> [| specifiedItemClass |]
            | _ -> [||]
        )

    let collectConduitClasses item =
        item.itemClasses
        |> Array.collect( fun itemClass->
            match itemClass with
            | ConduitClass specifiedItemClass -> [| specifiedItemClass |]
            | _ -> [||]
        )

    let collectWeaponResourceItemClasses item =
        item.itemClasses
        |> Array.collect( fun itemClass->
            match itemClass with
            | WeaponResourceClass specifiedItemClass -> [| specifiedItemClass |]
            | _ -> [||]
        )

    let collectDefenseClasses item =
        item.itemClasses
        |> Array.collect( fun itemClass->
            match itemClass with
            | DefenseClass specifiedItemClass -> [| specifiedItemClass |]
            | _ -> [||]
        )

// Magic

module MagicResource =

    open Dice
    open ResourceClass

    type MagicResource = {
        magicResouceClass    : ResourceClass
        numMagicResourceDice : uint
    }

    let determineMagicResource (resource:MagicResource) =
        ( sprintf "( %u %s )" resource.numMagicResourceDice resource.magicResouceClass, createD6DicePoolMod resource.numMagicResourceDice )

    type ResourcePool = {
        name               : ResourceClass
        remainingResources : uint
        poolMax            : uint
    }

module MagicSkill = 
    open Damage
    open ResourceClass

    type MagicSkill = {
        desc                 : string
        damageTypes          : DamageType array
        rangeAdjustment      : int
        meleeCapable         : bool
        magicResourceClass   : ResourceClass
    }

module MagicCombat =
    open Range
    open EngageableOpponents
    open AreaOfEffect
    open Neg1To4
    open Penetration
    open Dice

    type MagicCombat = {
        desc                  : string
        lvlRequirment         : Neg1To4
        diceModification      : DicePoolModification
        penetration           : Penetration
        range                 : Range
        engageableOpponents   : EngageableOpponents
        minResourceRequirment : uint
        canVocationAssist     : bool
        areaOfEffect          : AreaOfEffect option
    }

    let isMeleeOrReachRange (magicCombatCalculatedRange:CalculatedRange) =
        match magicCombatCalculatedRange.desc with
        | "Melee" | "Reach" -> true
        | _ -> false

    let determineMagicCombatTypes (meleeCapable:bool) (lvl:Neg1To4) (magicCombatList:MagicCombat []) =
        // Logic for filtering by Lvl requirement
        Array.filter (fun (magicCombat:MagicCombat) -> 
            let skillLvl       = convert_Neg1To4_To_Int lvl
            let requirementLvl = convert_Neg1To4_To_Int magicCombat.lvlRequirment
            (skillLvl >= requirementLvl)
        ) magicCombatList
        // Logic for filtering by if it can melee
        |> Array.filter (fun (magicCombat:MagicCombat) ->
            match magicCombat.range with
            | CalculatedRange magicCombatCalculatedRange ->
                not (isMeleeOrReachRange magicCombatCalculatedRange && not meleeCapable)
            | _ -> true
        )
        |> Array.map ( fun (magicCombat:MagicCombat) -> magicCombat.desc) 

// Character

module Container =
    open Item
    type Container = {
        name            : string
        weightCapacity  : float
        weightContained : float
        itemList        : Item list
    }

module Equipment =
    open Item
    open ConduitClass

    type EquipmentItem = {
        equipped : bool
        item     : Item
        quantity : uint
    }

    let tupleToEquipmentItem (itemMap:Map<string,Item>) (equipped, desc, quantity) = { 
        equipped = equipped
        item     = itemMap.Item desc
        quantity = quantity 
    }

    let calculateEquipmentItemArrayWeight (equipmentArray:EquipmentItem array) =
        equipmentArray
        |> Array.fold ( fun acc (equipmentItem:EquipmentItem) -> 
            (equipmentItem.item.weight * float equipmentItem.quantity) + acc
        ) 0.0

    let getEquipedItems equipment =
        Array.filter (fun equipmentItem  -> equipmentItem.equipped = true && equipmentItem.quantity > 0u ) equipment
        |> Array.map (fun equipmentItem -> equipmentItem.item )

    let getEquipedConduitItemsWithSkillName equipment (skillName:string) : Item array=
        getEquipedItems equipment
        |> Array.filter ( fun (item) ->
            item
            |> collectConduitClasses
            |> Array.filter ( fun conduitClass -> conduitClass.desc.Contains skillName )
            |> Array.isEmpty
            |> not
        )

module SkillStat = 
    open Neg1To4
    open Attribute

    type SkillStat = {
        name               : string
        lvl                : Neg1To4
        governingAttributes : Attribute array
    }

    let initSkillStat = {
        name = "NULL"
        lvl = NegOne
        governingAttributes = [||]
    }

    let tupleToSkillStat (attributeMap:Map<string,Attribute>) (name, lvl, governingAttribute) = { 
        name               = name
        lvl                = intToNeg1To4  lvl
        governingAttributes = attributeMap.Item governingAttribute |> Array.singleton
    }

    let findSkillStat (skillName:string) (skillStatArray: SkillStat array) =
        Array.filter (fun skill -> skill.name = skillName) skillStatArray
        |> (fun list ->
            if list.Length = 0 then
                {name = skillName; lvl = Zero; governingAttributes = [||]}
            else
                Array.maxBy (fun skill -> skill.lvl) list
        )

    let skillStatLvlToInt skillStat = convert_Neg1To4_To_Int skillStat.lvl

module SkillRoll = 
    open Attribute
    open Dice
    open SkillStat
    open Neg1To4

    type SkillRoll = {
        desc     : string
        dicePool : DicePool
        lvl      : Neg1To4
    }

    let matchAttributeDiceWithSkillDice (skillStat:SkillStat) (attributeStats:AttributeStat array) : DicePoolModification=
        Array.filter ( fun attributeStat -> 
            Array.exists ( fun skillGoverningAttribute -> 
                attributeStat.attribute = skillGoverningAttribute
            ) skillStat.governingAttributes
        ) attributeStats
        |> Array.sumBy ( fun attributeStat -> convert_Neg1To4_To_Int ( attributeStat.lvl ) )
        |> intToDicePoolModification

    let skillStatToSkillRoll (skillStat:SkillStat) (attributeStats:AttributeStat array) (baseDice:DicePool) attributeDeterminedDiceModArray =

        let skillDiceMods     = convert_Neg1To4_To_Int ( skillStat.lvl ) |> intToDicePoolModification
        let attributeDiceMods = matchAttributeDiceWithSkillDice skillStat attributeStats
        let diceMod = 
            determineAttributeDeterminedDiceMod skillStat.governingAttributes attributeDeterminedDiceModArray
            |> Array.append [| skillDiceMods; attributeDiceMods |]
        {
            desc     = skillStat.name
            dicePool = modifyDicePoolByModList baseDice diceMod
            lvl      = skillStat.lvl
        }

    let skillStatsToSkillRolls (skillStats:SkillStat array) (attributeStats:AttributeStat array) (baseDice:DicePool) attributeDeterminedDiceModArray =
        Array.map ( fun skillStat ->
            skillStatToSkillRoll skillStat attributeStats baseDice attributeDeterminedDiceModArray
        ) skillStats

module VocationStat =

    open Neg1To4
    open Attribute
    open SkillStat
    
    type VocationStat = {
        name                : string
        lvl                 : Neg1To4
        governingAttributes : Attribute array
        vocationalSkills    : SkillStat []
    }

    let findVocationalSkill (vocations: VocationStat array) (vocationalSkillName:string) =
        Array.map (fun vocation -> vocation.vocationalSkills) vocations
        |> Array.collect (fun x -> x)
        |> findSkillStat vocationalSkillName

module VocationRoll =
    open VocationStat
    open Neg1To4
    open Dice
    open Attribute
    open SkillRoll

    type VocationRoll = {
        name             : string
        vocationDicePool : DicePool
        vocationalSkills : SkillRoll array
    }

    let determineVocationAttributeDiceMod (vocationStat:VocationStat) (attributeStats:AttributeStat array) : DicePoolModification =
        Array.filter ( fun attributeStat -> 
            Array.exists ( fun vocationGoverningAttribute -> 
                attributeStat.attribute = vocationGoverningAttribute
            ) vocationStat.governingAttributes
        ) attributeStats
        |> Array.sumBy ( fun attributeStat -> convert_Neg1To4_To_Int ( attributeStat.lvl ) )
        |> intToDicePoolModification

    let vocationStatToVocationRoll (vocationStat:VocationStat) (attributeStats:AttributeStat array) (baseDice:DicePool) attributeDeterminedDiceModArray : VocationRoll=
        let name = vocationStat.name
        let attDiceMod = determineVocationAttributeDiceMod vocationStat attributeStats

        let vocationLvlDiceMod = convert_Neg1To4_To_Int ( vocationStat.lvl ) |> intToDicePoolModification

        let diceMods = 
            determineAttributeDeterminedDiceMod vocationStat.governingAttributes attributeDeterminedDiceModArray
            |> Array.append [| attDiceMod; vocationLvlDiceMod |]

        {
            name = name
            vocationDicePool = modifyDicePoolByModList baseDice diceMods
            vocationalSkills = skillStatsToSkillRolls vocationStat.vocationalSkills attributeStats baseDice attributeDeterminedDiceModArray
        }

module CombatRoll =
    open Dice
    open Damage
    open Shape
    open Range
    open EngageableOpponents
    open Penetration

    type CombatRoll = {
        desc                          : string
        combatRoll                    : DicePool
        calculatedRange               : CalculatedRange
        penetration                   : CalculatedPenetration
        damageTypes                   : DamageType []
        areaOfEffectShape             : Shape option
        engageableOpponents           : CalculatedEngageableOpponents
    }

    let combatRollToStringArray combatRoll =
        [|
            combatRoll.desc
            dicePoolToString combatRoll.combatRoll
            string combatRoll.penetration
            (calculatedRangeToString combatRoll.calculatedRange) + " ft (" + combatRoll.calculatedRange.desc + ")"
            damageTypesToString combatRoll.damageTypes
            string combatRoll.engageableOpponents
            shapeOptionToString combatRoll.areaOfEffectShape
        |]

    let combatRollsToStringArrays combatRolls =
        Array.map ( fun combatRoll -> combatRollToStringArray combatRoll ) combatRolls

module WeaponCombatRoll =
    open Dice
    open Attribute
    open Neg1To4
    open Shape
    open Range
    open Penetration
    open EngageableOpponents

    open Equipment
    open VocationStat
    open SkillStat
    open CombatRoll

    open WeaponResourceClass   
    open ItemTier
    open Item
    open WeaponClass

    let createCombatRoll desc weaponClass weaponTierBaseDice attributeDeterminedDiceModArray attributeStats skillStatLvl resource descSuffix wieldingDiceMods =

        let (resourceDesc, resourceDice, resourcePenetration, resourceRange, resourceDamageTypes, resourceAreaOfEffect) = determineResource resource

        let dicePool =
            determineAttributeDeterminedDiceMod weaponClass.governingAttributes attributeDeterminedDiceModArray // These are injuries, weight penalties, ect...
            |> Array.append wieldingDiceMods
            |> Array.append [|
                attributeStats |> determineAttributeDiceMod weaponClass.governingAttributes
                skillStatLvl   |> convert_Neg1To4_To_Int |> intToDicePoolModification
                resourceDice
            |]
            |> modifyDicePoolByModList weaponTierBaseDice

        let numDice = sumDicePool dicePool

        {
            desc                = desc + resourceDesc + descSuffix
            combatRoll          = dicePool
            calculatedRange     = determineGreatestRange numDice weaponClass.range resourceRange
            penetration         = determinePenetration numDice weaponClass.penetration (CalculatedPenetration resourcePenetration)
            damageTypes         = Array.append weaponClass.damageTypes resourceDamageTypes
            areaOfEffectShape   = compareAndDetermineAOE numDice weaponClass.areaOfEffect resourceAreaOfEffect
            engageableOpponents = determineEngageableOpponents numDice weaponClass.engageableOpponents
        }

    let createHandedVariationsCombatRolls twoHandedWeaponDice oneHandedWeaponDiceOption dualWieldableBonusOption preloadedCreateCombatRoll =

        let twoHandedCombat = preloadedCreateCombatRoll " (Two-handed)" [|twoHandedWeaponDice|]
        
        match oneHandedWeaponDiceOption with
        | None -> [|twoHandedCombat|]
        | Some oneHandedWeaponDice ->

            let oneHandedCombat = preloadedCreateCombatRoll " (One-handed)" [|oneHandedWeaponDice|]

            // If two and one handed have the same dice roll, then just return one handed to not clutter UI
            let handedVariations = if oneHandedWeaponDice = twoHandedWeaponDice then [|oneHandedCombat|] else [|twoHandedCombat;oneHandedCombat|]
     
            match dualWieldableBonusOption  with
            | None -> handedVariations
            | Some dualWieldableBonus -> 
                preloadedCreateCombatRoll " (Dual-wielded)" [|oneHandedWeaponDice;dualWieldableBonus|]
                |> Array.singleton
                |> Array.append handedVariations

    let createWeaponCombatRollWithEquipmentList (equipment:EquipmentItem array) (attributeStats:AttributeStat array) (vocationStats:VocationStat array) attributeDeterminedDiceModArray =

        getEquipedItems equipment
        |> Array.collect (fun weaponItem ->

            collectWeaponItemClasses weaponItem
            |> Array.collect (fun weaponClass ->

                let skillStat = findVocationalSkill vocationStats weaponClass.desc

                let preloadedCreateHandVariationsCombatRolls =
                    createHandedVariationsCombatRolls weaponClass.twoHandedWeaponDice weaponClass.oneHandedWeaponDice weaponClass.dualWieldableBonus

                let preloadedCreateCombatRoll = 
                    createCombatRoll weaponItem.name weaponClass weaponItem.itemTier.baseDice attributeDeterminedDiceModArray attributeStats skillStat.lvl

                match weaponClass.resourceClass with
                | Some resourceClass -> 
                    getEquipedItems equipment
                    |> Array.collect( fun item ->
                        collectWeaponResourceItemClasses item
                        |> Array.filter ( fun weaponResourceItem -> weaponResourceItem.resourceClass = resourceClass)
                        |> Array.collect (fun weaponResourceClass -> 
                           Some weaponResourceClass |> preloadedCreateCombatRoll |> preloadedCreateHandVariationsCombatRolls

                        )
                    )
                | None -> None |> preloadedCreateCombatRoll |> preloadedCreateHandVariationsCombatRolls
            )
        )

module MagicCombatRoll =
    open Dice
    open Attribute
    open Neg1To4
    open Range
    open Penetration
    open Shape
    open EngageableOpponents

    open Equipment
    open VocationStat
    open SkillStat
    open CombatRoll

    open ItemTier
    open Item

    open MagicResource
    open MagicSkill
    open MagicCombat
    open ConduitClass

    let determineMagicRangedClass (rangeMap:Map<string,Range>) (lvl:int) : Range =
        match lvl with
        | n when n = 0  -> rangeMap.Item "Short"
        | n when n = 1  -> rangeMap.Item "Medium"
        | n when n = 2  -> rangeMap.Item "Extended"
        | n when n = 3  -> rangeMap.Item "Long"
        | n when n = 4  -> rangeMap.Item "Sharpshooter"
        | n when n >= 5 -> rangeMap.Item "Extreme"
        | _             -> rangeMap.Item "Close"

    let determineMagicRange (rangeMap:Map<string,Range>) (magicCombatName:string) (lvl:int) : Range =
        match magicCombatName with
        | "Melee"       -> rangeMap.Item "Reach"
        | "Melee Trick"  -> rangeMap.Item "Melee"
        | "Ranged Trick" -> rangeMap.Item "Short"
        | _           -> determineMagicRangedClass rangeMap lvl

    let createMagicCombatRoll (magicResource:MagicResource) (attributeStats:AttributeStat array) skillStat (magicSkill:MagicSkill)
                              (magicCombatType:MagicCombat) rangeMap attributeDeterminedDiceModArray : CombatRoll =

        let (resourceDesc, resourceDice) = determineMagicResource magicResource

        let range = determineMagicRange rangeMap magicCombatType.desc <| (convert_Neg1To4_To_Int skillStat.lvl)

        let diceMods =
            determineAttributeDeterminedDiceMod skillStat.governingAttributes attributeDeterminedDiceModArray
            |> Array.append [|
                determineAttributeDiceMod skillStat.governingAttributes attributeStats
                convert_Neg1To4_To_Int skillStat.lvl |> intToDicePoolModification
                magicCombatType.diceModification
                resourceDice
            |]

        let combatRoll = modifyDicePoolByModList baseDicePool diceMods

        let numDice = sumDicePool combatRoll

        {
            desc                = sprintf "%s %s %s" magicSkill.desc magicCombatType.desc resourceDesc
            combatRoll          = combatRoll
            calculatedRange     = rangeToCalculatedRange numDice range
            penetration         = calculatePenetration numDice magicCombatType.penetration
            damageTypes         = magicSkill.damageTypes
            areaOfEffectShape   = determineAOE numDice magicCombatType.areaOfEffect
            engageableOpponents = determineEngageableOpponents numDice magicCombatType.engageableOpponents
        }

    let createMagicCombatRollWithConduit rangeMap magicResource attributeStats skillStatLvl (magicSkill:MagicSkill) (magicCombatType:MagicCombat) (conduit:ConduitClass)
        conduitItemDesc conduitTierBaseDice attributeDeterminedDiceModArray descSuffix wieldingDiceMods : CombatRoll =

        let (resourceDesc, resourceDice) = determineMagicResource magicResource

        let skillStatLvlAsInt = convert_Neg1To4_To_Int skillStatLvl

        let range = determineMagicRange rangeMap magicCombatType.desc (skillStatLvlAsInt + conduit.rangeAdjustment)
        let damageTypes = Array.append magicSkill.damageTypes conduit.damageTypes

        let engageableOpponents = match conduit.engageableOpponents with | Some EO -> EO | None -> magicCombatType.engageableOpponents

        let dicePool = 
            determineAttributeDeterminedDiceMod conduit.governingAttributes attributeDeterminedDiceModArray
            |> Array.append wieldingDiceMods
            |> Array.append [|
                attributeStats |> determineAttributeDiceMod conduit.governingAttributes
                skillStatLvlAsInt |> intToDicePoolModification
                magicCombatType.diceModification
                resourceDice
            |]
            |> modifyDicePoolByModList conduitTierBaseDice

        let numDice = sumDicePool dicePool

        {
            desc                = sprintf "%s %s with %s %s %s" magicSkill.desc magicCombatType.desc conduitItemDesc resourceDesc descSuffix
            combatRoll          = dicePool
            calculatedRange     = rangeToCalculatedRange numDice range
            penetration         = determinePenetration numDice magicCombatType.penetration conduit.penetration
            damageTypes         = damageTypes
            areaOfEffectShape   = compareAndDetermineAOE numDice magicCombatType.areaOfEffect conduit.areaOfEffect
            engageableOpponents = determineEngageableOpponents numDice engageableOpponents
        }


    let createMagicCombatRollWithConduitHandVariations (conduit:ConduitClass) preloadedCreatMagicCombatRollWithConduit : CombatRoll array =
        
        let twoHandedCombatRoll = preloadedCreatMagicCombatRollWithConduit  " (Two-handed)" [|conduit.twoHandedDice|]

        match conduit.oneHandedDice with
        | None -> [|twoHandedCombatRoll|]
        | Some oneHandedDice ->

            let oneHandedCombatRoll = preloadedCreatMagicCombatRollWithConduit " (One-handed)" [|oneHandedDice|]
            let handedVariations = if conduit.twoHandedDice = oneHandedDice then [|oneHandedCombatRoll|] else [|twoHandedCombatRoll;oneHandedCombatRoll|]


            match conduit.dualWieldableBonus with
            | None -> handedVariations
            | Some dualWieldableBonus ->
               
                preloadedCreatMagicCombatRollWithConduit " (Dual-wielded)" [|oneHandedDice;dualWieldableBonus|]
                |> Array.singleton
                |> Array.append handedVariations

    let createMagicCombatRolls (attributeStats:AttributeStat array) (vocationStats:VocationStat array) (magicSkillMap: Map<string,MagicSkill>)
                               (magicCombatMap:Map<string,MagicCombat>) (equipment:EquipmentItem []) rangeMap attributeDeterminedDiceModArray =

        let magicMapKeys  = magicSkillMap.Keys |> Array.ofSeq
        vocationStats
        |>Array.collect ( fun (vocationStat:VocationStat) -> 
            Array.filter ( fun (skillStat:SkillStat) -> Array.contains skillStat.name magicMapKeys ) vocationStat.vocationalSkills
            |>Array.collect ( fun skillStat -> 
                // Indexes into tho the magicMap for the type of Magic
                let magicSkill        = magicSkillMap.Item skillStat.name
                // Determines what types of MagicCombat can be preformed for the Magic
                let magicCombatNames = determineMagicCombatTypes magicSkill.meleeCapable skillStat.lvl (magicCombatMap.Values |> Seq.toArray)

                // Maps across the magicCombat types that the magic is capable of
                magicCombatNames
                |>Array.collect ( fun (magicCombatName:string) -> 
                    let magicCombatType = magicCombatMap.Item magicCombatName

                    let equipedConduits = getEquipedConduitItemsWithSkillName equipment skillStat.name

                    let magicResource : MagicResource = { magicResouceClass = magicSkill.magicResourceClass; numMagicResourceDice = magicCombatType.minResourceRequirment}

                    if equipedConduits.Length > 0 then
                        Array.collect ( fun (conduitItem:Item) -> 
                            collectConduitClasses conduitItem
                            |> Array.collect ( fun conduitClass ->
                                let preloadedCreatMagicCombatRollWithConduit = 
                                    createMagicCombatRollWithConduit rangeMap magicResource attributeStats skillStat.lvl magicSkill magicCombatType conduitClass conduitItem.name
                                        conduitItem.itemTier.baseDice attributeDeterminedDiceModArray
                                createMagicCombatRollWithConduitHandVariations conduitClass preloadedCreatMagicCombatRollWithConduit

                                                            )
                        ) equipedConduits
                    else 
                        createMagicCombatRoll magicResource attributeStats skillStat magicSkill magicCombatType rangeMap attributeDeterminedDiceModArray |> Array.singleton
                )
            )
        )

module CarryWeightCalculation =
    open Attribute
    open SkillStat

    type CarryWeightCalculation = {
        name                        : string
        baseWeight                  : uint
        governingAttribute          : Attribute
        weightIncreasePerAttribute  : uint
        governingSkill              : string
        weightIncreasePerSkill      : uint
    }

    let createCarryWeightCalculation (name, baseWeight, governingAttribute, weightIncreasePerAttribute, governingSkill, weightIncreasePerSkill) = {   
        name                       = name
        baseWeight                 = baseWeight 
        governingAttribute         = governingAttribute
        weightIncreasePerAttribute = weightIncreasePerAttribute
        governingSkill             = governingSkill
        weightIncreasePerSkill     = weightIncreasePerSkill
    }

    type WeightClass = {
        name                   : string
        bottomPercent          : float
        topPercent             : float
        percentOfMovementSpeed : float
    }

    let tupleToWeightClass (name, bottomPercent, topPercent, percentOfMovementSpeed) : WeightClass = {
        name                   = name
        bottomPercent          = bottomPercent
        topPercent             = topPercent
        percentOfMovementSpeed = percentOfMovementSpeed
    }

    let calculateMaxCarryWeight (maxCarryWeightCalculation:CarryWeightCalculation) (attributeStatArray:AttributeStat array) (skillStatArray:SkillStat array) =
        let attributeLevel = determineAttributeLvl [| maxCarryWeightCalculation.governingAttribute |] attributeStatArray
        let skillLevel     = findSkillStat maxCarryWeightCalculation.governingSkill skillStatArray |> skillStatLvlToInt

        int maxCarryWeightCalculation.baseWeight
        |> (+) (attributeLevel * int maxCarryWeightCalculation.weightIncreasePerAttribute)
        |> (+) (skillLevel * int maxCarryWeightCalculation.weightIncreasePerSkill)
        |> float

module MovementSpeedCalculation =

    open Attribute

    type MovementSpeedCalculation = {
        desc                : string
        baseMovementSpeed   : uint
        governingAttributes : Attribute array
        feetPerAttributeLvl : uint
        skillString         : string
        feetPerSkillLvl     : uint
    }

    let calculateMovementSpeed movementSpeedCalculation (attributeLvl:int) (skillLvl:int) =
        let rfxMod = attributeLvl *  int movementSpeedCalculation.feetPerAttributeLvl
        let athleticsMod = skillLvl * int movementSpeedCalculation.feetPerSkillLvl

        match (int movementSpeedCalculation.baseMovementSpeed + rfxMod + athleticsMod) with
        | n when n >= 0 -> uint n
        | _ -> 0u

    let createMovementSpeedString movementSpeedCalculation reflexLvl athleticsLvl percentOfMovementSpeed =
        let decimalPlaces = 0
        let movementSpeed = calculateMovementSpeed movementSpeedCalculation reflexLvl athleticsLvl
        let scaledMovementSpeed = float movementSpeed * percentOfMovementSpeed
        sprintf "%s ft" (scaledMovementSpeed.ToString("F" +  decimalPlaces.ToString()))

module Effects =
    open MovementSpeedCalculation
    open Attribute
    open SkillStat
    open Equipment
    open Item
    open DefenseClass
    open CarryWeightCalculation

    type EffectTable = (string * string * string) array

    type Effect =
    | MovementSpeedCalculation  of MovementSpeedCalculation
    | CarryWeightCalculation of CarryWeightCalculation

    let descToEffect (movementSpeedMap:Map<string,MovementSpeedCalculation>) desc =
        match desc with
        | desc when Map.containsKey desc movementSpeedMap -> movementSpeedMap.Item desc |> MovementSpeedCalculation |> Some
        | _ -> None

    let effectToEffectString (effect:Effect) attributeStatArray skillStatArray weightClass =
        match effect with
        | MovementSpeedCalculation calculation ->
            let attributeLvl = determineAttributeLvl calculation.governingAttributes attributeStatArray
            let athleticsLvl = findSkillStat calculation.skillString skillStatArray |> skillStatLvlToInt
            createMovementSpeedString calculation attributeLvl athleticsLvl weightClass.percentOfMovementSpeed
        | CarryWeightCalculation maxCarryWeightCalculation ->

            "Wrong path, fix this" // Remove this path

    let createCalculatedEffectTableArray effectArray attributeStatArray skillStats equipment (weightInfoTuple:(WeightClass * float * float)) : EffectTable =

        let (weightClass, totalWeight, maxWeight) = weightInfoTuple
        Array.map ( fun tuple ->
            match tuple with
            | (effectOption, desc, effectString, time) -> 
                match effectOption with
                | Some effect -> 
                    let calculatedEffectString = effectToEffectString effect attributeStatArray skillStats weightClass
                    (desc, calculatedEffectString, time)
                | None ->
                    match desc with
                    | "Defense Level" -> 
                        let temp = Array.collect ( fun item -> collectDefenseClasses item ) (getEquipedItems equipment)
                        let temp2 : ( float * float * float) array = 
                            Array.collect ( fun  defenseClass ->
                                [| (defenseClass.physicalDefense, defenseClass.mentalDefense, defenseClass.spiritualDefense) |]
                            ) temp
                        let temp3 = 
                            Array.fold ( fun acc tuple ->
                                match ( acc, tuple ) with
                                | (accPhysical, accMental, accSpiritual), (physical, mental, spiritual) ->
                                    (accPhysical + physical, accMental + mental, accSpiritual + spiritual)
                            ) (0.0, 0.0, 0.0) temp2

                        let (physicalDefense, spiritualDefense, mentalDefense) = temp3

                        let decimalPlaces = 2

                        let defenseEffectString = 
                            sprintf "Physical: %s, Mental: %s, Spiritual: %s" (physicalDefense.ToString("F" +  decimalPlaces.ToString())) (mentalDefense.ToString("F" +  decimalPlaces.ToString())) (spiritualDefense.ToString("F" +  decimalPlaces.ToString()))

                        (desc, defenseEffectString, "")
                    | "Inventory Weight" ->

                        let decimalPlaces = 2
                        let inventoryWeight = 
                            sprintf "%s/%s lb (%s)" (totalWeight.ToString("F" +  decimalPlaces.ToString())) (maxWeight.ToString("F" +  decimalPlaces.ToString())) weightClass.name
                        (desc, inventoryWeight, "")

                    | _ -> ("", "", "")
        ) effectArray

module Character =

    open Dice
    open MagicCombatRoll
    open WeaponCombatRoll
    open SkillRoll
    open MagicCombat
    open CombatRoll
    open VocationRoll
    open Effects
    open Attribute
    open Item
    open CarryWeightCalculation
    open Equipment

    type Character = {
        vocationRolls              : VocationRoll array
        coreSkillRolls             : SkillRoll array
        combatRolls                : CombatRoll array
        calculatedEffectTable      : EffectTable
    }

    let createCharacter skillStats attributeStats equipment  vocationData magicSkillMap (magicCombatMap:Map<string,MagicCombat>) rangeMap effectOptionTupleArray 
        (attributeDeterminedDicePoolModMap:Map<string,AttributeDeterminedDiceMod>) (carryWeightCalculation:CarryWeightCalculation) (weightClassData:WeightClass array) =

        let totalWeight = calculateEquipmentItemArrayWeight equipment
        let maxWeight   = calculateMaxCarryWeight carryWeightCalculation attributeStats skillStats
        let percentOfMaxWeight = totalWeight / maxWeight

        let weightClass =
            if maxWeight > 0 then
                Array.collect ( fun (weightClass:WeightClass) -> 
                    match weightClass with
                    | n when n.bottomPercent <= percentOfMaxWeight && percentOfMaxWeight < n.topPercent -> n |> Array.singleton
                    | _ -> [||]
                ) weightClassData |> Array.head
            else
                Array.last weightClassData // If 0 max weight, default to hightest weight catagory

        let attributeDeterminedDiceModArray = 
            effectOptionTupleArray
            |> Array.collect ( fun tuple ->
                let (_,name,_,_) = tuple
                if attributeDeterminedDicePoolModMap.ContainsKey name then
                    attributeDeterminedDicePoolModMap.Item name |> Array.singleton
                elif name = "Inventory Weight" then
                    attributeDeterminedDicePoolModMap.Item weightClass.name |> Array.singleton
                else
                    [||]
            ) 

        let weaponCombatRolls = createWeaponCombatRollWithEquipmentList equipment attributeStats vocationData attributeDeterminedDiceModArray

        let magicCombatRolls = createMagicCombatRolls attributeStats vocationData magicSkillMap magicCombatMap equipment rangeMap attributeDeterminedDiceModArray

        let calculatedEffectTable  = createCalculatedEffectTableArray effectOptionTupleArray attributeStats skillStats equipment (weightClass, totalWeight, maxWeight)

        {
            vocationRolls         = Array.map ( fun vocationStat -> vocationStatToVocationRoll vocationStat attributeStats (stringToDicePool "3d6") attributeDeterminedDiceModArray ) vocationData
            coreSkillRolls        = skillStatsToSkillRolls skillStats attributeStats (stringToDicePool "3d6") attributeDeterminedDiceModArray
            combatRolls           = Array.append weaponCombatRolls magicCombatRolls
            calculatedEffectTable = calculatedEffectTable
        }

module BuildRules =

    open MapUtils
    open Damage
    open Penetration
    open EngageableOpponents
    open Range
    open ResourceClass
    open Attribute
    open MagicSkill
    open MagicCombat
    open Dice
    open WeaponClass
    open ConduitClass
    open WeaponResourceClass    
    open ItemTier
    open Item
    open Equipment
    open Neg1To4
    open SkillStat
    open VocationStat
    open Character
    open TypeUtils
    open MovementSpeedCalculation
    open Effects
    open AreaOfEffect
    open DefenseClass
    open CarryWeightCalculation

    type EffectMap = {
        movementSpeedCalculationMap : Map<string,MovementSpeedCalculation>
    }

    let buildRules damageTypeData penetrationCalculationData calculatedEngageableOpponentsData 
                   engageableOpponentsCalculationData calculatedRangeData rangeCalculationData resourceClassData
                   attributeData  magicSkillData magicCombatData weaponClassData conduitClassData itemTierData itemData
                   weaponResourceClassData equipmentData skillStatData attributeStatData movementSpeedData
                   (vocationDataArray: (string * int * (string * int) array ) array) effectsTableData defenseClassData attributeDeterminedDiceModData 
                   carryWeightCalculationData weightClassData =
        // DamageTypeData
        let stringToDamageTypeArray =
            damageTypeData
            |> stringArrayToTypeMap 
            |> stringAndMapToDamageTypeArray 

        // PenetrationData
        let penetrationMap = 
            penetrationCalculationData 
            |> createMapFromTuple2List tupleToPenetrationCalculation 
            |> createPenetrationMap 

        // EngageableOpponentsData
        let EOInterface =
            createEOInterface 
            <| createCalculatedEOMap calculatedEngageableOpponentsData
            <| createEOCalculationMap engageableOpponentsCalculationData

        // RangeData
        let rangeMap = createRangeMap calculatedRangeData rangeCalculationData

        let RangeOptionMap string =
            match string with
            | "None" -> None
            | _      -> rangeMap.Item string |> Some

        // ResourceClassData
        let resourceClassMap : Map<string,ResourceClass> = stringArrayToTypeMap resourceClassData

        let weaponResourceClassOptionMap string =
            match string with
            | "None" -> None
            | _      -> Some <| resourceClassMap.Item string

        // AttributeData
        let internalAttributeMap = stringArrayToTypeMap attributeData

        let MapAndStringToAttributes (attributeMap:Map<string,Attribute>) = 
            String.filter ((<>)' ')
            >> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
            >> Array.map (fun attributeString -> attributeMap.Item attributeString)

        let stringToAttributes = MapAndStringToAttributes internalAttributeMap

        let stringDescToAttributes (inputString:string) =
            if inputString.Contains('{') then
                let splitArray = inputString.Split('{', System.StringSplitOptions.RemoveEmptyEntries)
                let removedFrontCurly = Array.last splitArray
                let removedBackCurly  = removedFrontCurly.Split('}', System.StringSplitOptions.RemoveEmptyEntries)
                stringToAttributes (removedBackCurly.[0])
            else
               [||]

        // MagicSkillData
        let createMagicSkill (desc, damageTypes, rangeAdjustment, meleeCapable, magicResourceClass) : MagicSkill = { 
                desc               = desc
                damageTypes        = stringToDamageTypeArray damageTypes
                rangeAdjustment    = rangeAdjustment
                meleeCapable       = meleeCapable
                magicResourceClass = resourceClassMap.Item magicResourceClass
            }

        let magicSkillMap = createMapFromTuple5List createMagicSkill magicSkillData

        // MagicCombatData
        let createMagicCombat (desc, lvlRequirment, diceModification, penetration, range, 
            engageableOpponents, minResourceRequirment, canVocationAssist, areaOfEffect ) =
            { 
                desc                  = desc
                lvlRequirment         = intToNeg1To4 lvlRequirment
                diceModification      = stringToDicePoolModification diceModification
                penetration           = penetrationMap penetration
                range                 = rangeMap.Item range
                engageableOpponents   = EOInterface engageableOpponents
                minResourceRequirment = minResourceRequirment
                canVocationAssist     = canVocationAssist
                areaOfEffect          = AreaOfEffectOptionMap.Item areaOfEffect
            }

        let magicCombatMap = createMapFromTuple9List createMagicCombat magicCombatData

        // WeaponClassData
        let createWeaponClass (desc, oneHandedWeaponDice, twoHandedWeaponDice, penetration, range, damageTypes, engageableOpponents, 
               dualWieldableBonus, areaOfEffect, resourceClass, governingAttributes) = 
            { 
                desc                = desc
                oneHandedWeaponDice = stringToDicePoolModificationOption oneHandedWeaponDice
                twoHandedWeaponDice = stringToDicePoolModification twoHandedWeaponDice
                penetration         = penetrationMap penetration
                range               = rangeMap.Item range
                damageTypes         = stringToDamageTypeArray damageTypes
                engageableOpponents = EOInterface engageableOpponents
                dualWieldableBonus  = stringToDicePoolModificationOption dualWieldableBonus
                areaOfEffect        = AreaOfEffectOptionMap.Item areaOfEffect
                resourceClass       = weaponResourceClassOptionMap resourceClass
                governingAttributes = stringDescToAttributes governingAttributes
            }

        let weaponClassMap = createMapFromTuple11List createWeaponClass weaponClassData

        // ConduitClassData
        let createConduitClass (desc, oneHandedDice, twoHandedDice, penetration, rangeAdjustment, damageTypes, engageableOpponents, 
                 dualWieldableBonus, areaOfEffect, resourceClass, governingAttributes) : ConduitClass = { 
                desc                = desc
                oneHandedDice       = stringToDicePoolModificationOption oneHandedDice
                twoHandedDice       = stringToDicePoolModification twoHandedDice
                penetration         = penetrationMap penetration
                rangeAdjustment     = rangeAdjustment
                damageTypes         = stringToDamageTypeArray damageTypes
                engageableOpponents = match engageableOpponents with | "None" -> None | _ -> Some <| EOInterface engageableOpponents
                dualWieldableBonus  = stringToDicePoolModificationOption dualWieldableBonus
                areaOfEffect        = AreaOfEffectOptionMap.Item areaOfEffect
                resourceClass       = weaponResourceClassOptionMap resourceClass
                governingAttributes = stringDescToAttributes governingAttributes
            }

        let conduitClassMap = createMapFromTuple11List createConduitClass conduitClassData

        // WeaponResourceClassData
        let createWeaponResourceClass (name, resourceClass, resourceDice, penetration, range, damageTypes, areaOfEffect) = {
            name                = name
            resourceClass       = resourceClassMap.Item resourceClass
            resourceDice        = stringToDicePoolModification resourceDice
            penetration         = penetration
            range               = RangeOptionMap range
            damageTypes         = stringToDamageTypeArray damageTypes
            areaOfEffect        = AreaOfEffectOptionMap.Item areaOfEffect
        }
    
        let weaponResourceClassMap = createMapFromTuple7List createWeaponResourceClass weaponResourceClassData

        // DefenseClassData

        let defenseClassMap = createMapFromTuple4List tupleToDefenseClass defenseClassData

        // ItemTierData
        let createItemTier (desc, level, runeSlots, baseDice, durabilityMax) = 
            { 
                desc = desc
                level = level
                runeSlots = runeSlots 
                baseDice = stringToDicePool baseDice
                durabilityMax = durabilityMax 
            }

        let itemTierMap = createMapFromTuple5List createItemTier itemTierData

        // ItemData

        let stringToItemClassArray (weaponClassMap:Map<string,WeaponClass>) (conduitClassMap:Map<string,ConduitClass>) (weaponResourceClassMap:Map<string,WeaponResourceClass>) (input:string)  =
            input.Split ", " |> 
            Array.collect ( fun className ->
                match className with
                | weaponClassName  when weaponClassMap.Keys.Contains weaponClassName  -> weaponClassMap.Item weaponClassName |> WeaponClass |> Array.singleton
                | conduitClassName when conduitClassMap.Keys.Contains conduitClassName -> conduitClassMap.Item conduitClassName |> ConduitClass |> Array.singleton
                | weaponResourceClassName when weaponResourceClassMap.Keys.Contains weaponResourceClassName -> weaponResourceClassMap.Item weaponResourceClassName |> WeaponResourceClass |> Array.singleton
                | defenseClassName when defenseClassMap.Keys.Contains defenseClassName -> defenseClassMap.Item defenseClassName |> DefenseClass |> Array.singleton
                | _ -> [||]
            )

        let createItem (name, itemClasses, itemTier, durabilityOrAmount, weight, value) = 
            {
                name           = name
                itemClasses    = stringToItemClassArray weaponClassMap conduitClassMap weaponResourceClassMap itemClasses
                itemTier       = itemTierMap.Item itemTier
                value          = value
                weight         = weight
            }

        let itemMap   = createMapFromTuple6List createItem itemData

        // EquipmentData

        let equipmentArray = Array.map ( fun tuple -> tupleToEquipmentItem itemMap tuple) equipmentData

        // SkillStatData
        let createSkillStatArray skillStatData = Array.map ( fun tuple -> tupleToSkillStat internalAttributeMap tuple) skillStatData
        let skillStatArray = createSkillStatArray skillStatData

        // VocationData
        let filterVocationData vocationDataArray : (string * int * (string * int) array ) array = 
            Array.collect ( fun vocationData -> 
                match vocationData with
                | ( vocationDataName, vocationDataLvl, skillDataArray) ->
                    if vocationDataName = "" then
                        [||]
                    else
                        let filteredVocationalSkillData : (string * int) array= 
                            Array.collect (fun skillData ->
                                match skillData with
                                | (skillDataName, skillDataLvl) ->
                                    if skillDataName <> "" then
                                        [|(skillDataName, skillDataLvl)|]
                                    else
                                        [||]
                            ) skillDataArray
                        [|(vocationDataName, vocationDataLvl, filteredVocationalSkillData)|]

            ) vocationDataArray

        let createVocationalSkill vocationGoverningAttributes (weaponClassMap: Map<string, WeaponClass>) vocationalSkillStatTuple: SkillStat =
            match vocationalSkillStatTuple with
            | (name, lvl) ->
                if weaponClassMap.Keys.Contains name then
                    let weaponClass = weaponClassMap.Item name
                    { name = name; lvl = intToNeg1To4 lvl; governingAttributes = weaponClass.governingAttributes }
                else
                    { name = name; lvl = intToNeg1To4 lvl; governingAttributes = vocationGoverningAttributes }

        let createVocationStat (vocationDataArray: (string * int * (string * int) array ) array) =
            Array.map ( fun vocationData ->
                match vocationData with
                | ((vocationStatName:string), (vocationStatLvl:int),  (vocationalSkillDataArray)) ->
                    let vocationGoverningAttributes = stringDescToAttributes vocationStatName;
                    { 
                        name = vocationStatName
                        lvl = intToNeg1To4 vocationStatLvl
                        governingAttributes = vocationGoverningAttributes
                        vocationalSkills = Array.map (createVocationalSkill vocationGoverningAttributes weaponClassMap) vocationalSkillDataArray
                    }
            ) vocationDataArray

        let vocationArray = filterVocationData vocationDataArray |> createVocationStat

        // AttributeStatData
        let attributeStatArray = Array.map ( fun tuple -> tupleToAttributeStat internalAttributeMap tuple) attributeStatData
                
        // MovementSpeedCalculationData
        let createMovementSpeedCalculation (desc, baseMovementSpeed, attributesString, feetPerAttributeLvl, skillString, feetPerSkillLvl) =  
            {
                desc                = desc
                baseMovementSpeed   = baseMovementSpeed
                governingAttributes = stringToAttributes attributesString
                feetPerAttributeLvl = feetPerAttributeLvl
                skillString         = skillString
                feetPerSkillLvl     = feetPerSkillLvl
            }

        let movementSpeedCalculationMap = createMapFromTuple6List createMovementSpeedCalculation movementSpeedData

        // AttributeDeterminedDicePoolMod
        let createAttributeDeterminedDicePoolMod (name, attributeString, diceModString) = { 
            name                 = name
            attributesToEffect   = stringToAttributes attributeString
            dicePoolModification = stringToDicePoolModification diceModString 
        }

        // EffectTableData
        let createEffectOptionTuples (desc, effect, time) = (
            descToEffect movementSpeedCalculationMap desc,
            desc, 
            effect,
            time
        )

        createCharacter 
            skillStatArray 
            attributeStatArray 
            equipmentArray 
            vocationArray 
            magicSkillMap 
            magicCombatMap 
            rangeMap 
            ( Array.map ( fun tuple -> createEffectOptionTuples tuple) effectsTableData )  // EffectionOtionTuple Array
            ( createMapFromTuple3List createAttributeDeterminedDicePoolMod attributeDeterminedDiceModData ) // AttributeDeterminedDicePoolMod Data
            ( createCarryWeightCalculation (carryWeightCalculationData |> Array.head) )                     // CarryWeightCalculation Data
            ( Array.map ( fun tuple -> tupleToWeightClass tuple) weightClassData )  // WeightClassCalculation Data
