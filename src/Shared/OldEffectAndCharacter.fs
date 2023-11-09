module Effects =
    open FallenLib.MovementSpeedCalculation
    open Attribute
    open Equipment
    open Item
    open DefenseClass
    open CarryWeightCalculation
    open VocationGroup
    open Neg1To4

    type EffectTable = (string * string * string) list

    type Effect =
        | MovementSpeedCalculation of MovementSpeedCalculation
        | CarryWeightCalculation of CarryWeightCalculation

    let descToEffect (movementSpeedMap: Map<string, MovementSpeedCalculation>) name =
        match name with
        | name when Map.containsKey name movementSpeedMap ->
            movementSpeedMap.Item name
            |> MovementSpeedCalculation
            |> Some
        | _ -> None

    let effectToEffectString (effect: Effect) attributeStatArray skillStatArray weightClass =
        match effect with
        | MovementSpeedCalculation calculation ->
            let attributeLvl =
                determineAttributeLvl calculation.governingAttributes attributeStatArray

            let skillLvl =
                findVocationalSkillLvlWithDefault calculation.governingSkill Zero skillStatArray
                |> neg1To4ToInt

            createMovementSpeedString calculation attributeLvl skillLvl weightClass.percentOfMovementSpeed
        | CarryWeightCalculation maxCarryWeightCalculation ->

            "Wrong path, fix this" // Remove this path

    let createCalculatedEffectTableArray
        effectArray
        attributeStatArray
        skillStats
        equipment
        (weightInfoTuple: (WeightClass * float * float))
        : EffectTable =

        let (weightClass, totalWeight, maxWeight) = weightInfoTuple

        List.map
            (fun tuple ->
                match tuple with
                | (effectOption, desc, effectString, time) ->
                    match effectOption with
                    | Some effect ->
                        let calculatedEffectString =
                            effectToEffectString effect attributeStatArray skillStats weightClass

                        (desc, calculatedEffectString, time)
                    | None ->
                        match desc with
                        | "Defense Level" ->
                            let temp = List.collect collectDefenseClasses (getEquipedItems equipment)

                            let temp2: (float * float * float) list =
                                List.collect
                                    (fun defenseClass ->
                                        [ (defenseClass.physicalDefense,
                                           defenseClass.mentalDefense,
                                           defenseClass.spiritualDefense) ])
                                    temp

                            let temp3 =
                                List.fold
                                    (fun acc tuple ->
                                        match (acc, tuple) with
                                        | (accPhysical, accMental, accSpiritual), (physical, mental, spiritual) ->
                                            (accPhysical + physical, accMental + mental, accSpiritual + spiritual))
                                    (0.0, 0.0, 0.0)
                                    temp2

                            let (physicalDefense, spiritualDefense, mentalDefense) = temp3

                            let decimalPlaces = 2

                            let defenseEffectString =
                                sprintf
                                    "Physical: %s, Mental: %s, Spiritual: %s"
                                    (physicalDefense.ToString("F" + decimalPlaces.ToString()))
                                    (mentalDefense.ToString("F" + decimalPlaces.ToString()))
                                    (spiritualDefense.ToString("F" + decimalPlaces.ToString()))

                            (desc, defenseEffectString, "")
                        | "Inventory Weight" ->

                            let decimalPlaces = 2

                            let inventoryWeight =
                                sprintf
                                    "%s/%s lb (%s)"
                                    (totalWeight.ToString("F" + decimalPlaces.ToString()))
                                    (maxWeight.ToString("F" + decimalPlaces.ToString()))
                                    weightClass.name

                            (desc, inventoryWeight, "")

                        | _ -> ("", "", ""))
            effectArray

module Character =

    open CombatRoll
    open Effects
    open Attribute
    open CarryWeightCalculation
    open Equipment
    open VocationGroup
    open CoreSkillGroup

    type Character =
        { vocationRolls: VocationGroup list
          coreSkillRolls: CoreSkillGroup list
          combatRolls: CombatRoll list
          calculatedEffectTable: EffectTable }

    let createCharacter
        skillStats
        attributeStats
        equipment
        effectOptionTupleArray
        (attributeDeterminedDicePoolModMap: Map<string, AttributeDeterminedDiceMod>)
        (carryWeightCalculation: CarryWeightCalculation)
        (weightClassData: WeightClass list)
        =

        let totalWeight = calculateEquipmentListWeight equipment

        let maxWeight =
            calculateMaxCarryWeight carryWeightCalculation attributeStats skillStats

        let percentOfMaxWeight = totalWeight / maxWeight

        let weightClass =
            if maxWeight > 0 then
                List.collect
                    (fun (weightClass: WeightClass) ->
                        match weightClass with
                        | n when
                            n.bottomPercent <= percentOfMaxWeight
                            && percentOfMaxWeight < n.topPercent
                            ->
                            n |> List.singleton
                        | _ -> [])
                    weightClassData
                |> List.head
            else
                List.last weightClassData // If 0 max weight, default to hightest weight catagory

        let attributeDeterminedDiceModArray =
            effectOptionTupleArray
            |> List.collect (fun tuple ->
                let (_, name, _, _) = tuple

                if attributeDeterminedDicePoolModMap.ContainsKey name then
                    attributeDeterminedDicePoolModMap.Item name
                    |> List.singleton
                elif name = "Inventory Weight" then
                    attributeDeterminedDicePoolModMap.Item weightClass.name
                    |> List.singleton
                else
                    [])

        let calculatedEffectTable =
            createCalculatedEffectTableArray
                effectOptionTupleArray
                attributeStats
                skillStats
                equipment
                (weightClass, totalWeight, maxWeight)

        { vocationRolls = []
          coreSkillRolls = []
          combatRolls = []
          calculatedEffectTable = calculatedEffectTable }