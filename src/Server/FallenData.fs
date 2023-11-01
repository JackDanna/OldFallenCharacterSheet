namespace FallenData

module Data =
    open FSharp.Data

    open OldFallenLib.Damage
    open OldFallenLib.EngageableOpponents


    let makeFallenDataPath fileName =
        __SOURCE_DIRECTORY__ + "/FallenData/" + fileName

    let makeFallenData fileName mappingFunc =
        CsvFile.Load(makeFallenDataPath fileName, hasHeaders = true).Rows
        |> Seq.map (mappingFunc) 
        |> List.ofSeq

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _-> failwith("Error: returns " + boolString)

    let damageTypeData = 
        makeFallenData
            "DamageTypeData.csv"
            (fun row -> (DamageType row.["desc"]))

    let calculatedEngageableOpponentsData = 
        makeFallenData 
            "CalculatedEngageableOpponentsData.csv"
            (fun row -> (
                string row.["desc"],
                uint   row.["engageableOpponents"]
            ))

    let engageableOpponentsCalculationData  = 
        makeFallenData 
            "EngageableOpponentsCalculationData.csv"
            (fun row -> {
                combatRollDivisor = uint row.["combatRollDivisor"]
                desc = string row.["desc"]
                maxEO = mapMaxEO row.["maxEO"]
            })

    let calculatedRangeData = 
        makeFallenData
            "CalculatedRangeData.csv"
            (fun row -> (
                string row.["desc"],
                uint   row.["effectiveRange"],
                uint   row.["maxRange"]
            ))
    
    let rangeCalculationData = 
        makeFallenData
            "RangeCalculationData.csv"
            (fun row -> (
                string     row.["desc"],
                uint       row.["numDicePerEffectiveRangeUnit"],
                uint       row.["ftPerEffectiveRangeUnit"],
                Bool       row.["roundEffectiveRangeUp"],
                uint       row.["maxRange"]
            ))
    
    let resourceClassData =
        makeFallenData
            "ResourceClassData.csv"
            (fun row -> (string row.["desc"]))
    
    let attributeData =
        makeFallenData
            "AttributeData.csv"
            (fun row -> (string row.["desc"]))

    let magicSkillData = 
        makeFallenData
            "MagicSkillData.csv"
            (fun row -> (
                string     row.["desc"],
                string     row.["damageTypes"],
                int        row.["rangeAdjustment"],
                Bool       row.["meleeCapable"],
                string     row.["magicResourceClass"]
            ))

    let magicCombatData = 
        makeFallenData
            "MagicCombatData.csv"
            (fun row -> (
                string     row.["Description"],
                int        row.["Lvl Requirment"],
                string     row.["Dice Modification"],
                string     row.["Penetration"],
                string     row.["Range"],
                string     row.["Engageable Opponents"],
                uint       row.["Resource Requirment"],
                Bool       row.["Vocation Assistable"],
                string     row.["Area Of Effect"]
            ))

    let weaponClassData =
        makeFallenData
            "WeaponClassData.csv"
            (fun row -> (
                string row.["desc"],
                string row.["oneHandedWeaponDice"],
                string row.["twoHandedWeaponDice"],
                string row.["penetration"],
                string row.["range"],
                string row.["damageTypes"],
                string row.["engageableOpponents"],
                string row.["dualWieldableBonus"],
                string row.["areaOfEffect"],
                string row.["resourceClass"],
                string row.["governingAttributes"]
            ))

    let conduitClassData =
        makeFallenData
            "ConduitClassData.csv"
            (fun row -> (
                string row.["desc"],
                string row.["oneHandedDice"],
                string row.["twoHandedDice"],
                string row.["penetration"],
                int    row.["rangeAdjustment"],
                string row.["damageTypes"],
                string row.["engageableOpponents"],
                string row.["dualWieldableBonus"],
                string row.["areaOfEffect"],
                string row.["resourceClass"],
                string row.["governingAttributes"]
            ))

    let itemTierData = 
        makeFallenData
            "ItemTierData.csv"
            (fun row -> (
                string row.["desc"],
                int    row.["level"],
                uint   row.["runeSlots"],
                string row.["baseDice"],
                uint   row.["durabilityMax"]
            ))

    let itemData = 
        makeFallenData
            "ItemData.csv"
            (fun row -> (
                string row.["desc"],
                string row.["itemClasses"],
                string row.["itemTier"],
                string row.["durability"],
                float  row.["weight"],
                string row.["value"]
                //uint row.["quantity"] This exists on the csv, but is for equipment table purposes on the goolge Fallen Character Sheet
            ))

    let weaponResourceClassData = 
        makeFallenData
            "WeaponResourceClassData.csv"
            (fun row -> (
                string row.["desc"],
                string row.["resourceClass"],
                string row.["resourceDice"],
                uint   row.["penetration"],
                string row.["range"],
                string row.["damageTypes"],
                string row.["areaOfEffect"]
            ))

    let movementSpeedCalculationData =
        makeFallenData
            "MovementSpeedCalculationData.csv"
            (fun row -> (
                string row.["desc"],
                uint   row.["baseMovementSpeed"],
                string row.["governingAttributes"],
                uint   row.["feetPerAttributeLvl"],
                string row.["governingSkill"],
                uint   row.["feetPerSkillLvl"]
            ))

    let carryWeightCalculationData = 
        makeFallenData
            "CarryWeightCalculationData.csv"
            (fun row -> (
                string row.["desc"],
                uint   row.["baseWeight"],
                string row.["governingAttribute"],
                uint   row.["weightIncreasePerAttribute"],
                string row.["governingSkill"],
                uint   row.["weightIncreasePerSkill"]
            ))

    let weightClassData =
        makeFallenData
            "WeightClassData.csv"
            (fun row -> (
                string row.["name"],
                float  row.["bottomPercent"],
                float  row.["topPercent"],
                float  row.["percentOfMovementSpeed"]
               
            ))

    let defenseClassData = 
        makeFallenData
            "DefenseClassData.csv"
            (fun row -> (
                string row.["desc"],
                float  row.["physicalDefense"],
                float  row.["mentalDefense"],
                float  row.["spiritualDefense"]
            ))

    let attributeDeterminedDiceModData =
        makeFallenData
            "AttributeDeterminedDiceModData.csv"
            (fun row -> (
                string row.["name"],
                string row.["attributesToEffect"],
                string row.["dicePoolModification"]
            ))

    let equipmentData = [|
        (true,             "Shoddy Club", 1u)
        //(true,       "Arming Sword",  1u )
        (true,       "Light Crossbow",  1u )
        (true,           "Bodkin Bolt", 1u )
        //(true,  "Adversary - Damaged",  1u )
        (true,          "Gambeson Coat", 1u)
        (true,         "Leather Helmet", 1u)
    |]

    let skillStatData = [|
        ("Endurance",          1, "STR")
        ("Athletics",          3, "STR")
        ("Climb",              1, "STR")
        ("Swim",               2, "STR")
        ("Lift",              -1, "STR")
        ("Perception",         2, "RFX")
        ("Acrobatics",         2, "RFX")
        ("Ride/Pilot",        -1, "RFX")
        ("Sleight of Hand",   -1, "RFX")
        ("Stealth",            0, "RFX")
        ("General Knowledge",  4, "INT")
        ("Willpower",          1, "INT")
        ("Communication",      1, "INT")
        ("Spiritual",          1, "INT")
        ("Survival",           1, "INT")
    |]

    let attributeStatData = [|
        ("STR",  0) 
        ("RFX",  1)
        ("INT",  3)
    |]

    let vocationalSkill1Data = [|
        ("Small Bladed", 2)
    |]

    let vocationalSkill2Data = [|
        ("Blazecraft", -1)
        //("Radiance", 4)
    |]

    let vocationalSkill3Data = [|
        //("Blazecraft", 4)
        ("Fire Rune", 1)
    |]

    let vocationDataArray = [|
        ("Warrior {STR}"          , 2, vocationalSkill1Data)
        (""                       , 0, vocationalSkill2Data)
        ("Fellcraft {STR,RFX,INT}", 2, vocationalSkill3Data)
    |]

    let effectsTableData = [|
        ( "Humaniod Movement Speed", "(Run Character Sheet Calculation)", "-")
        ( "Random ass effect lalal", "(Should be unchanged  lalalalala)", "-")
        ( "Defense Level",           "(Should be unchanged  lalalalala)", "-")
        //( "Level 5 Injury",                         "-1d6 to all checks", "-")
        ( "Inventory Weight",                       "-1d6 to all checks", "-")
    |]
