module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module FallenServerData =
    open FSharp.Data
    open FallenLib.DamageType
    open FallenLib.TypeUtils
    open FallenLib.EngageableOpponents
    open FallenLib.Range
    open FallenLib.ResourceClass
    open FallenLib.Attribute
    open FallenLib.MagicSkill
    open FallenLib.AreaOfEffect
    open FallenLib.Neg1To4
    open FallenLib.Dice
    open FallenLib.MagicCombat
    open FallenLib.WeaponClass
    open FallenLib.ConduitClass
    open FallenLib.PhysicalDefenseEffect
    open FallenLib.WeaponResourceClass
    open FallenLib.ItemTier
    open FallenLib.Item
    open FallenLib.MovementSpeedCalculation
    open FallenLib.CoreSkillGroup
    open FallenLib.SkillStat
    open FallenLib.SkillDiceModificationEffect
    open FallenLib.AttributeStatAdjustmentEffect
    open FallenLib.ItemEffect
    open FallenLib.ContainerClass
    open FallenLib.EffectForDisplay
    open FallenLib.CharacterEffect
    open FallenLib.CarryWeightCalculation

    let makeFallenDataPath fileName =
        __SOURCE_DIRECTORY__ + "/FallenData/" + fileName

    let makeFallenData fileName mappingFunc =
        CsvFile
            .Load(
                makeFallenDataPath fileName,
                hasHeaders = true
            )
            .Rows
        |> Seq.map (mappingFunc)
        |> List.ofSeq

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _ -> failwith ("Error: returns " + boolString)

    // DamageType
    let damageTypeData =
        makeFallenData "DamageTypeData.csv" (fun row -> (DamageType row.["desc"]))

    let stringToDamageTypeList =
        damageTypeData
        |> stringListToTypeMap
        |> stringAndMapToDamageTypeArray

    // EngageableOpponents
    let engageableOpponentsCalculationData =
        makeFallenData "EngageableOpponentsCalculationData.csv" (fun row ->
            { name = string row.["desc"]
              combatRollDivisor = uint row.["combatRollDivisor"]
              maxEO = mapMaxEO row.["maxEO"] })

    let engageableOpponentsMap =
        eoMap (eoCalculationListToMap engageableOpponentsCalculationData)

    // Range
    let calculatedRangeData =
        makeFallenData "CalculatedRangeData.csv" (fun row ->
            { name = string row.["desc"]
              effectiveRange = uint row.["effectiveRange"]
              maxRange = uint row.["maxRange"] })

    let rangeCalculationData =
        makeFallenData "RangeCalculationData.csv" (fun row ->
            { name = string row.["desc"]
              numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
              ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
              roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
              maxRange = uint row.["maxRange"] })

    let rangeMap = createRangeMap calculatedRangeData rangeCalculationData

    let rangeOptionMap string =
        match string with
        | "None" -> None
        | _ -> rangeMap.Item string |> Some

    // ResourceClass
    let resourceClassData =
        makeFallenData "ResourceClassData.csv" (fun row -> (ResourceClass row.["desc"]))

    let resourceClassMap = stringListToTypeMap resourceClassData

    let resourceClassOptionMap string =
        match string with
        | "None" -> None
        | _ -> Some <| resourceClassMap.Item string

    // AttributeAndCoreSkill
    let coreSkillGroupData: CoreSkillGroup list =
        makeFallenData "AttributeAndCoreSkillData.csv" (fun row ->
            let skillStatList: SkillStat list =
                List.collect
                    (fun (coreSkillString: string) ->
                        match coreSkillString with
                        | "None" -> []
                        | _ ->
                            let lvl = Zero

                            { name = coreSkillString
                              lvl = lvl
                              dicePool = coreSkillToDicePool baseDicePool lvl Zero [] }
                            |> List.singleton)
                    [ (string row.["Core Skill 1"])
                      (string row.["Core Skill 2"])
                      (string row.["Core Skill 3"])
                      (string row.["Core Skill 4"])
                      (string row.["Core Skill 5"]) ]

            { attributeStat =
                { attribute = Attribute row.["desc"]
                  lvl = Zero }
              coreSkillList = skillStatList })

    let attributeData = coreSkillGroupToAttributes coreSkillGroupData

    let attributeMap = stringListToTypeMap attributeData

    let mapAndStringToAttributes (attributeMap: Map<string, Attribute>) (input) =
        String.filter ((<>) ' ') input
        |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun attributeString -> attributeMap.Item attributeString)

    let stringToAttributes = mapAndStringToAttributes attributeMap

    // MagicSkill
    let magicSkillData =
        makeFallenData "MagicSkillData.csv" (fun row ->
            { name = string row.["desc"]
              damageTypes = stringToDamageTypeList (string row.["damageTypes"])
              rangeAdjustment = int row.["rangeAdjustment"]
              isMeleeCapable = Bool row.["meleeCapable"]
              magicResourceClass = string row.["magicResourceClass"] })

    let magicSkillMap =
        List.map (fun (magicSkill: MagicSkill) -> magicSkill.name, magicSkill) magicSkillData
        |> Map.ofList

    // MagicCombat
    let magicCombatData =
        makeFallenData "MagicCombatData.csv" (fun row ->
            { name = string row.["Description"]
              lvlRequirment = int row.["Lvl Requirment"] |> intToNeg1To4
              diceModification = stringToDicePoolModification row.["Dice Modification"]
              penetration = uint row.["Penetration"]
              range = rangeMap.Item(string row.["Range"])
              engageableOpponents = engageableOpponentsMap row.["Engageable Opponents"]
              minResourceRequirment = uint row.["Resource Requirment"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["Area Of Effect"] })

    let magicCombatMap =
        List.map (fun (magicCombat: MagicCombat) -> magicCombat.name, magicCombat) magicCombatData
        |> Map.ofList

    // WeaponClass
    let weaponClassData =
        makeFallenData "WeaponClassData.csv" (fun row ->
            { name = string row.["desc"]
              oneHandedWeaponDice = stringToDicePoolModificationOption row.["oneHandedWeaponDice"]
              twoHandedWeaponDice = stringToDicePoolModification row.["twoHandedWeaponDice"]
              penetration = uint row.["penetration"]
              range = rangeMap.Item row.["range"]
              damageTypes = stringToDamageTypeList row.["damageTypes"]
              engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
              dualWieldableBonus = stringToDicePoolModificationOption row.["dualWieldableBonus"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
              resourceClass = resourceClassOptionMap row.["resourceClass"] })

    let weaponClassMap =
        List.map (fun (weaponClass: WeaponClass) -> weaponClass.name, weaponClass) weaponClassData
        |> Map.ofList

    // ConduitClass
    let conduitClassData =
        makeFallenData "ConduitClassData.csv" (fun row ->

            { name = string row.["desc"]
              oneHandedDice = stringToDicePoolModificationOption row.["oneHandedDice"]
              twoHandedDice = stringToDicePoolModification row.["twoHandedDice"]
              penetration = uint row.["penetration"]
              rangeAdjustment = int row.["rangeAdjustment"]
              damageTypes = stringToDamageTypeList row.["damageTypes"]
              engageableOpponents =
                match row.["engageableOpponents"] with
                | "None" -> None
                | something -> Some(engageableOpponentsMap something)
              dualWieldableBonus = stringToDicePoolModificationOption row.["dualWieldableBonus"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
              resourceClass = resourceClassOptionMap row.["resourceClass"]
              effectedMagicSkills =
                row.["effectedMagicSkills"].Split ", "
                |> List.ofArray
                |> List.map (fun magicSkillStr -> magicSkillMap.Item magicSkillStr) })

    let conduitClassMap =
        List.map (fun (conduitClass: ConduitClass) -> conduitClass.name, conduitClass) conduitClassData
        |> Map.ofList

    // ContainerClass
    let containerClassData =
        makeFallenData "ContainerClassData.csv" (fun row ->
            { name = string row.["Name"]
              weightCapacity = float row.["Weight Capacity"] })

    let containerClassMap =
        List.map (fun (containerClass: ContainerClass) -> containerClass.name, containerClass) containerClassData
        |> Map.ofList

    // DefenseClass
    let defenseClassData: PhysicalDefenseEffect list =
        makeFallenData "PhysicalDefenseEffect.csv" (fun row ->
            { name = string row.["desc"]
              physicalDefense = float row.["physicalDefense"] })

    let defenseClassMap =
        List.map (fun (defenseClass: PhysicalDefenseEffect) -> defenseClass.name, defenseClass) defenseClassData
        |> Map.ofList

    // SkillDiceModificationEffect
    let skillDiceModificationEffectData: SkillDiceModificationEffect list =
        makeFallenData "SkillDiceModificationEffect.csv" (fun row ->
            { name = string row.["Name"]
              skill = string row.["Skill"]
              diceMod = stringToDicePoolModification row.["Dice Modification"] })

    let skillDiceModificationEffectMap =
        skillDiceModificationEffectData
        |> List.map (fun (skillAdjustment: SkillDiceModificationEffect) -> skillAdjustment.name, skillAdjustment)
        |> Map.ofList

    // AttributeStatAdjustmentEffect
    let attributeStatAdjustmentEffectData =
        makeFallenData "AttributeStatAdjustmentEffect.csv" (fun row ->
            { name = string row.["Name"]
              attribute = Attribute row.["Attribute"]
              adjustment = int row.["Adjustment"] })

    let attributeStatAdjustmentEffectMap =
        attributeStatAdjustmentEffectData
        |> List.map (fun (attributeStatAdjustmentEffect: AttributeStatAdjustmentEffect) ->
            attributeStatAdjustmentEffect.name, attributeStatAdjustmentEffect)
        |> Map.ofList

    // AttributeDeterminedDiceMod
    let attributeDeterminedDiceModEffectData =
        makeFallenData "AttributeDeterminedDIceModData.csv" (fun row ->
            { name = row.["name"]
              attributesToEffect = stringToAttributes row.["attributesToEffect"]
              dicePoolModification = stringToDicePoolModification row.["dicePoolModification"] })

    let attributeDeterminedDiceModEffectMap =
        attributeDeterminedDiceModEffectData
        |> List.map (fun (attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect) ->
            attributeDeterminedDiceModEffect.name, attributeDeterminedDiceModEffect)
        |> Map.ofList

    // WeightClass
    let weightClassData =
        makeFallenData "AttributeDeterminedDiceModData.csv" (fun row ->
            { name = row.["name"]
              bottomPercent = float row.["bottomPercent"]
              topPercent = float row.["topPercent"]
              percentOfMovementSpeed = float row.["percentOfMovementSpeed"]
              attributeDeterminedDiceModEffect =
                attributeDeterminedDiceModEffectMap.Item row.["attributeDeterminedDiceModEffect"] })

    // ItemEffect
    let itemEffectData: ItemEffect list =
        List.map DefenseClass defenseClassData
        @ List.map SkillDiceModificationEffect skillDiceModificationEffectData
          @ List.map AttributeStatAdjustmentEffect attributeStatAdjustmentEffectData

    let itemEffectDataMap =
        itemEffectData
        |> List.map (fun (itemEffect: ItemEffect) ->
            match itemEffect with
            | SkillDiceModificationEffect sdme -> sdme.name, SkillDiceModificationEffect sdme
            | AttributeStatAdjustmentEffect asae -> asae.name, AttributeStatAdjustmentEffect asae
            | DefenseClass dc -> dc.name, DefenseClass dc)
        |> Map.ofList

    // CharacterEffectForDisplay
    let characterEffectForDisplayData: EffectForDisplay list =
        makeFallenData "CharacterEffectForDisplayData.csv" (fun row ->
            { name = string row.["Name"]
              effect = string row.["Effect"]
              durationAndSource =
                { duration = string row.["Duration"]
                  source = string row.["Source"] } })

    // CharacterEffect
    let characterEffectData: CharacterEffect list =
        let skillDiceModificationEffectForDisplayList =
            List.map skillDiceModificationEffectToForDisplay skillDiceModificationEffectData

        List.map EffectForDisplay characterEffectForDisplayData
        @ List.map SkillDiceModificationEffectForDisplay skillDiceModificationEffectForDisplayList

    let characterEffectMap: Map<string, CharacterEffect> =
        characterEffectData
        |> List.map (fun (characterEffect: CharacterEffect) ->
            match characterEffect with
            | EffectForDisplay efd -> efd.name, EffectForDisplay efd
            | SkillDiceModificationEffectForDisplay (sdme, durationAndSource) ->
                sdme.name, SkillDiceModificationEffectForDisplay(sdme, durationAndSource))
        |> Map.ofList

    // WeaponResourceClass
    let weaponResourceClassData =
        makeFallenData "WeaponResourceClassData.csv" (fun row ->
            { name = string row.["desc"]
              resourceClass = resourceClassMap.Item row.["resourceClass"]
              resourceDice = stringToDicePoolModification row.["resourceDice"]
              penetration = uint row.["penetration"]
              range = rangeOptionMap row.["range"]
              damageTypes = stringToDamageTypeList row.["damageTypes"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"] })

    let weaponResourceClassMap =
        List.map
            (fun (weaponResourceClass: WeaponResourceClass) -> weaponResourceClass.name, weaponResourceClass)
            weaponResourceClassData
        |> Map.ofList

    // ItemTier
    let itemTierData =
        makeFallenData "ItemTierData.csv" (fun row ->
            { name = string row.["desc"]
              level = int row.["level"]
              runeSlots = uint row.["runeSlots"]
              baseDice = stringToDicePool row.["baseDice"]
              durabilityMax = uint row.["durabilityMax"] })

    let itemTierMap =
        List.map (fun (itemTier: ItemTier) -> itemTier.name, itemTier) itemTierData
        |> Map.ofList

    // Item
    let stringToItemClassList
        (weaponClassMap: Map<string, WeaponClass>)
        (conduitClassMap: Map<string, ConduitClass>)
        (weaponResourceClassMap: Map<string, WeaponResourceClass>)
        (input: string)
        =
        input.Split ", "
        |> List.ofArray
        |> List.collect (fun className ->
            match className with
            | weaponClassName when weaponClassMap.Keys.Contains weaponClassName ->
                weaponClassMap.Item weaponClassName
                |> WeaponClass
                |> List.singleton
            | conduitClassName when conduitClassMap.Keys.Contains conduitClassName ->
                conduitClassMap.Item conduitClassName
                |> ConduitClass
                |> List.singleton
            | weaponResourceClassName when weaponResourceClassMap.Keys.Contains weaponResourceClassName ->
                weaponResourceClassMap.Item weaponResourceClassName
                |> WeaponResourceClass
                |> List.singleton
            | containerClassName when containerClassMap.Keys.Contains containerClassName ->
                containerClassMap.Item containerClassName
                |> ContainerClass
                |> List.singleton
            | itemEffectName when itemEffectDataMap.Keys.Contains itemEffectName ->
                itemEffectDataMap.Item itemEffectName
                |> ItemEffect
                |> List.singleton
            | _ -> [])

    let itemData =
        makeFallenData "ItemData.csv" (fun row ->
            { name = string row.["desc"]
              itemClasses =
                stringToItemClassList weaponClassMap conduitClassMap weaponResourceClassMap row.["itemClasses"]
              itemTier = itemTierMap.Item row.["itemTier"]
              value = string row.["value"]
              weight = float row.["weight"] })

    // MovementSpeedCalculation
    let movementSpeedCalculationData =
        makeFallenData "MovementSpeedCalculationData.csv" (fun row ->
            { name = string row.["desc"]
              baseMovementSpeed = uint row.["baseMovementSpeed"]
              governingAttributes = stringToAttributes row.["governingAttributes"]
              feetPerAttributeLvl = uint row.["feetPerAttributeLvl"]
              governingSkill = string row.["governingSkill"]
              feetPerSkillLvl = uint row.["feetPerSkillLvl"] })

    // CarryWeightCalculation
    let carryWeightCalculationData =
        makeFallenData "CarryWeightCalculationData.csv" (fun row ->
            (string row.["desc"],
             uint row.["baseWeight"],
             string row.["governingAttribute"],
             uint row.["weightIncreasePerAttribute"],
             string row.["governingSkill"],
             uint row.["weightIncreasePerSkill"]))

    let combatVocationalSkill =
        List.append
            (List.map (fun (weaponClassData: WeaponClass) -> weaponClassData.name) weaponClassData)
            (List.map (fun (magicSkill: MagicSkill) -> magicSkill.name) magicSkillData)

let fallenDataApi: IFallenDataApi =
    { getInitData =
        fun () ->
            async {
                return
                    (FallenServerData.coreSkillGroupData,
                     FallenServerData.itemData,
                     FallenServerData.magicSkillMap,
                     FallenServerData.magicCombatMap,
                     FallenServerData.rangeMap,
                     FallenServerData.characterEffectMap,
                     FallenServerData.combatVocationalSkill)
            } }


let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue fallenDataApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0