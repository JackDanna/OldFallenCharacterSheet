module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.Character
open FallenLib.EffectForDisplay
open FallenLib.CarryWeightCalculation
open FallenLib.MovementSpeedEffect
open FallenLib.WeightClass

type Msg =
    | CoreSkillGroupListMsg of CoreSkillGroupList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | ContainerListMsg of ContainerList.Msg
    | DestinyPointsMsg of DestinyPoints.Msg
    | CharacterEffectListMsg of CharacterEffectForDisplayList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg
    | CarryWeightStatOptionMsg of CarryWeightStatOption.Msg
    | SetDefault

let init (coreSkillGroups: CoreSkillGroup list) : Character =
    let attributeStatList = coreSkillGroupListToAttributeStats coreSkillGroups

    { name = ""
      coreSkillGroupList = coreSkillGroups
      vocationGroupList = VocationGroupList.init attributeStatList
      equipmentList = EquipmentList.init ()
      combatRollList = []
      containerList = []
      destinyPoints = DestinyPoints.init ()
      characterEffectForDisplayList = []
      equipmentEffectForDisplayList = []
      characterInformation = CharacterInformation.init ()
      carryWeightStatOption = None }

let update
    (defaultCoreSkillTables: CoreSkillGroup list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (characterEffectMap: Map<string, EffectForDisplay>)
    (carryWeightCalculationMap: Map<string, CarryWeightCalculation>)
    (weightClassList: WeightClass list)
    (movementSpeedCalculationMap: Map<string, MovementSpeedCalculation>)
    (msg: Msg)
    (model: Character)
    : Character =

    let loadedCombatRollUpdate =
        CombatRollTable.update
            magicSkillMap
            magicCombatMap
            rangeMap
            (coreSkillGroupListToAttributes defaultCoreSkillTables)
            []

    match msg with
    | SetDefault ->

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                model.characterEffectForDisplayList

        { model with
            coreSkillGroupList = defaultCoreSkillTables
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    (coreSkillGroupListToAttributeStats defaultCoreSkillTables)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

    | CoreSkillGroupListMsg coreSkillTableMsg ->

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                model.characterEffectForDisplayList

        let newCoreSkillTables =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                coreSkillTableMsg
                model.coreSkillGroupList

        let newVocationTables =
            VocationGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                (coreSkillGroupListToAttributeStats newCoreSkillTables)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            coreSkillGroupList = newCoreSkillTables
            vocationGroupList = newVocationTables
            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupListToAttributeStats newCoreSkillTables)
                    newVocationTables
            carryWeightStatOption =
                CarryWeightStatOption.update
                    (calculateCharacterWeight model.equipmentList model.containerList)
                    newCoreSkillTables
                    carryWeightCalculationMap
                    weightClassList
                    CarryWeightStatOption.Msg.Recalculate
                    model.carryWeightStatOption }

    | VocationGroupListMsg vocationTableMsg ->
        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                model.characterEffectForDisplayList

        let newVocationTables =
            VocationGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                vocationTableMsg
                model.vocationGroupList

        { model with
            vocationGroupList = newVocationTables
            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                    newVocationTables }

    | SetName newName -> { model with name = newName }

    | EquipmentListMsg equipmentRowListMsg ->

        let newEquipmentList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentList

        let newEquipmentEffectForDisplayList =
            newEquipmentList
            |> equipmentListToEquipedEquipmentEffects
            |> List.map (fun (itemName, itemEffect) ->
                itemEffectToEffectForDisplay
                    (carryWeightStatOptionToPercentOfMovementSpeed model.carryWeightStatOption)
                    model.coreSkillGroupList
                    itemEffect
                    itemName)

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                newEquipmentEffectForDisplayList
                model.characterEffectForDisplayList

        let newVocationGroupList =
            VocationGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        let newCoreSkillGroupList =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            coreSkillGroupList = newCoreSkillGroupList
            vocationGroupList = newVocationGroupList
            equipmentList = newEquipmentList
            combatRollList =
                loadedCombatRollUpdate
                    newEquipmentList
                    (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                    newVocationGroupList

            characterEffectForDisplayList =
                CharacterEffectForDisplayList.update
                    newCoreSkillGroupList
                    (calculateCharacterWeight newEquipmentList model.containerList)
                    (carryWeightStatOptionToPercentOfMovementSpeed model.carryWeightStatOption)
                    weightClassList
                    characterEffectMap
                    movementSpeedCalculationMap
                    CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                    model.characterEffectForDisplayList }

    | ContainerListMsg containerListMsg ->
        let newContainerList =
            ContainerList.update allItemList containerListMsg model.containerList

        let newCharacterEffectList =
            CharacterEffectForDisplayList.update
                model.coreSkillGroupList
                (calculateCharacterWeight model.equipmentList newContainerList)
                (carryWeightStatOptionToPercentOfMovementSpeed model.carryWeightStatOption)
                weightClassList
                characterEffectMap
                movementSpeedCalculationMap
                CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                model.characterEffectForDisplayList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                newCharacterEffectList

        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            containerList = newContainerList
            characterEffectForDisplayList = newCharacterEffectList
            coreSkillGroupList = newCoreSkillTablesWithSkillAdjustments
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    (coreSkillGroupListToAttributeStats newCoreSkillTablesWithSkillAdjustments)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

    | DestinyPointsMsg destinyPointsMsg ->
        { model with destinyPoints = DestinyPoints.update destinyPointsMsg model.destinyPoints }

    | CharacterEffectListMsg msg ->
        let newCharacterEffectList =
            CharacterEffectForDisplayList.update
                model.coreSkillGroupList
                (calculateCharacterWeight model.equipmentList model.containerList)
                (carryWeightStatOptionToPercentOfMovementSpeed model.carryWeightStatOption)
                weightClassList
                characterEffectMap
                movementSpeedCalculationMap
                msg
                model.characterEffectForDisplayList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                newCharacterEffectList

        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            characterEffectForDisplayList = newCharacterEffectList
            coreSkillGroupList = newCoreSkillTablesWithSkillAdjustments
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    (coreSkillGroupListToAttributeStats newCoreSkillTablesWithSkillAdjustments)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }
    | CharacterInformationMsg msg ->
        { model with characterInformation = CharacterInformation.update msg model.characterInformation }

    | CarryWeightStatOptionMsg msg ->
        { model with
            carryWeightStatOption =
                CarryWeightStatOption.update
                    (calculateCharacterWeight model.equipmentList model.containerList)
                    model.coreSkillGroupList
                    carryWeightCalculationMap
                    weightClassList
                    msg
                    model.carryWeightStatOption }

open Feliz
open Feliz.Bulma

let view
    (carryWeightCalculationNameList: string list)
    (characterEffectKeyList: string list)
    (combatVocationalSkill)
    (allItemList: Item list)
    (model: Character)
    (dispatch: Msg -> unit)
    =

    let allItemNameList = (List.map (fun (item: Item) -> item.name) allItemList)

    Bulma.container [

        Bulma.input.text [
            prop.value model.name
            prop.placeholder "Character Name"
            prop.onTextChange (SetName >> dispatch)
            prop.classes [
                "is-large"
                "has-text-centered"
            ]
        ]
        |> Bulma.content

        Bulma.image [
            Html.img [
                prop.style [
                    style.height 500
                    style.width 500
                ]
                prop.classes [ "center" ]

                prop.src "https://cogentroleplaycommunity.github.io/Fallen/src/Characters/PC/JavkWick/Javk-Wick.png"
            ]
        ]
        |> Bulma.content

        CoreSkillGroupList.view model.coreSkillGroupList (CoreSkillGroupListMsg >> dispatch)

        VocationGroupList.view combatVocationalSkill model.vocationGroupList (VocationGroupListMsg >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        CharacterEffectForDisplayList.view
            characterEffectKeyList
            model.characterEffectForDisplayList
            (CharacterEffectListMsg >> dispatch)

        CarryWeightStatOption.view
            carryWeightCalculationNameList
            model.carryWeightStatOption
            (CarryWeightStatOptionMsg >> dispatch)

        EquipmentEffectForDisplayList.view model.equipmentEffectForDisplayList

        EquipmentList.view allItemNameList model.equipmentList (EquipmentListMsg >> dispatch)

        CombatRollTable.view model.combatRollList

        ContainerList.view
            (List.collect itemToContainerClassNames allItemList)
            allItemNameList
            model.containerList
            (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]