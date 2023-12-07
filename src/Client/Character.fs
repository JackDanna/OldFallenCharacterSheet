module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.Character
open FallenLib.CharacterEffect
open FallenLib.CarryWeightEffect
open FallenLib.MovementSpeedCalculation

type Msg =
    | CoreSkillGroupListMsg of CoreSkillGroupList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | ContainerListMsg of ContainerList.Msg
    | DestinyPointsMsg of DestinyPoints.Msg
    | CharacterEffectListMsg of CharacterEffectList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg
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
      characterEffectList = []
      characterInformation = CharacterInformation.init () }

let update
    (defaultCoreSkillTables: CoreSkillGroup list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (characterEffectMap: Map<string, CharacterEffect>)
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
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects model.equipmentList model.characterEffectList

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

        // 1nd, update the core skill stats without factoring in the skillDiceModification list since we update coreSkillTables again in step 4
        let newCoreSkillTables =
            CoreSkillGroupList.update [] [] coreSkillTableMsg model.coreSkillGroupList

        // 2rd, update the character effects based on the new skill stats
        let newCharacterEffectList =
            CharacterEffectList.update
                newCoreSkillTables
                (calculateCharacterWeight model.equipmentList model.containerList)
                carryWeightCalculationMap
                weightClassList
                characterEffectMap
                movementSpeedCalculationMap
                CharacterEffectList.Msg.RecalculateCarryWeightAndMovementSpeed
                model.characterEffectList

        // 3th, grab the new skillAdjustments from both the itms and character effects
        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects model.equipmentList model.characterEffectList

        // 4th, with the new CharacterEffects, update the skill Dice pools
        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                newCoreSkillTables

        // 5th, update the Vocation tables, which only needs the Attribute Stats
        let newVocationTables =
            VocationGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                (coreSkillGroupListToAttributeStats newCoreSkillTablesWithSkillAdjustments)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            coreSkillGroupList = newCoreSkillTablesWithSkillAdjustments
            vocationGroupList = newVocationTables
            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupListToAttributeStats newCoreSkillTablesWithSkillAdjustments)
                    newVocationTables

            characterEffectList = newCharacterEffectList }

    | VocationGroupListMsg vocationTableMsg ->
        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects model.equipmentList model.characterEffectList

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

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects newEquipmentList model.characterEffectList

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

            characterEffectList =
                CharacterEffectList.update
                    newCoreSkillGroupList
                    (calculateCharacterWeight newEquipmentList model.containerList)
                    carryWeightCalculationMap
                    weightClassList
                    characterEffectMap
                    movementSpeedCalculationMap
                    CharacterEffectList.Msg.RecalculateCarryWeightAndMovementSpeed
                    model.characterEffectList }

    | ContainerListMsg containerListMsg ->
        let newContainerList =
            ContainerList.update allItemList containerListMsg model.containerList

        let newCharacterEffectList =
            CharacterEffectList.update
                model.coreSkillGroupList
                (calculateCharacterWeight model.equipmentList newContainerList)
                carryWeightCalculationMap
                weightClassList
                characterEffectMap
                movementSpeedCalculationMap
                CharacterEffectList.Msg.RecalculateCarryWeightAndMovementSpeed
                model.characterEffectList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects model.equipmentList newCharacterEffectList

        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            containerList = newContainerList
            characterEffectList = newCharacterEffectList
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
            CharacterEffectList.update
                model.coreSkillGroupList
                (calculateCharacterWeight model.equipmentList model.containerList)
                carryWeightCalculationMap
                weightClassList
                characterEffectMap
                movementSpeedCalculationMap
                msg
                model.characterEffectList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects model.equipmentList newCharacterEffectList

        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                newAttributeDeterminedDiceModEffects
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            characterEffectList = newCharacterEffectList
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

open Feliz
open Feliz.Bulma

let view
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

        //CharacterInformation.view

        CoreSkillGroupList.view model.coreSkillGroupList (CoreSkillGroupListMsg >> dispatch)

        VocationGroupList.view combatVocationalSkill model.vocationGroupList (VocationGroupListMsg >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        CharacterEffectList.view characterEffectKeyList model.characterEffectList (CharacterEffectListMsg >> dispatch)

        ItemEffectList.view (getEquipedEffectItems model.equipmentList)

        EquipmentList.view allItemNameList model.equipmentList (EquipmentListMsg >> dispatch)

        CombatRollTable.view model.combatRollList

        ContainerList.view
            (List.collect collectContainerClassItemName allItemList)
            allItemNameList
            model.containerList
            (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]