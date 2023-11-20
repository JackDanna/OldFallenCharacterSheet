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

type Msg =
    | CoreSkillGroupListMsg of CoreSkillGroupList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | ContainerListMsg of ContainerList.Msg
    | DestinyPointsMsg of DestinyPoints.Msg
    | CharacterEffectListMsg of CharacterEffectList.Msg
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
      characterEffectList = [] }

let update
    (defaultCoreSkillTables: CoreSkillGroup list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (characterEffectMap: Map<string, CharacterEffect>)
    (carryWeightCalculationMap: Map<string, CarryWeightCalculation>)
    (weightClassList: WeightClass list)
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
        let newSkillAdjustments =
            collectEquipmentSkillAdjustments model.equipmentList
            @ collectCharacterSkillDiceModifications model.characterEffectList

        { model with
            coreSkillGroupList = defaultCoreSkillTables
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    (coreSkillGroupListToAttributeStats defaultCoreSkillTables)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

    | CoreSkillGroupListMsg coreSkillTableMsg ->
        // 1nd, update the core skill stats without factoring in the skillDiceModification list since we update coreSkillTables again in step 4
        let newCoreSkillTables =
            CoreSkillGroupList.update [] coreSkillTableMsg model.coreSkillGroupList

        // 2rd, update the character effects based on the new skill stats
        let newCharacterEffectList =
            CharacterEffectList.update
                newCoreSkillTables
                (calculateCharacterWeight model.equipmentList model.containerList)
                carryWeightCalculationMap
                weightClassList
                characterEffectMap
                CharacterEffectList.Msg.RecalculateCarryWeightAndMovementSpeed
                model.characterEffectList

        // 3th, grab the new skillAdjustments from both the itms and character effects
        let newSkillAdjustments =
            collectEquipmentSkillAdjustments model.equipmentList
            @ collectCharacterSkillDiceModifications model.characterEffectList

        // 4th, with the new CharacterEffects, update the skill Dice pools
        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                newCoreSkillTables

        // 5th, update the Vocation tables, which only needs the Attribute Stats
        let newVocationTables =
            VocationGroupList.update
                newSkillAdjustments
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
                    CombatRollTable.Msg.RecalculateCombatRolls
                    model.combatRollList
            characterEffectList = newCharacterEffectList }

    | VocationGroupListMsg vocationTableMsg ->
        let newSkillAdjustments =
            collectEquipmentSkillAdjustments model.equipmentList
            @ collectCharacterSkillDiceModifications model.characterEffectList

        let newVocationTables =
            VocationGroupList.update
                newSkillAdjustments
                (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                vocationTableMsg
                model.vocationGroupList

        { model with
            vocationGroupList = newVocationTables
            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                    newVocationTables
                    CombatRollTable.Msg.RecalculateCombatRolls
                    model.combatRollList }

    | SetName newName -> { model with name = newName }

    | EquipmentListMsg equipmentRowListMsg ->
        let newSkillAdjustments =
            collectEquipmentSkillAdjustments model.equipmentList
            @ collectCharacterSkillDiceModifications model.characterEffectList

        let newEquipmentRowList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentList

        let newVocationGroupList =
            VocationGroupList.update
                newSkillAdjustments
                (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            coreSkillGroupList =
                CoreSkillGroupList.update
                    newSkillAdjustments
                    CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                    model.coreSkillGroupList
            vocationGroupList = newVocationGroupList
            equipmentList = newEquipmentRowList
            combatRollList =
                loadedCombatRollUpdate
                    newEquipmentRowList
                    (coreSkillGroupListToAttributeStats model.coreSkillGroupList)
                    newVocationGroupList
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRollList }

    | ContainerListMsg containerListMsg ->
        { model with containerList = ContainerList.update allItemList containerListMsg model.containerList }

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
                msg
                model.characterEffectList

        let newSkillAdjustments =
            collectEquipmentSkillAdjustments model.equipmentList
            @ collectCharacterSkillDiceModifications newCharacterEffectList

        let newCoreSkillTablesWithSkillAdjustments =
            CoreSkillGroupList.update
                newSkillAdjustments
                CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                model.coreSkillGroupList

        { model with
            characterEffectList = newCharacterEffectList
            coreSkillGroupList = newCoreSkillTablesWithSkillAdjustments
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    (coreSkillGroupListToAttributeStats newCoreSkillTablesWithSkillAdjustments)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

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

        CoreSkillGroupList.view model.coreSkillGroupList (CoreSkillGroupListMsg >> dispatch)

        VocationGroupList.view combatVocationalSkill model.vocationGroupList (VocationGroupListMsg >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        CharacterEffectList.view characterEffectKeyList model.characterEffectList (CharacterEffectListMsg >> dispatch)

        ItemEffectList.view (getEquipedEffectItems model.equipmentList)

        EquipmentList.view allItemNameList model.equipmentList (EquipmentListMsg >> dispatch)

        CombatRollTable.view model.combatRollList

        ContainerList.view
            (List.collect collectItemNameWithContainerClasses allItemList)
            allItemNameList
            model.containerList
            (ContainerListMsg >> dispatch)
    ]