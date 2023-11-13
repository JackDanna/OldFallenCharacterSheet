module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.Character
open FallenLib.CharacterEffect

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
    let attributeStatList = coreSkillGroupToAttributeStats coreSkillGroups

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
    (msg: Msg)
    (model: Character)
    : Character =
    let loadedCombatRollUpdate =
        CombatRollTable.update
            magicSkillMap
            magicCombatMap
            rangeMap
            (coreSkillGroupToAttributes defaultCoreSkillTables)
            []

    match msg with
    | SetDefault ->
        { model with
            coreSkillGroupList = defaultCoreSkillTables
            vocationGroupList =
                VocationGroupList.update
                    (collectEquipmentSkillAdjustments model.equipmentList)
                    (coreSkillGroupToAttributeStats defaultCoreSkillTables)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

    | CoreSkillGroupListMsg coreSkillTableMsg ->
        let newCoreSkillTables =
            CoreSkillGroupList.update
                (collectEquipmentSkillAdjustments model.equipmentList)
                coreSkillTableMsg
                model.coreSkillGroupList

        let newVocationTables =
            VocationGroupList.update
                (collectEquipmentSkillAdjustments model.equipmentList)
                (coreSkillGroupToAttributeStats newCoreSkillTables)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            coreSkillGroupList = newCoreSkillTables
            vocationGroupList = newVocationTables
            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupToAttributeStats newCoreSkillTables)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRollList }

    | VocationGroupListMsg vocationTableMsg ->
        let newVocationTables =
            VocationGroupList.update
                (collectEquipmentSkillAdjustments model.equipmentList)
                (coreSkillGroupToAttributeStats model.coreSkillGroupList)
                vocationTableMsg
                model.vocationGroupList

        { model with
            vocationGroupList = newVocationTables

            combatRollList =
                loadedCombatRollUpdate
                    model.equipmentList
                    (coreSkillGroupToAttributeStats model.coreSkillGroupList)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRollList }

    | SetName name -> { model with name = name }

    | EquipmentListMsg equipmentRowListMsg ->
        let newEquipmentRowList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentList

        let newVocationGroupList =
            VocationGroupList.update
                (collectEquipmentSkillAdjustments newEquipmentRowList)
                (coreSkillGroupToAttributeStats model.coreSkillGroupList)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            coreSkillGroupList =
                CoreSkillGroupList.update
                    (collectEquipmentSkillAdjustments newEquipmentRowList)
                    CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                    model.coreSkillGroupList
            vocationGroupList = newVocationGroupList
            equipmentList = newEquipmentRowList
            combatRollList =
                loadedCombatRollUpdate
                    newEquipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillGroupList)
                    newVocationGroupList
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRollList }

    | ContainerListMsg containerListMsg ->
        { model with containerList = ContainerList.update allItemList containerListMsg model.containerList }
    | DestinyPointsMsg destinyPointsMsg ->
        { model with destinyPoints = DestinyPoints.update destinyPointsMsg model.destinyPoints }

    | CharacterEffectListMsg msg ->
        { model with characterEffectList = CharacterEffectList.update characterEffectMap msg model.characterEffectList }

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
            prop.onChange (SetName >> dispatch)
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