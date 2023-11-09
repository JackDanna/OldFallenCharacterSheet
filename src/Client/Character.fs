module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.Character

type Msg =
    | CoreSkillGroupListMsg of CoreSkillGroupList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | SetDefault

let init (coreSkillGroups: CoreSkillGroup list) : Character =
    let attributeStatList = coreSkillGroupToAttributeStats coreSkillGroups

    { name = ""
      coreSkillGroupList = coreSkillGroups
      vocationGroupList = VocationGroupList.init attributeStatList
      equipmentList = EquipmentList.init ()
      combatRollList = [] }

let update
    (defaultCoreSkillTables: CoreSkillGroup list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
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

        { model with
            coreSkillGroupList =
                CoreSkillGroupList.update
                    (collectEquipmentSkillAdjustments newEquipmentRowList)
                    CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                    model.coreSkillGroupList
            equipmentList = newEquipmentRowList
            combatRollList =
                loadedCombatRollUpdate
                    newEquipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillGroupList)
                    model.vocationGroupList
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRollList }

open Feliz
open Feliz.Bulma

let view (combatVocationalSkill) (allItemList: Item list) (model: Character) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.content [
            Bulma.input.text [
                prop.value model.name
                prop.placeholder "Character Name"
                prop.onChange (fun newName -> SetName newName |> dispatch)
                prop.classes [
                    "is-large"
                    "has-text-centered"
                ]
            ]
        ]
        Bulma.container [
            CoreSkillGroupList.view model.coreSkillGroupList (CoreSkillGroupListMsg >> dispatch)
        ]
        Bulma.container [
            VocationGroupList.view combatVocationalSkill model.vocationGroupList (VocationGroupListMsg >> dispatch)
        ]
        Bulma.container [
            ItemEffectList.view (getEquipedEffectItems model.equipmentList)
            |> Bulma.content
        ]
        Bulma.container [
            EquipmentList.view
                (List.map (fun (item: Item) -> item.name) allItemList)
                model.equipmentList
                (EquipmentListMsg >> dispatch)
            |> Bulma.content
        ]
        Bulma.container [
            CombatRollTable.view model.combatRollList
            |> Bulma.content
        ]
    ]