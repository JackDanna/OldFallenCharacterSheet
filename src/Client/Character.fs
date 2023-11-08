module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.CombatRoll
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.VocationGroup

type Model =
    { name: string
      coreSkillTables: CoreSkillGroup list
      vocationTables: VocationGroup list
      equipmentRowList: Equipment list
      combatRolls: CombatRoll list }

type Msg =
    | CoreSkillTablesMsg of CoreSkillGroups.Msg
    | VocationTableMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentRowListMsg of EquipmentList.Msg
    | SetDefault

let init (coreSkillGroups: CoreSkillGroup list) : Model =
    let attributeStatList = coreSkillGroupToAttributeStats coreSkillGroups

    { name = ""
      coreSkillTables = coreSkillGroups
      vocationTables = VocationGroupList.init attributeStatList
      equipmentRowList = EquipmentList.init ()
      combatRolls = [] }

let update
    (defaultCoreSkillTables: CoreSkillGroup list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (msg: Msg)
    (model: Model)
    : Model =
    let temp =
        CombatRollTable.update
            magicSkillMap
            magicCombatMap
            rangeMap
            (coreSkillGroupToAttributes defaultCoreSkillTables)
            []

    match msg with
    | SetDefault ->

        { model with
            coreSkillTables = defaultCoreSkillTables
            vocationTables =
                VocationGroupList.update
                    (coreSkillGroupToAttributeStats defaultCoreSkillTables)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationTables }
    | CoreSkillTablesMsg coreSkillTableMsg ->
        let newCoreSkillTables =
            CoreSkillGroups.update
                (collectEquipmentSkillAdjustments model.equipmentRowList)
                coreSkillTableMsg
                model.coreSkillTables

        let newVocationTables =
            VocationGroupList.update
                (coreSkillGroupToAttributeStats newCoreSkillTables)
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationTables

        { model with
            coreSkillTables = newCoreSkillTables
            vocationTables = newVocationTables
            combatRolls =
                temp
                    model.equipmentRowList
                    (coreSkillGroupToAttributeStats newCoreSkillTables)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls }

    | VocationTableMsg vocationTableMsg ->
        let newVocationTables =
            VocationGroupList.update
                (coreSkillGroupToAttributeStats model.coreSkillTables)
                vocationTableMsg
                model.vocationTables

        { model with
            vocationTables = newVocationTables

            combatRolls =
                temp
                    model.equipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillTables)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls }

    | SetName name -> { model with name = name }
    | EquipmentRowListMsg equipmentRowListMsg ->
        let newEquipmentRowList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentRowList

        { model with
            coreSkillTables =
                CoreSkillGroups.update
                    (collectEquipmentSkillAdjustments newEquipmentRowList)
                    CoreSkillGroups.Msg.RecalculateCoreSkillGroups
                    model.coreSkillTables
            equipmentRowList = newEquipmentRowList
            combatRolls =
                temp
                    newEquipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillTables)
                    model.vocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls }


open Feliz
open Feliz.Bulma

let view (combatVocationalSkill) (allItemList: Item list) (model: Model) (dispatch: Msg -> unit) =

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
            CoreSkillGroups.view model.coreSkillTables (CoreSkillTablesMsg >> dispatch)
        ]
        Bulma.container [
            VocationGroupList.view combatVocationalSkill model.vocationTables (VocationTableMsg >> dispatch)
        ]
        Bulma.container [
            EquipmentList.view
                (List.map (fun (item: Item) -> item.name) allItemList)
                model.equipmentRowList
                (EquipmentRowListMsg >> dispatch)
            |> Bulma.content
        ]
        Bulma.container [
            CombatRollTable.view model.combatRolls
        ]
    ]