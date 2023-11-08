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
    | CoreSkillGroupListMsg of CoreSkillGroupList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
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
            coreSkillTables = defaultCoreSkillTables
            vocationTables =
                VocationGroupList.update
                    (coreSkillGroupToAttributeStats defaultCoreSkillTables)
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationTables }
    | CoreSkillGroupListMsg coreSkillTableMsg ->
        let newCoreSkillTables =
            CoreSkillGroupList.update
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
                loadedCombatRollUpdate
                    model.equipmentRowList
                    (coreSkillGroupToAttributeStats newCoreSkillTables)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls }

    | VocationGroupListMsg vocationTableMsg ->
        let newVocationTables =
            VocationGroupList.update
                (coreSkillGroupToAttributeStats model.coreSkillTables)
                vocationTableMsg
                model.vocationTables

        { model with
            vocationTables = newVocationTables

            combatRolls =
                loadedCombatRollUpdate
                    model.equipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillTables)
                    newVocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls }

    | SetName name -> { model with name = name }
    | EquipmentListMsg equipmentRowListMsg ->
        let newEquipmentRowList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentRowList

        { model with
            coreSkillTables =
                CoreSkillGroupList.update
                    (collectEquipmentSkillAdjustments newEquipmentRowList)
                    CoreSkillGroupList.Msg.RecalculateCoreSkillGroups
                    model.coreSkillTables
            equipmentRowList = newEquipmentRowList
            combatRolls =
                loadedCombatRollUpdate
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
            CoreSkillGroupList.view model.coreSkillTables (CoreSkillGroupListMsg >> dispatch)
        ]
        Bulma.container [
            VocationGroupList.view combatVocationalSkill model.vocationTables (VocationGroupListMsg >> dispatch)
        ]
        Bulma.container [
            EquipmentList.view
                (List.map (fun (item: Item) -> item.name) allItemList)
                model.equipmentRowList
                (EquipmentListMsg >> dispatch)
            |> Bulma.content
        ]
        Bulma.container [
            CombatRollTable.view model.combatRolls
        ]
    ]