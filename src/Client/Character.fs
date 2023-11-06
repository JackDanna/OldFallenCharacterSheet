module Character

open Elmish
open Fable.Remoting.Client
open Shared

open FallenLib.Dice
open FallenLib.CoreSkillGroup
open FallenLib.Attribute
open FallenLib.Item
open FallenLib.CombatRoll
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range

type Model =
    { name: string
      coreSkillTables: CoreSkillGroups.Model
      vocationTables: VocationTables.Model
      equipmentRowList: EquipmentRowList.Model
      combatRolls: CombatRoll list
      AllItemList: Item list
      magicSkillMap: Map<string, MagicSkill>
      magicCombatMap: Map<string, MagicCombat>
      rangeMap: Map<string, Range>
      combatVocationalSkill: string list }

type Msg =
    | CoreSkillTablesMsg of CoreSkillGroups.Msg
    | VocationTableMsg of VocationTables.Msg
    | SetName of string
    | GotInitData of
        CoreSkillGroup list *
        Item list *
        Map<string, MagicSkill> *
        Map<string, MagicCombat> *
        Map<string, Range> *
        string list
    | EquipmentRowListMsg of EquipmentRowList.Msg

let fallenDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFallenDataApi>

let init () : Model * Cmd<Msg> =

    { name = "Javk Wick"
      coreSkillTables = []
      vocationTables = []
      equipmentRowList = EquipmentRowList.init ()
      combatRolls = []
      AllItemList = []
      magicSkillMap = Map.empty
      magicCombatMap = Map.empty
      rangeMap = Map.empty
      combatVocationalSkill = [] },
    Cmd.OfAsync.perform fallenDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let temp =
        CombatRollTable.update model.magicSkillMap model.magicCombatMap model.rangeMap [ "STR"; "RFX"; "INT" ] []

    match msg with
    | CoreSkillTablesMsg coreSkillTableMsg ->
        let newCoreSkillTables =
            CoreSkillGroups.update coreSkillTableMsg model.coreSkillTables

        let newVocationTables =
            VocationTables.update
                (coreSkillGroupToAttributeStats newCoreSkillTables)
                VocationTables.Msg.SetAttributeStatsAndCalculateDicePools
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
                    model.combatRolls },
        Cmd.none

    | VocationTableMsg vocationTableMsg ->
        let newVocationTables =
            VocationTables.update
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
                    model.combatRolls },
        Cmd.none

    | SetName name -> { model with name = name }, Cmd.none
    | GotInitData (coreSkillGroups, allItemData, magicSkillMap, magicCombatMap, rangeMap, combatVocationalSkill) ->
        let attributeStatList = coreSkillGroupToAttributeStats coreSkillGroups

        { model with
            coreSkillTables = coreSkillGroups
            vocationTables = VocationTables.init attributeStatList
            AllItemList = allItemData
            magicSkillMap = magicSkillMap
            magicCombatMap = magicCombatMap
            rangeMap = rangeMap
            combatVocationalSkill = combatVocationalSkill },
        Cmd.none
    | EquipmentRowListMsg equipmentRowListMsg ->
        let newEquipmentRowList =
            EquipmentRowList.update model.AllItemList equipmentRowListMsg model.equipmentRowList

        { model with
            equipmentRowList = newEquipmentRowList
            combatRolls =
                temp
                    newEquipmentRowList
                    (coreSkillGroupToAttributeStats model.coreSkillTables)
                    model.vocationTables
                    (CombatRollTable.Msg.RecalculateCombatRolls)
                    model.combatRolls },
        Cmd.none


open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    Bulma.hero [
        hero.isFullHeight
        color.isDanger

        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl
                "https://www.onlygfx.com/wp-content/uploads/2015/12/simple-old-paper-1-transparent.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]

        prop.children [

            Bulma.heroHead [
                Bulma.navbar [
                    color.isPrimary
                    prop.children [
                        Bulma.navbarItem.div [
                            Bulma.title.h3 [
                                prop.text "Fallen"
                                prop.style [
                                    style.fontFamily "PT Serif Caption"
                                ]
                            ]
                        ]
                    ]
                ]
            ]

            Bulma.heroBody [

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
                        VocationTables.view
                            model.combatVocationalSkill
                            model.vocationTables
                            (VocationTableMsg >> dispatch)
                    ]
                    Bulma.container [
                        EquipmentRowList.view
                            (List.map (fun (item: Item) -> item.name) model.AllItemList)
                            model.equipmentRowList
                            (EquipmentRowListMsg >> dispatch)
                        |> Bulma.content
                    ]
                    Bulma.container [
                        CombatRollTable.view model.combatRolls
                    ]
                ]
            ]
        ]
    ]