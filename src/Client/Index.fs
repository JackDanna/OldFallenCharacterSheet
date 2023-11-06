module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.CoreSkillGroup

type Model =
    { defaultCoreSkillTables: CoreSkillGroups.Model
      allItemList: Item list
      magicSkillMap: Map<string, MagicSkill>
      magicCombatMap: Map<string, MagicCombat>
      rangeMap: Map<string, Range>
      combatVocationalSkill: string list
      character: Character.Model }

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitData of
        CoreSkillGroup list *
        Item list *
        Map<string, MagicSkill> *
        Map<string, MagicCombat> *
        Map<string, Range> *
        string list

let fallenDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFallenDataApi>

let init () : Model * Cmd<Msg> =

    { defaultCoreSkillTables = []
      allItemList = []
      magicSkillMap = Map.empty
      magicCombatMap = Map.empty
      rangeMap = Map.empty
      combatVocationalSkill = []
      character = Character.init (List.Empty) },
    Cmd.OfAsync.perform fallenDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->
        { model with
            character =
                Character.update
                    model.defaultCoreSkillTables
                    model.allItemList
                    model.magicSkillMap
                    model.magicCombatMap
                    model.rangeMap
                    characterMsg
                    model.character },
        Cmd.none
    | GotInitData (defaultCoreSkillGroups, allItemData, magicSkillMap, magicCombatMap, rangeMap, combatVocationalSkill) ->

        { model with
            defaultCoreSkillTables = defaultCoreSkillGroups
            allItemList = allItemData
            magicSkillMap = magicSkillMap
            magicCombatMap = magicCombatMap
            rangeMap = rangeMap
            combatVocationalSkill = combatVocationalSkill
            character =
                Character.update
                    defaultCoreSkillGroups
                    allItemData
                    magicSkillMap
                    magicCombatMap
                    rangeMap
                    Character.Msg.SetDefault
                    model.character },
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
                Character.view model.combatVocationalSkill model.allItemList model.character (CharacterMsg >> dispatch)
            ]
        ]
    ]