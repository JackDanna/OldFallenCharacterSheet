module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FallenLib.Character

type Model =
    { fallenData: FallenData
      character: Character }

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitData of FallenData

let fallenDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFallenDataApi>

let init () : Model * Cmd<Msg> =
    { fallenData =
        { defaultCoreSkillGroupList = []
          allItemList = []
          magicSkillMap = Map.empty
          magicCombatMap = Map.empty
          rangeMap = Map.empty
          combatVocationalSkill = []
          effectForDisplayMap = Map.empty
          carryWeightCalculationMap = Map.empty
          weightClassList = []
          movementSpeedCalculationMap = Map.empty }
      character = Character.init (List.Empty) },

    Cmd.OfAsync.perform fallenDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->

        { model with
            character =
                Character.update
                    model.fallenData.defaultCoreSkillGroupList
                    model.fallenData.allItemList
                    model.fallenData.magicSkillMap
                    model.fallenData.magicCombatMap
                    model.fallenData.rangeMap
                    model.fallenData.effectForDisplayMap
                    model.fallenData.carryWeightCalculationMap
                    model.fallenData.weightClassList
                    model.fallenData.movementSpeedCalculationMap
                    characterMsg
                    model.character },
        Cmd.none
    | GotInitData newFallenData ->

        { model with
            fallenData = newFallenData
            character =
                Character.update
                    newFallenData.defaultCoreSkillGroupList
                    newFallenData.allItemList
                    newFallenData.magicSkillMap
                    newFallenData.magicCombatMap
                    newFallenData.rangeMap
                    newFallenData.effectForDisplayMap
                    newFallenData.carryWeightCalculationMap
                    newFallenData.weightClassList
                    model.fallenData.movementSpeedCalculationMap
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
                Character.view
                    (Seq.toList model.fallenData.carryWeightCalculationMap.Keys)
                    ((List.ofSeq model.fallenData.effectForDisplayMap.Keys)
                     @ (List.ofSeq model.fallenData.carryWeightCalculationMap.Keys)
                       @ (List.ofSeq model.fallenData.movementSpeedCalculationMap.Keys))
                    model.fallenData.combatVocationalSkill
                    model.fallenData.allItemList
                    model.character
                    (CharacterMsg >> dispatch)
            ]
        ]
    ]