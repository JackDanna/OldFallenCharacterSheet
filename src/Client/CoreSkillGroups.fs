module CoreSkillGroupList

open FallenLib.SkillDiceModificationEffect
open FallenLib.CoreSkillGroup

type Msg =
    | Modify of int * CoreSkillGroup.Msg
    | RecalculateCoreSkillGroups

let init () : CoreSkillGroup list = []

let update
    (skillDiceModificationEffectList: SkillDiceModificationEffect list)
    (msg: Msg)
    (model: CoreSkillGroup list)
    : CoreSkillGroup list =
    match msg with
    | Modify (position, coreSkillTableMsg) ->
        List.mapi
            (fun i coreSkilTableModel ->
                if i = position then
                    CoreSkillGroup.update skillDiceModificationEffectList coreSkillTableMsg coreSkilTableModel
                else
                    coreSkilTableModel)
            model
    | RecalculateCoreSkillGroups ->
        List.map
            (fun coreSkillGroup ->
                CoreSkillGroup.update
                    skillDiceModificationEffectList
                    CoreSkillGroup.Msg.RecalculateCoreSkillGroup
                    coreSkillGroup)
            model

open Feliz
open Feliz.Bulma

let view (model: CoreSkillGroup list) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.label [
            prop.text "Attributes and Core Skills:"
        ]
        |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> List.mapi (fun position coreSkillTable ->
                    Bulma.column [
                        CoreSkillGroup.view coreSkillTable (fun msg -> dispatch (Modify(position, msg)))
                    ])
                |> Bulma.columns
            ]
        ]
    ]