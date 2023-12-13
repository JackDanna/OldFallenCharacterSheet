module CoreSkillGroupList

open FallenLib.SkillDiceModEffect
open FallenLib.CoreSkillGroup
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | Modify of int * CoreSkillGroup.Msg
    | RecalculateCoreSkillGroups

let init () : CoreSkillGroup list = []

let update
    (skillDiceModificationEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (msg: Msg)
    (model: CoreSkillGroup list)
    : CoreSkillGroup list =
    match msg with
    | Modify (position, coreSkillTableMsg) ->
        List.mapi
            (fun i coreSkilTableModel ->
                if i = position then
                    CoreSkillGroup.update
                        skillDiceModificationEffectList
                        attributeDeterminedDiceModEffectList
                        coreSkillTableMsg
                        coreSkilTableModel
                else
                    coreSkilTableModel)
            model
    | RecalculateCoreSkillGroups ->
        List.map
            (fun coreSkillGroup ->
                CoreSkillGroup.update
                    skillDiceModificationEffectList
                    attributeDeterminedDiceModEffectList
                    CoreSkillGroup.Msg.RecalculateCoreSkillGroup
                    coreSkillGroup)
            model

open Feliz
open Feliz.Bulma

let view (model: CoreSkillGroup list) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.label "Attributes and Core Skills:"
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