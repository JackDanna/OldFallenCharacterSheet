module OldCoreSkillGroupList

open FallenLib.SkillDiceModEffect
open FallenLib.CoreSkillGroup
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | Modify of int * OldSkillGroup.Msg
    | RecalculateCoreSkillGroups

let init () : CoreSkillGroup list = []

let update
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (msg: Msg)
    (model: CoreSkillGroup list)
    : CoreSkillGroup list =
    match msg with
    | Modify (position, coreSkillTableMsg) ->
        List.mapi
            (fun i coreSkilTableModel ->
                if i = position then
                    OldSkillGroup.update
                        skillDiceModEffectList
                        attributeDeterminedDiceModEffectList
                        coreSkillTableMsg
                        coreSkilTableModel
                else
                    coreSkilTableModel)
            model
    | RecalculateCoreSkillGroups ->
        List.map
            (fun coreSkillGroup ->
                OldSkillGroup.update
                    skillDiceModEffectList
                    attributeDeterminedDiceModEffectList
                    OldSkillGroup.Msg.RecalculateCoreSkillGroup
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
                        OldSkillGroup.view coreSkillTable (fun msg -> dispatch (Modify(position, msg)))
                    ])
                |> Bulma.columns
            ]
        ]
    ]