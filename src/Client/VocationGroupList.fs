module VocationGroupList

open FallenLib.Attribute
open FallenLib.VocationGroup
open FallenLib.SkillDiceModificationEffect

type Msg =
    | Modify of int * VocationGroup.Msg
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : VocationGroup list =

    [ VocationGroup.init attributeStatList
      VocationGroup.init attributeStatList ]

let update
    (skillDiceModificationEffectList: SkillDiceModificationEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (attributeStatList: AttributeStat List)
    (msg: Msg)
    (model: VocationGroup list)
    : VocationGroup list =
    match msg with
    | Modify (position, vocationTableMsg) ->
        model
        |> List.mapi (fun i vocationTableModel ->
            if i = position then
                VocationGroup.update
                    skillDiceModificationEffectList
                    attributeDeterminedDiceModEffectList
                    attributeStatList
                    vocationTableMsg
                    vocationTableModel
            else
                vocationTableModel)
    | SetAttributeStatsAndCalculateDicePools ->
        model
        |> List.map (fun vocationTable ->
            VocationGroup.update
                skillDiceModificationEffectList
                attributeDeterminedDiceModEffectList
                attributeStatList
                VocationGroup.Msg.SetAttributeStatsAndCalculateDicePools
                vocationTable)

open Feliz
open Feliz.Bulma

let view (combatVocationalSkills: string list) (model: VocationGroup list) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.label [
            prop.text "Vocations and Vocational Skills:"
        ]
        |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> List.mapi (fun position vocationTable ->
                    Bulma.column [
                        VocationGroup.view combatVocationalSkills vocationTable (fun msg ->
                            dispatch (Modify(position, msg)))
                    ])
                |> Bulma.columns
            ]
        ]
    ]