module VocationGroupList

open FallenLib.Attribute
open FallenLib.VocationGroup
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | Modify of int * VocationGroup.Msg
    | SetAttributeStatsAndCalculateDicePools
    | Insert
    | Remove of int

let init (attributeStatList: AttributeStat List) : VocationGroup list =

    [ VocationGroup.init attributeStatList
      VocationGroup.init attributeStatList ]

let update
    (skillDiceModEffectList: SkillDiceModEffect list)
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
                    skillDiceModEffectList
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
                skillDiceModEffectList
                attributeDeterminedDiceModEffectList
                attributeStatList
                VocationGroup.Msg.SetAttributeStatsAndCalculateDicePools
                vocationTable)
    | Insert -> model @ [ VocationGroup.init attributeStatList ]
    | Remove position -> List.removeAt position model


open Feliz
open Feliz.Bulma

let view (combatVocationalSkills: string list) (model: VocationGroup list) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.label [
            prop.text "Vocations and Vocational Skills:"
        ]
        |> Bulma.content
        Bulma.button.button [
            prop.onClick (fun _ -> dispatch Insert)
            prop.text "+"
        ]
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> List.mapi (fun position vocationTable ->
                    Bulma.column [
                        VocationGroup.view combatVocationalSkills vocationTable (fun msg ->
                            dispatch (Modify(position, msg)))

                        Bulma.button.button [
                            prop.onClick (fun _ -> dispatch (Remove position))
                            prop.text "-"
                        ]
                    ])
                |> Bulma.columns
            ]
        ]
    ]