module VocationList

open FallenLib.Attribute
open FallenLib.Vocation

type Msg =
    | Modify of int * Vocation.Msg
    | Insert
    | Remove of int

let init () : Vocation list = [ Vocation.init () ]

let update (attributeStatList: Attribute List) (msg: Msg) (model: Vocation list) : Vocation list =
    match msg with
    | Modify (position, vocationTableMsg) ->
        model
        |> List.mapi (fun i vocationTableModel ->
            if i = position then
                Vocation.update attributeStatList vocationTableMsg vocationTableModel
            else
                vocationTableModel)
    | Insert -> model @ [ Vocation.init () ]
    | Remove position -> List.removeAt position model


open Feliz
open Feliz.Bulma

let view
    (combatVocationalSkills: string list)
    (stringifiedVocationDicePool: StringifiedVocationDicePool list)
    (allAttributeNames: AttributeName list)
    (model: Vocation list)
    (dispatch: Msg -> unit)
    =

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
                        Vocation.view
                            combatVocationalSkills
                            allAttributeNames
                            stringifiedVocationDicePool[position]
                            vocationTable
                            (fun msg -> dispatch (Modify(position, msg)))

                        Bulma.button.button [
                            prop.onClick (fun _ -> dispatch (Remove position))
                            prop.text "-"
                        ]
                    ])
                |> Bulma.columns
            ]
        ]
    ]