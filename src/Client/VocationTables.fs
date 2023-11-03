module VocationTables

open FallenLib.Attribute

type Model = VocationTable.Model list

type Msg = Modify of int * VocationTable.Msg

let init () : Model =

    [ VocationTable.init ()
      VocationTable.init ()
      VocationTable.init () ]

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, vocationTableMsg) ->
        model
        |> List.mapi (fun i vocationTableModel ->
            if i = position then
                VocationTable.update vocationTableMsg vocationTableModel
            else
                vocationTableModel)

open Feliz
open Feliz.Bulma

let view (attributeStats: AttributeStat list) (model: Model) (dispatch: Msg -> unit) =

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
                        VocationTable.view attributeStats vocationTable (fun msg -> dispatch (Modify(position, msg)))
                    ])
                |> Bulma.columns
            ]
        ]
    ]