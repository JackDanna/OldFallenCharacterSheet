module VocationTables

open FallenLib.Attribute

type Model = VocationTable.Model list

type Msg =
    | Modify of int * VocationTable.Msg
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : Model =

    [ VocationTable.init attributeStatList
      VocationTable.init attributeStatList
      VocationTable.init attributeStatList ]

let update (attributeStatList: AttributeStat List) (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, vocationTableMsg) ->
        model
        |> List.mapi (fun i vocationTableModel ->
            if i = position then
                VocationTable.update attributeStatList vocationTableMsg vocationTableModel
            else
                vocationTableModel)
    | SetAttributeStatsAndCalculateDicePools ->
        model
        |> List.map (fun vocationTable ->
            VocationTable.update
                attributeStatList
                VocationTable.Msg.SetAttributeStatsAndCalculateDicePools
                vocationTable)

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

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
                        VocationTable.view vocationTable (fun msg -> dispatch (Modify(position, msg)))
                    ])
                |> Bulma.columns
            ]
        ]
    ]