module ItemRowList

open FallenLib.Item

type Model = ItemRow.Model list

type Msg =
    | Modify of int * ItemRow.Msg

let init () = []

let update (itemList:Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, msg) ->
        List.mapi ( fun i (itemRow:ItemRow.Model) ->
            if position = i then
                ItemRow.update itemList msg itemRow
            else
                itemRow
        ) model


open Feliz
open Feliz.Bulma

let view (itemList:Item list) (model: Model) (dispatch: Msg -> unit) =

    let temp =
        List.mapi ( fun position itemRow ->
            ItemRow.view
                itemList
                itemRow
                ( fun msg -> dispatch ( Modify (position,msg) ) )
        ) model
    Bulma.table [
        Html.thead [
            Html.tr (
                List.map ( fun (headerName:string) ->
                    Html.th headerName
                ) [
                    "Name"
                    "#"
                    "Type"
                    "Tier"
                    "Dur."
                    "LB"
                    "Value"
                ]
            )
        ]
        Html.tbody temp
    ]