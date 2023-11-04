module ItemRowList

open FallenLib.Item

type Model = Item.Model list

type Msg = Modify of int * Item.Msg

let init () = []

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, msg) ->
        List.mapi
            (fun i (itemRow: Item.Model) ->
                if position = i then
                    Item.update itemList msg itemRow
                else
                    itemRow)
            model


open Feliz
open Feliz.Bulma

let view (itemList: Item list) (model: Model) (dispatch: Msg -> unit) =

    let temp =
        List.mapi
            (fun position itemRow -> Item.view itemList itemRow (fun msg -> dispatch (Modify(position, msg))))
            model

    Bulma.table [
        Html.thead [
            Html.tr (
                List.map
                    (fun (headerName: string) -> Html.th headerName)
                    [ "Name"
                      "#"
                      "Type"
                      "Tier"
                      "Dur."
                      "LB"
                      "Value" ]
            )
        ]
        Html.tbody temp
    ]