module ItemRowList

open FallenLib.Item

type Msg = Modify of int * ItemRow.Msg

let init () = []

let update (itemList: Item list) (msg: Msg) (model: Item list) : Item list =
    match msg with
    | Modify (position, msg) ->
        List.mapi
            (fun i (itemRow: Item) ->
                if position = i then
                    ItemRow.update itemList msg itemRow
                else
                    itemRow)
            model

open Feliz
open Feliz.Bulma

let view (itemNameList: string list) (model: Item list) (dispatch: Msg -> unit) =

    let temp =
        List.mapi
            (fun position itemRow -> ItemRow.view itemNameList itemRow (fun msg -> dispatch (Modify(position, msg))))
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
    ]