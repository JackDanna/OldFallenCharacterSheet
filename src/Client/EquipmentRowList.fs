module EquipmentRowList

open FallenLib.Item

type Model = EquipmentRow.Model list

type Msg =
    | ModifyEquipmentRow of int * EquipmentRow.Msg
    | Insert
    | Remove

let init () : Model = [ EquipmentRow.init () ]

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | ModifyEquipmentRow (position, equipmentRowMsg) ->
        List.mapi
            (fun index equipmentRow ->
                if position = index then
                    EquipmentRow.update itemList equipmentRowMsg equipmentRow
                else
                    equipmentRow)
            model
    | Insert -> List.append model [ EquipmentRow.init () ]
    | Remove -> model |> List.rev |> List.tail |> List.rev

open Feliz
open Feliz.Bulma

let view (itemList: Item list) (model: Model) (dispatch: Msg -> unit) =
    Bulma.table [
        table.isBordered
        prop.children [
            Html.tableHeader [
                Html.tr (
                    List.map
                        (fun (thString: string) -> Html.th thString)
                        [ "On"
                          "Name"
                          "#"
                          "Type"
                          "Tier"
                          "Dur."
                          "LB"
                          "Value" ]
                )
            ]
            Html.tableBody (
                List.mapi
                    (fun position equipmentRow ->
                        EquipmentRow.view itemList equipmentRow (fun msg -> dispatch (ModifyEquipmentRow(position, msg))))
                    model
            )
        ]
    ]