module EquipmentRowList

open FallenLib.Item
open FallenLib.Equipment

type Model = EquipmentRow.Model list

type Msg =
    | ModifyEquipmentRow of int * EquipmentRow.Msg
    | Insert of string
    | Remove of int

let init () : Model = []

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
    | Insert itemName ->
        { isEquipped = false
          quantity = 1u
          item = (List.find (fun item -> item.name = itemName) itemList) }
        |> List.singleton
        |> List.append model
    | Remove position -> List.removeAt position model

open Feliz
open Feliz.Bulma

let view (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    Bulma.table [
        table.isBordered
        prop.children [
            Html.thead [
                List.map
                    (fun (thString: string) -> Html.th thString)
                    [ "On"
                      "#"
                      "Name"
                      "Type"
                      "Tier"
                      //"Dur."
                      "LB"
                      "Value" ]
                |> Html.tr
            ]
            Html.tableBody (
                List.mapi
                    (fun position equipmentRow ->
                        let equipmentRowTableData =
                            (EquipmentRow.equipmentRowTableData itemNameList equipmentRow (fun msg ->
                                dispatch (ModifyEquipmentRow(position, msg))))

                        let deleteEquipmentRowButton =
                            Html.td [
                                Html.button [
                                    prop.onClick (fun _ -> dispatch (Remove(position)))
                                    prop.text "-"
                                ]
                            ]
                            |> List.singleton

                        Html.tr (List.append equipmentRowTableData deleteEquipmentRowButton))
                    model
            )
            Html.tfoot [
                ItemRow.itemInput "" itemNameList (fun input -> dispatch (Insert input))
            ]
        ]
    ]