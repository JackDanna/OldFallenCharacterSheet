module EquipmentList

open FallenLib.Item
open FallenLib.Equipment

type Msg =
    | ModifyEquipmentRow of int * Equipment.Msg
    | Insert of string
    | Remove of int

let init () : Equipment list = []

let update (allItemList: Item list) (msg: Msg) (model: Equipment list) : Equipment list =
    match msg with
    | ModifyEquipmentRow (position, equipmentMsg) ->
        List.mapi
            (fun index equipment ->
                if position = index then
                    Equipment.update allItemList equipmentMsg equipment
                else
                    equipment)
            model
    | Insert itemName ->
        { isEquipped = false
          quantity = 1u
          item = (List.find (fun item -> item.name = itemName) allItemList) }
        |> List.singleton
        |> List.append model
    | Remove position -> List.removeAt position model

open Feliz
open Feliz.Bulma

let view (itemNameList: string list) (model: Equipment list) (dispatch: Msg -> unit) =
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
                            (Equipment.equipmentRowTableData itemNameList equipmentRow (fun msg ->
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
                Html.div [
                    Bulma.input.text [
                        prop.list "temp"
                        prop.onTextChange (fun input -> dispatch (Insert input))
                    ]
                    Html.datalist [
                        prop.id "temp"
                        prop.children (
                            List.map (fun (itemName: string) -> Html.option [ prop.value itemName ]) itemNameList
                        )
                    ]
                ]
            ]
        ]
    ]