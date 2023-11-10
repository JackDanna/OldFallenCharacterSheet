module ItemList

open FallenLib.Item

type Msg =
    | ModifyItemRow of int * Item.Msg
    | Insert of string
    | Remove of int


let update (allItemList: Item list) (msg: Msg) (model: Item List) : Item List =
    match msg with
    | ModifyItemRow (position, itemMsg) ->
        model
        |> List.mapi (fun index item ->
            if position = index then
                Item.update allItemList itemMsg item
            else
                item)
    | Insert itemName ->
        allItemList
        |> List.find (fun item -> item.name = itemName)
        |> List.singleton
        |> List.append model
    | Remove position -> List.removeAt position model

open Feliz
open Feliz.Bulma

let itemHeaders =
    [ "Name"
      "Type"
      "Tier"
      //"Dur."
      "LB"
      "Value" ]

let view (itemNameList: string list) (model: Item list) (dispatch: Msg -> unit) =
    Bulma.table [
        table.isBordered
        prop.children [
            Html.thead [
                itemHeaders
                |> List.map (fun (thString: string) -> Html.th thString)
                |> Html.tr
            ]
            Html.tableBody (
                List.mapi
                    (fun position (item: Item) ->
                        let equipmentRowTableData =
                            (Item.itemRowColumns itemNameList item (fun msg -> dispatch (ModifyItemRow(position, msg))))

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