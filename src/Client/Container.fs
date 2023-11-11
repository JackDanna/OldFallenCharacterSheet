module Container

open FallenLib.Item
open FallenLib.Container
open FallenLib.ContainerClass

type Msg =
    | ToggleIsEquipped of bool
    | ModifyItemRow of int * Item.Msg
    | Insert of string
    | Remove of int

let init (containerClas: ContainerClass) : Container =
    { name = ""
      isEquipped = false
      itemList = []
      containerClass = containerClas }

let update (allItemList: Item list) (msg: Msg) (model: Container) : Container =
    match msg with
    | ToggleIsEquipped isEquipped -> { model with isEquipped = isEquipped }
    | ModifyItemRow (position, itemMsg) ->
        { model with
            itemList =
                model.itemList
                |> List.mapi (fun index item ->
                    if position = index then
                        Item.update allItemList itemMsg item
                    else
                        item) }
    | Insert itemName ->
        let item: Item = List.find (fun item -> item.name = itemName) allItemList
        let currentWeight = List.sumBy (fun item -> item.weight) model.itemList

        if item.weight + currentWeight > model.containerClass.weightCapacity then
            model
        else
            { model with
                itemList =
                    item
                    |> List.singleton
                    |> List.append model.itemList }
    | Remove position -> { model with itemList = List.removeAt position model.itemList }

open Feliz
open Feliz.Bulma

let itemHeaders =
    [ "Name"
      "Type"
      "Tier"
      //"Dur."
      "LB"
      "Value" ]

let itemList (itemNameList: string list) (model: Item list) (dispatch: Msg -> unit) =
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

let view (itemNameList: string list) (model: Container) (dispatch: Msg -> unit) =
    Html.div [
        Html.input [ prop.value model.name ]
        Bulma.input.checkbox [
            prop.value model.isEquipped
            prop.onCheckedChange (fun isChecked -> dispatch (ToggleIsEquipped isChecked))
        ]
        itemList itemNameList model.itemList (dispatch)
    ]