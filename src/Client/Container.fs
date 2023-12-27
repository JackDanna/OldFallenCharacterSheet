module Container

open FallenLib.ItemStack
open FallenLib.Container
open FallenLib.ContainerClass
open FallenLib.Item

type Msg =
    | ToggleIsEquipped of bool
    | ModifyItemStackRow of int * ItemStack.Msg
    | Insert of string
    | Remove of int

let init (containerClas: ContainerClass) : Container =
    { name = ""
      isEquipped = false
      itemStackList = []
      containerClass = containerClas }

let update (allItemStackList: ItemStack list) (msg: Msg) (model: Container) : Container =
    match msg with
    | ToggleIsEquipped isEquipped -> { model with isEquipped = isEquipped }
    | ModifyItemStackRow (position, msg) ->
        { model with
            itemStackList =
                model.itemStackList
                |> List.mapi (fun index item ->
                    if position = index then
                        ItemStack.update allItemStackList msg item
                    else
                        item) }
    | Insert itemName ->
        let itemStack: ItemStack =
            List.find (fun item -> item.item.name = itemName) allItemStackList

        let currentWeight = sumItemStackListWeight model.itemStackList

        if itemStack.item.weight + currentWeight > model.containerClass.weightCapacity then
            model
        else
            { model with
                itemStackList =
                    itemStack
                    |> List.singleton
                    |> List.append model.itemStackList }
    | Remove position -> { model with itemStackList = List.removeAt position model.itemStackList }

open Feliz
open Feliz.Bulma

let itemHeaders =
    [ "#"
      "Name"
      "Type"
      "Tier"
      //"Dur."
      "LB"
      "Value" ]

let itemStackList (itemNameList: string list) (model: ItemStack list) (dispatch: Msg -> unit) =
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
                    (fun position (itemStack: ItemStack) ->
                        let equipmentRowTableData =
                            (ItemStack.itemStackRowTableData itemNameList itemStack (fun msg ->
                                dispatch (ModifyItemStackRow(position, msg))))

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
        Html.div [
            prop.text
                $"Container Capacity: {sumItemStackListWeight model.itemStackList}/{model.containerClass.weightCapacity} lb"
        ]
        itemStackList itemNameList model.itemStackList (dispatch)
    ]