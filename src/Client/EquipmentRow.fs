module EquipmentRow

open FallenLib.Equipment
open FallenLib.Item

type Model =
    | Equipment of Equipment
    | Empty

type Msg =
    | ItemRowMsg of ItemRow.Msg
    | SetEquipmentIsEquiped of bool
    | SetEquipmentQuantity of uint

let init () = Empty

let equipmentRowToItemRow equipmentRow =
    match equipmentRow with
    | Empty -> ItemRow.Model.Empty
    | Equipment equipment -> ItemRow.Model.Item equipment.item

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | ItemRowMsg itemRowMsg ->

        match ItemRow.update itemList itemRowMsg (equipmentRowToItemRow model) with
        | ItemRow.Model.Item item ->
            Equipment
                { isEquipped = false
                  item = item
                  quantity = 1u }
        | ItemRow.Model.Empty -> Empty

    | SetEquipmentIsEquiped newIsEquiped ->
        match model with
        | Empty -> Empty
        | Equipment equipment -> Equipment { equipment with isEquipped = newIsEquiped }

    | SetEquipmentQuantity newQuantity ->
        match model with
        | Empty -> Empty
        | Equipment equipment -> Equipment { equipment with quantity = newQuantity }

open Feliz
open Feliz.Bulma

let equipmentRowTableData (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    let makeEquipmentRow isEquipped (quantity: uint) item =
        List.append
            [ Html.td [
                  Bulma.input.checkbox [
                      prop.isChecked isEquipped
                      prop.onCheckedChange (fun isChecked -> dispatch (SetEquipmentIsEquiped(isChecked)))
                  ]
              ]
              Html.td [
                  Bulma.input.number [
                      prop.min 0
                      prop.value (int quantity)
                      prop.onChange (fun (num: int) -> dispatch (SetEquipmentQuantity(uint num)))
                  ]
              ] ]
            (ItemRow.itemRowColumns itemNameList item (ItemRowMsg >> dispatch))

    match model with
    | Equipment equipment -> makeEquipmentRow equipment.isEquipped equipment.quantity (ItemRow.Item equipment.item)
    | Empty -> makeEquipmentRow false 0u ItemRow.Empty

let view (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    equipmentRowTableData itemNameList model dispatch
    |> Html.tr