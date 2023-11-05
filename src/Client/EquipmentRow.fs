module EquipmentRow

open FallenLib.Equipment
open FallenLib.Item

type Model = Equipment

type Msg =
    | ItemRowMsg of ItemRow.Msg
    | SetEquipmentIsEquiped of bool
    | SetEquipmentQuantity of uint

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | ItemRowMsg itemRowMsg ->
        { isEquipped = false
          item = ItemRow.update itemList itemRowMsg model.item
          quantity = 1u }

    | SetEquipmentIsEquiped newIsEquiped -> { model with isEquipped = newIsEquiped }

    | SetEquipmentQuantity newQuantity -> { model with quantity = newQuantity }

open Feliz
open Feliz.Bulma

let equipmentRowTableData (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    List.append
        [ Html.td [
              Bulma.input.checkbox [
                  prop.isChecked model.isEquipped
                  prop.onCheckedChange (fun isChecked -> dispatch (SetEquipmentIsEquiped(isChecked)))
              ]
          ]
          Html.td [
              Bulma.input.number [
                  prop.min 0
                  prop.value (int model.quantity)
                  prop.onChange (fun (num: int) -> dispatch (SetEquipmentQuantity(uint num)))
              ]
          ] ]
        (ItemRow.itemRowColumns itemNameList model.item (ItemRowMsg >> dispatch))

let view (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    equipmentRowTableData itemNameList model dispatch
    |> Html.tr