module Equipment

open FallenLib.Equipment
open FallenLib.ItemStack

type Msg =
    | ItemStackRowMsg of ItemStack.Msg
    | SetEquipmentIsEquiped of bool

let update (itemStackList: ItemStack list) (msg: Msg) (model: Equipment) : Equipment =
    match msg with
    | ItemStackRowMsg itemRowMsg ->
        { isEquipped = false
          itemStack = ItemStack.update itemStackList itemRowMsg model.itemStack }

    | SetEquipmentIsEquiped newIsEquiped -> { model with isEquipped = newIsEquiped }

open Feliz
open Feliz.Bulma

let equipmentRowTableData (itemNameList: string list) (model: Equipment) (dispatch: Msg -> unit) =
    List.append
        [ Html.td [
              Bulma.input.checkbox [
                  prop.isChecked model.isEquipped
                  prop.onCheckedChange (fun isChecked -> dispatch (SetEquipmentIsEquiped(isChecked)))
              ]
          ] ]
        (ItemStack.itemStackRowTableData itemNameList model.itemStack (ItemStackRowMsg >> dispatch))

let view (itemNameList: string list) (model: Equipment) (dispatch: Msg -> unit) =
    equipmentRowTableData itemNameList model dispatch
    |> Html.tr