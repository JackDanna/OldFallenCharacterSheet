module ItemStack

open FallenLib.ItemStack

type Msg =
    | ItemRowMsg of Item.Msg
    | SetItemStackQuantity of uint

let update (allItemStackList: ItemStack list) (msg: Msg) (model: ItemStack) : ItemStack =
    match msg with
    | ItemRowMsg itemRowMsg ->
        { item = Item.update (itemStackListToItemList allItemStackList) itemRowMsg model.item
          quantity = 1u }
    | SetItemStackQuantity newQuantity -> { model with quantity = newQuantity }

open Feliz
open Feliz.Bulma

let itemStackRowTableData (itemNameList: string list) (model: ItemStack) (dispatch: Msg -> unit) =
    List.append
        [ (Html.td [
              Bulma.input.number [
                  prop.min 0
                  prop.value (int model.quantity)
                  prop.onChange (fun (num: int) -> dispatch (SetItemStackQuantity(uint num)))
              ]
           ]) ]

        (Item.itemRowColumns itemNameList model.item (ItemRowMsg >> dispatch))

let view (itemNameList: string list) (model: ItemStack) (dispatch: Msg -> unit) =
    itemStackRowTableData itemNameList model dispatch
    |> Html.tr