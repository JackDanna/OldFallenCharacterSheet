module Container

open FallenLib.Item
open FallenLib.Container
open FallenLib.ContainerClass

type Msg =
    | ToggleIsEquipped of bool
    | ItemListMsg of ItemList.Msg

let init (containerClas: ContainerClass) : Container =
    { name = ""
      isEquipped = false
      itemList = []
      containerClass = containerClas }

let update (allItemList: Item list) (msg: Msg) (model: Container) : Container =
    match msg with
    | ToggleIsEquipped isEquipped -> { model with isEquipped = isEquipped }
    | ItemListMsg itemListMsg -> { model with itemList = ItemList.update allItemList itemListMsg model.itemList }


open Feliz
open Feliz.Bulma

let view (itemNameList: string list) (model: Container) (dispatch: Msg -> unit) =
    Html.div [
        Html.input [ prop.value model.name ]
        Bulma.input.checkbox [
            prop.value model.isEquipped
            prop.onCheckedChange (fun isChecked -> dispatch (ToggleIsEquipped isChecked))
        ]
        ItemList.view itemNameList model.itemList (ItemListMsg >> dispatch)
    ]