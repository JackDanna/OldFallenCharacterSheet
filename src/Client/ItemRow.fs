module ItemRow

open FallenLib.Item

type Model = Item

type Msg = SetItem of string

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | SetItem itemName -> List.find (fun item -> item.name = itemName) itemList

open Feliz
open Feliz.Bulma

let itemInput (nameValue: string) itemNameList onTextChange =
    Html.div [
        Bulma.input.text [
            prop.list "temp"
            prop.value nameValue
            prop.onTextChange onTextChange
        ]
        Html.datalist [
            prop.id "temp"
            prop.children (List.map (fun (itemName: string) -> Html.option [ prop.value itemName ]) itemNameList)
        ]
    ]


let itemRowColumns (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =


    [ itemInput model.name itemNameList (fun input -> dispatch (SetItem input))
      |> Html.td
      Html.td (itemClassesToString model.itemClasses)
      Html.td model.itemTier.name
      Html.td model.weight
      Html.td model.value ]

let view (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    itemRowColumns itemNameList model dispatch
    |> Html.tr