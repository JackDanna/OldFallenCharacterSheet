module ItemRow

open FallenLib.Item

type Model =
    | Item of Item
    | Empty

type Msg = SetItem of string

let init () = Empty

let update (itemList: Item list) (msg: Msg) (model: Model) : Model =
    match msg with
    | SetItem itemName ->
        match List.tryFind (fun item -> item.name = itemName) itemList with
        | Some item -> Item item
        | None -> Empty

open Feliz
open Feliz.Bulma

let itemRowColumns (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    let makeRow (name: string) (itemClassesDesc: string) (itemTierDesc: string) (weight: float) (value: string) =
        [ Html.td [
              prop.children [
                  Bulma.input.text [
                      prop.list "temp"
                      prop.onTextChange (fun input -> dispatch (SetItem input))
                  ]
                  Html.datalist [
                      prop.id "temp"
                      prop.children (
                          List.map (fun (itemName: string) -> Html.option [ prop.value itemName ]) itemNameList
                      )
                  ]
              ]
          ]
          Html.td itemClassesDesc
          Html.td itemTierDesc
          Html.td weight
          Html.td value ]

    match model with
    | Item item -> makeRow item.name (itemClassesToString item.itemClasses) item.itemTier.name item.weight item.value
    | Empty -> makeRow "" "" "" 0.0 ""

let view (itemNameList: string list) (model: Model) (dispatch: Msg -> unit) =
    itemRowColumns itemNameList model dispatch
    |> Html.tr