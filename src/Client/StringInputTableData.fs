module StringInputTableData

type Msg = SetString of string

let init () : string = ""

let update (msg: Msg) (model: string) : string =
    match msg with
    | SetString newName -> newName

open Feliz
open Feliz.Bulma

let interactiveView (model: string) (dispatch: Msg -> unit) =
    Bulma.input.text [
        prop.value model
        prop.onTextChange (fun input -> dispatch (SetString input))
    ]
    |> Html.td