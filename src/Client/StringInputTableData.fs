module StringInputTableData

type Msg = SetString of string

let init () : string = ""

let update (msg: Msg) (model: string) : string =
    match msg with
    | SetString newName -> newName

open Feliz
open Feliz.Bulma

let nonInteractiveView (model: string) =
    Bulma.input.text [ prop.text model ] |> Html.td

let interactiveView (model: string) (dispatch: Msg -> unit) =
    Bulma.input.text [
        prop.value model
        prop.onTextChange (fun input -> dispatch (SetString input))
    ]
    |> Html.td