module StringInputTableData

type Msg = SetString of string

let update (msg: Msg) (model: string) : string =
    match msg with
    | SetString newName -> newName

open Feliz
open Feliz.Bulma

let nonInteractiveView (model: string) =
    Bulma.input.text [ prop.text model ] |> Html.td

let interactiveView (model: string) (dispatch: Msg -> unit) =
    Bulma.input.text [
        prop.text model
        prop.onChange (fun input -> dispatch (SetString input))
    ]
    |> Html.td