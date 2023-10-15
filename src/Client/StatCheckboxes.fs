module StatCheckboxes

type Model = bool list

type Msg = Toggle of int

let update (msg:Msg) (model:Model) : Model =
    match msg with
    | Toggle index ->
        model
        |> List.mapi (fun i x -> if i = index then not x else x)

let init() : Model = List.init 4 (fun _ -> false)

open Feliz

let view (model : Model) (dispatch : Msg -> unit) =
    Html.div [
        prop.children [
            for (i, isChecked) in List.indexed model ->
            Html.input [
                prop.type' "checkbox"
                prop.isChecked isChecked
                prop.onCheckedChange (fun _ -> dispatch (Toggle i))
            ]
        ]
    ]
