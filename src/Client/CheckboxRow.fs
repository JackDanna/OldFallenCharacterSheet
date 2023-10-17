module CheckboxRow

type Model = bool list

type Msg = Toggle of int

let update (msg:Msg) (model:Model) : Model =
    match msg with
    | Toggle index ->
        List.mapi (fun i x -> if i = index then not x else x) model


let init() : Model = [true;true;false]

open Feliz
open Feliz.Bulma

let view (model : Model) (dispatch : Msg -> unit) =
    Bulma.columns [
        prop.children [
            for (i, isChecked) in List.indexed model ->
                Bulma.column [
                    Html.input [
                        prop.type' "checkbox"
                        prop.isChecked isChecked
                        prop.onCheckedChange (fun _ -> dispatch (Toggle i))
                    ]
                ]
        ]
    ]
    
