module StatCheckboxes

type Model = {
    checkBoxes   : bool list
    numTrueBoxes : int
}

type Msg = Toggle of int

let update (msg:Msg) (model:Model) : Model =
    match msg with
    | Toggle index ->
        let newCheckBoxes = List.mapi (fun i x -> if i = index then not x else x) model.checkBoxes
        let newNumTrueBoxes = newCheckBoxes |> List.filter id |> List.length
    
        { checkBoxes = newCheckBoxes; numTrueBoxes = newNumTrueBoxes }

let init() : Model = {
    checkBoxes = List.init 4 (fun _ -> false)
    numTrueBoxes = 0
}

open Feliz
open Feliz.Bulma

let view (model : Model) (dispatch : Msg -> unit) =
    Bulma.columns [
        prop.children [
            for (i, isChecked) in List.indexed model.checkBoxes ->
                Bulma.column [
                    Html.input [
                        prop.type' "checkbox"
                        prop.isChecked isChecked
                        prop.onCheckedChange (fun _ -> dispatch (Toggle i))
                    ]
                ]
        ]
    ]
    
