module Stat

type Model = {
    negCheckboxes : CheckboxRow.Model
    posCheckboxes : CheckboxRow.Model
}

type Msg =
    | NegSummedCheckboxesMsg of CheckboxRow.Msg
    | PosSummedCheckboxesMsg of CheckboxRow.Msg


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | NegSummedCheckboxesMsg negSummedCheckboxesMsg ->
        { model with negCheckboxes = CheckboxRow.update negSummedCheckboxesMsg model.negCheckboxes }

    | PosSummedCheckboxesMsg posSummedCheckboxesMsg ->
        { model with posCheckboxes = CheckboxRow.update posSummedCheckboxesMsg model.posCheckboxes }

let init() : Model = {
    negCheckboxes = [false]
    posCheckboxes = [false;false;false;false]
}

open Feliz
open Feliz.Bulma

let view (model : Model) (dispatch : Msg -> unit) =
    Bulma.columns [
        prop.children [
            Bulma.column [
                CheckboxRow.view 
                    model.negCheckboxes (NegSummedCheckboxesMsg >> dispatch)
            ]
            Bulma.column [ prop.text "-" ]
            Bulma.column [
                CheckboxRow.view 
                    model.posCheckboxes (PosSummedCheckboxesMsg >> dispatch)
            ]
        ]
    ]