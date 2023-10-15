module Stat

open FallenLib.Neg1To4

type Model = {
    negCheckboxes : SummedCheckboxes.Model
    posCheckboxes : SummedCheckboxes.Model
    level : Neg1To4
}

type Msg =
    | NegSummedCheckboxesMsg of SummedCheckboxes.Msg
    | PosSummedCheckboxesMsg of SummedCheckboxes.Msg


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | NegSummedCheckboxesMsg negSummedCheckboxesMsg ->
        let updatedNegSummedCheckboxes = 
            SummedCheckboxes.update negSummedCheckboxesMsg model.negCheckboxes
        { model with 
            negCheckboxes = updatedNegSummedCheckboxes
            level = intToNeg1To4 
                (model.posCheckboxes.numTrueBoxes - updatedNegSummedCheckboxes.numTrueBoxes ) }

    | PosSummedCheckboxesMsg posSummedCheckboxesMsg ->
        let updatedPosSummedCheckboxes =
            SummedCheckboxes.update posSummedCheckboxesMsg model.posCheckboxes
        { model with 
            posCheckboxes = updatedPosSummedCheckboxes
            level = intToNeg1To4
                (updatedPosSummedCheckboxes.numTrueBoxes - model.negCheckboxes.numTrueBoxes) }

let init() : Model = {
    negCheckboxes = { 
        checkBoxes = [false]
        numTrueBoxes = 0
    }
    posCheckboxes = SummedCheckboxes.init()
    level = Zero
}

open Feliz
open Feliz.Bulma

let view (model : Model) (dispatch : Msg -> unit) =
    Bulma.columns [
        prop.children [
            Bulma.column [
                SummedCheckboxes.view 
                    model.negCheckboxes (NegSummedCheckboxesMsg >> dispatch)
            ]
            Bulma.column [ prop.text "-" ]
            Bulma.column [
                SummedCheckboxes.view 
                    model.posCheckboxes (PosSummedCheckboxesMsg >> dispatch)
            ]
            Bulma.column [ 
                prop.text (convert_Neg1To4_To_Int model.level)
            ]
        ]
    ]