module NewStat

open FallenLib.Neg1To4

type Model = Neg1To4

type Msg =
    | Increment
    | Decrement
    | Reset

let incrementNeg1To4 neg1To4 =
    match neg1To4 with
    | NegOne -> Zero
    | Zero -> One
    | One -> Two
    | Two -> Three
    | Three -> Four
    | Four -> Four

let decrementNeg1To4 neg1To4 =
    match neg1To4 with
    | Four -> Three
    | Three -> Two
    | Two -> One
    | One -> Zero
    | Zero -> NegOne
    | NegOne -> NegOne

let init() : Model = Zero
let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Increment -> incrementNeg1To4 model
    | Decrement -> decrementNeg1To4 model
    | Reset -> init()

open Feliz
open Feliz.Bulma

let posCheckboxDispatches dispatch isChecked =
    match isChecked with 
    | true -> dispatch Increment 
    | false -> dispatch Decrement

let negChecboxDispatches dispatch isChecked =
    match isChecked with 
    | true -> dispatch Decrement 
    | false -> dispatch Increment

let posOrNegChecboxDispatches isPosCheckbox =
    match isPosCheckbox with
    | true -> posCheckboxDispatches
    | false -> negChecboxDispatches

let checkboxes isPosCheckbox (model : bool list) (dispatch : Msg -> unit) =
    Bulma.columns [
        prop.children [
            for (i, isChecked) in List.indexed model ->
                Bulma.column [
                    Html.input [
                        prop.type' "checkbox"
                        prop.isChecked isChecked
                        prop.onCheckedChange (
                                fun isChecked -> (posOrNegChecboxDispatches isPosCheckbox) dispatch isChecked
                        )
                    ]
                ]
        ]
    ]

let posCheckboxes = checkboxes true
let negCheckboxes = checkboxes false

let statRow dispatch negList posList = 
    Bulma.columns [
        prop.children [
            Bulma.column [ negCheckboxes negList dispatch ]
            Bulma.column [ prop.text "-" ]
            Bulma.column [ posCheckboxes posList dispatch ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    let preloadedStatRow = statRow dispatch
    match model with
    | NegOne -> preloadedStatRow [true] [false;false;false;false]
    | Zero   -> preloadedStatRow [false] [false;false;false;false]
    | One    -> preloadedStatRow [false] [true;false;false;false]
    | Two    -> preloadedStatRow [false] [true;true;false;false]
    | Three  -> preloadedStatRow [false] [true;true;true;false]
    | Four   -> preloadedStatRow [false] [true;true;true;true]
    