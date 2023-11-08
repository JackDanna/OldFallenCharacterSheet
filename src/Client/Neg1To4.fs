module Neg1To4

open FallenLib.Neg1To4

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init () : Neg1To4 = Zero

let update (msg: Msg) (model: Neg1To4) : Neg1To4 =
    match msg with
    | ToggleNegOne isChecked -> if isChecked then NegOne else Zero
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two
    | ToggleFour isChecked -> if isChecked then Four else Three

let isCheckedLogic currentLevel checkboxRepresented =
    match checkboxRepresented = NegOne with
    | true -> currentLevel = NegOne
    | _ ->
        neg1To4ToInt checkboxRepresented
        <= neg1To4ToInt currentLevel

let isCheckboxDisabled currentLevel checkboxRepresented =

    match checkboxRepresented = NegOne with
    | true -> currentLevel <> NegOne && currentLevel <> Zero
    | _ ->
        let currentLevelInt = neg1To4ToInt currentLevel
        let checkboxRepresentedInt = neg1To4ToInt checkboxRepresented

        checkboxRepresentedInt <> currentLevelInt
        && checkboxRepresentedInt <> currentLevelInt + 1

open Feliz
open Feliz.Bulma

let view (model: Neg1To4) (dispatch: Msg -> unit) =

    let checkbox neg1To4 toggleNeg1To4 =
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model neg1To4)
                prop.isChecked (isCheckedLogic model neg1To4)
                prop.onCheckedChange (fun isChecked -> dispatch (toggleNeg1To4 isChecked))
            ]
        ]

    Bulma.columns [
        checkbox NegOne ToggleNegOne
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        checkbox One ToggleOne
        checkbox Two ToggleTwo
        checkbox Three ToggleThree
        checkbox Four ToggleFour
    ]