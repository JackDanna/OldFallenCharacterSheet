module ZeroToFour

open FallenLib.ZeroToFour

type Msg =
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init () : ZeroToFour = Zero

let update (msg: Msg) (model: ZeroToFour) : ZeroToFour =
    match msg with
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two
    | ToggleFour isChecked -> if isChecked then Four else Three

let isCheckedLogic currentLevel checkboxRepresented =
    zeroToFourToUint checkboxRepresented
    <= zeroToFourToUint currentLevel

let isCheckboxDisabled currentLevel checkboxRepresented =

    let currentLevelInt = zeroToFourToUint currentLevel
    let checkboxRepresentedInt = zeroToFourToUint checkboxRepresented

    checkboxRepresentedInt <> currentLevelInt
    && checkboxRepresentedInt <> currentLevelInt + 1u


open Feliz
open Feliz.Bulma

let makeCheckbox (model: ZeroToFour) (dispatch: Msg -> unit) specifiedCheckbox toggleLogic =
    Bulma.column [
        Bulma.input.checkbox [
            prop.disabled (isCheckboxDisabled model specifiedCheckbox)
            prop.isChecked (isCheckedLogic model specifiedCheckbox)
            prop.onCheckedChange (fun isChecked -> dispatch (toggleLogic isChecked))
        ]
    ]



let view (model: ZeroToFour) (dispatch: Msg -> unit) =

    let preloadedMakeCheckbox = makeCheckbox model dispatch

    Bulma.columns [
        preloadedMakeCheckbox One ToggleOne
        preloadedMakeCheckbox Two ToggleTwo
        preloadedMakeCheckbox Three ToggleThree
        preloadedMakeCheckbox Four ToggleFour
    ]