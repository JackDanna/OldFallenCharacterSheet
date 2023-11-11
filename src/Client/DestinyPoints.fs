module DestinyPoints

open FallenLib.ZeroToThree

type Msg =
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool

let init () : ZeroToThree = Zero

let update (msg: Msg) (model: ZeroToThree) : ZeroToThree =
    match msg with
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two

let isCheckedLogic currentLevel checkboxRepresented =
    zeroToThreeToUint checkboxRepresented
    <= zeroToThreeToUint currentLevel

let isCheckboxDisabled currentLevel checkboxRepresented =

    let currentLevelInt = zeroToThreeToUint currentLevel
    let checkboxRepresentedInt = zeroToThreeToUint checkboxRepresented

    checkboxRepresentedInt <> currentLevelInt
    && checkboxRepresentedInt <> currentLevelInt + 1u

open Feliz
open Feliz.Bulma

let view (model: ZeroToThree) (dispatch: Msg -> unit) =

    let makeCheckbox specifiedCheckbox toggleLogic =
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model specifiedCheckbox)
                prop.isChecked (isCheckedLogic model specifiedCheckbox)
                prop.onCheckedChange (fun isChecked -> dispatch (toggleLogic isChecked))
            ]
        ]

    Bulma.container [
        Bulma.label "Destiny Points:"
        Bulma.columns [
            makeCheckbox One ToggleOne
            makeCheckbox Two ToggleTwo
            makeCheckbox Three ToggleThree
        ]
    ]