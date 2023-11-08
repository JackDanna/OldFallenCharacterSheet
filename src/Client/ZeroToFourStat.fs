module ZeroToFourStat

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


let view (model: ZeroToFour) (dispatch: Msg -> unit) =

    Bulma.columns [
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model One)
                prop.isChecked (isCheckedLogic model One)
                prop.onCheckedChange (fun isChecked -> dispatch (ToggleOne isChecked))
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Two)
                prop.isChecked (isCheckedLogic model Two)
                prop.onCheckedChange (fun isChecked -> dispatch (ToggleTwo isChecked))
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Three)
                prop.isChecked (isCheckedLogic model Three)
                prop.onCheckedChange (fun isChecked -> dispatch (ToggleThree isChecked))
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Four)
                prop.isChecked (isCheckedLogic model Four)
                prop.onCheckedChange (fun isChecked -> dispatch (ToggleFour isChecked))
            ]
        ]
    ]