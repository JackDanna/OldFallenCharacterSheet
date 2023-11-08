module VocationalSkillStat

open FallenLib.Neg1To4
open FallenLib.VocationGroup
open FallenLib.ZeroToFour
open Neg1To4

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool
    | CheckIfLevelCapExceeded

let init () : Neg1To4 = Neg1To4.init ()

let determineIfCapped levelCap level =
    (neg1To4ToInt level) > neg1To4ToInt (zeroToFourToNegOneToFour levelCap)

let determineIfCappedAndReturnModel levelCap level =

    match determineIfCapped levelCap level with
    | true -> zeroToFourToNegOneToFour levelCap
    | false -> level

let update (levelCap: ZeroToFour) (msg: Msg) (model: Neg1To4) : Neg1To4 =

    let toggleLogic levelCap isChecked levelIfTrue levelIfFalse =
        match isChecked with
        | true -> determineIfCappedAndReturnModel levelCap levelIfTrue
        | false -> determineIfCappedAndReturnModel levelCap levelIfFalse

    match msg with
    | ToggleNegOne isChecked -> toggleLogic levelCap isChecked NegOne Neg1To4.Zero
    | ToggleOne isChecked -> toggleLogic levelCap isChecked Neg1To4.One Neg1To4.Zero
    | ToggleTwo isChecked -> toggleLogic levelCap isChecked Neg1To4.Two Neg1To4.One
    | ToggleThree isChecked -> toggleLogic levelCap isChecked Neg1To4.Three Neg1To4.Two
    | ToggleFour isChecked -> toggleLogic levelCap isChecked Neg1To4.Four Neg1To4.Three
    | CheckIfLevelCapExceeded -> determineIfCappedAndReturnModel levelCap model

open Feliz
open Feliz.Bulma

let isVocationalCheckboxDisabled levelCap level checkboxRepresented =
    match determineIfCapped levelCap checkboxRepresented with
    | true -> true
    | _ -> isCheckboxDisabled level checkboxRepresented

let view (levelCap: ZeroToFour) (model: Neg1To4) (dispatch: Msg -> unit) =

    let checkbox neg1To4 toggleNeg1To4 =
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model neg1To4)
                prop.isChecked (isCheckedLogic model neg1To4)
                prop.onCheckedChange (fun isChecked -> dispatch (toggleNeg1To4 isChecked))
            ]
        ]

    Bulma.columns [
        checkbox NegOne ToggleNegOne
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        checkbox Neg1To4.One ToggleOne
        checkbox Neg1To4.Two ToggleTwo
        checkbox Neg1To4.Three ToggleThree
        checkbox Neg1To4.Four ToggleFour
    ]