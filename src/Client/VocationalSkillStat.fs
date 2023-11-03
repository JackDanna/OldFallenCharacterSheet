module VocationalSkillStat

open FallenLib.Neg1To4
open Neg1To4Stat
open FallenLib.VocationGroup

type Model = Neg1To4Stat.Model

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init () : Model = Neg1To4Stat.init ()

let determineIfCapped levelCap level =
    (neg1To4ToInt level) > neg1To4ToInt (zeroToFourToNegOneToFour levelCap)

let determineIfCappedAndReturnModel levelCap level =

    match determineIfCapped levelCap level with
    | true -> zeroToFourToNegOneToFour levelCap
    | false -> level

let update (levelCap: ZeroToFourStat.Model) (msg: Msg) (model: Model) : Model =

    let toggleLogic levelCap isChecked levelIfTrue levelIfFalse =
        match isChecked with
        | true -> determineIfCappedAndReturnModel levelCap levelIfTrue
        | false -> determineIfCappedAndReturnModel levelCap levelIfFalse

    match msg with
    | ToggleNegOne isChecked -> toggleLogic levelCap isChecked NegOne Zero
    | ToggleOne isChecked -> toggleLogic levelCap isChecked One Zero
    | ToggleTwo isChecked -> toggleLogic levelCap isChecked Two One
    | ToggleThree isChecked -> toggleLogic levelCap isChecked Three Two
    | ToggleFour isChecked -> toggleLogic levelCap isChecked Four Three

open Feliz
open Feliz.Bulma

let isVocationalCheckboxDisabled levelCap level checkboxRepresented =
    match determineIfCapped levelCap checkboxRepresented with
    | true -> true
    | _ -> isCheckboxDisabled level checkboxRepresented

let view (levelCap: ZeroToFourStat.Model) (model: Model) (dispatch: Msg -> unit) =

    let checkbox levelCap model neg1To4 toggleNeg1To4 =
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model neg1To4)
                prop.isChecked (isCheckedLogic model neg1To4)
                prop.onCheckedChange (fun isChecked -> dispatch (toggleNeg1To4 isChecked))
            ]
        ]

    let loadedCheckbox = checkbox levelCap model

    Bulma.columns [
        loadedCheckbox NegOne ToggleNegOne
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        loadedCheckbox One ToggleOne
        loadedCheckbox Two ToggleTwo
        loadedCheckbox Three ToggleThree
        loadedCheckbox Four ToggleFour
    ]