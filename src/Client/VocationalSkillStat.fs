module VocationalSkillStat

open FallenLib.Neg1To4
open Neg1To4Stat
open FallenLib.VocationalSkill

type Model = Neg1To4

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init() : Model = Zero

let determineIfCapped levelCap level =
    (neg1To4ToInt level) > neg1To4ToInt (zeroToFourToNegOneToFour levelCap)

let determineIfCappedAndReturnModel levelCap level =
    
    match determineIfCapped levelCap level with
    | true  -> zeroToFourToNegOneToFour levelCap
    | false -> level

let update (levelCap: VocationStat.Model) (msg: Msg) (model: Model) : Model =

    let toggleLogic levelCap isChecked levelIfTrue levelIfFalse =
        match isChecked with
        | true  -> determineIfCappedAndReturnModel levelCap levelIfTrue
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

let view (levelCap:VocationStat.Model) (model : Model) (dispatch : Msg -> unit) =
    
    Bulma.columns [
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model NegOne)
                prop.isChecked (isCheckedLogic model NegOne)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleNegOne isChecked) )
            ]
        ]
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model One)
                prop.isChecked (isCheckedLogic model One)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleOne isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model Two)
                prop.isChecked (isCheckedLogic model Two)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleTwo isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model Three)
                prop.isChecked (isCheckedLogic model Three)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleThree isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled levelCap model Four)
                prop.isChecked (isCheckedLogic model Four)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleFour isChecked) )
            ]
        ]
    ]