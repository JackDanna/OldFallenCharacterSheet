module VocationalSkillStat

open FallenLib.Neg1To4
open Neg1To4Stat
open FallenLib.VocationalSkill


type Model = {
    level : Neg1To4Stat.Model
    levelCap : VocationStat.Model
}

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool
    | SetLevelCap of VocationStat.Model

let init (levelCap : VocationStat.Model) : Model = {
    level = Zero
    levelCap = levelCap
}

let determineIfCapped levelCap level =
    (neg1To4ToInt level) > neg1To4ToInt (zeroToFourToNegOneToFour levelCap)

let determineIfCappedAndReturnModel levelCap level =
    
    match determineIfCapped levelCap level with
    | true  -> zeroToFourToNegOneToFour levelCap
    | false -> level

let update (msg: Msg) (model: Model) : Model =

    let toggleLogic levelCap isChecked levelIfTrue levelIfFalse =
        match isChecked with
        | true  -> determineIfCappedAndReturnModel levelCap levelIfTrue
        | false -> determineIfCappedAndReturnModel levelCap levelIfFalse

    match msg with
    | ToggleNegOne isChecked -> { model with level = toggleLogic model.levelCap isChecked NegOne Zero }
    | ToggleOne isChecked -> { model with level = toggleLogic model.levelCap isChecked One Zero }
    | ToggleTwo isChecked -> { model with level = toggleLogic model.levelCap isChecked Two One }
    | ToggleThree isChecked -> { model with level = toggleLogic model.levelCap isChecked Three Two }
    | ToggleFour isChecked -> { model with level = toggleLogic model.levelCap isChecked Four Three }
    | SetLevelCap levelCap ->
        { model with
            level = determineIfCappedAndReturnModel levelCap model.level
            levelCap = levelCap
        }

open Feliz
open Feliz.Bulma

let isVocationalCheckboxDisabled levelCap level checkboxRepresented =
        match determineIfCapped levelCap checkboxRepresented with
        | true -> true
        | _ -> isCheckboxDisabled level checkboxRepresented

let view (model : Model) (dispatch : Msg -> unit) =
    
    Bulma.columns [
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled model.levelCap model.level NegOne)
                prop.isChecked (isCheckedLogic model.level NegOne)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleNegOne isChecked) )
            ]
        ]
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled model.levelCap model.level One)
                prop.isChecked (isCheckedLogic model.level One)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleOne isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled model.levelCap model.level Two)
                prop.isChecked (isCheckedLogic model.level Two)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleTwo isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled model.levelCap model.level Three)
                prop.isChecked (isCheckedLogic model.level Three)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleThree isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isVocationalCheckboxDisabled model.levelCap model.level Four)
                prop.isChecked (isCheckedLogic model.level Four)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleFour isChecked) )
            ]
        ]
    ]