module VocationalSkillStat

open FallenLib.Neg1To4

type Model = {
    level : Neg1To4
    levelCap : Neg1To4Stat.Model
}

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init() : Model = {
    level = Zero
    levelCap = Zero
}

let determineIfCappedAndReturnModel levelCap level =
    
    match (neg1To4ToInt level) > (neg1To4ToInt levelCap) with
    | true  -> { level = levelCap; levelCap = levelCap }
    | false -> { level = level; levelCap = levelCap }

let update (msg: Msg) (model: Model) : Model =

    let toggleLogic levelCap isChecked levelIfTrue levelIfFalse =
        match isChecked with
        | true  -> determineIfCappedAndReturnModel levelCap levelIfTrue
        | false -> determineIfCappedAndReturnModel levelCap levelIfFalse

    match msg with
    | ToggleNegOne isChecked -> toggleLogic model.levelCap isChecked NegOne Zero
    | ToggleOne isChecked -> toggleLogic model.levelCap isChecked One Zero
    | ToggleTwo isChecked -> toggleLogic model.levelCap isChecked Two One
    | ToggleThree isChecked -> toggleLogic model.levelCap isChecked Three Two
    | ToggleFour isChecked -> toggleLogic model.levelCap isChecked Four Three

let isCheckedLogic currentLevel checkboxRepresented =
    match checkboxRepresented = NegOne with
    | true ->
        match currentLevel = NegOne with
        | true -> true
        | _ -> false
    | _ -> neg1To4ToInt checkboxRepresented <= neg1To4ToInt currentLevel

let isCheckboxDisabled currentLevel checkboxRepresented =

    match checkboxRepresented = NegOne with
    | true -> currentLevel <> NegOne && currentLevel <> Zero
    | _ ->
        let currentLevelInt = neg1To4ToInt currentLevel
        let checkboxRepresentedInt = neg1To4ToInt checkboxRepresented
        checkboxRepresentedInt <> currentLevelInt && checkboxRepresentedInt <> currentLevelInt + 1

open Feliz
open Feliz.Bulma


let view (model : Model) (dispatch : Msg -> unit) =
    
    Bulma.columns [
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model.level NegOne)
                prop.isChecked (isCheckedLogic model.level NegOne)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleNegOne isChecked) )
            ]
        ]
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model.level One)
                prop.isChecked (isCheckedLogic model.level One)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleOne isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model.level Two)
                prop.isChecked (isCheckedLogic model.level Two)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleTwo isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model.level Three)
                prop.isChecked (isCheckedLogic model.level Three)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleThree isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model.level Four)
                prop.isChecked (isCheckedLogic model.level Four)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleFour isChecked) )
            ]
        ]
    ]