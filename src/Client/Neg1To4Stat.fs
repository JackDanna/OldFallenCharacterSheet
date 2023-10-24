module Neg1To4Stat

open FallenLib.Neg1To4

type Model = Neg1To4

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init() : Model = Two

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | ToggleNegOne isChecked -> if isChecked then NegOne else Zero
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two
    | ToggleFour isChecked -> if isChecked then Four else Three

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
                prop.disabled (isCheckboxDisabled model NegOne)
                prop.isChecked (isCheckedLogic model NegOne)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleNegOne isChecked) )
            ]
        ]
        Bulma.column [
            Html.div [ prop.text "-" ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model One)
                prop.isChecked (isCheckedLogic model One)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleOne isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Two)
                prop.isChecked (isCheckedLogic model Two)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleTwo isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Three)
                prop.isChecked (isCheckedLogic model Three)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleThree isChecked) )
            ]
        ]
        Bulma.column [
            Bulma.input.checkbox [
                prop.disabled (isCheckboxDisabled model Four)
                prop.isChecked (isCheckedLogic model Four)
                prop.onCheckedChange ( fun isChecked -> dispatch (ToggleFour isChecked) )
            ]
        ]
    ]