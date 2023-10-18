module Neg1To4Stat

open FallenLib.Neg1To4

type Model = Neg1To4

type Msg =
    | ToggleNegOne of bool
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool
    | ToggleFour of bool

let init() : Model = Zero

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | ToggleNegOne isChecked -> if isChecked then NegOne else Zero
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two
    | ToggleFour isChecked -> if isChecked then Four else Three

let NegOneLogic neg1To4 =
    match neg1To4 with
    | NegOne -> true
    | _ -> false

let OneLogic neg1To4 =
    match neg1To4 with
    | NegOne | Zero -> false
    | _ -> true

let TwoLogic neg1To4 =
    match neg1To4 with
    | NegOne | Zero | One -> false
    | _ -> true

let ThreeLogic neg1To4 =
    match neg1To4 with
    | NegOne | Zero | One | Two -> false
    | _ -> true

let FourLogic neg1To4 =
    match neg1To4 with
    | NegOne | Zero | One | Two | Three -> false
    | _ -> true

let isNegOneDisabled neg1To4 =
    match neg1To4 with
    | NegOne | Zero -> false
    | _ -> true

let isOneDisabled neg1To4 =
    match neg1To4 with
    | Zero | One -> false
    | _ -> true

let isTwoDisabled neg1To4 =
    match neg1To4 with
    | One | Two -> false
    | _ -> true

let isThreeDisabled neg1To4 =
    match neg1To4 with
    | Two | Three -> false
    | _ -> true

let isFourDisabled neg1To4 =
    match neg1To4 with
    | Three | Four -> false
    | _ -> true

open Feliz
open Feliz.Bulma


let view (model : Model) (dispatch : Msg -> unit) =
    
    Bulma.columns [
        prop.children [
            Bulma.column [
                Html.input [
                    prop.type' "checkbox"
                    prop.disabled (isNegOneDisabled model)
                    prop.isChecked (NegOneLogic model)
                    prop.onCheckedChange ( fun isChecked -> dispatch (ToggleNegOne isChecked) )
                ]
            ]
            Bulma.column [
                Html.div [ prop.text "-" ]
            ]
            Bulma.column [
                Html.input [
                    prop.type' "checkbox"
                    prop.disabled (isOneDisabled model)
                    prop.isChecked (OneLogic model)
                    prop.onCheckedChange ( fun isChecked -> dispatch (ToggleOne isChecked) )
                ]
            ]
            Bulma.column [
                Html.input [
                    prop.type' "checkbox"
                    prop.disabled (isTwoDisabled model)
                    prop.isChecked (TwoLogic model)
                    prop.onCheckedChange ( fun isChecked -> dispatch (ToggleTwo isChecked) )
                ]
            ]
            Bulma.column [
                Html.input [
                    prop.type' "checkbox"
                    prop.disabled (isThreeDisabled model)
                    prop.isChecked (ThreeLogic model)
                    prop.onCheckedChange ( fun isChecked -> dispatch (ToggleThree isChecked) )
                ]
            ]
            Bulma.column [
                Html.input [
                    prop.type' "checkbox"
                    prop.disabled (isFourDisabled model)
                    prop.isChecked (FourLogic model)
                    prop.onCheckedChange ( fun isChecked -> dispatch (ToggleFour isChecked) )
                ]
            ]
        ]
    ]