module Skill

open FallenLib.Skill

type Msg = Neg1To4Msg of Neg1To4.Msg

let init () = { name = ""; level = Neg1To4.init () }

let update msg model =
    match msg with
    | Neg1To4Msg neg1To4 -> { model with level = Neg1To4.update neg1To4 model.level }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            Neg1To4.view model.level (Neg1To4Msg >> dispatch)
        ]
    ]