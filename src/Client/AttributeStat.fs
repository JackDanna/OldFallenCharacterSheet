module AttributeStat

open FallenLib.Attribute

type Msg = Neg1To4Msg of Neg1To4.Msg

let init () =
    { attribute = ""
      lvl = Neg1To4.init () }

let update (msg: Msg) (model: Attribute) : Attribute =
    match msg with
    | Neg1To4Msg neg1To4Stat -> { model with lvl = Neg1To4.update neg1To4Stat model.lvl }

open Feliz
open Feliz.Bulma

let view (model: Attribute) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            prop.text model.attribute
        ]
        Bulma.column [
            Neg1To4.view model.lvl (Neg1To4Msg >> dispatch)
        ]
    ]