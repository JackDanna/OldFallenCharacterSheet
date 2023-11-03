module AttributeStat

open FallenLib.Attribute

type Model = AttributeStat

type Msg = Neg1To4StatMsg of Neg1To4Stat.Msg

let init () =
    { attribute = ""
      lvl = Neg1To4Stat.init () }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4Stat -> { model with lvl = Neg1To4Stat.update neg1To4Stat model.lvl }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            prop.text model.attribute
        ]
        Bulma.column [
            Neg1To4Stat.view model.lvl (Neg1To4StatMsg >> dispatch)
        ]
    ]