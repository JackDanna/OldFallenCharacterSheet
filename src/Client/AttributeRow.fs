module AttributeRow

open FallenLib.Attribute

type Model = Attribute

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg

let init() = {
    name = ""
    neg1To4Stat = Neg1To4Stat.init()
}
    
let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4Stat ->
        { model with neg1To4Stat = Neg1To4Stat.update neg1To4Stat model.neg1To4Stat }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            Neg1To4Stat.view model.neg1To4Stat (Neg1To4StatMsg >> dispatch)
        ]
    ]