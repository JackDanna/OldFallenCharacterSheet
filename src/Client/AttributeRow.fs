module AttributeRow

type Model = {
    name : string
    level : Stat.Model
}

type Msg =
    | StatMsg of Stat.Msg

let init() = {
    name = "STR"
    level = Stat.init()
}
    
let update (msg: Msg) (model: Model) : Model =
    match msg with
    | StatMsg statMsg ->
        { model with level = Stat.update statMsg model.level }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            Stat.view model.level (StatMsg >> dispatch)
        ]
    ]