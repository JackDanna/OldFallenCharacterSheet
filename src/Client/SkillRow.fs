module SkillRow

type Model = {
    name      : string
    dicePool  : string
    level     : Stat.Model
    attributeLevel : Stat.Model
}

type Msg =
    | StatMsg of Stat.Msg

let init() : Model = 
    { name = "Lift"
      dicePool = "0d6"
      level =  Stat.init()
      attributeLevel = Stat.init() }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | StatMsg statMsg ->
        { model with level = Stat.update statMsg model.level }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [ prop.text model.dicePool ]
        Bulma.column [
            Stat.view model.level (StatMsg >> dispatch)
        ]
    ]