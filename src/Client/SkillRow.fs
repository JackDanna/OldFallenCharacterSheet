module SkillRow

open FallenLib.SkillStat

type Model = {
    skillStat : SkillStat
    dicePool  : string
}

type Msg =
    | Reset

let init() : Model = 
    { skillStat = emptySkillStat
      dicePool = "" }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.skillStat.name ]
        Bulma.column [ prop.text model.dicePool ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [ prop.text "-" ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
    ]