module SkillRow

open Elmish
open FallenLib.SkillStat

type Model = {
    skillStat : SkillStat
    dicePool  : string
}

type Msg =
    | Reset

let init() : Model * Cmd<Msg> =
    let model = {
        skillStat = emptySkillStat
        dicePool = ""
    }
    let cmd = Cmd.none
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
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