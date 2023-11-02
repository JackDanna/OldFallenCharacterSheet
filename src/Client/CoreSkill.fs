module CoreSkill

open FallenLib.CoreSkillGroup
open FallenLib.Dice
open FallenLib.SkillStat

type Model = SkillStat

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg

let init() = {
    name = ""
    lvl = Neg1To4Stat.init()
}

let update (msg:Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4StatMsg ->
        { model with lvl = Neg1To4Stat.update neg1To4StatMsg model.lvl}

open Feliz
open Feliz.Bulma

let view (attributeLvl:Neg1To4Stat.Model) (model: Model) (dispatch: Msg-> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            coreSkillToString baseDicePool model.lvl attributeLvl 
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view
                model.lvl
                (Neg1To4StatMsg >> dispatch)
        ]
    ]