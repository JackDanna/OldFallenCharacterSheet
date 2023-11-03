module CoreSkill

open FallenLib.CoreSkillGroup
open FallenLib.Dice
open FallenLib.SkillStat

type Model = SkillStat

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | CalculateDicePool

let init (attributeLvl: Neg1To4Stat.Model) =
    let lvl = Neg1To4Stat.init ()

    { name = ""
      lvl = lvl
      dicePool = coreSkillToDicePool baseDicePool lvl attributeLvl }

let update (attributeLvl: Neg1To4Stat.Model) (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4StatMsg ->
        let newLvl = Neg1To4Stat.update neg1To4StatMsg model.lvl

        { model with
            lvl = newLvl
            dicePool = coreSkillToDicePool baseDicePool newLvl attributeLvl }
    | CalculateDicePool -> { model with dicePool = coreSkillToDicePool baseDicePool model.lvl attributeLvl }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            model.dicePool |> dicePoolToString |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view model.lvl (Neg1To4StatMsg >> dispatch)
        ]
    ]