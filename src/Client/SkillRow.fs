module SkillRow

open FallenLib.SkillStat

type Model = {
    dicePool  : string
    skillStat : SkillStat
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg

let init() : Model = 
    { 
      skillStat = {
        lvl = Neg1To4Stat.init()
        name = ""
        governingAttributes = [||]
      }
      dicePool = "3d6"
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1ToStatMsg ->

        let newSkillStat = {
            name = model.skillStat.name
            governingAttributes = model.skillStat.governingAttributes
            lvl = Neg1To4Stat.update neg1ToStatMsg model.skillStat.lvl
        }
        { model with skillStat = newSkillStat }
            
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.skillStat.name ]
        Bulma.column [ prop.text model.dicePool ]
        Bulma.column [
            Neg1To4Stat.view model.skillStat.lvl (Neg1To4StatMsg >> dispatch)
        ]
    ]