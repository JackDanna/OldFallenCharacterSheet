module VocationalSkillRow

open FallenLib.Dice
open FallenLib.SkillUtils

type Model = {
    name                 : string
    VocationalSkillStat  : VocationalSkillStat.Model
    attributeDiceCalc    : DicePoolCalculation
    baseDice             : DicePoolCalculation
}

type Msg =
    | VocationalSkillStatMsg of VocationalSkillStat.Msg
    | SetAttributeDiceCalc of DicePoolCalculation

let init() : Model = 
    {
      name = "Med. Hafted Blade"
      VocationalSkillStat = VocationalSkillStat.init()
      attributeDiceCalc = emptyDicePoolCalculation
      baseDice = baseDicePoolCalculation
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationalSkillStatMsg vocationalSkillStatMsg ->
        { model with VocationalSkillStat = VocationalSkillStat.update vocationalSkillStatMsg model.VocationalSkillStat }
    | SetAttributeDiceCalc attributeDicePoolCalc ->
        { model with attributeDiceCalc = attributeDicePoolCalc }
            
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            skillToDicePoolString 
                model.baseDice model.VocationalSkillStat.level model.attributeDiceCalc
            |> prop.text
        ]
        Bulma.column [
            VocationalSkillStat.view model.VocationalSkillStat (VocationalSkillStatMsg >> dispatch)
        ]
    ]