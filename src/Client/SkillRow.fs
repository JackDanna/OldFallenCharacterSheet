module SkillRow

open FallenLib.Dice
open FallenLib.SkillUtils

type Model = {
    name                 : string
    skillLevel           : Neg1To4Stat.Model
    attributeDiceCalc    : DicePoolCalculation
    baseDice             : DicePoolCalculation
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | SetAttributeDiceCalc of DicePoolCalculation

let init() : Model = 
    {
      name = ""
      skillLevel = Neg1To4Stat.init()
      attributeDiceCalc = emptyDicePoolCalculation
      baseDice = baseDicePoolCalculation
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1ToStatMsg ->
        { model with skillLevel = Neg1To4Stat.update neg1ToStatMsg model.skillLevel }
    | SetAttributeDiceCalc attributeDicePoolCalc ->
        { model with attributeDiceCalc = attributeDicePoolCalc }
            
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            skillToDicePoolString 
                model.baseDice model.skillLevel model.attributeDiceCalc
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view model.skillLevel (Neg1To4StatMsg >> dispatch)
        ]
    ]