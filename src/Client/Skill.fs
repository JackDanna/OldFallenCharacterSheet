module Skill

open FallenLib.Dice
open FallenLib.SkillUtils

type Model = {
    name                 : string
    skillLevel           : Neg1To4Stat.Model
    attributeDiceMod     : DicePoolModification
    baseDice             : DicePool
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg

let init() : Model = 
    {
      name = "Lift"
      skillLevel = Neg1To4Stat.init()
      attributeDiceMod = AddDice emptyDicePool
      baseDice = baseDicePool
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1ToStatMsg ->

        { model with skillLevel = Neg1To4Stat.update neg1ToStatMsg model.skillLevel }
            
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            skillToDicePoolString model.baseDice model.skillLevel model.attributeDiceMod
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view model.skillLevel (Neg1To4StatMsg >> dispatch)
        ]
    ]