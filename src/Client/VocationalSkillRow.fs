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
    | SetName of string

let init() : Model = 
    {
      name = ""
      VocationalSkillStat = VocationalSkillStat.init()
      attributeDiceCalc = emptyDicePoolCalculation
      baseDice = baseDicePoolCalculation
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationalSkillStatMsg vocationalSkillStatMsg ->
        { model with VocationalSkillStat = VocationalSkillStat.update vocationalSkillStatMsg model.VocationalSkillStat }
    | SetName name -> { model with name = name }
            
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            Bulma.dropdown [
                Bulma.dropdownTrigger [
                    Bulma.input.text [ 
                        prop.defaultValue model.name
                        prop.onTextChange (fun value -> dispatch (SetName value) )
                    ]
                ]
                Bulma.dropdownMenu [
                    Bulma.dropdownContent [
                        Bulma.dropdownItem.a [
                            prop.children [
                                Html.span "Single Sequence"
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
            skillToDicePoolString
                model.baseDice model.VocationalSkillStat.level model.attributeDiceCalc
            |> prop.text
        ]
        Bulma.column [
            VocationalSkillStat.view model.VocationalSkillStat (VocationalSkillStatMsg >> dispatch)
        ]
    ]