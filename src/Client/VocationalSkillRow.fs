module VocationalSkillRow

open FallenLib.Dice
open FallenLib.VocationalSkill

type Model = {
    name : string
    vocationalSkillStat : VocationalSkillStat.Model
    vocationRow : VocationRow.Model
}

type Msg =
    | SetName of string
    | SetVocationRow of VocationRow.Model
    | VocationalSkillStatMsg of VocationalSkillStat.Msg

let init (vocationRow:VocationRow.Model) : Model = 
    {
        name = ""
        vocationalSkillStat = VocationalSkillStat.init (vocationRow.level)
        vocationRow = vocationRow
    }


let update(msg: Msg) (model: Model) : Model =
    match msg with
    | SetName name -> { model with name = name }
    | SetVocationRow newVocationRow ->
        { model with
            vocationRow = newVocationRow
            vocationalSkillStat = 
                VocationalSkillStat.update (VocationalSkillStat.Msg.SetLevelCap (newVocationRow.level)) model.vocationalSkillStat
        }
    | VocationalSkillStatMsg vocationalSkillStatMsg ->
        { model with
            vocationalSkillStat = VocationalSkillStat.update vocationalSkillStatMsg model.vocationalSkillStat
        }
        
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
            vocationalToDicePoolString
                baseDicePoolCalculation
                model.vocationRow.governingAttributes
                (model.vocationRow.level |> zeroToFourToNegOneToFour)
            |> prop.text
        ]
        Bulma.column [
            VocationalSkillStat.view
                model.vocationalSkillStat
                (VocationalSkillStatMsg >> dispatch)
        ]
    ]