module VocationTable

open FallenLib.SkillUtils
open FallenLib.Dice

type VocationalSkill = {
    name : string
    vocationalSkillStat : VocationalSkillStat.Model
}

let vocationalSkillInit() = { name = ""; vocationalSkillStat = VocationalSkillStat.init() }

type Model = {
    vocationRow : VocationRow.Model
    vocationalSkillRowList : VocationalSkill List
}

type Msg =
    | VocationRowMsg of VocationRow.Msg
    | Insert
    | Remove
    | ModifyName of int * string
    | ModifyVocationalSkillStat of int * VocationalSkillStat.Msg

let init() : Model = {
    vocationRow = VocationRow.init()
    vocationalSkillRowList = [vocationalSkillInit()]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocationRow = VocationRow.update vocationRowMsg model.vocationRow }

    | Insert ->
        { model with 
            vocationalSkillRowList = 
                List.append model.vocationalSkillRowList [vocationalSkillInit()]
        }

    | Remove ->
        { model with 
            vocationalSkillRowList = 
                model.vocationalSkillRowList |> List.rev |> List.tail |> List.rev
        }

    | ModifyName (position, newName) ->
        { model with 
            vocationalSkillRowList = 
                List.mapi ( fun i skillRow ->
                    if position = i then
                        { skillRow with name =  newName }
                    else 
                        skillRow
                ) model.vocationalSkillRowList
        }

    | ModifyVocationalSkillStat (position, newVocationalSkillStat) ->
        { model with 
            vocationalSkillRowList = 
                List.mapi ( fun i (skillRow:VocationalSkill) ->
                    if position = i then
                        { skillRow with vocationalSkillStat = VocationalSkillStat.update newVocationalSkillStat skillRow.vocationalSkillStat }
                    else 
                        skillRow
                ) model.vocationalSkillRowList
        }
    

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        VocationRow.view model.vocationRow (VocationRowMsg >> dispatch)
        Html.ul (
            List.append (
                List.mapi ( 
                    fun position skillRow ->

                        Bulma.columns [
                            Bulma.column [
                                Bulma.dropdown [
                                    Bulma.dropdownTrigger [
                                        Bulma.input.text [ 
                                            prop.defaultValue skillRow.name
                                            prop.onTextChange (fun value -> dispatch (ModifyName (position,value)) )
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
                                vocationToDicePoolString baseDicePoolCalculation model.vocationRow.governingAttributes skillRow.vocationalSkillStat.level
                                |> prop.text
                            ]
                            Bulma.column [
                                VocationalSkillStat.view skillRow.vocationalSkillStat (fun msg -> dispatch (ModifyVocationalSkillStat (position, msg)) )
                            ]
                        ]

                ) model.vocationalSkillRowList
            ) [
                Html.button [
                    prop.onClick (fun _ -> dispatch Insert)
                    prop.text "+"
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch Remove)
                    prop.text "-"
                ]
            ]
        )
    ]