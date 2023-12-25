module VocationalSkill

open FallenLib.ZeroToFour
open FallenLib.VocationalSkill

type Msg =
    | SetName of string
    | VocationalSkillStatMsg of VocationalSkillStat.Msg

let init () =
    { skill = { name = ""; level = Neg1To4.init () }
      governingAttributes = [] }


let update (levelCap: ZeroToFour) (msg: Msg) (model: VocationalSkill) : VocationalSkill =
    match msg with
    | SetName newName ->
        { model with
            skill =
                { name = newName
                  level = model.skill.level } }
    | VocationalSkillStatMsg vocationalSkillStatMsg ->

        { model with
            skill =
                { level = VocationalSkillStat.update levelCap vocationalSkillStatMsg model.skill.level
                  name = model.skill.name } }

open Feliz
open Feliz.Bulma

let view
    (combatVocationalSkills: string list)
    (levelCap: ZeroToFour)
    (vocationalSkillDicePoolString: string)
    (model: VocationalSkill)
    (dispatch: Msg -> unit)
    =
    Bulma.columns [
        Bulma.column [
            Bulma.dropdown [
                Bulma.dropdownTrigger [
                    Bulma.input.text [
                        prop.list "combatVocationalSkills"
                        prop.defaultValue model.skill.name
                        prop.onTextChange (fun value -> dispatch (SetName value))
                    ]
                    Html.datalist [
                        prop.id "combatVocationalSkills"
                        prop.children (
                            List.map
                                (fun (combatVocationalSkill: string) ->
                                    Html.option [
                                        prop.value combatVocationalSkill
                                    ])
                                combatVocationalSkills
                        )
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
            prop.text vocationalSkillDicePoolString
        ]
        Bulma.column [
            VocationalSkillStat.view levelCap model.skill.level (VocationalSkillStatMsg >> dispatch)
        ]
    ]