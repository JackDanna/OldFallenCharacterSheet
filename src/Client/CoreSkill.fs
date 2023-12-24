module CoreSkill

open FallenLib.CoreSkill

type Msg = SkillMsg of Skill.Msg

let init () =
    { skill = Skill.init ()
      governingAttribute = "" }

let update msg model =
    match msg with
    | SkillMsg msg -> { model with skill = Skill.update msg model.skill }

let view model dispatch =
    Skill.view model.skill (SkillMsg >> dispatch)