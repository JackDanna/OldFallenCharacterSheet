module CoreSkill

open FallenLib.CoreSkill
open FallenLib.Neg1To4

type Msg = Neg1To4Msg of Neg1To4.Msg

let init () =
    { skill = { name = ""; level = Zero }
      governingAttribute = "" }

let update msg model =
    match msg with
    | Neg1To4Msg msg -> { model with skill.level = Neg1To4.update msg model.skill.level }

open Feliz
open Feliz.Bulma

let view (coreSkillDicePoolString: string) model dispatch =

    Bulma.columns [
        Bulma.column [
            prop.text model.skill.name
        ]
        Bulma.column [
            prop.text coreSkillDicePoolString
        ]
        Bulma.column [
            Neg1To4.view model.skill.level (Neg1To4Msg >> dispatch)
        ]
    ]