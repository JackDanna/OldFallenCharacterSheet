module CoreSkillList

type Msg = ModifyCoreSkillAtPostion of int * CoreSkill.Msg

let init () = []

let update msg model =
    match msg with
    | ModifyCoreSkillAtPostion (position, coreSkillMsg) ->
        model
        |> List.mapi

            (fun index coreSkill ->
                if index = position then
                    CoreSkill.update coreSkillMsg coreSkill
                else
                    coreSkill)

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.box [
        Html.ul (
            model
            |> List.mapi (fun position coreSkill ->
                CoreSkill.view coreSkill (fun msg ->
                    ModifyCoreSkillAtPostion(position, msg)
                    |> dispatch))
        )
    ]