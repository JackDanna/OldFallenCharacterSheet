module CoreSkillList

open FallenLib.Attribute
open FallenLib.CoreSkill
open FallenLib

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

let view
    (coreSkillDicePoolStringList: string List)
    (model: CoreSkill list)
    dispatch
    (governingAttribute: AttributeName)
    =
    Html.ul (
        List.mapi2
            (fun position coreSkill coreSkillDicePoolString ->
                if governingAttribute = coreSkill.governingAttribute then
                    CoreSkill.view coreSkillDicePoolString coreSkill (fun msg ->
                        ModifyCoreSkillAtPostion(position, msg)
                        |> dispatch)
                else
                    Html.none)
            model
            coreSkillDicePoolStringList
    )