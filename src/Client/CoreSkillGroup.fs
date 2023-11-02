module CoreSkillGroup

open FallenLib.CoreSkillGroup

type Model = CoreSkillGroup

type Msg =
    | AttributeRowMsg of AttributeStat.Msg
    | ModifyCoreSkill of int * CoreSkill.Msg

let init() : Model = {
    attributeStat = AttributeStat.init()
    coreSkillList = [
        CoreSkill.init()
    ]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->
        { model with attributeStat = AttributeStat.update attributeRowMsg model.attributeStat }

    | ModifyCoreSkill (position, skillRowMsg) ->
        { model with 
            coreSkillList =
                List.mapi ( fun i skillRowModel ->
                    if position = i then
                        CoreSkill.update skillRowMsg skillRowModel
                    else 
                        skillRowModel
                ) model.coreSkillList
        }
    
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeStat.view model.attributeStat (AttributeRowMsg >> dispatch)
        Html.ul (
            List.mapi ( 
                fun position skill ->
                    CoreSkill.view
                        model.attributeStat.lvl
                        skill 
                        (fun msg -> dispatch (ModifyCoreSkill (position, msg)))
            ) model.coreSkillList
        )
    ]