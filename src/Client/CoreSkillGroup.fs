module CoreSkillGroup

open FallenLib.CoreSkillGroup

type Model = CoreSkillGroup

type Msg =
    | AttributeRowMsg of AttributeStat.Msg
    | ModifyCoreSkill of int * CoreSkill.Msg

let init () : Model =
    let attributeStat = AttributeStat.init ()

    { attributeStat = attributeStat
      coreSkillList = [ CoreSkill.init (attributeStat.lvl) ] }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->
        let newAttributeStat = AttributeStat.update attributeRowMsg model.attributeStat

        { model with
            attributeStat = newAttributeStat
            coreSkillList =
                List.map
                    (fun coreSkill -> CoreSkill.update newAttributeStat.lvl CoreSkill.CalculateDicePool coreSkill)
                    model.coreSkillList }

    | ModifyCoreSkill (position, skillRowMsg) ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.mapi (fun i skillRowModel ->
                    if position = i then
                        CoreSkill.update model.attributeStat.lvl skillRowMsg skillRowModel
                    else
                        skillRowModel) }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeStat.view model.attributeStat (AttributeRowMsg >> dispatch)
        Html.ul (
            List.mapi
                (fun position skill -> CoreSkill.view skill (fun msg -> dispatch (ModifyCoreSkill(position, msg))))
                model.coreSkillList
        )
    ]