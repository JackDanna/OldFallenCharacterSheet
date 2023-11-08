module CoreSkillGroup

open FallenLib.CoreSkillGroup
open FallenLib.SkillAdjustment

type Msg =
    | AttributeMsg of AttributeStat.Msg
    | ModifyCoreSkill of int * CoreSkill.Msg
    | RecalculateCoreSkillGroup

let init (skillAdjustmentList: SkillAdjustment list) : CoreSkillGroup =
    let attributeStat = AttributeStat.init ()

    { attributeStat = attributeStat
      coreSkillList = [ CoreSkill.init skillAdjustmentList attributeStat.lvl ] }

let update (skillAdjustmentList: SkillAdjustment list) (msg: Msg) (model: CoreSkillGroup) : CoreSkillGroup =
    match msg with
    | AttributeMsg attributeRowMsg ->
        let newAttributeStat = AttributeStat.update attributeRowMsg model.attributeStat

        { model with
            attributeStat = newAttributeStat
            coreSkillList =
                List.map
                    (fun coreSkill ->
                        CoreSkill.update skillAdjustmentList newAttributeStat.lvl CoreSkill.CalculateDicePool coreSkill)
                    model.coreSkillList }

    | ModifyCoreSkill (position, skillRowMsg) ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.mapi (fun i skillRowModel ->
                    if position = i then
                        CoreSkill.update skillAdjustmentList model.attributeStat.lvl skillRowMsg skillRowModel
                    else
                        skillRowModel) }
    | RecalculateCoreSkillGroup ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.map (fun coreSkill ->
                    CoreSkill.update skillAdjustmentList model.attributeStat.lvl CoreSkill.CalculateDicePool coreSkill) }

open Feliz
open Feliz.Bulma

let view (model: CoreSkillGroup) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeStat.view model.attributeStat (AttributeMsg >> dispatch)
        Html.ul (
            List.mapi
                (fun position skill -> CoreSkill.view skill (fun msg -> dispatch (ModifyCoreSkill(position, msg))))
                model.coreSkillList
        )
    ]