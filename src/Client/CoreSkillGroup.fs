module CoreSkillGroup

open FallenLib.CoreSkillGroup
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | AttributeMsg of Attribute.Msg
    | ModifyCoreSkill of int * CoreSkill.Msg
    | RecalculateCoreSkillGroup

let init
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    : CoreSkillGroup =
    let attributeStat = Attribute.init ()

    { attributeStat = attributeStat
      coreSkillList = [ CoreSkill.init skillDiceModEffectList attributeDeterminedDiceModEffectList attributeStat ] }

let update
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (msg: Msg)
    (model: CoreSkillGroup)
    : CoreSkillGroup =
    match msg with
    | AttributeMsg attributeRowMsg ->
        let newAttributeStat = Attribute.update attributeRowMsg model.attributeStat

        { model with
            attributeStat = newAttributeStat
            coreSkillList =
                List.map
                    (fun coreSkill ->
                        CoreSkill.update
                            skillDiceModEffectList
                            attributeDeterminedDiceModEffectList
                            newAttributeStat
                            CoreSkill.CalculateDicePool
                            coreSkill)
                    model.coreSkillList }

    | ModifyCoreSkill (position, skillRowMsg) ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.mapi (fun i skillRowModel ->
                    if position = i then
                        CoreSkill.update
                            skillDiceModEffectList
                            attributeDeterminedDiceModEffectList
                            model.attributeStat
                            skillRowMsg
                            skillRowModel
                    else
                        skillRowModel) }
    | RecalculateCoreSkillGroup ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.map (fun coreSkill ->
                    CoreSkill.update
                        skillDiceModEffectList
                        attributeDeterminedDiceModEffectList
                        model.attributeStat
                        CoreSkill.CalculateDicePool
                        coreSkill) }

open Feliz
open Feliz.Bulma

let view (model: CoreSkillGroup) (dispatch: Msg -> unit) =
    Bulma.box [
        Attribute.view model.attributeStat (AttributeMsg >> dispatch)
        Html.ul (
            List.mapi
                (fun position skill -> CoreSkill.view skill (fun msg -> dispatch (ModifyCoreSkill(position, msg))))
                model.coreSkillList
        )
    ]