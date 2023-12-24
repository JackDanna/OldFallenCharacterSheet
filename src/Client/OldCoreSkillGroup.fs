module OldSkillGroup

open FallenLib.CoreSkillGroup
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | AttributeMsg of Attribute.Msg
    | ModifyCoreSkill of int * OldCoreSkill.Msg
    | RecalculateCoreSkillGroup

let init
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    : CoreSkillGroup =
    let attributeStat = Attribute.init ()

    { attributeStat = attributeStat
      coreSkillList = [ OldCoreSkill.init skillDiceModEffectList attributeDeterminedDiceModEffectList attributeStat ] }

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
                        OldCoreSkill.update
                            skillDiceModEffectList
                            attributeDeterminedDiceModEffectList
                            newAttributeStat
                            OldCoreSkill.CalculateDicePool
                            coreSkill)
                    model.coreSkillList }

    | ModifyCoreSkill (position, skillRowMsg) ->
        { model with
            coreSkillList =
                model.coreSkillList
                |> List.mapi (fun i skillRowModel ->
                    if position = i then
                        OldCoreSkill.update
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
                    OldCoreSkill.update
                        skillDiceModEffectList
                        attributeDeterminedDiceModEffectList
                        model.attributeStat
                        OldCoreSkill.CalculateDicePool
                        coreSkill) }

open Feliz
open Feliz.Bulma

let view (model: CoreSkillGroup) (dispatch: Msg -> unit) =
    Bulma.box [
        Attribute.view model.attributeStat (AttributeMsg >> dispatch)
        Html.ul (
            List.mapi
                (fun position skill -> OldCoreSkill.view skill (fun msg -> dispatch (ModifyCoreSkill(position, msg))))
                model.coreSkillList
        )
    ]