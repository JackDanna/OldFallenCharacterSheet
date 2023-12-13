module VocationGroup

open FallenLib.Attribute
open FallenLib.VocationGroup
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | VocationRowMsg of Vocation.Msg
    | Insert
    | Remove
    | Modify of int * VocationalSkill.Msg
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : VocationGroup =
    let vocation = Vocation.init attributeStatList

    { vocation = vocation
      vocationalSkills = [ VocationalSkill.init vocation.governingAttributes ] }

let update
    (skillDiceModificationEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (attributeStatList: AttributeStat List)
    (msg: Msg)
    (model: VocationGroup)
    : VocationGroup =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        let newVocation =
            Vocation.update
                skillDiceModificationEffectList
                attributeDeterminedDiceModEffectList
                attributeStatList
                vocationRowMsg
                model.vocation

        { model with
            vocation = newVocation
            vocationalSkills =
                List.map
                    (fun vocationalSkill ->
                        VocationalSkill.update
                            skillDiceModificationEffectList
                            attributeDeterminedDiceModEffectList
                            newVocation.level
                            newVocation.governingAttributes
                            VocationalSkill.Msg.CalculateDicePool
                            vocationalSkill)
                    model.vocationalSkills }

    | Insert ->
        { model with
            vocationalSkills =
                List.append model.vocationalSkills [ VocationalSkill.init model.vocation.governingAttributes ] }

    | Remove ->
        { model with
            vocationalSkills =
                model.vocationalSkills
                |> List.rev
                |> List.tail
                |> List.rev }

    | Modify (position, msg) ->
        { model with
            vocationalSkills =
                model.vocationalSkills
                |> List.mapi (fun index vocationalSkill ->
                    if position = index then
                        VocationalSkill.update
                            skillDiceModificationEffectList
                            attributeDeterminedDiceModEffectList
                            model.vocation.level
                            model.vocation.governingAttributes
                            msg
                            vocationalSkill
                    else
                        vocationalSkill) }
    | SetAttributeStatsAndCalculateDicePools ->
        let newVocation =
            Vocation.update
                skillDiceModificationEffectList
                attributeDeterminedDiceModEffectList
                attributeStatList
                Vocation.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocation

        { model with
            vocation = newVocation
            vocationalSkills =
                List.map
                    (fun vocationalSkill ->
                        VocationalSkill.update
                            skillDiceModificationEffectList
                            attributeDeterminedDiceModEffectList
                            newVocation.level
                            newVocation.governingAttributes
                            VocationalSkill.Msg.CalculateDicePool
                            vocationalSkill)
                    model.vocationalSkills }


open Feliz
open Feliz.Bulma

let view (combatVocationalSkills: string list) (model: VocationGroup) (dispatch: Msg -> unit) =
    Bulma.box [
        Vocation.view model.vocation (VocationRowMsg >> dispatch)
        Html.ul (
            List.append
                (List.mapi
                    (fun position skillRow ->
                        VocationalSkill.view combatVocationalSkills model.vocation.level skillRow (fun msg ->
                            dispatch (Modify(position, msg))))
                    model.vocationalSkills)
                [ Html.button [
                      prop.onClick (fun _ -> dispatch Insert)
                      prop.text "+"
                  ]
                  Html.button [
                      prop.onClick (fun _ -> dispatch Remove)
                      prop.text "-"
                  ] ]
        )
    ]