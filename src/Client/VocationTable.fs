module VocationTable

open FallenLib.Attribute

type Model =
    { vocation: Vocation.Model
      vocationalSkillList: VocationalSkill.Model List }

type Msg =
    | VocationRowMsg of Vocation.Msg
    | Insert
    | Remove
    | Modify of int * VocationalSkill.Msg
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : Model =
    let vocation = Vocation.init attributeStatList

    { vocation = vocation
      vocationalSkillList = [ VocationalSkill.init vocation.governingAttributes ] }

let update (attributeStatList: AttributeStat List) (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocation = Vocation.update attributeStatList vocationRowMsg model.vocation }

    | Insert ->
        { model with
            vocationalSkillList =
                List.append model.vocationalSkillList [ VocationalSkill.init model.vocation.governingAttributes ] }

    | Remove ->
        { model with
            vocationalSkillList =
                model.vocationalSkillList
                |> List.rev
                |> List.tail
                |> List.rev }

    | Modify (position, msg) ->
        { model with
            vocationalSkillList =
                model.vocationalSkillList
                |> List.mapi (fun index vocationalSkill ->
                    if position = index then
                        VocationalSkill.update
                            model.vocation.level
                            model.vocation.governingAttributes
                            msg
                            vocationalSkill
                    else
                        vocationalSkill) }
    | SetAttributeStatsAndCalculateDicePools ->
        let newVocation =
            Vocation.update attributeStatList Vocation.Msg.SetAttributeStatsAndCalculateDicePools model.vocation

        { model with
            vocation = newVocation
            vocationalSkillList =
                List.map
                    (fun vocationalSkill ->
                        VocationalSkill.update
                            newVocation.level
                            newVocation.governingAttributes
                            VocationalSkill.Msg.CalculateDicePool
                            vocationalSkill)
                    model.vocationalSkillList }


open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Vocation.view model.vocation (VocationRowMsg >> dispatch)
        Html.ul (
            List.append
                (List.mapi
                    (fun position skillRow ->
                        VocationalSkill.view model.vocation.level skillRow (fun msg -> dispatch (Modify(position, msg))))
                    model.vocationalSkillList)
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