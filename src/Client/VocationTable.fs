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
    | SetGoverningAttributes of AttributeStat.Model list

let init () : Model =
    { vocation = Vocation.init ()
      vocationalSkillList = [ VocationalSkill.init () ] }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg -> { model with vocation = Vocation.update vocationRowMsg model.vocation }

    | Insert -> { model with vocationalSkillList = List.append model.vocationalSkillList [ VocationalSkill.init () ] }

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
                List.mapi
                    (fun i vocationalSkill ->
                        if position = i then
                            VocationalSkill.update model.vocation.level msg vocationalSkill
                        else
                            vocationalSkill)
                    model.vocationalSkillList }
    | SetGoverningAttributes attributes ->
        { model with vocation = Vocation.update (Vocation.Msg.SetGoverningAttributes(attributes)) model.vocation }


open Feliz
open Feliz.Bulma

let view (attributeStats: AttributeStat list) (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Vocation.view attributeStats model.vocation (VocationRowMsg >> dispatch)

        Html.ul (
            List.append
                (List.mapi
                    (fun position skillRow ->

                        VocationalSkill.view
                            model.vocation.governingAttributes
                            model.vocation.level
                            skillRow
                            (fun msg -> dispatch (Modify(position, msg))))
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