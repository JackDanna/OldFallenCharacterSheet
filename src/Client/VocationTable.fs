module VocationTable

open FallenLib.Vocation

type Model = {
    vocationRow : Vocation.Model
    vocationalSkillRowList : VocationalSkill.Model List
}

type Msg =
    | VocationRowMsg of Vocation.Msg
    | Insert
    | Remove
    | Modify of int * VocationalSkill.Msg
    | SetGoverningAttributes of AttributeStat.Model list

let init() : Model = {
    vocationRow =  Vocation.init()
    vocationalSkillRowList = [VocationalSkill.init ()]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocationRow = Vocation.update vocationRowMsg model.vocationRow }

    | Insert ->
        { model with 
            vocationalSkillRowList = List.append model.vocationalSkillRowList [VocationalSkill.init()]}

    | Remove ->
        { model with 
            vocationalSkillRowList = 
                model.vocationalSkillRowList |> List.rev |> List.tail |> List.rev
        }

    | Modify (position, msg) ->
        { model with 
            vocationalSkillRowList = 
                List.mapi ( fun i vocationalSkill ->
                    if position = i then
                        VocationalSkill.update model.vocationRow.level msg vocationalSkill
                    else 
                        vocationalSkill
                ) model.vocationalSkillRowList
        }
    | SetGoverningAttributes attributes ->
        { model with
            vocationRow = Vocation.update (Vocation.Msg.SetGoverningAttributes (attributes)) model.vocationRow }
    

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Vocation.view model.vocationRow (VocationRowMsg >> dispatch)

        Html.ul (
            List.append (
                List.mapi ( 
                    fun position skillRow ->

                        VocationalSkill.view
                            model.vocationRow.governingAttributes
                            model.vocationRow .level
                            skillRow
                            ( fun msg -> dispatch ( Modify (position,msg) ) )
                ) model.vocationalSkillRowList
            ) [
                Html.button [
                    prop.onClick (fun _ -> dispatch Insert)
                    prop.text "+"
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch Remove)
                    prop.text "-"
                ]
            ]
        )
    ]