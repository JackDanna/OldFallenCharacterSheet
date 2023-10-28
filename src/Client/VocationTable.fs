module VocationTable

open FallenLib.Vocation

type Model = {
    vocationRow : VocationRow.Model
    vocationalSkillRowList : VocationalSkillRow.Model List
}

type Msg =
    | VocationRowMsg of VocationRow.Msg
    | Insert
    | Remove
    | Modify of int * VocationalSkillRow.Msg
    | SetGoverningAttributes of AttributeRow.Model list

let init (governingAttributes:GoverningAttribute list) : Model =
    let vocationRow = VocationRow.init(governingAttributes)
    {
        vocationRow = vocationRow
        vocationalSkillRowList = [VocationalSkillRow.init (vocationRow) ]
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->

        let newVocationRow = VocationRow.update vocationRowMsg model.vocationRow
        { model with
            vocationRow = newVocationRow
            vocationalSkillRowList =
                List.map ( fun vocationalSkillRow ->
                    VocationalSkillRow.update (VocationalSkillRow.Msg.SetVocationRow (newVocationRow)) vocationalSkillRow
                ) model.vocationalSkillRowList
        }

    | Insert ->
        { model with 
            vocationalSkillRowList = 
                List.append model.vocationalSkillRowList [VocationalSkillRow.init(model.vocationRow)]
        }

    | Remove ->
        { model with 
            vocationalSkillRowList = 
                model.vocationalSkillRowList |> List.rev |> List.tail |> List.rev
        }

    | Modify (position, msg) ->
        { model with 
            vocationalSkillRowList = 
                List.mapi ( fun i (skillRow:VocationalSkillRow.Model) ->
                    if position = i then
                        VocationalSkillRow.update msg skillRow
                    else 
                        skillRow
                ) model.vocationalSkillRowList
        }
    | SetGoverningAttributes attributes ->
        let newVocationRow = VocationRow.update (VocationRow.Msg.SetGoverningAttributes (attributes)) model.vocationRow
        { model with
            vocationRow = newVocationRow 
            vocationalSkillRowList =
                List.map ( fun vocationalSkill ->
                    VocationalSkillRow.update (VocationalSkillRow.Msg.SetVocationRow (newVocationRow)) vocationalSkill 
                ) model.vocationalSkillRowList}
    

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        VocationRow.view model.vocationRow (VocationRowMsg >> dispatch)
        Html.ul (
            List.append (
                List.mapi ( 
                    fun position skillRow ->

                        VocationalSkillRow.view
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