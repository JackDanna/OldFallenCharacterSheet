module VocationTable

type Model = {
    vocationRow : VocationRow.Model
    vocationalSkillRowList : VocationalSkillRow.Model List
}

type Msg =
    | VocationRowMsg of VocationRow.Msg
    | Insert
    | Remove
    | Modify of int * VocationalSkillRow.Msg

let init() : Model = {
    vocationRow = VocationRow.init()
    vocationalSkillRowList = [VocationalSkillRow.init()]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocationRow = VocationRow.update vocationRowMsg model.vocationRow }
    | Insert ->
        { model with 
            vocationalSkillRowList = 
                List.append model.vocationalSkillRowList [VocationalSkillRow.init()]
        }

    | Remove ->
        { model with 
            vocationalSkillRowList = 
                model.vocationalSkillRowList |> List.rev |> List.tail |> List.rev
        }

    | Modify (position, skillRowMsg) ->

        { model with 
            vocationalSkillRowList = 
                List.mapi ( fun i skillRowModel ->
                    if position = i then
                        VocationalSkillRow.update skillRowMsg skillRowModel
                    else 
                        skillRowModel
                ) model.vocationalSkillRowList
        }

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
                            (fun msg -> dispatch (Modify (position, msg)) )
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