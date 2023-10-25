module VocationTable

type Model = {
    vocationRow : VocationRow.Model
    vocationalSkillRowList : VocationalSkillRowList.Model
}

type Msg =
    | VocationRowMsg of VocationRow.Msg
    | VocationalSkillRowListMsg of VocationalSkillRowList.Msg

let init() : Model = {
    vocationRow = VocationRow.init()
    vocationalSkillRowList = VocationalSkillRowList.init()
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocationRow = VocationRow.update vocationRowMsg model.vocationRow }
    | VocationalSkillRowListMsg vocationalSkillRowListMsg ->
        { model with vocationalSkillRowList = VocationalSkillRowList.update vocationalSkillRowListMsg model.vocationalSkillRowList }

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        VocationRow.view model.vocationRow (VocationRowMsg >> dispatch)
        VocationalSkillRowList.view model.vocationalSkillRowList (VocationalSkillRowListMsg >> dispatch)
    ]