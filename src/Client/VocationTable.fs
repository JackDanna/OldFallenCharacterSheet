module VocationTable

open FallenLib.SkillUtils

type Model = {
    vocationRow          : SkillRow.Model
    governingAttributes  : AttributeRow.Model list
    skillRowList         : SkillRowList.Model
}

type Msg =
    | VocationRowMsg  of SkillRow.Msg
    | SkillRowListMsg of SkillRowList.Msg
    | SetGoverningAttributes of AttributeRow.Model list
    | Reset

let init() : Model = {
    vocationRow  = SkillRow.init()
    governingAttributes = [ AttributeRow.init() ]
    skillRowList = SkillRowList.init()
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | VocationRowMsg vocationRowMsg ->
        { model with vocationRow = SkillRow.update vocationRowMsg model.vocationRow }

    | SetGoverningAttributes attributeRowList ->
        { model with governingAttributes = attributeRowList }

    | SkillRowListMsg skillRowListMsg ->
        { model with skillRowList = SkillRowList.update skillRowListMsg model.skillRowList }
        
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        SkillRow.view model.vocationRow (VocationRowMsg >> dispatch)
        SkillRowList.view model.skillRowList (SkillRowListMsg >> dispatch)
    ]