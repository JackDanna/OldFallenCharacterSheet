module CoreSkillTable

type Model = {
    attributeRow : AttributeRow.Model
    skillRowList  : SkillRowList.Model
}

type Msg =
    | AttributeRowMsg of AttributeRow.Msg
    | SkillRowListMsg of SkillRowList.Msg
    | Reset

let init() : Model =
    {
        attributeRow = AttributeRow.init()
        skillRowList = SkillRowList.init()
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->
        { model with attributeRow = AttributeRow.update attributeRowMsg model.attributeRow }
    | SkillRowListMsg skillRowListMsg ->
        { model with skillRowList = SkillRowList.update skillRowListMsg model.skillRowList }
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.notification [
        color.isPrimary
        prop.children [
            AttributeRow.view model.attributeRow (AttributeRowMsg >> dispatch)
            SkillRowList.view model.skillRowList (SkillRowListMsg >> dispatch)
        ]
    ]