module CoreSkillTable

type Model = {
    attributeRow  : SkillHeaderRow.Model
    skillRowList  : SkillRowList.Model
}

type Msg =
    | AttributeRowMsg of SkillHeaderRow.Msg
    | SkillRowListMsg of SkillRowList.Msg
    | Reset

let init() : Model = {
    attributeRow = SkillHeaderRow.init()
    skillRowList = SkillRowList.init()
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->
        { model with attributeRow = SkillHeaderRow.update attributeRowMsg model.attributeRow }

    | SkillRowListMsg skillRowListMsg ->

        // let skillRowListWithAttribute = 
        //     List.map ( fun (skillRow:SkillRow.Model) ->
        //         {skillRow with attributeLevel = model.attributeRow.level}
        //     ) model.skillRowList

        

        { model with skillRowList = SkillRowList.update skillRowListMsg skillRowListWithAttribute }
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