module CoreSkillTable

open FallenLib.SkillUtils

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
        let newSkillHeaderRow = SkillHeaderRow.update attributeRowMsg model.attributeRow
        let attributeDicePoolCalc = neg1To4_To_d6_DicePoolCalc newSkillHeaderRow.neg1To4Stat
        let newSkillList = SkillRowList.update (SkillRowList.Msg.SetAttributeDiceCalc attributeDicePoolCalc) model.skillRowList
        { model with 
            attributeRow = newSkillHeaderRow
            skillRowList = newSkillList }

    | SkillRowListMsg skillRowListMsg ->

        { model with skillRowList = SkillRowList.update skillRowListMsg model.skillRowList }
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        SkillHeaderRow.view model.attributeRow (AttributeRowMsg >> dispatch)
        SkillRowList.view model.skillRowList (SkillRowListMsg >> dispatch)
    ]