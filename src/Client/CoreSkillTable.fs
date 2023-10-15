module CoreSkillTable

open FallenLib.Attribute
open Feliz
open Feliz.Bulma


type Model = {
    attributeStat : AttributeStat
    skillRowList  : SkillRowList.Model
}

type Msg =
    | SkillRowListMsg of SkillRowList.Msg
    | Reset

let init() : Model =
    {
        attributeStat = emptyAttributeStat
        skillRowList = [
            { skillStat = FallenLib.SkillStat.emptySkillStat
              dicePool = "3d6" }
        ]
    }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SkillRowListMsg skillRowListMsg ->
        { model with skillRowList = SkillRowList.update skillRowListMsg model.skillRowList }
    | Reset -> init()

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.column [
        Bulma.notification [
            color.isPrimary
            prop.children [
                SkillRowList.view model.skillRowList (SkillRowListMsg >> dispatch)
            ]
        ]
    ]