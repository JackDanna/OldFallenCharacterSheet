module CoreSkillTable

open FallenLib.SkillUtils
open FallenLib.Dice

type Skill = {
    name  : string
    level : Neg1To4Stat.Model
}

type Model = {
    attributeRow   : AttributeRow.Model
    coreSkillList  : Skill list
}

type Msg =
    | AttributeRowMsg of AttributeRow.Msg
    | ModifyCoreSkillLevel of int * Neg1To4Stat.Msg

let init() : Model = {
    attributeRow = AttributeRow.init()
    coreSkillList = []
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->
        { model with 
            attributeRow = AttributeRow.update attributeRowMsg model.attributeRow }

    | ModifyCoreSkillLevel (position, skillRowMsg) ->
        
        { model with 
            coreSkillList =
                List.mapi ( fun i skillRowModel ->
                    if position = i then
                        { skillRowModel with level = Neg1To4Stat.update skillRowMsg skillRowModel.level}
                    else 
                        skillRowModel
                ) model.coreSkillList
        }
    
open Feliz
open Feliz.Bulma

let skillRow (skill:Skill) (preloadedSkillToDicePoolString: Neg1To4Stat.Model -> string) (dispatch: Neg1To4Stat.Msg-> unit) =
    Bulma.columns [
        Bulma.column [ prop.text skill.name ]
        Bulma.column [
            preloadedSkillToDicePoolString skill.level
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view skill.level dispatch
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeRow.view model.attributeRow (AttributeRowMsg >> dispatch)
        Html.ul (
            List.mapi ( 
                fun position skill ->
                    skillRow
                        skill
                        (coreSkillToDicePoolString baseDicePoolCalculation model.attributeRow.neg1To4Stat)
                        (fun msg -> dispatch (ModifyCoreSkillLevel (position, msg)) )
            ) model.coreSkillList
        )
    ]