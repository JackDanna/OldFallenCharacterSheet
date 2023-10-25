module CoreSkillTable

open FallenLib.SkillUtils

type Model = {
    attributeRow  : AttributeRow.Model
    skillRowList  : SkillRow.Model list
}

type Msg =
    | AttributeRowMsg of AttributeRow.Msg
    | Modify of int * SkillRow.Msg

let init() : Model = {
    attributeRow = AttributeRow.init()
    skillRowList = []
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->

        let newAttributeRow = AttributeRow.update attributeRowMsg model.attributeRow

        let attributeRowDiceCalc = neg1To4_To_d6_DicePoolCalc newAttributeRow.neg1To4Stat

        { model with 
            attributeRow = newAttributeRow
            skillRowList =
                List.map (fun skill -> 
                    SkillRow.update 
                        (SkillRow.Msg.SetAttributeDiceCalc attributeRowDiceCalc) 
                        skill
                ) model.skillRowList
        }

    | Modify (position, skillRowMsg) ->
        
        { model with 
            skillRowList =
                List.mapi ( fun i skillRowModel ->
                    if position = i then
                        SkillRow.update skillRowMsg skillRowModel
                    else 
                        skillRowModel
                ) model.skillRowList
        }
    
open Feliz
open Feliz.Bulma

let skillRowList (model: SkillRow.Model list) (dispatch: Msg -> unit) =
    
    model
    |> List.mapi ( 
        fun position skillRow -> 
            SkillRow.view 
                skillRow 
                (fun msg -> dispatch (Modify (position, msg)) )
    )
    |> Html.ul

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeRow.view model.attributeRow (AttributeRowMsg >> dispatch)
        skillRowList model.skillRowList dispatch
    ]