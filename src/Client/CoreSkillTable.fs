module CoreSkillTable

type Model = {
    attributeRow   : AttributeRow.Model
    coreSkillList  : CoreSkillRow.Model list
}

type Msg =
    | AttributeRowMsg of AttributeRow.Msg
    | ModifyCoreSkill of int * CoreSkillRow.Msg

let init() : Model = {
    attributeRow = AttributeRow.init()
    coreSkillList = [
        CoreSkillRow.init (AttributeRow.init())
    ]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AttributeRowMsg attributeRowMsg ->

        let newAttributeRow = AttributeRow.update attributeRowMsg model.attributeRow

        { model with 
            attributeRow = newAttributeRow
            // When Attribute Row updates, set the new governing attributes for the CoreSkills
            coreSkillList =
                List.map (fun (coreSkill:CoreSkillRow.Model) -> { coreSkill with governingAttribute = newAttributeRow }) model.coreSkillList
        }

    | ModifyCoreSkill (position, skillRowMsg) ->
        
        { model with 
            coreSkillList =
                List.mapi ( fun i skillRowModel ->
                    if position = i then

                        CoreSkillRow.update skillRowMsg {skillRowModel with governingAttribute = model.attributeRow}
                    else 
                        skillRowModel
                ) model.coreSkillList
        }
    
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        AttributeRow.view model.attributeRow (AttributeRowMsg >> dispatch)
        Html.ul (
            List.mapi ( 
                fun position skill ->
                    CoreSkillRow.view 
                        skill 
                        (fun msg -> dispatch (ModifyCoreSkill (position, msg)))
            ) model.coreSkillList
        )
    ]