module SkillRowList

type Model = {
    governingAttributeLevel : Neg1To4Stat.Model
    skillRows : SkillRow.Model list
}

type Msg =
    //| Insert
    //| Remove
    | SetGoverningAttributeLevel of Neg1To4Stat.Model
    | Modify of int * SkillRow.Msg
    | Reset

let init() : Model = { 
    governingAttributeLevel = Neg1To4Stat.init()
    skillRows = [SkillRow.init();SkillRow.init();]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetGoverningAttributeLevel attrbuteNeg1To4Stat ->
        
    | Modify (position, skillRowMsg) ->
        
        let newskillRows =
            List.mapi ( fun i skillRowModel ->
                if position = i then
                    SkillRow.update skillRowMsg skillRowModel
                else 
                    skillRowModel
            ) model.skillRows

        { model with skillRows = newskillRows}
             
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    model.skillRows
    |> List.mapi ( 
        fun position skillRow -> 
            SkillRow.view 
                skillRow 
                (fun msg -> dispatch (Modify (position, msg)) )
    )
    |> Html.ul

