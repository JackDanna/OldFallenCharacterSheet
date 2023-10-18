module SkillRowList

type Model = {
    governingAttributeLevel : Neg1To4Stat.Model
    skillRowList : SkillRow.Model list
}

type Msg =
    //| Insert
    //| Remove
    | Modify of int * SkillRow.Msg
    | Reset

let init() : Model = { 
    governingAttributeLevel = Neg1To4Stat.init()
    skillRowList = [SkillRow.init();SkillRow.init();]
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, skillRowMsg) ->
        
        let newSkillRowList =
            List.mapi ( fun i skillRowModel ->
                if position = i then
                    SkillRow.update skillRowMsg skillRowModel
                else 
                    skillRowModel
            ) model.skillRowList

        { model with skillRowList = newSkillRowList}
             
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    model.skillRowList
    |> List.mapi ( 
        fun position skillRow -> 
            SkillRow.view 
                skillRow 
                (fun msg -> dispatch (Modify (position, msg)) )
    )
    |> Html.ul

