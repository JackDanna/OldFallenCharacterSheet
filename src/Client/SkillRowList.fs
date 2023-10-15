module SkillRowList

type Model = SkillRow.Model List

type Msg =
    //| Insert
    //| Remove
    | Modify of int * SkillRow.Msg
    | Reset

let init() : Model = [SkillRow.init();SkillRow.init()]

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, skillRowMsg) ->
        model
        |> List.mapi (fun i skillRowModel ->
            if i = position then
                SkillRow.update skillRowMsg skillRowModel
            else
                skillRowModel
        )      
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    model
    |> List.mapi ( 
        fun position skillRow -> 
            SkillRow.view 
                skillRow 
                (fun msg -> dispatch (Modify (position, msg)) )
    )
    |> Html.ul

