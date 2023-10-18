module SkillRowList

open FallenLib.Dice

type Model = SkillRow.Model list

type Msg =
    //| Insert
    //| Remove
    | SetAttributeDiceCalc of DicePoolCalculation
    | Modify of int * SkillRow.Msg
    | Reset

let init() : Model = [SkillRow.init();SkillRow.init();]

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetAttributeDiceCalc attrbuteNeg1To4Stat ->

        List.map (fun skill -> 
            SkillRow.update (SkillRow.Msg.SetAttributeDiceCalc attrbuteNeg1To4Stat) skill
        ) model

    | Modify (position, skillRowMsg) ->
        
        List.mapi ( fun i skillRowModel ->
            if position = i then
                SkillRow.update skillRowMsg skillRowModel
            else 
                skillRowModel
        ) model

             
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

