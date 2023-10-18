module SkillRowList

open FallenLib.Dice

type Model = Skill.Model list

type Msg =
    //| Insert
    //| Remove
    | SetAttributeDiceCalc of DicePoolCalculation
    | Modify of int * Skill.Msg
    | Reset

let init() : Model = [Skill.init();Skill.init();]

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetAttributeDiceCalc attrbuteNeg1To4Stat ->

        List.map (fun skill -> 
            Skill.update (Skill.Msg.SetAttributeDiceCalc attrbuteNeg1To4Stat) skill
        ) model

    | Modify (position, skillRowMsg) ->
        
        List.mapi ( fun i skillRowModel ->
            if position = i then
                Skill.update skillRowMsg skillRowModel
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
            Skill.view 
                skillRow 
                (fun msg -> dispatch (Modify (position, msg)) )
    )
    |> Html.ul

