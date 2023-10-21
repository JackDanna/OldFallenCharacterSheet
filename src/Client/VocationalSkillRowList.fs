module VocationalSkillRowList

open FallenLib.Dice

type Model = VocationalSkillRow.Model list

type Msg =
    | Insert
    //| Remove
    | SetAttributeDiceCalc of DicePoolCalculation
    | Modify of int * VocationalSkillRow.Msg
    | Reset

let init() : Model = [VocationalSkillRow.init();VocationalSkillRow.init();]

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetAttributeDiceCalc attrbuteNeg1To4Stat ->

        List.map (fun skill -> 
            VocationalSkillRow.update (VocationalSkillRow.Msg.SetAttributeDiceCalc attrbuteNeg1To4Stat) skill
        ) model

    // Set Vocation Skill
    | Insert ->
        VocationalSkillRow.init() :: model

    // Remove


    | Modify (position, skillRowMsg) ->
        
        List.mapi ( fun i skillRowModel ->
            if position = i then
                VocationalSkillRow.update skillRowMsg skillRowModel
            else 
                skillRowModel
        ) model

             
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    
    let vocationalSkillList = 
        List.mapi ( 
            fun position skillRow -> 
                VocationalSkillRow.view 
                    skillRow 
                    (fun msg -> dispatch (Modify (position, msg)) )
        ) model

    let fullList = 
        List.append vocationalSkillList [
            Html.button [
                prop.onClick (fun _ -> dispatch Insert)
                prop.text "+"
            ]
        ]

    Html.ul fullList

