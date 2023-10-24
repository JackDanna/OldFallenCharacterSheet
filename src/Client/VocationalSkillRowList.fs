module VocationalSkillRowList

open FallenLib.Dice

type Model = VocationalSkillRow.Model list

type Msg =
    | Insert
    | Remove
    | Modify of int * VocationalSkillRow.Msg

let init() : Model = []

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Insert ->
        List.append model [VocationalSkillRow.init()]

    | Remove ->
        model |> List.rev |> List.tail |> List.rev

    | Modify (position, skillRowMsg) ->
        
        List.mapi ( fun i skillRowModel ->
            if position = i then
                VocationalSkillRow.update skillRowMsg skillRowModel
            else 
                skillRowModel
        ) model


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
            Html.button [
                prop.onClick (fun _ -> dispatch Remove)
                prop.text "-"
            ]
        ]

    Html.ul fullList

