module CoreSkillTables

type Model = CoreSkillTable.Model list

type Msg =
    | Modify of int * CoreSkillTable.Msg

let init() : Model = []

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Modify (position, coreSkillTableMsg) ->

        List.mapi ( fun i coreSkilTableModel ->
            if i = position then
                CoreSkillTable.update coreSkillTableMsg coreSkilTableModel
            else
                coreSkilTableModel
        ) model

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    Bulma.container [
        Bulma.label [
            prop.text "Attributes and Core Skills:"
        ]
        |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> List.mapi ( 
                    fun position coreSkillTable -> 
                        Bulma.column [
                            CoreSkillTable.view 
                                coreSkillTable
                                (fun msg -> dispatch (Modify (position, msg)) )
                        ]
                )
                |> Bulma.columns
            ]
        ]
    ]
    

