module VocationalSkill

open FallenLib.VocationGroup
open FallenLib.Vocation
open FallenLib.Dice
open FallenLib.SkillStat

type Model = SkillStat

type Msg =
    | SetName of string
    | VocationalSkillStatMsg of VocationalSkillStat.Msg

let init () : Model = 
    {
        name = ""
        lvl = Neg1To4Stat.init()
    }


let update (levelCap:ZeroToFourStat.Model) (msg: Msg) (model: Model) : Model =
    match msg with
    | SetName name -> { model with name = name }
    | VocationalSkillStatMsg vocationalSkillStatMsg ->
        { model with lvl = VocationalSkillStat.update levelCap vocationalSkillStatMsg model.lvl }
        
open Feliz
open Feliz.Bulma

let view (governingAttributes:GoverningAttribute list) (levelCap:ZeroToFourStat.Model) (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            Bulma.dropdown [
                Bulma.dropdownTrigger [
                    Bulma.input.text [ 
                        prop.defaultValue model.name
                        prop.onTextChange (fun value -> dispatch (SetName value) )
                    ]
                ]
                Bulma.dropdownMenu [
                    Bulma.dropdownContent [
                        Bulma.dropdownItem.a [
                            prop.children [
                                Html.span "Single Sequence"
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
            vocationalSkillToString
                baseDicePool
                model.lvl
                governingAttributes
            |> prop.text
        ]
        Bulma.column [
            VocationalSkillStat.view
                levelCap
                model.lvl
                (VocationalSkillStatMsg >> dispatch)
        ]
    ]