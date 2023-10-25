module VocationTable

open FallenLib.Dice
open FallenLib.SkillUtils
open FallenLib.Attribute

type Model = {
    name                   : string
    vocationLevel          : Neg1To4Stat.Model
    governingAttributes    : GoverningAttribute list
    baseDice               : DicePoolCalculation
    vocationalSkillRowList : VocationalSkillRowList.Model
}

type Msg =
    | SetName of string
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | ToggleGoverningAttribute of int
    | VocationalSkillRowListMsg of VocationalSkillRowList.Msg

let init() : Model = {
    name = ""
    vocationLevel = Neg1To4Stat.init()
    governingAttributes = []
    baseDice = baseDicePoolCalculation
    vocationalSkillRowList = VocationalSkillRowList.init()
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetName newName -> { model with name = newName }
    | Neg1To4StatMsg neg1ToStatMsg ->
        { model with vocationLevel = Neg1To4Stat.update neg1ToStatMsg model.vocationLevel }

    | ToggleGoverningAttribute index ->

        let newGoverningAttributes =
            List.mapi ( fun i governingAttribute ->
                    if index = i then
                        { governingAttribute with isGoverning = not governingAttribute.isGoverning }
                    else 
                        governingAttribute
                ) model.governingAttributes

        let newGoverningAttributeDiceCalc = governingAttributesToDiceCalc newGoverningAttributes

        { model with 
            governingAttributes = newGoverningAttributes
            vocationalSkillRowList = 
                List.map ( fun (vocationalSkillRow:VocationalSkillRow.Model) ->
                    { vocationalSkillRow with attributeDiceCalc = newGoverningAttributeDiceCalc }
                ) model.vocationalSkillRowList
        }

    | VocationalSkillRowListMsg vocationalSkillRowListMsg ->
        { model with vocationalSkillRowList = VocationalSkillRowList.update vocationalSkillRowListMsg model.vocationalSkillRowList }

open Feliz
open Feliz.Bulma

let governingAttributeItems (model: Model) (dispatch: Msg -> unit) =
    List.mapi ( fun i governingAttribute ->
        Bulma.dropdownItem.a [
            prop.onClick (fun _ -> dispatch (ToggleGoverningAttribute i))
            prop.children [
                Bulma.columns [
                    Bulma.column [
                        Bulma.input.checkbox [
                            prop.isChecked governingAttribute.isGoverning
                        ]
                    ]
                    Bulma.column [
                        prop.text governingAttribute.attributeStat.name
                    ]
                ]
            ]
        ]
    ) model.governingAttributes

    
let vocationRow (model: Model) (dispatch: Msg -> unit) =

    Bulma.columns [
        Bulma.column [
            Bulma.input.text [ 
                prop.value model.name
                prop.onTextChange (fun value -> dispatch (SetName value) )
            ]
        ]
        Bulma.column [
            Bulma.dropdown [
                dropdown.isHoverable
                prop.children [
                    Bulma.dropdownTrigger [
                        Bulma.button.button [
                            Html.span "Gov. Att."
                        ]
                    ]
                    Bulma.dropdownMenu [
                        governingAttributeItems model dispatch 
                        |> Bulma.dropdownContent
                    ]
                ]
            ]
        ]
        Bulma.column [
            vocationToDicePoolString model.baseDice model.vocationLevel model.governingAttributes
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view model.vocationLevel (Neg1To4StatMsg >> dispatch)
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        vocationRow model dispatch
        VocationalSkillRowList.view model.vocationalSkillRowList (VocationalSkillRowListMsg >> dispatch)
    ]