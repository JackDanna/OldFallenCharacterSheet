module VocationRow

open FallenLib.SkillUtils
open FallenLib.Dice

type Model = {
    name : string
    level : Neg1To4Stat.Model
    governingAttributes : GoverningAttribute list
}

type Msg =
    | SetName of string
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | ToggleGoverningAttribute of int

let init() : Model = {
    name = ""
    level = Neg1To4Stat.init()
    governingAttributes = []
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetName newName -> { model with name = newName }

    | Neg1To4StatMsg neg1ToStatMsg ->
        { model with level = Neg1To4Stat.update neg1ToStatMsg model.level }

    | ToggleGoverningAttribute index ->

        { model with 
            governingAttributes =
                List.mapi ( fun i governingAttribute ->
                    if index = i then
                        { governingAttribute with isGoverning = not governingAttribute.isGoverning }
                    else 
                        governingAttribute
                ) model.governingAttributes 
        }

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

let view (model: Model) (dispatch: Msg -> unit) =

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
            vocationToDicePoolString baseDicePoolCalculation model.level model.governingAttributes
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view model.level (Neg1To4StatMsg >> dispatch)
        ]
    ]
