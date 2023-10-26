module VocationRow

open FallenLib.Vocation
open FallenLib.Dice

type Model = {
    name : string
    level : VocationStat.Model
    governingAttributes : GoverningAttribute list
}

type Msg =
    | SetName of string
    | VocationStatMsg of VocationStat.Msg
    | ToggleGoverningAttribute of int

let init() : Model = {
    name = ""
    level = VocationStat.init()
    governingAttributes = []
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetName newName -> { model with name = newName }

    | VocationStatMsg neg1ToStatMsg ->
        { model with level = VocationStat.update neg1ToStatMsg model.level }

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
            vocationToDicePoolString baseDicePoolCalculation model.governingAttributes model.level
            |> prop.text
        ]
        Bulma.column [
            VocationStat.view model.level (VocationStatMsg >> dispatch)
        ]
    ]
