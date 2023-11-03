module Vocation

open FallenLib.Vocation
open FallenLib.Attribute
open FallenLib.Dice

type Model = Vocation

let attributesToGoverningAttributesInit attributes =
    List.map ( fun (attributeStat:AttributeStat.Model) ->
        {
            attributeStat = attributeStat
            isGoverning = false
        }
    ) attributes

let attributesToGoverningAttributes attributes governingAttributes =
    attributes
    |> List.map ( fun (attributeStat:AttributeStat) ->
        {
            attributeStat = attributeStat
            isGoverning =
                governingAttributes
                |> List.collect ( fun currentGoverningAttribute ->
                    if (currentGoverningAttribute.attributeStat.attribute = attributeStat.attribute) && currentGoverningAttribute.isGoverning then
                        [currentGoverningAttribute.isGoverning]
                    else
                        []
                )
                |> List.isEmpty
                |> not
        }
    )

type Msg =
    | SetName of string
    | ZeroToFourStat of ZeroToFourStat.Msg
    | ToggleGoverningAttribute of int

let init (attributeStatList:AttributeStat List) : Model = {
    name = ""
    level = ZeroToFourStat.init()
    governingAttributes = attributesToGoverningAttributesInit attributeStatList
}

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetName newName -> { model with name = newName }

    | ZeroToFourStat neg1ToStatMsg ->
        { model with level = ZeroToFourStat.update neg1ToStatMsg model.level }

    | ToggleGoverningAttribute position ->

        { model with 
            governingAttributes =
                List.mapi ( fun index governingAttribute ->
                    if position = index then
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
                        prop.text governingAttribute.attributeStat.attribute
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
            vocationToString baseDicePool model.level model.governingAttributes
            |> prop.text
        ]
        Bulma.column [
            ZeroToFourStat.view model.level (ZeroToFourStat >> dispatch)
        ]
    ]
