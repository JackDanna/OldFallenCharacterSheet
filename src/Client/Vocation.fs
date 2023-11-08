module Vocation

open FallenLib.Vocation
open FallenLib.Attribute
open FallenLib.Dice

type Msg =
    | SetName of string
    | ZeroToFourStat of ZeroToFourStat.Msg
    | ToggleGoverningAttribute of int
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : Vocation =
    let lvl = ZeroToFourStat.init ()
    let governingAttribues = attributesToGoverningAttributesInit attributeStatList

    { name = ""
      level = lvl
      governingAttributes = governingAttribues
      dicePool = vocationToDicePool baseDicePool lvl governingAttribues }

let update (attributeStatList: AttributeStat List) (msg: Msg) (model: Vocation) : Vocation =
    match msg with
    | SetName newName -> { model with name = newName }

    | ZeroToFourStat neg1ToStatMsg -> { model with level = ZeroToFourStat.update neg1ToStatMsg model.level }

    | ToggleGoverningAttribute position ->
        let toggledGoverningAttributes =
            model.governingAttributes
            |> List.mapi (fun i governingAttribute ->
                if position = i then
                    { governingAttribute with isGoverning = not governingAttribute.isGoverning }
                else
                    governingAttribute)

        { model with governingAttributes = attributesToGoverningAttributes attributeStatList toggledGoverningAttributes }

    | SetAttributeStatsAndCalculateDicePools ->
        { model with governingAttributes = attributesToGoverningAttributes attributeStatList model.governingAttributes }

open Feliz
open Feliz.Bulma

let governingAttributeItems (model: Vocation) (dispatch: Msg -> unit) =
    List.mapi
        (fun i governingAttribute ->
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
            ])
        model.governingAttributes

let view (model: Vocation) (dispatch: Msg -> unit) =

    Bulma.columns [
        Bulma.column [
            Bulma.input.text [
                prop.value model.name
                prop.onTextChange (fun value -> dispatch (SetName value))
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
            vocationToDicePool baseDicePool model.level model.governingAttributes
            |> dicePoolToString
            |> prop.text
        ]
        Bulma.column [
            ZeroToFourStat.view model.level (ZeroToFourStat >> dispatch)
        ]
    ]