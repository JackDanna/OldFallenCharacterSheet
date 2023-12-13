module Vocation

open FallenLib.Vocation
open FallenLib.Attribute
open FallenLib.Dice
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | SetName of string
    | ZeroToFourStat of ZeroToFour.Msg
    | ToggleGoverningAttribute of int
    | SetAttributeStatsAndCalculateDicePools

let init (attributeStatList: AttributeStat List) : Vocation =
    let lvl = ZeroToFour.init ()
    let governingAttribues = attributesToGoverningAttributesInit attributeStatList

    { name = ""
      level = lvl
      governingAttributes = governingAttribues
      dicePool = vocationToDicePool baseDicePool lvl governingAttribues [] }

let update
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (attributeStatList: AttributeStat List)
    (msg: Msg)
    (model: Vocation)
    : Vocation =
    match msg with
    | SetName newName -> { model with name = newName }

    | ZeroToFourStat neg1ToStatMsg ->

        let newLevel = ZeroToFour.update neg1ToStatMsg model.level

        { model with
            level = newLevel
            dicePool =
                vocationToDicePool
                    baseDicePool
                    newLevel
                    model.governingAttributes
                    (collectSkillAdjustmentDiceMods model.name skillDiceModEffectList) }

    | ToggleGoverningAttribute position ->
        let toggledGoverningAttributes =
            model.governingAttributes
            |> List.mapi (fun i governingAttribute ->
                if position = i then
                    { governingAttribute with isGoverning = not governingAttribute.isGoverning }
                else
                    governingAttribute)

        let newGoverningAttributes =
            attributesToGoverningAttributes attributeStatList toggledGoverningAttributes

        { model with
            governingAttributes = newGoverningAttributes
            dicePool =
                vocationToDicePool
                    baseDicePool
                    model.level
                    newGoverningAttributes
                    (collectSkillAdjustmentDiceMods model.name skillDiceModEffectList) }

    | SetAttributeStatsAndCalculateDicePools ->
        let newGoverningAttributes =
            attributesToGoverningAttributes attributeStatList model.governingAttributes

        { model with
            governingAttributes = newGoverningAttributes
            dicePool =
                vocationToDicePool
                    baseDicePool
                    model.level
                    newGoverningAttributes
                    (collectSkillAdjustmentDiceMods model.name skillDiceModEffectList) }

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
            prop.text (dicePoolToString model.dicePool)
        ]
        Bulma.column [
            ZeroToFour.view model.level (ZeroToFourStat >> dispatch)
        ]
    ]