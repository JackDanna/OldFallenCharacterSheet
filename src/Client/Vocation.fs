module Vocation

open FallenLib.Vocation
open FallenLib.Attribute

type Msg =
    | SetName of string
    | ZeroToFourStat of ZeroToFour.Msg
    | ToggleGoverningAttribute of AttributeName
    | Insert
    | Remove
    | Modify of int * VocationalSkill.Msg

let init () : Vocation =

    { name = ""
      level = ZeroToFour.init ()
      governingAttributes = []
      vocationalSkills = [] }

let update (attributeList: Attribute List) (msg: Msg) (model: Vocation) : Vocation =
    match msg with
    | SetName newName -> { model with name = newName }

    | ZeroToFourStat neg1ToStatMsg ->

        { model with level = ZeroToFour.update neg1ToStatMsg model.level }

    | ToggleGoverningAttribute attributeName ->
        // If the attributeName is already one of the governing attributes, remove it
        if List.contains attributeName model.governingAttributes then
            { model with
                governingAttributes =
                    model.governingAttributes
                    |> List.filter (fun currentAttributeName -> attributeName <> currentAttributeName) }
        // Else, if the attributeName isn't already one of the governing attributes, add it
        else
            { model with governingAttributes = List.append [ attributeName ] model.governingAttributes }

    | Insert -> { model with vocationalSkills = List.append model.vocationalSkills [ VocationalSkill.init () ] }

    | Remove ->
        { model with
            vocationalSkills =
                model.vocationalSkills
                |> List.rev
                |> List.tail
                |> List.rev }

    | Modify (position, msg) ->
        { model with
            vocationalSkills =
                model.vocationalSkills
                |> List.mapi (fun index vocationalSkill ->
                    if position = index then
                        VocationalSkill.update model.level msg vocationalSkill
                    else
                        vocationalSkill) }

open Feliz
open Feliz.Bulma

let governingAttributeItems (allAttributeNames: AttributeName list) (model: Vocation) (dispatch: Msg -> unit) =
    Bulma.dropdown [
        dropdown.isHoverable
        prop.children [
            Bulma.dropdownTrigger [
                Bulma.button.button [
                    Html.span "Gov. Att."
                ]
            ]
            Bulma.dropdownMenu [

                List.map
                    (fun attributeName ->
                        Bulma.dropdownItem.a [
                            prop.onClick (fun _ -> dispatch (ToggleGoverningAttribute attributeName))
                            prop.children [
                                Bulma.columns [
                                    Bulma.column [
                                        Bulma.input.checkbox [
                                            prop.isChecked (List.contains attributeName model.governingAttributes)
                                        ]
                                    ]
                                    Bulma.column [ prop.text attributeName ]
                                ]
                            ]
                        ])
                    allAttributeNames
                |> Bulma.dropdownContent
            ]
        ]
    ]


let view
    (combatVocationalSkills: string list)
    (allAttributeNames: AttributeName list)
    (stringifiedVocationDicePool: StringifiedVocationDicePool)
    (model: Vocation)
    (dispatch: Msg -> unit)
    =



    Bulma.box [

        Bulma.columns [
            Bulma.column [
                Bulma.input.text [
                    prop.value model.name
                    prop.onTextChange (fun value -> dispatch (SetName value))
                ]
            ]
            Bulma.column [
                governingAttributeItems allAttributeNames model dispatch
            ]
            Bulma.column [
                prop.text stringifiedVocationDicePool.dicePoolString
            ]
            Bulma.column [
                ZeroToFour.view model.level (ZeroToFourStat >> dispatch)
            ]
        ]

        Html.ul (
            List.append
                (List.mapi2
                    (fun position skillRow vocationalSkillDicePoolString ->
                        VocationalSkill.view
                            combatVocationalSkills
                            model.level
                            vocationalSkillDicePoolString
                            skillRow
                            (fun msg -> Modify(position, msg) |> dispatch))
                    model.vocationalSkills
                    stringifiedVocationDicePool.vocationalSkillDicePoolStrings)
                [ Html.button [
                      prop.onClick (fun _ -> dispatch Insert)
                      prop.text "+"
                  ]
                  Html.button [
                      prop.onClick (fun _ -> dispatch Remove)
                      prop.text "-"
                  ] ]
        )
    ]