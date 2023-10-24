module VocationRow

open FallenLib.Dice
open FallenLib.SkillUtils

type GoverningAttribute = {
    isGoverning : bool
    attributeStat : AttributeRow.Model
}

type Model = {
    name                 : string
    vocationLevel        : Neg1To4Stat.Model
    governingAttributes  : GoverningAttribute list
    baseDice             : DicePoolCalculation
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | SetGoverningAttributes of GoverningAttribute list

let init() : Model = 
    {
      name = "Fellkin"
      vocationLevel = Neg1To4Stat.init()
      governingAttributes = [ {isGoverning=true;attributeStat=AttributeRow.init()} ]
      baseDice = baseDicePoolCalculation
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1ToStatMsg ->
        { model with vocationLevel = Neg1To4Stat.update neg1ToStatMsg model.vocationLevel }
    | SetGoverningAttributes governingAttributes ->
        { model with governingAttributes = governingAttributes }
        
open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =

    let temp (governingAttributes:GoverningAttribute list) =
        List.map ( fun governingAttribute ->
            Html.option [
                //prop.text governingAttribute.attributeStat.name
                //prop.isChecked governingAttribute.isGoverning
                prop.children [
                    Bulma.input.checkbox []
                ]
            ]
        ) governingAttributes

    let attributeDropdown (governingAttributes:GoverningAttribute list) =

        Bulma.dropdown [
            dropdown.isHoverable
            prop.children [
                Bulma.dropdownTrigger [
                    Bulma.button.button [
                        Html.span "Gov. Att."
                    ]
                ]
                Bulma.dropdownMenu [
                    Bulma.dropdownContent (
                        List.map ( fun governingAttribute ->
                            Bulma.dropdownItem.a [
                                prop.children [
                                    Bulma.columns [
                                        Bulma.column [
                                            Bulma.input.checkbox [
                                                //prop.text governingAttribute.attributeStat.name
                                                prop.onClick ( fun _ -> () )
                                                prop.isChecked governingAttribute.isGoverning
                                            ]
                                        ]
                                        Bulma.column [
                                            prop.text governingAttribute.attributeStat.name
                                        ]
                                    ]
                                ]
                            ]
                        ) governingAttributes
                    )
                ]
            ]
        ]

    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            (attributeDropdown model.governingAttributes) //(temp model.governingAttributes)
        ]
        Bulma.column [
            // skillToDicePoolString 
            //     model.baseDice model.vocationLevel model.attributeDiceCalc
            // |> prop.text

            prop.text "3d6"
        ]
        Bulma.column [
            Neg1To4Stat.view model.vocationLevel (Neg1To4StatMsg >> dispatch)
        ]
    ]