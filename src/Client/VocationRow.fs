module VocationRow

open FallenLib.Dice

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
    | ToggleGoverningAttribute of int

let init() : Model = 
    {
      name = "Fellkin"
      vocationLevel = Neg1To4Stat.init()
      governingAttributes = [
            { isGoverning=false;attributeStat=AttributeRow.init() }
            { isGoverning=false;attributeStat= {AttributeRow.init() with name = "RFX"} }
        ]
      baseDice = baseDicePoolCalculation
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1ToStatMsg ->
        { model with vocationLevel = Neg1To4Stat.update neg1ToStatMsg model.vocationLevel }
    | SetGoverningAttributes governingAttributes ->
        { model with governingAttributes = governingAttributes }
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

let view (model: Model) (dispatch: Msg -> unit) =

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
            prop.text "3d6"
        ]
        Bulma.column [
            Neg1To4Stat.view model.vocationLevel (Neg1To4StatMsg >> dispatch)
        ]
    ]