module VocationTable

open FallenLib.Dice
open FallenLib.SkillUtils

type Model = {
    name                 : string
    vocationLevel        : Neg1To4Stat.Model
    governingAttributes  : GoverningAttribute list
    baseDice             : DicePoolCalculation
    skillRowList         : VocationalSkillRowList.Model
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | SetGoverningAttributes of GoverningAttribute list
    | ToggleGoverningAttribute of int
    | SetName of string
    | SkillRowListMsg of VocationalSkillRowList.Msg
    | Reset

let init() : Model = {
    name = "Fellkin"
    vocationLevel = Neg1To4Stat.init()
    governingAttributes = [
        { isGoverning=false;attributeStat= AttributeRow.init() }
        { isGoverning=false;attributeStat= {AttributeRow.init() with name = "RFX" } }
    ]
    baseDice = baseDicePoolCalculation
    skillRowList = VocationalSkillRowList.init()
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

    | SetName name ->
        { model with name = name }

    | SkillRowListMsg skillRowListMsg ->
        { model with skillRowList = VocationalSkillRowList.update skillRowListMsg model.skillRowList }
        
    | Reset -> init()

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
        VocationalSkillRowList.view model.skillRowList (SkillRowListMsg >> dispatch)
    ]