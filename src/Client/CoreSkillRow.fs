module CoreSkillRow

open FallenLib.SkillUtils

type Model = {
    name : string
    level : Neg1To4Stat.Model
    governingAttributeLevel : Neg1To4Stat.Model
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | SetGoveringAttribute of Neg1To4Stat.Model

let init (newGoverningAttributeLevel:Neg1To4Stat.Model) = {
    name = ""
    level = Neg1To4Stat.init()
    governingAttributeLevel = newGoverningAttributeLevel
}

let update (msg:Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4StatMsg ->
        { model with level = Neg1To4Stat.update neg1To4StatMsg model.level}
    | SetGoveringAttribute newGoverningAttributeLevel ->
        { model with governingAttributeLevel = newGoverningAttributeLevel}

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg-> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            coreSkillToDicePoolStringLoadedBaseDice model.level model.governingAttributeLevel 
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view
                model.level
                (Neg1To4StatMsg >> dispatch)
        ]
    ]