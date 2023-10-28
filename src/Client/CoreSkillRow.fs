module CoreSkillRow

open FallenLib.SkillUtils

type Model = {
    name : string
    level : Neg1To4Stat.Model
    governingAttribute : AttributeRow.Model
}

type Msg =
    | Neg1To4StatMsg of Neg1To4Stat.Msg
    | SetGoveringAttribute of AttributeRow.Model

let init (governingAttribute:AttributeRow.Model) = {
    name = ""
    level = Neg1To4Stat.init()
    governingAttribute = governingAttribute
}

let update (msg:Msg) (model: Model) : Model =
    match msg with
    | Neg1To4StatMsg neg1To4StatMsg ->
        { model with level = Neg1To4Stat.update neg1To4StatMsg model.level}
    | SetGoveringAttribute newGoverningAttribute ->
        { model with governingAttribute = newGoverningAttribute}

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg-> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            coreSkillToDicePoolStringLoadedBaseDice model.level model.governingAttribute.neg1To4Stat 
            |> prop.text
        ]
        Bulma.column [
            Neg1To4Stat.view
                model.level
                (Neg1To4StatMsg >> dispatch)
        ]
    ]