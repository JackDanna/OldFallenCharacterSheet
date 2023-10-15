module AttributeTable

open Elmish
open FallenLib.Attribute
open Feliz
open Feliz.Bulma


type Model = {
    attributeStat : AttributeStat
    skillRows     : SkillRow.Model list
}

type Msg =
    | SkillRowMsgs of SkillRow.Msg
    | Reset

let init() : Model =
    {
        attributeStat = emptyAttributeStat
        skillRows = []
    }


// let update (msg: Msg) (model: Model) : Model =
//     match msg with
//     | SkillRowMsgs skillRowMsg ->
//         { model with }
//     | Reset -> init()

let attributeTable (model: Model) (dispatch: Msg -> unit) (governingAttribute:string) (governingAttributes:int) (skillNames:list<string>) (skillLevels:list<int>) =
    Bulma.column [
        Bulma.notification [
            color.isPrimary
            prop.children [
                model.skillRows
                |> List.map ( 
                    fun skillRow -> 
                        Html.li [ 
                            prop.children [ 
                                SkillRow.view skillRow (SkillRowMsg >> dispatch)
                            ] 
                        ]
                )
                |> Html.ul
            ]
        ]
    ]