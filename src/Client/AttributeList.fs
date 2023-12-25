module AttributeList

open FallenLib.Attribute

type Msg = ModifyAttribute of int * Attribute.Msg

let init () : Attribute list = []

let update (msg: Msg) (model: Attribute list) =
    match msg with
    | ModifyAttribute (position, msg) ->
        List.mapi
            (fun index attribute ->
                if index = position then
                    Attribute.update msg attribute
                else
                    attribute)
            model

open Feliz
open Feliz.Bulma

let view model dispatch (incompleteAttributeListView: AttributeName -> ReactElement) =
    Bulma.container [
        Bulma.label "Attributes and Core Skills:"
        |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> List.mapi (fun position attribute ->
                    Bulma.column [
                        Bulma.box [
                            Attribute.view attribute (fun msg -> dispatch (ModifyAttribute(position, msg)))

                            incompleteAttributeListView attribute.name
                        ]
                    ])
                |> Bulma.columns
            ]
        ]
    ]