module CharacterEffectList

open FallenLib.CharacterEffect

type Msg =
    | ModifyCharacterEffect of int * CharacterEffect.Msg
    | Insert of string
    | Remove of int

let init () : CharacterEffect list = []

let update (characterEffectMap: Map<string, CharacterEffect>) (msg: Msg) (model: CharacterEffect list) =
    match msg with
    | ModifyCharacterEffect (position, msg) ->
        model
        |> List.mapi (fun index characterEffect ->
            if position = index then
                CharacterEffect.update msg characterEffect
            else
                characterEffect)
    | Insert characterEffectName -> characterEffectMap.Values |> List.ofSeq

    // (characterEffectMap.Item characterEffectName)
    // |> List.singleton
    // |> List.append model
    | Remove position -> List.removeAt position model

open Feliz
open Feliz.Bulma

let view (characterEffectNameList: string list) (model: CharacterEffect list) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.label "Character Effects:"
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    List.map
                        (fun (thString: string) -> Html.th thString)
                        [ "Name"
                          "Effect"
                          "Duration"
                          "Source" ]
                    |> Html.tr
                ]
                Html.tableBody (
                    List.mapi
                        (fun position equipmentRow ->
                            let characterEffect =
                                (CharacterEffect.characterEffectTableData equipmentRow (fun msg ->
                                    dispatch (ModifyCharacterEffect(position, msg))))

                            let deleteEquipmentRowButton =
                                Html.td [
                                    Html.button [
                                        prop.onClick (fun _ -> dispatch (Remove(position)))
                                        prop.text "-"
                                    ]
                                ]
                                |> List.singleton

                            Html.tr (List.append characterEffect deleteEquipmentRowButton))
                        model
                )
                Html.tfoot [
                    Html.div [
                        Bulma.input.text [
                            prop.list "re"
                            prop.onTextChange (fun input -> dispatch (Insert input))
                        ]
                        Html.datalist [
                            prop.id "re"
                            prop.children (
                                List.map
                                    (fun (characterEffect: string) ->
                                        Html.option [
                                            prop.value characterEffect
                                        ])
                                    characterEffectNameList
                            )
                        ]
                    ]
                ]
            ]
        ]
    ]