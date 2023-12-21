module EquipmentEffectForDisplayList

open FallenLib.EffectForDisplay
open FallenLib.WeightClass
open FallenLib.CoreSkillGroup

type Msg = | RecalculateMovementSpeed

let init () : EffectForDisplay list = []

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (percentOfMovementSpeed: float)
    (msg: Msg)
    (model: EffectForDisplay list)
    : EffectForDisplay list =
    match msg with
    | RecalculateMovementSpeed ->
        model
        |> List.map (fun effectForDisplay ->
            EquipmentEffectForDisplay.update
                (EquipmentEffectForDisplay.Msg.CalculationEffectForDisplayMsg(
                    MovementSpeedEffectForDisplay.Msg.RecalculateMovementSpeed(
                        coreSkillGroupList,
                        percentOfMovementSpeed
                    )
                ))
                effectForDisplay)


open Feliz
open Feliz.Bulma

let view (model: EffectForDisplay list) =
    Bulma.container [
        Bulma.label "Equipment Effects:"
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
                    model
                    |> List.map EquipmentEffectForDisplay.view
                    |> List.map Html.tr
                )
            ]
        ]
    ]