module EquipmentEffectForDisplayList

open FallenLib.EffectForDisplay
open FallenLib.CarryWeightCalculation
open FallenLib.CoreSkillGroup

type Msg = | RecalculateCarryWeightAndMovementSpeed

let init () : EffectForDisplay list = []

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (msg: Msg)
    (model: EffectForDisplay list)
    : EffectForDisplay list =
    match msg with
    | RecalculateCarryWeightAndMovementSpeed ->
        model
        |> List.map (fun effectForDisplay ->
            EquipmentEffectForDisplay.update
                (EquipmentEffectForDisplay.Msg.CalculationEffectForDisplayMsg(
                    CalculationEffectForDisplay.Msg.RecalculateCarryWeight(
                        coreSkillGroupList,
                        inventoryWeight,
                        weightClassList
                    )
                ))
                effectForDisplay)
        |> List.map (fun effectForDisplay ->
            EquipmentEffectForDisplay.update
                (EquipmentEffectForDisplay.Msg.CalculationEffectForDisplayMsg(
                    CalculationEffectForDisplay.Msg.RecalculateMovementSpeed(
                        coreSkillGroupList,
                        (findPercentageOfMovementSpeed model)
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