module CarryWeightEffectForDisplay

open FallenLib.CarryWeightEffect
open FallenLib.CoreSkillGroup
open FallenLib.Attribute

type Msg = | Recalculate

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (msg: Msg)
    (model: CalculatedCarryWeightEffectForDisplay)
    : CalculatedCarryWeightEffectForDisplay =
    match msg with
    | Recalculate ->
        determineCarryWeightCalculationForDisplay
            coreSkillGroupList
            inventoryWeight
            weightClassList
            model.carryWeightCalculation

open Feliz

let carryWeightEffectForDisplay (model: CalculatedCarryWeightEffectForDisplay) =
    [ Html.td [
          prop.text model.carryWeightCalculation.name
      ]
      Html.td [
          model.attributeDeterminedDiceModEffect
          |> attributeDeterminedDiceModEffectToEffectString
          |> prop.text
      ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource