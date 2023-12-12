module CarryWeightEffectForDisplay

open FallenLib.CarryWeightEffect
open FallenLib.CoreSkillGroup
open FallenLib.AttributeDeterminedDiceModEffect

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (model: CarryWeightEffectForDisplay)
    : CarryWeightEffectForDisplay =
    determineCarryWeightCalculationForDisplay
        coreSkillGroupList
        inventoryWeight
        weightClassList
        model.carryWeightCalculation

open Feliz

let carryWeightEffectForDisplay (model: CarryWeightEffectForDisplay) =
    [ Html.td [
          prop.text model.carryWeightCalculation.name
      ]
      Html.td [
          attributeDeterminedDiceModEffectToEffectString model.weightClass.attributeDeterminedDiceModEffect
          |> prop.text
      ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource