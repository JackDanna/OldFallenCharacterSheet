module CalculationEffectForDisplay

open FallenLib.CarryWeightEffect
open FallenLib.CarryWeightEffectForDisplay
open FallenLib.CoreSkillGroup
open FallenLib.EffectForDisplay
open FallenLib.MovementSpeedEffectForDisplay

type Msg =
    | RecalculateCarryWeight of (CoreSkillGroup list * float * WeightClass list)
    | RecalculateMovementSpeed of (CoreSkillGroup list * float)

let update (msg: Msg) (model: CalculationEffectForDisplay) =
    match msg, model with
    | RecalculateCarryWeight (coreSkillGroupList, inventoryWeight, weightClassList), CarryWeightEffectForDisplay cwefd ->
        determineCarryWeightCalculationForDisplay
            coreSkillGroupList
            inventoryWeight
            weightClassList
            cwefd.carryWeightCalculation
        |> CarryWeightEffectForDisplay
    | RecalculateMovementSpeed (coreSkillGroupList, percentOfMovementSpeed), MovementSpeedEffectForDisplay msefd ->
        determineMovementSpeedEffectForDisplay coreSkillGroupList percentOfMovementSpeed msefd.movementSpeedCalculation
        |> MovementSpeedEffectForDisplay
    | _ -> model

open Feliz

let view (model: CalculationEffectForDisplay) =
    match model with
    | CarryWeightEffectForDisplay ccwefd ->
        NonInteractiveEffectForDisplay.view (carryWeightEffectForDisplayToEffectForDisplay ccwefd)
    | MovementSpeedEffectForDisplay msefd ->
        NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay msefd)