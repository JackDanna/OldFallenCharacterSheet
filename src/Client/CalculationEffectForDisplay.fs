module CalculationEffectForDisplay

open FallenLib.WeightClass
open FallenLib.CoreSkillGroup
open FallenLib.MovementSpeedEffectForDisplay

type Msg =
    | RecalculateCarryWeight of (CoreSkillGroup list * float * WeightClass list)
    | RecalculateMovementSpeed of (CoreSkillGroup list * float)

let update (msg: Msg) (model: MovementSpeedEffectForDisplay) =
    match msg, model with
    | RecalculateMovementSpeed (coreSkillGroupList, percentOfMovementSpeed), model ->
        determineMovementSpeedEffectForDisplay coreSkillGroupList percentOfMovementSpeed model.movementSpeedCalculation
    | _ -> model

let view (model: MovementSpeedEffectForDisplay) =
    NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay model)