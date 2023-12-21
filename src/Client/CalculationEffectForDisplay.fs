module MovementSpeedEffectForDisplay

open FallenLib.CoreSkillGroup
open FallenLib.MovementSpeedEffectForDisplay

type Msg = RecalculateMovementSpeed of (CoreSkillGroup list * float)

let update (msg: Msg) (model: MovementSpeedEffectForDisplay) =
    match msg, model with
    | RecalculateMovementSpeed (coreSkillGroupList, percentOfMovementSpeed), model ->
        determineMovementSpeedEffectForDisplay coreSkillGroupList percentOfMovementSpeed model.movementSpeedCalculation

let view (model: MovementSpeedEffectForDisplay) =
    NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay model)