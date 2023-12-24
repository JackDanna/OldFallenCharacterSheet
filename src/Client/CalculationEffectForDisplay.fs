module MovementSpeedEffectForDisplay

open FallenLib.CoreSkill
open FallenLib.Attribute
open FallenLib.MovementSpeedEffectForDisplay

type Msg = RecalculateMovementSpeed of (Attribute list * CoreSkill list * float)

let update (msg: Msg) (model: MovementSpeedEffectForDisplay) =
    match msg, model with
    | RecalculateMovementSpeed (attributeList, coreSkillList, percentOfMovementSpeed), model ->
        determineMovementSpeedEffectForDisplay
            attributeList
            coreSkillList
            percentOfMovementSpeed
            model.movementSpeedCalculation

let view (model: MovementSpeedEffectForDisplay) =
    NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay model)