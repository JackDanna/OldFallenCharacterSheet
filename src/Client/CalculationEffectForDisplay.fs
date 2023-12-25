module MovementSpeedEffectForDisplay

open FallenLib.CoreSkill
open FallenLib.Attribute
open FallenLib.MovementSpeedEffect
open FallenLib.MovementSpeedEffectForDisplay
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg = RecalculateMovementSpeed of CoreSkillAndAttributeData

let update (msg: Msg) (model: MovementSpeedEffectForDisplay) =
    match msg, model with
    | RecalculateMovementSpeed coreSkillAndAttributeData, model ->
        determineMovementSpeedEffectForDisplay coreSkillAndAttributeData model.movementSpeedCalculation

let view (model: MovementSpeedEffectForDisplay) =
    NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay model)