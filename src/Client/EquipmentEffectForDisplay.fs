module EquipmentEffectForDisplay

open FallenLib.EffectForDisplay

type Msg = CalculationEffectForDisplayMsg of MovementSpeedEffectForDisplay.Msg

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =
    match msg, model with
    | CalculationEffectForDisplayMsg rmsg, MovementSpeedEffectForDisplay msefd ->
        MovementSpeedEffectForDisplay.update rmsg msefd
        |> MovementSpeedEffectForDisplay
    | _ -> model

let view (model: EffectForDisplay) =
    NonInteractiveEffectForDisplay.view (effectForDisplayToTextEffectForDisplay model)