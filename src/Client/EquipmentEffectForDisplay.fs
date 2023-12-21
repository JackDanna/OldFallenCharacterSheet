module EquipmentEffectForDisplay

open FallenLib.EffectForDisplay

type Msg = CalculationEffectForDisplayMsg of CalculationEffectForDisplay.Msg

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =
    match msg, model with
    | CalculationEffectForDisplayMsg rmsg, MovementSpeedEffectForDisplay msefd ->
        CalculationEffectForDisplay.update rmsg msefd
        |> CalculationEffectForDisplay
    | _ -> model

let view (model: EffectForDisplay) =
    NonInteractiveEffectForDisplay.view (effectForDisplayToTextEffectForDisplay model)