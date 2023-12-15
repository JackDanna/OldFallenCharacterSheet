module ItemEffectForDisplay

open FallenLib.EffectForDisplay

type Msg = CalculationEffectForDisplayMsg of CalculationEffectForDisplay.Msg

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =
    match msg, model with
    | CalculationEffectForDisplayMsg rmsg, CalculationEffectForDisplay cefd ->
        CalculationEffectForDisplay.update rmsg cefd
        |> CalculationEffectForDisplay
    | _ -> model

let view (model: EffectForDisplay) =
    NonInteractiveEffectForDisplay.view (effectForDisplayToTextEffectForDisplay model)