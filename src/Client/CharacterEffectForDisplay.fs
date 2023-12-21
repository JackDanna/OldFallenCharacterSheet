module CharacterEffectForDisplay

open FallenLib.EffectForDisplay
open FallenLib.SkillDiceModEffectForDisplay
open FallenLib.AttributeDeterminedDiceModEffectForDisplay

type Msg =
    | TextForDisplayMsg of InteractiveEffectForDisplay.Msg
    | SkillDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg
    | AttributeDeterminedDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg
    | CalculationEffectForDisplayMsg of MovementSpeedEffectForDisplay.Msg

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =

    match msg, model with
    | TextForDisplayMsg msg, TextEffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.update msg effectForDisplay
        |> TextEffectForDisplay

    | SkillDiceModEffectForDisplayMsg msg, SkillDiceModEffectForDisplay sdmefd ->
        let newEffectForDisplay =
            PartiallyInteractiveEffectForDisplay.update msg (skillDiceModEffectForDisplayToTextEffectForDisplay sdmefd)

        { sdmefd with durationAndSource = newEffectForDisplay.durationAndSource }
        |> SkillDiceModEffectForDisplay

    | AttributeDeterminedDiceModEffectForDisplayMsg msg, AttributeDeterminedDiceModEffectForDisplay addme ->
        let newEffectForDisplay =
            PartiallyInteractiveEffectForDisplay.update
                msg
                (attributeDeterminedDiceModEffectToTextEffectForDisplay addme)

        { addme with durationAndSource = newEffectForDisplay.durationAndSource }
        |> AttributeDeterminedDiceModEffectForDisplay

    | CalculationEffectForDisplayMsg rmsg, CalculationEffectForDisplay cefd ->
        MovementSpeedEffectForDisplay.update rmsg cefd
        |> CalculationEffectForDisplay

    | _ -> model

let view (model: EffectForDisplay) (dispatch: Msg -> unit) =

    match model with
    | TextEffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.interactiveEffectForDisplayTableData
            effectForDisplay
            (TextForDisplayMsg >> dispatch)
    | SkillDiceModEffectForDisplay sdmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (skillDiceModEffectForDisplayToTextEffectForDisplay sdmefd)
            (SkillDiceModEffectForDisplayMsg >> dispatch)
    | AttributeDeterminedDiceModEffectForDisplay addmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (attributeDeterminedDiceModEffectToTextEffectForDisplay addmefd)
            (AttributeDeterminedDiceModEffectForDisplayMsg
             >> dispatch)
    | CalculationEffectForDisplay cefd -> MovementSpeedEffectForDisplay.view cefd