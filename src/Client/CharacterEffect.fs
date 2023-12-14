module CharacterEffect

open FallenLib.CarryWeightEffect
open FallenLib.CarryWeightEffectForDisplay
open FallenLib.CoreSkillGroup
open FallenLib.CharacterEffect
open FallenLib.SkillDiceModEffectForDisplay
open FallenLib.AttributeDeterminedDiceModEffectForDisplay
open FallenLib.MovementSpeedEffect

type Msg =
    | EffectForDisplayMsg of InteractiveEffectForDisplay.Msg
    | SkillDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg
    | AttributeDeterminedDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg
    | RecalculateCarryWeight of (CoreSkillGroup list * float * WeightClass list)
    | RecalculateMovementSpeed of (CoreSkillGroup list * float)

let update (msg: Msg) (model: CharacterEffect) : CharacterEffect =

    match msg, model with
    | EffectForDisplayMsg msg, EffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.update msg effectForDisplay
        |> EffectForDisplay

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

let viewTableData (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.interactiveEffectForDisplayTableData
            effectForDisplay
            (EffectForDisplayMsg >> dispatch)
    | SkillDiceModEffectForDisplay sdmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (skillDiceModEffectForDisplayToTextEffectForDisplay sdmefd)
            (SkillDiceModEffectForDisplayMsg >> dispatch)
    | AttributeDeterminedDiceModEffectForDisplay addmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (attributeDeterminedDiceModEffectToTextEffectForDisplay addmefd)
            (AttributeDeterminedDiceModEffectForDisplayMsg
             >> dispatch)
    | CarryWeightEffectForDisplay ccwefd ->
        NonInteractiveEffectForDisplay.view (carryWeightEffectForDisplayToEffectForDisplay ccwefd)
    | MovementSpeedEffectForDisplay msefd ->
        NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay msefd)