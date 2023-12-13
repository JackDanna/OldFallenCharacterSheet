module CharacterEffect

open FallenLib.CarryWeightEffect
open FallenLib.CoreSkillGroup

open FallenLib.CharacterEffect

open FallenLib.SkillDiceModEffectForDisplay
open FallenLib.AttributeDeterminedDiceModEffectForDisplay

open FallenLib.MovementSpeedCalculation

type Msg =
    | EffectForDisplayMsg of InteractiveEffectForDisplay.Msg
    | SkillDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg
    | AttributeDeterminedDiceModEffectForDisplayMsg of PartiallyInteractiveEffectForDisplay.Msg

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (msg: Msg)
    (model: CharacterEffect)
    : CharacterEffect =

    match msg, model with
    | EffectForDisplayMsg msg, EffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.update msg effectForDisplay
        |> EffectForDisplay

    | SkillDiceModEffectForDisplayMsg msg, SkillDiceModEffectForDisplay sdmefd ->
        let newEffectForDisplay =
            PartiallyInteractiveEffectForDisplay.update msg (skillDiceModEffectToEffectForDisplay sdmefd)

        { sdmefd with durationAndSource = newEffectForDisplay.durationAndSource }
        |> SkillDiceModEffectForDisplay

    | AttributeDeterminedDiceModEffectForDisplayMsg msg, AttributeDeterminedDiceModEffectForDisplay addme ->
        let newEffectForDisplay =
            PartiallyInteractiveEffectForDisplay.update msg (attributeDeterminedDiceModEffectToEffectForDisplay addme)

        { addme with durationAndSource = newEffectForDisplay.durationAndSource }
        |> AttributeDeterminedDiceModEffectForDisplay

    | _, CarryWeightEffectForDisplay ccwefd ->

        determineCarryWeightCalculationForDisplay
            coreSkillGroupList
            inventoryWeight
            weightClassList
            ccwefd.carryWeightCalculation
        |> CarryWeightEffectForDisplay
    // | _, MovementSpeedEffectForDisplay msefd ->
    //     determineMovementSpeedEffectForDisplay coreSkillGroupList percentOfMovementSpeed movementSpeedCalculation
    //     |> MovementSpeedEffectForDisplay
    | _ -> model

open Feliz

let characterEffectTableData (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay ->
        InteractiveEffectForDisplay.interactiveEffectForDisplayTableData
            effectForDisplay
            (EffectForDisplayMsg >> dispatch)
    | SkillDiceModEffectForDisplay sdmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (skillDiceModEffectToEffectForDisplay sdmefd)
            (SkillDiceModEffectForDisplayMsg >> dispatch)
    | AttributeDeterminedDiceModEffectForDisplay addmefd ->
        PartiallyInteractiveEffectForDisplay.view
            (attributeDeterminedDiceModEffectToEffectForDisplay addmefd)
            (AttributeDeterminedDiceModEffectForDisplayMsg
             >> dispatch)
    | CarryWeightEffectForDisplay ccwefd ->
        NonInteractiveEffectForDisplay.view (carryWeightEffectForDisplayToEffectForDisplay ccwefd)
    | MovementSpeedEffectForDisplay msefd ->
        NonInteractiveEffectForDisplay.view (movementSpeedEffectForDisplayToEffectForDisplay msefd)

let view (model: CharacterEffect) (dispatch: Msg -> unit) =
    characterEffectTableData model dispatch |> Html.tr