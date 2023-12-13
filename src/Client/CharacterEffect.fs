module CharacterEffect

open FallenLib.CarryWeightEffect
open FallenLib.CoreSkillGroup

open FallenLib.CharacterEffect

type Msg =
    | EffectForDisplayMsg of EffectForDisplay.Msg
    | SkillDiceModEffectForDisplayMsg of SkillDiceModEffectForDisplay.Msg
    | AttributeDeterminedDiceModEffectForDisplayMsg of AttributeDeterminedDiceModEffectForDisplay.Msg

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (msg: Msg)
    (model: CharacterEffect)
    : CharacterEffect =
    match msg, model with
    | EffectForDisplayMsg msg, EffectForDisplay effectForDisplay ->
        EffectForDisplay.update msg effectForDisplay
        |> EffectForDisplay
    | SkillDiceModEffectForDisplayMsg msg, SkillDiceModEffectForDisplay (sdmew, das) ->
        SkillDiceModEffectForDisplay.update msg (sdmew, das)
        |> SkillDiceModEffectForDisplay
    | _, CarryWeightEffectForDisplay ccwefd ->
        CarryWeightEffectForDisplay.update coreSkillGroupList inventoryWeight weightClassList ccwefd
        |> CarryWeightEffectForDisplay
    | _ -> model

open Feliz

let characterEffectTableData (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay ->
        EffectForDisplay.effectForDisplayTableData effectForDisplay (EffectForDisplayMsg >> dispatch)
    | SkillDiceModEffectForDisplay sdmefd ->
        SkillDiceModEffectForDisplay.skillDiceModForDisplayTableData
            sdmefd
            (SkillDiceModEffectForDisplayMsg >> dispatch)
    | CarryWeightEffectForDisplay ccwefd -> CarryWeightEffectForDisplay.carryWeightEffectForDisplay ccwefd
    | AttributeDeterminedDiceModEffectForDisplay addmefd ->
        AttributeDeterminedDiceModEffectForDisplay.attributeDeterminedDiceModEffectForDisplay
            addmefd
            (AttributeDeterminedDiceModEffectForDisplayMsg
             >> dispatch)
    | MovementSpeedEffectForDisplay msefd -> MovementSpeedEffectForDisplay.movementSpeedEffectForDisplay msefd



let view (model: CharacterEffect) (dispatch: Msg -> unit) =
    characterEffectTableData model dispatch |> Html.tr