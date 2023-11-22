module CharacterEffect

open FallenLib.EffectForDisplay
open FallenLib.SkillDiceModificationEffect
open FallenLib.CarryWeightEffect
open FallenLib.CoreSkillGroup

open FallenLib.CharacterEffect

type Msg =
    | EffectForDisplayMsg of EffectForDisplay.Msg
    | SkillDiceModificationEffectForDisplayMsg of SkillDiceModificationEffectForDisplay.Msg
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
    | SkillDiceModificationEffectForDisplayMsg msg, SkillDiceModificationEffectForDisplay (sdmew, das) ->
        SkillDiceModificationEffectForDisplay.update msg (sdmew, das)
        |> SkillDiceModificationEffectForDisplay
    | _, CarryWeightEffectForDisplay ccwefd ->
        CarryWeightEffectForDisplay.update
            (coreSkillGroupList: CoreSkillGroup list)
            (inventoryWeight: float)
            (weightClassList: WeightClass list)
            ccwefd
        |> CarryWeightEffectForDisplay
    | _ -> model

open Feliz

let characterEffectTableData (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay ->
        EffectForDisplay.effectForDisplayTableData effectForDisplay (EffectForDisplayMsg >> dispatch)
    | SkillDiceModificationEffectForDisplay sdmefd ->
        SkillDiceModificationEffectForDisplay.skillDiceModificationForDisplayTableData
            sdmefd
            (SkillDiceModificationEffectForDisplayMsg
             >> dispatch)
    | CarryWeightEffectForDisplay ccwefd -> CarryWeightEffectForDisplay.carryWeightEffectForDisplay ccwefd
    | AttributeDeterminedDiceModEffectForDisplay addmefd ->
        AttributeDeterminedDiceModEffectForDisplay.attributeDeterminedDiceModEffectForDisplay
            addmefd
            (AttributeDeterminedDiceModEffectForDisplayMsg
             >> dispatch)
    | MovementSpeedEffectForDisplay msefd -> MovementSpeedEffectForDisplay.movementSpeedEffectForDisplay msefd



let view (model: CharacterEffect) (dispatch: Msg -> unit) =
    characterEffectTableData model dispatch |> Html.tr