module CharacterEffect

open FallenLib.EffectForDisplay
open FallenLib.SkillDiceModificationEffect

open FallenLib.CharacterEffect

let getCharacterEffectName characterEffect =
    match characterEffect with
    | EffectForDisplay effectForDisplay -> effectForDisplay.name
    | SkillDiceModificationEffectForDisplay (sdmew, _) -> sdmew.name

type Msg =
    | EffectForDisplayMsg of EffectForDisplay.Msg
    | SkillDiceModificationEffectForDisplayMsg of SkillDiceModificationEffectForDisplay.Msg

let update (msg: Msg) (model: CharacterEffect) : CharacterEffect =
    match msg, model with
    | EffectForDisplayMsg msg, EffectForDisplay effectForDisplay ->
        EffectForDisplay.update msg effectForDisplay
        |> EffectForDisplay
    | SkillDiceModificationEffectForDisplayMsg msg, SkillDiceModificationEffectForDisplay (sdmew, das) ->
        SkillDiceModificationEffectForDisplay.update msg (sdmew, das)
        |> SkillDiceModificationEffectForDisplay
    | _ -> model


open Feliz
open Feliz.Bulma

let characterEffectTableData (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay ->
        EffectForDisplay.effectForDisplayTableData effectForDisplay (EffectForDisplayMsg >> dispatch)
    | SkillDiceModificationEffectForDisplay sdmefd ->
        SkillDiceModificationEffectForDisplay.skillDiceModificationForDisplayTableData
            sdmefd
            (SkillDiceModificationEffectForDisplayMsg
             >> dispatch)

let view (model: CharacterEffect) (dispatch: Msg -> unit) =

    characterEffectTableData model dispatch |> Html.tr