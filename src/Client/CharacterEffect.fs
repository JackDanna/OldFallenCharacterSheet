module CharacterEffect

open FallenLib.EffectForDisplay
open FallenLib.SkillDiceModificationEffect
open SkillDiceModificationEffectForDisplay

type CharacterEffect =
    | EffectForDisplay of EffectForDisplay
    | SkillDiceModificationEffectForDisplay of SkillDiceModificationEffectForDisplay

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

let view (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay -> EffectForDisplay.view effectForDisplay (EffectForDisplayMsg >> dispatch)
    | SkillDiceModificationEffectForDisplay sdmefd ->
        SkillDiceModificationEffectForDisplay.view
            sdmefd
            (SkillDiceModificationEffectForDisplayMsg
             >> dispatch)