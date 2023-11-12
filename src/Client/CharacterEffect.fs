module CharacterEffect

open FallenLib.EffectForDisplay

type CharacterEffect = EffectForDisplay of EffectForDisplay

let getCharacterEffectName characterEffect =
    match characterEffect with
    | EffectForDisplay effectForDisplay -> effectForDisplay.name

type Msg = EffectForDisplayMsg of EffectForDisplay.Msg

let update (msg: Msg) (model: CharacterEffect) : CharacterEffect =
    match msg, model with
    | EffectForDisplayMsg msg, EffectForDisplay effectForDisplay ->
        EffectForDisplay.update msg effectForDisplay
        |> EffectForDisplay


open Feliz
open Feliz.Bulma

let view (model: CharacterEffect) (dispatch: Msg -> unit) =

    match model with
    | EffectForDisplay effectForDisplay -> EffectForDisplay.view effectForDisplay (EffectForDisplayMsg >> dispatch)