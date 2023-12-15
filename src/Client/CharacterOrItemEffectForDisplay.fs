module CharacterOrItemEffectForDisplay

open FallenLib.EffectForDisplay

type Msg =
    | CharacterEffectForDisplayMsg of CharacterEffectForDisplay.Msg
    | ItemEffectForDisplayMsg of ItemEffectForDisplay.Msg

let update (msg: Msg) (model: CharacterOrItemEffectForDisplay) : CharacterOrItemEffectForDisplay =
    match msg, model with
    | CharacterEffectForDisplayMsg msg, CharacterEffectForDisplay cefd ->
        CharacterEffectForDisplay.update msg cefd
        |> CharacterEffectForDisplay
    | ItemEffectForDisplayMsg msg, ItemEffectForDisplay iefd ->
        ItemEffectForDisplay.update msg iefd
        |> ItemEffectForDisplay
    | _ -> model

open Feliz

let viewTableData (model: CharacterOrItemEffectForDisplay) (dispatch: Msg -> unit) =

    match model with
    | CharacterEffectForDisplay cefd -> CharacterEffectForDisplay.view cefd (CharacterEffectForDisplayMsg >> dispatch)
    | ItemEffectForDisplay iefd -> ItemEffectForDisplay.view iefd