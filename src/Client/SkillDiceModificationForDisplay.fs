module SkillDiceModificationEffectForDisplay

open FallenLib.SkillDiceModificationEffect
open FallenLib.EffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

open Feliz
open Feliz.Bulma

type SkillDiceModificationEffectForDisplay = SkillDiceModificationEffect * DurationAndSource

let update (msg: Msg) (model: SkillDiceModificationEffectForDisplay) : SkillDiceModificationEffect * DurationAndSource =
    match msg with
    | DurationAndSourceMsg msg ->
        let (skillDiceModificationEffect, durationAndSource) = model
        skillDiceModificationEffect, (DurationAndSource.update msg durationAndSource)

let skillDiceModificationForDisplayTableData (model: SkillDiceModificationEffectForDisplay) (dispatch: Msg -> unit) =
    let (skillDiceModificationEffect, durationAndSource) = model

    [ Html.td [
          prop.text skillDiceModificationEffect.name
      ]
      Html.td [
          skillDiceModificationEffectToEffectString skillDiceModificationEffect
          |> prop.text
      ] ]
    @ DurationAndSource.interactiveView durationAndSource (DurationAndSourceMsg >> dispatch)

let view (model: SkillDiceModificationEffectForDisplay) (dispatch: Msg -> unit) =
    skillDiceModificationForDisplayTableData model dispatch
    |> Html.tr