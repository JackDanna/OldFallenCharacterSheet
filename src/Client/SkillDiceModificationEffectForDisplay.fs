module SkillDiceModificationEffectForDisplay

open FallenLib.SkillDiceModEffect
open FallenLib.EffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

open Feliz

let update (msg: Msg) (model: SkillDiceModEffectForDisplay) : SkillDiceModEffectForDisplay =
    match msg with
    | DurationAndSourceMsg msg ->
        let (skillDiceModificationEffect, durationAndSource) = model
        skillDiceModificationEffect, (DurationAndSource.update msg durationAndSource)

let skillDiceModificationForDisplayTableData (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    let (skillDiceModificationEffect, durationAndSource) = model

    [ Html.td [
          prop.text skillDiceModificationEffect.name
      ]
      Html.td [
          skillDiceModEffectToEffectString skillDiceModificationEffect
          |> prop.text
      ] ]
    @ DurationAndSource.interactiveView durationAndSource (DurationAndSourceMsg >> dispatch)

let view (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    skillDiceModificationForDisplayTableData model dispatch
    |> Html.tr