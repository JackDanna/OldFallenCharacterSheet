module SkillDiceModEffectForDisplay

open FallenLib.SkillDiceModEffect
open FallenLib.EffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

open Feliz

let update (msg: Msg) (model: SkillDiceModEffectForDisplay) : SkillDiceModEffectForDisplay =
    match msg with
    | DurationAndSourceMsg msg ->
        let (skillDiceModEffect, durationAndSource) = model
        skillDiceModEffect, (DurationAndSource.update msg durationAndSource)

let skillDiceModForDisplayTableData (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    let (skillDiceModEffect, durationAndSource) = model

    [ Html.td [
          prop.text skillDiceModEffect.name
      ]
      Html.td [
          skillDiceModEffectToEffectString skillDiceModEffect
          |> prop.text
      ] ]
    @ DurationAndSource.interactiveView durationAndSource (DurationAndSourceMsg >> dispatch)

let view (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    skillDiceModForDisplayTableData model dispatch
    |> Html.tr