module SkillDiceModEffectForDisplay

open FallenLib.SkillDiceModEffect
open FallenLib.SkillDiceModEffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

open Feliz

let update (msg: Msg) (model: SkillDiceModEffectForDisplay) : SkillDiceModEffectForDisplay =
    match msg with
    | DurationAndSourceMsg msg ->
        { model with durationAndSource = (DurationAndSource.update msg model.durationAndSource) }

let skillDiceModForDisplayTableData (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =

    [ Html.td [
          prop.text model.skillDiceModEffect.name
      ]
      Html.td [
          skillDiceModEffectToEffectString model.skillDiceModEffect
          |> prop.text
      ] ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)

let view (model: SkillDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    skillDiceModForDisplayTableData model dispatch
    |> Html.tr