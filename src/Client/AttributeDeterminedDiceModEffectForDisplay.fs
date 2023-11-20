module AttributeDeterminedDiceModEffectForDisplay

open FallenLib.Attribute
open FallenLib.EffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

open Feliz

let update (msg: Msg) (model: AttributeDeterminedDiceModEffectForDisplay) : AttributeDeterminedDiceModEffectForDisplay =
    match msg with
    | DurationAndSourceMsg msg -> { model with durationAndSource = model.durationAndSource }


let attributeDeterminedDiceModEffectForDisplay
    (model: AttributeDeterminedDiceModEffectForDisplay)
    (dispatch: Msg -> unit)
    =
    [ Html.td [
          prop.text model.attributeDeterminedDiceModEffect.name
      ]
      Html.td [
          attributeDeterminedDiceModEffectToEffectString model.attributeDeterminedDiceModEffect
          |> prop.text
      ] ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)

let view (model: AttributeDeterminedDiceModEffectForDisplay) (dispatch: Msg -> unit) =
    attributeDeterminedDiceModEffectForDisplay model dispatch
    |> Html.tr