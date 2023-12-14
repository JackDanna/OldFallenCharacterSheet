module PartiallyInteractiveEffectForDisplay

open FallenLib.TextEffectForDisplay

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

let update (msg: Msg) (model: TextEffectForDisplay) : TextEffectForDisplay =
    match msg with
    | DurationAndSourceMsg msg ->
        { model with durationAndSource = DurationAndSource.update msg model.durationAndSource }

open Feliz

let view (model: TextEffectForDisplay) (dispatch: Msg -> unit) =
    [ Html.td [ prop.text model.name ]
      Html.td [ prop.text model.effect ] ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)