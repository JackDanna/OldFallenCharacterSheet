module NonInteractiveEffectForDisplay

open FallenLib.TextEffectForDisplay

open Feliz

let view (model: TextEffectForDisplay) =
    [ Html.td [ prop.text model.name ]
      Html.td [ prop.text model.effect ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource