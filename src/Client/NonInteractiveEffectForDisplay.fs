module NonInteractiveEffectForDisplay

open FallenLib.EffectForDisplay

open Feliz

let view (model: EffectForDisplay) =
    [ Html.td [ prop.text model.name ]
      Html.td [ prop.text model.effect ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource