module MovementSpeedEffectForDisplay

open FallenLib.CoreSkillGroup
open FallenLib.Attribute
open FallenLib.MovementSpeedCalculation

let update
    (coreSkillGroupList: CoreSkillGroup list)
    (percentOfMovementSpeed: float)
    (movementSpeedCalculation: MovementSpeedCalculation)
    : MovementSpeedEffectForDisplay =

    determineMovementSpeedEffectForDisplay coreSkillGroupList percentOfMovementSpeed movementSpeedCalculation

open Feliz

let movementSpeedEffectForDisplay (model: MovementSpeedEffectForDisplay) =
    [ Html.td [
          prop.text model.movementSpeedCalculation.name
      ]
      Html.td [
          string model.movementSpeed |> prop.text
      ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource