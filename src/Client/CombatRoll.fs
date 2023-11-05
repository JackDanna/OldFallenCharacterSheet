module CombatRoll

open FallenLib.CombatRoll
open FallenLib.Dice
open FallenLib.Range
open FallenLib.Damage
open FallenLib.Shape

type Model = CombatRoll

open Feliz

let view (model: Model) =
    Html.tr [
        Html.td model.name
        Html.td (dicePoolToString model.dicePool)
        Html.td (int model.penetration)
        Html.td (calculatedRangeToString model.calculatedRange)
        Html.td (damageTypesToString model.damageTypes)
        Html.td (int model.engageableOpponents)
        Html.td (shapeOptionToString model.areaOfEffectShape)
    ]