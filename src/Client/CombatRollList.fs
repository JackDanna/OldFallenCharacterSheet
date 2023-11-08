module CombatRollTable

open FallenLib.CombatRoll
open FallenLib.Dice
open FallenLib.Range
open FallenLib.Damage
open FallenLib.Shape
open FallenLib.Equipment
open FallenLib.Attribute
open FallenLib.VocationGroup
open FallenLib.MagicSkill
open FallenLib.MagicCombat

type Msg = RecalculateCombatRolls

let update
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (combatRollGoverningAttributeList: Attribute list)
    (attributeDeterminedDiceModList: AttributeDeterminedDiceMod list)
    (equipmentList: Equipment list)
    (attributeStatList: AttributeStat list)
    (vocationGroupList: VocationGroup list)
    (msg: Msg)
    (model: CombatRoll list)
    : CombatRoll list =

    match msg with
    | RecalculateCombatRolls ->
        createCombatRolls
            magicSkillMap
            magicCombatMap
            rangeMap
            combatRollGoverningAttributeList
            attributeDeterminedDiceModList
            equipmentList
            attributeStatList
            vocationGroupList

open Feliz
open Feliz.Bulma

let CombatRollRow (combatRoll: CombatRoll) =
    Html.tr [
        Html.td combatRoll.name
        Html.td (dicePoolToString combatRoll.dicePool)
        Html.td (int combatRoll.penetration)
        Html.td (calculatedRangeToString combatRoll.calculatedRange)
        Html.td (damageTypesToString combatRoll.damageTypes)
        Html.td (int combatRoll.engageableOpponents)
        Html.td (shapeOptionToString combatRoll.areaOfEffectShape)
    ]

let view (model: CombatRoll list) =
    Bulma.table [
        table.isBordered
        prop.children [
            Html.thead [
                List.map
                    (fun (thString: string) -> Html.th thString)
                    [ "Name"
                      "Dice Poll"
                      "Penetration"
                      "Effective/MaxRange"
                      "Damage Type"
                      "EO"
                      "AOE" ]
                |> Html.tr
            ]
            Html.tableBody (List.map (fun combatRoll -> CombatRollRow combatRoll) model)
        ]
    ]