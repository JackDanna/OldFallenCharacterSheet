module CombatRollTable

open FallenLib.CombatRoll
open FallenLib.Dice
open FallenLib.Range
open FallenLib.DamageType
open FallenLib.CalculatedAOE
open FallenLib.Equipment
open FallenLib.Attribute
open FallenLib.VocationGroup
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.AttributeDeterminedDiceModEffect

let update
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (combatRollGoverningAttributeList: Attribute list)
    (attributeDeterminedDiceModList: AttributeDeterminedDiceModEffect list)
    (equipmentList: Equipment list)
    (attributeStatList: AttributeStat list)
    (vocationGroupList: VocationGroup list)
    : CombatRoll list =

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

let combatRollRow (combatRoll: CombatRoll) =
    Html.tr [
        Html.td combatRoll.name
        Html.td (dicePoolToString combatRoll.dicePool)
        Html.td (int combatRoll.penetration)
        Html.td (calculatedRangeToString combatRoll.calculatedRange)
        Html.td (damageTypesToString combatRoll.damageTypes)
        Html.td (int combatRoll.engageableOpponents)
        Html.td (calculatedAOEOptionToString combatRoll.areaOfEffectShape)
    ]

let view (model: CombatRoll list) =
    Bulma.container [
        Bulma.label "Combat Rolls:"
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
                Html.tableBody (List.map combatRollRow model)
            ]
        ]
    ]