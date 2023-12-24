module DicePool

open FallenLib.CoreSkillGroup
open FallenLib.Dice
open FallenLib.SkillStat
open FallenLib.SkillDiceModEffect
open FallenLib.Attribute
open FallenLib.AttributeDeterminedDiceModEffect

type Msg = CalculateDicePool

// let init
//     (skillDiceModEffectList: SkillDiceModEffect list)
//     (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
//     (attributeStat: Attribute list)
//     =

let update
    (skillDiceModEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (attribute: Attribute)
     =

    { model with
            dicePool =
                coreSkillToDicePool
                    baseDicePool
                    model.lvl
                    attribute
                    (collectSkillAdjustmentDiceMods model.name skillDiceModEffectList)
                    attributeDeterminedDiceModEffectList }