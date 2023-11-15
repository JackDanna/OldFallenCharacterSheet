namespace Shared

open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.CoreSkillGroup
open FallenLib.CharacterEffect

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FallenData =
    { defaultCoreSkillGroupList: CoreSkillGroup list
      allItemList: Item list
      magicSkillMap: Map<string, MagicSkill>
      magicCombatMap: Map<string, MagicCombat>
      rangeMap: Map<string, Range>
      combatVocationalSkill: string list
      characterEffectMap: Map<string, CharacterEffect> }

type IFallenDataApi =
    { getInitData: unit -> Async<FallenData> }