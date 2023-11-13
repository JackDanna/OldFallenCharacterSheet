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

type IFallenDataApi =
    { getInitData: unit
        -> Async<CoreSkillGroup list * Item list * Map<string, MagicSkill> * Map<string, MagicCombat> * Map<string, Range> * Map<string, CharacterEffect> * string list> }