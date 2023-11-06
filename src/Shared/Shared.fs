namespace Shared

open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.CoreSkillGroup

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IFallenDataApi =
    { getInitData: unit
        -> Async<CoreSkillGroup list * Item list * Map<string, MagicSkill> * Map<string, MagicCombat> * Map<string, Range> * string list> }