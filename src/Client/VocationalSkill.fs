module VocationalSkill

open FallenLib.VocationGroup
open FallenLib.Vocation
open FallenLib.Dice
open FallenLib.SkillStat
open FallenLib.ZeroToFour
open FallenLib.SkillDiceModEffect
open FallenLib.AttributeDeterminedDiceModEffect

type Msg =
    | SetName of string
    | VocationalSkillStatMsg of VocationalSkillStat.Msg
    | CalculateDicePool

let init (governingAttribute: GoverningAttribute list) : SkillStat =
    let lvl = Neg1To4.init ()

    { name = ""
      lvl = lvl
      dicePool = vocationalSkillToDicePool baseDicePool lvl governingAttribute [] [] }


let update
    (skillDiceModificationEffectList: SkillDiceModEffect list)
    (attributeDeterminedDiceModEffectList: AttributeDeterminedDiceModEffect list)
    (levelCap: ZeroToFour)
    (governingAttributeList: GoverningAttribute list)
    (msg: Msg)
    (model: SkillStat)
    : SkillStat =
    match msg with
    | SetName newName -> { model with name = newName }
    | VocationalSkillStatMsg vocationalSkillStatMsg ->
        let lvl = VocationalSkillStat.update levelCap vocationalSkillStatMsg model.lvl

        { model with
            lvl = lvl
            dicePool =
                vocationalSkillToDicePool
                    baseDicePool
                    lvl
                    governingAttributeList
                    (collectSkillAdjustmentDiceMods model.name skillDiceModificationEffectList)
                    attributeDeterminedDiceModEffectList }
    | CalculateDicePool ->
        let newLvl =
            VocationalSkillStat.update levelCap VocationalSkillStat.Msg.CheckIfLevelCapExceeded model.lvl

        { model with
            lvl = newLvl
            dicePool =
                vocationalSkillToDicePool
                    baseDicePool
                    newLvl
                    governingAttributeList
                    (collectSkillAdjustmentDiceMods model.name skillDiceModificationEffectList)
                    attributeDeterminedDiceModEffectList }

open Feliz
open Feliz.Bulma

let view (combatVocationalSkills: string list) (levelCap: ZeroToFour) (model: SkillStat) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            Bulma.dropdown [
                Bulma.dropdownTrigger [
                    Bulma.input.text [
                        prop.list "combatVocationalSkills"
                        prop.defaultValue model.name
                        prop.onTextChange (fun value -> dispatch (SetName value))
                    ]
                    Html.datalist [
                        prop.id "combatVocationalSkills"
                        prop.children (
                            List.map
                                (fun (combatVocationalSkill: string) ->
                                    Html.option [
                                        prop.value combatVocationalSkill
                                    ])
                                combatVocationalSkills
                        )
                    ]
                ]
                Bulma.dropdownMenu [
                    Bulma.dropdownContent [
                        Bulma.dropdownItem.a [
                            prop.children [
                                Html.span "Single Sequence"
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
            prop.text (dicePoolToString model.dicePool)
        ]
        Bulma.column [
            VocationalSkillStat.view levelCap model.lvl (VocationalSkillStatMsg >> dispatch)
        ]
    ]