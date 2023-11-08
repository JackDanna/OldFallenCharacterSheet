module CoreSkill

open FallenLib.CoreSkillGroup
open FallenLib.Dice
open FallenLib.SkillStat
open FallenLib.SkillAdjustment
open FallenLib.Neg1To4

type Msg =
    | Neg1To4Msg of Neg1To4.Msg
    | CalculateDicePool

let init (skillAdjustmentList: SkillAdjustment list) (attributeLvl: Neg1To4) =
    let lvl = Neg1To4.init ()

    let name = ""

    { name = name
      lvl = lvl
      dicePool =
        coreSkillToDicePool baseDicePool lvl attributeLvl (collectSkillAdjustmentDiceMods name skillAdjustmentList) }

let update
    (skillAdjustmentList: SkillAdjustment list)
    (attributeLvl: Neg1To4)
    (msg: Msg)
    (model: SkillStat)
    : SkillStat =

    match msg with
    | Neg1To4Msg neg1To4StatMsg ->
        let newLvl = Neg1To4.update neg1To4StatMsg model.lvl

        { model with
            lvl = newLvl
            dicePool =
                coreSkillToDicePool
                    baseDicePool
                    newLvl
                    attributeLvl
                    (collectSkillAdjustmentDiceMods model.name skillAdjustmentList) }
    | CalculateDicePool ->
        { model with
            dicePool =
                coreSkillToDicePool
                    baseDicePool
                    model.lvl
                    attributeLvl
                    (collectSkillAdjustmentDiceMods model.name skillAdjustmentList) }

open Feliz
open Feliz.Bulma

let view (model: SkillStat) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [ prop.text model.name ]
        Bulma.column [
            model.dicePool |> dicePoolToString |> prop.text
        ]
        Bulma.column [
            Neg1To4.view model.lvl (Neg1To4Msg >> dispatch)
        ]
    ]