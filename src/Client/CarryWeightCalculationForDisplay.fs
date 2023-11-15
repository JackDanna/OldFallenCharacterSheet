module CarryWeightEffectForDisplay

open FallenLib.CarryWeightEffect
open FallenLib.EffectForDisplay
open FallenLib.Attribute
open FallenLib.SkillStat

let determineWeightClass
    (attributeStatList: AttributeStat list)
    (coreSkillList: SkillStat list)
    (inventoryWeight: float)
    (weightClassList: WeightClass list)
    (carryWeightCalculation: CarryWeightCalculation)
    =

    let maxCarryWeight =
        calculateMaxCarryWeight carryWeightCalculation attributeStatList coreSkillList

    let percentOfMaxCarryWeight = maxCarryWeight / inventoryWeight

    List.find
        (fun weightClass ->
            (weightClass.topPercent >= percentOfMaxCarryWeight)
            && (percentOfMaxCarryWeight
                >= weightClass.bottomPercent))
        weightClassList

//let determine