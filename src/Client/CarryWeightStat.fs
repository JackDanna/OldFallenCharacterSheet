module CarryWeightStat

open FallenLib.CarryWeightStat
open FallenLib.CarryWeightCalculation
open FallenLib.CoreSkillGroup
open FallenLib.WeightClass

type SetCarryWeightCalculationData =
    { newCarryWeightCalculationName: string
      coreSkillGroupList: CoreSkillGroup list
      carryWeightCalculationMap: Map<string, CarryWeightCalculation>
      weightClassList: WeightClass list }

type SetCurrentWeightData =
    { newCurrentWeight: float
      weightClassList: WeightClass list }

type Msg =
    | SetCarryWeightCalculation of SetCarryWeightCalculationData
    | SetCurrentWeight of SetCurrentWeightData

let update (msg: Msg) (model: CarryWeightStat) : CarryWeightStat =
    match msg with
    | SetCarryWeightCalculation data ->
        let newCarryWeightCalculation =
            data.carryWeightCalculationMap.Item data.newCarryWeightCalculationName

        let newMaxCarryWeight =
            calculateCarryWeight newCarryWeightCalculation data.coreSkillGroupList

        { model with
            carryWeightCalculation = newCarryWeightCalculation
            maxWeight = newMaxCarryWeight
            weightClass = determineWeightClass newMaxCarryWeight model.currentWeight data.weightClassList }
    | SetCurrentWeight data ->
        { model with
            currentWeight = data.newCurrentWeight
            weightClass = determineWeightClass model.maxWeight model.currentWeight data.weightClassList }

open Feliz
open Feliz.Bulma

//let view  (model:CarryWeightStat) (dispatch: Msg -> unit) =