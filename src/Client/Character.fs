module Character

open FallenLib.CoreSkillGroup
open FallenLib.Item
open FallenLib.MagicSkill
open FallenLib.MagicCombat
open FallenLib.Range
open FallenLib.Equipment
open FallenLib.Character
open FallenLib.EffectForDisplay
open FallenLib.CarryWeightCalculation
open FallenLib.MovementSpeedEffect
open FallenLib.WeightClass
open FallenLib.Attribute
open FallenLib.CoreSkill
open FallenLib.CoreSkillDicePool

type Msg =
    | CoreSkillListMsg of CoreSkillList.Msg
    | VocationGroupListMsg of VocationGroupList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | ContainerListMsg of ContainerList.Msg
    | DestinyPointsMsg of DestinyPoints.Msg
    | CharacterEffectListMsg of CharacterEffectForDisplayList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg
    | CarryWeightStatOptionMsg of CarryWeightStatOption.Msg
    | SetDefault

let init (attributeList: Attribute list) (coreSkillList: CoreSkill list) : Character =

    { name = ""
      attributeList = attributeList
      coreSkillList = coreSkillList
      coreSkillDicePoolList = []
      vocationGroupList = VocationGroupList.init attributeList
      equipmentList = EquipmentList.init ()
      combatRollList = []
      containerList = []
      destinyPoints = DestinyPoints.init ()
      characterEffectForDisplayList = []
      equipmentEffectForDisplayList = []
      characterInformation = CharacterInformation.init ()
      carryWeightStatOption = None }

let update
    (defaultAttributeList: Attribute list)
    (defaultCoreSkillList: CoreSkill list)
    (allItemList: Item list)
    (magicSkillMap: Map<string, MagicSkill>)
    (magicCombatMap: Map<string, MagicCombat>)
    (rangeMap: Map<string, Range>)
    (characterEffectMap: Map<string, EffectForDisplay>)
    (carryWeightCalculationMap: Map<string, CarryWeightCalculation>)
    (weightClassList: WeightClass list)
    (movementSpeedCalculationMap: Map<string, MovementSpeedCalculation>)
    (msg: Msg)
    (model: Character)
    : Character =

    let loadedCombatRollUpdate =
        CombatRollTable.update
            magicSkillMap
            magicCombatMap
            rangeMap
            (attributesToAttributeNames defaultAttributeList)
            []

    let (skillAdjustments, attributeDeterminedDiceModEffects) =
        collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
            model.equipmentEffectForDisplayList
            model.characterEffectForDisplayList
            (carryWeightStatOptionToAttributeDeterminedDiceMod model.carryWeightStatOption)

    let coreSkillAndAttributeData =
        { attributeList = model.attributeList
          coreSkillList = model.coreSkillList
          skillDiceModEffectList = skillAdjustments
          attributeDeterminedDiceModEffectList = attributeDeterminedDiceModEffects }

    match msg with
    | SetDefault ->

        { model with
            attributeList = defaultAttributeList
            coreSkillList = defaultCoreSkillList
            vocationGroupList =
                VocationGroupList.update
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    defaultAttributeList
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList }

    | CoreSkillListMsg msg ->
        let newCoreSkillList = CoreSkillList.update msg model.coreSkillList

        let newCarryWeightStatOption =
            CarryWeightStatOption.update
                (calculateCharacterWeight model.equipmentList model.containerList)
                model.attributeList
                model.coreSkillList
                carryWeightCalculationMap
                weightClassList
                CarryWeightStatOption.Msg.Recalculate
                model.carryWeightStatOption

        let newCoreSkillAndAttributeData =
            { attributeList = model.attributeList
              coreSkillList = newCoreSkillList
              skillDiceModEffectList = skillAdjustments
              attributeDeterminedDiceModEffectList = attributeDeterminedDiceModEffects }

        { model with
            coreSkillList = newCoreSkillList
            carryWeightStatOption = newCarryWeightStatOption
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    model.attributeList
                    newCoreSkillList
            characterEffectForDisplayList =
                CharacterEffectForDisplayList.update
                    newCoreSkillAndAttributeData
                    characterEffectMap
                    movementSpeedCalculationMap
                    CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                    model.characterEffectForDisplayList
            equipmentEffectForDisplayList =
                EquipmentEffectForDisplayList.update
                    newCoreSkillAndAttributeData
                    EquipmentEffectForDisplayList.Msg.RecalculateMovementSpeed
                    model.equipmentEffectForDisplayList }

    | VocationGroupListMsg vocationTableMsg ->

        let newVocationTables =
            VocationGroupList.update
                skillAdjustments
                attributeDeterminedDiceModEffects
                model.attributeList
                vocationTableMsg
                model.vocationGroupList

        { model with
            vocationGroupList = newVocationTables
            combatRollList = loadedCombatRollUpdate model.equipmentList model.attributeList newVocationTables }

    | SetName newName -> { model with name = newName }

    | EquipmentListMsg equipmentRowListMsg ->

        let newEquipmentList =
            EquipmentList.update allItemList equipmentRowListMsg model.equipmentList

        let newEquipmentEffectForDisplayList =
            newEquipmentList
            |> equipmentListToEquipedEquipmentEffects
            |> List.map (fun (itemName, itemEffect) ->
                itemEffectToEffectForDisplay coreSkillAndAttributeData itemEffect itemName)

        let (newSkillDiceModEffects, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                newEquipmentEffectForDisplayList
                model.characterEffectForDisplayList
                (carryWeightStatOptionToAttributeDeterminedDiceMod model.carryWeightStatOption)

        let newVocationGroupList =
            VocationGroupList.update
                newSkillDiceModEffects
                newAttributeDeterminedDiceModEffects
                model.attributeList
                VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                model.vocationGroupList

        { model with
            vocationGroupList = newVocationGroupList
            equipmentList = newEquipmentList
            equipmentEffectForDisplayList = newEquipmentEffectForDisplayList
            combatRollList = loadedCombatRollUpdate newEquipmentList model.attributeList newVocationGroupList
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    newSkillDiceModEffects
                    newAttributeDeterminedDiceModEffects
                    model.attributeList
                    model.coreSkillList
            characterEffectForDisplayList =
                CharacterEffectForDisplayList.update
                    { attributeList = model.attributeList
                      coreSkillList = model.coreSkillList
                      skillDiceModEffectList = newSkillDiceModEffects
                      attributeDeterminedDiceModEffectList = newAttributeDeterminedDiceModEffects }
                    characterEffectMap
                    movementSpeedCalculationMap
                    CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                    model.characterEffectForDisplayList
            carryWeightStatOption =
                CarryWeightStatOption.update
                    (calculateCharacterWeight newEquipmentList model.containerList)
                    model.attributeList
                    model.coreSkillList
                    carryWeightCalculationMap
                    weightClassList
                    CarryWeightStatOption.Msg.Recalculate
                    model.carryWeightStatOption }

    | ContainerListMsg containerListMsg ->
        let newContainerList =
            ContainerList.update allItemList containerListMsg model.containerList

        let newCharacterEffectList =
            CharacterEffectForDisplayList.update
                coreSkillAndAttributeData
                characterEffectMap
                movementSpeedCalculationMap
                CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                model.characterEffectForDisplayList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                newCharacterEffectList
                (carryWeightStatOptionToAttributeDeterminedDiceMod model.carryWeightStatOption)

        { model with
            containerList = newContainerList
            characterEffectForDisplayList = newCharacterEffectList
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    model.attributeList
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    model.attributeList
                    model.coreSkillList }

    | DestinyPointsMsg destinyPointsMsg ->
        { model with destinyPoints = DestinyPoints.update destinyPointsMsg model.destinyPoints }

    | CharacterEffectListMsg msg ->
        let newCharacterEffectList =
            CharacterEffectForDisplayList.update
                coreSkillAndAttributeData
                characterEffectMap
                movementSpeedCalculationMap
                msg
                model.characterEffectForDisplayList

        let (newSkillAdjustments, newAttributeDeterminedDiceModEffects) =
            collectSkillAdjustmentsAndAttributeDeterminedDiceModEffects
                model.equipmentEffectForDisplayList
                newCharacterEffectList
                (carryWeightStatOptionToAttributeDeterminedDiceMod model.carryWeightStatOption)

        { model with
            characterEffectForDisplayList = newCharacterEffectList
            vocationGroupList =
                VocationGroupList.update
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    model.attributeList
                    VocationGroupList.Msg.SetAttributeStatsAndCalculateDicePools
                    model.vocationGroupList
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    newSkillAdjustments
                    newAttributeDeterminedDiceModEffects
                    model.attributeList
                    model.coreSkillList }
    | CharacterInformationMsg msg ->
        { model with characterInformation = CharacterInformation.update msg model.characterInformation }

    | CarryWeightStatOptionMsg msg ->
        { model with
            carryWeightStatOption =
                CarryWeightStatOption.update
                    (calculateCharacterWeight model.equipmentList model.containerList)
                    model.attributeList
                    model.coreSkillList
                    carryWeightCalculationMap
                    weightClassList
                    msg
                    model.carryWeightStatOption }

open Feliz
open Feliz.Bulma

let view
    (carryWeightCalculationNameList: string list)
    (characterEffectKeyList: string list)
    (combatVocationalSkill)
    (allItemList: Item list)
    (model: Character)
    (dispatch: Msg -> unit)
    =

    let allItemNameList = (List.map (fun (item: Item) -> item.name) allItemList)

    Bulma.container [

        Bulma.input.text [
            prop.value model.name
            prop.placeholder "Character Name"
            prop.onTextChange (SetName >> dispatch)
            prop.classes [
                "is-large"
                "has-text-centered"
            ]
        ]
        |> Bulma.content

        Bulma.image [
            Html.img [
                prop.style [
                    style.height 500
                    style.width 500
                ]
                prop.classes [ "center" ]

                prop.src "https://cogentroleplaycommunity.github.io/Fallen/src/Characters/PC/JavkWick/Javk-Wick.png"
            ]
        ]
        |> Bulma.content

        CoreSkillList.view model.coreSkillList (CoreSkillListMsg >> dispatch)

        VocationGroupList.view combatVocationalSkill model.vocationGroupList (VocationGroupListMsg >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        CharacterEffectForDisplayList.view
            characterEffectKeyList
            model.characterEffectForDisplayList
            (CharacterEffectListMsg >> dispatch)

        CarryWeightStatOption.view
            carryWeightCalculationNameList
            model.carryWeightStatOption
            (CarryWeightStatOptionMsg >> dispatch)

        EquipmentEffectForDisplayList.view model.equipmentEffectForDisplayList

        EquipmentList.view allItemNameList model.equipmentList (EquipmentListMsg >> dispatch)

        CombatRollTable.view model.combatRollList

        ContainerList.view
            (List.collect itemToContainerClassNames allItemList)
            allItemNameList
            model.containerList
            (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]