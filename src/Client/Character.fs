module Character

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
open FallenLib.Dice
open FallenLib.Vocation

type Msg =
    | AttributeListMsg of AttributeList.Msg
    | CoreSkillListMsg of CoreSkillList.Msg
    | VocationListMsg of VocationList.Msg
    | SetName of string
    | EquipmentListMsg of EquipmentList.Msg
    | ContainerListMsg of ContainerList.Msg
    | DestinyPointsMsg of DestinyPoints.Msg
    | CharacterEffectListMsg of CharacterEffectForDisplayList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg
    | CarryWeightStatOptionMsg of CarryWeightStatOption.Msg
    | SetDefault

let init (attributeList: Attribute list) (coreSkillList: CoreSkill list) : Character =

    let vocationList = VocationList.init ()

    { name = ""
      attributeList = attributeList
      coreSkillList = coreSkillList
      coreSkillDicePoolList = calculateCoreSkillDicePoolList [] [] attributeList coreSkillList
      vocationList = vocationList
      vocationDicePoolList = vocationListToVocationDicePoolList [] [] attributeList vocationList
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
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    defaultAttributeList
                    defaultCoreSkillList }

    | AttributeListMsg msg ->
        let newAttributeList = AttributeList.update msg model.attributeList

        { model with
            attributeList = newAttributeList
            coreSkillDicePoolList =
                calculateCoreSkillDicePoolList
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    newAttributeList
                    model.coreSkillList
            vocationDicePoolList =
                vocationListToVocationDicePoolList
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    newAttributeList
                    model.vocationList
            characterEffectForDisplayList =
                CharacterEffectForDisplayList.update
                    { attributeList = newAttributeList
                      coreSkillList = model.coreSkillList
                      skillDiceModEffectList = skillAdjustments
                      attributeDeterminedDiceModEffectList = attributeDeterminedDiceModEffects }
                    characterEffectMap
                    movementSpeedCalculationMap
                    CharacterEffectForDisplayList.Msg.RecalculateMovementSpeed
                    model.characterEffectForDisplayList
            carryWeightStatOption =
                CarryWeightStatOption.update
                    (calculateCharacterWeight model.equipmentList model.containerList)
                    newAttributeList
                    model.coreSkillList
                    carryWeightCalculationMap
                    weightClassList
                    (CarryWeightStatOption.Msg.Recalculate)
                    model.carryWeightStatOption }

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

    | VocationListMsg msg ->

        let newVocationList = VocationList.update model.attributeList msg model.vocationList

        { model with
            vocationList = newVocationList
            vocationDicePoolList =
                vocationListToVocationDicePoolList
                    skillAdjustments
                    attributeDeterminedDiceModEffects
                    model.attributeList
                    newVocationList
            combatRollList = loadedCombatRollUpdate model.equipmentList model.attributeList newVocationList }

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

        { model with
            equipmentList = newEquipmentList
            equipmentEffectForDisplayList = newEquipmentEffectForDisplayList
            combatRollList = loadedCombatRollUpdate newEquipmentList model.attributeList model.vocationList
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

        CoreSkillList.view
            (List.map dicePoolToString model.coreSkillDicePoolList)
            model.coreSkillList
            (CoreSkillListMsg >> dispatch)

        |> AttributeList.view model.attributeList (AttributeListMsg >> dispatch)

        VocationList.view
            combatVocationalSkill
            (vocationDicePoolListToStringifiedVocationDicePoolList model.vocationDicePoolList)
            (attributesToAttributeNames model.attributeList)
            model.vocationList
            (VocationListMsg >> dispatch)

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