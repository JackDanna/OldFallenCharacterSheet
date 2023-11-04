module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared


module Storage =
    let todos = ResizeArray()

    let addTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

    do
        addTodo (Todo.create "Create new SAFE project")
        |> ignore

        addTodo (Todo.create "Write your app") |> ignore
        addTodo (Todo.create "Ship it !!!") |> ignore

let todosApi =
    { getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
      addTodo =
        fun todo ->
            async {
                return
                    match Storage.addTodo todo with
                    | Ok () -> todo
                    | Error e -> failwith e
            } }

module FallenServerData =
    open FSharp.Data
    open FallenLib.Damage
    open FallenLib.TypeUtils
    open FallenLib.EngageableOpponents
    open FallenLib.Range
    open FallenLib.ResourceClass
    open FallenLib.Attribute
    open FallenLib.MagicSkill
    open FallenLib.AreaOfEffect
    open FallenLib.Neg1To4
    open FallenLib.Dice
    open FallenLib.MagicCombat
    open FallenLib.WeaponClass
    open FallenLib.ConduitClass
    open FallenLib.DefenseClass
    open FallenLib.WeaponResourceClass
    open FallenLib.ItemTier
    open FallenLib.Item
    open FallenLib.MovementSpeedCalculation

    let makeFallenDataPath fileName =
        __SOURCE_DIRECTORY__ + "/FallenData/" + fileName

    let makeFallenData fileName mappingFunc =
        CsvFile
            .Load(
                makeFallenDataPath fileName,
                hasHeaders = true
            )
            .Rows
        |> Seq.map (mappingFunc)
        |> List.ofSeq

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _ -> failwith ("Error: returns " + boolString)

    // DamageType
    let damageTypeData =
        makeFallenData "DamageTypeData.csv" (fun row -> (DamageType row.["desc"]))

    let stringToDamageTypeList =
        damageTypeData
        |> stringListToTypeMap
        |> stringAndMapToDamageTypeArray

    // EngageableOpponents
    let engageableOpponentsCalculationData =
        makeFallenData "EngageableOpponentsCalculationData.csv" (fun row ->
            { name = string row.["desc"]
              combatRollDivisor = uint row.["combatRollDivisor"]
              maxEO = mapMaxEO row.["maxEO"] })

    let engageableOpponentsMap =
        eoMap (eoCalculationListToMap engageableOpponentsCalculationData)

    // Range
    let calculatedRangeData =
        makeFallenData "CalculatedRangeData.csv" (fun row ->
            { name = string row.["desc"]
              effectiveRange = uint row.["effectiveRange"]
              maxRange = uint row.["maxRange"] })

    let rangeCalculationData =
        makeFallenData "RangeCalculationData.csv" (fun row ->
            { name = string row.["desc"]
              numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
              ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
              roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
              maxRange = uint row.["maxRange"] })

    let rangeMap = createRangeMap calculatedRangeData rangeCalculationData

    let rangeOptionMap string =
        match string with
        | "None" -> None
        | _ -> rangeMap.Item string |> Some

    // ResourceClass
    let resourceClassData =
        makeFallenData "ResourceClassData.csv" (fun row -> (ResourceClass row.["desc"]))

    let resourceClassMap = stringListToTypeMap resourceClassData

    let resourceClassOptionMap string =
        match string with
        | "None" -> None
        | _ -> Some <| resourceClassMap.Item string

    // Attribute
    let attributeData =
        makeFallenData "AttributeData.csv" (fun row -> Attribute row.["desc"])

    let attributeMap = stringListToTypeMap attributeData

    let mapAndStringToAttributes (attributeMap: Map<string, Attribute>) (input) =
        String.filter ((<>) ' ') input
        |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun attributeString -> attributeMap.Item attributeString)

    let stringToAttributes = mapAndStringToAttributes attributeMap

    // MagicSkill
    let magicSkillData =
        makeFallenData "MagicSkillData.csv" (fun row ->
            { name = string row.["desc"]
              damageTypes = stringToDamageTypeList (string row.["damageTypes"])
              rangeAdjustment = int row.["rangeAdjustment"]
              isMeleeCapable = Bool row.["meleeCapable"]
              magicResourceClass = string row.["magicResourceClass"] })

    let magicSkillMap =
        List.map (fun (magicSkill: MagicSkill) -> magicSkill.name, magicSkill) magicSkillData
        |> Map.ofList

    // MagicCombat
    let magicCombatData =
        makeFallenData "MagicCombatData.csv" (fun row ->
            { name = string row.["Description"]
              lvlRequirment = int row.["Lvl Requirment"] |> intToNeg1To4
              diceModification = stringToDicePoolModification row.["Dice Modification"]
              penetration = uint row.["Penetration"]
              range = rangeMap.Item(string row.["Range"])
              engageableOpponents = engageableOpponentsMap row.["Engageable Opponents"]
              minResourceRequirment = uint row.["Resource Requirment"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["Area Of Effect"] })

    // WeaponClass
    let weaponClassData =
        makeFallenData "WeaponClassData.csv" (fun row ->
            { name = string row.["desc"]
              oneHandedWeaponDice = stringToDicePoolModificationOption row.["oneHandedWeaponDice"]
              twoHandedWeaponDice = stringToDicePoolModification row.["twoHandedWeaponDice"]
              penetration = uint row.["penetration"]
              range = rangeMap.Item row.["range"]
              damageTypes = stringToDamageTypeList row.["damageTypes"]
              engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
              dualWieldableBonus = stringToDicePoolModificationOption row.["dualWieldableBonus"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
              resourceClass = resourceClassOptionMap row.["resourceClass"] })

    let weaponClassMap =
        List.map (fun (weaponClass: WeaponClass) -> weaponClass.name, weaponClass) weaponClassData
        |> Map.ofList

    // ConduitClass
    let conduitClassData =
        makeFallenData "ConduitClassData.csv" (fun row ->

            { name = string row.["desc"]
              oneHandedDice = stringToDicePoolModificationOption row.["oneHandedDice"]
              twoHandedDice = stringToDicePoolModification row.["twoHandedDice"]
              penetration = uint row.["penetration"]
              rangeAdjustment = int row.["rangeAdjustment"]
              damageTypes = stringToDamageTypeList row.["damageTypes"]
              engageableOpponents =
                match row.["engageableOpponents"] with
                | "None" -> None
                | something -> Some(engageableOpponentsMap something)
              dualWieldableBonus = stringToDicePoolModificationOption row.["dualWieldableBonus"]
              areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
              resourceClass = resourceClassOptionMap row.["resourceClass"]
              effectedMagicSkills =
                row.["effectedMagicSkills"].Split ", "
                |> List.ofArray
                |> List.map (fun magicSkillStr -> magicSkillMap.Item magicSkillStr) })

    let conduitClassMap =
        List.map (fun (conduitClass: ConduitClass) -> conduitClass.name, conduitClass) conduitClassData
        |> Map.ofList


let fallenDataApi: IFallenDataApi =
    { getDamageTypes = fun () -> async { return FallenServerData.damageTypeData } }


let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue fallenDataApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0