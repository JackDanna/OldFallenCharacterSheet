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