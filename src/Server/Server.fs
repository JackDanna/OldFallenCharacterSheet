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

    let damageTypeData =
        makeFallenData "DamageTypeData.csv" (fun row -> (DamageType row.["desc"]))

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