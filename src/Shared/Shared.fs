namespace Shared

open System
open FallenLib.Item
open FallenLib.Damage

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }


type IFallenDataApi =
    { //getItems: unit -> Async<Item list>
      getDamageTypes: unit -> Async<DamageType list> }