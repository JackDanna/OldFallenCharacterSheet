module SkillRow

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    Name: string
    Dice: string
}

type Msg =
    | Reset

let init () : Model * Cmd<Msg> =
    let model = { Name = "NULL"; Dice = "0d6"}
    let cmd = Cmd.none
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | Reset -> init()

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.label [
        Html.input [
            prop.classes ["checkbox"]
        ]
    ]