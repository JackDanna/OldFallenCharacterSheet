module EffectForDisplay

open FallenLib.EffectForDisplay

type Msg =
    | SetName of string
    | SetEffect of string
    | SetDuration of string
    | SetSource of string

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =
    match msg with
    | SetName newName -> { model with name = newName }
    | SetEffect newEffect -> { model with effect = newEffect }
    | SetDuration newDuration -> { model with duration = newDuration }
    | SetSource newSource -> { model with source = newSource }

open Feliz
open Feliz.Bulma

let view (model: EffectForDisplay) (dispatch: Msg -> unit) =
    Html.tr [
        Bulma.input.text [
            prop.text model.name
            prop.onChange (fun input -> dispatch (SetName input))
        ]
        |> Html.td

        Bulma.input.text [
            prop.text model.effect
            prop.onChange (fun input -> dispatch (SetEffect input))
        ]
        |> Html.td

        Bulma.input.text [
            prop.text model.duration
            prop.onChange (fun input -> dispatch (SetDuration input))
        ]
        |> Html.td

        Bulma.input.text [
            prop.text model.source
            prop.onChange (fun input -> dispatch (SetSource input))
        ]
        |> Html.td
    ]