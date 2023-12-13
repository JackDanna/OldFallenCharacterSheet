module InteractiveEffectForDisplay

open FallenLib.EffectForDisplay

type Msg =
    | NameMsg of StringInputTableData.Msg
    | EffectMsg of StringInputTableData.Msg
    | DurationAndSourceMsg of DurationAndSource.Msg

let update (msg: Msg) (model: EffectForDisplay) : EffectForDisplay =
    match msg with
    | NameMsg msg -> { model with name = StringInputTableData.update msg model.name }
    | EffectMsg msg -> { model with effect = StringInputTableData.update msg model.effect }
    | DurationAndSourceMsg msg ->
        { model with durationAndSource = DurationAndSource.update msg model.durationAndSource }

open Feliz

let interactiveEffectForDisplayTableData (model: EffectForDisplay) (dispatch: Msg -> unit) =
    [ StringInputTableData.interactiveView model.name (NameMsg >> dispatch)
      StringInputTableData.interactiveView model.effect (EffectMsg >> dispatch) ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)