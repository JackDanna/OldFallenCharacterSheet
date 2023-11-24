module CharacterInformation

open FallenLib.Character

type Msg =
    | SetBackstory of string
    | SetNotes of string
    | SetBeliefsAndMorality of string
    | SetGoalsAndAspirations of string
    | SetDisposition of string

let init () =
    { backstory = ""
      notes = ""
      beliefsAndMorality = ""
      goalsAndAspirations = ""
      disposition = "" }

let update (msg: Msg) (model: CharacterInformation) =
    match msg with
    | SetBackstory newText -> { model with backstory = newText }
    | SetNotes newText -> { model with notes = newText }
    | SetBeliefsAndMorality newText -> { model with beliefsAndMorality = newText }
    | SetGoalsAndAspirations newText -> { model with goalsAndAspirations = newText }
    | SetDisposition newText -> { model with disposition = newText }


open Feliz
open Feliz.Bulma

let view (model: CharacterInformation) (dispatch: Msg -> unit) =

    let characterInformationTextArea (labelName: string) dispatchMsg =
        Bulma.container [
            Html.label [ prop.text labelName ]
            Bulma.textarea [
                prop.placeholder $"Enter {labelName}..."
                prop.onTextChange (fun text -> dispatch (dispatchMsg text))
            ]
        ]

    Bulma.container [
        characterInformationTextArea "Backstory" SetBackstory
        characterInformationTextArea "Notes" SetNotes
        characterInformationTextArea "Beliefs/Morality" SetBeliefsAndMorality
        characterInformationTextArea "Goals/Aspirations" SetGoalsAndAspirations
        characterInformationTextArea "Disposition" SetDisposition
    ]