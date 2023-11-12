module ItemEffectList

open FallenLib.EffectForDisplay
open FallenLib.ItemEffect

type Msg = Reload

let update (msg: Msg) (model: ItemEffect list) : ItemEffect list =
    match msg with
    | Reload -> []

open Feliz
open Feliz.Bulma

let itemEffectForDisplayRow (itemEffectForDisplay: EffectForDisplay) =
    Html.tr [
        Html.td itemEffectForDisplay.name
        Html.td itemEffectForDisplay.effect
        Html.td itemEffectForDisplay.duration
        Html.td itemEffectForDisplay.source
    ]

let view (model: (string * ItemEffect) list) =
    let itemEffectForDisplayHeaders =
        [ "Name"
          "Effect"
          "Duration"
          "Source" ]

    Bulma.container [

        Bulma.label "Item Effects:"

        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    itemEffectForDisplayHeaders
                    |> List.map (fun (thString: string) -> Html.th thString)
                    |> Html.tr
                ]
                Html.tableBody (
                    model
                    |> List.map (fun (itemName, itemEffect) -> itemEffectToEffectForDisplay itemEffect itemName)
                    |> List.map itemEffectForDisplayRow
                )
            ]
        ]
    ]