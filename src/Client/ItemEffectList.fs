module ItemEffectList

open FallenLib.EffectForDisplay
open FallenLib.ItemEffect

open Feliz
open Feliz.Bulma

let itemEffectForDisplayRow (itemEffectForDisplay: EffectForDisplay) =
    [ Html.td itemEffectForDisplay.name
      Html.td itemEffectForDisplay.effect ]
    @ DurationAndSource.nonInteractiveView itemEffectForDisplay.durationAndSource
    |> Html.tr

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