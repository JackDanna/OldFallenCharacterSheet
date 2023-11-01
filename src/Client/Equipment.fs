module Equipment

open OldFallenLib.Equipment
open OldFallenLib.Item

type Model =
    | EquipmentItem of EquipmentItem
    | Empty

type Msg =
    | SetEquipmentItem of EquipmentItem

let init () = Empty

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | SetEquipmentItem equipmentItem ->
        EquipmentItem (equipmentItem)

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    let makeRow (equipmentItem:EquipmentItem) =
        Html.tableRow [
            Html.tableCell [
                Bulma.input.checkbox [
                    prop.isChecked equipmentItem.equipped
                ]
            ]
            Html.tableCell [
                Bulma.input.number [
                    prop.value ( int equipmentItem.quantity )
                    prop.min 0
                ]
            ]
            Item.view () equipmentItem.item
            
        ]

    match model with
    | EquipmentItem equipmentItem ->
        makeRow
            equipmentItem.equipped
            equipmentItem.quantity
            equipmentItem.item
            
    | Empty ->
        Empty
            
    // Bulma.table [
    //     Html.tableHeader [
    //         Html.tr (
    //             List.map ( fun (thString:string) ->
    //                 Html.th thString
    //             ) ["On"; "Name"; "#"; "Type"; "Tier"; "Dur."; "LB"; "Value"]
    //         )
    //     ]
    //     Html.tableBody [
    //         Html.tr (
    //             List.map ( fun (thString:string) ->
    //                 Html.th thString
    //             ) [model.; "Name"; "#"; "Type"; "Tier"; "Dur."; "LB"; "Value"]
    //         )
    //     ]
    // ]