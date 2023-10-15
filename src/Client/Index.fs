module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    Name: string
}

type Msg =
    | SetName of string
    | Reset

let init () : Model * Cmd<Msg> =
    { Name = "Name"}, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetName name -> { model with Name = name}, Cmd.none
    | Reset -> init()

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let skillRow (skillName:string) (dice:string)  =
    Bulma.columns [
        Bulma.column [ prop.text skillName ]
        Bulma.column [ prop.text dice ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [ prop.text "-" ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]
        Bulma.column [
            Html.input [
                prop.type' "checkbox"
            ]
        ]

    ]

let attributeTable (governingAttribute:string) (governingAttributes:int) (skillNames:list<string>) (skillLevels:list<int>) =
    Bulma.column [
        Bulma.notification [
            prop.style [
                
            ]
            color.isPrimary
            prop.children [
                List.zip skillNames skillLevels
                |> List.map ( 
                    fun (skillName, skillLevel)-> 
                        Html.li [ prop.children [ skillRow skillName "0" ] ]
                )
                |> Html.ul
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isDanger

        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://www.onlygfx.com/wp-content/uploads/2015/12/simple-old-paper-1-transparent.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]

        prop.children [

            Bulma.heroHead [
                Bulma.navbar [
                    color.isPrimary
                    prop.children [
                        Bulma.navbarItem.div [
                            Bulma.title.h3 [
                                prop.text "Fallen"
                                prop.style [
                                    style.fontFamily "PT Serif Caption"
                                ]
                            ]
                        ]
                    ]
                ]
            ]

            Bulma.heroBody [
                Bulma.container [
                    Bulma.columns [
                        
                        attributeTable "" 0 ["Athletics"] [1]
                        attributeTable "" 0 ["Athletics"] [1]
                        attributeTable "" 0 ["Athletics"] [1]
                    ]
                    // Bulma.control.p [
                    //     control.isExpanded
                    //     prop.children [
                    //         Bulma.input.text [
                    //             prop.value model.Name
                    //             prop.onChange (fun input -> SetName input |> dispatch)
                    //         ]
                    //     ]
                    // ]
                    //Bulma.title model.Name
                ]
            ]
        ]
    ] 