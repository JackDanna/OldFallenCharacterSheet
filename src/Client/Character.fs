module Character

open Elmish
open Fable.Remoting.Client
open Shared

type Model = {
    name: string
    coreSkillTables : CoreSkillTables.Model
}

let defaultCoreSkillTables : CoreSkillTables.Model = [
    {
        attributeRow = { AttributeRow.init() with name = "STR"}
        skillRowList = [
            { SkillRow.init() with name = "Athletics" }
            { SkillRow.init() with name = "Climb" }
            { SkillRow.init() with name = "Endurance" }
            { SkillRow.init() with name = "Lift" }
        ]
    }
    {
        attributeRow = { AttributeRow.init() with name = "RFX"}
        skillRowList = [
            { SkillRow.init() with name = "Acrobatics" }
            { SkillRow.init() with name = "Perception" }
            { SkillRow.init() with name = "Sleight of Hand" }
            { SkillRow.init() with name = "Stealth" }
        ]
    }
    {
        attributeRow = { AttributeRow.init() with name = "INT"}
        skillRowList = [
            { SkillRow.init() with name = "Communication" }
            { SkillRow.init() with name = "General Knowledge" }
            { SkillRow.init() with name = "Survival" }
            { SkillRow.init() with name = "Willpower" }
        ]
    }
]

type Msg =
    | CoreSkillTablesMsg of CoreSkillTables.Msg
    | SetName of string
    | Reset

let init () : Model =
    { name = "Javk Wick"; coreSkillTables = defaultCoreSkillTables }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | CoreSkillTablesMsg coreSkillTableMsg ->
        { model with coreSkillTables = CoreSkillTables.update coreSkillTableMsg model.coreSkillTables }
    | SetName name -> { model with name = name}
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
                    Bulma.content [
                        Bulma.input.text [
                            prop.value model.name
                            prop.placeholder "Character Name"
                            prop.onChange (fun newName -> SetName newName |> dispatch)
                            prop.classes ["is-large"; "has-text-centered"]
                        ]
                    ]
                    Bulma.container [
                        CoreSkillTables.view model.coreSkillTables ( CoreSkillTablesMsg >> dispatch )
                    ]
                ]
            ]
        ]
    ] 