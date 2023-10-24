module Character

open Elmish
open Fable.Remoting.Client
open Shared

open FallenLib.Dice
open FallenLib.SkillUtils

type Model = {
    name: string
    coreSkillTables : CoreSkillTables.Model
    vocationTables : VocationTables.Model
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

let defaultVocationTables : VocationTables.Model = [
    {
        name = ""
        vocationLevel = Neg1To4Stat.init()
        governingAttributes = [
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "STR"}
            }
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "RFX"}
            }
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "INT"}
            }
        ]
        baseDice = baseDicePoolCalculation
        vocationalSkillRowList = [VocationalSkillRow.init()]
    }
    {
        name = ""
        vocationLevel = Neg1To4Stat.init()
        governingAttributes = [
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "STR"}
            }
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "RFX"}
            }
            { 
                isGoverning = false
                attributeStat = { AttributeRow.init() with name = "INT"}
            }
        ]
        baseDice = baseDicePoolCalculation
        vocationalSkillRowList = [VocationalSkillRow.init()]
    }
]

type Msg =
    | CoreSkillTablesMsg of CoreSkillTables.Msg
    | VocationTableMsg of VocationTables.Msg
    | SetName of string
    | Reset

let attributesToGoverningAttributes attributes governingAttributes =
    attributes
    |> List.map ( fun (attribute:AttributeRow.Model) ->
        {
            attributeStat = attribute
            isGoverning =
                governingAttributes
                |> List.collect ( fun currentGoverningAttribute ->
                    if (currentGoverningAttribute.attributeStat.name = attribute.name) && currentGoverningAttribute.isGoverning then
                        [currentGoverningAttribute.isGoverning]
                    else
                        []
                )
                |> List.isEmpty
                |> not
        }
    )

let init () : Model =
    {
        name = "Javk Wick"
        coreSkillTables = defaultCoreSkillTables
        vocationTables = defaultVocationTables
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | CoreSkillTablesMsg coreSkillTableMsg ->
        let newCoreSkillTables = CoreSkillTables.update coreSkillTableMsg model.coreSkillTables

        let newAttributes = 
            List.map(fun (coreSkillTable: CoreSkillTable.Model) ->
                coreSkillTable.attributeRow
            ) newCoreSkillTables

        {
            model with 
                coreSkillTables = newCoreSkillTables
                vocationTables =
                    List.map ( fun (vocationTable:VocationTable.Model) ->
                        { vocationTable with governingAttributes = attributesToGoverningAttributes newAttributes vocationTable.governingAttributes }
                    ) model.vocationTables
        }

    | VocationTableMsg vocationTableMsg ->
        { model with vocationTables = VocationTables.update vocationTableMsg model.vocationTables }

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
                    Bulma.container [
                        VocationTables.view model.vocationTables ( VocationTableMsg >> dispatch )
                    ]
                ]
            ]
        ]
    ] 