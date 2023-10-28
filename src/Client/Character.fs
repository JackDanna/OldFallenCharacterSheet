module Character

open Elmish
open Fable.Remoting.Client
open Shared

open FallenLib.Vocation

type Model = {
    name: string
    coreSkillTables : CoreSkillTables.Model
    vocationTables : VocationTables.Model
}

let defaultCoreSkillTables : CoreSkillTables.Model = [
    {
        attributeRow = { AttributeRow.init() with name = "STR"}
        coreSkillList = [
            { name = "Athletics"; level = Neg1To4Stat.init(); governingAttribute = AttributeRow.init()}
            { name = "Climb"; level = Neg1To4Stat.init(); governingAttribute = AttributeRow.init()}
            { name = "Endurance"; level = Neg1To4Stat.init(); governingAttribute = AttributeRow.init()}
            { name = "Lift"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init()}
        ]
    }
    {
        attributeRow = { AttributeRow.init() with name = "RFX"}
        coreSkillList = [
            { name = "Athletics"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init()}
            { name = "Climb"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
            { name = "Endurance"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
            { name = "Lift"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
        ]
    }
    {
        attributeRow = { AttributeRow.init() with name = "INT"}
        coreSkillList = [
            { name = "Athletics"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
            { name = "Climb"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
            { name = "Endurance"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
            { name = "Lift"; level = Neg1To4Stat.init();governingAttribute = AttributeRow.init() }
        ]
    }
]

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

let attributesToGoverningAttributesInit attributes =
    List.map ( fun (attribute:AttributeRow.Model) ->
        {
            attributeStat = attribute
            isGoverning = false
        }
    ) attributes

let coreSkillTablesToAttributes (coreSkillTables:CoreSkillTables.Model) =
    List.map ( fun (table: CoreSkillTable.Model) ->
        table.attributeRow
    ) coreSkillTables

let defaultGoverningAttribute =
    defaultCoreSkillTables 
    |> coreSkillTablesToAttributes 
    |> attributesToGoverningAttributesInit


type Msg =
    | CoreSkillTablesMsg of CoreSkillTables.Msg
    | VocationTableMsg of VocationTables.Msg
    | SetName of string

let init () : Model =
    {
        name = "Javk Wick"
        coreSkillTables = defaultCoreSkillTables
        vocationTables = VocationTables.init (defaultGoverningAttribute)
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
                        let newVocationRow = attributesToGoverningAttributes newAttributes vocationTable.vocationRow.governingAttributes
                        { vocationTable with 
                            vocationRow = { vocationTable.vocationRow with governingAttributes = newVocationRow }
                        }
                    ) model.vocationTables
        }

    | VocationTableMsg vocationTableMsg ->
        { model with vocationTables = VocationTables.update vocationTableMsg model.vocationTables }

    | SetName name -> { model with name = name}

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