module Character

open Elmish
open Fable.Remoting.Client
open Shared

open FallenLib.Vocation

type Model = {
    name: string
    coreSkillTables : CoreSkillGroups.Model
    vocationTables : VocationTables.Model
}

let defaultCoreSkillTables : CoreSkillGroups.Model = [
    {
        attributeStat = { AttributeStat.init() with attribute = "STR"}
        coreSkillList = [
            { name = "Athletics"; lvl = Neg1To4Stat.init()}
            { name = "Climb"; lvl = Neg1To4Stat.init()}
            { name = "Endurance"; lvl = Neg1To4Stat.init()}
            { name = "Lift"; lvl = Neg1To4Stat.init()}
        ]
    }
    {
        attributeStat = { AttributeStat.init() with attribute = "RFX"}
        coreSkillList = [
            { name = "Athletics"; lvl = Neg1To4Stat.init()}
            { name = "Climb"; lvl = Neg1To4Stat.init() }
            { name = "Endurance"; lvl = Neg1To4Stat.init() }
            { name = "Lift"; lvl = Neg1To4Stat.init() }
        ]
    }
    {
        attributeStat = { AttributeStat.init() with attribute = "INT"}
        coreSkillList = [
            { name = "Athletics"; lvl = Neg1To4Stat.init() }
            { name = "Climb"; lvl = Neg1To4Stat.init() }
            { name = "Endurance"; lvl = Neg1To4Stat.init() }
            { name = "Lift"; lvl = Neg1To4Stat.init() }
        ]
    }
]

let coreSkillTablesToAttributes (coreSkillTables:CoreSkillGroups.Model) =
    List.map ( fun (table: CoreSkillGroup.Model) ->
        table.attributeStat
    ) coreSkillTables


type Msg =
    | CoreSkillTablesMsg of CoreSkillGroups.Msg
    | VocationTableMsg of VocationTables.Msg
    | SetName of string

let init () : Model =
    {
        name = "Javk Wick"
        coreSkillTables = defaultCoreSkillTables
        vocationTables = VocationTables.init ()
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | CoreSkillTablesMsg coreSkillTableMsg ->
        let newCoreSkillTables = CoreSkillGroups.update coreSkillTableMsg model.coreSkillTables

        {
            model with 
                coreSkillTables = newCoreSkillTables
                vocationTables =
                    List.map ( fun vocationTable ->
                        VocationTable.update
                            (VocationTable.Msg.SetGoverningAttributes (coreSkillTablesToAttributes newCoreSkillTables))
                            vocationTable
                    ) model.vocationTables
        }

    | VocationTableMsg vocationTableMsg ->
        { model with vocationTables = VocationTables.update vocationTableMsg model.vocationTables }

    | SetName name -> { model with name = name }

open Feliz
open Feliz.Bulma

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
                        CoreSkillGroups.view model.coreSkillTables ( CoreSkillTablesMsg >> dispatch )
                    ]
                    Bulma.container [
                        VocationTables.view
                            (coreSkillGroupsToGoverningAttribute model.coreSkillTables)
                            model.vocationTables
                            ( VocationTableMsg >> dispatch )
                    ]
                ]
            ]
        ]
    ] 