module LittleWorldPuzzler.Automata.Rule exposing (rules)

import CellAutomata exposing (Rule)
import LittleWorldPuzzler.Automata.Neighborhood as Neighborhood
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))


type RuleType
    = Surrounds CellType
    | CombinesInto CellType
    | TurnsInto CellType
    | KilledBy CellType
    | KilledBy2 CellType
    | Spawns CellType
    | Disapears


rule : { from : Maybe CellType, to : Maybe CellType } -> List ( Int, Maybe CellType ) -> Rule CellType
rule { from, to } list =
    { from = from, to = to, neighbors = Neighborhood.fromList list }


intoRule : RuleType -> (CellType -> Rule CellType)
intoRule ruleType =
    case ruleType of
        Surrounds to ->
            \containedBy -> rule { from = Nothing, to = Just to } [ ( 3, Just containedBy ) ]

        CombinesInto to ->
            \from -> rule { from = Just from, to = Just to } [ ( 3, Just from ) ]

        TurnsInto to ->
            \from -> rule { from = Just from, to = Just to } [ ( 1, Just to ) ]

        KilledBy2 by ->
            \from -> rule { from = Just from, to = Nothing } [ ( 2, Just by ) ]

        KilledBy by ->
            \from -> rule { from = Just from, to = Nothing } [ ( 1, Just by ) ]

        Spawns to ->
            \by -> rule { from = Nothing, to = Just to } [ ( 1, Just by ) ]

        Disapears ->
            \from -> rule { from = Just from, to = Nothing } []


rules : CellType -> List (Rule CellType)
rules cellType =
    (case cellType of
        Wood ->
            [ TurnsInto Fire
            , CombinesInto Evergreen

            --, rule { from = Just Wood, to = Just Evergreen } [ ( 1, Just Ice ) ]
            , Surrounds Bug --new
            ]

        Water ->
            [ KilledBy2 Fire
            , TurnsInto Ice
            , Spawns Wood
            ]

        Fire ->
            [ {--rule { from = Just Fire, to = Just Volcano } [ ( 3, Just Stone ) ]
            ,--}
              --, Fire |> surrounds Desert
              Disapears
            , Surrounds Volcano
            ]

        Stone ->
            [ CombinesInto Glacier
            , KilledBy Glacier --new

            {--, rule
                { from = Just Stone, to = Nothing }
                [ ( 1, Just Volcano ), ( 1, Just Water ) ]--}
            ]

        Volcano ->
            [ --rule { from = Just Volcano, to = Just Volcano } [ ( 2, Just Stone ) ]
              --, rule { from = Just Volcano, to = Just Fog } []
              Disapears
            , Spawns Fire --Fog
            ]

        Fog ->
            [ TurnsInto Water
            , TurnsInto Fire --new

            --, rule { from = Just Fog, to = Just Fire } [ ( 1, Just Wood ) ]
            ]

        Desert ->
            [--Desert |> killedBy2 Water
            ]

        Glacier ->
            [ {--rule { from = Just Glacier, to = Just Glacier } [ ( 2, Just Stone ) ]
            ,--}
              KilledBy Fire
            , Spawns Ice
            ]

        Ice ->
            [ Disapears
            , Surrounds Snow --new

            --rule { from = Nothing, to = Just Snow } [ ( 2, Just Ice ) ]
            ]

        Snow ->
            [ KilledBy2 Fire
            ]

        Evergreen ->
            [ KilledBy Fire
            ]

        Bug ->
            [ TurnsInto Fire --new
            , Spawns Bug --new
            ]
    )
        |> List.map (intoRule >> (\f -> f cellType))
