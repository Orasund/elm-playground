module LittleWorldPuzzler.Automata.Rule exposing (rules)

import CellAutomata exposing (Rule)
import LittleWorldPuzzler.Automata.Neighborhood as Neighborhood
import LittleWorldPuzzler.Data.CellType exposing (CellType(..))


rule : { from : Maybe CellType, to : Maybe CellType } -> List ( Int, Maybe CellType ) -> Rule CellType
rule { from, to } list =
    { from = from, to = to, neighbors = Neighborhood.fromList list }


rules : CellType -> List (Rule CellType)
rules cellType =
    case cellType of
        Wood ->
            [ rule { from = Just Wood, to = Just Fire } [ ( 1, Just Fire ) ]
            , rule { from = Just Wood, to = Just Evergreen } [ ( 1, Just Ice ) ] --new
            ]

        Water ->
            [ rule { from = Just Water, to = Nothing } [ ( 2, Just Fire ) ]
            , rule { from = Just Water, to = Just Ice } [ ( 1, Just Ice ) ]
            , rule { from = Nothing, to = Just Wood } [ ( 1, Just Water ) ]
            ]

        Fire ->
            [ rule { from = Just Fire, to = Just Volcano } [ ( 2, Just Stone ) ]
            , rule { from = Just Fire, to = Nothing } []
            , rule { from = Nothing, to = Just Desert } [ ( 3, Just Fire ) ]
            ]

        Stone ->
            [ rule { from = Just Stone, to = Just Glacier } [ ( 2, Just Stone ) ]
            , rule { from = Just Stone, to = Nothing } [ ( 1, Just Volcano ), ( 1, Just Water ) ]
            ]

        Volcano ->
            [ rule { from = Just Volcano, to = Just Volcano } [ ( 2, Just Stone ) ]
            , rule { from = Just Volcano, to = Just Fog } []
            , rule { from = Nothing, to = Just Fog } [ ( 1, Just Volcano ) ]
            ]

        Fog ->
            [ rule { from = Just Fog, to = Just Water } [ ( 1, Just Water ) ]
            , rule { from = Just Fog, to = Just Fire } [ ( 1, Just Wood ) ]
            ]

        Desert ->
            [ rule { from = Just Desert, to = Nothing } [ ( 2, Just Water ) ]
            ]

        Glacier ->
            [ rule { from = Just Glacier, to = Just Glacier } [ ( 2, Just Stone ) ]
            , rule { from = Just Glacier, to = Nothing } []
            , rule { from = Nothing, to = Just Ice } [ ( 1, Just Glacier ) ]
            ]

        Ice ->
            [ rule { from = Just Ice, to = Nothing } []
            , rule { from = Nothing, to = Just Snow } [ ( 2, Just Ice ) ]
            ]

        Snow ->
            [ rule { from = Just Snow, to = Nothing } [ ( 2, Just Fire ) ]
            ]

        Evergreen ->
            --new
            [ rule { from = Just Evergreen, to = Nothing } [ ( 1, Just Fire ) ]
            ]
