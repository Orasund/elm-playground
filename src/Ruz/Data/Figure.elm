module Ruz.Data.Figure exposing (Figure(..), asList, moves, player, toString)

import Random exposing (Generator)
import Ruz.Config as Config


type Figure
    = King
    | Rook
    | Biship


asList : List Figure
asList =
    [ King, Rook, Rook, Biship, Biship ]


player : Figure
player =
    King


toString : Figure -> String
toString figure =
    case figure of
        King ->
            "♚"

        Rook ->
            "♜"

        Biship ->
            "♝"


moves : ( Int, Int ) -> Figure -> List ( Int, Int )
moves ( x, y ) figure =
    case figure of
        King ->
            [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ), ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]

        Rook ->
            [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ) ]

        Biship ->
            [ ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]
