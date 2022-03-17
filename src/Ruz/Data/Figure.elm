module Ruz.Data.Figure exposing (Figure(..), asList, moves, player, toString)

import Random exposing (Generator)
import Ruz.Config as Config


type Figure
    = King
    | Rook
    | Biship
    | Pawn


asList : List Figure
asList =
    [ List.repeat 1 King, List.repeat 2 Rook, List.repeat 2 Biship, List.repeat 8 Pawn ]
        |> List.concat


player : Figure
player =
    Rook


toString : Bool -> Figure -> String
toString isWhite figure =
    case figure of
        King ->
            if isWhite then
                "♔"

            else
                "♚"

        Rook ->
            if isWhite then
                "♖"

            else
                "♜"

        Biship ->
            if isWhite then
                "♗"

            else
                "♝"

        Pawn ->
            if isWhite then
                "♙"

            else
                "♟"


moves : ( Int, Int ) -> Figure -> List ( Int, Int )
moves ( x, y ) figure =
    case figure of
        King ->
            [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ), ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]

        Rook ->
            [ ( x + 1, y ), ( x, y + 1 ), ( x - 1, y ) ]

        Biship ->
            [ ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]

        Pawn ->
            [ ( x, y + 1 ), ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]
