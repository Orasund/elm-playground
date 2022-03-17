module Ruz.Data.Figure exposing (Figure(..), asList, moves, player, score, toString)

import Random exposing (Generator)
import Ruz.Config as Config


type Figure
    = King
    | Rook
    | Biship
    | Knight
    | Pawn


score : Figure -> Int
score figure =
    case figure of
        Pawn ->
            1

        Knight ->
            3

        Biship ->
            3

        Rook ->
            5

        King ->
            10


asList : List Figure
asList =
    [ List.repeat 1 King
    , List.repeat 2 Rook
    , List.repeat 2 Biship
    , List.repeat 2 Knight
    , List.repeat 8 Pawn
    ]
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

        Knight ->
            if isWhite then
                "♘"

            else
                "♞"

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

        Knight ->
            [ ( x + 2, y + 1 ), ( x + 1, y + 2 ), ( x - 1, y + 2 ), ( x - 2, y + 1 ) ]

        Pawn ->
            [ ( x, y + 1 ), ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]
