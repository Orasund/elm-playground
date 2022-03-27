module Zess.Data.Figure exposing (Figure(..), FigureId, asList, moves, player, score, toString)

import Random exposing (Generator)
import Zess.Config as Config


type alias FigureId =
    Int


type Figure
    = King
    | Rook
    | Biship
    | Knight
    | Pawn
    | Queen


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

        Queen ->
            9


asList : List Figure
asList =
    [ List.repeat 1 Queen
    , List.repeat 1 King
    , List.repeat 2 Rook
    , List.repeat 2 Biship
    , List.repeat 2 Knight
    , List.repeat 8 Pawn
    ]
        |> List.concat


{-|

  - Queen gives you Option parallize
  - King seems as capable as Rook, but has no way to flee out of a bad situation
  - Biship can't reach all fields.

-}
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
                "king_black"

        Rook ->
            if isWhite then
                "rook_white"

            else
                "rook_black"

        Biship ->
            if isWhite then
                "♗"

            else
                "biship_black"

        Knight ->
            if isWhite then
                "♘"

            else
                "knight_black"

        Pawn ->
            if isWhite then
                "♙"

            else
                "pawn_black"

        Queen ->
            if isWhite then
                "♕"

            else
                "queen_black"


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
            [ List.repeat 2 ( x + 2, y + 1 )
            , List.repeat 2 ( x - 2, y + 1 )
            , [ ( x + 1, y + 2 ), ( x - 1, y + 2 ) ]
            ]
                |> List.concat

        Pawn ->
            [ ( x, y + 1 ), ( x + 1, y + 1 ), ( x - 1, y + 1 ) ]

        Queen ->
            [ ( x + 1, y + 1 )
            , ( x - 1, y + 1 )
            , ( x + 1, y )
            , ( x, y + 1 )
            , ( x - 1, y )
            ]
