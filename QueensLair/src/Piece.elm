module Piece exposing (..)

import Config
import Set exposing (Set)


type Piece
    = King
    | Rook
    | Bishop
    | Knight
    | Pawn


list : List Piece
list =
    [ King, Bishop, Knight, Pawn ]


name : Piece -> String
name piece =
    case piece of
        King ->
            "King"

        Rook ->
            "Rook"

        Bishop ->
            "Bishop"

        Knight ->
            "Knight"

        Pawn ->
            "Pawn"


movement : Piece -> Set ( Int, Int )
movement piece =
    case piece of
        King ->
            [ ( -1, 0 )
            , ( -1, -1 )
            , ( 0, -1 )
            , ( 1, -1 )
            , ( 1, 0 )
            , ( 1, 1 )
            , ( 0, 1 )
            , ( -1, 1 )
            ]
                |> Set.fromList

        Rook ->
            List.range 1 (Config.boardSize - 2)
                |> List.concatMap
                    (\i ->
                        [ ( 0, i ), ( i, 0 ) ]
                    )
                |> Set.fromList

        Bishop ->
            List.range 1 (Config.boardSize - 2)
                |> List.concatMap
                    (\i ->
                        [ ( i, i ), ( -i, i ), ( i, -i ), ( -i, -i ) ]
                    )
                |> Set.fromList

        Knight ->
            [ ( 2, 1 )
            , ( 2, -1 )
            , ( -2, 1 )
            , ( -2, -1 )
            , ( 1, 2 )
            , ( -1, 2 )
            , ( 1, -2 )
            , ( -1, -2 )
            ]
                |> Set.fromList

        Pawn ->
            [ ( 0, 1 )
            , ( 0, -1 )
            , ( 1, 1 )
            , ( -1, 1 )
            , ( 1, -1 )
            , ( -1, -1 )
            ]
                |> Set.fromList


value : Piece -> Int
value piece =
    let
        muliplier =
            1

        --Config.boardSize * 2
    in
    muliplier
        * (case piece of
            King ->
                10

            Rook ->
                5

            Bishop ->
                3

            Knight ->
                2

            Pawn ->
                1
          )


promote : Piece -> Maybe Piece
promote piece =
    case piece of
        Pawn ->
            Just Knight

        Knight ->
            Just Bishop

        Bishop ->
            Nothing

        Rook ->
            Nothing

        King ->
            Nothing
