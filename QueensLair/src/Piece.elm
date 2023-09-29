module Piece exposing (..)

import Config
import Set exposing (Set)


type Piece
    = King
    | Bishop
    | Knight
    | Pawn


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

        Bishop ->
            List.range 1 (Config.boardSize - 1)
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
                5

            Bishop ->
                3

            Knight ->
                2

            Pawn ->
                1
          )
