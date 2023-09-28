module Piece exposing (..)

import Set exposing (Set)


type Piece
    = King


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


value : Piece -> Int
value piece =
    case piece of
        King ->
            1
