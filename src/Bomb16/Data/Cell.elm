module Bomb16.Data.Cell exposing (Cell(..), generate, order, toString)

import Random exposing (Generator)


type Cell
    = Effect String
    | Sword Int
    | Monster Int
    | Wall Int
    | Bomb Int


generate : Generator Cell
generate =
    Random.weighted ( 1, Wall 1 )
        [ ( 1, Bomb 2 )
        , ( 8, Monster 1 )
        , ( 8, Monster 2 )
        ]


order : Cell -> Int
order cell =
    case cell of
        Effect _ ->
            0

        Sword _ ->
            1

        Monster _ ->
            2

        Wall _ ->
            3

        Bomb _ ->
            4


toString : Cell -> ( String, String )
toString cell =
    case cell of
        Effect string ->
            ( string, "" )

        Monster int ->
            ( String.fromInt int, "" )

        Sword int ->
            ( String.fromInt int, "🗡" )

        Wall int ->
            if int == 1 then
                ( "\u{1F9F1}", "" )

            else
                ( "x" ++ String.fromInt int, "\u{1F9F1}" )

        Bomb int ->
            if int == 1 then
                ( "💣", "" )

            else
                ( "x" ++ String.fromInt int, "💣" )
