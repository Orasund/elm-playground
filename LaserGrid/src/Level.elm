module Level exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)


empty : Dict ( Int, Int ) Cell
empty =
    parse
        [ "⬛⬛⬛⬛⬛⬛"
        , "⬛⬜️⬜️⬜️⬜️⬛"
        , "⬛⬜️⬜️⬜️⬜️⬛"
        , "⬛⬜️⬜️⬜️⬜️⬛"
        , "⬛⬜️⬜️⬜️⬜️⬛"
        , "⬛⬛⬛⬛⬛⬛"
        ]


fromInt : Int -> Dict ( Int, Int ) Cell
fromInt int =
    case int of
        4 ->
            parse
                [ "⬛⬛⬛🔘🔘⬛"
                , "🟥⬜⬜⬜⬜🟥"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🟥"
                , "⬛🔘🔘⬛⬛⬛"
                ]

        3 ->
            parse
                [ "⬛🟥🟥⬛⬛⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬛⬛⬛⬛⬛"
                ]

        2 ->
            parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛🔘⬛⬛🔘⬛"
                ]

        _ ->
            parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "⬛⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬛⬛⬛⬜⬛"
                , "⬛⬛⬛⬛⬜🔘"
                , "⬛⬛⬛⬛⬛⬛"
                ]


parse : List String -> Dict ( Int, Int ) Cell
parse rows =
    rows
        |> List.indexedMap
            (\y string ->
                string
                    |> String.toList
                    |> List.indexedMap
                        (\x char ->
                            let
                                pos =
                                    ( x - 1, y - 1 )
                            in
                            case char of
                                '🔲' ->
                                    Just ( pos, Glass [] )

                                '🟥' ->
                                    Just ( pos, Laser )

                                '🔘' ->
                                    Just ( pos, Target False )

                                '⬛' ->
                                    Just ( pos, Wall )

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.concat
        |> Dict.fromList


withLaserAt : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
withLaserAt pos =
    Dict.insert pos Laser
