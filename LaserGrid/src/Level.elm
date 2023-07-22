module Level exposing (..)

import Cell exposing (Cell(..))
import Dict exposing (Dict)
import Grid exposing (Grid(..))


fromInt : Int -> Grid
fromInt int =
    let
        default =
            5
    in
    case int of
        5 ->
            parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛🔘⬛⬛🔘⬛"
                ]
                |> Stage2

        4 ->
            parse
                [ "⬛⬛⬛🔘🔘⬛"
                , "🟥⬜⬜⬜⬜🟥"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🟥"
                , "⬛🔘🔘⬛⬛⬛"
                ]
                |> Stage1

        3 ->
            parse
                [ "⬛🟥🟥⬛⬛⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🔘⬜⬜⬜⬜⬛"
                , "⬛⬛⬛⬛⬛⬛"
                ]
                |> Stage1

        2 ->
            parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬜⬜⬜⬜⬛"
                , "🟥⬜⬜⬜⬜🔘"
                , "⬛🔘⬛⬛🔘⬛"
                ]
                |> Stage1

        0 ->
            parse
                [ "⬛🟥⬛⬛🟥⬛"
                , "⬛⬜⬜⬜⬜🔘"
                , "⬛⬜⬜⬜⬜⬛"
                , "⬛⬛⬛⬛⬜⬛"
                , "⬛⬛⬛⬛⬜🔘"
                , "⬛⬛⬛⬛⬛⬛"
                ]
                |> Stage1

        _ ->
            fromInt default


parse : List String -> Dict ( Int, Int ) (Cell a)
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


withLaserAt : ( Int, Int ) -> Dict ( Int, Int ) (Cell a) -> Dict ( Int, Int ) (Cell a)
withLaserAt pos =
    Dict.insert pos Laser
