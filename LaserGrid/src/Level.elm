module Level exposing (..)

import Cell exposing (Cell(..), ConnectionShape)
import Dict exposing (Dict)
import Dir exposing (Dir)


type alias Module =
    Dict
        Dir
        { from : Dir
        , shape : ConnectionShape
        }


modules : Dict Int Module
modules =
    [ ( 1
      , [ ( Dir.new ( 1, 0 )
          , { from = Dir.new ( 0, -1 )
            , shape = Cell.DoubleConnection
            }
          )
        , ( Dir.new ( 0, -1 )
          , { from = Dir.new ( 1, 0 )
            , shape = Cell.DoubleConnection
            }
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


fromInt : Int -> Dict ( Int, Int ) (Cell a)
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
