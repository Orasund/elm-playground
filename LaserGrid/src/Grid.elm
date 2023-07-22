module Grid exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2, Connection1, Connection2, ConnectionShape)
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
    , ( 2
      , [ ( Dir.new ( -1, 0 )
          , { from = Dir.new ( 0, 1 )
            , shape = Cell.DoubleConnection
            }
          )
        , ( Dir.new ( 0, 1 )
          , { from = Dir.new ( -1, 0 )
            , shape = Cell.DoubleConnection
            }
          )
        ]
            |> Dict.fromList
      )
    , ( 3
      , [ ( Dir.new ( 1, 0 )
          , { from = Dir.new ( 0, 1 )
            , shape = Cell.DoubleConnection
            }
          )
        , ( Dir.new ( 0, 1 )
          , { from = Dir.new ( 1, 0 )
            , shape = Cell.DoubleConnection
            }
          )
        ]
            |> Dict.fromList
      )
    , ( 4
      , [ ( Dir.new ( -1, 0 )
          , { from = Dir.new ( 0, -1 )
            , shape = Cell.DoubleConnection
            }
          )
        , ( Dir.new ( 0, -1 )
          , { from = Dir.new ( -1, 0 )
            , shape = Cell.DoubleConnection
            }
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


type Grid
    = Stage1 (Dict ( Int, Int ) Cell1)
    | Stage2 (Dict ( Int, Int ) Cell2)


toDict : Grid -> Dict ( Int, Int ) (Cell ())
toDict grid =
    let
        fun : Dict ( Int, Int ) (Cell a) -> Dict ( Int, Int ) (Cell ())
        fun =
            Dict.map (\_ -> Cell.map (\_ -> ()))
    in
    case grid of
        Stage1 dict ->
            fun dict

        Stage2 dict ->
            fun dict


getEmoji : ( Int, Int ) -> Grid -> Maybe String
getEmoji pos grid =
    case grid of
        Stage1 dict ->
            Dict.get pos dict
                |> Maybe.map Cell.cell1ToEmoji

        Stage2 dict ->
            Dict.get pos dict
                |> Maybe.map Cell.cell2ToEmoji


update : Grid -> ( Grid, Bool )
update grid =
    let
        neighborsDir pos dict =
            neighboringDir
                |> List.filterMap
                    (\dir ->
                        case Dict.get (Dir.add dir pos) dict of
                            Just (Glass _) ->
                                Just dir

                            Just Laser ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )

        tick :
            { connection : ( ( Int, Int ), a ) -> Dict ( Int, Int ) (Cell a) -> a
            , sendsEnergy : ( Int, Int ) -> a -> Bool
            , toGrid : Dict ( Int, Int ) (Cell a) -> Grid
            }
            -> Dict ( Int, Int ) (Cell a)
            -> ( Grid, Bool )
        tick args dict =
            dict
                |> Dict.map
                    (\pos cell ->
                        case cell of
                            Glass a ->
                                dict
                                    |> args.connection ( pos, a )
                                    |> Glass

                            Target _ ->
                                neighboringDir
                                    |> List.any
                                        (\fromDir ->
                                            sendsEnergy args.sendsEnergy
                                                { from = Dir.add fromDir pos
                                                , pos = pos
                                                }
                                                dict
                                        )
                                    |> Target

                            _ ->
                                cell
                    )
                |> (\d ->
                        ( args.toGrid d
                        , Dict.toList d /= Dict.toList dict
                        )
                   )
    in
    case grid of
        Stage1 dict ->
            tick
                { connection = \( pos, _ ) -> connection1Tick (neighborsDir pos dict) pos
                , sendsEnergy = Cell.cell1sendsEnergyTo
                , toGrid = Stage1
                }
                dict

        Stage2 dict ->
            tick
                { connection = \( pos, a ) -> connection2Tick a.moduleId pos
                , sendsEnergy = Cell.cell2sendsEnergyTo
                , toGrid = Stage2
                }
                dict


connection2Tick :
    Int
    -> ( Int, Int )
    -> Dict ( Int, Int ) Cell2
    -> Connection2
connection2Tick moduleId pos dict =
    modules
        |> Dict.get moduleId
        |> Maybe.withDefault Dict.empty
        |> Dict.filter
            (\_ { from } ->
                sendsEnergy Cell.cell2sendsEnergyTo
                    { from = Dir.add from pos, pos = pos }
                    dict
            )
        |> Dict.keys
        |> List.map (\dir -> Dir.add dir pos)
        |> (\list -> { moduleId = moduleId, activePos = list })


connection1Tick : List Dir -> ( Int, Int ) -> Dict ( Int, Int ) Cell1 -> Connection1
connection1Tick neighborsDir pos dict =
    let
        ( x, y ) =
            pos
    in
    case neighborsDir of
        [ dir1, dir2 ] ->
            if
                sendsEnergy Cell.cell1sendsEnergyTo
                    { from = Dir.add dir1 pos
                    , pos = pos
                    }
                    dict
            then
                [ Dir.add dir2 pos ]

            else if
                sendsEnergy Cell.cell1sendsEnergyTo
                    { from = Dir.add dir2 pos
                    , pos = pos
                    }
                    dict
            then
                [ Dir.add dir1 pos ]

            else
                []

        _ ->
            neighboringDir
                |> List.filter
                    (\fromDir ->
                        sendsEnergy Cell.cell1sendsEnergyTo
                            { from = Dir.add fromDir pos
                            , pos = pos
                            }
                            dict
                    )
                |> List.map
                    (\dir ->
                        Dir.add
                            (dir |> Dir.reverse)
                            ( x, y )
                    )


sendsEnergy : (( Int, Int ) -> a -> Bool) -> { from : ( Int, Int ), pos : ( Int, Int ) } -> Dict ( Int, Int ) (Cell a) -> Bool
sendsEnergy fun args dict =
    case dict |> Dict.get args.from of
        Just (Glass to) ->
            fun args.pos to

        Just Laser ->
            True

        _ ->
            False


neighboringDir : List Dir
neighboringDir =
    [ ( -1, 0 )
    , ( 1, 0 )
    , ( 0, -1 )
    , ( 0, 1 )
    ]
        |> List.map Dir.new
