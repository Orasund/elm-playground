module Grid exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2, Connection, ConnectionSort1, ConnectionSort2)
import Dict exposing (Dict)
import Dir exposing (Dir)
import RelativePos exposing (RelativePos)


type alias Module =
    Dict
        RelativePos
        { from : RelativePos
        }


toModule : Grid -> Module
toModule grid =
    Debug.todo "implement toModule"


modules : Dict Int Module
modules =
    [ ( 1
      , [ ( RelativePos.fromTuple ( 0, -1 )
          , { from = RelativePos.fromTuple ( 4, 0 )
            }
          )
        , ( RelativePos.fromTuple ( 4, 0 )
          , { from = RelativePos.fromTuple ( 0, -1 )
            }
          )
        , ( RelativePos.fromTuple ( 3, -1 )
          , { from = RelativePos.fromTuple ( 4, 3 )
            }
          )
        , ( RelativePos.fromTuple ( 4, 3 )
          , { from = RelativePos.fromTuple ( 3, -1 )
            }
          )
        ]
            |> Dict.fromList
      )
    ]
        |> Dict.fromList


type Grid
    = Level1 (Dict ( Int, Int ) Cell1)
    | Level2 (Dict ( Int, Int ) Cell2)


toDict : Grid -> Dict ( Int, Int ) (Cell ())
toDict grid =
    let
        fun : Dict ( Int, Int ) (Cell a) -> Dict ( Int, Int ) (Cell ())
        fun =
            Dict.map (\_ -> Cell.map (\_ -> ()))
    in
    case grid of
        Level1 dict ->
            fun dict

        Level2 dict ->
            fun dict


update : Grid -> ( Grid, Bool )
update grid =
    let
        neighborsDir pos dict =
            Dir.list
                |> List.filterMap
                    (\dir ->
                        case Dict.get (dir |> Dir.add pos) dict of
                            Just (ConnectionCell _) ->
                                Just dir

                            Just Laser ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )

        tick :
            { connection : ( ( Int, Int ), Connection a ) -> Dict ( Int, Int ) (Cell a) -> List ( Int, Int )
            , toGrid : Dict ( Int, Int ) (Cell a) -> Grid
            }
            -> Dict ( Int, Int ) (Cell a)
            -> ( Grid, Bool )
        tick args dict =
            dict
                |> Dict.map
                    (\pos cell ->
                        case cell of
                            ConnectionCell a ->
                                { a
                                    | active = dict |> args.connection ( pos, a )
                                }
                                    |> ConnectionCell

                            Target _ ->
                                Dir.list
                                    |> List.any
                                        (\fromDir ->
                                            sendsEnergy
                                                { from = fromDir |> Dir.add pos
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
        Level1 dict ->
            tick
                { connection = \( pos, _ ) -> computeActiveConnectionsLv1 (neighborsDir pos dict) pos
                , toGrid = Level1
                }
                dict

        Level2 dict ->
            tick
                { connection = \( pos, a ) -> computeActiveConnectionsLv2 a.sort pos
                , toGrid = Level2
                }
                dict


computeActiveConnectionsLv2 :
    ConnectionSort2
    -> ( Int, Int )
    -> Dict ( Int, Int ) Cell2
    -> List ( Int, Int )
computeActiveConnectionsLv2 { moduleId, rotation } pos dict =
    modules
        |> Dict.get moduleId
        |> Maybe.withDefault Dict.empty
        |> Dict.filter
            (\_ { from } ->
                sendsEnergy
                    { from =
                        from
                            |> RelativePos.rotate rotation
                            |> RelativePos.add pos
                    , pos = pos
                    }
                    dict
            )
        |> Dict.keys
        |> List.map
            (\dir ->
                dir
                    |> RelativePos.rotate rotation
                    |> RelativePos.add pos
            )


computeActiveConnectionsLv1 : List Dir -> ( Int, Int ) -> Dict ( Int, Int ) Cell1 -> List ( Int, Int )
computeActiveConnectionsLv1 neighborsDir pos dict =
    let
        ( x, y ) =
            pos
    in
    case neighborsDir of
        [ dir1, dir2 ] ->
            if
                sendsEnergy
                    { from = dir1 |> Dir.add pos
                    , pos = pos
                    }
                    dict
            then
                [ dir2 |> Dir.add pos ]

            else if
                sendsEnergy
                    { from = dir2 |> Dir.add pos
                    , pos = pos
                    }
                    dict
            then
                [ dir1 |> Dir.add pos ]

            else
                []

        _ ->
            Dir.list
                |> List.filter
                    (\fromDir ->
                        sendsEnergy
                            { from = fromDir |> Dir.add pos
                            , pos = pos
                            }
                            dict
                    )
                |> List.map
                    (\dir ->
                        dir
                            |> Dir.reverse
                            |> Dir.add
                                ( x, y )
                    )


sendsEnergy : { from : ( Int, Int ), pos : ( Int, Int ) } -> Dict ( Int, Int ) (Cell a) -> Bool
sendsEnergy args dict =
    case dict |> Dict.get args.from of
        Just (ConnectionCell to) ->
            Cell.connectionSendsEnergyTo args.pos to

        Just Laser ->
            True

        _ ->
            False
