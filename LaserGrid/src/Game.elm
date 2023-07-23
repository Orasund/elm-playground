module Game exposing (..)

import Cell exposing (Cell(..), Cell1, Cell2, Connection, ConnectionSort1, ConnectionSort2)
import Dict exposing (Dict)
import Dir exposing (Dir)
import RelativePos exposing (RelativePos)
import Stage exposing (Stage)


type Game
    = Level1 (Stage ConnectionSort1)
    | Level2 (Stage ConnectionSort2)


type alias Module =
    Dict
        RelativePos
        { from : RelativePos
        }


isSolved : Game -> Bool
isSolved game =
    case game of
        Level1 stage ->
            Stage.isSolved stage

        Level2 stage ->
            Stage.isSolved stage


toModule : Game -> Module
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


toDict : Game -> Dict ( Int, Int ) (Cell ())
toDict game =
    case game of
        Level1 dict ->
            Stage.toDict dict

        Level2 dict ->
            Stage.toDict dict


update : Game -> ( Game, Bool )
update grid =
    let
        neighborsDir pos stage =
            Dir.list
                |> List.filterMap
                    (\dir ->
                        case Dict.get (dir |> Dir.add pos) stage.grid of
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
            { connection : ( ( Int, Int ), Connection a ) -> Stage a -> List ( Int, Int )
            , toGame : Stage a -> Game
            }
            -> Stage a
            -> ( Game, Bool )
        tick args stage =
            stage.grid
                |> Dict.map
                    (\pos cell ->
                        case cell of
                            ConnectionCell a ->
                                { a
                                    | active = stage |> args.connection ( pos, a )
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
                                                stage
                                        )
                                    |> Target

                            _ ->
                                cell
                    )
                |> (\d ->
                        ( args.toGame { stage | grid = d }
                        , Dict.toList d /= Dict.toList stage.grid
                        )
                   )
    in
    case grid of
        Level1 stage ->
            tick
                { connection = \( pos, _ ) -> computeActiveConnectionsLv1 (neighborsDir pos stage) pos
                , toGame = Level1
                }
                stage

        Level2 stage ->
            tick
                { connection = \( pos, a ) -> computeActiveConnectionsLv2 a.sort pos
                , toGame = Level2
                }
                stage


computeActiveConnectionsLv2 :
    ConnectionSort2
    -> ( Int, Int )
    -> Stage ConnectionSort2
    -> List ( Int, Int )
computeActiveConnectionsLv2 { moduleId, rotation } pos stage =
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
                    stage
            )
        |> Dict.keys
        |> List.map
            (\dir ->
                dir
                    |> RelativePos.rotate rotation
                    |> RelativePos.add pos
            )


computeActiveConnectionsLv1 : List Dir -> ( Int, Int ) -> Stage ConnectionSort1 -> List ( Int, Int )
computeActiveConnectionsLv1 neighborsDir pos stage =
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
                    stage
            then
                [ dir2 |> Dir.add pos ]

            else if
                sendsEnergy
                    { from = dir2 |> Dir.add pos
                    , pos = pos
                    }
                    stage
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
                            stage
                    )
                |> List.map
                    (\dir ->
                        dir
                            |> Dir.reverse
                            |> Dir.add
                                ( x, y )
                    )


sendsEnergy : { from : ( Int, Int ), pos : ( Int, Int ) } -> Stage a -> Bool
sendsEnergy args stage =
    case stage.grid |> Dict.get args.from of
        Just (ConnectionCell to) ->
            Cell.connectionSendsEnergyTo args.pos to

        Just Laser ->
            True

        _ ->
            False
