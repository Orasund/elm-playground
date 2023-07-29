module Game exposing (..)

import Cell exposing (Cell(..), Cell1, ConnectionSort1, ConnectionSort2)
import Dict exposing (Dict)
import Dir exposing (Dir)
import RelativePos exposing (RelativePos)
import Stage exposing (Stage)


type Game
    = Level1 (Stage ConnectionSort1)
    | Level2 (Stage ConnectionSort2)


type alias SavedLevel =
    { connections :
        Dict
            RelativePos
            { from : RelativePos
            , path : List RelativePos
            }
    , grid : Dict RelativePos Cell1
    }


isSolved : Game -> Bool
isSolved game =
    case game of
        Level1 stage ->
            Stage.isSolved stage

        Level2 stage ->
            Stage.isSolved stage


fromSave : SavedLevel -> Game
fromSave savedLevel =
    savedLevel.grid
        |> Dict.toList
        |> List.map (\( k, v ) -> ( RelativePos.unsafeToTuple k, v ))
        |> Dict.fromList
        |> Stage.fromDict
        |> Level1


toSave : Game -> Maybe SavedLevel
toSave game =
    let
        targets =
            case game of
                Level1 stage ->
                    stage.targets

                Level2 stage ->
                    stage.targets

        buildPath :
            Dict ( Int, Int ) Cell1
            -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
            -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
        buildPath grid =
            Maybe.andThen
                (\{ pos, to, path } ->
                    grid
                        |> Dict.get pos
                        |> Maybe.andThen
                            (\cell ->
                                case cell |> Debug.log "cell" of
                                    ConnectionCell connection ->
                                        connection.sendsTo
                                            |> Dict.toList
                                            |> List.map (\( k, v ) -> ( Dir.addTo pos k, v ))
                                            |> Dict.fromList
                                            |> Dict.get (Debug.log "to" to)
                                            |> Maybe.map
                                                (\{ from } ->
                                                    { pos = Dir.addTo pos from
                                                    , path = pos :: path
                                                    , to = pos
                                                    }
                                                )

                                    Origin ->
                                        Just
                                            { pos = pos
                                            , path = pos :: path
                                            , to = pos
                                            }

                                    Target maybe ->
                                        maybe
                                            |> Maybe.map
                                                (\from ->
                                                    { pos = from
                                                    , path = pos :: path
                                                    , to = pos
                                                    }
                                                )

                                    _ ->
                                        Nothing
                            )
                )
    in
    case game of
        Level1 stage ->
            targets
                |> List.filterMap
                    (\target ->
                        List.range 0 16
                            |> List.foldl (\_ -> buildPath stage.grid)
                                (Just { pos = target, to = target, path = [] })
                            |> Maybe.map
                                (\{ pos, path } ->
                                    [ ( RelativePos.fromTuple target
                                      , { from = RelativePos.fromTuple pos
                                        , path =
                                            path
                                                |> List.map RelativePos.fromTuple
                                        }
                                      )
                                    , ( RelativePos.fromTuple pos
                                      , { from = RelativePos.fromTuple target
                                        , path =
                                            path
                                                |> List.map RelativePos.fromTuple
                                        }
                                      )
                                    ]
                                )
                    )
                |> List.concat
                |> Dict.fromList
                |> (\dict ->
                        { connections = dict |> Debug.log "connections"
                        , grid =
                            stage.grid
                                |> Dict.toList
                                |> List.map (\( k, v ) -> ( RelativePos.fromTuple k, v ))
                                |> Dict.fromList
                        }
                   )
                |> Just

        Level2 _ ->
            Nothing


toDict : Game -> Dict ( Int, Int ) (Cell ())
toDict game =
    case game of
        Level1 dict ->
            Stage.toDict dict

        Level2 dict ->
            Stage.toDict dict


update : Dict Int SavedLevel -> Game -> ( Game, Bool )
update modules grid =
    let
        neighborsDir pos stage =
            Dir.list
                |> List.filterMap
                    (\dir ->
                        case Dict.get (dir |> Dir.addTo pos) stage.grid of
                            Just (ConnectionCell _) ->
                                Just dir

                            Just Origin ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )

        tick :
            { computeActiveConnections : ( ( Int, Int ), a ) -> Stage a -> a
            , toGame : Stage a -> Game
            , connectionSendsTo : dir -> a -> Bool
            , dirList : ( Int, Int ) -> List { pos : ( Int, Int ), to : dir }
            , powerStrength : Int
            }
            -> Stage a
            -> ( Game, Bool )
        tick args stage =
            stage.grid
                |> Dict.map
                    (\pos cell ->
                        case cell of
                            ConnectionCell conncetion ->
                                stage
                                    |> args.computeActiveConnections ( pos, conncetion )
                                    |> ConnectionCell

                            Target _ ->
                                args.dirList pos
                                    |> List.filter
                                        (\dir ->
                                            dir.pos
                                                |> sendsEnergy
                                                    { to = dir.to
                                                    , connectionSends = args.connectionSendsTo
                                                    }
                                                    stage
                                        )
                                    |> (\list ->
                                            if List.length list < args.powerStrength then
                                                []

                                            else
                                                list
                                       )
                                    |> List.head
                                    |> Maybe.map .pos
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
                { computeActiveConnections = \( pos, a ) -> computeActiveConnectionsLv1 (neighborsDir pos stage) ( pos, a )
                , toGame = Level1
                , connectionSendsTo = connection1SendsTo
                , dirList = \p -> Dir.list |> List.map (\dir -> { pos = dir |> Dir.addTo p, to = dir |> Dir.reverse })
                , powerStrength = 1
                }
                stage

        Level2 stage ->
            tick
                { computeActiveConnections = \( pos, a ) -> computeActiveConnectionsLv2 modules a pos
                , toGame = Level2
                , connectionSendsTo = connection2SendsTo
                , dirList =
                    \p ->
                        RelativePos.list
                            |> List.map
                                (\relPos ->
                                    { pos = relPos |> RelativePos.toDir |> Dir.addTo p
                                    , to = relPos |> RelativePos.reverse
                                    }
                                )
                , powerStrength = 2
                }
                stage


computeActiveConnectionsLv2 :
    Dict Int SavedLevel
    -> ConnectionSort2
    -> ( Int, Int )
    -> Stage ConnectionSort2
    -> ConnectionSort2
computeActiveConnectionsLv2 modules connection pos stage =
    modules
        |> Dict.get connection.moduleId
        |> Maybe.map .connections
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( to, { from } ) ->
                if
                    from
                        |> RelativePos.toDir
                        |> Dir.rotate connection.rotation
                        |> Dir.addTo pos
                        |> sendsEnergy
                            { to =
                                from
                                    |> RelativePos.reverse
                                    |> RelativePos.rotate connection.rotation
                            , connectionSends = connection2SendsTo
                            }
                            stage
                then
                    ( to
                        |> RelativePos.rotate connection.rotation
                    , { from =
                            from
                                |> RelativePos.rotate connection.rotation
                      }
                    )
                        |> Just

                else
                    Nothing
            )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


computeActiveConnectionsLv1 : List Dir -> ( ( Int, Int ), ConnectionSort1 ) -> Stage ConnectionSort1 -> ConnectionSort1
computeActiveConnectionsLv1 neighborsDir ( pos, connection ) stage =
    (case neighborsDir of
        [ dir1, dir2 ] ->
            if
                dir1
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir1 |> Dir.reverse
                        , connectionSends = connection1SendsTo
                        }
                        stage
            then
                [ ( dir2, { from = dir1 } ) ]

            else if
                dir2
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir2 |> Dir.reverse
                        , connectionSends = connection1SendsTo
                        }
                        stage
            then
                [ ( dir1, { from = dir2 } ) ]

            else
                []

        _ ->
            Dir.list
                |> List.filterMap
                    (\fromDir ->
                        if
                            fromDir
                                |> Dir.addTo pos
                                |> sendsEnergy
                                    { to = fromDir |> Dir.reverse
                                    , connectionSends = connection1SendsTo
                                    }
                                    stage
                        then
                            ( fromDir |> Dir.reverse
                            , { from = fromDir }
                            )
                                |> Just

                        else
                            Nothing
                    )
    )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


connection2SendsTo : RelativePos -> ConnectionSort2 -> Bool
connection2SendsTo to connection =
    connection.sendsTo
        |> Dict.keys
        |> List.any
            (\relPos ->
                relPos
                    |> (==) to
            )


connection1SendsTo : Dir -> ConnectionSort1 -> Bool
connection1SendsTo to connection =
    connection.sendsTo
        |> Dict.keys
        |> List.any
            (\dir ->
                dir == to
            )


sendsEnergy :
    { connectionSends : dir -> a -> Bool
    , to : dir
    }
    -> Stage a
    -> ( Int, Int )
    -> Bool
sendsEnergy args stage pos =
    case stage.grid |> Dict.get pos of
        Just (ConnectionCell a) ->
            args.connectionSends args.to a

        Just Origin ->
            True

        _ ->
            False
