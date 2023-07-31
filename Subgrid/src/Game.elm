module Game exposing (..)

import Cell exposing (Cell(..), Connection)
import Config
import Dict exposing (Dict)
import Dir
import Level exposing (Level(..))
import RelativePos exposing (RelativePos)
import Stage exposing (Stage)


type alias Game =
    { stage : Stage
    }


type alias SavedStage =
    { connections :
        Dict
            RelativePos
            { from : RelativePos
            , path : List RelativePos
            }
    , grid : Dict RelativePos Cell
    , level : Level
    }


fromStage : Stage -> Game
fromStage stage =
    { stage = stage }


isSolved : Game -> Bool
isSolved game =
    Stage.isSolved game.stage


fromSave : SavedStage -> Game
fromSave savedLevel =
    { stage =
        savedLevel.grid
            |> Dict.toList
            |> List.map (\( k, v ) -> ( RelativePos.unsafeToTuple k, v ))
            |> Dict.fromList
            |> Stage.fromDict
    }


toSave : Level -> Game -> Maybe SavedStage
toSave level game =
    let
        buildPath :
            Dict ( Int, Int ) Cell
            -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
            -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ) }
        buildPath grid =
            Maybe.andThen
                (\{ pos, to, path } ->
                    grid
                        |> Dict.get pos
                        |> Maybe.andThen
                            (\cell ->
                                case cell of
                                    ConnectionCell connection ->
                                        connection.sendsTo
                                            |> Dict.toList
                                            |> List.map
                                                (\( k, v ) ->
                                                    ( k
                                                        |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                                                        |> Dir.addTo pos
                                                    , v
                                                    )
                                                )
                                            |> Dict.fromList
                                            |> Dict.get to
                                            |> Maybe.map
                                                (\{ from } ->
                                                    { pos =
                                                        from
                                                            |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                                                            |> Dir.addTo pos
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
    case level of
        Level1 ->
            game.stage.targets
                |> List.filterMap
                    (\target ->
                        List.range 0 16
                            |> List.foldl (\_ -> buildPath game.stage.grid)
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
                        { connections = dict
                        , grid =
                            game.stage.grid
                                |> Dict.toList
                                |> List.map (\( k, v ) -> ( RelativePos.fromTuple k, v ))
                                |> Dict.fromList
                        , level = level
                        }
                   )
                |> Just

        Level2 ->
            Nothing


update : Level -> Dict Int SavedStage -> Game -> ( Game, Bool )
update level modules game =
    let
        neighborsDirLevel1 pos stage =
            Dir.list
                |> List.map (Dir.addTo ( 0, 0 ))
                |> List.map RelativePos.fromTuple
                |> List.filterMap
                    (\dir ->
                        case
                            Dict.get
                                (dir
                                    |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                                    |> Dir.addTo pos
                                )
                                stage.grid
                        of
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
            { computeActiveConnections : ( ( Int, Int ), Connection ) -> Stage -> Connection
            , toGame : Stage -> Game
            , dirList : ( Int, Int ) -> List { pos : ( Int, Int ), to : RelativePos }
            , powerStrength : Int
            }
            -> Stage
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
    case level of
        Level1 ->
            tick
                { computeActiveConnections = \( pos, a ) -> computeActiveConnectionsLv1 (neighborsDirLevel1 pos game.stage) ( pos, a )
                , toGame = \stage -> { game | stage = stage }
                , dirList =
                    \p ->
                        RelativePos.list { maxPos = Config.maxPosLevel1 }
                            |> List.map
                                (\relPos ->
                                    { pos =
                                        relPos
                                            |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                                            |> Dir.addTo p
                                    , to =
                                        relPos
                                            |> RelativePos.reverse { maxPos = Config.maxPosLevel1 }
                                    }
                                )
                , powerStrength = 1
                }
                game.stage

        Level2 ->
            tick
                { computeActiveConnections = \( pos, a ) -> computeActiveConnectionsLv2 modules a pos
                , toGame = \stage -> { game | stage = stage }
                , dirList =
                    \p ->
                        RelativePos.list { maxPos = Config.maxPosLevel2 }
                            |> List.map
                                (\relPos ->
                                    { pos =
                                        relPos
                                            |> RelativePos.toDir { maxPos = Config.maxPosLevel2 }
                                            |> Dir.addTo p
                                    , to =
                                        relPos
                                            |> RelativePos.reverse { maxPos = Config.maxPosLevel2 }
                                    }
                                )
                , powerStrength = 2
                }
                game.stage


computeActiveConnectionsLv2 :
    Dict Int SavedStage
    -> Connection
    -> ( Int, Int )
    -> Stage
    -> Connection
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
                        |> RelativePos.toDir { maxPos = Config.maxPosLevel2 }
                        |> Dir.rotate connection.rotation
                        |> Dir.addTo pos
                        |> sendsEnergy
                            { to =
                                from
                                    |> RelativePos.reverse { maxPos = Config.maxPosLevel2 }
                                    |> RelativePos.rotate { maxPos = Config.maxPosLevel2 } connection.rotation
                            }
                            stage
                then
                    ( to
                        |> RelativePos.rotate { maxPos = Config.maxPosLevel2 } connection.rotation
                    , { from =
                            from
                                |> RelativePos.rotate { maxPos = Config.maxPosLevel2 } connection.rotation
                      }
                    )
                        |> Just

                else
                    Nothing
            )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


computeActiveConnectionsLv1 : List RelativePos -> ( ( Int, Int ), Connection ) -> Stage -> Connection
computeActiveConnectionsLv1 neighborsDir ( pos, connection ) stage =
    (case neighborsDir of
        [ dir1, dir2 ] ->
            if
                dir1
                    |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir1 |> RelativePos.reverse { maxPos = Config.maxPosLevel1 }
                        }
                        stage
            then
                [ ( dir2, { from = dir1 } ) ]

            else if
                dir2
                    |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir2 |> RelativePos.reverse { maxPos = Config.maxPosLevel1 }
                        }
                        stage
            then
                [ ( dir1, { from = dir2 } ) ]

            else
                []

        _ ->
            RelativePos.list { maxPos = Config.maxPosLevel1 }
                |> List.filterMap
                    (\fromDir ->
                        if
                            fromDir
                                |> RelativePos.toDir { maxPos = Config.maxPosLevel1 }
                                |> Dir.addTo pos
                                |> sendsEnergy
                                    { to = fromDir |> RelativePos.reverse { maxPos = Config.maxPosLevel1 }
                                    }
                                    stage
                        then
                            ( fromDir |> RelativePos.reverse { maxPos = Config.maxPosLevel1 }
                            , { from = fromDir }
                            )
                                |> Just

                        else
                            Nothing
                    )
    )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


connectionSendsTo : RelativePos -> Connection -> Bool
connectionSendsTo to connection =
    connection.sendsTo
        |> Dict.keys
        |> List.any ((==) to)


sendsEnergy :
    { to : RelativePos
    }
    -> Stage
    -> ( Int, Int )
    -> Bool
sendsEnergy args stage pos =
    case stage.grid |> Dict.get pos of
        Just (ConnectionCell a) ->
            connectionSendsTo args.to a

        Just Origin ->
            True

        _ ->
            False
