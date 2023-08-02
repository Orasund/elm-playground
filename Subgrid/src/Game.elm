module Game exposing (..)

import Cell exposing (Cell(..), Connection)
import Dict exposing (Dict)
import Dir
import Level exposing (Level(..))
import RelativePos exposing (RelativePos)
import Set exposing (Set)
import Stage exposing (SavedStage, Stage)


type alias Game =
    { stage : Stage
    , isConnected : Dict RelativePos (Set Int)
    }


type alias PathBuilder =
    List
        { from : RelativePos
        , to : RelativePos
        , path : List RelativePos
        , id : Int
        }


fromStage : Stage -> Game
fromStage stage =
    { stage = stage
    , isConnected = Dict.empty
    }


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
    , isConnected = savedLevel.paths
    }


buildPath :
    Level
    -> Dict ( Int, Int ) Cell
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ), id : Int }
    -> Maybe { pos : ( Int, Int ), to : ( Int, Int ), path : List ( Int, Int ), id : Int }
buildPath level grid =
    Maybe.andThen
        (\{ pos, to, path, id } ->
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
                                                |> RelativePos.toDir level
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
                                                    |> RelativePos.toDir level
                                                    |> Dir.addTo pos
                                            , path = pos :: path
                                            , to = pos
                                            , id = id
                                            }
                                        )

                            Origin ->
                                Just
                                    { pos = pos
                                    , path = pos :: path
                                    , to = pos
                                    , id = id
                                    }

                            Target target ->
                                target.dir
                                    |> Maybe.map
                                        (\from ->
                                            { pos = from
                                            , path = pos :: path
                                            , to = pos
                                            , id = target.id
                                            }
                                        )

                            _ ->
                                Nothing
                    )
        )


reevaluatePaths : PathBuilder -> Dict RelativePos (Set Int)
reevaluatePaths list =
    list
        |> List.foldl
            (\{ path, id } d ->
                path
                    |> List.foldl
                        (\pos ->
                            Dict.update pos
                                (\maybe ->
                                    (case maybe of
                                        Nothing ->
                                            Set.singleton id

                                        Just l ->
                                            Set.insert id l
                                    )
                                        |> Just
                                )
                        )
                        d
            )
            Dict.empty


buildPaths : Level -> Game -> PathBuilder
buildPaths level game =
    game.stage.targets
        |> List.filterMap
            (\target ->
                List.range 0 16
                    |> List.foldl (\_ -> buildPath level game.stage.grid)
                        (Just { pos = target, to = target, path = [], id = 0 })
                    |> Maybe.map
                        (\{ pos, path, id } ->
                            { from = RelativePos.fromTuple target
                            , to = RelativePos.fromTuple pos
                            , path = path |> List.map RelativePos.fromTuple
                            , id = id
                            }
                        )
            )


toSave : Level -> Game -> Maybe SavedStage
toSave level game =
    game
        |> buildPaths level
        |> (\list ->
                { connections =
                    list
                        |> List.indexedMap (\id { from, to } -> [ ( from, { from = to, pathId = id } ), ( to, { from = from, pathId = id } ) ])
                        |> List.concat
                        |> Dict.fromList
                , paths = game.isConnected
                , grid =
                    game.stage.grid
                        |> Dict.toList
                        |> List.map (\( k, v ) -> ( RelativePos.fromTuple k, v ))
                        |> Dict.fromList
                , level = level
                }
           )
        |> Just


tick :
    { computeActiveConnections : ( ( Int, Int ), Connection ) -> Stage -> Connection
    , toGame : Stage -> Game
    , powerStrength : Int
    , level : Level
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

                    Target { id } ->
                        RelativePos.list args.level
                            |> List.map
                                (\relPos ->
                                    { pos =
                                        relPos
                                            |> RelativePos.toDir args.level
                                            |> Dir.addTo pos
                                    , to =
                                        relPos
                                            |> RelativePos.reverse args.level
                                    }
                                )
                            |> List.filter
                                (\dir ->
                                    (stage.grid
                                        |> Dict.get dir.pos
                                        |> (/=) (Just Origin)
                                    )
                                        && (dir.pos
                                                |> Stage.sendsEnergy
                                                    { to = dir.to
                                                    }
                                                    stage
                                           )
                                )
                            |> (\list ->
                                    if List.length list < args.powerStrength then
                                        []

                                    else
                                        list
                               )
                            |> List.head
                            |> Maybe.map .pos
                            |> (\dir -> Target { dir = dir, id = id })

                    _ ->
                        cell
            )
        |> (\d ->
                ( args.toGame { stage | grid = d }
                    |> (\g -> { g | isConnected = g |> buildPaths args.level |> reevaluatePaths })
                , Dict.toList d /= Dict.toList stage.grid
                )
           )


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
                                    |> RelativePos.toDir level
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
    in
    case level of
        Level1 ->
            tick
                { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv1 (neighborsDirLevel1 pos game.stage) ( pos, a )
                , toGame = \stage -> { game | stage = stage }
                , level = level
                , powerStrength = 1
                }
                game.stage

        Level2 ->
            tick
                { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv2 modules a pos
                , toGame = \stage -> { game | stage = stage }
                , level = level
                , powerStrength = 2
                }
                game.stage

        Level3 ->
            tick
                { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv3 modules a pos
                , toGame = \stage -> { game | stage = stage }
                , level = level
                , powerStrength = 1
                }
                game.stage

        Level4 ->
            tick
                { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv4 modules a pos
                , toGame = \stage -> { game | stage = stage }
                , level = level
                , powerStrength = 1
                }
                game.stage
