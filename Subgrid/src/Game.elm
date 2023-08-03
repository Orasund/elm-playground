module Game exposing (..)

import Cell exposing (Cell(..), Connection)
import Dict exposing (Dict)
import Dir
import Level exposing (Level)
import Path
import RelativePos exposing (RelativePos)
import Set exposing (Set)
import Stage exposing (SavedStage, Stage)
import StaticArray.Index as Index


type alias Game =
    { stage : Stage
    , isConnected : Dict RelativePos (Set Int)
    }


fromStage : Stage -> Game
fromStage stage =
    { stage = stage
    , isConnected = Dict.empty
    }


isSolved : Game -> Bool
isSolved game =
    game.stage.origins
        |> List.all
            (\pos ->
                game.isConnected
                    |> Dict.get (RelativePos.fromTuple pos)
                    |> Maybe.map
                        (\s ->
                            Set.size s == 1
                        )
                    |> Maybe.withDefault False
            )


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


toSave : Level -> Game -> Maybe SavedStage
toSave level game =
    game.stage
        |> Path.build level
        |> (\list ->
                { connections =
                    list
                        |> List.indexedMap
                            (\id { from, to, path } ->
                                [ ( from
                                  , { from = to
                                    , pathId = id
                                    , path = path
                                    }
                                  )
                                , ( to
                                  , { from = from
                                    , pathId = id
                                    , path = path
                                    }
                                  )
                                ]
                            )
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
    , powerStrength : Int
    , level : Level
    }
    -> Game
    -> ( Game, Bool )
tick args game =
    game.stage.grid
        |> Dict.map
            (\pos cell ->
                case cell of
                    ConnectionCell conncetion ->
                        game.stage
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
                                    , from = relPos
                                    , to =
                                        relPos
                                            |> RelativePos.reverse args.level
                                    }
                                )
                            |> List.filter
                                (\dir ->
                                    (game.stage.origins
                                        |> List.member dir.pos
                                    )
                                        && (dir.pos
                                                |> Stage.sendsEnergy
                                                    { to = dir.to
                                                    }
                                                    game.stage
                                                |> (/=) Nothing
                                           )
                                )
                            |> (\list ->
                                    if List.length list < args.powerStrength then
                                        []

                                    else
                                        list
                               )
                            |> List.map .from
                            |> (\dir -> Target { dir = dir, id = id })

                    _ ->
                        cell
            )
        |> (\d ->
                ( { game | stage = game.stage |> (\stage -> { stage | grid = d }) }
                    |> (\g -> { g | isConnected = g.stage |> Path.build args.level |> Path.toDict })
                , Dict.toList d /= Dict.toList game.stage.grid
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

                            Just (Origin _) ->
                                Just dir

                            Just (Target _) ->
                                Just dir

                            _ ->
                                Nothing
                    )
    in
    if level == Index.first then
        tick
            { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsLv1 (neighborsDirLevel1 pos game.stage) ( pos, a )
            , level = level
            , powerStrength = 1
            }
            game

    else
        tick
            { computeActiveConnections = \( pos, a ) -> Stage.computeActiveConnectionsGeneric level modules a pos
            , level = level
            , powerStrength = 2
            }
            game


clearStage : Game -> Game
clearStage game =
    { game | stage = game.stage |> Stage.clear }
