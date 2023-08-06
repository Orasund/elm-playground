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
    , isConnected : Dict RelativePos { targetIds : Set Int }
    }


fromStage : Stage -> Game
fromStage stage =
    { stage = stage
    , isConnected = Dict.empty
    }


isSolved : Game -> Bool
isSolved game =
    game.stage.targets
        |> Dict.toList
        |> List.all
            (\( pos, _ ) ->
                case game.stage.grid |> Dict.get pos of
                    Just (Target target) ->
                        target.from
                            |> Dict.values
                            |> List.map .originId
                            |> Set.fromList
                            |> Set.size
                            |> (==) 1

                    _ ->
                        False
            )


fromSave : SavedStage -> Game
fromSave savedLevel =
    savedLevel.grid
        |> Dict.toList
        |> List.map (Tuple.mapFirst RelativePos.unsafeToTuple)
        |> Dict.fromList
        |> Stage.fromDict
        |> fromStage


toSave : Level -> Game -> Maybe SavedStage
toSave level game =
    let
        list =
            game.stage
                |> Path.build level

        connections :
            Dict
                RelativePos
                { from : RelativePos
                , originId : Int
                , path : List RelativePos
                }
        connections =
            list
                |> List.map
                    (\{ from, to, path, originId } ->
                        [ ( from
                          , { from = to
                            , originId = originId
                            , path = path
                            }
                          )
                        , ( to
                          , { from = from
                            , originId = originId
                            , path = path
                            }
                          )
                        ]
                    )
                |> List.concat
                |> Dict.fromList

        paths : Dict RelativePos { origins : Set Int }
        paths =
            list
                |> List.foldl
                    (\{ path, originId } d ->
                        path
                            |> List.foldl
                                (\pos ->
                                    Dict.update pos
                                        (\maybe ->
                                            { origins =
                                                maybe
                                                    |> Maybe.map (\{ origins } -> origins |> Set.insert originId)
                                                    |> Maybe.withDefault (Set.singleton originId)
                                            }
                                                |> Just
                                        )
                                )
                                d
                    )
                    Dict.empty

        grid : Dict RelativePos Cell
        grid =
            game.stage.grid
                |> Dict.toList
                |> List.map (\( k, v ) -> ( RelativePos.fromTuple k, v ))
                |> Dict.fromList
    in
    { connections = connections
    , paths = paths
    , grid = grid
    , level = level
    }
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
                            |> List.filterMap
                                (\dir ->
                                    if
                                        game.stage.origins
                                            |> Dict.member dir.pos
                                    then
                                        Nothing

                                    else
                                        dir.pos
                                            |> Stage.sendsEnergy
                                                { to = dir.to
                                                }
                                                game.stage
                                            |> Maybe.map
                                                (\{ originId } ->
                                                    ( dir.from, { originId = originId } )
                                                )
                                )
                            |> (\list ->
                                    if List.length list < args.powerStrength then
                                        []

                                    else
                                        list
                               )
                            |> Dict.fromList
                            |> (\from -> Target { from = from, id = id })

                    _ ->
                        cell
            )
        |> (\d ->
                ( { game | stage = game.stage |> (\stage -> { stage | grid = d }) }
                    |> (\g -> { g | isConnected = g.stage |> Path.build args.level |> Path.toDict |> Debug.log "isConnected" })
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
