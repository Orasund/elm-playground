module Stage exposing (..)

import Cell exposing (Cell(..), Connection)
import Connection
import Dict exposing (Dict)
import Dir
import Level exposing (Level(..))
import RelativePos exposing (RelativePos)
import Set exposing (Set)


type alias Stage =
    { grid : Dict ( Int, Int ) Cell
    , targets : List ( Int, Int )
    }


type alias SavedStage =
    { connections :
        Dict
            RelativePos
            { from : RelativePos
            , pathId : Int
            }
    , paths : Dict RelativePos (Set Int)
    , grid : Dict RelativePos Cell
    , level : Level
    }


isSolved : Stage -> Bool
isSolved stage =
    List.all
        (\pos ->
            case stage.grid |> Dict.get pos of
                Just (Target (Just _)) ->
                    True

                _ ->
                    False
        )
        stage.targets


fromDict : Dict ( Int, Int ) Cell -> Stage
fromDict dict =
    { grid = dict
    , targets =
        dict
            |> Dict.filter
                (\_ cell ->
                    case cell of
                        Target _ ->
                            True

                        _ ->
                            False
                )
            |> Dict.keys
    }


parse : List String -> Stage
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
                                    Just ( pos, Origin )

                                '🔘' ->
                                    Just ( pos, Target Nothing )

                                '⬛' ->
                                    Just ( pos, Wall )

                                _ ->
                                    Nothing
                        )
                    |> List.filterMap identity
            )
        |> List.concat
        |> Dict.fromList
        |> fromDict


withLaserAt : ( Int, Int ) -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
withLaserAt pos =
    Dict.insert pos Origin


computeActiveConnectionsLv3 :
    Dict Int SavedStage
    -> Connection
    -> ( Int, Int )
    -> Stage
    -> Connection
computeActiveConnectionsLv3 modules connection pos stage =
    let
        level =
            Level3
    in
    modules
        |> Dict.get connection.moduleId
        |> Maybe.map .connections
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( to, { from } ) ->
                if
                    from
                        |> RelativePos.toDir level
                        |> Dir.rotate connection.rotation
                        |> Dir.addTo pos
                        |> sendsEnergy
                            { to =
                                from
                                    |> RelativePos.reverse level
                                    |> RelativePos.rotate level connection.rotation
                            }
                            stage
                then
                    ( to
                        |> RelativePos.rotate level connection.rotation
                    , { from =
                            from
                                |> RelativePos.rotate level connection.rotation
                      }
                    )
                        |> Just

                else
                    Nothing
            )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


computeActiveConnectionsLv2 :
    Dict Int SavedStage
    -> Connection
    -> ( Int, Int )
    -> Stage
    -> Connection
computeActiveConnectionsLv2 modules connection pos stage =
    let
        level =
            Level2
    in
    modules
        |> Dict.get connection.moduleId
        |> Maybe.map .connections
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( to, { from } ) ->
                if
                    from
                        |> RelativePos.toDir level
                        |> Dir.rotate connection.rotation
                        |> Dir.addTo pos
                        |> sendsEnergy
                            { to =
                                from
                                    |> RelativePos.reverse level
                                    |> RelativePos.rotate level connection.rotation
                            }
                            stage
                then
                    ( to
                        |> RelativePos.rotate level connection.rotation
                    , { from =
                            from
                                |> RelativePos.rotate level connection.rotation
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
    let
        level =
            Level1
    in
    (case neighborsDir of
        [ dir1, dir2 ] ->
            if
                dir1
                    |> RelativePos.toDir level
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir1 |> RelativePos.reverse level
                        }
                        stage
            then
                [ ( dir2, { from = dir1 } ) ]

            else if
                dir2
                    |> RelativePos.toDir level
                    |> Dir.addTo pos
                    |> sendsEnergy
                        { to = dir2 |> RelativePos.reverse level
                        }
                        stage
            then
                [ ( dir1, { from = dir2 } ) ]

            else
                []

        _ ->
            RelativePos.list level
                |> List.filterMap
                    (\fromDir ->
                        if
                            fromDir
                                |> RelativePos.toDir level
                                |> Dir.addTo pos
                                |> sendsEnergy
                                    { to = fromDir |> RelativePos.reverse level
                                    }
                                    stage
                        then
                            ( fromDir |> RelativePos.reverse level
                            , { from = fromDir }
                            )
                                |> Just

                        else
                            Nothing
                    )
    )
        |> Dict.fromList
        |> (\sendsTo -> { connection | sendsTo = sendsTo })


sendsEnergy :
    { to : RelativePos
    }
    -> Stage
    -> ( Int, Int )
    -> Bool
sendsEnergy args stage pos =
    case stage.grid |> Dict.get pos of
        Just (ConnectionCell a) ->
            Connection.connectionSendsTo args.to a

        Just Origin ->
            True

        _ ->
            False
