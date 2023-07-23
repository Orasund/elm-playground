module Game exposing (..)

import Cell exposing (Cell(..), Cell1, Connection, ConnectionSort1, ConnectionSort2)
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
            , path : List ( Int, Int )
            }
    , grid : Dict ( Int, Int ) Cell1
    }


isSolved : Game -> Bool
isSolved game =
    case game of
        Level1 stage ->
            Stage.isSolved stage

        Level2 stage ->
            Stage.isSolved stage


toSave : Game -> SavedLevel
toSave game =
    let
        grid =
            game
                |> toDict

        targets =
            case game of
                Level1 stage ->
                    stage.targets

                Level2 stage ->
                    stage.targets

        buildPath =
            Maybe.andThen
                (\{ pos, to, path } ->
                    grid
                        |> Dict.get pos
                        |> Maybe.andThen
                            (\cell ->
                                case cell |> Debug.log "cell" of
                                    ConnectionCell connection ->
                                        connection.sendsTo
                                            |> Dict.get (Debug.log "to" to)
                                            |> Maybe.map (\{ from } -> { pos = from, path = pos :: path, to = pos })

                                    Origin ->
                                        Just { pos = pos, path = path, to = pos }

                                    Target maybe ->
                                        maybe |> Maybe.map (\from -> { pos = from, path = pos :: path, to = pos })

                                    _ ->
                                        Nothing
                            )
                )
    in
    targets
        |> List.filterMap
            (\target ->
                List.range 0 16
                    |> List.foldl (\_ -> buildPath)
                        (Just { pos = target, to = target, path = [] })
                    |> Maybe.map
                        (\{ pos, path } ->
                            [ ( RelativePos.fromTuple target, { from = RelativePos.fromTuple pos, path = path } )
                            , ( RelativePos.fromTuple pos, { from = RelativePos.fromTuple target, path = path } )
                            ]
                        )
            )
        |> List.concat
        |> Dict.fromList
        |> (\dict ->
                { connections = dict |> Debug.log "connections"
                , grid =
                    case game of
                        Level1 stage ->
                            stage.grid

                        Level2 _ ->
                            Dict.empty
                }
           )


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
                        case Dict.get (dir |> Dir.add pos) stage.grid of
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
            { connection : ( ( Int, Int ), Connection a ) -> Stage a -> Dict ( Int, Int ) { from : ( Int, Int ) }
            , toGame : Stage a -> Game
            }
            -> Stage a
            -> ( Game, Bool )
        tick args stage =
            stage.grid
                |> Dict.map
                    (\pos cell ->
                        case cell of
                            ConnectionCell conncetion ->
                                { conncetion
                                    | sendsTo = stage |> args.connection ( pos, conncetion )
                                }
                                    |> ConnectionCell

                            Target _ ->
                                Dir.list
                                    |> List.filter
                                        (\fromDir ->
                                            sendsEnergy
                                                { from = fromDir |> Dir.add pos
                                                , to = pos
                                                }
                                                stage
                                        )
                                    |> List.head
                                    |> Maybe.map (Dir.add pos)
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
                { connection = \( pos, a ) -> computeActiveConnectionsLv2 modules a.sort pos
                , toGame = Level2
                }
                stage


computeActiveConnectionsLv2 :
    Dict Int SavedLevel
    -> ConnectionSort2
    -> ( Int, Int )
    -> Stage ConnectionSort2
    -> Dict ( Int, Int ) { from : ( Int, Int ) }
computeActiveConnectionsLv2 modules { moduleId, rotation } pos stage =
    modules
        |> Dict.get moduleId
        |> Maybe.map .connections
        |> Maybe.withDefault Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( to, { from } ) ->
                if
                    sendsEnergy
                        { from =
                            from
                                |> RelativePos.rotate rotation
                                |> RelativePos.add pos
                        , to = pos
                        }
                        stage
                then
                    ( to
                        |> RelativePos.rotate rotation
                        |> RelativePos.add pos
                    , { from =
                            from
                                |> RelativePos.rotate rotation
                                |> RelativePos.add pos
                      }
                    )
                        |> Just

                else
                    Nothing
            )
        |> Dict.fromList


computeActiveConnectionsLv1 : List Dir -> ( Int, Int ) -> Stage ConnectionSort1 -> Dict ( Int, Int ) { from : ( Int, Int ) }
computeActiveConnectionsLv1 neighborsDir pos stage =
    let
        ( x, y ) =
            pos
    in
    (case neighborsDir of
        [ dir1, dir2 ] ->
            if
                sendsEnergy
                    { from = dir1 |> Dir.add pos
                    , to = pos
                    }
                    stage
            then
                [ ( dir2 |> Dir.add pos, { from = dir1 |> Dir.add pos } ) ]

            else if
                sendsEnergy
                    { from = dir2 |> Dir.add pos
                    , to = pos
                    }
                    stage
            then
                [ ( dir1 |> Dir.add pos, { from = dir2 |> Dir.add pos } ) ]

            else
                []

        _ ->
            Dir.list
                |> List.filterMap
                    (\fromDir ->
                        if
                            sendsEnergy
                                { from = fromDir |> Dir.add pos
                                , to = pos
                                }
                                stage
                        then
                            ( fromDir |> Dir.reverse |> Dir.add ( x, y )
                            , { from = fromDir |> Dir.add pos }
                            )
                                |> Just

                        else
                            Nothing
                    )
    )
        |> Dict.fromList


sendsEnergy : { from : ( Int, Int ), to : ( Int, Int ) } -> Stage a -> Bool
sendsEnergy args stage =
    case stage.grid |> Dict.get args.from of
        Just (ConnectionCell { sendsTo }) ->
            sendsTo |> Dict.member args.to

        Just Origin ->
            True

        _ ->
            False
