module Data.Animation exposing (..)

import Array exposing (Array)
import Data.Actor
import Data.Behavior
import Data.Block
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.Minecart
import Data.Player
import Data.Train
import Data.World exposing (World)
import Dict
import Random


type alias Animation =
    { frames : Array Game
    , width : Int
    , height : Int
    }


emptyWorld : { width : Int, height : Int } -> World
emptyWorld args =
    List.range 0 (args.height - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (args.width - 1)
                    |> List.map (\x -> ( ( x, y ), Data.Block.FloorBlock Data.Floor.Ground ))
            )
        |> Data.World.fromList


tutorial : Animation
tutorial =
    let
        width =
            4

        height =
            3

        train =
            ( 3, 1 )

        player =
            ( 2, 1 )

        initGame : Game
        initGame =
            { world =
                emptyWorld { width = width, height = height }
                    |> Data.World.insertItem Data.Item.Coal ( 1, 0 )
                    |> Data.World.insertItem Data.Item.Coal ( 0, 1 )
                    |> Data.World.insertItem Data.Item.Coal ( 1, 2 )
                    |> Data.World.insertActor (Data.Actor.Train (Data.Train.fromPos train)) train
            , player = Data.Player.fromPos player
            , trainId = 0
            , selected = player
            , improvements = []
            }

        selections =
            [ ( 0, ( 0, 1 ) )
            , ( 5, train )
            , ( 5, ( 1, 0 ) )
            , ( 5, train )
            , ( 5, ( 1, 2 ) )
            , ( 5, train )
            ]
                |> List.foldl (\( i, p ) ( l, time ) -> ( ( i + time, p ) :: l, i + time ))
                    ( [], 0 )
                |> Tuple.first
                |> List.reverse
                |> Dict.fromList
    in
    { frames =
        List.range 0 30
            |> List.foldl
                (\int ( ( game, seed ), l ) ->
                    ( Random.step (Data.Behavior.passTime game) seed
                        |> Tuple.mapFirst
                            (\( g, _ ) ->
                                Dict.get int selections
                                    |> Maybe.map (\pos -> g |> Data.Game.select pos)
                                    |> Maybe.withDefault g
                            )
                    , game :: l
                    )
                )
                ( ( initGame
                  , Random.initialSeed 42
                  )
                , []
                )
            |> Tuple.second
            |> List.reverse
            |> Array.fromList
    , width = width
    , height = height
    }


animate : Animation
animate =
    let
        width =
            6

        height =
            3

        train =
            ( 5, 1 )

        wagon =
            ( 3, 1 )

        player =
            ( 2, 1 )

        initGame : Game
        initGame =
            { world =
                emptyWorld { width = width, height = height }
                    |> Data.World.insertItem Data.Item.Coal ( 1, 0 )
                    |> Data.World.insertItem Data.Item.Coal ( 0, 1 )
                    |> Data.World.insertItem Data.Item.Coal ( 1, 2 )
                    |> Data.World.insertActor (Data.Actor.Train (Data.Train.fromPos train)) train
                    |> Data.World.insertActor (Data.Actor.Minecart Data.Minecart.emptyWagon) wagon
            , player = Data.Player.fromPos player
            , trainId = 0
            , selected = player
            , improvements = []
            }

        selections =
            [ ( 0, ( 0, 1 ) )
            , ( 5, wagon )
            , ( 5, ( 1, 0 ) )
            , ( 5, wagon )
            , ( 5, ( 1, 2 ) )
            , ( 5, wagon )
            , ( 5, train )
            ]
                |> List.foldl (\( i, p ) ( l, time ) -> ( ( i + time, p ) :: l, i + time ))
                    ( [], 0 )
                |> Tuple.first
                |> List.reverse
                |> Dict.fromList
    in
    { frames =
        List.range 0 40
            |> List.foldl
                (\int ( ( game, seed ), l ) ->
                    ( Random.step (Data.Behavior.passTime game) seed
                        |> Tuple.mapFirst
                            (\( g, _ ) ->
                                Dict.get int selections
                                    |> Maybe.map (\pos -> g |> Data.Game.select pos)
                                    |> Maybe.withDefault g
                            )
                    , game :: l
                    )
                )
                ( ( initGame
                  , Random.initialSeed 42
                  )
                , []
                )
            |> Tuple.second
            |> List.reverse
            |> Array.fromList
    , width = width
    , height = height
    }
