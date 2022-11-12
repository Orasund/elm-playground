module Data.World.Generation exposing (..)

import Config
import Data.Actor exposing (CaveType(..))
import Data.Block
import Data.Entity
import Data.Floor
import Data.Item
import Data.World exposing (World)
import Random exposing (Generator)


mine : ( Int, Int ) -> World -> Generator World
mine ( x, y ) world =
    case world |> Data.World.get ( x, y ) of
        Just (Data.Block.EntityBlock entity) ->
            (case entity of
                Data.Entity.Vein item ->
                    [ item ]
                        |> Just

                Data.Entity.Train ->
                    Nothing

                Data.Entity.Actor id ->
                    case world |> Data.World.getActor id |> Maybe.map Tuple.second of
                        Just (Data.Actor.Wagon _) ->
                            Nothing

                        _ ->
                            Nothing

                _ ->
                    Just []
            )
                |> Maybe.map
                    (\items ->
                        world
                            |> Data.World.removeEntity ( x, y )
                            |> (case items of
                                    [] ->
                                        Data.World.insertFloorAt ( x, y ) (Data.Floor.Ground Nothing)

                                    [ item ] ->
                                        Data.World.insertFloorAt ( x, y ) (Data.Floor.Ground (Just item))

                                    _ ->
                                        Data.World.insertEntityAt ( x, y ) (Data.Entity.Rubble items)
                               )
                            |> generateContent
                                { probability =
                                    [ ( 0, ( x, y - 1 ) )
                                    , ( 0.8, ( x, y + 1 ) )
                                    , ( 0.5, ( x - 1, y ) )
                                    , ( 0.5, ( x + 1, y ) )
                                    ]
                                , content =
                                    if y < Config.tracksPerTrip then
                                        Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                                            [ ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                                            ]

                                    else if y < Config.tracksPerTrip * 2 then
                                        Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                                            [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                                            , ( 1 / 4, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                                            , ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.RubbleCave) )
                                            ]

                                    else if y < Config.tracksPerTrip * 3 then
                                        Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                                            [ ( 1 / 2, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                                            , ( 1 / 4, Data.World.insertActor (Data.Actor.Cave Data.Actor.RubbleCave) )
                                            , ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave) )
                                            ]

                                    else
                                        Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                                            [ ( 1 / 2, Data.World.insertActor (Data.Actor.Cave Data.Actor.RubbleCave) )
                                            , ( 1 / 4, Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave) )
                                            , ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave) )
                                            ]
                                }
                    )
                |> Maybe.withDefault (Random.constant world)

        _ ->
            Random.constant world


exposedCave : CaveType -> ( Int, Int ) -> World -> Generator World
exposedCave caveType ( x, y ) world =
    case caveType of
        WaterCave ->
            world
                |> Data.World.insertEntityAt ( x, y ) Data.Entity.Water
                |> generateContent
                    { probability =
                        [ ( 0, ( x, y - 1 ) )
                        , ( 0.5, ( x, y + 1 ) )
                        , ( 0.5, ( x - 1, y ) )
                        , ( 0.5, ( x + 1, y ) )
                        ]
                    , content =
                        Random.weighted ( 1, \pos -> Data.World.insertActorAt pos (Data.Actor.Cave Data.Actor.WaterCave) )
                            [ ( 1 / 2, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Iron) )
                            , ( 1 / 8, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Coal) )
                            , ( 1 / 8, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Gold) )
                            ]
                    }

        RubbleCave ->
            Random.uniform (Data.Block.FloorBlock Data.Floor.ground)
                [ Data.Block.EntityBlock Data.Entity.rubble
                , Data.Block.FloorBlock Data.Floor.ground
                , Data.Block.FloorBlock Data.Floor.ground
                ]
                |> Random.andThen
                    (\block ->
                        world
                            |> Data.World.removeEntity ( x, y )
                            |> Data.World.insert ( x, y ) block
                            |> generateContent
                                { probability =
                                    [ ( 0, ( x, y - 1 ) )
                                    , ( 1, ( x, y + 1 ) )
                                    , ( 1, ( x - 1, y ) )
                                    , ( 1, ( x + 1, y ) )
                                    ]
                                , content =
                                    Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.RubbleCave) )
                                        [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                                        , ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                                        ]
                                }
                    )

        CoalCave ->
            world
                |> Data.World.removeEntity ( x, y )
                |> Data.World.insert ( x, y ) (Data.Block.FloorBlock (Data.Floor.Ground (Just Data.Item.Coal)))
                |> generateContent
                    { probability =
                        [ ( 0, ( x, y - 1 ) )
                        , ( 0.5, ( x, y + 1 ) )
                        , ( 0.5, ( x - 1, y ) )
                        , ( 0.5, ( x + 1, y ) )
                        ]
                    , content =
                        Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                            [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                            , ( 1 / 8, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                            ]
                    }

        IronCave ->
            world
                |> Data.World.removeEntity ( x, y )
                |> Data.World.insert ( x, y ) (Data.Block.FloorBlock (Data.Floor.Ground (Just Data.Item.Iron)))
                |> generateContent
                    { probability =
                        [ ( 0, ( x, y - 1 ) )
                        , ( 0.5, ( x, y + 1 ) )
                        , ( 0.5, ( x - 1, y ) )
                        , ( 0.5, ( x + 1, y ) )
                        ]
                    , content =
                        Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                            [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                            , ( 1 / 8, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                            ]
                    }


generateContent : { probability : List ( Float, ( Int, Int ) ), content : Generator (( Int, Int ) -> World -> World) } -> World -> Generator World
generateContent args dict =
    args.probability
        |> List.foldl
            (\( prob, pos ) ->
                Random.andThen
                    (\d ->
                        Random.map2
                            (\float updateAt ->
                                d
                                    |> Data.World.get pos
                                    |> (\maybe ->
                                            if maybe == Nothing then
                                                d
                                                    |> (if float < prob then
                                                            updateAt

                                                        else
                                                            Data.World.insertEntity Data.Entity.Wall
                                                       )
                                                        pos

                                            else
                                                d
                                       )
                            )
                            (Random.float 0 1)
                            args.content
                    )
            )
            (Random.constant dict)
