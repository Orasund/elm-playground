module Data.World.Generation exposing (..)

import AnyBag
import Config
import Data.Actor exposing (CaveType(..))
import Data.Block
import Data.Entity
import Data.Floor
import Data.Item
import Data.Wagon
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

                                    item :: _ ->
                                        Data.World.insertFloorAt ( x, y ) (Data.Floor.Ground (Just item))
                               )
                            |> wall ( x, y )
                    )
                |> Maybe.withDefault (Random.constant world)

        _ ->
            Random.constant world


wall : ( Int, Int ) -> World -> Generator World
wall ( x, y ) =
    generateContent
        { probability =
            [ ( 0, ( x, y - 1 ) )
            , ( 1, ( x, y + 1 ) )
            , ( 0.7, ( x - 1, y ) )
            , ( 0.7, ( x + 1, y ) )
            ]
        , content =
            if y < Config.tracksPerTrip then
                Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                    [ ( 1 / 2, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                    , ( 1 / 2, Data.World.insertEntity Data.Entity.Wall )
                    ]

            else if y < Config.tracksPerTrip * 2 then
                Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                    [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                    , ( 1 / 4, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                    , ( 1 / 4, Data.World.insertEntity Data.Entity.Wall )
                    ]

            else if y < Config.tracksPerTrip * 3 then
                Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                    [ ( 1 / 2, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                    , ( 1 / 4, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                    , ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                    , ( 1 / 8, Data.World.insertEntity Data.Entity.Wall )
                    ]

            else if y < Config.tracksPerTrip * 4 then
                Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave) )
                    [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                    , ( 1 / 4, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                    , ( 1 / 8, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                    , ( 1 / 16, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                    , ( 1 / 16, Data.World.insertEntity Data.Entity.Wall )
                    ]

            else
                Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.OldMine) )
                    [ ( 1 / 2, Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave) )
                    , ( 1 / 4, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                    , ( 1 / 8, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                    , ( 1 / 16, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                    , ( 1 / 32, Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave) )
                    , ( 1 / 32, Data.World.insertEntity Data.Entity.Wall )
                    ]
        }


exposedCave : CaveType -> ( Int, Int ) -> World -> Generator World
exposedCave caveType ( x, y ) world =
    let
        probability =
            [ ( 0.2, ( x, y - 1 ) )
            , ( 0.5, ( x, y + 1 ) )
            , ( 0.5, ( x - 1, y ) )
            , ( 0.5, ( x + 1, y ) )
            ]
    in
    case caveType of
        WaterCave ->
            Random.weighted ( 1, Data.World.insertFloor (Data.Floor.Ground Nothing) )
                [ ( 1 / 2, Data.World.insertEntity Data.Entity.Water ) ]
                |> Random.andThen
                    (\fun ->
                        world
                            |> fun ( x, y )
                            |> generateContent
                                { probability = probability
                                , content =
                                    Random.weighted ( 1, \pos -> Data.World.insertActorAt pos (Data.Actor.Cave Data.Actor.WaterCave) )
                                        [ ( 1 / 2, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Iron) )
                                        , ( 1 / 8, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Coal) )
                                        , ( 1 / 8, \pos -> Data.World.insertEntityAt pos (Data.Entity.Vein Data.Item.Gold) )
                                        ]
                                }
                    )

        CoalCave ->
            world
                |> Data.World.removeEntity ( x, y )
                |> Data.World.insert ( x, y ) (Data.Block.FloorBlock (Data.Floor.Ground (Just Data.Item.Coal)))
                |> generateContent
                    { probability = probability
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
                    { probability = probability
                    , content =
                        Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave) )
                            [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                            , ( 1 / 8, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                            ]
                    }

        OldMine ->
            Random.weighted ( 1, Data.World.insertFloor (Data.Floor.Ground Nothing) )
                [ ( 1 / 8, Data.World.insertFloor Data.Floor.Track )
                , ( 1 / 16
                  , Data.Wagon.emptyWagon
                        |> Data.Wagon.load
                            ([ ( Data.Item.Coal, Config.wagonMaxItems ) ]
                                |> AnyBag.fromAssociationList Data.Item.toString
                            )
                        |> Data.Actor.Wagon
                        |> Data.World.insertActor
                  )
                , ( 1 / 32
                  , Data.Wagon.emptyWagon
                        |> Data.Wagon.load
                            ([ ( Data.Item.Iron, Config.wagonMaxItems ) ]
                                |> AnyBag.fromAssociationList Data.Item.toString
                            )
                        |> Data.Actor.Wagon
                        |> Data.World.insertActor
                  )
                ]
                |> Random.andThen
                    (\fun ->
                        world
                            |> Data.World.removeEntity ( x, y )
                            |> fun ( x, y )
                            |> generateContent
                                { probability = probability
                                , content =
                                    Random.weighted ( 1, Data.World.insertActor (Data.Actor.Cave Data.Actor.OldMine) )
                                        [ ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                                        , ( 1 / 2, Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron) )
                                        ]
                                }
                    )


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
