module Data.Behavior.Generation exposing (..)

import Data.Block
import Data.Entity exposing (CaveType, Entity)
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.World exposing (World)
import Random exposing (Generator)


mine : ( Int, Int ) -> World -> Generator World
mine ( x, y ) world =
    case world |> Data.World.get ( x, y ) of
        Just (Data.Block.EntityBlock entity) ->
            (case entity of
                Data.Entity.Vein item ->
                    Just (Just item)

                Data.Entity.Train ->
                    Nothing

                Data.Entity.Wagon _ ->
                    Nothing

                _ ->
                    Just Nothing
            )
                |> Maybe.map
                    (\maybeItem ->
                        world
                            |> Data.World.removeEntity ( x, y )
                            |> Data.World.insertFloor ( x, y ) (Data.Floor.Ground maybeItem)
                            |> generateContent
                                { probability =
                                    [ ( 0, ( x, y - 1 ) )
                                    , ( 0.8, ( x, y + 1 ) )
                                    , ( 0.5, ( x - 1, y ) )
                                    , ( 0.5, ( x + 1, y ) )
                                    ]
                                , content =
                                    Random.weighted ( 1, Data.Entity.Vein Data.Item.Coal )
                                        [ ( 1 / 2, Data.Entity.Vein Data.Item.Iron )
                                        , ( 1 / 8, Data.Entity.Cave Data.Entity.WaterCave )
                                        , ( 1 / 8, Data.Entity.Rubble [ Data.Item.Coal, Data.Item.Coal, Data.Item.Iron ] )
                                        ]
                                }
                    )
                |> Maybe.withDefault (Random.constant world)

        _ ->
            Random.constant world


exposedCave : CaveType -> ( Int, Int ) -> World -> Generator World
exposedCave caveType ( x, y ) world =
    world
        |> Data.World.insertEntity ( x, y ) Data.Entity.Water
        |> generateContent
            { probability =
                [ ( 0, ( x, y - 1 ) )
                , ( 0.5, ( x, y + 1 ) )
                , ( 0.5, ( x - 1, y ) )
                , ( 0.5, ( x + 1, y ) )
                ]
            , content =
                Random.weighted ( 1, Data.Entity.Cave Data.Entity.WaterCave )
                    [ ( 1 / 2, Data.Entity.Vein Data.Item.Iron )
                    , ( 1 / 8, Data.Entity.Vein Data.Item.Coal )
                    , ( 1 / 8, Data.Entity.Vein Data.Item.Gold )
                    ]
            }


generateContent : { probability : List ( Float, ( Int, Int ) ), content : Generator Entity } -> World -> Generator World
generateContent args dict =
    args.probability
        |> List.foldl
            (\( prob, pos ) ->
                Random.andThen
                    (\d ->
                        Random.map2
                            (\float entity ->
                                d
                                    |> Data.World.update pos
                                        (\maybe ->
                                            maybe
                                                |> Maybe.withDefault
                                                    ((if float < prob then
                                                        entity

                                                      else
                                                        Data.Entity.Wall
                                                     )
                                                        |> Data.Block.EntityBlock
                                                    )
                                                |> Just
                                        )
                            )
                            (Random.float 0 1)
                            args.content
                    )
            )
            (Random.constant dict)
