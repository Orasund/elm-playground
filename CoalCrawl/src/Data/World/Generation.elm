module Data.World.Generation exposing (..)

import Config
import Data.Actor exposing (CaveType(..))
import Data.Block
import Data.Entity
import Data.Floor
import Data.Item
import Data.Minecart
import Data.Position
import Data.World exposing (World)
import Random exposing (Generator)


mine : ( Int, Int ) -> World -> Generator World
mine ( x, y ) world =
    case world |> Data.World.get ( x, y ) of
        Just ( Data.Block.EntityBlock entity, _ ) ->
            (case entity of
                Data.Entity.Vein item ->
                    [ item ]
                        |> Just

                Data.Entity.Actor _ ->
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
                                        identity

                                    item :: _ ->
                                        Data.World.insertItemAt ( x, y ) item
                               )
                            |> generateContent
                                { probability =
                                    [ ( if y > 0 then
                                            0.7

                                        else
                                            0
                                      , ( x, y - 1 )
                                      )
                                    , ( 1, ( x, y + 1 ) )
                                    , ( 0.7, ( x - 1, y ) )
                                    , ( 0.7, ( x + 1, y ) )
                                    ]
                                , content = wallGenerator ( x, y )
                                }
                    )
                |> Maybe.withDefault (Random.constant world)

        _ ->
            Random.constant world


wallGenerator : ( Int, Int ) -> Generator (( Int, Int ) -> World -> World)
wallGenerator ( x, y ) =
    let
        content i =
            [ Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.CoalCave))
            , Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal)
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.IronCave))
            , Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron)
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.WaterCave))
            , Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal)
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.LavaCave))
            ]
                |> List.take (i + 1)
                |> List.reverse
    in
    ((y // Config.tracksPerTrip) - (abs x // Config.tracksPerTrip) + 1)
        |> (\int ->
                if y < int then
                    []

                else
                    content int
           )
        |> (\list ->
                case list of
                    [] ->
                        Random.constant (\_ -> identity)

                    head :: tail ->
                        tail
                            |> List.indexedMap (\i fun -> ( 1 / (2 ^ (toFloat i + 1)), fun ))
                            |> Random.weighted ( 1, head )
           )


mineGenerator : ( Int, Int ) -> World -> Generator World
mineGenerator pos world =
    case
        Data.Position.neighbors pos
            |> List.filter
                (\p ->
                    Data.World.get p world
                        == Nothing
                )
    of
        head :: tail ->
            Random.weighted ( 1, False ) [ ( 1 / 4, True ) ]
                |> Random.andThen
                    (\stop ->
                        if stop then
                            pos
                                |> Data.Position.neighbors
                                |> List.filter
                                    (\p ->
                                        Data.World.get p world
                                            == Nothing
                                    )
                                |> List.foldl
                                    (\p ->
                                        Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) p
                                    )
                                    world
                                |> Data.World.insertActor
                                    (Data.Minecart.fullWagon Data.Item.Coal
                                        |> Data.Actor.Minecart
                                    )
                                    pos
                                |> Random.constant

                        else
                            Random.uniform head tail
                                |> Random.andThen
                                    (\nextPos ->
                                        pos
                                            |> Data.Position.neighbors
                                            |> List.filter
                                                (\p ->
                                                    Data.World.get p world
                                                        == Nothing
                                                )
                                            |> List.foldl
                                                (\p ->
                                                    Random.andThen
                                                        (\it ->
                                                            if p == nextPos then
                                                                it
                                                                    |> Data.World.insertActor (Data.Actor.Helper Data.Actor.Mine) p
                                                                    |> Random.constant

                                                            else
                                                                wallGenerator p
                                                                    |> Random.map (\fun -> fun p it)
                                                        )
                                                )
                                                (Random.constant world)
                                            |> Random.map (Data.World.removeEntity pos)
                                            |> Random.map (Data.World.insertFloor Data.Floor.Track pos)
                                    )
                    )

        [] ->
            world
                |> Data.World.removeEntity pos
                |> Random.constant


baseProbability : ( Int, Int ) -> List ( Float, ( Int, Int ) )
baseProbability ( x, y ) =
    [ ( if y < 0 then
            0.45

        else
            0
      , ( x, y - 1 )
      )
    , ( 0.45, ( x, y + 1 ) )
    , ( 0.45, ( x - 1, y ) )
    , ( 0.45, ( x + 1, y ) )
    ]


caveGenerator :
    { ground : Generator (( Int, Int ) -> World -> World)
    , cave : CaveType
    }
    -> ( Int, Int )
    -> World
    -> Generator World
caveGenerator args ( x, y ) world =
    let
        probability =
            baseProbability ( x, y )

        pos =
            ( x, y )
    in
    args.ground
        |> Random.andThen
            (\fun ->
                world
                    |> Data.World.removeEntity pos
                    |> fun pos
                    |> generateContent
                        { probability = probability
                        , content =
                            Random.weighted
                                ( 1
                                , args.cave
                                    |> Data.Actor.Cave
                                    |> Data.Actor.Helper
                                    |> Data.World.insertActor
                                    |> Random.constant
                                )
                                [ ( 1 / 4, wallGenerator pos )
                                , ( 1 / 8
                                  , Data.Actor.Path
                                        |> Data.Actor.Helper
                                        |> Data.World.insertActor
                                        |> Random.constant
                                  )
                                , ( 1 / 64
                                  , Data.Actor.Mine
                                        |> Data.Actor.Helper
                                        |> Data.World.insertActor
                                        |> Random.constant
                                  )
                                ]
                                |> Random.andThen identity
                        }
            )


exposedCave : CaveType -> ( Int, Int ) -> World -> Generator World
exposedCave caveType =
    (case caveType of
        CoalCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Coal )
                [ ( 1 / 4
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Coal)
                            |> Data.Actor.Helper
                        )
                  )
                ]

        IronCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Iron )
                [ ( 1 / 4
                  , Data.World.insertActor
                        (Data.Actor.Falling (Data.Entity.Vein Data.Item.Iron)
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 8, Data.World.insertEntity Data.Entity.Wall )
                ]

        WaterCave ->
            Random.weighted ( 1, Data.World.insertFloor Data.Floor.Ground )
                [ ( 1 / 4
                  , Data.World.insertActor
                        (Data.Actor.Falling Data.Entity.Water
                            |> Data.Actor.Helper
                        )
                  )
                , ( 1 / 8, Data.World.insertItem Data.Item.Gold )
                ]

        LavaCave ->
            Random.weighted ( 1, Data.World.insertEntity Data.Entity.Lava )
                [ ( 1 / 4, Data.World.insertItem Data.Item.Gold )
                ]
    )
        |> (\ground ->
                caveGenerator
                    { ground = ground
                    , cave = caveType
                    }
           )


{-| base function for generating content
-}
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
