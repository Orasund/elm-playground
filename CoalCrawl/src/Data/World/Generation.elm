module Data.World.Generation exposing (..)

import AnyBag
import Config
import Data.Actor exposing (CaveType(..))
import Data.Block
import Data.Entity
import Data.Floor
import Data.Item
import Data.Position
import Data.Wagon
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
                                        Data.World.insertFloorAt ( x, y ) Data.Floor.Ground

                                    item :: _ ->
                                        \w ->
                                            w
                                                |> Data.World.insertFloorAt ( x, y ) Data.Floor.Ground
                                                |> Data.World.insertItemAt ( x, y ) item
                               )
                            |> generateContent
                                { probability =
                                    [ ( 0, ( x, y - 1 ) )
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
            [ Data.World.insertActor (Data.Actor.Cave Data.Actor.CoalCave)
            , Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal)
            , Data.World.insertActor (Data.Actor.Cave Data.Actor.IronCave)
            , Data.World.insertEntity (Data.Entity.Vein Data.Item.Iron)
            , Data.World.insertActor (Data.Actor.Cave Data.Actor.WaterCave)
            , Data.World.insertActor (Data.Actor.Cave Data.Actor.GoldCave)
            ]
                |> List.take (i + 1)
                |> List.reverse
    in
    (if modBy Config.tracksPerTrip y == 0 && (x == -2 || x == 2) then
        [ Data.World.insertEntity (Data.Entity.Vein Data.Item.Gold) ]

     else if y < Config.tracksPerTrip * 0 then
        []

     else if y < Config.tracksPerTrip * 1 then
        content 1

     else if y < Config.tracksPerTrip * 2 then
        content 2

     else if y < Config.tracksPerTrip * 3 then
        content 3

     else if y < Config.tracksPerTrip * 4 then
        content 4

     else if y < Config.tracksPerTrip * 5 then
        content 5

     else
        content 6
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
                                    (Data.Wagon.fullWagon Data.Item.Coal
                                        |> Data.Actor.Wagon
                                    )
                                    pos
                                |> Random.constant

                        else
                            Random.uniform head tail
                                |> Random.map
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
                                                    if p == nextPos then
                                                        Data.World.insertActor Data.Actor.Mine p

                                                    else
                                                        Data.World.insertEntity Data.Entity.Wall p
                                                )
                                                world
                                            |> Data.World.removeEntity pos
                                            |> Data.World.insertFloor Data.Floor.Track pos
                                    )
                    )

        [] ->
            world
                |> Data.World.removeEntity pos
                |> Random.constant


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
            [ ( 0.45, ( x, y - 1 ) )
            , ( 0.45, ( x, y + 1 ) )
            , ( 0.45, ( x - 1, y ) )
            , ( 0.45, ( x + 1, y ) )
            ]

        pos =
            ( x, y )
    in
    (if modBy Config.tracksPerTrip y == 0 && (x == -2 || x == 2) then
        Data.World.insertEntity (Data.Entity.Vein Data.Item.Gold)
            |> Random.constant

     else
        args.ground
    )
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
                                    |> Data.World.insertActor
                                    |> Random.constant
                                )
                                [ ( 1 / 4, wallGenerator pos )
                                , ( 1 / 64
                                  , Data.Actor.Mine
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
        WaterCave ->
            Random.weighted ( 1, Data.World.insertEntity Data.Entity.Water )
                [ ( 1 / 2, Data.World.insertFloor Data.Floor.Ground )
                , ( 1 / 4, Data.World.insertItem Data.Item.Gold )
                ]

        CoalCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Coal )
                [ ( 1 / 4, Data.World.insertActor Data.Actor.FallingCoal ) ]

        IronCave ->
            Random.weighted ( 1, Data.World.insertItem Data.Item.Iron )
                [ ( 1 / 8, Data.World.insertFloor Data.Floor.Track )
                , ( 1 / 32
                  , Data.Wagon.emptyWagon
                        |> Data.Wagon.load
                            ([ ( Data.Item.Coal, Config.wagonMaxItems ) ]
                                |> AnyBag.fromAssociationList Data.Item.toString
                            )
                        |> Data.Actor.Wagon
                        |> Data.World.insertActor
                  )
                ]

        GoldCave ->
            Random.weighted ( 1, Data.World.insertFloor Data.Floor.Ground )
                [ ( 1 / 4, Data.World.insertEntity Data.Entity.Wall )
                , ( 1 / 8, Data.World.insertItem Data.Item.Gold )
                ]
    )
        |> (\ground ->
                caveGenerator
                    { ground = ground
                    , cave = caveType
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
