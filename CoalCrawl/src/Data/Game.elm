module Data.Game exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Item exposing (Item)
import Data.Player exposing (Player)
import Data.Train exposing (Train)
import Data.World exposing (World, setActor)


type alias Game =
    { world : World
    , player : Player
    , trainId : Int
    , selected : ( Int, Int )
    }


getTrain : Game -> Train
getTrain game =
    case game.world |> Data.World.getActor game.trainId of
        Just ( _, Data.Actor.Train train ) ->
            train

        _ ->
            Data.Train.fromPos ( 0, 0 )


setTrain : Train -> Game -> Game
setTrain train game =
    { game
        | world =
            game.world
                |> setActor game.trainId (Data.Actor.Train train)
    }


setTrainOf : Game -> Train -> Game
setTrainOf game train =
    setTrain train game


setWorldOf : Game -> World -> Game
setWorldOf game world =
    setWorld world game


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game
        | selected = pos
        , player = game.player |> Data.Player.startMovingTo pos
    }


buildBlock : ( Int, Int ) -> ( Item, Int ) -> Block -> Game -> Game
buildBlock pos ( item, cost ) block game =
    if
        case block of
            FloorBlock _ ->
                Data.World.getBlock pos game.world
                    == Just (Data.Block.FloorBlock Data.Floor.Ground)

            EntityBlock _ ->
                Data.World.isFloor pos game.world
    then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insert pos block
                    }
                        |> setTrain train
                )
            |> Maybe.withDefault game

    else
        game


buildActor : ( Int, Int ) -> ( Item, Int ) -> Actor -> Game -> Game
buildActor pos ( item, cost ) actor game =
    if Data.World.isFloor pos game.world then
        game
            |> getTrain
            |> Data.Train.removeItem cost item
            |> Maybe.map
                (\train ->
                    { game
                        | world = game.world |> Data.World.insertActorAt pos actor
                    }
                        |> setTrain train
                )
            |> Maybe.withDefault game

    else
        game


destroyBlock : Game -> Game
destroyBlock game =
    case Data.World.get game.selected game.world of
        Just ( Data.Block.EntityBlock entity, _ ) ->
            case entity of
                Data.Entity.Actor id ->
                    case game.world |> Data.World.getActor id of
                        Just ( _, Data.Actor.Minecart _ ) ->
                            game
                                |> getTrain
                                |> Data.Train.addAll
                                    (AnyBag.fromAssociationList Data.Item.toString
                                        [ ( Data.Item.Iron, Config.wagonCost ) ]
                                    )
                                |> (\train -> game |> setTrain train)
                                |> (\g ->
                                        { g
                                            | world =
                                                game.world
                                                    |> Data.World.removeEntity game.selected
                                        }
                                   )

                        _ ->
                            game.world
                                |> Data.World.removeEntity game.selected
                                |> (\world -> { game | world = world })

                _ ->
                    game.world
                        |> Data.World.removeEntity game.selected
                        |> (\world -> { game | world = world })

        Just ( Data.Block.FloorBlock _, _ ) ->
            game.world
                |> Data.World.removeFloor game.selected
                |> (\world -> { game | world = world })

        Nothing ->
            game


new : Game
new =
    let
        train =
            ( 0, 2 )

        player =
            ( 0, 3 )

        tracks =
            List.range 0 1
                |> List.map
                    (\i ->
                        ( ( 0, i )
                        , Data.Block.FloorBlock Data.Floor.RailwayTrack
                        )
                    )

        walls =
            List.range 0 1
                |> List.concatMap
                    (\y ->
                        [ -1, 1 ]
                            |> List.map (\x -> ( ( x, y ), Data.Block.EntityBlock Data.Entity.Wall ))
                    )

        coals =
            [ ( 0, 4 )
            , ( 0 - 1, 3 )
            , ( 0 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.FloorBlock Data.Floor.RailwayTrack )
        , ( player, Data.Block.FloorBlock Data.Floor.Ground )
        ]
            ++ tracks
            ++ walls
            ++ (coals |> List.map (\pos -> ( pos, Data.Entity.Vein Data.Item.Coal |> Data.Block.EntityBlock )))
            |> Data.World.fromList
            |> Data.World.insertActor
                (train
                    |> Data.Train.fromPos
                    |> Data.Train.addAll
                        (AnyBag.fromAssociationList Data.Item.toString
                            [ ( Data.Item.Iron, 100 ) ]
                        )
                    |> Data.Actor.Train
                )
                train
    , player = player |> Data.Player.fromPos
    , trainId = 0
    , selected = player
    }


setWorld : World -> Game -> Game
setWorld world game =
    { game | world = world }
