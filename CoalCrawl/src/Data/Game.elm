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
import Data.World exposing (World)


type alias Game =
    { world : World
    , player : Player
    , train : Train
    , selected : ( Int, Int )
    }


select : ( Int, Int ) -> Game -> Game
select pos game =
    { game
        | selected = pos
        , player = game.player |> Data.Player.startMovingTo pos
    }


buildBlock : ( Item, Int ) -> Block -> Game -> Game
buildBlock ( item, cost ) block game =
    game.train
        |> Data.Train.removeItem cost item
        |> Maybe.map
            (\train ->
                { game
                    | world = game.world |> Data.World.insert game.selected block
                    , train = train
                }
            )
        |> Maybe.withDefault game


buildActor : ( Item, Int ) -> Actor -> Game -> Game
buildActor ( item, cost ) actor game =
    game.train
        |> Data.Train.removeItem cost item
        |> Maybe.map
            (\train ->
                { game
                    | world = game.world |> Data.World.insertActorAt game.selected actor
                    , train = train
                }
            )
        |> Maybe.withDefault game


destroyBlock : Game -> Game
destroyBlock game =
    case Data.World.get game.selected game.world of
        Just ( Data.Block.EntityBlock entity, _ ) ->
            case entity of
                Data.Entity.Actor id ->
                    case game.world |> Data.World.getActor id of
                        Just ( _, Data.Actor.Wagon _ ) ->
                            { game
                                | train =
                                    game.train
                                        |> Data.Train.addAll
                                            (AnyBag.fromAssociationList Data.Item.toString
                                                [ ( Data.Item.Iron, Config.wagonCost ) ]
                                            )
                                , world =
                                    game.world
                                        |> Data.World.removeEntity game.selected
                            }

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

        coals =
            [ ( 0, 4 )
            , ( 0 - 1, 3 )
            , ( 0 + 1, 3 )
            ]
    in
    { world =
        [ ( train, Data.Block.EntityBlock Data.Entity.Train )
        , ( train, Data.Block.FloorBlock Data.Floor.RailwayTrack )
        , ( player, Data.Block.FloorBlock Data.Floor.Ground )
        ]
            ++ tracks
            ++ (coals |> List.map (\pos -> ( pos, Data.Entity.Vein Data.Item.Coal |> Data.Block.EntityBlock )))
            |> Data.World.fromList
    , player = player |> Data.Player.fromPos
    , train = train |> Data.Train.fromPos
    , selected = player
    }
