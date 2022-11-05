module Data.Behavior.Player exposing (..)

import AStar
import Data.Actor
import Data.Behavior.Wagon
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Data.Wagon
import Data.World
import Data.World.Generation
import Dict
import Html exposing (i)
import Random exposing (Generator)
import Set


passTime : Game -> Generator Game
passTime game =
    (if Data.Position.neighbors game.player.pos |> List.member game.selected then
        Data.World.get game.selected game.world

     else
        Nothing
    )
        |> (\maybe ->
                case maybe of
                    Just block ->
                        case block of
                            Data.Block.FloorBlock floor ->
                                case floor of
                                    Data.Floor.Ground _ ->
                                        moveTowardsSelected game

                                    Data.Floor.Track ->
                                        moveTowardsSelected game

                                    Data.Floor.RailwayTrack ->
                                        moveTowardsSelected game

                            Data.Block.EntityBlock entity ->
                                case entity of
                                    Data.Entity.Train ->
                                        putIntoTrain game |> Random.constant

                                    Data.Entity.Wall ->
                                        Random.constant game

                                    Data.Entity.Cave _ ->
                                        Random.constant game

                                    Data.Entity.Actor id ->
                                        game.world.actors
                                            |> Dict.get id
                                            |> Maybe.map
                                                (\( _, actor ) ->
                                                    case actor of
                                                        Data.Actor.Wagon _ ->
                                                            game |> putIntoWagon |> Random.constant
                                                )
                                            |> Maybe.withDefault (Random.constant game)

                                    Data.Entity.Vein _ ->
                                        game.world
                                            |> Data.World.Generation.mine game.selected
                                            |> Random.map (\world -> { game | world = world })
                                            |> Random.andThen moveTowardsSelected

                                    Data.Entity.Water ->
                                        game |> walkThroughWater game.selected

                                    Data.Entity.Rubble _ ->
                                        game |> takeFromRubble

                    Nothing ->
                        moveTowardsSelected game
           )
        |> Random.map (\g -> pickUp g.player.pos g)


walkThroughWater : ( Int, Int ) -> Game -> Generator Game
walkThroughWater pos game =
    pos
        |> Data.Position.neighbors
        |> List.filter
            (\p ->
                case game.world |> Data.World.get p of
                    Just (FloorBlock _) ->
                        True

                    _ ->
                        False
            )
        |> (\list ->
                case list of
                    head :: tail ->
                        Random.uniform head tail

                    [] ->
                        Random.constant game.player.pos
           )
        |> Random.map
            (\p ->
                game.world
                    |> Data.World.removeEntity pos
                    |> Data.World.insertEntity p Data.Entity.Water
                    |> (\world -> { game | world = world, player = game.player |> Data.Player.moveTo pos })
            )


moveTowardsSelected : Game -> Generator Game
moveTowardsSelected game =
    case game.player.riding of
        Just id ->
            game.world
                |> Data.World.getActor id
                |> Maybe.map
                    (\( _, actor ) ->
                        case actor of
                            Data.Actor.Wagon wagon ->
                                { game
                                    | player =
                                        wagon.movedFrom
                                            |> Maybe.map
                                                (\pos ->
                                                    game.player
                                                        |> Data.Player.moveTo pos
                                                        |> (if pos == game.selected then
                                                                Data.Player.stopRiding

                                                            else
                                                                identity
                                                           )
                                                )
                                            |> Maybe.withDefault (game.player |> Data.Player.stopRiding)
                                }
                    )
                |> Maybe.withDefault game
                |> Random.constant

        Nothing ->
            AStar.findPath AStar.straightLineCost
                (\pos ->
                    Data.Position.neighbors pos
                        |> List.filter
                            (\p ->
                                (p == game.selected)
                                    || (case Data.World.get p game.world of
                                            Just (Data.Block.FloorBlock _) ->
                                                True

                                            Just (Data.Block.EntityBlock (Data.Entity.Actor _)) ->
                                                True

                                            Just (Data.Block.EntityBlock Data.Entity.Water) ->
                                                True

                                            _ ->
                                                False
                                       )
                            )
                        |> Set.fromList
                )
                game.player.pos
                game.selected
                |> Maybe.andThen List.head
                |> Maybe.map
                    (\pos ->
                        case Data.World.get pos game.world of
                            Just (Data.Block.EntityBlock (Data.Entity.Actor id)) ->
                                case game.world.actors |> Dict.get id |> Maybe.map Tuple.second of
                                    Just (Data.Actor.Wagon _) ->
                                        game
                                            |> Data.Behavior.Wagon.move { backPos = game.player.pos } id
                                            |> Random.map
                                                (\g ->
                                                    { g
                                                        | player =
                                                            g.player
                                                                |> Data.Player.startRiding id
                                                                |> Data.Player.moveTo pos
                                                    }
                                                )

                                    Nothing ->
                                        game |> Random.constant

                            Just (Data.Block.EntityBlock Data.Entity.Water) ->
                                game |> walkThroughWater pos

                            Just (Data.Block.FloorBlock _) ->
                                { game | player = game.player |> Data.Player.moveTo pos }
                                    |> Random.constant

                            _ ->
                                game |> Random.constant
                    )
                |> Maybe.withDefault (Random.constant game)


pickUp : ( Int, Int ) -> Game -> Game
pickUp pos game =
    case Data.World.get pos game.world of
        Just (Data.Block.FloorBlock (Data.Floor.Ground maybeItem)) ->
            maybeItem
                |> Maybe.andThen
                    (\item ->
                        game.player
                            |> Data.Player.hold item
                    )
                |> Maybe.map
                    (\player ->
                        { game
                            | player = player
                            , world =
                                game.world
                                    |> Data.World.insert pos (Data.Floor.Ground Nothing |> Data.Block.FloorBlock)
                        }
                    )
                |> Maybe.withDefault game

        _ ->
            game


takeFromRubble : Game -> Generator Game
takeFromRubble game =
    case Data.World.get game.selected game.world of
        Just (Data.Block.EntityBlock (Data.Entity.Rubble list)) ->
            Random.int 0 (List.length list - 1)
                |> Random.andThen
                    (\i ->
                        let
                            l1 =
                                List.take i list
                        in
                        case list |> List.drop i of
                            head :: tail ->
                                game.player
                                    |> Data.Player.hold head
                                    |> Maybe.map
                                        (\player ->
                                            game.world
                                                |> Data.World.insertEntity game.selected
                                                    (Data.Entity.Rubble (l1 ++ tail))
                                                |> (\world ->
                                                        { game
                                                            | world = world
                                                            , player = player
                                                        }
                                                   )
                                        )
                                    |> Maybe.withDefault game
                                    |> Random.constant

                            [] ->
                                game.world
                                    |> Data.World.Generation.mine game.selected
                                    |> Random.map (\world -> { game | world = world })
                    )

        _ ->
            game |> Random.constant


putIntoWagon : Game -> Game
putIntoWagon game =
    case Data.World.get game.selected game.world of
        Just (Data.Block.EntityBlock (Data.Entity.Actor id)) ->
            case game.world.actors |> Dict.get id |> Maybe.map Tuple.second of
                Just (Data.Actor.Wagon wagon) ->
                    (if Data.Wagon.isFull wagon then
                        Nothing

                     else
                        game.player
                            |> Data.Player.dropItem
                            |> Maybe.map
                                (\( player, item ) ->
                                    wagon
                                        |> Data.Wagon.insert item
                                        |> (\w2 -> game.world |> Data.World.updateActor id (\_ -> Data.Actor.Wagon w2))
                                        |> (\world -> { game | world = world })
                                        |> (\g -> { g | player = player })
                                )
                    )
                        |> Maybe.withDefault game
                        |> Data.Behavior.Wagon.unload id

                Nothing ->
                    game

        _ ->
            game


putIntoTrain : Game -> Game
putIntoTrain game =
    Data.Player.dropItem game.player
        |> Maybe.map
            (\( player, item ) ->
                { game | player = player }
                    |> (\g -> { g | train = g.train |> Data.Train.addItem item })
            )
        |> Maybe.withDefault game
