module Data.Behavior.Player exposing (..)

import AStar
import Data.Behavior.Wagon
import Data.Behavior.Wall
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Data.Wagon
import Data.World
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

                                    Data.Entity.Wall _ ->
                                        Random.constant game

                                    Data.Entity.Wagon _ ->
                                        game |> putIntoWagon |> Random.constant

                                    Data.Entity.Vein _ ->
                                        game
                                            |> Data.Behavior.Wall.mine game.selected
                                            |> Random.andThen moveTowardsSelected

                                    Data.Entity.Water ->
                                        game |> walkThroughWater game.selected

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
    if game.player.riding then
        Random.constant game

    else
        AStar.findPath AStar.straightLineCost
            (\pos ->
                Data.Position.neighbors pos
                    |> List.filter
                        (\p ->
                            (p == game.selected)
                                || (case Data.World.get p game.world of
                                        Just (Data.Block.FloorBlock _) ->
                                            True

                                        Just (Data.Block.EntityBlock (Data.Entity.Wagon _)) ->
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
                        Just (Data.Block.EntityBlock (Data.Entity.Wagon content)) ->
                            let
                                newWagonPos =
                                    game.player.pos
                                        |> Data.Position.vecTo pos
                                        |> Data.Position.plus pos
                            in
                            game
                                |> Data.Behavior.Wagon.move { backPos = game.player.pos, forwardPos = newWagonPos } ( pos, content )
                                |> Random.map (\g -> { g | player = g.player |> Data.Player.moveTo pos })

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


putIntoWagon : Game -> Game
putIntoWagon game =
    case Data.World.get game.selected game.world of
        Just (Data.Block.EntityBlock (Data.Entity.Wagon wagon)) ->
            (if Data.Wagon.isFull wagon then
                Nothing

             else
                game.player
                    |> Data.Player.dropItem
                    |> Maybe.map
                        (\( player, item ) ->
                            game
                                |> (\g ->
                                        wagon
                                            |> Data.Wagon.insert item
                                            |> Data.Entity.Wagon
                                            |> Data.Block.EntityBlock
                                            |> (\block -> g.world |> Data.World.insert g.selected block)
                                   )
                                |> (\world -> { game | world = world })
                                |> (\g -> { g | player = player })
                        )
            )
                |> Maybe.withDefault game
                |> Data.Behavior.Wagon.unload ( game.selected, wagon )

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
