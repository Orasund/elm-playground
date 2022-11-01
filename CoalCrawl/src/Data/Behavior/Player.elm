module Data.Behavior.Player exposing (..)

import AStar
import AnyBag
import Config
import Data.Behavior.Wagon
import Data.Behavior.Wall
import Data.Block exposing (Block(..))
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Data.World
import Dict
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
                                    Data.Floor.Train ->
                                        putIntoTrain game |> Random.constant

                                    Data.Floor.Ground _ ->
                                        moveTowardsSelected game

                                    Data.Floor.Track ->
                                        moveTowardsSelected game

                            Data.Block.EntityBlock entity ->
                                case entity of
                                    Data.Entity.RailwayTrack ->
                                        Random.constant game

                                    Data.Entity.Wall _ ->
                                        Random.constant game

                                    Data.Entity.Wagon _ ->
                                        game |> putIntoWagon |> Random.constant

                                    Data.Entity.Vein _ ->
                                        Data.Behavior.Wall.mine game.selected game

                                    Data.Entity.Water ->
                                        game |> walkThroughWater game.selected |> Random.constant

                    Nothing ->
                        moveTowardsSelected game
           )
        |> Random.map (\g -> pickUp g.player.pos g)


walkThroughWater : ( Int, Int ) -> Game -> Game
walkThroughWater pos game =
    game.world
        |> Data.World.removeEntity pos
        |> Data.World.insertEntity game.player.pos Data.Entity.Water
        |> (\world -> { game | world = world, player = game.player |> Data.Player.moveTo pos })


moveTowardsSelected : Game -> Generator Game
moveTowardsSelected game =
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
                            |> Data.Behavior.Wagon.moveTo newWagonPos ( pos, content )
                            |> Random.map (\g -> { g | player = g.player |> Data.Player.moveTo pos })

                    Just (Data.Block.EntityBlock Data.Entity.Water) ->
                        game |> walkThroughWater pos |> Random.constant

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
        Just (Data.Block.EntityBlock (Data.Entity.Wagon anyBag)) ->
            (if AnyBag.size anyBag < Config.wagonMaxItems then
                game.player
                    |> Data.Player.dropItem
                    |> Maybe.map
                        (\( player, item ) ->
                            game
                                |> (\g ->
                                        anyBag
                                            |> AnyBag.insert 1 item
                                            |> Data.Entity.Wagon
                                            |> Data.Block.EntityBlock
                                            |> (\block -> g.world |> Data.World.insert g.selected block)
                                   )
                                |> (\world -> { game | world = world })
                                |> (\g -> { g | player = player })
                        )

             else
                Nothing
            )
                |> Maybe.withDefault game
                |> Data.Behavior.Wagon.unload ( game.selected, anyBag )

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
