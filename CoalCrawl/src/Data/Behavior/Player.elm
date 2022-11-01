module Data.Behavior.Player exposing (..)

import AStar
import AnyBag
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
                                            |> Random.constant

                                    Data.Floor.Track ->
                                        moveTowardsSelected game
                                            |> Random.constant

                            Data.Block.EntityBlock entity ->
                                case entity of
                                    Data.Entity.RailwayTrack ->
                                        Random.constant game

                                    Data.Entity.Wall ->
                                        Random.constant game

                                    Data.Entity.Wagon _ ->
                                        game |> putIntoWagon |> Random.constant

                                    Data.Entity.Vein _ ->
                                        Data.Behavior.Wall.mine game.selected game

                    Nothing ->
                        moveTowardsSelected game
                            |> Random.constant
           )
        |> Random.map (\g -> pickUp g.player.pos g)


moveTowardsSelected : Game -> Game
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
                            |> (\g -> { g | player = g.player |> Data.Player.moveTo pos })

                    Just (Data.Block.FloorBlock _) ->
                        { game | player = game.player |> Data.Player.moveTo pos }

                    _ ->
                        game
            )
        |> Maybe.withDefault game


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
