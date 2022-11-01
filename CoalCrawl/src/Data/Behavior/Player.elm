module Data.Behavior.Player exposing (..)

import AStar
import Data.Behavior.Wagon
import Data.Behavior.Wall
import Data.Block exposing (Block(..))
import Data.Game exposing (Game)
import Data.Player
import Data.Position
import Data.Train
import Dict
import Random exposing (Generator)
import Set


passTime : Game -> Generator Game
passTime game =
    (if Data.Position.neighbors game.player.pos |> List.member game.selected then
        Dict.get game.selected game.world

     else
        Nothing
    )
        |> (\maybe ->
                case maybe of
                    Just block ->
                        case block of
                            Data.Block.Vein veinType ->
                                Data.Behavior.Wall.mine game.selected game

                            Data.Block.Train ->
                                putIntoTrain game |> Random.constant

                            Data.Block.Ground _ ->
                                moveTowardsSelected game
                                    |> Random.constant

                            Data.Block.Track ->
                                Random.constant game

                            Data.Block.Wall ->
                                Random.constant game

                            Data.Block.Wagon _ ->
                                game |> putIntoWagon |> Random.constant

                    Nothing ->
                        moveTowardsSelected game
                            |> Random.constant
           )


moveTowardsSelected : Game -> Game
moveTowardsSelected game =
    AStar.findPath AStar.straightLineCost
        (\pos ->
            Data.Position.neighbors pos
                |> List.filter
                    (\p ->
                        (p == game.selected)
                            || (case Dict.get p game.world of
                                    Just (Data.Block.Ground _) ->
                                        True

                                    Just Data.Block.Train ->
                                        True

                                    Just (Data.Block.Wagon _) ->
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
                case Dict.get pos game.world of
                    Just (Ground maybeItem) ->
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
                                                |> Dict.insert pos (Ground Nothing)
                                    }
                                )
                            |> Maybe.withDefault game
                            |> (\g -> { g | player = g.player |> Data.Player.moveTo pos })

                    Just Train ->
                        { game | player = game.player |> Data.Player.moveTo pos }

                    Just (Wagon content) ->
                        let
                            newWagonPos =
                                game.player.pos
                                    |> Data.Position.vecTo pos
                                    |> Data.Position.plus pos
                        in
                        game
                            |> Data.Behavior.Wagon.moveTo newWagonPos ( pos, content )
                            |> (\g -> { g | player = g.player |> Data.Player.moveTo pos })

                    _ ->
                        game
            )
        |> Maybe.withDefault game


putIntoWagon : Game -> Game
putIntoWagon game =
    case Dict.get game.selected game.world of
        Just (Data.Block.Wagon list) ->
            game.player
                |> Data.Player.dropItem
                |> Maybe.map
                    (\( player, item ) ->
                        game
                            |> (\g ->
                                    { g
                                        | world = g.world |> Dict.insert g.selected (Data.Block.Wagon (item :: list))
                                        , player = player
                                    }
                               )
                    )
                |> Maybe.withDefault game
                |> Data.Behavior.Wagon.unload ( game.selected, list )

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
