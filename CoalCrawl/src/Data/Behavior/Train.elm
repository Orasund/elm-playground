module Data.Behavior.Train exposing (..)

import Config
import Data.Behavior.Wagon
import Data.Behavior.Wall
import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.Position
import Data.Train
import Data.World
import Random exposing (Generator)


passTime : Game -> Generator Game
passTime game =
    let
        newPos =
            game.train |> Data.Train.forwardPos
    in
    if game.train.pos == Config.hqPos then
        { game
            | train =
                game.train
                    |> Data.Train.addTracks 8
                    |> Data.Train.turnAround
        }
            |> move
            |> Maybe.withDefault game
            |> Random.constant

    else if game.train.moving && (game.player.pos /= newPos) then
        Data.World.get newPos game.world
            |> Maybe.andThen
                (\block ->
                    case block of
                        Data.Block.FloorBlock floor ->
                            case floor of
                                Data.Floor.Ground maybeItem ->
                                    if game.train.tracks > 0 then
                                        maybeItem
                                            |> Maybe.map (\item -> game.train |> Data.Train.addItem item)
                                            |> Maybe.withDefault game.train
                                            |> (\train -> { game | train = train })
                                            |> placeTrack
                                            |> Maybe.map mine

                                    else
                                        turnToHQ game
                                            |> Random.constant
                                            |> Just

                                _ ->
                                    if game.train.tracks > 0 then
                                        game
                                            |> placeTrack
                                            |> Maybe.map mine

                                    else
                                        game
                                            |> mine
                                            |> Random.map turnToHQ
                                            |> Just

                        Data.Block.EntityBlock entity ->
                            case entity of
                                Data.Entity.RailwayTrack ->
                                    move game |> Maybe.map Random.constant

                                Data.Entity.Wagon anyBag ->
                                    { game
                                        | train = Data.Train.addAll anyBag game.train
                                        , world = game.world |> Data.World.removeEntity newPos
                                    }
                                        |> Random.constant
                                        |> Just

                                _ ->
                                    game
                                        |> mine
                                        |> Random.map turnToHQ
                                        |> Just
                )
            |> Maybe.withDefault (Random.constant game)

    else
        Random.constant game


turnToHQ : Game -> Game
turnToHQ game =
    { game | train = game.train |> Data.Train.stop |> Data.Train.turnAround }


placeTrack : Game -> Maybe Game
placeTrack game =
    let
        newPos =
            Data.Train.forwardPos game.train
    in
    game.train
        |> Data.Train.removeTrack
        |> Maybe.map
            (\train ->
                { game
                    | world =
                        game.world
                            |> Data.World.insertEntity newPos Data.Entity.RailwayTrack
                    , train = train
                }
            )


move : Game -> Maybe Game
move game =
    let
        newPos =
            Data.Train.forwardPos game.train
    in
    game.train
        |> Data.Train.removeItem 1 Data.Item.Coal
        |> Maybe.map
            (\train ->
                game
                    |> (\g -> { g | train = train })
                    |> (\g ->
                            g.world
                                |> Data.World.insertEntity g.train.pos Data.Entity.RailwayTrack
                                |> Data.World.removeEntity newPos
                                |> Data.World.insertFloor newPos Data.Floor.Train
                                |> (\world -> { g | world = world })
                       )
                    |> (\g -> { g | train = g.train |> Data.Train.move })
            )


mine : Game -> Generator Game
mine game =
    let
        newPos =
            game.train |> Data.Train.forwardPos
    in
    newPos
        :: Data.Position.neighbors newPos
        |> List.foldl
            (\pos ->
                Random.andThen (Data.Behavior.Wall.mine pos)
            )
            (Random.constant game)
