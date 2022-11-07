module Data.Behavior.Train exposing (..)

import Config
import Data.Actor
import Data.Block
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.Position
import Data.Train
import Data.World
import Data.World.Generation
import Dict
import Random exposing (Generator)


passTime : Game -> Generator ( Game, Bool )
passTime game =
    let
        newPos =
            game.train |> Data.Train.forwardPos

        returnGame =
            Random.map (\g -> ( g, False ))
    in
    if game.train.pos == Config.hqPos then
        stockUpAtBase game |> Random.constant

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
                                            |> mineAndPlaceTrack

                                    else
                                        turnToHQ game
                                            |> Random.constant
                                            |> Just

                                Data.Floor.RailwayTrack ->
                                    move game |> Maybe.map Random.constant

                                _ ->
                                    if game.train.tracks > 0 then
                                        game
                                            |> mineAndPlaceTrack

                                    else
                                        game
                                            |> mine
                                            |> Random.map turnToHQ
                                            |> Just

                        Data.Block.EntityBlock entity ->
                            case entity of
                                Data.Entity.Actor id ->
                                    game.world.actors
                                        |> Dict.get id
                                        |> Maybe.andThen
                                            (\( _, actor ) ->
                                                case actor of
                                                    Data.Actor.Wagon wagon ->
                                                        { game
                                                            | train = Data.Train.addAll wagon.items game.train
                                                            , world = game.world |> Data.World.removeEntity newPos
                                                        }
                                                            |> Random.constant
                                                            |> Just

                                                    Data.Actor.Cave _ ->
                                                        Nothing

                                                    Data.Actor.Bomb _ ->
                                                        Just (Random.constant game)
                                            )

                                _ ->
                                    if game.train.tracks > 0 then
                                        game
                                            --|> mine
                                            |> Random.constant
                                            |> Just

                                    else
                                        game
                                            |> mine
                                            |> Random.map turnToHQ
                                            |> Just
                )
            |> Maybe.withDefault (Random.constant game)
            |> returnGame

    else
        Random.constant game
            |> returnGame


stockUpAtBase : Game -> ( Game, Bool )
stockUpAtBase game =
    { game
        | train =
            game.train
                |> Data.Train.addTracks Config.tracksPerTrip
                |> Data.Train.turnAround
    }
        |> move
        |> Maybe.map (\g -> ( g, True ))
        |> Maybe.withDefault ( game, False )


turnToHQ : Game -> Game
turnToHQ game =
    { game | train = game.train |> Data.Train.turnAround }


mineAndPlaceTrack : Game -> Maybe (Generator Game)
mineAndPlaceTrack game =
    let
        newPos =
            Data.Train.forwardPos game.train
    in
    game.train
        |> Data.Train.removeTrack
        |> Maybe.map
            (\train ->
                { game | train = train }
                    |> mine
                    |> Random.map
                        (\g ->
                            { g
                                | world =
                                    g.world
                                        |> Data.World.insertFloorAt newPos Data.Floor.RailwayTrack
                            }
                        )
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
                                |> Data.World.removeEntity g.train.pos
                                |> Data.World.insertEntityAt newPos Data.Entity.Train
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
                Random.andThen (Data.World.Generation.mine pos)
            )
            (Random.constant game.world)
        |> Random.map (\world -> { game | world = world })
