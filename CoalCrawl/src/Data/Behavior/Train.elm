module Data.Behavior.Train exposing (..)

import AnyBag
import Config
import Data.Actor
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.Position
import Data.Sound
import Data.Train
import Data.World
import Data.World.Generation
import Dict
import Random exposing (Generator)


passTime : Game -> Generator ( Game, List Effect )
passTime game =
    let
        newPos =
            game.train |> Data.Train.forwardPos

        returnGame =
            Random.map (\g -> ( g, [] ))
    in
    if game.train.pos == Config.hqPos then
        stockUpAtBase game |> Random.constant

    else if game.train.moving && (game.player.pos /= newPos) then
        Data.World.get newPos game.world
            |> Maybe.andThen
                (\block ->
                    case block of
                        ( Data.Block.FloorBlock floor, maybeItem ) ->
                            case floor of
                                Data.Floor.Ground ->
                                    if game.train.tracks > 0 then
                                        maybeItem
                                            |> Maybe.map (\item -> game.train |> Data.Train.addItem item)
                                            |> Maybe.withDefault game.train
                                            |> (\train -> { game | train = train })
                                            |> mineAndPlaceTrack
                                            |> Maybe.map returnGame

                                    else
                                        turnToHQ game
                                            |> Random.constant
                                            |> returnGame
                                            |> Just

                                Data.Floor.RailwayTrack ->
                                    if game.train.tracks > 0 then
                                        move game
                                            |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                                            |> Maybe.map Random.constant

                                    else if
                                        Data.Train.coalNeeded game.train
                                            <= AnyBag.count Data.Item.Coal game.train.items
                                    then
                                        move game
                                            |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                                            |> Maybe.map Random.constant

                                    else
                                        Nothing

                                _ ->
                                    if game.train.tracks > 0 then
                                        game
                                            |> mineAndPlaceTrack
                                            |> Maybe.map returnGame

                                    else
                                        game
                                            |> mine
                                            |> Random.map turnToHQ
                                            |> returnGame
                                            |> Just

                        ( Data.Block.EntityBlock entity, _ ) ->
                            case entity of
                                Data.Entity.Actor id ->
                                    game.world.actors
                                        |> Dict.get id
                                        |> Maybe.andThen
                                            (\( _, actor ) ->
                                                case actor of
                                                    Data.Actor.Wagon wagon ->
                                                        { game
                                                            | train =
                                                                game.train
                                                                    |> Data.Train.addAll
                                                                        (wagon.items
                                                                            |> AnyBag.insert (Config.wagonCost // 2) Data.Item.Iron
                                                                        )
                                                            , world = game.world |> Data.World.removeEntity newPos
                                                        }
                                                            |> Random.constant
                                                            |> returnGame
                                                            |> Just

                                                    Data.Actor.Cave _ ->
                                                        Nothing

                                                    Data.Actor.Mine ->
                                                        Nothing

                                                    Data.Actor.FallingCoal ->
                                                        Nothing

                                                    Data.Actor.Path ->
                                                        Nothing

                                                    Data.Actor.Bomb _ ->
                                                        Random.constant game
                                                            |> returnGame
                                                            |> Just
                                            )

                                _ ->
                                    if game.train.tracks > 0 then
                                        game
                                            |> mine
                                            |> returnGame
                                            |> Just

                                    else
                                        game
                                            |> mine
                                            |> Random.map turnToHQ
                                            |> returnGame
                                            |> Just
                )
            |> Maybe.withDefault (Random.constant ( game, [] ))

    else
        Random.constant ( game, [] )


stockUpAtBase : Game -> ( Game, List Effect )
stockUpAtBase game =
    { game
        | train =
            game.train
                |> Data.Train.addTracks Config.tracksPerTrip
                |> Data.Train.turnAround
    }
        |> move
        |> Maybe.map (\g -> ( g, [ Data.Effect.OpenModal, Data.Effect.LevelUp ] ))
        |> Maybe.withDefault ( game, [] )


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
