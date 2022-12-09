module Data.Behavior.Train exposing (..)

import AnyBag
import Config
import Data.Actor
import Data.Block exposing (Block)
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity)
import Data.Floor
import Data.Game exposing (Game, getTrain)
import Data.Item exposing (Item)
import Data.Sound
import Data.Train
import Data.World
import Generation
import Random exposing (Generator)


act : Game -> Generator ( Game, List Effect )
act game =
    let
        train =
            game |> Data.Game.getTrain

        newPos =
            train
                |> Data.Train.forwardPos
    in
    if train.pos == Config.hqPos then
        stockUpAtBase game |> Random.constant

    else if train.moving && (game.player.pos /= newPos) then
        Data.World.get newPos game.world
            |> Maybe.andThen (\block -> tryMovingTo ( newPos, block ) game)
            |> Maybe.withDefault (Random.constant ( game, [] ))

    else
        Random.constant ( game, [] )


tryMovingTo : ( ( Int, Int ), ( Block, Maybe Item ) ) -> Game -> Maybe (Generator ( Game, List Effect ))
tryMovingTo ( newPos, block ) game =
    let
        returnGame =
            Random.map (\g -> ( g, [] ))

        train =
            game |> Data.Game.getTrain
    in
    case block of
        ( Data.Block.FloorBlock floor, maybeItem ) ->
            (case floor of
                Data.Floor.Ground ->
                    if train.tracks > 0 then
                        game
                            |> mineAndPlaceTrack
                            |> Maybe.map returnGame

                    else
                        turnToHQ game
                            |> Random.constant
                            |> returnGame
                            |> Just

                Data.Floor.RailwayTrack ->
                    if train.tracks > 0 then
                        move game
                            |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                            |> Maybe.map Random.constant

                    else if
                        Data.Train.coalNeeded train
                            <= AnyBag.count Data.Item.Coal train.items
                    then
                        move game
                            |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                            |> Maybe.map Random.constant

                    else
                        Nothing

                _ ->
                    if train.tracks > 0 then
                        game
                            |> mineAndPlaceTrack
                            |> Maybe.map returnGame

                    else
                        game
                            |> mine
                            |> Random.map turnToHQ
                            |> returnGame
                            |> Just
            )
                |> Maybe.map
                    (Random.map
                        (Tuple.mapFirst
                            (\g ->
                                maybeItem
                                    |> Maybe.map
                                        (\item ->
                                            g
                                                |> Data.Game.getTrain
                                                |> Data.Train.addItem item
                                                |> Data.Game.setTrainOf g
                                        )
                                    |> Maybe.withDefault g
                            )
                        )
                    )

        ( Data.Block.EntityBlock entity, _ ) ->
            collideWith ( newPos, entity ) game


collideWith : ( ( Int, Int ), Entity ) -> Game -> Maybe (Generator ( Game, List Effect ))
collideWith ( newPos, entity ) game =
    case entity of
        Data.Entity.Actor id ->
            game.world
                |> Data.World.getActor id
                |> Maybe.andThen
                    (\( _, actor ) ->
                        case actor of
                            Data.Actor.Minecart wagon ->
                                game
                                    |> Data.Game.getTrain
                                    |> Data.Train.addAll
                                        (List.repeat Config.wagonCost Data.Item.Iron)
                                    |> Data.Game.setTrainOf game
                                    |> Data.Game.setWorld
                                        (game.world
                                            |> Data.World.removeEntity newPos
                                            |> Data.World.insertAllItems wagon.storage.items newPos
                                        )
                                    |> Data.Effect.withNone
                                    |> Just

                            Data.Actor.Excavator _ ->
                                game
                                    |> Data.Game.getTrain
                                    |> Data.Train.addAll
                                        (List.repeat Config.excavatorCost Data.Item.Iron)
                                    |> Data.Game.setTrainOf game
                                    |> (\g ->
                                            { g
                                                | world = game.world |> Data.World.removeEntity newPos
                                            }
                                       )
                                    |> Data.Effect.withNone
                                    |> Just

                            Data.Actor.Helper _ ->
                                Nothing

                            Data.Actor.Train _ ->
                                Nothing

                            Data.Actor.Bomb _ ->
                                game
                                    |> Data.Effect.withNone
                                    |> Just

                            Data.Actor.MovingWater _ ->
                                Nothing
                    )

        _ ->
            if (game |> Data.Game.getTrain |> .tracks) > 0 then
                game
                    |> mine
                    |> Data.Effect.genWithNone
                    |> Just

            else
                game
                    |> mine
                    |> Random.map turnToHQ
                    |> Data.Effect.genWithNone
                    |> Just


stockUpAtBase : Game -> ( Game, List Effect )
stockUpAtBase game =
    game
        |> Data.Game.getTrain
        |> Data.Train.addTracks Config.tracksPerTrip
        |> Data.Train.turnDownwards
        |> Data.Game.setTrainOf game
        |> move
        |> Maybe.map (\g -> ( g, [ Data.Effect.OpenModal, Data.Effect.LevelUp ] ))
        |> Maybe.withDefault ( game, [] )


turnToHQ : Game -> Game
turnToHQ game =
    game
        |> Data.Game.getTrain
        |> Data.Train.turnUpwards
        |> Data.Game.setTrainOf game


mineAndPlaceTrack : Game -> Maybe (Generator Game)
mineAndPlaceTrack game =
    let
        newPos =
            game
                |> Data.Game.getTrain
                |> Data.Train.forwardPos
    in
    game
        |> Data.Game.getTrain
        |> Data.Train.removeTrack
        |> Maybe.map (Data.Game.setTrainOf game)
        |> Maybe.map mine
        |> Maybe.map
            (Random.map
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
            Data.Train.forwardPos (game |> getTrain)
    in
    game
        |> getTrain
        |> Data.Train.removeItem 1 Data.Item.Coal
        |> Maybe.map (Data.Game.setTrainOf game)
        |> Maybe.map
            (\g ->
                g.world
                    |> Data.World.moveActorTo newPos game.trainId
                    |> (\world -> { g | world = world })
            )
        |> Maybe.map (\g -> g |> Data.Game.getTrain |> Data.Train.move |> Data.Game.setTrainOf g)


mine : Game -> Generator Game
mine game =
    let
        newPos =
            game
                |> Data.Game.getTrain
                |> Data.Train.forwardPos
    in
    newPos
        |> List.singleton
        -- :: Data.Position.neighbors newPos
        |> List.foldl
            (\pos ->
                Random.andThen (Generation.mine pos)
            )
            (Random.constant game.world)
        |> Random.map (\world -> { game | world = world })
