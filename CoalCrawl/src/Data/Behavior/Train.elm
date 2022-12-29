module Data.Behavior.Train exposing (..)

import Config
import Data.Actor
import Data.Block exposing (Block)
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity)
import Data.Floor
import Data.Improvement exposing (Improvement)
import Data.Item exposing (Item)
import Data.Modal
import Data.Sound
import Data.Train exposing (Train)
import Data.World exposing (World)
import Generation
import ListBag
import Random exposing (Generator)
import Random.List


act : List Improvement -> Int -> World -> Generator ( World, List Effect )
act improvements id world =
    world
        |> getTrain id
        |> Maybe.andThen
            (\train ->
                let
                    newPos =
                        train
                            |> Data.Train.forwardPos
                in
                if train.pos == Config.hqPos then
                    world
                        |> stockUpAtBase id improvements
                        |> Just

                else if train.moving && ListBag.member Data.Item.Coal train.items then
                    world
                        |> Data.World.get newPos
                        |> Maybe.andThen (\block -> tryMovingTo ( newPos, block ) id improvements world)

                else
                    Nothing
            )
        |> Maybe.withDefault (Data.Effect.withNone world)


tryMovingTo : ( ( Int, Int ), ( Block, Maybe Item ) ) -> Int -> List Improvement -> World -> Maybe (Generator ( World, List Effect ))
tryMovingTo ( newPos, block ) id improvements world =
    world
        |> getTrain id
        |> Maybe.andThen
            (\train ->
                case block of
                    ( Data.Block.FloorBlock floor, maybeItem ) ->
                        (case floor of
                            Data.Floor.Ground ->
                                if train.tracks > 0 then
                                    world
                                        |> mineAndPlaceTrack id
                                        |> Maybe.map Data.Effect.genWithNone

                                else
                                    world
                                        |> turnToHQ id
                                        |> Maybe.map Data.Effect.withNone

                            Data.Floor.RailwayTrack ->
                                if train.tracks > 0 then
                                    world
                                        |> move id
                                        |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                                        |> Maybe.map Random.constant

                                else if
                                    Data.Train.coalNeeded train
                                        <= ListBag.count Data.Item.Coal train.items
                                then
                                    world
                                        |> move id
                                        |> Maybe.map (\g -> ( g, [ Data.Effect.PlaySound Data.Sound.MovingTrain ] ))
                                        |> Maybe.map Random.constant

                                else
                                    Nothing

                            _ ->
                                if train.tracks > 0 then
                                    world
                                        |> mineAndPlaceTrack id
                                        |> Maybe.map Data.Effect.genWithNone

                                else
                                    world
                                        |> turnToHQ id
                                        |> Maybe.andThen (mine id)
                                        |> Maybe.map Data.Effect.genWithNone
                        )
                            |> Maybe.map
                                (Random.map
                                    (Tuple.mapFirst
                                        (\w ->
                                            maybeItem
                                                |> Maybe.andThen
                                                    (\item ->
                                                        w
                                                            |> getTrain id
                                                            |> Maybe.map (Data.Train.addItem item)
                                                            |> Maybe.map (setTrainOf w id)
                                                    )
                                                |> Maybe.withDefault w
                                        )
                                    )
                                )

                    ( Data.Block.EntityBlock entity, _ ) ->
                        if
                            (train.lookingUp
                                && Data.Train.coalNeeded train
                                <= ListBag.count Data.Item.Coal train.items
                            )
                                || (not train.lookingUp
                                        && ListBag.member Data.Item.Coal train.items
                                   )
                        then
                            collideWith ( newPos, entity ) id world

                        else
                            Nothing
            )
        |> Maybe.map (Random.map (Tuple.mapFirst (collect id improvements)))


collect : Int -> List Improvement -> World -> World
collect id improvements world =
    if List.member Data.Improvement.TrainCanCollect improvements then
        world
            |> getTrain id
            |> Maybe.andThen
                (\train ->
                    train
                        |> Data.Train.forwardPos
                        |> (\pos -> Data.World.getItem pos world)
                        |> Maybe.map (\item -> train |> Data.Train.addItem item)
                        |> Maybe.map (setTrainOf world id)
                )
            |> Maybe.withDefault world

    else
        world


collideWith : ( ( Int, Int ), Entity ) -> Int -> World -> Maybe (Generator ( World, List Effect ))
collideWith ( newPos, entity ) id world =
    world
        |> getTrain id
        |> Maybe.andThen
            (\train ->
                case entity of
                    Data.Entity.Actor targetId ->
                        world
                            |> Data.World.getActor targetId
                            |> Maybe.andThen
                                (\( _, actor ) ->
                                    case actor of
                                        Data.Actor.Minecart wagon ->
                                            train
                                                |> Data.Train.addAll
                                                    (List.repeat Config.wagonCost Data.Item.Iron)
                                                |> setTrainOf world id
                                                |> Data.World.removeEntity newPos
                                                |> Data.World.insertAllItems wagon.storage.items newPos
                                                |> Data.Effect.withNone
                                                |> Just

                                        Data.Actor.Helper _ ->
                                            Nothing

                                        Data.Actor.Train _ ->
                                            Nothing

                                        Data.Actor.Bomb _ ->
                                            world
                                                |> Data.Effect.withNone
                                                |> Just

                                        Data.Actor.MovingWater _ ->
                                            Nothing
                                )

                    _ ->
                        world
                            |> (if train.tracks > 0 then
                                    Just

                                else
                                    turnToHQ id
                               )
                            |> Maybe.andThen (mine id)
                            |> Maybe.map Data.Effect.genWithNone
            )


stockUpAtBase : Int -> List Improvement -> World -> Generator ( World, List Effect )
stockUpAtBase id improvements world =
    world
        |> getTrain id
        |> Maybe.andThen
            (\t ->
                t
                    |> Data.Train.addTracks Config.tracksPerTrip
                    |> (if List.member Data.Improvement.GetOneGoldEachLevel improvements then
                            Data.Train.addItem Data.Item.Gold

                        else
                            identity
                       )
                    |> Data.Train.turnDownwards
                    |> Data.Actor.Train
                    |> (\train -> Data.World.setActor id train world)
                    |> move id
            )
        |> Maybe.map
            (\w ->
                Data.Improvement.asList
                    |> List.filter
                        (\e ->
                            improvements
                                |> List.member e
                                |> not
                        )
                    |> Random.List.choices 2
                    |> Random.map Tuple.first
                    |> Random.map
                        (\list ->
                            ( w
                            , [ Data.Modal.levelUp list
                                    |> Data.Effect.OpenModal
                              , Data.Effect.LevelUp
                              ]
                            )
                        )
            )
        |> Maybe.withDefault (Data.Effect.withNone world)


turnToHQ : Int -> World -> Maybe World
turnToHQ id world =
    world
        |> getTrain id
        |> Maybe.map Data.Train.turnUpwards
        |> Maybe.map (setTrainOf world id)


mineAndPlaceTrack : Int -> World -> Maybe (Generator World)
mineAndPlaceTrack id world =
    world
        |> getTrain id
        |> Maybe.andThen
            (\train ->
                let
                    newPos =
                        train
                            |> Data.Train.forwardPos
                in
                train
                    |> Data.Train.removeTrack
                    |> Maybe.map Data.Actor.Train
                    |> Maybe.map (\t -> Data.World.setActor id t world)
                    |> Maybe.andThen (mine id)
                    |> Maybe.map
                        (Random.map
                            (Data.World.insertFloorAt newPos Data.Floor.RailwayTrack)
                        )
            )


move : Int -> World -> Maybe World
move id world =
    world
        |> getTrain id
        |> Maybe.map
            (\train ->
                ( train |> Data.Train.forwardPos
                , train
                )
            )
        |> Maybe.andThen
            (\( newPos, train ) ->
                train
                    |> Data.Train.removeItem 1 Data.Item.Coal
                    |> Maybe.map Data.Train.move
                    |> Maybe.map (setTrainOf world id)
                    |> Maybe.map (Data.World.moveActorTo newPos id)
            )


mine : Int -> World -> Maybe (Generator World)
mine id world =
    world
        |> getTrain id
        |> Maybe.map Data.Train.forwardPos
        |> Maybe.map (\pos -> Generation.mine pos world)


getTrain : Int -> World -> Maybe Train
getTrain id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( _, actor ) ->
                case actor of
                    Data.Actor.Train train ->
                        Just train

                    _ ->
                        Nothing
            )


setTrainOf : World -> Int -> Train -> World
setTrainOf world id train =
    world |> Data.World.setActor id (Data.Actor.Train train)
