module Minecart exposing (..)

import Data.Actor exposing (Actor)
import Data.Behavior
import Data.Entity
import Data.Floor
import Data.Game exposing (Game)
import Data.Item
import Data.Minecart
import Data.Player
import Data.Storage
import Data.Train
import Data.World
import Expect
import Fuzz
import Random exposing (Generator)
import Test exposing (..)


minecartCanMove : Test
minecartCanMove =
    let
        minecart : Actor
        minecart =
            Data.Actor.Minecart Data.Minecart.emptyWagon
    in
    fuzz
        (Data.World.empty
            |> Data.World.insertActorAt ( 0, 1 ) minecart
            |> Data.World.insertFloorAt ( 0, 2 ) Data.Floor.Ground
            |> Data.Game.setWorldOf Data.Game.new
            |> (\g -> { g | player = Data.Player.fromPos ( 0, 0 ) |> Data.Player.startMovingTo ( 0, 2 ) })
            |> passTime 1
            |> Fuzz.fromGenerator
        )
        "minecart can move"
        (\g ->
            Expect.equal (Just minecart)
                (g.world
                    |> Data.World.getActorAt ( 0, 2 )
                    |> Maybe.map Tuple.second
                )
        )


movesOnTracks : Test
movesOnTracks =
    let
        minecart : Actor
        minecart =
            Data.Actor.Minecart Data.Minecart.emptyWagon
    in
    fuzz
        (Data.World.empty
            |> Data.World.insertActorAt ( 0, 1 ) minecart
            |> Data.World.insertFloor Data.Floor.Track ( 0, 2 )
            |> Data.World.insertFloor Data.Floor.RailwayTrack ( 0, 3 )
            |> Data.World.insertFloor Data.Floor.Track ( 0, 4 )
            |> Data.Game.setWorldOf Data.Game.new
            |> (\g -> { g | player = Data.Player.fromPos ( 0, 0 ) |> Data.Player.startMovingTo ( 0, 1 ) })
            |> passTime 20
            |> Fuzz.fromGenerator
        )
        "minecart can move on tracks"
        (\g ->
            Expect.equal (Just minecart)
                (g.world
                    |> Data.World.getActorAt ( 0, 1 )
                    |> Maybe.map Tuple.second
                )
        )


playerCanPutItemInCart : Test
playerCanPutItemInCart =
    let
        item =
            Data.Item.Coal
    in
    fuzz
        (Data.World.empty
            |> Data.World.insertActorAt ( 0, 1 ) (Data.Actor.Minecart Data.Minecart.emptyWagon)
            |> Data.Game.setWorldOf Data.Game.new
            |> (\g ->
                    { g
                        | player =
                            Data.Player.fromPos ( 0, 0 )
                                |> Data.Player.startMovingTo ( 0, 1 )
                                |> (\p ->
                                        p
                                            |> Data.Player.hold item
                                            |> Maybe.withDefault p
                                   )
                    }
               )
            |> passTime 20
            |> Fuzz.fromGenerator
        )
        "player can put item in minecart"
        (\g ->
            Expect.equal
                (Data.Minecart.emptyWagon
                    |> Data.Minecart.insert item
                    |> Maybe.map Tuple.first
                    |> Maybe.map Data.Actor.Minecart
                )
                (g.world
                    |> Data.World.getActorAt ( 0, 1 )
                    |> Maybe.map Tuple.second
                )
        )


minecartCanLoadfromContainer : Test
minecartCanLoadfromContainer =
    let
        item =
            Data.Item.Coal
    in
    fuzz
        (Data.World.empty
            |> Data.World.insertFloorAt ( 0, 0 ) Data.Floor.Ground
            |> Data.World.insertFloorAt ( 0, 1 ) Data.Floor.Ground
            |> Data.World.insertFloorAt ( 0, 2 ) Data.Floor.Ground
            |> Data.World.insertActorAt ( 0, 1 ) (Data.Actor.Minecart Data.Minecart.emptyWagon)
            |> Data.World.insertEntityAt ( 1, 2 )
                (Data.Storage.empty 1
                    |> Data.Storage.insert item
                    |> Maybe.withDefault (Data.Storage.empty 1)
                    |> Data.Entity.Container
                )
            |> Data.Game.setWorldOf Data.Game.new
            |> (\g ->
                    { g
                        | player =
                            Data.Player.fromPos ( 0, 0 )
                                |> Data.Player.startMovingTo ( 0, 2 )
                    }
               )
            |> passTime 20
            |> Fuzz.fromGenerator
        )
        "minecart can load items from container"
        (\g ->
            Expect.equal
                (Data.Minecart.emptyWagon
                    |> Data.Minecart.insert item
                    |> Maybe.map Tuple.first
                    |> Maybe.map Data.Actor.Minecart
                )
                (g.world
                    |> Data.World.getActorAt ( 0, 2 )
                    |> Maybe.map Tuple.second
                )
        )


minecartCanTransfer : Test
minecartCanTransfer =
    let
        item =
            Data.Item.Coal

        train =
            Data.Train.fromPos ( 0, 3 )
    in
    fuzz
        (Data.World.empty
            |> Data.World.insertFloorAt ( 0, 0 ) Data.Floor.Ground
            |> Data.World.insertActorAt ( 0, 1 )
                (Data.Minecart.emptyWagon
                    |> Data.Minecart.insert item
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault Data.Minecart.emptyWagon
                    |> Data.Actor.Minecart
                )
            |> Data.World.insertFloorAt ( 0, 1 ) Data.Floor.Ground
            |> Data.World.insertFloorAt ( 0, 3 ) Data.Floor.Ground
            |> Data.World.insertActorAt ( 0, 3 ) (Data.Actor.Train train)
            |> Data.World.insertFloorAt ( 0, 2 ) Data.Floor.Ground
            |> Data.Game.setWorldOf Data.Game.new
            |> (\g ->
                    { g
                        | player =
                            Data.Player.fromPos ( 0, 0 )
                                |> Data.Player.startMovingTo ( 0, 2 )
                    }
               )
            |> passTime 20
            |> Fuzz.fromGenerator
        )
        "minecart can transfer items"
        (\g ->
            Expect.equal
                (train
                    |> Data.Train.addItem item
                    |> Data.Actor.Train
                    |> Just
                )
                (g.world
                    |> Data.World.getActorAt ( 0, 3 )
                    |> Maybe.map Tuple.second
                )
        )



---------------------------------------------------------------------
-- Utility Functions
---------------------------------------------------------------------


passTime : Int -> Game -> Generator Game
passTime times game =
    List.repeat times ()
        |> List.foldl
            (\() g ->
                g
                    |> Random.andThen Data.Behavior.passTime
                    |> Random.map Tuple.first
            )
            (Random.constant game)


fuzz =
    Test.fuzzWith
        { runs = 10
        , distribution = Test.noDistribution
        }
