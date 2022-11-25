module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Behavior.Bomb
import Data.Behavior.Excavator
import Data.Behavior.FallingCoal
import Data.Behavior.Minecart
import Data.Behavior.Path
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.WaterSource
import Data.Effect exposing (Effect)
import Data.Game exposing (Game)
import Data.Item
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


passTime : Game -> Generator ( Game, List Effect )
passTime game =
    game
        |> Data.Behavior.Player.act
        |> Data.Effect.andThen Data.Behavior.Train.act
        |> Data.Effect.andThen
            (\g ->
                g.world
                    |> Data.World.getActors
                    |> List.foldl (\a -> Data.Effect.andThen (actorsAct a))
                        (Random.constant ( g.world, [] ))
                    |> Random.map
                        (Tuple.mapFirst
                            (Data.Game.setWorldOf g)
                        )
            )
        |> Random.map (\( g, l ) -> ( g, promt g ++ l ))


actorsAct : ( Int, ( ( Int, Int ), Actor ) ) -> World -> Generator ( World, List Effect )
actorsAct ( id, ( pos, actor ) ) world =
    case actor of
        Data.Actor.Minecart _ ->
            world
                |> Data.Behavior.Minecart.act id
                |> Maybe.withDefault ( world, [] )
                |> Random.constant

        Data.Actor.Excavator excavator ->
            case excavator.momentum of
                Just _ ->
                    world
                        |> Data.Behavior.Excavator.move
                            ( pos, excavator )
                            id

                Nothing ->
                    Data.Effect.withNone world

        Data.Actor.Bomb _ ->
            world
                |> Data.Behavior.Bomb.timePassed id
                |> Data.Effect.genWithNone

        Data.Actor.Helper helper ->
            case helper of
                Data.Actor.Cave caveType ->
                    world
                        |> Data.World.removeEntity pos
                        |> Data.World.Generation.exposedCave caveType pos
                        |> Data.Effect.genWithNone

                Data.Actor.Mine ->
                    world
                        |> Data.World.removeEntity pos
                        |> Data.World.Generation.mineGenerator pos
                        |> Data.Effect.genWithNone

                Data.Actor.Falling item ->
                    world
                        |> Data.Behavior.FallingCoal.act item pos
                        |> Data.Effect.genWithNone

                Data.Actor.Path ->
                    world
                        |> Data.Behavior.Path.act pos
                        |> Data.Effect.genWithNone

        Data.Actor.Train _ ->
            Data.Effect.withNone world

        Data.Actor.WaterSource ->
            world
                |> Data.Behavior.WaterSource.act pos
                |> Data.Effect.withNone


promt : Game -> List Effect
promt game =
    let
        train =
            game |> Data.Game.getTrain
    in
    if AnyBag.count Data.Item.Iron train.items >= Config.wagonCost then
        "You can build a wagon (W) when standing on an empty ground"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else if not (AnyBag.member Data.Item.Coal train.items) then
        "Put coal (C) into the Train (T)"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else
        []
