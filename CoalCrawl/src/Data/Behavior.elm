module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Actor exposing (Actor)
import Data.Behavior.Bomb
import Data.Behavior.FallingCoal
import Data.Behavior.Path
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.Wagon
import Data.Effect exposing (Effect)
import Data.Game exposing (Game)
import Data.Item
import Data.World
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
                        (Random.constant ( g, [] ))
            )
        |> Random.map (\( g, l ) -> ( g, promt g ++ l ))


actorsAct : ( Int, ( ( Int, Int ), Actor ) ) -> Game -> Generator ( Game, List Effect )
actorsAct ( id, ( pos, actor ) ) game =
    case actor of
        Data.Actor.Wagon wagon ->
            wagon.movedFrom
                |> Maybe.map
                    (\movedFrom ->
                        game
                            |> Data.Behavior.Wagon.act
                                { backPos = movedFrom }
                                id
                            |> Random.constant
                    )
                |> Maybe.withDefault (Data.Effect.withNone game)

        Data.Actor.Cave caveType ->
            game.world
                |> Data.World.removeEntity pos
                |> Data.World.Generation.exposedCave caveType pos
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone

        Data.Actor.Mine ->
            game.world
                |> Data.World.removeEntity pos
                |> Data.World.Generation.mineGenerator pos
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone

        Data.Actor.FallingCoal ->
            game.world
                |> Data.Behavior.FallingCoal.act pos
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone

        Data.Actor.Bomb _ ->
            game.world
                |> Data.Behavior.Bomb.timePassed id
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone

        Data.Actor.Path ->
            game.world
                |> Data.Behavior.Path.act pos
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone


promt : Game -> List Effect
promt game =
    if AnyBag.count Data.Item.Iron game.train.items >= Config.wagonCost then
        "You can build a wagon (W) when standing on an empty ground"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else if not (AnyBag.member Data.Item.Coal game.train.items) then
        "Put coal (C) into the Train (T)"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else
        []
