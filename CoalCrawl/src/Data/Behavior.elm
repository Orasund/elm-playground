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
        Data.Actor.Minecart wagon ->
            wagon.movedFrom
                |> Maybe.map
                    (\movedFrom ->
                        game
                            |> Data.Behavior.Minecart.act
                                { backPos = movedFrom }
                                id
                            |> Random.constant
                    )
                |> Maybe.withDefault (Data.Effect.withNone game)

        Data.Actor.Excavator excavator ->
            case excavator.momentum of
                Just _ ->
                    game.world
                        |> Data.Behavior.Excavator.move
                            ( pos, excavator )
                            id
                        |> Random.map (Tuple.mapFirst (\world -> { game | world = world }))

                Nothing ->
                    Data.Effect.withNone game

        Data.Actor.Bomb _ ->
            game.world
                |> Data.Behavior.Bomb.timePassed id
                |> Random.map (\world -> { game | world = world })
                |> Data.Effect.genWithNone

        Data.Actor.Helper helper ->
            case helper of
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

                Data.Actor.Falling item ->
                    game.world
                        |> Data.Behavior.FallingCoal.act item pos
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
