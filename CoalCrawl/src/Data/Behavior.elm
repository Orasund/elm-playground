module Data.Behavior exposing (..)

import Data.Actor exposing (Actor)
import Data.Behavior.Bomb
import Data.Behavior.Excavator
import Data.Behavior.Falling
import Data.Behavior.Minecart
import Data.Behavior.MovingWater
import Data.Behavior.Path
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Effect exposing (Effect)
import Data.Game exposing (Game)
import Data.Improvement exposing (Improvement)
import Data.World exposing (World)
import Generation.Cave
import Generation.Mine
import Random exposing (Generator)


passTime : Game -> Generator ( Game, List Effect )
passTime game =
    game
        |> Data.Behavior.Player.act
        |> Data.Effect.andThen
            (\g ->
                g.world
                    |> Data.World.getActors
                    |> List.foldl (\a -> Data.Effect.andThen (actorsAct a game.improvements))
                        (Random.constant ( g.world, [] ))
                    |> Random.map
                        (Tuple.mapFirst
                            (Data.Game.setWorldOf g)
                        )
            )


actorsAct : ( Int, ( ( Int, Int ), Actor ) ) -> List Improvement -> World -> Generator ( World, List Effect )
actorsAct ( id, ( pos, actor ) ) improvements world =
    case actor of
        Data.Actor.Minecart _ ->
            world
                |> Data.Behavior.Minecart.act id improvements

        Data.Actor.Excavator excavator ->
            world
                |> Data.Behavior.Excavator.act
                    ( pos, excavator )
                    id

        Data.Actor.Bomb _ ->
            world
                |> Data.Behavior.Bomb.timePassed id
                |> Data.Effect.genWithNone

        Data.Actor.Helper helper ->
            case helper of
                Data.Actor.Cave caveType ->
                    world
                        |> Data.World.removeEntity pos
                        |> Generation.Cave.exposedCave caveType pos
                        |> Data.Effect.genWithNone

                Data.Actor.Mine ->
                    world
                        |> Data.World.removeEntity pos
                        |> Generation.Mine.mineGenerator pos
                        |> Data.Effect.genWithNone

                Data.Actor.Falling entity ->
                    world
                        |> Data.Behavior.Falling.act entity pos
                        |> Data.Effect.genWithNone

                Data.Actor.Path ->
                    world
                        |> Data.Behavior.Path.act pos
                        |> Data.Effect.genWithNone

        Data.Actor.Train _ ->
            world
                |> Data.Behavior.Train.act improvements id

        Data.Actor.MovingWater _ ->
            world
                |> Data.Behavior.MovingWater.act id
                |> Data.Effect.genWithNone
