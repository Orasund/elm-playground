module Data.Behavior.Excavator exposing (..)

import Data.Actor
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Excavator exposing (Excavator)
import Data.Position
import Data.Sound
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


move :
    ( ( Int, Int ), Excavator )
    -> Int
    -> World
    -> Generator ( World, List Effect )
move ( pos, excavator ) id world =
    excavator.momentum
        |> Maybe.map
            (\momentum ->
                let
                    newPos =
                        pos |> Data.Position.plus momentum
                in
                case world |> Data.World.getBlock newPos of
                    Just (Data.Block.EntityBlock entity) ->
                        case entity of
                            Data.Entity.Vein _ ->
                                world
                                    |> Data.World.Generation.mine newPos
                                    |> Random.map (Data.World.moveActorTo newPos id)
                                    |> Random.map (\w -> ( w, [ Data.Effect.PlaySound Data.Sound.Mine ] ))

                            _ ->
                                world
                                    |> Data.World.updateActor id
                                        (\_ ->
                                            excavator
                                                |> Data.Excavator.reverse
                                                |> Data.Actor.Excavator
                                        )
                                    |> Data.Effect.withNone

                    Just (Data.Block.FloorBlock _) ->
                        world
                            |> Data.World.moveActorTo newPos id
                            |> Data.Effect.withNone

                    Nothing ->
                        world |> Data.Effect.withNone
            )
        |> Maybe.withDefault (Data.Effect.withNone world)
