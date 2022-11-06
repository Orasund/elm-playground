module Data.Behavior.Bomb exposing (..)

import Data.Actor
import Data.Position
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


timePassed : Int -> World -> Generator World
timePassed id world =
    world
        |> Data.World.getActor id
        |> Maybe.map
            (\( pos, actor ) ->
                case actor of
                    Data.Actor.Bomb bomb ->
                        if bomb.explodesIn > 0 then
                            world
                                |> Data.World.updateActor id
                                    (\_ -> Data.Actor.Bomb { explodesIn = bomb.explodesIn - 1 })
                                |> Random.constant

                        else
                            Data.Position.neighbors pos
                                |> List.foldl (\p -> Random.andThen (Data.World.Generation.mine p))
                                    (Random.constant (Data.World.removeEntity pos world))

                    _ ->
                        Random.constant world
            )
        |> Maybe.withDefault (Random.constant world)
