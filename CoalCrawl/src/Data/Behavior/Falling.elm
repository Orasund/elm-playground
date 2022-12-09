module Data.Behavior.Falling exposing (..)

import Data.Actor
import Data.Entity exposing (Entity)
import Data.Position
import Data.World exposing (World)
import Random exposing (Generator)


act : Entity -> ( Int, Int ) -> World -> Generator World
act entity pos world =
    pos
        |> Data.Position.neighbors
        |> List.filter (\p -> world |> Data.World.isFloor p)
        |> List.foldl
            (\p ->
                Random.andThen
                    (\w ->
                        Random.weighted ( 1, Data.World.insertEntity entity )
                            [ ( 1 / 4, Data.World.insertActor (Data.Actor.Helper (Data.Actor.Falling entity)) ) ]
                            |> Random.map (\fun -> w |> fun p)
                    )
            )
            (Random.constant world)
        |> Random.map (Data.World.insertEntity entity pos)
