module Data.Behavior.Path exposing (..)

import Data.Actor
import Data.Entity
import Data.Position
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


act : ( Int, Int ) -> World -> Generator World
act pos world =
    let
        floorNeighbors =
            pos
                |> Data.Position.neighbors
                |> List.filter (\p -> Data.World.isFloor p world)
                |> List.length
    in
    pos
        |> Data.Position.neighbors
        |> List.filter (\p -> Data.World.getBlock p world == Nothing)
        |> (if floorNeighbors > 1 then
                List.foldl
                    (\p ->
                        Random.andThen
                            (\w ->
                                w
                                    |> Data.World.insertEntity Data.Entity.Wall p
                                    |> Random.constant
                            )
                    )
                    (Random.constant world)

            else
                List.foldl
                    (\p ->
                        Random.andThen
                            (\w ->
                                Random.float 0 1
                                    |> Random.andThen
                                        (\rand ->
                                            if rand < 1 / 3 then
                                                w
                                                    |> Data.World.insertActor Data.Actor.Path p
                                                    |> Random.constant

                                            else
                                                Data.World.Generation.wallGenerator p
                                                    |> Random.map (\fun -> fun p w)
                                        )
                            )
                    )
                    (Random.constant world)
           )
        |> Random.map (Data.World.removeEntity pos)
