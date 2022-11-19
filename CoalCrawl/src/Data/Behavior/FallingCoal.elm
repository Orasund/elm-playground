module Data.Behavior.FallingCoal exposing (..)

import Data.Actor
import Data.Entity
import Data.Item
import Data.Position
import Data.World exposing (World)
import Random exposing (Generator)


act : ( Int, Int ) -> World -> Generator World
act pos world =
    pos
        |> Data.Position.neighbors
        |> List.filter (\p -> world |> Data.World.isFloor p)
        |> List.foldl
            (\p ->
                Random.andThen
                    (\w ->
                        Random.weighted ( 1, Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) )
                            [ ( 1 / 2, Data.World.insertActor Data.Actor.FallingCoal ) ]
                            |> Random.map (\fun -> w |> fun p)
                    )
            )
            (Random.constant world)
        |> Random.map (Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) pos)
