module Data.Behavior.WaterSource exposing (..)

import Data.Entity
import Data.Position
import Data.World exposing (World)


act : ( Int, Int ) -> World -> World
act pos world =
    pos
        |> Data.Position.neighbors
        |> List.foldl
            (\p ->
                Data.World.updateEntity p
                    (\maybe ->
                        maybe
                            |> Maybe.withDefault Data.Entity.Water
                            |> Just
                    )
            )
            world
