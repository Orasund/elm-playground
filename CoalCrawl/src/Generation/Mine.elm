module Generation.Mine exposing (..)

import Data.Actor
import Data.Entity
import Data.Floor
import Data.Item
import Data.Minecart
import Data.Position
import Data.World exposing (World)
import Generation.Wall
import Random exposing (Generator)


mineGenerator : ( Int, Int ) -> World -> Generator World
mineGenerator pos world =
    case
        Data.Position.neighbors pos
            |> List.filter
                (\p ->
                    Data.World.get p world
                        == Nothing
                )
    of
        head :: tail ->
            Random.weighted ( 1, False ) [ ( 1 / 4, True ) ]
                |> Random.andThen
                    (\stop ->
                        if stop then
                            Random.weighted ( 1, Data.Item.Coal ) []
                                |> Random.map Data.Minecart.fullWagon
                                |> Random.map Data.Actor.Minecart
                                |> Random.map
                                    (\actor ->
                                        pos
                                            |> Data.Position.neighbors
                                            |> List.filter
                                                (\p ->
                                                    Data.World.get p world
                                                        == Nothing
                                                )
                                            |> List.foldl
                                                (\p ->
                                                    Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal) p
                                                )
                                                world
                                            |> Data.World.insertActor actor pos
                                    )

                        else
                            Random.uniform head tail
                                |> Random.andThen
                                    (\nextPos ->
                                        pos
                                            |> Data.Position.neighbors
                                            |> List.filter
                                                (\p ->
                                                    Data.World.get p world
                                                        == Nothing
                                                )
                                            |> List.foldl
                                                (\p ->
                                                    Random.andThen
                                                        (\it ->
                                                            if p == nextPos then
                                                                it
                                                                    |> Data.World.insertActor (Data.Actor.Helper Data.Actor.Mine) p
                                                                    |> Random.constant

                                                            else
                                                                Generation.Wall.wallGenerator p
                                                                    |> Random.map (\fun -> fun p it)
                                                        )
                                                )
                                                (Random.constant world)
                                            |> Random.map (Data.World.removeEntity pos)
                                            |> Random.map (Data.World.insertFloor Data.Floor.Track pos)
                                    )
                    )

        [] ->
            world
                |> Data.World.removeEntity pos
                |> Random.constant
