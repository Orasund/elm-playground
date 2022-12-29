module Generation.Wall exposing (..)

import Config
import Data.Actor
import Data.Entity
import Data.Item
import Data.World exposing (World)
import Random exposing (Generator)


wallGenerator : ( Int, Int ) -> Generator (( Int, Int ) -> World -> World)
wallGenerator ( x, y ) =
    let
        content i =
            [ Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.IronCave))
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.WaterCave))
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.CollapsedCave))
            , Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.LavaCave))
            ]
                |> List.intersperse (Data.World.insertEntity Data.Entity.CrackedWall)
                |> (++)
                    [ Data.World.insertActor (Data.Actor.Helper (Data.Actor.Cave Data.Actor.CoalCave))
                    , Data.World.insertEntity (Data.Entity.Vein Data.Item.Coal)
                    ]
                |> List.take (i + 1)
                |> (::) (Data.World.insertEntity Data.Entity.CrackedWall)
                |> List.reverse
    in
    ((y // Config.tracksPerTrip) + 1)
        |> (\int ->
                if y <= int then
                    []

                else
                    content int
           )
        |> (\list ->
                case list of
                    [] ->
                        Data.World.insertEntity Data.Entity.Wall
                            |> Random.constant

                    head :: tail ->
                        tail
                            |> List.indexedMap (\i fun -> ( 1 / (2 ^ (toFloat i + 1)), fun ))
                            |> Random.weighted ( 1, head )
           )
