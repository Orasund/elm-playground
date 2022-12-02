module Data.Behavior.Excavator exposing (..)

import Config
import Data.Actor
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity
import Data.Excavator exposing (Excavator)
import Data.Position
import Data.World exposing (World)
import Generation
import Random exposing (Generator)


act :
    ( ( Int, Int ), Excavator )
    -> Int
    -> World
    -> Generator ( World, List Effect )
act ( pos, excavator ) id world =
    world
        |> mine pos
        |> Random.map (collectOrRemove pos id)
        |> Data.Effect.genWithNone


collectOrRemove : ( Int, Int ) -> Int -> World -> World
collectOrRemove pos id w =
    let
        list =
            pos
                |> Data.Position.neighborsWithDistance Config.excavatorRadius
                |> List.filterMap
                    (\p ->
                        Data.World.getItem p w
                            |> Maybe.map (\i -> ( p, i ))
                    )
    in
    if List.isEmpty list then
        w |> Data.World.removeEntity pos

    else
        list
            |> List.foldl
                (\( p, item ) world ->
                    getExcavator id world
                        |> Maybe.andThen (Data.Excavator.insertItem item)
                        |> Maybe.map
                            (\excavator ->
                                world
                                    |> Data.World.setActor id
                                        (Data.Actor.Excavator excavator)
                            )
                        |> Maybe.map (Data.World.removeItem p)
                        |> Maybe.withDefault world
                )
                w


mine : ( Int, Int ) -> World -> Generator World
mine pos world =
    pos
        |> Data.Position.neighborsWithDistance Config.excavatorRadius
        |> List.foldl
            (\p ->
                case world |> Data.World.getBlock p of
                    Just (Data.Block.EntityBlock entity) ->
                        case entity of
                            Data.Entity.Vein _ ->
                                Random.andThen (Generation.mine p)

                            _ ->
                                identity

                    _ ->
                        identity
            )
            (Random.constant world)


getExcavator : Int -> World -> Maybe Excavator
getExcavator id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( _, actor ) ->
                case actor of
                    Data.Actor.Excavator excavator ->
                        Just excavator

                    _ ->
                        Nothing
            )
