module Data.Behavior.MovingWater exposing (..)

import Data.Actor
import Data.Block
import Data.Entity
import Data.Momentum exposing (Momentum)
import Data.Position
import Data.World exposing (World)
import Random exposing (Generator)


act :
    ( ( Int, Int ), Int, Momentum )
    -> World
    -> Generator World
act ( _, id, _ ) world =
    world
        --|> setMomentum id
        |> Random.constant
        |> Random.map (getNewPos id)
        |> Random.map
            (Maybe.andThen
                (\{ from, to } ->
                    world
                        |> Data.World.getBlock to
                        |> Maybe.map (\block -> { block = block, from = from, to = to })
                )
            )
        |> Random.map
            (Maybe.map
                (\{ block, from, to } ->
                    case block of
                        Data.Block.EntityBlock entity ->
                            case entity of
                                Data.Entity.Actor id0 ->
                                    world
                                        |> Data.World.pushFrom from id0
                                        |> Maybe.withDefault world
                                        |> move id

                                _ ->
                                    world
                                        |> Data.World.setActor id
                                            (Data.Momentum.fromPoints { from = from, to = to }
                                                |> Data.Momentum.revert
                                                |> Data.Actor.MovingWater
                                            )
                                        |> move id

                        Data.Block.FloorBlock _ ->
                            move id world
                )
            )
        |> Random.map (Maybe.withDefault world)
        |> Random.map (destroyNearLava id)


setMomentum : Int -> World -> Generator World
setMomentum id world =
    world
        |> getNewPos id
        |> Maybe.map
            (\{ from, to } ->
                world
                    |> getFloorNeighbors from
                    |> List.filter
                        ((/=)
                            (to
                                |> Data.Position.vecTo from
                                |> Data.Position.plus from
                            )
                        )
                    |> (\list ->
                            case list of
                                head :: tail ->
                                    if list |> List.member to then
                                        Random.constant to

                                    else
                                        Random.uniform head tail

                                [] ->
                                    Random.constant from
                       )
                    |> Random.map
                        (\p ->
                            world
                                |> Data.World.setActor id
                                    ({ from = from, to = p }
                                        |> Data.Momentum.fromPoints
                                        |> Data.Actor.MovingWater
                                    )
                        )
            )
        |> Maybe.withDefault (Random.constant world)


move : Int -> World -> World
move id world =
    world
        |> getNewPos id
        |> Maybe.map
            (\{ from, to } ->
                if world |> Data.World.isFloor to then
                    moveAndStop { from = from, to = to } world

                else
                    moveAndStop { from = from, to = from } world
            )
        |> Maybe.withDefault world


moveAndStop : { from : ( Int, Int ), to : ( Int, Int ) } -> World -> World
moveAndStop args world =
    world
        |> Data.World.removeEntity args.from
        |> Data.World.insertEntity Data.Entity.Water args.to


destroyNearLava : Int -> World -> World
destroyNearLava id world =
    world
        |> getMomentum id
        |> Maybe.map
            (\( pos, _ ) ->
                world
                    |> getLavaNeighbors pos
                    |> (\list ->
                            if List.isEmpty list then
                                world

                            else
                                list
                                    |> List.foldl Data.World.removeEntity world
                                    |> Data.World.removeEntity pos
                       )
            )
        |> Maybe.withDefault world


getLavaNeighbors : ( Int, Int ) -> World -> List ( Int, Int )
getLavaNeighbors pos world =
    pos
        |> Data.Position.neighbors
        |> List.filter
            (\p ->
                Data.World.getBlock p world
                    == Just (Data.Block.EntityBlock Data.Entity.Lava)
            )


getFloorNeighbors : ( Int, Int ) -> World -> List ( Int, Int )
getFloorNeighbors pos world =
    pos
        |> Data.Position.neighbors
        |> List.filter (\p -> world |> Data.World.isFloor p)


getNewPos : Int -> World -> Maybe { from : ( Int, Int ), to : ( Int, Int ) }
getNewPos id world =
    world
        |> getMomentum id
        |> Maybe.andThen
            (\( pos, momentum ) ->
                momentum.momentum
                    |> Maybe.map (Data.Position.plus pos)
                    |> Maybe.map
                        (\to -> { from = pos, to = to })
            )


getMomentum : Int -> World -> Maybe ( ( Int, Int ), Momentum )
getMomentum id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( pos, actor ) ->
                case actor of
                    Data.Actor.MovingWater momentum ->
                        Just ( pos, momentum )

                    _ ->
                        Nothing
            )
