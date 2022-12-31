module Data.Behavior.MovingWater exposing (..)

import Data.Actor
import Data.Block
import Data.Entity
import Data.Momentum exposing (Momentum)
import Data.Position
import Data.World exposing (World)
import Random exposing (Generator)


act : Int -> World -> Generator World
act id w =
    case destroyNearLava id w of
        Just world ->
            Random.constant world

        Nothing ->
            w
                |> setMomentum id
                |> Random.map
                    (\world ->
                        world
                            |> getNewPos id
                            |> Maybe.andThen
                                (\{ from, to } ->
                                    world
                                        |> Data.World.getBlock to
                                        |> Maybe.map (\block -> { block = block, from = from, to = to })
                                )
                            |> Maybe.map
                                (\{ block, from, to } ->
                                    case block of
                                        Data.Block.FloorBlock _ ->
                                            move id world

                                        _ ->
                                            world
                                                |> Data.World.push { from = from, pos = to }
                                                |> Maybe.withDefault world
                                                |> Data.World.setActor id
                                                    (Data.Momentum.fromPoints { from = from, to = to }
                                                        |> Data.Momentum.revert
                                                        |> Data.Actor.MovingWater
                                                    )
                                                |> move id
                                )
                    )
                |> Random.map (Maybe.withDefault w)


setMomentum : Int -> World -> Generator World
setMomentum id world =
    world
        |> getMomentum id
        |> Maybe.map
            (\( from, momentum ) ->
                let
                    backwardPos =
                        momentum
                            |> Data.Momentum.revert
                            |> Data.Momentum.applyTo from

                    forwardPos =
                        momentum
                            |> Data.Momentum.applyTo from
                in
                world
                    |> getFloorNeighbors from
                    |> List.filter ((/=) backwardPos)
                    |> (\list ->
                            case list of
                                head :: tail ->
                                    if list |> List.member forwardPos then
                                        Random.constant forwardPos

                                    else
                                        Random.uniform head tail

                                [] ->
                                    Random.constant forwardPos
                       )
                    |> Random.map
                        (\p ->
                            Data.World.setActor id
                                ({ from = from, to = p }
                                    |> Data.Momentum.fromPoints
                                    |> Data.Actor.MovingWater
                                )
                                world
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


destroyNearLava : Int -> World -> Maybe World
destroyNearLava id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( pos, _ ) ->
                world
                    |> getLavaNeighbors pos
                    |> (\list ->
                            if List.isEmpty list then
                                Nothing

                            else
                                list
                                    |> List.foldl Data.World.removeEntity world
                                    |> Data.World.removeEntity pos
                                    |> Just
                       )
            )


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
        |> Maybe.map
            (\( pos, momentum ) ->
                momentum
                    |> Data.Momentum.applyTo pos
                    |> (\to -> { from = pos, to = to })
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
