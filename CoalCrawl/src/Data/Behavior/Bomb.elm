module Data.Behavior.Bomb exposing (..)

import Data.Actor exposing (Actor(..))
import Data.Bomb exposing (Bomb)
import Data.Position
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


timePassed : Int -> World -> Generator World
timePassed id world =
    world
        |> getBomb id
        |> Maybe.map (Tuple.mapSecond Data.Bomb.tick)
        |> Maybe.map
            (\( pos, maybeBomb ) ->
                case maybeBomb of
                    Just bomb ->
                        world
                            |> Data.World.setActor id
                                (Data.Actor.Bomb bomb)
                            |> Random.constant

                    Nothing ->
                        Data.Position.neighbors pos
                            |> List.foldl
                                (\p ->
                                    Random.andThen (Data.World.Generation.mine p)
                                )
                                (Random.constant (Data.World.removeEntity pos world))
            )
        |> Maybe.withDefault (Random.constant world)


getBomb : Int -> World -> Maybe ( ( Int, Int ), Bomb )
getBomb id world =
    world
        |> Data.World.getActor id
        |> Maybe.andThen
            (\( pos, actor ) ->
                case actor of
                    Data.Actor.Bomb bomb ->
                        Just ( pos, bomb )

                    _ ->
                        Nothing
            )
