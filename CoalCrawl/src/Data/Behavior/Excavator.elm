module Data.Behavior.Excavator exposing (..)

import Data.Actor
import Data.Behavior.Minecart
import Data.Block
import Data.Effect exposing (Effect)
import Data.Entity exposing (Entity)
import Data.Excavator exposing (Excavator)
import Data.Position
import Data.Sound
import Data.World exposing (World)
import Data.World.Generation
import Random exposing (Generator)


move :
    ( ( Int, Int ), Excavator )
    -> Int
    -> World
    -> Generator ( World, List Effect )
move ( pos, excavator ) id world =
    excavator.momentum
        |> Maybe.andThen
            (\momentum ->
                let
                    newPos =
                        pos |> Data.Position.plus momentum
                in
                world
                    |> Data.World.getBlock newPos
                    |> Maybe.map
                        (\block ->
                            { newPos = newPos
                            , block = block
                            }
                        )
            )
        |> Maybe.map
            (\{ newPos, block } ->
                case block of
                    Data.Block.EntityBlock entity ->
                        world
                            |> collideWith ( newPos, entity )
                                ( pos, id, excavator )

                    Data.Block.FloorBlock _ ->
                        world
                            |> mine pos
                            |> Random.map (Data.World.moveActorTo newPos id)
                            |> Data.Effect.genWithNone
            )
        |> Maybe.map
            (Data.Effect.map
                (\w ->
                    ( w
                        |> collect ( pos, id, excavator )
                    , []
                    )
                )
            )
        |> Maybe.withDefault (Data.Effect.withNone world)


collideWith : ( ( Int, Int ), Entity ) -> ( ( Int, Int ), Int, Excavator ) -> World -> Generator ( World, List Effect )
collideWith ( newPos, entity ) ( pos, id, excavator ) world =
    (case entity of
        Data.Entity.Vein _ ->
            world
                |> mine pos
                |> Random.map (\w -> ( w, [ Data.Effect.PlaySound Data.Sound.Mine ] ))
                |> Just

        Data.Entity.Actor id0 ->
            case world |> Data.World.getActor id0 of
                Just ( _, Data.Actor.Minecart minecart ) ->
                    world
                        |> Data.Behavior.Minecart.move id0
                            ( newPos, minecart )
                        |> Maybe.withDefault ( world, [] )
                        |> Tuple.mapFirst
                            (\w ->
                                w
                                    |> Data.World.transfer { from = pos, to = newPos }
                                    |> Maybe.withDefault w
                            )
                        |> Random.constant
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing
    )
        |> (\maybe ->
                case maybe of
                    Just a ->
                        a

                    Nothing ->
                        world
                            |> Data.World.updateActor id
                                (\_ ->
                                    excavator
                                        |> Data.Excavator.reverse
                                        |> Data.Actor.Excavator
                                )
                            |> mine pos
                            |> Data.Effect.genWithNone
           )


collect : ( ( Int, Int ), Int, Excavator ) -> World -> World
collect ( pos, id, e ) w =
    pos
        |> Data.Position.neighbors
        |> List.foldl
            (\p world ->
                world
                    |> Data.World.getItem p
                    |> Maybe.andThen
                        (\i ->
                            e
                                |> Data.Excavator.insertItem i
                        )
                    |> Maybe.map
                        (\excavator ->
                            world
                                |> Data.World.setActor id (Data.Actor.Excavator excavator)
                                |> Data.World.removeItem p
                        )
                    |> Maybe.withDefault world
            )
            w


mine : ( Int, Int ) -> World -> Generator World
mine pos world =
    pos
        |> Data.Position.neighbors
        |> List.foldl
            (\p ->
                case world |> Data.World.getBlock p of
                    Just (Data.Block.EntityBlock entity) ->
                        case entity of
                            Data.Entity.Vein _ ->
                                Random.andThen (Data.World.Generation.mine p)

                            _ ->
                                identity

                    _ ->
                        identity
            )
            (Random.constant world)
