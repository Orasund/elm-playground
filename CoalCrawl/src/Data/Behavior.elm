module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Actor
import Data.Behavior.Bomb
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.Wagon
import Data.Effect exposing (Effect)
import Data.Game exposing (Game)
import Data.Item
import Data.World
import Data.World.Generation
import Random exposing (Generator)


passTime : Game -> Generator ( Game, List Effect )
passTime game =
    game
        |> Data.Behavior.Player.act
        |> Data.Effect.andThen Data.Behavior.Train.passTime
        |> Data.Effect.andThen
            (\g ->
                g.world
                    |> Data.World.getActors
                    |> List.foldl
                        (\( id, ( pos, actor ) ) ->
                            case actor of
                                Data.Actor.Wagon wagon ->
                                    wagon.movedFrom
                                        |> Maybe.map
                                            (\movedFrom ->
                                                Data.Effect.map
                                                    (Data.Behavior.Wagon.act
                                                        { backPos = movedFrom }
                                                        id
                                                    )
                                            )
                                        |> Maybe.withDefault identity

                                Data.Actor.Cave caveType ->
                                    Random.andThen
                                        (\( it, l ) ->
                                            it.world
                                                |> Data.World.Generation.exposedCave caveType pos
                                                |> Random.map (\world -> ( { it | world = world }, l ))
                                        )

                                Data.Actor.Mine caveType ->
                                    Random.andThen
                                        (\( it, l ) ->
                                            it.world
                                                |> Data.World.Generation.exposedCave caveType pos
                                                |> Random.map (\world -> ( { it | world = world }, l ))
                                        )

                                Data.Actor.Bomb _ ->
                                    Random.andThen
                                        (\( it, l ) ->
                                            it.world
                                                |> Data.Behavior.Bomb.timePassed id
                                                |> Random.map (\world -> ( { it | world = world }, l ))
                                        )
                        )
                        (Random.constant ( g, [] ))
            )
        |> Random.map (\( g, l ) -> ( g, promt g ++ l ))


promt : Game -> List Effect
promt game =
    if AnyBag.count Data.Item.Iron game.train.items >= Config.wagonCost then
        "You can build a wagon (W) when standing on an empty ground"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else if not (AnyBag.member Data.Item.Coal game.train.items) then
        "Put coal (C) into the Train (T)"
            |> Data.Effect.ShowPromt
            |> List.singleton

    else
        []
