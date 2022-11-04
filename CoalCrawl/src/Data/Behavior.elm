module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Actor
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.Wagon
import Data.Entity
import Data.Game exposing (Game)
import Data.Item
import Data.World
import Data.World.Generation
import Dict
import Random exposing (Generator)


passTime : Game -> Generator ( Game, { promt : Maybe String, showModal : Bool } )
passTime game =
    game
        |> Data.Behavior.Player.passTime
        |> Random.andThen Data.Behavior.Train.passTime
        |> Random.andThen
            (\( g, showModal ) ->
                g.world
                    |> Data.World.getEntities
                    |> List.foldl
                        (\( pos, entity ) ->
                            case entity of
                                Data.Entity.Cave caveType ->
                                    Random.andThen
                                        (\it ->
                                            it.world
                                                |> Data.World.Generation.exposedCave caveType pos
                                                |> Random.map (\world -> { it | world = world })
                                        )

                                Data.Entity.Actor id ->
                                    game.world.actors
                                        |> Dict.get id
                                        |> Maybe.map
                                            (\( _, actor ) ->
                                                case actor of
                                                    Data.Actor.Wagon wagon ->
                                                        wagon.movedFrom
                                                            |> Maybe.map
                                                                (\movedFrom ->
                                                                    Random.andThen
                                                                        (Data.Behavior.Wagon.move
                                                                            { backPos = movedFrom }
                                                                            id
                                                                        )
                                                                )
                                                            |> Maybe.withDefault identity
                                            )
                                        |> Maybe.withDefault identity

                                _ ->
                                    identity
                        )
                        (Random.constant g)
                    |> Random.map (\g0 -> ( g0, showModal ))
            )
        |> Random.map (\( g, showModal ) -> ( g, { promt = promt g, showModal = showModal } ))


promt : Game -> Maybe String
promt game =
    if AnyBag.count Data.Item.Iron game.train.items >= Config.wagonCost then
        Just "You can build a wagon (W) when standing on an empty ground"

    else if not (AnyBag.member Data.Item.Coal game.train.items) then
        Just "Put coal (C) into the Train (T)"

    else
        Nothing
