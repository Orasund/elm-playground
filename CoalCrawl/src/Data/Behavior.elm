module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Actor
import Data.Behavior.Bomb
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.Wagon
import Data.Game exposing (Game)
import Data.Item
import Data.Sound exposing (Sound)
import Data.World
import Data.World.Generation
import Random exposing (Generator)


passTime : Game -> Generator ( Game, { promt : Maybe String, showModal : Bool, playSound : List Sound } )
passTime game =
    game
        |> Data.Behavior.Player.act
        |> Random.andThen
            (\( g, playSound ) ->
                g
                    |> Data.Behavior.Train.passTime
                    |> Random.map (\( g0, showModal ) -> ( g0, showModal, playSound ))
            )
        |> Random.andThen
            (\( g, showModal, playSound ) ->
                g.world
                    |> Data.World.getActors
                    |> List.foldl
                        (\( id, ( pos, actor ) ) ->
                            case actor of
                                Data.Actor.Wagon wagon ->
                                    wagon.movedFrom
                                        |> Maybe.map
                                            (\movedFrom ->
                                                Random.andThen
                                                    (\( g0, l ) ->
                                                        g0
                                                            |> Data.Behavior.Wagon.act
                                                                { backPos = movedFrom }
                                                                id
                                                            |> Random.map (Tuple.mapSecond ((++) l))
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

                                Data.Actor.Bomb _ ->
                                    Random.andThen
                                        (\( it, l ) ->
                                            it.world
                                                |> Data.Behavior.Bomb.timePassed id
                                                |> Random.map (\world -> ( { it | world = world }, l ))
                                        )
                        )
                        (Random.constant ( g, playSound ))
                    |> Random.map (\( g0, l ) -> ( g0, showModal, l ))
            )
        |> Random.map
            (\( g, showModal, playSound ) ->
                ( g
                , { promt = promt g
                  , showModal = showModal
                  , playSound = playSound
                  }
                )
            )


promt : Game -> Maybe String
promt game =
    if AnyBag.count Data.Item.Iron game.train.items >= Config.wagonCost then
        Just "You can build a wagon (W) when standing on an empty ground"

    else if not (AnyBag.member Data.Item.Coal game.train.items) then
        Just "Put coal (C) into the Train (T)"

    else
        Nothing
