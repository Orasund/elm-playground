module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Behavior.Wall
import Data.Entity
import Data.Game exposing (Game)
import Data.Item
import Data.World
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
                                Data.Entity.Wall { unstable } ->
                                    if unstable then
                                        Random.andThen (Data.Behavior.Wall.exposedUnstableWall pos)

                                    else
                                        identity

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
