module Data.Behavior exposing (..)

import AnyBag
import Config
import Data.Behavior.Player
import Data.Behavior.Train
import Data.Game exposing (Game)
import Data.Item
import Random exposing (Generator)


passTime : Game -> Generator ( Game, Maybe String )
passTime game =
    game
        |> Data.Behavior.Player.passTime
        |> Random.andThen Data.Behavior.Train.passTime
        |> Random.map (\g -> ( g, promt g ))


promt : Game -> Maybe String
promt game =
    if AnyBag.count Data.Item.IronOre game.train.items >= Config.wagonCost then
        Just "You can build a wagon (W) when standing on an empty ground"

    else if not (AnyBag.member Data.Item.Coal game.train.items) then
        Just "Put coal (C) into the Train (T)"

    else
        Nothing
